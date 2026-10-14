       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLPOST.
      *================================================================*
      * PROGRAM  : GLPOST                                              *
      * PURPOSE  : POSTS ONE BALANCED JOURNAL ENTRY (DEBIT GL X,       *
      *            CREDIT GL Y) TO THE GENERAL LEDGER AND APPENDS BOTH *
      *            LEGS TO THE GL JOURNAL. BOTH GLS ARE CHECKED BEFORE *
      *            ANYTHING IS WRITTEN, SO AN ENTRY IS ALL-OR-NOTHING. *
      *            THE FIRST CALL ON A NEW SYSTEM CREATES THE LEDGER   *
      *            AND SEEDS THE CHART OF ACCOUNTS.                    *
      * CALL     : CALL 'GLPOST' USING GL-PARMS  (COPY GLPARM)         *
      * FILES    : GLMAST (INDEXED, I-O), GLJRNL (SEQUENTIAL, EXTEND)  *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GL-FILE ASSIGN TO 'data/GLMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS GL-CODE
               FILE STATUS IS WS-GL-FS.
           SELECT JRNL-FILE ASSIGN TO 'data/GLJRNL.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JRNL-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  GL-FILE.
           COPY GLREC.
       FD  JRNL-FILE.
           COPY GLJREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY SESSION.

       01  WS-GL-FS                    PIC X(02).
           88  GL-FS-OK                          VALUE '00'.
           88  GL-FS-NOFILE                      VALUE '35'.
       01  WS-JRNL-FS                  PIC X(02).
       01  WS-JOURNAL-SEQ              PIC 9(06) VALUE ZERO.
       01  WS-RUN-ID                   PIC 9(06).

      * CHART OF ACCOUNTS: CODE, TYPE, NAME
       01  WS-CHART.
           05  FILLER PIC X(35) VALUE '1001ACASH IN HAND'.
           05  FILLER PIC X(35) VALUE '1002AATM CASH'.
           05  FILLER PIC X(35) VALUE '1201ALOANS AND ADVANCES'.
           05  FILLER PIC X(35) VALUE '1290ALOAN REPAYMENT SUSPENSE'.
           05  FILLER PIC X(35) VALUE '1301ACHEQUE CLEARING ACCOUNT'.
           05  FILLER PIC X(35) VALUE '1302APAYMENT SETTLEMENT (RBI)'.
           05  FILLER PIC X(35) VALUE '1210ACREDIT CARD RECEIVABLES'.
           05  FILLER PIC X(35) VALUE '1220AGOLD LOANS'.
           05  FILLER PIC X(35) VALUE '1230ALOANS AGAINST DEPOSITS'.
           05  FILLER PIC X(35) VALUE '1295ACASH SHORTAGE SUSPENSE'.
           05  FILLER PIC X(35) VALUE '1303ACURRENCY CHEST ACCOUNT'.
           05  FILLER PIC X(35) VALUE '1304ANACH SETTLEMENT'.
           05  FILLER PIC X(35) VALUE '1305ANOSTRO ACCOUNT (FOREX)'.
           05  FILLER PIC X(35) VALUE '1240AINTEREST ACCRUED - LOANS'.
           05  FILLER PIC X(35) VALUE '1299ALOAN LOSS PROVISION'.
           05  FILLER PIC X(35) VALUE '2101LSAVINGS DEPOSITS'.
           05  FILLER PIC X(35) VALUE '2102LCURRENT DEPOSITS'.
           05  FILLER PIC X(35) VALUE '2201LTERM DEPOSITS'.
           05  FILLER PIC X(35) VALUE '2501LGST PAYABLE'.
           05  FILLER PIC X(35) VALUE '2502LTDS PAYABLE'.
           05  FILLER PIC X(35) VALUE '2903LCARD MERCHANT SETTLEMENT'.
           05  FILLER PIC X(35) VALUE '2904LCASH EXCESS (SUNDRY)'.
           05  FILLER PIC X(35) VALUE '2905LMARGIN MONEY (LC / BG)'.
           05  FILLER PIC X(35) VALUE '2503LTCS PAYABLE'.
           05  FILLER PIC X(35) VALUE
               '2900LINTER-ACCOUNT TRANSFER SUSP'.
           05  FILLER PIC X(35) VALUE '3001COPENING BALANCE CONTROL'.
           05  FILLER PIC X(35) VALUE '4001IINTEREST INCOME - LOANS'.
           05  FILLER PIC X(35) VALUE
               '4002IINTEREST INCOME - OVERDRAFT'.
           05  FILLER PIC X(35) VALUE '4101IFEES AND CHARGES'.
           05  FILLER PIC X(35) VALUE '4102ILOCKER RENT'.
           05  FILLER PIC X(35) VALUE '4103IBAD DEBTS RECOVERED'.
           05  FILLER PIC X(35) VALUE '4104IEXCHANGE INCOME'.
           05  FILLER PIC X(35) VALUE '4105ITRADE FINANCE COMMISSION'.
           05  FILLER PIC X(35) VALUE '4003IINTEREST INCOME - CARDS'.
           05  FILLER PIC X(35) VALUE '5001EINTEREST PAID - SAVINGS'.
           05  FILLER PIC X(35) VALUE '5002EINTEREST PAID - TERM DEP'.
           05  FILLER PIC X(35) VALUE '5101EPROVISION FOR LOAN LOSSES'.
       01  WS-CHART-R REDEFINES WS-CHART.
           05  WS-COA OCCURS 37 TIMES.
               10  COA-CODE            PIC 9(04).
               10  COA-TYPE            PIC X(01).
               10  COA-NAME            PIC X(30).
       01  WS-I                        PIC 9(02).
       01  WS-WANTED-GL                PIC 9(04).

       01  WS-SAVE-DR-REC              PIC X(100).

       LINKAGE SECTION.
           COPY GLPARM.

       PROCEDURE DIVISION USING GL-PARMS.
       0000-MAIN.
           SET GP-OK TO TRUE
           IF GP-AMOUNT NOT > ZERO OR GP-DR-GL = GP-CR-GL
               SET GP-BAD-AMOUNT TO TRUE
               GOBACK
           END-IF
           PERFORM 1000-OPEN-LEDGER
           IF NOT GP-OK
               GOBACK
           END-IF

      *    VALIDATE BOTH SIDES BEFORE TOUCHING EITHER. A CODE ADDED TO
      *    THE CHART AFTER THE LEDGER WAS CREATED IS OPENED ON FIRST USE
           MOVE GP-CR-GL TO GL-CODE
           PERFORM 1200-READ-OR-OPEN-GL
           IF GP-OK
               MOVE GP-DR-GL TO GL-CODE
               PERFORM 1200-READ-OR-OPEN-GL
           END-IF
           IF NOT GP-OK
               DISPLAY 'GLPOST: UNKNOWN GL IN ENTRY ' GP-DR-GL '/'
                       GP-CR-GL ' REF ' GP-REF
               CLOSE GL-FILE
               GOBACK
           END-IF

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           ADD 1 TO WS-JOURNAL-SEQ
      *    DEBIT LEG (DR RECORD IS STILL IN THE BUFFER)
           ADD GP-AMOUNT TO GL-BALANCE GL-TOTAL-DEBITS
           MOVE WS-CURR-DATE TO GL-LAST-POST-DATE
           REWRITE GL-RECORD
               INVALID KEY SET GP-FILE-ERROR TO TRUE
           END-REWRITE
      *    CREDIT LEG
           MOVE GP-CR-GL TO GL-CODE
           READ GL-FILE
               INVALID KEY SET GP-FILE-ERROR TO TRUE
           END-READ
           IF GP-OK
               SUBTRACT GP-AMOUNT FROM GL-BALANCE
               ADD GP-AMOUNT TO GL-TOTAL-CREDITS
               MOVE WS-CURR-DATE TO GL-LAST-POST-DATE
               REWRITE GL-RECORD
                   INVALID KEY SET GP-FILE-ERROR TO TRUE
               END-REWRITE
           END-IF
           CLOSE GL-FILE
           IF NOT GP-OK
               DISPLAY 'GLPOST: SEVERE - LEDGER UPDATE FAILED FS='
                       WS-GL-FS ' REF ' GP-REF
               GOBACK
           END-IF
           PERFORM 2000-WRITE-JOURNAL
           GOBACK.

       1000-OPEN-LEDGER.
           OPEN I-O GL-FILE
           IF GL-FS-NOFILE
               PERFORM 1100-SEED-CHART
               OPEN I-O GL-FILE
           END-IF
           IF NOT GL-FS-OK
               DISPLAY 'GLPOST: LEDGER OPEN FAILED FS=' WS-GL-FS
               SET GP-FILE-ERROR TO TRUE
           END-IF.

       1100-SEED-CHART.
           OPEN OUTPUT GL-FILE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 37
               INITIALIZE GL-RECORD
               MOVE COA-CODE(WS-I) TO GL-CODE
               MOVE COA-TYPE(WS-I) TO GL-TYPE
               MOVE COA-NAME(WS-I) TO GL-NAME
               WRITE GL-RECORD
           END-PERFORM
           CLOSE GL-FILE.

       1200-READ-OR-OPEN-GL.
           READ GL-FILE
               INVALID KEY
                   PERFORM 1300-OPEN-FROM-CHART
           END-READ.

       1300-OPEN-FROM-CHART.
           MOVE GL-CODE TO WS-WANTED-GL
           SET GP-UNKNOWN-GL TO TRUE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 37
               IF COA-CODE(WS-I) = WS-WANTED-GL
                   INITIALIZE GL-RECORD
                   MOVE COA-CODE(WS-I) TO GL-CODE
                   MOVE COA-TYPE(WS-I) TO GL-TYPE
                   MOVE COA-NAME(WS-I) TO GL-NAME
                   WRITE GL-RECORD
                   SET GP-OK TO TRUE
                   READ GL-FILE
               END-IF
           END-PERFORM.

       2000-WRITE-JOURNAL.
           OPEN EXTEND JRNL-FILE
           IF WS-JRNL-FS = '35'
               OPEN OUTPUT JRNL-FILE
           END-IF
           IF WS-JRNL-FS NOT = '00'
               DISPLAY 'GLPOST: WARNING - JOURNAL UNAVAILABLE'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-CURR-DATE   TO GJ-DATE
           MOVE WS-CURR-TIME   TO GJ-TIME
           COMPUTE GJ-ENTRY-ID = WS-CURR-TIME * 1000000
                               + WS-JOURNAL-SEQ
           MOVE GP-REF         TO GJ-REF
           MOVE GP-NARRATION   TO GJ-NARRATION
           MOVE GP-AMOUNT      TO GJ-AMOUNT
           IF SES-USER-ID = SPACES OR LOW-VALUES
               MOVE 'SYSTEM' TO GJ-USER-ID
           ELSE
               MOVE SES-USER-ID TO GJ-USER-ID
           END-IF
           MOVE GP-DR-GL TO GJ-GL-CODE
           MOVE 'D'      TO GJ-DR-CR
           WRITE GJ-RECORD
           MOVE GP-CR-GL TO GJ-GL-CODE
           MOVE 'C'      TO GJ-DR-CR
           WRITE GJ-RECORD
           CLOSE JRNL-FILE.
