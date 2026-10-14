       IDENTIFICATION DIVISION.
       PROGRAM-ID. NACHBAT.
      *================================================================*
      * PROGRAM  : NACHBAT                                             *
      * PURPOSE  : PROCESSES THE INWARD NACH DEBIT FILE FROM THE       *
      *            CLEARING HOUSE (data/NACHIN.DAT). EACH ITEM IS      *
      *            CHECKED AGAINST ITS MANDATE AND THE ACCOUNT IS      *
      *            DEBITED, OR THE ITEM IS RETURNED WITH A REASON:     *
      *              01 INSUFFICIENT BALANCE                           *
      *              02 ACCOUNT CLOSED / FROZEN / DORMANT              *
      *              04 MANDATE NOT REGISTERED                         *
      *              05 MANDATE CANCELLED OR SUSPENDED                 *
      *              06 AMOUNT EXCEEDS MANDATE MAXIMUM                 *
      *              07 MANDATE NOT YET STARTED OR EXPIRED             *
      *              08 ALREADY DEBITED IN THIS FREQUENCY PERIOD       *
      *              09 DUPLICATE TRANSACTION REFERENCE IN FILE        *
      *            AN INSUFFICIENT-BALANCE RETURN COSTS THE CUSTOMER   *
      *            500 + 18% GST; THREE IN A ROW SUSPEND THE MANDATE.  *
      *            THE RESPONSE FILE GOES BACK TO THE CLEARING HOUSE   *
      *            AND THE DAY'S ACCEPTED TOTAL IS SETTLED:            *
      *              CUSTOMER DEBITS  DR CUSTOMER  CR 1304 (ACCTPOST)  *
      *              SETTLEMENT       DR 1304      CR 1302             *
      * INPUT    : RUN DATE (BLANK = TODAY), data/NACHIN.DAT           *
      *            LAYOUT: UMRN X(20) AMOUNT 9(11)V99 SETTLE-DATE 9(8) *
      *                    TXN-REF X(20) CORPORATE X(30)               *
      * OUTPUT   : data/NACHRESP_<DATE>.TXT                            *
      *            LAYOUT: UMRN X(20) TXN-REF X(20) AMOUNT 9(11)V99    *
      *                    STATUS X(1) A/R  REASON 9(2)  ACCOUNT 9(10) *
      * FILES    : MANDMAST (I-O), NACHIN (INPUT), NACHRESP (OUTPUT)   *
      * CALLS    : ACCTPOST, GLPOST, AUDITLOG                          *
      * RETURN   : 0 = ALL DEBITED, 4 = SOME RETURNED, 12 = NO FILE /  *
      *            FILE ERROR                                          *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAND-FILE ASSIGN TO 'data/MANDMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS RANDOM
               RECORD KEY IS NM-UMRN FILE STATUS IS WS-MAND-FS.
           SELECT IN-FILE ASSIGN TO 'data/NACHIN.DAT'
               ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS WS-IN-FS.
           SELECT RESP-FILE ASSIGN TO WS-RESP-NAME
               ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS WS-OUT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  MAND-FILE.
           COPY MANDREC.
       FD  IN-FILE.
       01  IN-RECORD.
           05  IN-UMRN                 PIC X(20).
           05  IN-AMOUNT               PIC 9(11)V99.
           05  IN-SETTLE-DATE          PIC 9(08).
           05  IN-TXN-REF              PIC X(20).
           05  IN-CORPORATE            PIC X(30).
       FD  RESP-FILE.
       01  RESP-RECORD.
           05  RS-UMRN                 PIC X(20).
           05  RS-TXN-REF              PIC X(20).
           05  RS-AMOUNT               PIC 9(11)V99.
           05  RS-STATUS               PIC X(01).
           05  RS-REASON               PIC 9(02).
           05  RS-ACCT-NO              PIC 9(10).

       WORKING-STORAGE SECTION.
       01  WS-MAND-FS                  PIC X(02).
       01  WS-IN-FS                    PIC X(02).
       01  WS-OUT-FS                   PIC X(02).
       01  WS-RESP-NAME                PIC X(40).
       01  WS-IN-RUN-DATE              PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-RUN-DATE-R REDEFINES WS-RUN-DATE.
           05  WS-RUN-YYYY             PIC 9(04).
           05  WS-RUN-MM               PIC 9(02).
           05  WS-RUN-DD               PIC 9(02).
       01  WS-EOF-SW                   PIC X(01) VALUE 'N'.
           88  END-OF-INPUT                      VALUE 'Y'.
       01  WS-REASON                   PIC 9(02).
           88  ITEM-ACCEPTED                     VALUE 00.
           88  RSN-NO-FUNDS                      VALUE 01.
       01  WS-MAND-FOUND-SW            PIC X(01).
           88  MANDATE-FOUND                     VALUE 'Y'.
      * TRANSACTION REFERENCES ALREADY SEEN IN THIS FILE
       01  WS-SEEN-COUNT               PIC 9(04) VALUE ZERO.
       01  WS-SEEN-TABLE.
           05  WS-SEEN-REF             PIC X(20) OCCURS 2000 TIMES.
       01  WS-S                        PIC 9(04).
       01  WS-DUP-SW                   PIC X(01).
           88  IS-DUPLICATE                      VALUE 'Y'.
      * FREQUENCY WINDOW
       01  WS-LAST                     PIC 9(08).
       01  WS-LAST-R REDEFINES WS-LAST.
           05  WS-LAST-YYYY            PIC 9(04).
           05  WS-LAST-MM              PIC 9(02).
           05  WS-LAST-DD              PIC 9(02).
       01  WS-MONTHS-APART             PIC S9(05).
      * CHARGES
       01  WS-BOUNCE-FEE               PIC 9(05)V99 VALUE 500.00.
       01  WS-GST-RATE                 PIC 9V99 VALUE 0.18.
       01  WS-GST                      PIC 9(05)V99.
       01  WS-SUSPEND-AFTER            PIC 9(02) VALUE 3.
      * TOTALS
       01  WS-TOTALS.
           05  WS-READ                 PIC 9(06) VALUE ZERO.
           05  WS-ACCEPTED             PIC 9(06) VALUE ZERO.
           05  WS-RETURNED             PIC 9(06) VALUE ZERO.
           05  WS-SUSPENDED            PIC 9(06) VALUE ZERO.
           05  WS-FEES                 PIC 9(06) VALUE ZERO.
           05  WS-ACCEPTED-AMT         PIC 9(13)V99 VALUE ZERO.
           05  WS-RETURNED-AMT         PIC 9(13)V99 VALUE ZERO.
           05  WS-REASON-CNT           PIC 9(05) OCCURS 9 TIMES.
       01  WS-R                        PIC 9(02).
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-CNT                      PIC ZZZ,ZZ9.
           COPY POSTPARM.
           COPY GLPARM.
           COPY GLCODES.
           COPY AUDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'RUN DATE (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-RUN-DATE
           IF WS-IN-RUN-DATE = SPACES OR WS-IN-RUN-DATE NOT NUMERIC
               ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           ELSE
               MOVE WS-IN-RUN-DATE TO WS-RUN-DATE
           END-IF
           INITIALIZE WS-REASON-CNT(1) WS-REASON-CNT(2)
               WS-REASON-CNT(3) WS-REASON-CNT(4) WS-REASON-CNT(5)
               WS-REASON-CNT(6) WS-REASON-CNT(7) WS-REASON-CNT(8)
               WS-REASON-CNT(9)
           OPEN INPUT IN-FILE
           IF WS-IN-FS NOT = '00'
               DISPLAY 'NACHBAT: NO INWARD FILE data/NACHIN.DAT'
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           OPEN I-O MAND-FILE
           IF WS-MAND-FS = '35'
               OPEN OUTPUT MAND-FILE
               CLOSE MAND-FILE
               OPEN I-O MAND-FILE
           END-IF
           STRING 'data/NACHRESP_' WS-RUN-DATE '.TXT'
               DELIMITED BY SIZE INTO WS-RESP-NAME
           OPEN OUTPUT RESP-FILE

           PERFORM UNTIL END-OF-INPUT
               READ IN-FILE
                   AT END SET END-OF-INPUT TO TRUE
                   NOT AT END
                       ADD 1 TO WS-READ
                       PERFORM 1000-PROCESS-ITEM
               END-READ
           END-PERFORM
           CLOSE IN-FILE MAND-FILE RESP-FILE

           PERFORM 5000-SETTLE
           PERFORM 9000-REPORT
           IF WS-RETURNED > ZERO
               MOVE 4 TO RETURN-CODE
           END-IF
           GOBACK.

      *================================================================*
      * ONE INWARD DEBIT                                               *
      *================================================================*
       1000-PROCESS-ITEM.
           MOVE ZERO TO WS-REASON
           MOVE ZERO TO NM-ACCT-NO
           PERFORM 1100-CHECK-DUPLICATE
           IF IS-DUPLICATE
               MOVE 09 TO WS-REASON
           ELSE
               PERFORM 1200-CHECK-MANDATE
           END-IF
           IF ITEM-ACCEPTED
               PERFORM 2000-DEBIT
           END-IF
           IF ITEM-ACCEPTED
               ADD 1 TO WS-ACCEPTED
               ADD IN-AMOUNT TO WS-ACCEPTED-AMT
               ADD 1 TO NM-DEBITS
               MOVE WS-RUN-DATE TO NM-LAST-DEBIT-DATE
               MOVE IN-AMOUNT   TO NM-LAST-AMOUNT
               MOVE ZERO        TO NM-CONSEC-BOUNCES
               REWRITE MAND-RECORD
           ELSE
               PERFORM 3000-RETURN-ITEM
           END-IF
           PERFORM 4000-WRITE-RESPONSE.

       1100-CHECK-DUPLICATE.
           MOVE 'N' TO WS-DUP-SW
           PERFORM VARYING WS-S FROM 1 BY 1 UNTIL WS-S > WS-SEEN-COUNT
               IF WS-SEEN-REF(WS-S) = IN-TXN-REF
                   SET IS-DUPLICATE TO TRUE
               END-IF
           END-PERFORM
           IF NOT IS-DUPLICATE AND WS-SEEN-COUNT < 2000
               ADD 1 TO WS-SEEN-COUNT
               MOVE IN-TXN-REF TO WS-SEEN-REF(WS-SEEN-COUNT)
           END-IF.

       1200-CHECK-MANDATE.
           MOVE 'N' TO WS-MAND-FOUND-SW
           MOVE IN-UMRN TO NM-UMRN
           READ MAND-FILE
               INVALID KEY
                   MOVE 04 TO WS-REASON
                   EXIT PARAGRAPH
           END-READ
           SET MANDATE-FOUND TO TRUE
           EVALUATE TRUE
               WHEN NOT NM-ACTIVE
                   MOVE 05 TO WS-REASON
               WHEN IN-AMOUNT > NM-MAX-AMOUNT
                   MOVE 06 TO WS-REASON
               WHEN WS-RUN-DATE < NM-START-DATE
                   MOVE 07 TO WS-REASON
               WHEN NM-END-DATE NOT = ZERO
                    AND WS-RUN-DATE > NM-END-DATE
                   MOVE 07 TO WS-REASON
               WHEN OTHER
                   PERFORM 1300-CHECK-FREQUENCY
           END-EVALUATE.

      *    ONE DEBIT PER MONTH / QUARTER / YEAR; 'A' HAS NO LIMIT
       1300-CHECK-FREQUENCY.
           IF NM-LAST-DEBIT-DATE = ZERO OR NM-AS-PRESENTED
               EXIT PARAGRAPH
           END-IF
           MOVE NM-LAST-DEBIT-DATE TO WS-LAST
           COMPUTE WS-MONTHS-APART =
                   (WS-RUN-YYYY - WS-LAST-YYYY) * 12
                   + WS-RUN-MM - WS-LAST-MM
           EVALUATE TRUE
               WHEN NM-MONTHLY   AND WS-MONTHS-APART < 1
                   MOVE 08 TO WS-REASON
               WHEN NM-QUARTERLY AND WS-MONTHS-APART < 3
                   MOVE 08 TO WS-REASON
               WHEN NM-YEARLY    AND WS-MONTHS-APART < 12
                   MOVE 08 TO WS-REASON
           END-EVALUATE.

       2000-DEBIT.
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE NM-ACCT-NO  TO PP-ACCT-NO
           MOVE IN-AMOUNT   TO PP-AMOUNT
           MOVE 'ND'        TO PP-TRAN-TYPE
           MOVE SPACES      TO PP-NARRATION
           STRING 'NACH ' IN-CORPORATE DELIMITED BY SIZE
               INTO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           EVALUATE TRUE
               WHEN PP-OK
                   CONTINUE
               WHEN PP-INSUFFICIENT
                   MOVE 01 TO WS-REASON
               WHEN OTHER
                   MOVE 02 TO WS-REASON
           END-EVALUATE.

      *================================================================*
      * RETURN: COUNT, BOUNCE FEE ON NO-FUNDS, SUSPEND AFTER 3 IN A ROW*
      *================================================================*
       3000-RETURN-ITEM.
           ADD 1 TO WS-RETURNED
           ADD IN-AMOUNT TO WS-RETURNED-AMT
           ADD 1 TO WS-REASON-CNT(WS-REASON)
           IF NOT MANDATE-FOUND
               EXIT PARAGRAPH
           END-IF
           IF RSN-NO-FUNDS
               ADD 1 TO NM-BOUNCES NM-CONSEC-BOUNCES
               PERFORM 3100-BOUNCE-FEE
               IF NM-CONSEC-BOUNCES >= WS-SUSPEND-AFTER
                  AND NM-ACTIVE
                   SET NM-SUSPENDED TO TRUE
                   ADD 1 TO WS-SUSPENDED
                   DISPLAY '  MANDATE ' NM-UMRN ' SUSPENDED AFTER '
                           NM-CONSEC-BOUNCES ' BOUNCES'
                   MOVE 'NACHBAT'   TO AU-PROGRAM
                   MOVE 'SUSPEND'   TO AU-ACTION
                   MOVE NM-UMRN(5:16) TO AU-KEY
                   MOVE 'CONSECUTIVE INSUFFICIENT-FUNDS RETURNS'
                        TO AU-DETAIL
                   CALL 'AUDITLOG' USING AUD-PARMS
               END-IF
               REWRITE MAND-RECORD
           END-IF.

       3100-BOUNCE-FEE.
           COMPUTE WS-GST ROUNDED = WS-BOUNCE-FEE * WS-GST-RATE
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE NM-ACCT-NO TO PP-ACCT-NO
           COMPUTE PP-AMOUNT = WS-BOUNCE-FEE + WS-GST
           MOVE 'CH'       TO PP-TRAN-TYPE
           MOVE 'NACH RETURN CHARGES' TO PP-NARRATION
           MOVE 'Y'        TO PP-IGNORE-MIN-BAL
           MOVE 'Y'        TO PP-SYSTEM-TXN
           CALL 'ACCTPOST' USING POST-PARMS
           IF PP-OK
               ADD 1 TO WS-FEES
               MOVE GLC-FEE-INCOME  TO GP-DR-GL
               MOVE GLC-GST-PAYABLE TO GP-CR-GL
               MOVE WS-GST          TO GP-AMOUNT
               MOVE NM-UMRN(5:16)   TO GP-REF
               MOVE 'GST ON NACH RETURN FEE' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
           ELSE
               DISPLAY '  RETURN FEE NOT RECOVERED FROM ' NM-ACCT-NO
                       ' (RC ' PP-RETURN-CODE ')'
           END-IF.

       4000-WRITE-RESPONSE.
           MOVE IN-UMRN    TO RS-UMRN
           MOVE IN-TXN-REF TO RS-TXN-REF
           MOVE IN-AMOUNT  TO RS-AMOUNT
           MOVE WS-REASON  TO RS-REASON
           MOVE NM-ACCT-NO TO RS-ACCT-NO
           IF ITEM-ACCEPTED
               MOVE 'A' TO RS-STATUS
           ELSE
               MOVE 'R' TO RS-STATUS
               MOVE IN-AMOUNT TO ED-AMT
               DISPLAY '  RETURNED ' IN-UMRN ' ' ED-AMT ' REASON '
                       WS-REASON
           END-IF
           WRITE RESP-RECORD.

      *    PAY THE CLEARING HOUSE WHAT WAS COLLECTED
       5000-SETTLE.
           IF WS-ACCEPTED-AMT > ZERO
               MOVE GLC-NACH-SETTLE    TO GP-DR-GL
               MOVE GLC-PAYMENT-SETTLE TO GP-CR-GL
               MOVE WS-ACCEPTED-AMT    TO GP-AMOUNT
               MOVE SPACES TO GP-REF
               STRING 'NACH' WS-RUN-DATE DELIMITED BY SIZE
                   INTO GP-REF
               MOVE 'NACH DEBIT SETTLEMENT' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
               IF NOT GP-OK
                   DISPLAY 'NACHBAT: SETTLEMENT GL FAILED, RC=' GP-RC
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF.

       9000-REPORT.
           DISPLAY ' '
           DISPLAY 'NACH INWARD DEBITS FOR ' WS-RUN-DATE
           MOVE WS-READ TO ED-CNT
           DISPLAY 'ITEMS RECEIVED   : ' ED-CNT
           MOVE WS-ACCEPTED TO ED-CNT
           MOVE WS-ACCEPTED-AMT TO ED-AMT
           DISPLAY 'DEBITED          : ' ED-CNT '  ' ED-AMT
           MOVE WS-RETURNED TO ED-CNT
           MOVE WS-RETURNED-AMT TO ED-AMT
           DISPLAY 'RETURNED         : ' ED-CNT '  ' ED-AMT
           PERFORM VARYING WS-R FROM 1 BY 1 UNTIL WS-R > 9
               IF WS-REASON-CNT(WS-R) > ZERO
                   MOVE WS-REASON-CNT(WS-R) TO ED-CNT
                   DISPLAY '  REASON ' WS-R '       : ' ED-CNT
               END-IF
           END-PERFORM
           MOVE WS-FEES TO ED-CNT
           DISPLAY 'RETURN FEES      : ' ED-CNT
           MOVE WS-SUSPENDED TO ED-CNT
           DISPLAY 'MANDATES SUSPENDED: ' ED-CNT
           DISPLAY 'RESPONSE FILE    : ' FUNCTION TRIM(WS-RESP-NAME).
