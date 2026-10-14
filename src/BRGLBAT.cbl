       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRGLBAT.
      *================================================================*
      * PROGRAM  : BRGLBAT                                             *
      * PURPOSE  : BRANCH ACCOUNTING FOR ONE BUSINESS DAY.             *
      *            A. BRANCH POSITION - SAVINGS, CURRENT, TERM         *
      *               DEPOSITS, OVERDRAWN CURRENT ACCOUNTS AND LOANS   *
      *               BOOKED AT EACH BRANCH (FROM THE SUB-LEDGERS).    *
      *            B. INTER-BRANCH (IB) ACCOUNTS - EVERY TRANSFER      *
      *               BETWEEN ACCOUNTS OF TWO DIFFERENT BRANCHES IS AN *
      *               IB ENTRY: THE ORIGINATING BRANCH OWES HEAD       *
      *               OFFICE, THE RESPONDING BRANCH IS OWED. THE DEBIT *
      *               LEG (TO) AND CREDIT LEG (TI) ARE MATCHED BY      *
      *               SORTING ON (FROM, TO, DATE, AMOUNT); A REVERSAL  *
      *               OF A FAILED TRANSFER CANCELS ITS DEBIT LEG.      *
      *               UNMATCHED LEGS ARE LISTED, AND THE IB POSITIONS  *
      *               OF ALL BRANCHES MUST ADD UP TO ZERO.             *
      *            THE BANK-LEVEL GL IS NOT TOUCHED: IB ENTRIES NET TO *
      *            ZERO ACROSS BRANCHES.                               *
      * INPUT    : BUSINESS DATE YYYYMMDD (BLANK = TODAY)              *
      * OUTPUT   : data/BRGL_<DATE>.TXT                                *
      * FILES    : ACCTMAST, LOANMAST, DEPMAST, BRMAST (INPUT),        *
      *            TRANLOG (INPUT), SORT WORK FILE                     *
      * RC       : 0 IB RECONCILED, 4 UNMATCHED LEGS / IB NOT ZERO,    *
      *            12 FILE ERROR                                       *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE ASSIGN TO 'data/ACCTMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-ACCT-FS.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS LOAN-NO
               FILE STATUS IS WS-LOAN-FS.
           SELECT DEP-FILE ASSIGN TO 'data/DEPMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS DEP-NO
               FILE STATUS IS WS-DEP-FS.
           SELECT BR-FILE ASSIGN TO 'data/BRMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BR-CODE
               FILE STATUS IS WS-BR-FS.
           SELECT TRAN-FILE ASSIGN TO 'data/TRANLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRAN-FS.
           SELECT SORT-FILE ASSIGN TO 'data/BRGLSORT.TMP'.
           SELECT RPT-FILE ASSIGN TO WS-RPT-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
           COPY ACCTREC.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  DEP-FILE.
           COPY DEPREC.
       FD  BR-FILE.
           COPY BRMREC.
       FD  TRAN-FILE.
           COPY TRANREC.
       SD  SORT-FILE.
       01  SORT-REC.
           05  SR-DR-ACCT              PIC 9(10).
           05  SR-CR-ACCT              PIC 9(10).
           05  SR-DATE                 PIC 9(08).
           05  SR-AMOUNT               PIC 9(11)V99.
      *    O = DEBIT LEG (TO), V = REVERSAL, R = CREDIT LEG (TI)
           05  SR-SIDE                 PIC X(01).
           05  SR-SEQ                  PIC 9(09).
           05  SR-DR-BRANCH            PIC X(06).
           05  SR-CR-BRANCH            PIC X(06).
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(130).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
       01  WS-LOAN-FS                  PIC X(02).
       01  WS-DEP-FS                   PIC X(02).
       01  WS-BR-FS                    PIC X(02).
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-NAME                 PIC X(40).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-EOF-SW                   PIC X(01).
       01  WS-SORT-EOF                 PIC X(01).
       01  WS-SEQ                      PIC 9(09) VALUE ZERO.
       01  WS-RC                       PIC 9(02) VALUE ZERO.
       01  WS-REVERSAL-TEXT            PIC X(27) VALUE
           'REVERSAL OF FAILED TRANSFER'.
       01  WS-WANT-BRANCH              PIC X(06).

      * BRANCH TABLE (POSITION AND IB)
       01  WS-BR-CNT                   PIC 9(03) VALUE ZERO.
       01  WS-BR-TABLE.
           05  WS-BR OCCURS 100 TIMES.
               10  BT-CODE             PIC X(06).
               10  BT-SAVINGS          PIC S9(15)V99.
               10  BT-CURRENT          PIC S9(15)V99.
               10  BT-OVERDRAWN        PIC S9(15)V99.
               10  BT-TERM-DEP         PIC S9(15)V99.
               10  BT-LOANS            PIC S9(15)V99.
               10  BT-ACCOUNTS         PIC 9(07).
      *        + = OWED BY HEAD OFFICE, - = OWES HEAD OFFICE
               10  BT-IB               PIC S9(15)V99.
               10  BT-IB-DR-CNT        PIC 9(07).
               10  BT-IB-CR-CNT        PIC 9(07).
       01  WS-B                        PIC 9(03).
       01  WS-B-DR                     PIC 9(03).
       01  WS-B-CR                     PIC 9(03).

      * BRANCH-PAIR TRAFFIC
       01  WS-PAIR-CNT                 PIC 9(04) VALUE ZERO.
       01  WS-PAIR-TABLE.
           05  WS-PAIR OCCURS 500 TIMES.
               10  PR-FROM             PIC X(06).
               10  PR-TO               PIC X(06).
               10  PR-COUNT            PIC 9(07).
               10  PR-AMOUNT           PIC 9(15)V99.
       01  WS-P                        PIC 9(04).

      * CURRENT MATCH GROUP (SAME FROM, TO, DATE, AMOUNT)
       01  WS-GRP-KEY.
           05  GK-DR                   PIC 9(10).
           05  GK-CR                   PIC 9(10).
           05  GK-DATE                 PIC 9(08).
           05  GK-AMOUNT               PIC 9(11)V99.
       01  WS-NEW-KEY.
           05  NK-DR                   PIC 9(10).
           05  NK-CR                   PIC 9(10).
           05  NK-DATE                 PIC 9(08).
           05  NK-AMOUNT               PIC 9(11)V99.
       01  WS-GRP-DR-BR                PIC X(06).
       01  WS-GRP-CR-BR                PIC X(06).
       01  WS-GRP-O                    PIC 9(05).
       01  WS-GRP-V                    PIC 9(05).
       01  WS-GRP-R                    PIC 9(05).
       01  WS-MATCHED                  PIC 9(05).
       01  WS-LEFT                     PIC 9(05).

       01  WS-TOTALS.
           05  WS-LEGS                 PIC 9(07).
           05  WS-PAIRS-MATCHED        PIC 9(07).
           05  WS-PAIRS-IB             PIC 9(07).
           05  WS-REVERSED             PIC 9(07).
           05  WS-UNMATCHED-O          PIC 9(07).
           05  WS-UNMATCHED-R          PIC 9(07).
           05  WS-IB-NET               PIC S9(15)V99.
           05  WS-T-SAV                PIC S9(15)V99.
           05  WS-T-CUR                PIC S9(15)V99.
           05  WS-T-OD                 PIC S9(15)V99.
           05  WS-T-TD                 PIC S9(15)V99.
           05  WS-T-LN                 PIC S9(15)V99.

       01  WS-DISP-1                   PIC Z,ZZZ,ZZZ,ZZ9.99-.
       01  WS-DISP-2                   PIC Z,ZZZ,ZZZ,ZZ9.99-.
       01  WS-DISP-3                   PIC Z,ZZZ,ZZZ,ZZ9.99-.
       01  WS-DISP-4                   PIC Z,ZZZ,ZZZ,ZZ9.99-.
       01  WS-DISP-5                   PIC Z,ZZZ,ZZZ,ZZ9.99-.
       01  WS-DISP-CNT                 PIC ZZZ,ZZ9.
       01  WS-BR-NAME                  PIC X(20).
       01  WS-NOTE                     PIC X(30).

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'BUSINESS DATE (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           IF WS-IN-DATE = SPACES OR WS-IN-DATE IS NOT NUMERIC
               ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           ELSE
               MOVE WS-IN-DATE TO WS-RUN-DATE
           END-IF
           INITIALIZE WS-TOTALS WS-BR-TABLE
           OPEN INPUT ACCT-FILE
           IF NOT ACCT-FS-OK
               DISPLAY 'BRGLBAT: ACCOUNT MASTER OPEN FAILED '
                       WS-ACCT-FS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           OPEN INPUT BR-FILE
           STRING 'data/BRGL_' WS-RUN-DATE '.TXT'
                  DELIMITED BY SIZE INTO WS-RPT-NAME
           OPEN OUTPUT RPT-FILE

           PERFORM 1000-POSITIONS
           SORT SORT-FILE
               ON ASCENDING KEY SR-DR-ACCT SR-CR-ACCT SR-DATE
                                SR-AMOUNT SR-SIDE SR-SEQ
               INPUT PROCEDURE  IS 2000-SELECT-LEGS
               OUTPUT PROCEDURE IS 3000-MATCH-LEGS
           PERFORM 4000-REPORT

           CLOSE ACCT-FILE RPT-FILE
           IF WS-BR-FS = '00'
               CLOSE BR-FILE
           END-IF
           MOVE WS-RC TO RETURN-CODE
           GOBACK.

      *----------------------------------------------------------------*
      * A. POSITIONS FROM THE SUB-LEDGERS                              *
      *----------------------------------------------------------------*
       1000-POSITIONS.
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ ACCT-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N' AND NOT ACCT-CLOSED
                   MOVE ACCT-BRANCH-CODE TO WS-WANT-BRANCH
                   PERFORM 8000-BRANCH-SLOT
                   ADD 1 TO BT-ACCOUNTS(WS-B)
                   EVALUATE TRUE
                       WHEN ACCT-SAVINGS
                           ADD ACCT-BALANCE TO BT-SAVINGS(WS-B)
                       WHEN ACCT-BALANCE < ZERO
                           ADD ACCT-BALANCE TO BT-OVERDRAWN(WS-B)
                       WHEN OTHER
                           ADD ACCT-BALANCE TO BT-CURRENT(WS-B)
                   END-EVALUATE
               END-IF
           END-PERFORM

           OPEN INPUT LOAN-FILE
           IF WS-LOAN-FS = '00'
               MOVE 'N' TO WS-EOF-SW
               PERFORM UNTIL WS-EOF-SW = 'Y'
                   READ LOAN-FILE NEXT
                       AT END MOVE 'Y' TO WS-EOF-SW
                   END-READ
                   IF WS-EOF-SW = 'N'
                      AND (LOAN-ACTIVE OR LOAN-NPA)
                       MOVE LOAN-ACCT-NO TO ACCT-NO
                       PERFORM 8100-BRANCH-OF-ACCOUNT
                       PERFORM 8000-BRANCH-SLOT
                       ADD LOAN-OUTSTANDING TO BT-LOANS(WS-B)
                   END-IF
               END-PERFORM
               CLOSE LOAN-FILE
           END-IF

           OPEN INPUT DEP-FILE
           IF WS-DEP-FS = '00'
               MOVE 'N' TO WS-EOF-SW
               PERFORM UNTIL WS-EOF-SW = 'Y'
                   READ DEP-FILE NEXT
                       AT END MOVE 'Y' TO WS-EOF-SW
                   END-READ
                   IF WS-EOF-SW = 'N' AND DEP-ACTIVE
                       MOVE DEP-ACCT-NO TO ACCT-NO
                       PERFORM 8100-BRANCH-OF-ACCOUNT
                       PERFORM 8000-BRANCH-SLOT
                       IF DEP-FD
                           ADD DEP-AMOUNT TO BT-TERM-DEP(WS-B)
                       ELSE
                           ADD DEP-TOTAL-DEPOSITED TO BT-TERM-DEP(WS-B)
                       END-IF
                   END-IF
               END-PERFORM
               CLOSE DEP-FILE
           END-IF.

      *----------------------------------------------------------------*
      * B. TRANSFER LEGS OF THE DAY -> SORT                            *
      *----------------------------------------------------------------*
       2000-SELECT-LEGS.
           OPEN INPUT TRAN-FILE
           IF WS-TRAN-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ TRAN-FILE
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N'
                   ADD 1 TO WS-SEQ
                   IF TRAN-DATE = WS-RUN-DATE
                      AND (TRAN-XFER-OUT OR TRAN-XFER-IN)
                      AND TRAN-REF-ACCT > ZERO
                       PERFORM 2100-RELEASE-LEG
                   END-IF
               END-IF
           END-PERFORM
           CLOSE TRAN-FILE.

       2100-RELEASE-LEG.
           ADD 1 TO WS-LEGS
           MOVE TRAN-AMOUNT TO SR-AMOUNT
           MOVE TRAN-DATE   TO SR-DATE
           MOVE WS-SEQ      TO SR-SEQ
           EVALUATE TRUE
               WHEN TRAN-XFER-OUT
                   MOVE TRAN-ACCT-NO  TO SR-DR-ACCT
                   MOVE TRAN-REF-ACCT TO SR-CR-ACCT
                   MOVE 'O' TO SR-SIDE
      *        A REVERSAL CREDITS THE ORIGINAL DEBIT ACCOUNT AND
      *        CANCELS THAT DEBIT LEG (SAME FROM / TO ORDER)
               WHEN TRAN-NARRATION(1:27) = WS-REVERSAL-TEXT
                   MOVE TRAN-ACCT-NO  TO SR-DR-ACCT
                   MOVE TRAN-REF-ACCT TO SR-CR-ACCT
                   MOVE 'V' TO SR-SIDE
               WHEN OTHER
                   MOVE TRAN-REF-ACCT TO SR-DR-ACCT
                   MOVE TRAN-ACCT-NO  TO SR-CR-ACCT
                   MOVE 'R' TO SR-SIDE
           END-EVALUATE
           MOVE SR-DR-ACCT TO ACCT-NO
           PERFORM 8100-BRANCH-OF-ACCOUNT
           MOVE WS-WANT-BRANCH TO SR-DR-BRANCH
           MOVE SR-CR-ACCT TO ACCT-NO
           PERFORM 8100-BRANCH-OF-ACCOUNT
           MOVE WS-WANT-BRANCH TO SR-CR-BRANCH
           RELEASE SORT-REC.

      *----------------------------------------------------------------*
      * MATCH: WITHIN A GROUP, EACH DEBIT LEG IS PAIRED WITH A         *
      * REVERSAL (CANCELLED) OR A CREDIT LEG (A COMPLETED TRANSFER)    *
      *----------------------------------------------------------------*
       3000-MATCH-LEGS.
           MOVE 'N' TO WS-SORT-EOF
           MOVE LOW-VALUES TO WS-GRP-KEY
           MOVE ZERO TO WS-GRP-O WS-GRP-V WS-GRP-R
           PERFORM UNTIL WS-SORT-EOF = 'Y'
               RETURN SORT-FILE
                   AT END MOVE 'Y' TO WS-SORT-EOF
               END-RETURN
               IF WS-SORT-EOF = 'N'
                   MOVE SR-DR-ACCT TO NK-DR
                   MOVE SR-CR-ACCT TO NK-CR
                   MOVE SR-DATE    TO NK-DATE
                   MOVE SR-AMOUNT  TO NK-AMOUNT
                   IF WS-NEW-KEY NOT = WS-GRP-KEY
                       PERFORM 3100-CLOSE-GROUP
                       MOVE WS-NEW-KEY TO WS-GRP-KEY
                       MOVE SR-DR-BRANCH TO WS-GRP-DR-BR
                       MOVE SR-CR-BRANCH TO WS-GRP-CR-BR
                   END-IF
                   EVALUATE SR-SIDE
                       WHEN 'O' ADD 1 TO WS-GRP-O
                       WHEN 'V' ADD 1 TO WS-GRP-V
                       WHEN OTHER ADD 1 TO WS-GRP-R
                   END-EVALUATE
               END-IF
           END-PERFORM
           PERFORM 3100-CLOSE-GROUP.

       3100-CLOSE-GROUP.
           IF WS-GRP-O + WS-GRP-V + WS-GRP-R = ZERO
               EXIT PARAGRAPH
           END-IF
      *    REVERSALS CANCEL DEBIT LEGS FIRST
           IF WS-GRP-V > WS-GRP-O
               MOVE WS-GRP-O TO WS-MATCHED
           ELSE
               MOVE WS-GRP-V TO WS-MATCHED
           END-IF
           ADD WS-MATCHED TO WS-REVERSED
           SUBTRACT WS-MATCHED FROM WS-GRP-O
           SUBTRACT WS-MATCHED FROM WS-GRP-V
      *    A REVERSAL WITH NO DEBIT LEG LEFT IS AN ORPHAN CREDIT
           ADD WS-GRP-V TO WS-GRP-R
           IF WS-GRP-R > WS-GRP-O
               MOVE WS-GRP-O TO WS-MATCHED
           ELSE
               MOVE WS-GRP-R TO WS-MATCHED
           END-IF
           ADD WS-MATCHED TO WS-PAIRS-MATCHED
           IF WS-GRP-DR-BR NOT = WS-GRP-CR-BR AND WS-MATCHED > ZERO
               ADD WS-MATCHED TO WS-PAIRS-IB
               PERFORM 3200-IB-ENTRIES
           END-IF
      *    LEFTOVERS
           IF WS-GRP-O > WS-MATCHED
               COMPUTE WS-LEFT = WS-GRP-O - WS-MATCHED
               ADD WS-LEFT TO WS-UNMATCHED-O
               MOVE 'DEBIT LEG WITHOUT CREDIT' TO WS-NOTE
               PERFORM 3300-UNMATCHED-LINE
               IF WS-GRP-DR-BR NOT = WS-GRP-CR-BR
                   MOVE WS-GRP-DR-BR TO WS-WANT-BRANCH
                   PERFORM 8000-BRANCH-SLOT
                   COMPUTE BT-IB(WS-B) = BT-IB(WS-B)
                                       - GK-AMOUNT * WS-LEFT
               END-IF
           END-IF
           IF WS-GRP-R > WS-MATCHED
               COMPUTE WS-LEFT = WS-GRP-R - WS-MATCHED
               ADD WS-LEFT TO WS-UNMATCHED-R
               MOVE 'CREDIT LEG WITHOUT DEBIT' TO WS-NOTE
               PERFORM 3300-UNMATCHED-LINE
               IF WS-GRP-DR-BR NOT = WS-GRP-CR-BR
                   MOVE WS-GRP-CR-BR TO WS-WANT-BRANCH
                   PERFORM 8000-BRANCH-SLOT
                   COMPUTE BT-IB(WS-B) = BT-IB(WS-B)
                                       + GK-AMOUNT * WS-LEFT
               END-IF
           END-IF
           MOVE ZERO TO WS-GRP-O WS-GRP-V WS-GRP-R.

       3200-IB-ENTRIES.
           MOVE WS-GRP-DR-BR TO WS-WANT-BRANCH
           PERFORM 8000-BRANCH-SLOT
           MOVE WS-B TO WS-B-DR
           MOVE WS-GRP-CR-BR TO WS-WANT-BRANCH
           PERFORM 8000-BRANCH-SLOT
           MOVE WS-B TO WS-B-CR
           COMPUTE BT-IB(WS-B-DR) = BT-IB(WS-B-DR)
                                  - GK-AMOUNT * WS-MATCHED
           COMPUTE BT-IB(WS-B-CR) = BT-IB(WS-B-CR)
                                  + GK-AMOUNT * WS-MATCHED
           ADD WS-MATCHED TO BT-IB-DR-CNT(WS-B-DR)
                             BT-IB-CR-CNT(WS-B-CR)
           MOVE ZERO TO WS-P
           PERFORM VARYING WS-B FROM 1 BY 1 UNTIL WS-B > WS-PAIR-CNT
               IF PR-FROM(WS-B) = WS-GRP-DR-BR
                  AND PR-TO(WS-B) = WS-GRP-CR-BR
                   MOVE WS-B TO WS-P
               END-IF
           END-PERFORM
           IF WS-P = ZERO AND WS-PAIR-CNT < 500
               ADD 1 TO WS-PAIR-CNT
               MOVE WS-PAIR-CNT TO WS-P
               MOVE WS-GRP-DR-BR TO PR-FROM(WS-P)
               MOVE WS-GRP-CR-BR TO PR-TO(WS-P)
           END-IF
           IF WS-P > ZERO
               ADD WS-MATCHED TO PR-COUNT(WS-P)
               COMPUTE PR-AMOUNT(WS-P) = PR-AMOUNT(WS-P)
                                       + GK-AMOUNT * WS-MATCHED
           END-IF.

       3300-UNMATCHED-LINE.
           IF WS-RC < 4
               MOVE 4 TO WS-RC
           END-IF
           MOVE GK-AMOUNT TO WS-DISP-1
           MOVE SPACES TO RPT-LINE
           STRING 'UNMATCHED ' GK-DR ' ' WS-GRP-DR-BR ' -> ' GK-CR ' '
                  WS-GRP-CR-BR ' ' WS-DISP-1 ' x' WS-LEFT ' '
                  WS-NOTE DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           DISPLAY FUNCTION TRIM(RPT-LINE TRAILING).

      *----------------------------------------------------------------*
      * REPORT                                                         *
      *----------------------------------------------------------------*
       4000-REPORT.
           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE SPACES TO RPT-LINE
           STRING 'BRANCH ACCOUNTING  ' WS-RUN-DATE
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE 'A. BRANCH POSITION (SUB-LEDGERS)' TO RPT-LINE
           WRITE RPT-LINE
           MOVE 'BRANCH NAME                         SAVINGS'
             TO RPT-LINE
           MOVE '          CURRENT     TERM DEPOSITS  ' TO RPT-LINE(45:)
           MOVE '   ADVANCES (LOANS+OD)' TO RPT-LINE(81:)
           WRITE RPT-LINE
           PERFORM VARYING WS-B FROM 1 BY 1 UNTIL WS-B > WS-BR-CNT
               PERFORM 4100-POSITION-LINE
           END-PERFORM
           MOVE WS-T-SAV TO WS-DISP-1
           MOVE WS-T-CUR TO WS-DISP-2
           MOVE WS-T-TD  TO WS-DISP-3
           COMPUTE WS-DISP-4 = WS-T-LN - WS-T-OD
           MOVE SPACES TO RPT-LINE
           STRING 'BANK  ' '                      ' WS-DISP-1 WS-DISP-2
                  WS-DISP-3 WS-DISP-4 DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE

           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE 'B. INTER-BRANCH ACCOUNTS (+ OWED BY HO, - OWES HO)'
             TO RPT-LINE
           WRITE RPT-LINE
           MOVE 'BRANCH   IB POSITION        ORIGINATED  RESPONDED'
             TO RPT-LINE
           WRITE RPT-LINE
           PERFORM VARYING WS-B FROM 1 BY 1 UNTIL WS-B > WS-BR-CNT
               IF BT-IB(WS-B) NOT = ZERO OR BT-IB-DR-CNT(WS-B) > ZERO
                  OR BT-IB-CR-CNT(WS-B) > ZERO
                   ADD BT-IB(WS-B) TO WS-IB-NET
                   MOVE BT-IB(WS-B) TO WS-DISP-1
                   MOVE SPACES TO RPT-LINE
                   STRING BT-CODE(WS-B) ' ' WS-DISP-1 '    '
                          BT-IB-DR-CNT(WS-B) '    '
                          BT-IB-CR-CNT(WS-B)
                          DELIMITED BY SIZE INTO RPT-LINE
                   WRITE RPT-LINE
               END-IF
           END-PERFORM
           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE 'BRANCH PAIRS   FROM    TO       TRANSFERS        '
             TO RPT-LINE
           MOVE 'AMOUNT' TO RPT-LINE(51:)
           WRITE RPT-LINE
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > WS-PAIR-CNT
               MOVE PR-COUNT(WS-P)  TO WS-DISP-CNT
               MOVE PR-AMOUNT(WS-P) TO WS-DISP-1
               MOVE SPACES TO RPT-LINE
               STRING '               ' PR-FROM(WS-P) '  '
                      PR-TO(WS-P) '  ' WS-DISP-CNT ' ' WS-DISP-1
                      DELIMITED BY SIZE INTO RPT-LINE
               WRITE RPT-LINE
           END-PERFORM

           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-LEGS TO WS-DISP-CNT
           MOVE SPACES TO RPT-LINE
           STRING 'TRANSFER LEGS READ      : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO RPT-LINE
           PERFORM 4900-OUT
           MOVE WS-PAIRS-MATCHED TO WS-DISP-CNT
           MOVE SPACES TO RPT-LINE
           STRING 'TRANSFERS MATCHED       : ' WS-DISP-CNT
                  '  (INTER-BRANCH ' WS-PAIRS-IB ', REVERSED '
                  WS-REVERSED ')' DELIMITED BY SIZE INTO RPT-LINE
           PERFORM 4900-OUT
           COMPUTE WS-DISP-CNT = WS-UNMATCHED-O + WS-UNMATCHED-R
           MOVE SPACES TO RPT-LINE
           STRING 'UNMATCHED LEGS          : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO RPT-LINE
           PERFORM 4900-OUT
           MOVE WS-IB-NET TO WS-DISP-1
           MOVE SPACES TO RPT-LINE
           IF WS-IB-NET = ZERO
               STRING 'INTER-BRANCH NET        : ' WS-DISP-1
                      '  INTER-BRANCH RECONCILED'
                      DELIMITED BY SIZE INTO RPT-LINE
           ELSE
               STRING 'INTER-BRANCH NET        : ' WS-DISP-1
                      '  NOT ZERO - INVESTIGATE'
                      DELIMITED BY SIZE INTO RPT-LINE
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
           END-IF
           PERFORM 4900-OUT.

       4100-POSITION-LINE.
           MOVE SPACES TO WS-BR-NAME
           IF WS-BR-FS = '00'
               MOVE BT-CODE(WS-B) TO BR-CODE
               READ BR-FILE
                   INVALID KEY MOVE '(NOT IN BRANCH MASTER)'
                                 TO WS-BR-NAME
                   NOT INVALID KEY MOVE BR-NAME TO WS-BR-NAME
               END-READ
           END-IF
           ADD BT-SAVINGS(WS-B)   TO WS-T-SAV
           ADD BT-CURRENT(WS-B)   TO WS-T-CUR
           ADD BT-TERM-DEP(WS-B)  TO WS-T-TD
           ADD BT-LOANS(WS-B)     TO WS-T-LN
           ADD BT-OVERDRAWN(WS-B) TO WS-T-OD
           MOVE BT-SAVINGS(WS-B)  TO WS-DISP-1
           MOVE BT-CURRENT(WS-B)  TO WS-DISP-2
           MOVE BT-TERM-DEP(WS-B) TO WS-DISP-3
           COMPUTE WS-DISP-4 = BT-LOANS(WS-B) - BT-OVERDRAWN(WS-B)
           MOVE SPACES TO RPT-LINE
           STRING BT-CODE(WS-B) ' ' WS-BR-NAME ' ' WS-DISP-1 WS-DISP-2
                  WS-DISP-3 WS-DISP-4 DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE.

       4900-OUT.
           WRITE RPT-LINE
           DISPLAY FUNCTION TRIM(RPT-LINE TRAILING).

      *----------------------------------------------------------------*
      * COMMON                                                         *
      *----------------------------------------------------------------*
      *    WS-WANT-BRANCH -> WS-B (ADDED IF NEW; SLOT 100 = OVERFLOW)
       8000-BRANCH-SLOT.
           IF WS-WANT-BRANCH = SPACES
               MOVE '??????' TO WS-WANT-BRANCH
           END-IF
           MOVE ZERO TO WS-B
           PERFORM VARYING WS-P FROM 1 BY 1
                   UNTIL WS-P > WS-BR-CNT OR WS-B > ZERO
               IF BT-CODE(WS-P) = WS-WANT-BRANCH
                   MOVE WS-P TO WS-B
               END-IF
           END-PERFORM
           IF WS-B = ZERO
               IF WS-BR-CNT < 100
                   ADD 1 TO WS-BR-CNT
               END-IF
               MOVE WS-BR-CNT TO WS-B
               MOVE WS-WANT-BRANCH TO BT-CODE(WS-B)
           END-IF.

       8100-BRANCH-OF-ACCOUNT.
           READ ACCT-FILE
               INVALID KEY MOVE '??????' TO WS-WANT-BRANCH
               NOT INVALID KEY MOVE ACCT-BRANCH-CODE TO WS-WANT-BRANCH
           END-READ.
