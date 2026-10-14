       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTGEN.
      *================================================================*
      * PROGRAM  : TESTGEN                                             *
      * PURPOSE  : TEST DATA GENERATOR. BUILDS A REPEATABLE DATA SET   *
      *            FROM A SEED SO BATCHES AND REPORTS HAVE SOMETHING   *
      *            REALISTIC TO WORK ON:                               *
      *              PHASE 1 - N ACCOUNTS ACROSS 5 BRANCHES (80%       *
      *                        SAVINGS), OPENING DEPOSITS LOGGED       *
      *              PHASE 2 - T RANDOM DEPOSITS / WITHDRAWALS POSTED  *
      *                        THROUGH ACCTPOST (SO ALL RULES APPLY)   *
      *              PHASE 3 - ONE LOAN PER 5 ACCOUNTS; EVERY THIRD    *
      *                        LOAN IS BACK-DATED 4 MONTHS SO THE EOD  *
      *                        BATCH HAS OVERDUE / NPA CASES           *
      *            INPUT (ONE VALUE PER LINE, BLANK = DEFAULT):        *
      *              ACCOUNTS (50), TRANSACTIONS (300), SEED (12345)   *
      *            REFUSES TO RUN IF ACCOUNTS ALREADY EXIST, SO IT     *
      *            CAN NEVER POLLUTE A REAL DATA SET.                  *
      * FILES    : ACCTMAST, LOANMAST (OUTPUT), TRANLOG (EXTEND)       *
      * CALLS    : ACCTPOST, DATEUTIL                                  *
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
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LOAN-NO
               FILE STATUS IS WS-LOAN-FS.
           SELECT TRAN-FILE ASSIGN TO 'data/TRANLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRAN-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
           COPY ACCTREC.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  TRAN-FILE.
           COPY TRANREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY DATEPARM.
           COPY GLPARM.
           COPY GLCODES.

       01  WS-LOAN-FS                  PIC X(02).
           88  LOAN-FS-OK                        VALUE '00'.

       01  WS-IN-VALUE                 PIC X(10).
       01  WS-NUM-ACCOUNTS             PIC 9(05) VALUE 50.
       01  WS-NUM-TRANS                PIC 9(06) VALUE 300.
       01  WS-SEED                     PIC 9(09) VALUE 12345.
       01  WS-RANDOM                   PIC V9(09).
       01  WS-LCG-STATE                PIC 9(18).
       01  WS-PICK                     PIC 9(05).
       01  WS-I                        PIC 9(06).
       01  WS-FIRST-ACCT               PIC 9(10) VALUE 1000000001.
       01  WS-LAST-ACCT                PIC 9(10).
       01  WS-AMOUNT                   PIC 9(09)V99.
       01  WS-PHONE-NUM                PIC 9(10).
       01  WS-TRAN-SEQ                 PIC 9(04) VALUE 7000.
       01  WS-LOAN-SEQ                 PIC 9(10) VALUE 5000000000.

      * NAME AND BRANCH TABLES
       01  WS-FIRST-NAMES.
           05  FILLER PIC X(10) VALUE 'AARAV'.
           05  FILLER PIC X(10) VALUE 'DIYA'.
           05  FILLER PIC X(10) VALUE 'KABIR'.
           05  FILLER PIC X(10) VALUE 'MEERA'.
           05  FILLER PIC X(10) VALUE 'ARJUN'.
           05  FILLER PIC X(10) VALUE 'ISHA'.
           05  FILLER PIC X(10) VALUE 'ROHAN'.
           05  FILLER PIC X(10) VALUE 'ANANYA'.
           05  FILLER PIC X(10) VALUE 'VIKRAM'.
           05  FILLER PIC X(10) VALUE 'SNEHA'.
       01  WS-FIRST-R REDEFINES WS-FIRST-NAMES.
           05  WS-FIRST PIC X(10) OCCURS 10 TIMES.
       01  WS-LAST-NAMES.
           05  FILLER PIC X(10) VALUE 'SHARMA'.
           05  FILLER PIC X(10) VALUE 'IYER'.
           05  FILLER PIC X(10) VALUE 'KHAN'.
           05  FILLER PIC X(10) VALUE 'REDDY'.
           05  FILLER PIC X(10) VALUE 'SINGH'.
           05  FILLER PIC X(10) VALUE 'DAS'.
           05  FILLER PIC X(10) VALUE 'PATEL'.
           05  FILLER PIC X(10) VALUE 'NAIR'.
       01  WS-LAST-R REDEFINES WS-LAST-NAMES.
           05  WS-LAST PIC X(10) OCCURS 8 TIMES.
       01  WS-BRANCHES.
           05  FILLER PIC X(06) VALUE 'DEL001'.
           05  FILLER PIC X(06) VALUE 'MUM002'.
           05  FILLER PIC X(06) VALUE 'KOL003'.
           05  FILLER PIC X(06) VALUE 'CHN004'.
           05  FILLER PIC X(06) VALUE 'BLR005'.
       01  WS-BRANCH-R REDEFINES WS-BRANCHES.
           05  WS-BRANCH PIC X(06) OCCURS 5 TIMES.

      * LOAN WORK
       01  WS-LOAN-TYPE-CODES          PIC X(03) VALUE 'HPV'.
       01  WS-LOAN-RATES.
           05  FILLER PIC 9(02)V9(04) VALUE 08.5000.
           05  FILLER PIC 9(02)V9(04) VALUE 13.5000.
           05  FILLER PIC 9(02)V9(04) VALUE 09.5000.
       01  WS-LOAN-RATES-R REDEFINES WS-LOAN-RATES.
           05  WS-LOAN-RATE PIC 9(02)V9(04) OCCURS 3 TIMES.
       01  WS-LX                       PIC 9(01).
       01  WS-MONTHLY-RATE             COMP-2.
       01  WS-POWER                    COMP-2.
       01  WS-DISB-DATE                PIC 9(08).

       01  WS-STATS.
           05  WS-ACCTS-MADE           PIC 9(07).
           05  WS-TRANS-OK             PIC 9(07).
           05  WS-TRANS-REFUSED        PIC 9(07).
           05  WS-LOANS-MADE           PIC 9(07).

       PROCEDURE DIVISION.
       0000-MAIN.
           INITIALIZE WS-STATS
           PERFORM 0100-READ-PARAMETERS
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE WS-SEED TO WS-LCG-STATE

           PERFORM 1000-CREATE-ACCOUNTS
           IF RETURN-CODE NOT = ZERO
               GOBACK
           END-IF
           PERFORM 2000-RANDOM-TRANSACTIONS
           PERFORM 3000-CREATE-LOANS

           DISPLAY 'TESTGEN COMPLETE'
           DISPLAY '  ACCOUNTS CREATED      : ' WS-ACCTS-MADE
           DISPLAY '  TRANSACTIONS POSTED   : ' WS-TRANS-OK
           DISPLAY '  TRANSACTIONS REFUSED  : ' WS-TRANS-REFUSED
                   ' (INSUFFICIENT FUNDS - EXPECTED)'
           DISPLAY '  LOANS CREATED         : ' WS-LOANS-MADE
           MOVE ZERO TO RETURN-CODE
           GOBACK.

       0100-READ-PARAMETERS.
           DISPLAY 'NUMBER OF ACCOUNTS (BLANK=50)   : '
                   WITH NO ADVANCING
           ACCEPT WS-IN-VALUE
           IF FUNCTION TEST-NUMVAL(WS-IN-VALUE) = ZERO
              AND WS-IN-VALUE NOT = SPACES
               COMPUTE WS-NUM-ACCOUNTS = FUNCTION NUMVAL(WS-IN-VALUE)
           END-IF
           DISPLAY 'NUMBER OF TRANSACTIONS (300)    : '
                   WITH NO ADVANCING
           ACCEPT WS-IN-VALUE
           IF FUNCTION TEST-NUMVAL(WS-IN-VALUE) = ZERO
              AND WS-IN-VALUE NOT = SPACES
               COMPUTE WS-NUM-TRANS = FUNCTION NUMVAL(WS-IN-VALUE)
           END-IF
           DISPLAY 'RANDOM SEED (12345)             : '
                   WITH NO ADVANCING
           ACCEPT WS-IN-VALUE
           IF FUNCTION TEST-NUMVAL(WS-IN-VALUE) = ZERO
              AND WS-IN-VALUE NOT = SPACES
               COMPUTE WS-SEED = FUNCTION NUMVAL(WS-IN-VALUE)
           END-IF
           IF WS-NUM-ACCOUNTS < 1
               MOVE 50 TO WS-NUM-ACCOUNTS
           END-IF.

      *================================================================*
      * PHASE 1 - ACCOUNTS                                             *
      *================================================================*
       1000-CREATE-ACCOUNTS.
           OPEN I-O ACCT-FILE
           IF ACCT-FS-OK
               MOVE ZERO TO ACCT-NO
               START ACCT-FILE KEY IS NOT LESS THAN ACCT-NO
                   NOT INVALID KEY
                       DISPLAY 'TESTGEN: ACCOUNTS ALREADY EXIST - '
                               'RUN ONLY ON AN EMPTY DATA SET.'
                       CLOSE ACCT-FILE
                       MOVE 8 TO RETURN-CODE
                       EXIT PARAGRAPH
               END-START
           ELSE
               OPEN OUTPUT ACCT-FILE
               CLOSE ACCT-FILE
               OPEN I-O ACCT-FILE
           END-IF
           OPEN EXTEND TRAN-FILE
           IF TRAN-FS-NOFILE
               OPEN OUTPUT TRAN-FILE
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-NUM-ACCOUNTS
               PERFORM 1100-BUILD-ACCOUNT
           END-PERFORM
           CLOSE ACCT-FILE TRAN-FILE
           COMPUTE WS-LAST-ACCT = WS-FIRST-ACCT + WS-NUM-ACCOUNTS - 1
           MOVE ZERO TO RETURN-CODE.

       1100-BUILD-ACCOUNT.
           INITIALIZE ACCT-RECORD
           COMPUTE ACCT-NO = WS-FIRST-ACCT + WS-I - 1
           PERFORM 9000-NEXT-RANDOM
           COMPUTE WS-PICK = WS-RANDOM * 10 + 1
           MOVE WS-FIRST(WS-PICK) TO ACCT-HOLDER-NAME
           PERFORM 9000-NEXT-RANDOM
           COMPUTE WS-PICK = WS-RANDOM * 8 + 1
           STRING FUNCTION TRIM(ACCT-HOLDER-NAME) ' ' WS-LAST(WS-PICK)
                  DELIMITED BY SIZE INTO ACCT-HOLDER-NAME
           END-STRING
           PERFORM 9000-NEXT-RANDOM
           COMPUTE WS-PICK = WS-RANDOM * 5 + 1
           MOVE WS-BRANCH(WS-PICK) TO ACCT-BRANCH-CODE
           PERFORM 9000-NEXT-RANDOM
           IF WS-RANDOM < 0.8
               MOVE 'S'     TO ACCT-TYPE
               MOVE 1000.00 TO ACCT-MIN-BALANCE
               MOVE 3.5     TO ACCT-INT-RATE
           ELSE
               MOVE 'C'     TO ACCT-TYPE
           END-IF
      *    OPENING BALANCE 5,000 - 205,000, WHOLE RUPEES
           PERFORM 9000-NEXT-RANDOM
           COMPUTE WS-AMOUNT = 5000 + WS-RANDOM * 200000
           COMPUTE WS-AMOUNT = FUNCTION INTEGER-PART(WS-AMOUNT)
           MOVE WS-AMOUNT TO ACCT-BALANCE
           SET ACCT-ACTIVE TO TRUE
           COMPUTE WS-PHONE-NUM = 9000000000 + ACCT-NO - WS-FIRST-ACCT
           MOVE WS-PHONE-NUM TO ACCT-PHONE
           MOVE WS-CURR-DATE TO ACCT-OPEN-DATE ACCT-LAST-TXN-DATE
                                ACCT-LAST-INT-DATE
           WRITE ACCT-RECORD
               INVALID KEY
                   DISPLAY 'TESTGEN: WRITE FAILED ' ACCT-NO
                   EXIT PARAGRAPH
           END-WRITE
           ADD 1 TO WS-ACCTS-MADE
           ADD 1 TO WS-TRAN-SEQ
           COMPUTE TRAN-ID = WS-CURR-DATE * 10000 + WS-TRAN-SEQ
           MOVE ACCT-NO      TO TRAN-ACCT-NO
           MOVE WS-CURR-DATE TO TRAN-DATE
           MOVE WS-CURR-TIME TO TRAN-TIME
           MOVE 'DP'         TO TRAN-TYPE
           MOVE WS-AMOUNT    TO TRAN-AMOUNT TRAN-BAL-AFTER
           MOVE ZERO         TO TRAN-REF-ACCT
           MOVE 'ACCOUNT OPENING DEPOSIT' TO TRAN-NARRATION
           WRITE TRAN-RECORD
           MOVE GLC-OPENING-CONTROL TO GP-DR-GL
           IF ACCT-SAVINGS
               MOVE GLC-SAVINGS-DEP TO GP-CR-GL
           ELSE
               MOVE GLC-CURRENT-DEP TO GP-CR-GL
           END-IF
           MOVE WS-AMOUNT    TO GP-AMOUNT
           MOVE ACCT-NO      TO GP-REF
           MOVE 'TEST DATA OPENING BALANCE' TO GP-NARRATION
           CALL 'GLPOST' USING GL-PARMS.

      *================================================================*
      * PHASE 2 - TRANSACTIONS (60% DEPOSITS, 40% WITHDRAWALS)         *
      *================================================================*
       2000-RANDOM-TRANSACTIONS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-TRANS
               INITIALIZE POST-PARMS
               PERFORM 9000-NEXT-RANDOM
               COMPUTE PP-ACCT-NO =
                       WS-FIRST-ACCT + WS-RANDOM * WS-NUM-ACCOUNTS
               PERFORM 9000-NEXT-RANDOM
               COMPUTE PP-AMOUNT = 100 + WS-RANDOM * 25000
               COMPUTE PP-AMOUNT = FUNCTION INTEGER-PART(PP-AMOUNT)
               PERFORM 9000-NEXT-RANDOM
               IF WS-RANDOM < 0.6
                   SET PP-CREDIT TO TRUE
                   MOVE 'DP' TO PP-TRAN-TYPE
                   MOVE 'TESTGEN DEPOSIT' TO PP-NARRATION
               ELSE
                   SET PP-DEBIT TO TRUE
                   MOVE 'WD' TO PP-TRAN-TYPE
                   MOVE 'TESTGEN WITHDRAWAL' TO PP-NARRATION
               END-IF
               CALL 'ACCTPOST' USING POST-PARMS
               IF PP-OK
                   ADD 1 TO WS-TRANS-OK
               ELSE
                   ADD 1 TO WS-TRANS-REFUSED
               END-IF
           END-PERFORM.

      *================================================================*
      * PHASE 3 - LOANS                                                *
      *================================================================*
       3000-CREATE-LOANS.
           OPEN I-O LOAN-FILE
           IF NOT LOAN-FS-OK
               OPEN OUTPUT LOAN-FILE
               CLOSE LOAN-FILE
               OPEN I-O LOAN-FILE
           END-IF
           MOVE ZERO TO LOAN-NO
           START LOAN-FILE KEY IS NOT LESS THAN LOAN-NO
               NOT INVALID KEY
                   DISPLAY 'TESTGEN: LOANS ALREADY EXIST - SKIPPED.'
                   CLOSE LOAN-FILE
                   EXIT PARAGRAPH
           END-START
           PERFORM VARYING WS-I FROM 1 BY 5
                   UNTIL WS-I > WS-NUM-ACCOUNTS
               PERFORM 3100-BUILD-LOAN
           END-PERFORM
           CLOSE LOAN-FILE.

       3100-BUILD-LOAN.
           INITIALIZE LOAN-RECORD
           ADD 1 TO WS-LOAN-SEQ
           MOVE WS-LOAN-SEQ TO LOAN-NO
           COMPUTE LOAN-ACCT-NO = WS-FIRST-ACCT + WS-I - 1
           PERFORM 9000-NEXT-RANDOM
           COMPUTE WS-LX = WS-RANDOM * 3 + 1
           MOVE WS-LOAN-TYPE-CODES(WS-LX:1) TO LOAN-TYPE
           MOVE WS-LOAN-RATE(WS-LX)          TO LOAN-RATE
           PERFORM 9000-NEXT-RANDOM
           COMPUTE LOAN-PRINCIPAL =
                   FUNCTION INTEGER-PART(50000 + WS-RANDOM * 950000)
           MOVE LOAN-PRINCIPAL TO LOAN-OUTSTANDING
           MOVE 36 TO LOAN-TENURE
           COMPUTE WS-MONTHLY-RATE = LOAN-RATE / 1200
           COMPUTE WS-POWER = (1 + WS-MONTHLY-RATE) ** LOAN-TENURE
           COMPUTE LOAN-EMI ROUNDED = LOAN-PRINCIPAL * WS-MONTHLY-RATE
                   * WS-POWER / (WS-POWER - 1)
           SET LOAN-ACTIVE TO TRUE
      *    EVERY THIRD LOAN STARTED 4 MONTHS AGO AND WAS NEVER PAID
           IF FUNCTION MOD(WS-LOANS-MADE, 3) = 0
               MOVE -4 TO DU-NUMBER
           ELSE
               MOVE ZERO TO DU-NUMBER
           END-IF
           SET DU-ADD-MONTHS TO TRUE
           MOVE WS-CURR-DATE TO DU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO WS-DISB-DATE LOAN-DISB-DATE
           MOVE WS-DISB-DATE TO DU-DATE-1
           MOVE 1 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO LOAN-NEXT-DUE-DATE

           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE LOAN-ACCT-NO   TO PP-ACCT-NO
           MOVE LOAN-PRINCIPAL TO PP-AMOUNT
           MOVE 'LD'           TO PP-TRAN-TYPE
           MOVE LOAN-NO        TO PP-REF-ACCT
           MOVE 'LOAN DISBURSAL' TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF PP-OK
               WRITE LOAN-RECORD
               ADD 1 TO WS-LOANS-MADE
           END-IF.

      *----------------------------------------------------------------*
      * RANDOM NUMBER IN [0, 1)                                        *
      *----------------------------------------------------------------*
       9000-NEXT-RANDOM.
      *    OWN LINEAR CONGRUENTIAL GENERATOR (THE C LIBRARY ONE IS
      *    SHARED WITH THE INDEXED FILE HANDLER, WHICH RESEEDS IT, SO
      *    FUNCTION RANDOM WOULD NOT GIVE A REPEATABLE DATA SET)
           COMPUTE WS-LCG-STATE = FUNCTION MOD(
                   WS-LCG-STATE * 1103515245 + 12345, 2147483648)
           COMPUTE WS-RANDOM = WS-LCG-STATE / 2147483648.
