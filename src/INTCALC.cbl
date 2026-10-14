       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTCALC.
      *================================================================*
      * PROGRAM  : INTCALC                                             *
      * PURPOSE  : MONTHLY INTEREST BATCH, ON DAILY PRODUCTS.          *
      *            SAVINGS : CREDIT INTEREST = SUM OF END-OF-DAY       *
      *                      BALANCES (ACCUMULATED BY EODBATCH)        *
      *                      * RATE / 36500                            *
      *            CURRENT : DEBIT OVERDRAFT INTEREST = SUM OF DAILY   *
      *                      OVERDRAWN BALANCES * OD RATE / 36500      *
      *            THE PRODUCTS ARE RESET AFTER EACH RUN. IF THE EOD   *
      *            BATCH HAS NOT BUILT A PRODUCT YET (NEW SYSTEM),     *
      *            SAVINGS FALL BACK TO BALANCE * RATE / 1200.         *
      *            ONE RUN PER ACCOUNT PER MONTH (LAST-INT-DATE).      *
      *            INTEREST IS A BANK-INITIATED ENTRY: IT DOES NOT     *
      *            RESET THE DORMANCY CLOCK.                           *
      * GL       : SAVINGS INTEREST  DR 5001 CR 2101                   *
      *            OD INTEREST       DR 2102 CR 4002                   *
      * FILES    : ACCTMAST (I-O), TRANLOG (EXTEND)                    *
      * CALLS    : GLPOST                                              *
      * RETURN   : 0 = OK, 4 = NOTHING TO PROCESS, 12 = FILE ERROR     *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION (SIMPLE MONTHLY ON BALANCE)         *
      * 2026-10-14 DAILY-PRODUCT METHOD, OVERDRAFT INTEREST, GL        *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE ASSIGN TO 'data/ACCTMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-ACCT-FS.
           SELECT TRAN-FILE ASSIGN TO 'data/TRANLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRAN-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
           COPY ACCTREC.
       FD  TRAN-FILE.
           COPY TRANREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY GLPARM.
           COPY GLCODES.
           COPY DATEPARM.
       01  WS-IN-RUN-DATE              PIC X(08).

       01  WS-EOF-SW                   PIC X(01) VALUE 'N'.
           88  END-OF-ACCOUNTS                   VALUE 'Y'.

       01  WS-LAST-INT-YYYYMM          PIC 9(06).
       01  WS-INTEREST                 PIC 9(11)V99.
       01  WS-TRAN-SEQ                 PIC 9(04) VALUE 5000.
       01  WS-CHANGED-SW               PIC X(01).
           88  RECORD-CHANGED                    VALUE 'Y'.

       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC 9(07).
           05  WS-CREDIT-CNT           PIC 9(07).
           05  WS-OD-CNT               PIC 9(07).
           05  WS-SKIP-CNT             PIC 9(07).
           05  WS-ERROR-CNT            PIC 9(07).
           05  WS-TOTAL-INT            PIC 9(13)V99.
           05  WS-TOTAL-OD-INT         PIC 9(13)V99.

       01  WS-DISP-CNT                 PIC Z,ZZZ,ZZ9.
       01  WS-DISP-AMT                 PIC -Z,ZZZ,ZZZ,ZZZ,ZZ9.99.

       LINKAGE SECTION.
       01  LS-RETURN-CODE              PIC S9(04) COMP.

       PROCEDURE DIVISION USING LS-RETURN-CODE.
       0000-MAIN.
           MOVE ZERO TO LS-RETURN-CODE
           INITIALIZE WS-COUNTERS
           MOVE 'N' TO WS-EOF-SW
           PERFORM 1000-INITIALIZE
           IF LS-RETURN-CODE = ZERO
               PERFORM 2000-PROCESS-ACCOUNT UNTIL END-OF-ACCOUNTS
               PERFORM 9000-SUMMARY
               CLOSE ACCT-FILE TRAN-FILE
               IF WS-CREDIT-CNT = ZERO AND WS-OD-CNT = ZERO
                   MOVE 4 TO LS-RETURN-CODE
               END-IF
           END-IF
           GOBACK.

       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
      *    AN EXPLICIT RUN DATE LETS MONTH-END BE RE-RUN OR TESTED
           DISPLAY 'INTEREST RUN DATE YYYYMMDD (BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-RUN-DATE
           IF WS-IN-RUN-DATE IS NUMERIC
               MOVE WS-IN-RUN-DATE TO DU-DATE-1
               SET DU-VALIDATE TO TRUE
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DU-OK
                   MOVE WS-IN-RUN-DATE TO WS-CURR-DATE
               END-IF
           END-IF
           OPEN I-O ACCT-FILE
           IF NOT ACCT-FS-OK
               DISPLAY 'INTCALC: ACCOUNT MASTER OPEN FAILED FS='
                       WS-ACCT-FS
               MOVE 12 TO LS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           OPEN EXTEND TRAN-FILE
           IF TRAN-FS-NOFILE
               OPEN OUTPUT TRAN-FILE
           END-IF
           IF NOT TRAN-FS-OK
               DISPLAY 'INTCALC: TRAN LOG OPEN FAILED FS=' WS-TRAN-FS
               CLOSE ACCT-FILE
               MOVE 12 TO LS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'INTCALC: INTEREST RUN FOR PERIOD ' WS-CURR-YYYYMM
           PERFORM 8000-READ-NEXT-ACCOUNT.

       2000-PROCESS-ACCOUNT.
           ADD 1 TO WS-READ-CNT
           MOVE 'N' TO WS-CHANGED-SW
           MOVE ACCT-LAST-INT-DATE(1:6) TO WS-LAST-INT-YYYYMM
           IF (ACCT-ACTIVE OR ACCT-DORMANT)
              AND WS-LAST-INT-YYYYMM < WS-CURR-YYYYMM
               IF ACCT-SAVINGS
                   PERFORM 2100-SAVINGS-INTEREST
               ELSE
                   PERFORM 2200-OVERDRAFT-INTEREST
               END-IF
           END-IF
           IF RECORD-CHANGED
               MOVE WS-CURR-DATE TO ACCT-LAST-INT-DATE
               REWRITE ACCT-RECORD
                   INVALID KEY
                       DISPLAY 'INTCALC: REWRITE FAILED ACCT=' ACCT-NO
                       ADD 1 TO WS-ERROR-CNT
               END-REWRITE
           ELSE
               ADD 1 TO WS-SKIP-CNT
           END-IF
           PERFORM 8000-READ-NEXT-ACCOUNT.

       2100-SAVINGS-INTEREST.
           IF ACCT-PRODUCT-DAYS > ZERO
               COMPUTE WS-INTEREST ROUNDED =
                       ACCT-INT-PRODUCT * ACCT-INT-RATE / 36500
           ELSE
               IF ACCT-BALANCE > ZERO
                   COMPUTE WS-INTEREST ROUNDED =
                           ACCT-BALANCE * ACCT-INT-RATE / 1200
               ELSE
                   MOVE ZERO TO WS-INTEREST
               END-IF
           END-IF
           MOVE ZERO TO ACCT-INT-PRODUCT ACCT-PRODUCT-DAYS
           MOVE 'Y' TO WS-CHANGED-SW
           IF WS-INTEREST = ZERO
               EXIT PARAGRAPH
           END-IF
           ADD WS-INTEREST TO ACCT-BALANCE
           ADD 1 TO WS-CREDIT-CNT
           ADD WS-INTEREST TO WS-TOTAL-INT
           MOVE 'IN' TO TRAN-TYPE
           MOVE 'SAVINGS INTEREST CREDIT' TO TRAN-NARRATION
           PERFORM 3000-LOG-TRANSACTION
           MOVE GLC-SAV-INT-EXPENSE TO GP-DR-GL
           MOVE GLC-SAVINGS-DEP     TO GP-CR-GL
           MOVE 'SAVINGS INTEREST' TO GP-NARRATION
           PERFORM 3100-POST-GL.

       2200-OVERDRAFT-INTEREST.
           IF ACCT-OD-PRODUCT = ZERO
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-INTEREST ROUNDED =
                   ACCT-OD-PRODUCT * ACCT-OD-RATE / 36500
           MOVE ZERO TO ACCT-OD-PRODUCT ACCT-PRODUCT-DAYS
           MOVE 'Y' TO WS-CHANGED-SW
           IF WS-INTEREST = ZERO
               EXIT PARAGRAPH
           END-IF
      *    OD INTEREST IS ALWAYS CHARGED, EVEN IF IT TAKES THE ACCOUNT
      *    PAST ITS LIMIT - THE EXCESS SHOWS UP AS AN OVER-LIMIT
           SUBTRACT WS-INTEREST FROM ACCT-BALANCE
           ADD 1 TO WS-OD-CNT
           ADD WS-INTEREST TO WS-TOTAL-OD-INT
           MOVE 'OI' TO TRAN-TYPE
           MOVE 'OVERDRAFT INTEREST' TO TRAN-NARRATION
           PERFORM 3000-LOG-TRANSACTION
           MOVE GLC-CURRENT-DEP   TO GP-DR-GL
           MOVE GLC-OD-INT-INCOME TO GP-CR-GL
           MOVE 'OVERDRAFT INTEREST' TO GP-NARRATION
           PERFORM 3100-POST-GL.

      *    CALLER SETS TRAN-TYPE AND TRAN-NARRATION
       3000-LOG-TRANSACTION.
           ADD 1 TO WS-TRAN-SEQ
           COMPUTE TRAN-ID = WS-CURR-DATE * 10000 + WS-TRAN-SEQ
           MOVE ACCT-NO       TO TRAN-ACCT-NO
           MOVE WS-CURR-DATE  TO TRAN-DATE
           MOVE WS-CURR-TIME  TO TRAN-TIME
           MOVE WS-INTEREST   TO TRAN-AMOUNT
           MOVE ACCT-BALANCE  TO TRAN-BAL-AFTER
           MOVE ZERO          TO TRAN-REF-ACCT
           WRITE TRAN-RECORD
           IF NOT TRAN-FS-OK
               DISPLAY 'INTCALC: TRAN LOG WRITE FAILED FS='
                       WS-TRAN-FS
               ADD 1 TO WS-ERROR-CNT
           END-IF.

      *    CALLER SETS GP-DR-GL, GP-CR-GL AND GP-NARRATION
       3100-POST-GL.
           MOVE WS-INTEREST TO GP-AMOUNT
           MOVE ACCT-NO     TO GP-REF
           CALL 'GLPOST' USING GL-PARMS
           IF NOT GP-OK
               DISPLAY 'INTCALC: GL ENTRY FAILED FOR ' ACCT-NO
               ADD 1 TO WS-ERROR-CNT
           END-IF.

       8000-READ-NEXT-ACCOUNT.
           READ ACCT-FILE NEXT RECORD
               AT END
                   SET END-OF-ACCOUNTS TO TRUE
           END-READ.

       9000-SUMMARY.
           DISPLAY '------------ INTEREST RUN SUMMARY ------------'
           MOVE WS-READ-CNT   TO WS-DISP-CNT
           DISPLAY 'ACCOUNTS READ       : ' WS-DISP-CNT
           MOVE WS-CREDIT-CNT TO WS-DISP-CNT
           DISPLAY 'SAVINGS CREDITED    : ' WS-DISP-CNT
           MOVE WS-TOTAL-INT  TO WS-DISP-AMT
           DISPLAY 'SAVINGS INTEREST    : ' WS-DISP-AMT
           MOVE WS-OD-CNT     TO WS-DISP-CNT
           DISPLAY 'OVERDRAFTS CHARGED  : ' WS-DISP-CNT
           MOVE WS-TOTAL-OD-INT TO WS-DISP-AMT
           DISPLAY 'OVERDRAFT INTEREST  : ' WS-DISP-AMT
           MOVE WS-SKIP-CNT   TO WS-DISP-CNT
           DISPLAY 'ACCOUNTS SKIPPED    : ' WS-DISP-CNT
           MOVE WS-ERROR-CNT  TO WS-DISP-CNT
           DISPLAY 'ERRORS              : ' WS-DISP-CNT
           DISPLAY '----------------------------------------------'.
