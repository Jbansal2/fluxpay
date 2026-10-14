       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHGBATCH.
      *================================================================*
      * PROGRAM  : CHGBATCH                                            *
      * PURPOSE  : PERIODIC SERVICE CHARGES WITH 18% GST.              *
      *              MBP - MINIMUM BALANCE PENALTY (MONTHLY, SAVINGS   *
      *                    BELOW MINIMUM BALANCE ON THE RUN DATE)      *
      *              SMS - SMS ALERT CHARGES (QUARTER-END MONTHS ONLY) *
      *              DCF - DEBIT CARD ANNUAL FEE (IN THE CARD'S ISSUE  *
      *                    MONTH, FROM THE SECOND YEAR ONWARDS)        *
      *              LKR - LOCKER RENT RENEWAL (WHEN PAID-UNTIL FALLS  *
      *                    IN OR BEFORE THE RUN MONTH; THE LOCKER IS   *
      *                    THEN EXTENDED BY 12 MONTHS)                 *
      *            TWO PHASES SO THE ACCOUNT MASTER IS NEVER OPEN      *
      *            TWICE: EXTRACT CANDIDATES TO A WORK FILE, THEN      *
      *            POST EACH ONE THROUGH ACCTPOST.                     *
      *            RE-RUNNABLE: THE CHARGES LEDGER KEY (ACCOUNT +      *
      *            CODE + PERIOD) SKIPS WHAT IS ALREADY POSTED AND     *
      *            RETRIES WHAT COULD NOT BE RECOVERED LAST TIME.      *
      * FILES    : ACCTMAST, CARDMAST (INPUT), CHGLOG (I-O),           *
      *            CHGWORK (WORK), CHGRPT (REPORT)                     *
      * CALLS    : ACCTPOST                                            *
      * RETURN   : 0 = CLEAN, 4 = SOME CHARGES NOT RECOVERED, 12 = ERR *
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
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-ACCT-FS.
           SELECT CARD-FILE ASSIGN TO 'data/CARDMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CARD-NO
               FILE STATUS IS WS-CARD-FS.
           SELECT CHG-FILE ASSIGN TO 'data/CHGLOG.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CHG-KEY
               FILE STATUS IS WS-CHG-FS.
           SELECT LOCK-FILE ASSIGN TO 'data/LOCKMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LOCK-NO
               FILE STATUS IS WS-LOCK-FS.
           SELECT DONE-FILE ASSIGN TO 'data/CHGLKR.TMP'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-DONE-FS.
           SELECT WORK-FILE ASSIGN TO 'data/CHGWORK.TMP'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-WORK-FS.
           SELECT RPT-FILE ASSIGN TO WS-RPT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
           COPY ACCTREC.
       FD  CARD-FILE.
           COPY CARDREC.
       FD  CHG-FILE.
           COPY CHGREC.
       FD  LOCK-FILE.
           COPY LOCKREC.
       FD  DONE-FILE.
       01  DONE-RECORD.
           05  DONE-LOCK-NO            PIC 9(05).
       FD  WORK-FILE.
       01  WORK-RECORD.
           05  WK-ACCT-NO              PIC 9(10).
           05  WK-CODE                 PIC X(03).
           05  WK-BASE-AMT             PIC 9(07)V99.
           05  WK-REF                  PIC X(16).
           05  WK-INCOME-GL            PIC 9(04).
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(80).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY DATEPARM.
           COPY GLPARM.
           COPY GLCODES.

       01  WS-CARD-FS                  PIC X(02).
           88  CARD-FS-OK                        VALUE '00'.
       01  WS-CHG-FS                   PIC X(02).
           88  CHG-FS-OK                         VALUE '00'.
           88  CHG-FS-NOFILE                     VALUE '35'.
           88  CHG-FS-NOTFOUND                   VALUE '23'.
       01  WS-WORK-FS                  PIC X(02).
       01  WS-LOCK-FS                  PIC X(02).
           88  LOCK-FS-OK                        VALUE '00'.
       01  WS-DONE-FS                  PIC X(02).
       01  WS-LOCKERS-RENEWED          PIC 9(05) VALUE ZERO.
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-FILENAME             PIC X(40).

       01  WS-IN-RUN-DATE              PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-RUN-DATE-R REDEFINES WS-RUN-DATE.
           05  WS-RUN-YYYYMM           PIC 9(06).
           05  WS-RUN-YYYYMM-R REDEFINES WS-RUN-YYYYMM.
               10  WS-RUN-YYYY         PIC 9(04).
               10  WS-RUN-MM           PIC 9(02).
                   88  QUARTER-END-MONTH         VALUES 03 06 09 12.
           05  WS-RUN-DD               PIC 9(02).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-EXISTS-SW                PIC X(01).
           88  LEDGER-ENTRY-EXISTS               VALUE 'Y'.
       01  WS-RETURN-CODE              PIC 9(02) VALUE ZERO.

      * TARIFF (BASE AMOUNTS, GST ADDED ON TOP)
       01  WS-TARIFF.
           05  WS-MBP-CHARGE           PIC 9(07)V99 VALUE 100.00.
           05  WS-SMS-CHARGE           PIC 9(07)V99 VALUE 15.00.
           05  WS-DCF-CHARGE           PIC 9(07)V99 VALUE 150.00.
           05  WS-GST-RATE             PIC 9V99     VALUE 0.18.

       01  WS-GST                      PIC 9(07)V99.
       01  WS-ADJ-BALANCE              PIC S9(13)V99.
       01  WS-LEDGER-OPEN-SW           PIC X(01).
           88  LEDGER-OPEN-FOR-READ              VALUE 'Y'.
       01  WS-TOTAL                    PIC 9(07)V99.

       01  WS-STATS.
           05  WS-CANDIDATES           PIC 9(07).
           05  WS-POSTED               PIC 9(07).
           05  WS-ALREADY              PIC 9(07).
           05  WS-NOT-RECOVERED        PIC 9(07).
           05  WS-BASE-TOT             PIC 9(11)V99.
           05  WS-GST-TOT              PIC 9(11)V99.

       01  WS-RPT-WORK                 PIC X(80).
       01  WS-DISP-CNT                 PIC Z,ZZZ,ZZ9.
       01  WS-DISP-AMT                 PIC ZZZ,ZZZ,ZZ9.99.
       01  RPT-DETAIL.
           05  RPT-ACCT                PIC 9(10).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  RPT-CODE                PIC X(03).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  RPT-REF                 PIC X(16).
           05  RPT-BASE                PIC ZZ,ZZ9.99.
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  RPT-GST                 PIC ZZ,ZZ9.99.
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  RPT-TOTAL               PIC ZZ,ZZ9.99.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  RPT-RESULT              PIC X(14).

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 0100-INITIALIZE
           PERFORM 1000-EXTRACT-ACCOUNT-CHARGES
           PERFORM 1500-EXTRACT-CARD-FEES
           PERFORM 1700-EXTRACT-LOCKER-RENT
           CLOSE WORK-FILE
           OPEN OUTPUT DONE-FILE
           PERFORM 2000-APPLY-CHARGES
           CLOSE DONE-FILE
           PERFORM 3000-RENEW-LOCKERS
           PERFORM 9000-FINISH
           GOBACK.

       0100-INITIALIZE.
           INITIALIZE WS-STATS
           MOVE ZERO TO WS-RETURN-CODE
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           DISPLAY 'CHARGES RUN DATE YYYYMMDD (BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-RUN-DATE
           MOVE WS-CURR-DATE TO WS-RUN-DATE
           IF WS-IN-RUN-DATE NOT = SPACES
               IF WS-IN-RUN-DATE IS NUMERIC
                   MOVE WS-IN-RUN-DATE TO DU-DATE-1
                   SET DU-VALIDATE TO TRUE
                   CALL 'DATEUTIL' USING DATE-PARMS
                   IF DU-OK
                       MOVE WS-IN-RUN-DATE TO WS-RUN-DATE
                   END-IF
               END-IF
               IF WS-RUN-DATE NOT = WS-IN-RUN-DATE
                   DISPLAY 'INVALID DATE - USING TODAY.'
               END-IF
           END-IF
           STRING 'data/CHG_' WS-RUN-YYYYMM '.TXT'
                  DELIMITED BY SIZE INTO WS-RPT-FILENAME
           END-STRING
           OPEN OUTPUT RPT-FILE WORK-FILE
           MOVE ALL '=' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           STRING 'SERVICE CHARGES RUN - PERIOD ' WS-RUN-YYYYMM
                  '  RUN DATE ' WS-RUN-DATE
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT
           IF QUARTER-END-MONTH
               MOVE 'QUARTER-END MONTH: SMS CHARGES INCLUDED'
                                                 TO WS-RPT-WORK
               PERFORM 8000-EMIT
           END-IF
           MOVE ALL '=' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE 'ACCOUNT     CHG  REFERENCE           BASE      GST'
                                                 TO WS-RPT-WORK
           MOVE '    TOTAL  RESULT' TO WS-RPT-WORK(51:)
           PERFORM 8000-EMIT.

      *----------------------------------------------------------------*
      * PHASE 1A - ACCOUNT LEVEL CHARGES                               *
      *----------------------------------------------------------------*
       1000-EXTRACT-ACCOUNT-CHARGES.
           OPEN INPUT ACCT-FILE
           IF NOT ACCT-FS-OK
               DISPLAY 'CHGBATCH: ACCOUNT MASTER OPEN FAILED FS='
                       WS-ACCT-FS
               MOVE 12 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
      *    THE LEDGER IS READ HERE SO THE MIN BALANCE TEST CAN IGNORE
      *    THIS PERIOD'S OWN CHARGES (ELSE A RE-RUN WOULD FIND NEW
      *    "SHORTFALLS" CAUSED BY THE FIRST RUN)
           MOVE 'N' TO WS-LEDGER-OPEN-SW
           OPEN INPUT CHG-FILE
           IF CHG-FS-OK
               MOVE 'Y' TO WS-LEDGER-OPEN-SW
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ ACCT-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF ACCT-ACTIVE
                           PERFORM 1100-CHECK-ACCOUNT
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACCT-FILE
           IF LEDGER-OPEN-FOR-READ
               CLOSE CHG-FILE
           END-IF.

       1100-CHECK-ACCOUNT.
           MOVE ACCT-BALANCE TO WS-ADJ-BALANCE
           IF LEDGER-OPEN-FOR-READ AND ACCT-SAVINGS
               PERFORM 1200-ADD-BACK-PERIOD-CHARGES
           END-IF
           IF ACCT-SAVINGS AND WS-ADJ-BALANCE < ACCT-MIN-BALANCE
               MOVE ACCT-NO       TO WK-ACCT-NO
               MOVE 'MBP'         TO WK-CODE
               MOVE WS-MBP-CHARGE TO WK-BASE-AMT
               MOVE 'MIN BAL SHORT' TO WK-REF
               MOVE GLC-FEE-INCOME TO WK-INCOME-GL
               WRITE WORK-RECORD
               ADD 1 TO WS-CANDIDATES
           END-IF
           IF QUARTER-END-MONTH
               MOVE ACCT-NO       TO WK-ACCT-NO
               MOVE 'SMS'         TO WK-CODE
               MOVE WS-SMS-CHARGE TO WK-BASE-AMT
               MOVE 'SMS ALERTS QTR' TO WK-REF
               MOVE GLC-FEE-INCOME TO WK-INCOME-GL
               WRITE WORK-RECORD
               ADD 1 TO WS-CANDIDATES
           END-IF.

       1200-ADD-BACK-PERIOD-CHARGES.
           MOVE ACCT-NO       TO CHG-ACCT-NO
           MOVE WS-RUN-YYYYMM TO CHG-PERIOD
           MOVE 'SMS'         TO CHG-CODE
           READ CHG-FILE
               NOT INVALID KEY
                   IF CHG-POSTED
                       ADD CHG-TOTAL-AMT TO WS-ADJ-BALANCE
                   END-IF
           END-READ
           MOVE 'DCF'         TO CHG-CODE
           READ CHG-FILE
               NOT INVALID KEY
                   IF CHG-POSTED
                       ADD CHG-TOTAL-AMT TO WS-ADJ-BALANCE
                   END-IF
           END-READ.

      *----------------------------------------------------------------*
      * PHASE 1B - CARD ANNUAL FEES                                    *
      *----------------------------------------------------------------*
       1500-EXTRACT-CARD-FEES.
           OPEN INPUT CARD-FILE
           IF NOT CARD-FS-OK
      *        NO CARD MASTER YET IS NORMAL ON A NEW SYSTEM
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ CARD-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF (CARD-ACTIVE OR CARD-PIN-LOCKED)
                          AND CARD-ISSUE-DATE(5:2) = WS-RUN-MM
                          AND CARD-ISSUE-DATE(1:4) < WS-RUN-YYYY
                           MOVE CARD-ACCT-NO  TO WK-ACCT-NO
                           MOVE 'DCF'         TO WK-CODE
                           MOVE WS-DCF-CHARGE TO WK-BASE-AMT
                           MOVE SPACES        TO WK-REF
                           STRING 'CARD ' CARD-NO(13:4)
                                  DELIMITED BY SIZE INTO WK-REF
                           END-STRING
                           MOVE GLC-FEE-INCOME TO WK-INCOME-GL
                           WRITE WORK-RECORD
                           ADD 1 TO WS-CANDIDATES
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CARD-FILE.

      *----------------------------------------------------------------*
      * PHASE 1C - LOCKER RENT FALLING DUE THIS MONTH (OR OVERDUE)     *
      *----------------------------------------------------------------*
       1700-EXTRACT-LOCKER-RENT.
           OPEN INPUT LOCK-FILE
           IF NOT LOCK-FS-OK
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ LOCK-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF LOCK-ALLOTTED
                          AND LOCK-PAID-UNTIL(1:6) <= WS-RUN-YYYYMM
                           PERFORM 1710-LOCKER-CANDIDATE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE LOCK-FILE.

       1710-LOCKER-CANDIDATE.
           MOVE LOCK-RENT-ACCT TO WK-ACCT-NO
           MOVE 'LKR'          TO WK-CODE
           EVALUATE TRUE
               WHEN LOCK-SMALL  MOVE 2000.00 TO WK-BASE-AMT
               WHEN LOCK-MEDIUM MOVE 4000.00 TO WK-BASE-AMT
               WHEN OTHER       MOVE 8000.00 TO WK-BASE-AMT
           END-EVALUATE
           MOVE SPACES TO WK-REF
           STRING 'LOCKER ' LOCK-NO DELIMITED BY SIZE INTO WK-REF
           END-STRING
           MOVE GLC-LOCKER-INCOME TO WK-INCOME-GL
           WRITE WORK-RECORD
           ADD 1 TO WS-CANDIDATES.

      *----------------------------------------------------------------*
      * PHASE 3 - EXTEND PAID-UNTIL BY A YEAR FOR RENT COLLECTED NOW   *
      *----------------------------------------------------------------*
       3000-RENEW-LOCKERS.
           OPEN INPUT DONE-FILE
           OPEN I-O LOCK-FILE
           IF NOT LOCK-FS-OK
               CLOSE DONE-FILE
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ DONE-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       MOVE DONE-LOCK-NO TO LOCK-NO
                       READ LOCK-FILE
                           NOT INVALID KEY
                               SET DU-ADD-MONTHS TO TRUE
                               MOVE LOCK-PAID-UNTIL TO DU-DATE-1
                               MOVE 12 TO DU-NUMBER
                               CALL 'DATEUTIL' USING DATE-PARMS
                               MOVE DU-RESULT-DATE TO LOCK-PAID-UNTIL
                               REWRITE LOCK-RECORD
                               ADD 1 TO WS-LOCKERS-RENEWED
                       END-READ
               END-READ
           END-PERFORM
           CLOSE DONE-FILE LOCK-FILE
           IF WS-LOCKERS-RENEWED > ZERO
               DISPLAY 'LOCKER RENT RENEWED FOR ' WS-LOCKERS-RENEWED
                       ' LOCKER(S).'
           END-IF.

      *----------------------------------------------------------------*
      * PHASE 2 - POST                                                 *
      *----------------------------------------------------------------*
       2000-APPLY-CHARGES.
           OPEN I-O CHG-FILE
           IF CHG-FS-NOFILE
               OPEN OUTPUT CHG-FILE
               CLOSE CHG-FILE
               OPEN I-O CHG-FILE
           END-IF
           IF NOT CHG-FS-OK
               DISPLAY 'CHGBATCH: CHARGES LEDGER OPEN FAILED FS='
                       WS-CHG-FS
               MOVE 12 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           OPEN INPUT WORK-FILE
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ WORK-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END PERFORM 2100-APPLY-ONE
               END-READ
           END-PERFORM
           CLOSE WORK-FILE CHG-FILE.

       2100-APPLY-ONE.
           MOVE WK-ACCT-NO    TO CHG-ACCT-NO
           MOVE WK-CODE       TO CHG-CODE
           MOVE WS-RUN-YYYYMM TO CHG-PERIOD
           MOVE 'N' TO WS-EXISTS-SW
           READ CHG-FILE
               INVALID KEY CONTINUE
               NOT INVALID KEY MOVE 'Y' TO WS-EXISTS-SW
           END-READ
           IF LEDGER-ENTRY-EXISTS AND CHG-POSTED
               ADD 1 TO WS-ALREADY
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-GST ROUNDED = WK-BASE-AMT * WS-GST-RATE
           COMPUTE WS-TOTAL = WK-BASE-AMT + WS-GST

           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE WK-ACCT-NO TO PP-ACCT-NO
           MOVE WS-TOTAL   TO PP-AMOUNT
           MOVE 'CH'       TO PP-TRAN-TYPE
           MOVE ZERO       TO PP-REF-ACCT
           MOVE 'Y'        TO PP-IGNORE-MIN-BAL
           MOVE 'Y'        TO PP-SYSTEM-TXN
           MOVE WK-INCOME-GL TO PP-CONTRA-GL
           STRING WK-CODE ' ' FUNCTION TRIM(WK-REF) ' INCL GST'
                  DELIMITED BY SIZE INTO PP-NARRATION
           END-STRING
           CALL 'ACCTPOST' USING POST-PARMS

           MOVE WK-REF      TO CHG-REF
           MOVE WK-BASE-AMT TO CHG-BASE-AMT
           MOVE WS-GST      TO CHG-GST-AMT
           MOVE WS-TOTAL    TO CHG-TOTAL-AMT
           MOVE WS-RUN-DATE TO CHG-RUN-DATE
           IF PP-OK
      *        THE GST PART OF THE INCOME BELONGS TO THE GOVERNMENT
               MOVE WK-INCOME-GL    TO GP-DR-GL
               MOVE GLC-GST-PAYABLE TO GP-CR-GL
               MOVE WS-GST          TO GP-AMOUNT
               MOVE WK-ACCT-NO      TO GP-REF
               MOVE 'GST ON SERVICE CHARGES' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
               SET CHG-POSTED TO TRUE
               ADD 1 TO WS-POSTED
               IF WK-CODE = 'LKR'
                   MOVE WK-REF(8:5) TO DONE-LOCK-NO
                   WRITE DONE-RECORD
               END-IF
               ADD WK-BASE-AMT TO WS-BASE-TOT
               ADD WS-GST      TO WS-GST-TOT
               MOVE 'POSTED' TO RPT-RESULT
           ELSE
               SET CHG-NOT-RECOVERED TO TRUE
               ADD 1 TO WS-NOT-RECOVERED
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               MOVE SPACES TO RPT-RESULT
               STRING 'NOT RECOV RC' PP-RETURN-CODE
                      DELIMITED BY SIZE INTO RPT-RESULT
               END-STRING
           END-IF
           IF LEDGER-ENTRY-EXISTS
               REWRITE CHG-RECORD
           ELSE
               WRITE CHG-RECORD
           END-IF
           IF NOT CHG-FS-OK
               DISPLAY 'CHGBATCH: SEVERE - LEDGER UPDATE FAILED FOR '
                       WK-ACCT-NO ' ' WK-CODE ' FS=' WS-CHG-FS
               MOVE 12 TO WS-RETURN-CODE
           END-IF
           MOVE WK-ACCT-NO  TO RPT-ACCT
           MOVE WK-CODE     TO RPT-CODE
           MOVE WK-REF      TO RPT-REF
           MOVE WK-BASE-AMT TO RPT-BASE
           MOVE WS-GST      TO RPT-GST
           MOVE WS-TOTAL    TO RPT-TOTAL
           MOVE RPT-DETAIL  TO WS-RPT-WORK
           PERFORM 8000-EMIT.

      *----------------------------------------------------------------*
      * COMMON                                                         *
      *----------------------------------------------------------------*
       8000-EMIT.
           MOVE WS-RPT-WORK TO RPT-LINE
           WRITE RPT-LINE
           DISPLAY WS-RPT-WORK
           MOVE SPACES TO WS-RPT-WORK.

       9000-FINISH.
           MOVE ALL '-' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-CANDIDATES TO WS-DISP-CNT
           STRING 'CANDIDATES        : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-POSTED TO WS-DISP-CNT
           STRING 'POSTED            : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-ALREADY TO WS-DISP-CNT
           STRING 'ALREADY CHARGED   : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-NOT-RECOVERED TO WS-DISP-CNT
           STRING 'NOT RECOVERED     : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-BASE-TOT TO WS-DISP-AMT
           STRING 'CHARGES (EX GST)  : ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-GST-TOT TO WS-DISP-AMT
           STRING 'GST COLLECTED     : ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           STRING 'RETURN CODE       : ' WS-RETURN-CODE
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           CLOSE RPT-FILE
           DISPLAY 'REPORT SAVED TO ' WS-RPT-FILENAME
           MOVE WS-RETURN-CODE TO RETURN-CODE.
