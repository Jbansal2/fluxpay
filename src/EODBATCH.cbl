       IDENTIFICATION DIVISION.
       PROGRAM-ID. EODBATCH.
      *================================================================*
      * PROGRAM  : EODBATCH                                            *
      * PURPOSE  : END-OF-DAY BATCH. RUNS FOUR STEPS IN ORDER:         *
      *              STEP 0 - ACCOUNTS: DAILY INTEREST PRODUCTS,       *
      *                       DORMANCY, OVER-LIMIT OVERDRAFTS          *
      *              STEP 1 - LOANS: OVERDUE EMI COUNT, PENALTY AND    *
      *                       NPA CLASSIFICATION (90+ DAYS)            *
      *              STEP 2 - DEPOSITS: PAY OUT MATURED FD/RD AND      *
      *                       COUNT MISSED RD INSTALMENTS              *
      *              STEP 3 - CARDS: MARK EXPIRED CARDS                *
      *              STEP 4 - DAILY TRANSACTION SUMMARY FROM TRANLOG   *
      *            EVERYTHING IS ALSO WRITTEN TO data/EOD_<DATE>.TXT.  *
      *            THE RUN DATE CAN BE OVERRIDDEN (FOR RE-RUNS AND     *
      *            TESTING); BLANK MEANS TODAY. STEPS 1-3 ARE SAFE TO  *
      *            RE-RUN FOR THE SAME DATE.                           *
      * FILES    : LOANMAST, DEPMAST, CARDMAST (INDEXED, I-O)          *
      *            TRANLOG (INPUT), EODRPT (OUTPUT)                    *
      * CALLS    : ACCTPOST, DATEUTIL, DEPCALC                         *
      * RETURN   : 0 = CLEAN, 4 = WARNINGS, 12 = SEVERE                *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      * 2026-10-14 STEP 0 (ACCOUNTS), PAYOUTS VIA DEPPAY WITH TDS      *
      * 2026-10-14 CHECKPOINT/RESTART (CHKPOINT) PER STEP AND EVERY   *
      *            10 ACCOUNTS                                         *
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
           SELECT CARD-FILE ASSIGN TO 'data/CARDMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CARD-NO
               FILE STATUS IS WS-CARD-FS.
           SELECT TRAN-FILE ASSIGN TO 'data/TRANLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRAN-FS.
           SELECT RPT-FILE ASSIGN TO WS-RPT-FILENAME
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
       FD  CARD-FILE.
           COPY CARDREC.
       FD  TRAN-FILE.
           COPY TRANREC.
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(80).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY DATEPARM.
           COPY DEPCPARM.
           COPY DPPPARM.

       01  WS-LOAN-FS                  PIC X(02).
           88  LOAN-FS-OK                        VALUE '00'.
       01  WS-DEP-FS                   PIC X(02).
           88  DEP-FS-OK                         VALUE '00'.
       01  WS-CARD-FS                  PIC X(02).
           88  CARD-FS-OK                        VALUE '00'.
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-FILENAME             PIC X(40).

       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-RUN-DATE-R REDEFINES WS-RUN-DATE.
           05  WS-RUN-YYYYMM           PIC 9(06).
           05  FILLER                  PIC 9(02).
       01  WS-IN-RUN-DATE              PIC X(08).
       01  WS-RETURN-CODE              PIC 9(02) VALUE ZERO.
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.

      * LOAN STEP
       01  WS-DUE-WALK                 PIC 9(08).
       01  WS-OVERDUE-CNT              PIC 9(03).
       01  WS-PENALTY-PCT              PIC 9V99 VALUE 0.02.
       01  WS-NPA-THRESHOLD            PIC 9(03) VALUE 3.

      * STEP COUNTERS
       01  WS-ACCT-STATS.
           05  WS-AC-READ              PIC 9(07).
           05  WS-AC-PRODUCT           PIC 9(07).
           05  WS-AC-DORMANT           PIC 9(07).
           05  WS-AC-OVER-LIMIT        PIC 9(07).
      * CHECKPOINT / RESTART
           COPY CKPPARM.
       01  WS-DONE-STEP                PIC 9(02) VALUE ZERO.
       01  WS-STEP-NO                  PIC 9(02).
       01  WS-RESUME-KEY               PIC X(20) VALUE SPACES.
       01  WS-COMMIT-EVERY             PIC 9(04) VALUE 10.
       01  WS-SINCE-COMMIT             PIC 9(04) VALUE ZERO.
       01  WS-CKP-COUNTERS.
           05  WS-CKP-ACCT             PIC X(28).
           05  FILLER                  PIC X(52).
       01  WS-PRODUCT-DAYS             PIC S9(07).
       01  WS-DORMANT-CUTOFF           PIC 9(08).
       01  WS-DORMANT-MONTHS           PIC S9(05) VALUE -24.
       01  WS-ACCT-CHANGED-SW          PIC X(01).
           88  ACCT-CHANGED                      VALUE 'Y'.

       01  WS-STATS.
           05  WS-LN-READ              PIC 9(07).
           05  WS-LN-OVERDUE           PIC 9(07).
           05  WS-LN-NEW-NPA           PIC 9(07).
           05  WS-LN-PENALTY-TOT       PIC 9(11)V99.
           05  WS-DP-READ              PIC 9(07).
           05  WS-DP-MATURED           PIC 9(07).
           05  WS-DP-PAID-TOT          PIC 9(11)V99.
           05  WS-DP-TDS-TOT           PIC 9(11)V99.
           05  WS-DP-FAILED            PIC 9(07).
           05  WS-DP-RD-MISSED         PIC 9(07).
           05  WS-CD-READ              PIC 9(07).
           05  WS-CD-EXPIRED           PIC 9(07).

      * DAILY SUMMARY BY TRANSACTION TYPE
       01  WS-TYPE-TABLE.
           05  FILLER PIC X(22) VALUE 'DPCASH DEPOSIT        '.
           05  FILLER PIC X(22) VALUE 'WDCASH WITHDRAWAL     '.
           05  FILLER PIC X(22) VALUE 'TOTRANSFER OUT        '.
           05  FILLER PIC X(22) VALUE 'TITRANSFER IN         '.
           05  FILLER PIC X(22) VALUE 'ININTEREST CREDIT     '.
           05  FILLER PIC X(22) VALUE 'LDLOAN DISBURSAL      '.
           05  FILLER PIC X(22) VALUE 'LRLOAN REPAYMENT      '.
           05  FILLER PIC X(22) VALUE 'AWATM WITHDRAWAL      '.
           05  FILLER PIC X(22) VALUE 'FODEPOSIT OPENED      '.
           05  FILLER PIC X(22) VALUE 'FMDEPOSIT PAYOUT      '.
           05  FILLER PIC X(22) VALUE 'RIRD INSTALMENT       '.
           05  FILLER PIC X(22) VALUE 'CHCHARGES             '.
           05  FILLER PIC X(22) VALUE 'CQCHEQUE PAID         '.
           05  FILLER PIC X(22) VALUE 'POPAYMENT SENT        '.
           05  FILLER PIC X(22) VALUE 'PIPAYMENT RECEIVED    '.
           05  FILLER PIC X(22) VALUE 'OIOVERDRAFT INTEREST  '.
           05  FILLER PIC X(22) VALUE 'CPCREDIT CARD PAYMENT '.
           05  FILLER PIC X(22) VALUE 'NDNACH DEBIT          '.
           05  FILLER PIC X(22) VALUE 'OCCHEQUE COLLECTED    '.
           05  FILLER PIC X(22) VALUE 'TFTRADE FINANCE DEBIT '.
           05  FILLER PIC X(22) VALUE 'TRTRADE FINANCE REFUND'.
           05  FILLER PIC X(22) VALUE 'XIFOREX INWARD        '.
           05  FILLER PIC X(22) VALUE 'XOFOREX OUTWARD       '.
       01  WS-TYPE-TABLE-R REDEFINES WS-TYPE-TABLE.
           05  WS-TYPE-ENTRY OCCURS 23 TIMES INDEXED BY TYP-IDX.
               10  TYP-CODE            PIC X(02).
               10  TYP-DESC            PIC X(20).
       01  WS-TYPE-TOTALS.
           05  WS-TYPE-TOT OCCURS 23 TIMES.
               10  TT-COUNT            PIC 9(07).
               10  TT-AMOUNT           PIC 9(13)V99.
       01  WS-DAY-CREDITS              PIC 9(13)V99.
       01  WS-DAY-DEBITS               PIC 9(13)V99.
       01  WS-DAY-COUNT                PIC 9(07).
       01  WS-UNKNOWN-TYPES            PIC 9(07).
       01  WS-T                        PIC 9(02).

      * REPORT FORMATTING
       01  WS-DISP-CNT                 PIC Z,ZZZ,ZZ9.
       01  WS-DISP-AMT                 PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-RPT-WORK                 PIC X(80).
       01  RPT-SUMMARY-LINE.
           05  RPT-S-DESC              PIC X(22).
           05  RPT-S-COUNT             PIC Z,ZZZ,ZZ9 BLANK WHEN ZERO.
           05  FILLER                  PIC X(04) VALUE SPACES.
           05  RPT-S-AMOUNT            PIC ZZZ,ZZZ,ZZZ,ZZ9.99
                                       BLANK WHEN ZERO.
       01  RPT-DETAIL-LINE.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  RPT-D-KEY               PIC X(16).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  RPT-D-TEXT              PIC X(40).
           05  RPT-D-AMOUNT            PIC ZZZ,ZZZ,ZZ9.99.

           COPY MKRPARM.

       PROCEDURE DIVISION.
      *    EACH STEP IS CHECKPOINTED. A RESTART FOR THE SAME RUN DATE
      *    SKIPS FINISHED STEPS AND RESUMES THE ACCOUNT STEP AFTER THE
      *    LAST COMMITTED ACCOUNT (SEE CHKPOINT).
       0000-MAIN.
           PERFORM 0100-INITIALIZE
           PERFORM 0200-CHECKPOINT-BEGIN
           IF WS-DONE-STEP < 1
               PERFORM 0500-ACCOUNT-STEP
               MOVE 1 TO WS-STEP-NO
               PERFORM 0300-STEP-DONE
           END-IF
           IF WS-DONE-STEP < 2
               PERFORM 1000-LOAN-STEP
               MOVE 2 TO WS-STEP-NO
               PERFORM 0300-STEP-DONE
           END-IF
           IF WS-DONE-STEP < 3
               PERFORM 2000-DEPOSIT-STEP
               MOVE 3 TO WS-STEP-NO
               PERFORM 0300-STEP-DONE
           END-IF
           IF WS-DONE-STEP < 4
               PERFORM 3000-CARD-STEP
               PERFORM 3500-EXPIRE-APPROVALS
               MOVE 4 TO WS-STEP-NO
               PERFORM 0300-STEP-DONE
           END-IF
           PERFORM 4000-SUMMARY-STEP
           SET CKP-COMPLETE TO TRUE
           CALL 'CHKPOINT' USING CKP-PARMS
           PERFORM 9000-FINISH
           GOBACK.

       0200-CHECKPOINT-BEGIN.
           INITIALIZE CKP-PARMS
           SET CKP-BEGIN TO TRUE
           MOVE 'EODBATCH'  TO CKP-PROGRAM
           MOVE WS-RUN-DATE TO CKP-RUN-DATE
           CALL 'CHKPOINT' USING CKP-PARMS
           MOVE ZERO TO WS-DONE-STEP
           MOVE SPACES TO WS-RESUME-KEY
           IF CKP-IS-RESTART
               MOVE CKP-STEP     TO WS-DONE-STEP
               MOVE CKP-LAST-KEY TO WS-RESUME-KEY
               MOVE CKP-COUNTERS TO WS-CKP-COUNTERS
               MOVE WS-CKP-ACCT  TO WS-ACCT-STATS
               MOVE CKP-RESTARTS TO WS-DISP-CNT
               STRING '*** RESTART ' WS-DISP-CNT ' - STEPS DONE: '
                      WS-DONE-STEP ', RESUME AFTER ACCOUNT '
                      WS-RESUME-KEY(1:10)
                      DELIMITED BY SIZE INTO WS-RPT-WORK
               PERFORM 8000-EMIT
           END-IF.

       0300-STEP-DONE.
           MOVE WS-ACCT-STATS TO WS-CKP-ACCT
           SET CKP-END-STEP TO TRUE
           MOVE WS-STEP-NO TO CKP-STEP
           MOVE WS-CKP-COUNTERS TO CKP-COUNTERS
           CALL 'CHKPOINT' USING CKP-PARMS.

      *    COMMIT POINT EVERY WS-COMMIT-EVERY ACCOUNTS (AFTER REWRITE)
       0700-ACCOUNT-COMMIT.
           ADD 1 TO WS-SINCE-COMMIT
           IF WS-SINCE-COMMIT >= WS-COMMIT-EVERY
               MOVE ZERO TO WS-SINCE-COMMIT
               MOVE WS-ACCT-STATS TO WS-CKP-ACCT
               SET CKP-SAVE TO TRUE
               MOVE ACCT-NO     TO CKP-LAST-KEY
               MOVE WS-AC-READ  TO CKP-RECORDS
               MOVE WS-CKP-COUNTERS TO CKP-COUNTERS
               CALL 'CHKPOINT' USING CKP-PARMS
           END-IF.

       0100-INITIALIZE.
           INITIALIZE WS-STATS WS-TYPE-TOTALS WS-ACCT-STATS
           MOVE ZERO TO WS-RETURN-CODE WS-DAY-CREDITS WS-DAY-DEBITS
                        WS-DAY-COUNT WS-UNKNOWN-TYPES
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           DISPLAY 'EOD RUN DATE YYYYMMDD (BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-RUN-DATE
           IF WS-IN-RUN-DATE = SPACES
               MOVE WS-CURR-DATE TO WS-RUN-DATE
           ELSE
               MOVE WS-IN-RUN-DATE TO DU-DATE-1
               SET DU-VALIDATE TO TRUE
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DU-OK AND WS-IN-RUN-DATE IS NUMERIC
                   MOVE WS-IN-RUN-DATE TO WS-RUN-DATE
               ELSE
                   DISPLAY 'INVALID DATE - USING TODAY.'
                   MOVE WS-CURR-DATE TO WS-RUN-DATE
               END-IF
           END-IF
           MOVE SPACES TO RPT-D-TEXT
           STRING 'data/EOD_' WS-RUN-DATE '.TXT'
                  DELIMITED BY SIZE INTO WS-RPT-FILENAME
           END-STRING
           OPEN OUTPUT RPT-FILE
           MOVE ALL '=' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           STRING 'CORE BANKING - END OF DAY REPORT FOR ' WS-RUN-DATE
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT
           MOVE ALL '=' TO WS-RPT-WORK
           PERFORM 8000-EMIT.

      *================================================================*
      * STEP 0 - ACCOUNTS                                              *
      *   DAILY PRODUCTS: ADD (BALANCE x DAYS SINCE THE LAST EOD) TO   *
      *   THE SAVINGS PRODUCT, OR (OVERDRAWN AMOUNT x DAYS) TO THE OD  *
      *   PRODUCT. KEYED ON LAST-PRODUCT-DATE, SO A RE-RUN FOR THE     *
      *   SAME DATE ADDS NOTHING AND A MISSED DAY IS STILL COUNTED.    *
      *   DORMANCY: NO CUSTOMER ACTIVITY FOR 24 MONTHS.                *
      *   OVER-LIMIT: OVERDRAFT BEYOND THE SANCTIONED LIMIT.           *
      *================================================================*
       0500-ACCOUNT-STEP.
           MOVE 'STEP 0 - ACCOUNTS' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           SET DU-ADD-MONTHS TO TRUE
           MOVE WS-RUN-DATE TO DU-DATE-1
           MOVE WS-DORMANT-MONTHS TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO WS-DORMANT-CUTOFF
           OPEN I-O ACCT-FILE
           IF NOT ACCT-FS-OK
               MOVE '  NO ACCOUNT MASTER - STEP SKIPPED' TO WS-RPT-WORK
               PERFORM 8000-EMIT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           MOVE ZERO TO WS-SINCE-COMMIT
           IF WS-RESUME-KEY NOT = SPACES
               MOVE WS-RESUME-KEY(1:10) TO ACCT-NO
               START ACCT-FILE KEY IS GREATER THAN ACCT-NO
                   INVALID KEY SET END-OF-FILE TO TRUE
               END-START
           END-IF
           PERFORM UNTIL END-OF-FILE
               READ ACCT-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-AC-READ
                       IF ACCT-ACTIVE OR ACCT-DORMANT
                           PERFORM 0600-ACCOUNT-EOD
                       END-IF
                       PERFORM 0700-ACCOUNT-COMMIT
               END-READ
           END-PERFORM
           CLOSE ACCT-FILE
           MOVE WS-AC-READ TO WS-DISP-CNT
           STRING '  ACCOUNTS READ     : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-AC-PRODUCT TO WS-DISP-CNT
           STRING '  PRODUCTS UPDATED  : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-AC-DORMANT TO WS-DISP-CNT
           STRING '  MARKED DORMANT    : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-AC-OVER-LIMIT TO WS-DISP-CNT
           STRING '  OVER OD LIMIT     : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE SPACES TO WS-RPT-WORK
           PERFORM 8000-EMIT.

       0600-ACCOUNT-EOD.
           MOVE 'N' TO WS-ACCT-CHANGED-SW
           IF ACCT-LAST-PRODUCT-DATE < WS-RUN-DATE
               IF ACCT-LAST-PRODUCT-DATE = ZERO
                   MOVE 1 TO WS-PRODUCT-DAYS
               ELSE
                   SET DU-DAYS-BETWEEN TO TRUE
                   MOVE ACCT-LAST-PRODUCT-DATE TO DU-DATE-1
                   MOVE WS-RUN-DATE TO DU-DATE-2
                   CALL 'DATEUTIL' USING DATE-PARMS
                   MOVE DU-RESULT-DAYS TO WS-PRODUCT-DAYS
               END-IF
               IF ACCT-SAVINGS AND ACCT-BALANCE > ZERO
                   COMPUTE ACCT-INT-PRODUCT = ACCT-INT-PRODUCT
                           + ACCT-BALANCE * WS-PRODUCT-DAYS
               END-IF
               IF ACCT-CURRENT AND ACCT-BALANCE < ZERO
                   COMPUTE ACCT-OD-PRODUCT = ACCT-OD-PRODUCT
                           - ACCT-BALANCE * WS-PRODUCT-DAYS
               END-IF
               ADD WS-PRODUCT-DAYS TO ACCT-PRODUCT-DAYS
               MOVE WS-RUN-DATE TO ACCT-LAST-PRODUCT-DATE
               ADD 1 TO WS-AC-PRODUCT
               MOVE 'Y' TO WS-ACCT-CHANGED-SW
           END-IF
           IF ACCT-ACTIVE AND ACCT-LAST-TXN-DATE < WS-DORMANT-CUTOFF
               SET ACCT-DORMANT TO TRUE
               ADD 1 TO WS-AC-DORMANT
               MOVE 'Y' TO WS-ACCT-CHANGED-SW
               MOVE ACCT-NO TO RPT-D-KEY
               STRING 'DORMANT - LAST ACTIVITY ' ACCT-LAST-TXN-DATE
                      DELIMITED BY SIZE INTO RPT-D-TEXT
               END-STRING
               MOVE ZERO TO RPT-D-AMOUNT
               MOVE RPT-DETAIL-LINE TO WS-RPT-WORK
               PERFORM 8000-EMIT
           END-IF
           IF ACCT-CURRENT AND ACCT-BALANCE < ZERO
              AND 0 - ACCT-BALANCE > ACCT-OD-LIMIT
               ADD 1 TO WS-AC-OVER-LIMIT
               MOVE ACCT-NO TO RPT-D-KEY
               MOVE 'OVERDRAWN BEYOND SANCTIONED LIMIT' TO RPT-D-TEXT
               COMPUTE RPT-D-AMOUNT = 0 - ACCT-BALANCE
               MOVE RPT-DETAIL-LINE TO WS-RPT-WORK
               PERFORM 8000-EMIT
           END-IF
           IF ACCT-CHANGED
               REWRITE ACCT-RECORD
           END-IF.

      *================================================================*
      * STEP 1 - LOANS                                                 *
      *================================================================*
       1000-LOAN-STEP.
           PERFORM 8100-STEP-HEADER-LOAN
           OPEN I-O LOAN-FILE
           IF NOT LOAN-FS-OK
               MOVE '  NO LOAN MASTER - STEP SKIPPED' TO WS-RPT-WORK
               PERFORM 8000-EMIT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ LOAN-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-LN-READ
                       IF (LOAN-ACTIVE OR LOAN-NPA)
                          AND LOAN-OUTSTANDING > ZERO
                           PERFORM 1100-CHECK-OVERDUE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE LOAN-FILE
           MOVE WS-LN-READ TO WS-DISP-CNT
           MOVE SPACES TO RPT-D-TEXT
           STRING '  LOANS READ        : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-LN-OVERDUE TO WS-DISP-CNT
           STRING '  LOANS OVERDUE     : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-LN-NEW-NPA TO WS-DISP-CNT
           STRING '  NEWLY NPA         : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-LN-PENALTY-TOT TO WS-DISP-AMT
           STRING '  PENALTY DUE TOTAL : ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT.

      *    COUNT DUE DATES FROM LOAN-NEXT-DUE-DATE THAT ARE BEFORE THE
      *    RUN DATE. PENALTY IS RECOMPUTED (NOT ADDED) SO A RE-RUN FOR
      *    THE SAME DATE GIVES THE SAME RESULT.
       1100-CHECK-OVERDUE.
           MOVE ZERO TO WS-OVERDUE-CNT
           MOVE LOAN-NEXT-DUE-DATE TO WS-DUE-WALK
           PERFORM UNTIL WS-DUE-WALK >= WS-RUN-DATE
                      OR LOAN-EMIS-PAID + WS-OVERDUE-CNT
                         >= LOAN-TENURE
               ADD 1 TO WS-OVERDUE-CNT
               MOVE WS-DUE-WALK TO DU-DATE-1
               MOVE 1           TO DU-NUMBER
               SET DU-ADD-MONTHS TO TRUE
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DU-RESULT-DATE TO WS-DUE-WALK
           END-PERFORM

           IF WS-OVERDUE-CNT = LOAN-OVERDUE-EMIS
              AND WS-OVERDUE-CNT = ZERO
               EXIT PARAGRAPH
           END-IF

           MOVE WS-OVERDUE-CNT TO LOAN-OVERDUE-EMIS
           COMPUTE LOAN-PENALTY-DUE ROUNDED =
                   LOAN-EMI * WS-PENALTY-PCT * WS-OVERDUE-CNT
           IF WS-OVERDUE-CNT > ZERO
               ADD 1 TO WS-LN-OVERDUE
               ADD LOAN-PENALTY-DUE TO WS-LN-PENALTY-TOT
               MOVE LOAN-NO TO RPT-D-KEY
               STRING 'OVERDUE EMIS ' WS-OVERDUE-CNT
                      ', PENALTY' DELIMITED BY SIZE
                      INTO RPT-D-TEXT
               END-STRING
               MOVE LOAN-PENALTY-DUE TO RPT-D-AMOUNT
               MOVE RPT-DETAIL-LINE TO WS-RPT-WORK
               PERFORM 8000-EMIT
           END-IF
           IF WS-OVERDUE-CNT >= WS-NPA-THRESHOLD AND LOAN-ACTIVE
               SET LOAN-NPA TO TRUE
               ADD 1 TO WS-LN-NEW-NPA
           END-IF
           IF WS-OVERDUE-CNT < WS-NPA-THRESHOLD AND LOAN-NPA
               SET LOAN-ACTIVE TO TRUE
           END-IF
           REWRITE LOAN-RECORD
           IF NOT LOAN-FS-OK
               DISPLAY 'EOD: LOAN REWRITE FAILED ' LOAN-NO
               PERFORM 8900-SET-RC-WARNING
           END-IF.

      *================================================================*
      * STEP 2 - DEPOSITS                                              *
      *================================================================*
       2000-DEPOSIT-STEP.
           MOVE SPACES TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE 'STEP 2 - TERM DEPOSITS' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           OPEN I-O DEP-FILE
           IF NOT DEP-FS-OK
               MOVE '  NO DEPOSIT MASTER - STEP SKIPPED' TO WS-RPT-WORK
               PERFORM 8000-EMIT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ DEP-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-DP-READ
                       IF DEP-ACTIVE
                           PERFORM 2100-PROCESS-DEPOSIT
                       END-IF
               END-READ
           END-PERFORM
           CLOSE DEP-FILE
           MOVE WS-DP-READ TO WS-DISP-CNT
           STRING '  DEPOSITS READ     : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-DP-MATURED TO WS-DISP-CNT
           STRING '  MATURED AND PAID  : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-DP-PAID-TOT TO WS-DISP-AMT
           STRING '  TOTAL PAID OUT    : ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-DP-TDS-TOT TO WS-DISP-AMT
           STRING '  TDS DEDUCTED      : ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-DP-RD-MISSED TO WS-DISP-CNT
           STRING '  RDS WITH MISSES   : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           IF WS-DP-FAILED > ZERO
               MOVE WS-DP-FAILED TO WS-DISP-CNT
               STRING '  PAYOUTS FAILED    : ' WS-DISP-CNT
                      DELIMITED BY SIZE INTO WS-RPT-WORK
               PERFORM 8000-EMIT
           END-IF.

       2100-PROCESS-DEPOSIT.
           IF DEP-MATURITY-DATE <= WS-RUN-DATE
              AND DEP-LIEN-LOAN-NO > ZERO
      *        A PLEDGED DEPOSIT IS HELD UNTIL THE LOAN IS SETTLED
               MOVE DEP-NO TO RPT-D-KEY
               MOVE SPACES TO RPT-D-TEXT
               STRING 'MATURED, HELD - LIEN FOR LOAN ' DEP-LIEN-LOAN-NO
                      DELIMITED BY SIZE INTO RPT-D-TEXT
               END-STRING
               MOVE DEP-MATURITY-AMT TO RPT-D-AMOUNT
               MOVE RPT-DETAIL-LINE TO WS-RPT-WORK
               PERFORM 8000-EMIT
               PERFORM 8900-SET-RC-WARNING
               EXIT PARAGRAPH
           END-IF
           IF DEP-MATURITY-DATE <= WS-RUN-DATE
               PERFORM 2200-PAY-MATURITY
           ELSE
               IF DEP-RD
                   PERFORM 2300-COUNT-RD-MISSES
               END-IF
           END-IF.

      *    RD WITH MISSED INSTALMENTS IS REVALUED ON WHAT WAS PAID
       2200-PAY-MATURITY.
           IF DEP-RD AND DEP-RD-PAID-CNT < DEP-TENURE
               MOVE 'RV'            TO DC-FUNCTION
               MOVE DEP-AMOUNT      TO DC-AMOUNT
               MOVE DEP-RATE        TO DC-RATE
               MOVE DEP-TENURE      TO DC-MONTHS
               MOVE DEP-RD-PAID-CNT TO DC-COUNT
               CALL 'DEPCALC' USING DEPC-PARMS
               MOVE DC-RESULT-AMT   TO DEP-PAYOUT-AMT
           ELSE
               MOVE DEP-MATURITY-AMT TO DEP-PAYOUT-AMT
           END-IF

      *    DEPPAY WORKS OUT INTEREST AND TDS, CREDITS THE NET AMOUNT
      *    AND POSTS THE GL ENTRIES
           MOVE DEP-PAYOUT-AMT TO DPP-GROSS
           MOVE 'DEPOSIT MATURITY PROCEEDS' TO DPP-NARRATION
           MOVE WS-RUN-DATE    TO DPP-RUN-DATE
           CALL 'DEPPAY' USING DPP-PARMS DEP-RECORD
           IF NOT DPP-OK
               ADD 1 TO WS-DP-FAILED
               MOVE DEP-NO TO RPT-D-KEY
               STRING 'PAYOUT FAILED, ACCTPOST RC=' DPP-RC
                      DELIMITED BY SIZE INTO RPT-D-TEXT
               END-STRING
               MOVE DEP-PAYOUT-AMT TO RPT-D-AMOUNT
               MOVE RPT-DETAIL-LINE TO WS-RPT-WORK
               PERFORM 8000-EMIT
               PERFORM 8900-SET-RC-WARNING
               EXIT PARAGRAPH
           END-IF

           SET DEP-MATURED TO TRUE
           MOVE WS-RUN-DATE TO DEP-CLOSE-DATE
           IF DEP-RD
               COMPUTE DEP-RD-MISSED-CNT = DEP-TENURE - DEP-RD-PAID-CNT
           END-IF
           REWRITE DEP-RECORD
           IF NOT DEP-FS-OK
      *        MONEY HAS MOVED BUT THE DEPOSIT IS STILL OPEN - A
      *        RE-RUN WOULD PAY TWICE. THIS NEEDS A HUMAN.
               DISPLAY 'EOD: SEVERE - DEPOSIT ' DEP-NO
                       ' PAID BUT NOT MARKED MATURED'
               MOVE 12 TO WS-RETURN-CODE
           END-IF
           ADD 1 TO WS-DP-MATURED
           ADD DPP-NET TO WS-DP-PAID-TOT
           ADD DPP-TDS TO WS-DP-TDS-TOT
           MOVE DEP-NO TO RPT-D-KEY
           IF DPP-TDS > ZERO
               MOVE DPP-TDS TO WS-DISP-AMT
               MOVE SPACES TO RPT-D-TEXT
               STRING 'MATURED, NET OF TDS ' FUNCTION TRIM(WS-DISP-AMT)
                      DELIMITED BY SIZE INTO RPT-D-TEXT
               END-STRING
           ELSE
               MOVE 'MATURED - CREDITED TO ACCOUNT' TO RPT-D-TEXT
           END-IF
           MOVE DPP-NET TO RPT-D-AMOUNT
           MOVE RPT-DETAIL-LINE TO WS-RPT-WORK
           PERFORM 8000-EMIT.

       2300-COUNT-RD-MISSES.
           MOVE ZERO TO WS-OVERDUE-CNT
           MOVE DEP-RD-NEXT-DUE TO WS-DUE-WALK
           PERFORM UNTIL WS-DUE-WALK >= WS-RUN-DATE
                      OR DEP-RD-PAID-CNT + WS-OVERDUE-CNT >= DEP-TENURE
               ADD 1 TO WS-OVERDUE-CNT
               MOVE WS-DUE-WALK TO DU-DATE-1
               MOVE 1           TO DU-NUMBER
               SET DU-ADD-MONTHS TO TRUE
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DU-RESULT-DATE TO WS-DUE-WALK
           END-PERFORM
           IF WS-OVERDUE-CNT NOT = DEP-RD-MISSED-CNT
               MOVE WS-OVERDUE-CNT TO DEP-RD-MISSED-CNT
               REWRITE DEP-RECORD
           END-IF
           IF WS-OVERDUE-CNT > ZERO
               ADD 1 TO WS-DP-RD-MISSED
           END-IF.

      *================================================================*
      * STEP 3 - CARDS                                                 *
      *================================================================*
       3000-CARD-STEP.
           MOVE SPACES TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE 'STEP 3 - DEBIT CARDS' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           OPEN I-O CARD-FILE
           IF NOT CARD-FS-OK
               MOVE '  NO CARD MASTER - STEP SKIPPED' TO WS-RPT-WORK
               PERFORM 8000-EMIT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ CARD-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-CD-READ
                       IF (CARD-ACTIVE OR CARD-PIN-LOCKED)
                          AND CARD-EXPIRY-YYYYMM < WS-RUN-YYYYMM
                           SET CARD-EXPIRED TO TRUE
                           REWRITE CARD-RECORD
                           ADD 1 TO WS-CD-EXPIRED
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CARD-FILE
           MOVE WS-CD-READ TO WS-DISP-CNT
           STRING '  CARDS READ        : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-CD-EXPIRED TO WS-DISP-CNT
           STRING '  MARKED EXPIRED    : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT.

      *================================================================*
      * STEP 3A - MAKER-CHECKER ITEMS NOT DECIDED BY THEIR OWN DAY     *
      * EXPIRE: THE CUSTOMER'S POSITION MAY HAVE CHANGED OVERNIGHT, SO *
      * THE TELLER MUST ENTER THEM AGAIN.                              *
      *================================================================*
       3500-EXPIRE-APPROVALS.
           INITIALIZE MKR-PARMS
           SET MKR-EXPIRE TO TRUE
           MOVE WS-RUN-DATE TO MKR-RUN-DATE
           CALL 'MKRQUEUE' USING MKR-PARMS
           MOVE MKR-PENDING TO WS-DISP-CNT
           STRING '  APPROVALS EXPIRED : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT.

      *================================================================*
      * STEP 4 - DAILY TRANSACTION SUMMARY                             *
      *================================================================*
       4000-SUMMARY-STEP.
           MOVE SPACES TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE 'STEP 4 - TRANSACTIONS POSTED ON RUN DATE'
                                                 TO WS-RPT-WORK
           PERFORM 8000-EMIT
           OPEN INPUT TRAN-FILE
           IF NOT TRAN-FS-OK
               MOVE '  NO TRANSACTION LOG' TO WS-RPT-WORK
               PERFORM 8000-EMIT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ TRAN-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF TRAN-DATE = WS-RUN-DATE
                           PERFORM 4100-ACCUMULATE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TRAN-FILE

           MOVE 'TYPE                      COUNT              AMOUNT'
                                                 TO WS-RPT-WORK
           PERFORM 8000-EMIT
           PERFORM VARYING WS-T FROM 1 BY 1 UNTIL WS-T > 23
               IF TT-COUNT(WS-T) > ZERO
                   MOVE TYP-DESC(WS-T)   TO RPT-S-DESC
                   MOVE TT-COUNT(WS-T)   TO RPT-S-COUNT
                   MOVE TT-AMOUNT(WS-T)  TO RPT-S-AMOUNT
                   MOVE RPT-SUMMARY-LINE TO WS-RPT-WORK
                   PERFORM 8000-EMIT
               END-IF
           END-PERFORM
           MOVE 'TOTAL CREDITS' TO RPT-S-DESC
           MOVE ZERO            TO RPT-S-COUNT
           MOVE WS-DAY-CREDITS  TO RPT-S-AMOUNT
           MOVE RPT-SUMMARY-LINE TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE 'TOTAL DEBITS'  TO RPT-S-DESC
           MOVE WS-DAY-DEBITS   TO RPT-S-AMOUNT
           MOVE RPT-SUMMARY-LINE TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE 'ALL TRANSACTIONS' TO RPT-S-DESC
           MOVE WS-DAY-COUNT    TO RPT-S-COUNT
           MOVE ZERO            TO RPT-S-AMOUNT
           MOVE RPT-SUMMARY-LINE TO WS-RPT-WORK
           PERFORM 8000-EMIT
           IF WS-UNKNOWN-TYPES > ZERO
               MOVE WS-UNKNOWN-TYPES TO WS-DISP-CNT
               STRING '  WARNING - UNKNOWN TRAN TYPES: ' WS-DISP-CNT
                      DELIMITED BY SIZE INTO WS-RPT-WORK
               PERFORM 8000-EMIT
               PERFORM 8900-SET-RC-WARNING
           END-IF.

       4100-ACCUMULATE.
           ADD 1 TO WS-DAY-COUNT
           IF TRAN-CREDIT-TYPE
               ADD TRAN-AMOUNT TO WS-DAY-CREDITS
           ELSE
               ADD TRAN-AMOUNT TO WS-DAY-DEBITS
           END-IF
           SET TYP-IDX TO 1
           SEARCH WS-TYPE-ENTRY
               AT END
                   ADD 1 TO WS-UNKNOWN-TYPES
               WHEN TYP-CODE(TYP-IDX) = TRAN-TYPE
                   SET WS-T TO TYP-IDX
                   ADD 1           TO TT-COUNT(WS-T)
                   ADD TRAN-AMOUNT TO TT-AMOUNT(WS-T)
           END-SEARCH.

      *================================================================*
      * COMMON                                                         *
      *================================================================*
       8000-EMIT.
           MOVE WS-RPT-WORK TO RPT-LINE
           WRITE RPT-LINE
           DISPLAY WS-RPT-WORK
           MOVE SPACES TO WS-RPT-WORK.

       8100-STEP-HEADER-LOAN.
           MOVE 'STEP 1 - LOANS' TO WS-RPT-WORK
           PERFORM 8000-EMIT.

       8900-SET-RC-WARNING.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.

       9000-FINISH.
           MOVE SPACES TO WS-RPT-WORK
           PERFORM 8000-EMIT
           STRING 'EOD COMPLETE. RETURN CODE ' WS-RETURN-CODE
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT
           CLOSE RPT-FILE
           DISPLAY 'REPORT SAVED TO ' WS-RPT-FILENAME
           MOVE WS-RETURN-CODE TO RETURN-CODE.
