       IDENTIFICATION DIVISION.
       PROGRAM-ID. AMLCHK.
      *================================================================*
      * PROGRAM  : AMLCHK                                              *
      * PURPOSE  : ANTI-MONEY-LAUNDERING SCREEN, CALLED BY ACCTPOST    *
      *            AFTER EVERY POSTING. KEEPS A PER-ACCOUNT, PER-DAY   *
      *            TALLY AND RAISES AN ALERT WHEN A RULE IS MET:       *
      *              CTR SINGLE   - ONE CASH TXN OF 10 LAKH OR MORE    *
      *              CTR DAILY-IN - CASH DEPOSITS IN A DAY REACH 10 L  *
      *              CTR DAILY-OUT- CASH WITHDRAWN IN A DAY REACHES 10L*
      *              STR STRUCTURE- 3RD CASH DEPOSIT OF 45,000-49,999  *
      *                             IN A DAY (SPLITTING TO STAY UNDER  *
      *                             THE 50,000 PAN THRESHOLD)          *
      *              STR PASSTHRU - 5 LAKH+ CREDITED AND 90%+ OF IT    *
      *                             TAKEN OUT AGAIN THE SAME DAY       *
      *            EACH DAILY RULE FIRES ONCE PER ACCOUNT PER DAY.     *
      *            A SCREENING FAILURE NEVER BLOCKS THE POSTING.       *
      * CALL     : CALL 'AMLCHK' USING AML-PARMS  (COPY AMLPARM)       *
      * FILES    : AMLDAY (INDEXED, I-O), AMLALERT (SEQUENTIAL, EXTEND)*
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DAY-FILE ASSIGN TO 'data/AMLDAY.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS AD-KEY
               FILE STATUS IS WS-DAY-FS.
           SELECT ALERT-FILE ASSIGN TO 'data/AMLALERT.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ALERT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  DAY-FILE.
           COPY AMLDREC.
       FD  ALERT-FILE.
           COPY AMLAREC.

       WORKING-STORAGE SECTION.
       01  WS-DAY-FS                   PIC X(02).
           88  DAY-FS-OK                         VALUE '00'.
           88  DAY-FS-NOFILE                     VALUE '35'.
       01  WS-ALERT-FS                 PIC X(02).
       01  WS-NEW-SW                   PIC X(01).
           88  NEW-DAY-RECORD                    VALUE 'Y'.
       01  WS-CASH-SW                  PIC X(01).
           88  IS-CASH                           VALUE 'Y'.
       01  WS-PRIOR                    PIC 9(13)V99.

      * THRESHOLDS
       01  WS-CTR-LIMIT                PIC 9(13)V99 VALUE 1000000.00.
       01  WS-NEAR-LOW                 PIC 9(13)V99 VALUE 45000.00.
       01  WS-NEAR-HIGH                PIC 9(13)V99 VALUE 49999.99.
       01  WS-STRUCT-COUNT             PIC 9(03) VALUE 3.
       01  WS-PASSTHRU-MIN             PIC 9(13)V99 VALUE 500000.00.
       01  WS-PASSTHRU-PCT             PIC 9V99 VALUE 0.90.

       LINKAGE SECTION.
           COPY AMLPARM.

       PROCEDURE DIVISION USING AML-PARMS.
       0000-MAIN.
           OPEN I-O DAY-FILE
           IF DAY-FS-NOFILE
               OPEN OUTPUT DAY-FILE
               CLOSE DAY-FILE
               OPEN I-O DAY-FILE
           END-IF
           IF NOT DAY-FS-OK
               DISPLAY 'AMLCHK: WARNING - TALLY FILE UNAVAILABLE FS='
                       WS-DAY-FS
               GOBACK
           END-IF
           PERFORM 1000-LOAD-TALLY
           PERFORM 2000-UPDATE-TALLY
           PERFORM 3000-APPLY-RULES
           IF NEW-DAY-RECORD
               WRITE AMLDAY-RECORD
           ELSE
               REWRITE AMLDAY-RECORD
           END-IF
           CLOSE DAY-FILE
           GOBACK.

       1000-LOAD-TALLY.
           MOVE AM-ACCT-NO TO AD-ACCT-NO
           MOVE AM-DATE    TO AD-DATE
           MOVE 'N' TO WS-NEW-SW
           READ DAY-FILE
               INVALID KEY
                   MOVE 'Y' TO WS-NEW-SW
                   INITIALIZE AMLDAY-RECORD
                   MOVE AM-ACCT-NO TO AD-ACCT-NO
                   MOVE AM-DATE    TO AD-DATE
                   MOVE 'NNNN'     TO AD-FLAGS
           END-READ.

       2000-UPDATE-TALLY.
           IF AM-TRAN-TYPE = 'DP' OR 'WD' OR 'AW'
               MOVE 'Y' TO WS-CASH-SW
           ELSE
               MOVE 'N' TO WS-CASH-SW
           END-IF
           IF AM-IS-CREDIT
               ADD AM-AMOUNT TO AD-ALL-CREDITS
           ELSE
               ADD AM-AMOUNT TO AD-ALL-DEBITS
           END-IF.

       3000-APPLY-RULES.
           IF IS-CASH
               IF AM-AMOUNT >= WS-CTR-LIMIT
                   MOVE 'CTR' TO AL-KIND
                   MOVE 'SINGLE' TO AL-RULE
                   MOVE AM-AMOUNT TO AL-AMOUNT
                   PERFORM 9000-RAISE
               END-IF
               IF AM-IS-CREDIT
                   PERFORM 3100-CASH-IN-RULES
               ELSE
                   PERFORM 3200-CASH-OUT-RULES
               END-IF
           END-IF
           PERFORM 3300-PASS-THROUGH-RULE.

       3100-CASH-IN-RULES.
           MOVE AD-CASH-IN TO WS-PRIOR
           ADD AM-AMOUNT TO AD-CASH-IN
           IF AD-CASH-IN >= WS-CTR-LIMIT AND AD-CTR-IN-RAISED = 'N'
              AND AM-AMOUNT < WS-CTR-LIMIT
               MOVE 'Y' TO AD-CTR-IN-RAISED
               MOVE 'CTR' TO AL-KIND
               MOVE 'DAILY-IN' TO AL-RULE
               MOVE AD-CASH-IN TO AL-AMOUNT
               PERFORM 9000-RAISE
           END-IF
           IF AM-AMOUNT >= WS-NEAR-LOW AND AM-AMOUNT <= WS-NEAR-HIGH
               ADD 1 TO AD-NEAR-LIMIT-CNT
               IF AD-NEAR-LIMIT-CNT >= WS-STRUCT-COUNT
                  AND AD-STRUCT-RAISED = 'N'
                   MOVE 'Y' TO AD-STRUCT-RAISED
                   MOVE 'STR' TO AL-KIND
                   MOVE 'STRUCTURING' TO AL-RULE
                   MOVE AD-CASH-IN TO AL-AMOUNT
                   PERFORM 9000-RAISE
               END-IF
           END-IF.

       3200-CASH-OUT-RULES.
           ADD AM-AMOUNT TO AD-CASH-OUT
           IF AD-CASH-OUT >= WS-CTR-LIMIT AND AD-CTR-OUT-RAISED = 'N'
              AND AM-AMOUNT < WS-CTR-LIMIT
               MOVE 'Y' TO AD-CTR-OUT-RAISED
               MOVE 'CTR' TO AL-KIND
               MOVE 'DAILY-OUT' TO AL-RULE
               MOVE AD-CASH-OUT TO AL-AMOUNT
               PERFORM 9000-RAISE
           END-IF.

       3300-PASS-THROUGH-RULE.
           IF AM-IS-DEBIT
              AND AD-PASSTHRU-RAISED = 'N'
              AND AD-ALL-CREDITS >= WS-PASSTHRU-MIN
              AND AD-ALL-DEBITS >= AD-ALL-CREDITS * WS-PASSTHRU-PCT
               MOVE 'Y' TO AD-PASSTHRU-RAISED
               MOVE 'STR' TO AL-KIND
               MOVE 'PASSTHRU' TO AL-RULE
               MOVE AD-ALL-DEBITS TO AL-AMOUNT
               PERFORM 9000-RAISE
           END-IF.

       9000-RAISE.
           MOVE AM-DATE    TO AL-DATE
           MOVE AM-TIME    TO AL-TIME
           MOVE AM-ACCT-NO TO AL-ACCT-NO
           MOVE AM-NAME    TO AL-NAME
           OPEN EXTEND ALERT-FILE
           IF WS-ALERT-FS = '35'
               OPEN OUTPUT ALERT-FILE
           END-IF
           IF WS-ALERT-FS = '00'
               WRITE ALERT-RECORD
               CLOSE ALERT-FILE
           ELSE
               DISPLAY 'AMLCHK: WARNING - ALERT NOT WRITTEN FOR '
                       AM-ACCT-NO
           END-IF.
