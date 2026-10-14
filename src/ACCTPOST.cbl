       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTPOST.
      *================================================================*
      * PROGRAM  : ACCTPOST                                            *
      * PURPOSE  : COMMON ACCOUNT POSTING ROUTINE. ONE CALL = ONE      *
      *            DEBIT, CREDIT OR INQUIRY AGAINST THE ACCOUNT        *
      *            MASTER, WITH A MATCHING TRANSACTION LOG ENTRY.      *
      *            LOANS, CARDS, DEPOSITS AND EOD ALL POST THROUGH     *
      *            HERE SO THE BALANCE RULES LIVE IN ONE PLACE.        *
      * CALL     : CALL 'ACCTPOST' USING POST-PARMS  (COPY POSTPARM)   *
      * FILES    : ACCTMAST (INDEXED, I-O)                             *
      *            TRANLOG  (SEQUENTIAL, EXTEND)                       *
      * NOTE     : FILES ARE OPENED AND CLOSED ON EVERY CALL. CALLERS  *
      *            MUST NOT HAVE ACCTMAST OPEN WHEN THEY CALL THIS.    *
      *            IN BATCH MODE (BATCHCTL 'S') THEY STAY OPEN UNTIL   *
      *            BATCHCTL 'E'.                                       *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      * 2026-10-14 AUDIT TRAIL ENTRY FOR EVERY POSTING                 *
      * 2026-10-14 GL DOUBLE ENTRY, OVERDRAFT LIMIT, DORMANCY RULES,   *
      *            AML MONITORING HOOK                                 *
      * 2026-10-14 BATCH KEEP-FILES-OPEN MODE (BATMODE / BATCHCTL)     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE ASSIGN TO 'data/ACCTMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
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
           COPY AUDPARM.
           COPY GLPARM.
           COPY GLCODES.
           COPY AMLPARM.
       01  WS-AUD-AMOUNT               PIC Z(10)9.99.

      * SEQUENCE STARTS HIGH TO STAY CLEAR OF TRANPOST / INTCALC IDS
       01  WS-TRAN-SEQ                 PIC 9(04) VALUE 2000.
       01  WS-AVAILABLE                PIC S9(13)V99 COMP-3.
       01  WS-CUSTOMER-GL              PIC 9(04).
       01  WS-CONTRA-GL                PIC 9(04).
           COPY BATMODE.
       01  WS-ACCT-OPEN-SW             PIC X(01) VALUE 'N'.
           88  ACCT-IS-OPEN                      VALUE 'Y'.
       01  WS-TRAN-OPEN-SW             PIC X(01) VALUE 'N'.
           88  TRAN-IS-OPEN                      VALUE 'Y'.

       LINKAGE SECTION.
           COPY POSTPARM.

       PROCEDURE DIVISION USING POST-PARMS.
       0000-MAIN.
           IF CB-CLOSE-ALL
               PERFORM 9000-CLOSE-FILES
               GOBACK
           END-IF
           SET PP-OK TO TRUE
           MOVE SPACES TO PP-HOLDER-NAME PP-ACCT-TYPE
           MOVE ZERO   TO PP-BALANCE PP-AVAILABLE
           IF NOT (PP-DEBIT OR PP-CREDIT OR PP-INQUIRE)
               SET PP-BAD-REQUEST TO TRUE
               GOBACK
           END-IF
           IF (PP-DEBIT OR PP-CREDIT) AND PP-AMOUNT NOT > ZERO
               SET PP-BAD-REQUEST TO TRUE
               GOBACK
           END-IF

           IF NOT ACCT-IS-OPEN
               OPEN I-O ACCT-FILE
               IF NOT ACCT-FS-OK
                   SET PP-FILE-ERROR TO TRUE
                   GOBACK
               END-IF
               SET ACCT-IS-OPEN TO TRUE
           END-IF

           PERFORM 1000-READ-ACCOUNT
           IF PP-OK AND NOT PP-INQUIRE
               PERFORM 2000-APPLY-POSTING
           END-IF
           IF NOT CB-KEEP-OPEN
               PERFORM 9000-CLOSE-FILES
           END-IF
           GOBACK.

      *    STATUS RULES:
      *      ACTIVE  - EVERYTHING ALLOWED
      *      DORMANT - CREDITS AND BANK-INITIATED DEBITS ONLY; A
      *                CUSTOMER DEBIT NEEDS REACTIVATION FIRST
      *      FROZEN / CLOSED - NOTHING
       1000-READ-ACCOUNT.
           MOVE PP-ACCT-NO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   SET PP-NOT-FOUND TO TRUE
                   EXIT PARAGRAPH
           END-READ
           PERFORM 1100-RETURN-SNAPSHOT
           IF PP-INQUIRE OR ACCT-ACTIVE
               EXIT PARAGRAPH
           END-IF
           IF ACCT-DORMANT AND (PP-CREDIT OR PP-IS-SYSTEM-TXN)
               EXIT PARAGRAPH
           END-IF
           SET PP-NOT-ACTIVE TO TRUE.

      *    AVAILABLE = BALANCE - MINIMUM BALANCE (SAVINGS), OR
      *                BALANCE + OVERDRAFT LIMIT (CURRENT)
       1100-RETURN-SNAPSHOT.
           MOVE ACCT-HOLDER-NAME TO PP-HOLDER-NAME
           MOVE ACCT-TYPE        TO PP-ACCT-TYPE
           MOVE ACCT-BALANCE     TO PP-BALANCE
           IF ACCT-CURRENT
               COMPUTE WS-AVAILABLE = ACCT-BALANCE + ACCT-OD-LIMIT
           ELSE
               COMPUTE WS-AVAILABLE = ACCT-BALANCE - ACCT-MIN-BALANCE
           END-IF
           IF WS-AVAILABLE < ZERO
               MOVE ZERO TO WS-AVAILABLE
           END-IF
           MOVE WS-AVAILABLE     TO PP-AVAILABLE.

       2000-APPLY-POSTING.
           IF PP-DEBIT
      *        BANK CHARGES MAY TAKE A SAVINGS ACCOUNT BELOW ITS
      *        MINIMUM BUT NEVER BELOW ZERO (OR BEYOND THE OD LIMIT)
               IF PP-ALLOW-BELOW-MIN AND ACCT-SAVINGS
                   MOVE ACCT-BALANCE TO WS-AVAILABLE
               END-IF
               IF PP-AMOUNT > WS-AVAILABLE
                   SET PP-INSUFFICIENT TO TRUE
                   EXIT PARAGRAPH
               END-IF
               SUBTRACT PP-AMOUNT FROM ACCT-BALANCE
           ELSE
               ADD PP-AMOUNT TO ACCT-BALANCE
           END-IF

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
      *    ONLY CUSTOMER ACTIVITY COUNTS TOWARDS DORMANCY
           IF NOT PP-IS-SYSTEM-TXN
               MOVE WS-CURR-DATE TO ACCT-LAST-TXN-DATE
           END-IF
           REWRITE ACCT-RECORD
               INVALID KEY
                   SET PP-FILE-ERROR TO TRUE
                   EXIT PARAGRAPH
           END-REWRITE
           PERFORM 1100-RETURN-SNAPSHOT
           PERFORM 3000-LOG-TRANSACTION
           PERFORM 4000-AUDIT-POSTING
           PERFORM 5000-POST-TO-GL
           PERFORM 6000-AML-CHECK.

       3000-LOG-TRANSACTION.
           IF NOT TRAN-IS-OPEN
               OPEN EXTEND TRAN-FILE
               IF TRAN-FS-NOFILE
                   OPEN OUTPUT TRAN-FILE
               END-IF
               IF NOT TRAN-FS-OK
                   DISPLAY 'ACCTPOST: WARNING - TRAN LOG UNAVAILABLE'
                           ' FS=' WS-TRAN-FS
                   EXIT PARAGRAPH
               END-IF
               SET TRAN-IS-OPEN TO TRUE
           END-IF
           ADD 1 TO WS-TRAN-SEQ
           COMPUTE TRAN-ID = WS-CURR-DATE * 10000 + WS-TRAN-SEQ
           MOVE ACCT-NO       TO TRAN-ACCT-NO
           MOVE WS-CURR-DATE  TO TRAN-DATE
           MOVE WS-CURR-TIME  TO TRAN-TIME
           MOVE PP-TRAN-TYPE  TO TRAN-TYPE
           MOVE PP-AMOUNT     TO TRAN-AMOUNT
           MOVE ACCT-BALANCE  TO TRAN-BAL-AFTER
           MOVE PP-REF-ACCT   TO TRAN-REF-ACCT
           MOVE PP-NARRATION  TO TRAN-NARRATION
           WRITE TRAN-RECORD.

      *    EVERY MONEY MOVEMENT IS ALSO IN THE AUDIT TRAIL WITH THE
      *    SIGNED-ON USER, SO TRANLOG + AUDITLOG CAN BE RECONCILED.
       4000-AUDIT-POSTING.
           MOVE 'ACCTPOST' TO AU-PROGRAM
           IF PP-DEBIT
               MOVE 'DEBIT'  TO AU-ACTION
           ELSE
               MOVE 'CREDIT' TO AU-ACTION
           END-IF
           MOVE ACCT-NO TO AU-KEY
           MOVE PP-AMOUNT TO WS-AUD-AMOUNT
           MOVE SPACES TO AU-DETAIL
           STRING PP-TRAN-TYPE ' ' FUNCTION TRIM(WS-AUD-AMOUNT) ' '
                  PP-NARRATION
                  DELIMITED BY SIZE INTO AU-DETAIL
           END-STRING
           CALL 'AUDITLOG' USING AUD-PARMS.

      *----------------------------------------------------------------*
      * DOUBLE ENTRY. THE CUSTOMER SIDE IS THE DEPOSIT CONTROL GL FOR  *
      * THE ACCOUNT TYPE; THE OTHER SIDE COMES FROM THE CALLER OR, BY  *
      * DEFAULT, FROM THE TRANSACTION TYPE.                            *
      *----------------------------------------------------------------*
       5000-POST-TO-GL.
           IF ACCT-SAVINGS
               MOVE GLC-SAVINGS-DEP TO WS-CUSTOMER-GL
           ELSE
               MOVE GLC-CURRENT-DEP TO WS-CUSTOMER-GL
           END-IF
           IF PP-CONTRA-GL > ZERO
               MOVE PP-CONTRA-GL TO WS-CONTRA-GL
           ELSE
               PERFORM 5100-DEFAULT-CONTRA
           END-IF
           IF PP-CREDIT
               MOVE WS-CONTRA-GL   TO GP-DR-GL
               MOVE WS-CUSTOMER-GL TO GP-CR-GL
           ELSE
               MOVE WS-CUSTOMER-GL TO GP-DR-GL
               MOVE WS-CONTRA-GL   TO GP-CR-GL
           END-IF
           MOVE PP-AMOUNT    TO GP-AMOUNT
           MOVE ACCT-NO      TO GP-REF
           MOVE PP-NARRATION TO GP-NARRATION
           CALL 'GLPOST' USING GL-PARMS
           IF NOT GP-OK
               DISPLAY 'ACCTPOST: WARNING - GL ENTRY FAILED RC=' GP-RC
                       ' FOR ' ACCT-NO
           END-IF.

       5100-DEFAULT-CONTRA.
           EVALUATE PP-TRAN-TYPE
               WHEN 'DP'
               WHEN 'WD'  MOVE GLC-CASH            TO WS-CONTRA-GL
               WHEN 'AW'  MOVE GLC-ATM-CASH        TO WS-CONTRA-GL
               WHEN 'IN'  MOVE GLC-SAV-INT-EXPENSE TO WS-CONTRA-GL
               WHEN 'OI'  MOVE GLC-OD-INT-INCOME   TO WS-CONTRA-GL
               WHEN 'LD'  MOVE GLC-LOANS           TO WS-CONTRA-GL
               WHEN 'LR'  MOVE GLC-LOAN-REPAY-SUSP TO WS-CONTRA-GL
               WHEN 'FO'
               WHEN 'FM'
               WHEN 'RI'  MOVE GLC-TERM-DEP        TO WS-CONTRA-GL
               WHEN 'CH'  MOVE GLC-FEE-INCOME      TO WS-CONTRA-GL
               WHEN 'CQ'  MOVE GLC-CLEARING        TO WS-CONTRA-GL
               WHEN 'CP'  MOVE GLC-CARD-RECEIVABLE TO WS-CONTRA-GL
               WHEN 'ND'  MOVE GLC-NACH-SETTLE     TO WS-CONTRA-GL
               WHEN 'OC'  MOVE GLC-CLEARING        TO WS-CONTRA-GL
               WHEN 'TF'
               WHEN 'TR'  MOVE GLC-MARGIN-MONEY    TO WS-CONTRA-GL
               WHEN 'XI'
               WHEN 'XO'  MOVE GLC-NOSTRO          TO WS-CONTRA-GL
               WHEN 'PO'
               WHEN 'PI'
               WHEN 'MR'  MOVE GLC-PAYMENT-SETTLE  TO WS-CONTRA-GL
               WHEN 'MP'
               WHEN 'IP'  MOVE GLC-MF-COLLECTIONS  TO WS-CONTRA-GL
               WHEN 'CM'  MOVE GLC-COMPENSATION-EXP TO WS-CONTRA-GL
               WHEN OTHER MOVE GLC-TRANSFER-SUSP   TO WS-CONTRA-GL
           END-EVALUATE.

      *    CASH AND LARGE-VALUE MONITORING (CTR / STR RULES IN AMLCHK)
       6000-AML-CHECK.
           MOVE ACCT-NO       TO AM-ACCT-NO
           MOVE PP-TRAN-TYPE  TO AM-TRAN-TYPE
           MOVE PP-FUNCTION   TO AM-DR-CR
           MOVE PP-AMOUNT     TO AM-AMOUNT
           MOVE WS-CURR-DATE  TO AM-DATE
           MOVE WS-CURR-TIME  TO AM-TIME
           MOVE ACCT-HOLDER-NAME TO AM-NAME
           CALL 'AMLCHK' USING AML-PARMS.

      *    CLOSES WHAT IS OPEN. ONLINE: AFTER EVERY CALL. BATCH: ONLY
      *    WHEN BATCHCTL 'E' ENDS THE RUN.
       9000-CLOSE-FILES.
           IF ACCT-IS-OPEN
               CLOSE ACCT-FILE
               MOVE 'N' TO WS-ACCT-OPEN-SW
           END-IF
           IF TRAN-IS-OPEN
               CLOSE TRAN-FILE
               MOVE 'N' TO WS-TRAN-OPEN-SW
           END-IF.
