       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTMNT.
      *================================================================*
      * PROGRAM  : ACCTMNT                                             *
      * PURPOSE  : ACCOUNT MAINTENANCE - OPEN, INQUIRE, UPDATE         *
      *            CONTACT DETAILS, FREEZE/UNFREEZE AND CLOSE.         *
      * FILES    : ACCTMAST (INDEXED, I-O)                             *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      * 2026-10-14 OPENING DEPOSIT WRITTEN TO TRANSACTION LOG          *
      * 2026-10-14 GL + AML FOR OPENING DEPOSIT, DORMANT REACTIVATION, *
      *            OVERDRAFT LIMITS                                    *
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
           COPY AMLPARM.
           COPY SESSION.
           COPY AUDPARM.
       01  WS-IN-OD-LIMIT              PIC 9(09)V99.
       01  WS-IN-OD-RATE               PIC 9(02)V9(04).
       01  WS-MAX-OD-LIMIT             PIC 9(09)V99 VALUE 5000000.00.
       01  WS-DEFAULT-OD-RATE          PIC 9(02)V9(04) VALUE 12.0000.

       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
           88  CHOICE-BACK                       VALUE '9'.
       01  WS-CONFIRM                  PIC X(01) VALUE SPACE.
           88  CONFIRM-YES                       VALUES 'Y' 'y'.

       01  WS-INPUT.
           05  WS-IN-ACCT-NO           PIC 9(10).
           05  WS-IN-NAME              PIC X(40).
           05  WS-IN-TYPE              PIC X(01).
           05  WS-IN-DEPOSIT           PIC 9(09)V99.
           05  WS-IN-PHONE             PIC X(10).
           05  WS-IN-BRANCH            PIC X(06).

       01  WS-NEXT-ACCT-NO             PIC 9(10) VALUE ZERO.
       01  WS-VALID-SW                 PIC X(01) VALUE 'Y'.
           88  INPUT-VALID                       VALUE 'Y'.
           88  INPUT-INVALID                     VALUE 'N'.

       01  WS-DISP-BAL                 PIC -Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-RATE                PIC Z9.9999.

      * BUSINESS PARAMETERS
       01  WS-PARMS.
           05  WS-SAV-MIN-OPEN         PIC 9(09)V99 VALUE 1000.00.
           05  WS-CUR-MIN-OPEN         PIC 9(09)V99 VALUE 5000.00.
           05  WS-SAV-INT-RATE         PIC 9(02)V9(04) VALUE 03.5000.
           05  WS-ACCT-NO-BASE         PIC 9(10) VALUE 1000000000.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-OPEN-FILE
           IF NOT ACCT-FS-OK
               GOBACK
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL CHOICE-BACK
               PERFORM 1100-SHOW-MENU
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-OPEN-ACCOUNT
                   WHEN '2' PERFORM 3000-INQUIRE-ACCOUNT
                   WHEN '3' PERFORM 4000-UPDATE-CONTACT
                   WHEN '4' PERFORM 5000-FREEZE-TOGGLE
                   WHEN '5' PERFORM 6000-CLOSE-ACCOUNT
                   WHEN '6' PERFORM 6500-REACTIVATE-DORMANT
                   WHEN '7' PERFORM 6700-SET-OD-LIMIT
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE ACCT-FILE
           GOBACK.

      *----------------------------------------------------------------*
      * OPEN MASTER IN I-O. IF IT DOES NOT EXIST YET, CREATE IT.       *
      *----------------------------------------------------------------*
       1000-OPEN-FILE.
           OPEN I-O ACCT-FILE
           IF ACCT-FS-NOFILE
               OPEN OUTPUT ACCT-FILE
               CLOSE ACCT-FILE
               OPEN I-O ACCT-FILE
           END-IF
           IF NOT ACCT-FS-OK
               DISPLAY 'ERROR OPENING ACCOUNT MASTER, FS=' WS-ACCT-FS
           END-IF.

       1100-SHOW-MENU.
           DISPLAY ' '
           DISPLAY '---------- ACCOUNT MAINTENANCE ----------'
           DISPLAY ' 1. OPEN NEW ACCOUNT'
           DISPLAY ' 2. ACCOUNT INQUIRY'
           DISPLAY ' 3. UPDATE PHONE / BRANCH'
           DISPLAY ' 4. FREEZE / UNFREEZE ACCOUNT'
           DISPLAY ' 5. CLOSE ACCOUNT'
           IF SES-CAN-APPROVE
               DISPLAY ' 6. REACTIVATE DORMANT ACCOUNT (MANAGER)'
               DISPLAY ' 7. SET OVERDRAFT LIMIT (MANAGER)'
           END-IF
           DISPLAY ' 9. BACK TO MAIN MENU'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT.

      *----------------------------------------------------------------*
      * OPEN ACCOUNT                                                   *
      *----------------------------------------------------------------*
       2000-OPEN-ACCOUNT.
           SET INPUT-VALID TO TRUE
           PERFORM 2100-ACCEPT-NEW-DETAILS
           IF INPUT-VALID
               PERFORM 2200-VALIDATE-NEW-DETAILS
           END-IF
           IF INPUT-VALID
               PERFORM 2300-GET-NEXT-ACCT-NO
               PERFORM 2400-BUILD-AND-WRITE
           END-IF.

       2100-ACCEPT-NEW-DETAILS.
           DISPLAY 'HOLDER NAME         : ' WITH NO ADVANCING
           ACCEPT WS-IN-NAME
           DISPLAY 'TYPE (S=SAV/C=CURR) : ' WITH NO ADVANCING
           ACCEPT WS-IN-TYPE
           MOVE FUNCTION UPPER-CASE(WS-IN-TYPE) TO WS-IN-TYPE
           DISPLAY 'OPENING DEPOSIT     : ' WITH NO ADVANCING
           ACCEPT WS-IN-DEPOSIT
           DISPLAY 'PHONE (10 DIGITS)   : ' WITH NO ADVANCING
           ACCEPT WS-IN-PHONE
           DISPLAY 'BRANCH CODE         : ' WITH NO ADVANCING
           ACCEPT WS-IN-BRANCH.

       2200-VALIDATE-NEW-DETAILS.
           IF WS-IN-NAME = SPACES
               DISPLAY 'NAME CANNOT BE BLANK.'
               SET INPUT-INVALID TO TRUE
           END-IF
           MOVE WS-IN-TYPE TO ACCT-TYPE
           IF NOT ACCT-TYPE-VALID
               DISPLAY 'ACCOUNT TYPE MUST BE S OR C.'
               SET INPUT-INVALID TO TRUE
           END-IF
           IF ACCT-SAVINGS AND WS-IN-DEPOSIT < WS-SAV-MIN-OPEN
               DISPLAY 'MINIMUM OPENING DEPOSIT FOR SAVINGS IS 1000.'
               SET INPUT-INVALID TO TRUE
           END-IF
           IF ACCT-CURRENT AND WS-IN-DEPOSIT < WS-CUR-MIN-OPEN
               DISPLAY 'MINIMUM OPENING DEPOSIT FOR CURRENT IS 5000.'
               SET INPUT-INVALID TO TRUE
           END-IF
           IF WS-IN-PHONE NOT NUMERIC
               DISPLAY 'PHONE MUST BE 10 DIGITS.'
               SET INPUT-INVALID TO TRUE
           END-IF.

      *----------------------------------------------------------------*
      * NEXT ACCOUNT NUMBER = HIGHEST KEY ON FILE + 1                  *
      *----------------------------------------------------------------*
       2300-GET-NEXT-ACCT-NO.
           MOVE WS-ACCT-NO-BASE TO WS-NEXT-ACCT-NO
           MOVE 9999999999 TO ACCT-NO
           START ACCT-FILE KEY IS LESS THAN OR EQUAL ACCT-NO
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   READ ACCT-FILE PREVIOUS RECORD
                       AT END CONTINUE
                       NOT AT END
                           MOVE ACCT-NO TO WS-NEXT-ACCT-NO
                   END-READ
           END-START
           ADD 1 TO WS-NEXT-ACCT-NO.

       2400-BUILD-AND-WRITE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           INITIALIZE ACCT-RECORD
           MOVE WS-NEXT-ACCT-NO       TO ACCT-NO
           MOVE WS-IN-NAME            TO ACCT-HOLDER-NAME
           MOVE WS-IN-TYPE            TO ACCT-TYPE
           SET ACCT-ACTIVE            TO TRUE
           MOVE WS-IN-DEPOSIT         TO ACCT-BALANCE
           MOVE WS-IN-PHONE           TO ACCT-PHONE
           MOVE WS-IN-BRANCH          TO ACCT-BRANCH-CODE
           MOVE WS-CURR-DATE          TO ACCT-OPEN-DATE
                                         ACCT-LAST-TXN-DATE
                                         ACCT-LAST-INT-DATE
           IF ACCT-SAVINGS
               MOVE WS-SAV-MIN-OPEN   TO ACCT-MIN-BALANCE
               MOVE WS-SAV-INT-RATE   TO ACCT-INT-RATE
           ELSE
               MOVE ZERO              TO ACCT-MIN-BALANCE
               MOVE ZERO              TO ACCT-INT-RATE
           END-IF
           WRITE ACCT-RECORD
               INVALID KEY
                   DISPLAY 'WRITE FAILED, FS=' WS-ACCT-FS
               NOT INVALID KEY
                   DISPLAY 'ACCOUNT OPENED. ACCOUNT NO: ' ACCT-NO
                   PERFORM 2500-LOG-OPENING-DEPOSIT
           END-WRITE.

      *    THE OPENING DEPOSIT GOES IN THE TRANSACTION LOG LIKE ANY
      *    OTHER CREDIT, SO THE LOG ALWAYS RECONCILES TO THE MASTER
       2500-LOG-OPENING-DEPOSIT.
           OPEN EXTEND TRAN-FILE
           IF TRAN-FS-NOFILE
               OPEN OUTPUT TRAN-FILE
           END-IF
           IF NOT TRAN-FS-OK
               DISPLAY 'WARNING: OPENING DEPOSIT NOT LOGGED, FS='
                       WS-TRAN-FS
               EXIT PARAGRAPH
           END-IF
           COMPUTE TRAN-ID = WS-CURR-DATE * 10000 + 9000
                           + FUNCTION MOD(ACCT-NO, 1000)
           MOVE ACCT-NO       TO TRAN-ACCT-NO
           MOVE WS-CURR-DATE  TO TRAN-DATE
           MOVE WS-CURR-TIME  TO TRAN-TIME
           MOVE 'DP'          TO TRAN-TYPE
           MOVE ACCT-BALANCE  TO TRAN-AMOUNT TRAN-BAL-AFTER
           MOVE ZERO          TO TRAN-REF-ACCT
           MOVE 'ACCOUNT OPENING DEPOSIT' TO TRAN-NARRATION
           WRITE TRAN-RECORD
           CLOSE TRAN-FILE
      *    CASH RECEIVED AT THE COUNTER: DR CASH, CR DEPOSIT CONTROL
           MOVE GLC-CASH TO GP-DR-GL
           IF ACCT-SAVINGS
               MOVE GLC-SAVINGS-DEP TO GP-CR-GL
           ELSE
               MOVE GLC-CURRENT-DEP TO GP-CR-GL
           END-IF
           MOVE ACCT-BALANCE TO GP-AMOUNT
           MOVE ACCT-NO      TO GP-REF
           MOVE 'ACCOUNT OPENING DEPOSIT' TO GP-NARRATION
           CALL 'GLPOST' USING GL-PARMS
           MOVE ACCT-NO       TO AM-ACCT-NO
           MOVE 'DP'          TO AM-TRAN-TYPE
           MOVE 'C'           TO AM-DR-CR
           MOVE ACCT-BALANCE  TO AM-AMOUNT
           MOVE WS-CURR-DATE  TO AM-DATE
           MOVE WS-CURR-TIME  TO AM-TIME
           MOVE ACCT-HOLDER-NAME TO AM-NAME
           CALL 'AMLCHK' USING AML-PARMS.

      *----------------------------------------------------------------*
      * INQUIRY                                                        *
      *----------------------------------------------------------------*
       3000-INQUIRE-ACCOUNT.
           PERFORM 8000-READ-BY-INPUT-KEY
           IF ACCT-FS-OK
               PERFORM 3100-DISPLAY-ACCOUNT
           END-IF.

       3100-DISPLAY-ACCOUNT.
           MOVE ACCT-BALANCE  TO WS-DISP-BAL
           MOVE ACCT-INT-RATE TO WS-DISP-RATE
           DISPLAY '----------------------------------------'
           DISPLAY 'ACCOUNT NO   : ' ACCT-NO
           DISPLAY 'HOLDER       : ' ACCT-HOLDER-NAME
           EVALUATE TRUE
               WHEN ACCT-SAVINGS DISPLAY 'TYPE         : SAVINGS'
               WHEN ACCT-CURRENT DISPLAY 'TYPE         : CURRENT'
           END-EVALUATE
           EVALUATE TRUE
               WHEN ACCT-ACTIVE  DISPLAY 'STATUS       : ACTIVE'
               WHEN ACCT-FROZEN  DISPLAY 'STATUS       : FROZEN'
               WHEN ACCT-CLOSED  DISPLAY 'STATUS       : CLOSED'
               WHEN ACCT-DORMANT DISPLAY 'STATUS       : DORMANT'
           END-EVALUATE
           DISPLAY 'BALANCE      : ' WS-DISP-BAL
           IF ACCT-CURRENT AND ACCT-OD-LIMIT > ZERO
               MOVE ACCT-OD-LIMIT TO WS-DISP-BAL
               DISPLAY 'OD LIMIT     : ' WS-DISP-BAL
           END-IF
           DISPLAY 'INT RATE (%) : ' WS-DISP-RATE
           DISPLAY 'OPENED ON    : ' ACCT-OPEN-DATE
           DISPLAY 'LAST TXN     : ' ACCT-LAST-TXN-DATE
           DISPLAY 'PHONE        : ' ACCT-PHONE
           DISPLAY 'BRANCH       : ' ACCT-BRANCH-CODE
           DISPLAY '----------------------------------------'.

      *----------------------------------------------------------------*
      * UPDATE CONTACT DETAILS                                         *
      *----------------------------------------------------------------*
       4000-UPDATE-CONTACT.
           PERFORM 8000-READ-BY-INPUT-KEY
           IF NOT ACCT-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF ACCT-CLOSED
               DISPLAY 'ACCOUNT IS CLOSED, CANNOT UPDATE.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'NEW PHONE (BLANK=NO CHANGE) : ' WITH NO ADVANCING
           ACCEPT WS-IN-PHONE
           DISPLAY 'NEW BRANCH (BLANK=NO CHANGE): ' WITH NO ADVANCING
           ACCEPT WS-IN-BRANCH
           IF WS-IN-PHONE NOT = SPACES
               IF WS-IN-PHONE IS NUMERIC
                   MOVE WS-IN-PHONE TO ACCT-PHONE
               ELSE
                   DISPLAY 'INVALID PHONE IGNORED.'
               END-IF
           END-IF
           IF WS-IN-BRANCH NOT = SPACES
               MOVE WS-IN-BRANCH TO ACCT-BRANCH-CODE
           END-IF
           PERFORM 8100-REWRITE-ACCOUNT.

      *----------------------------------------------------------------*
      * FREEZE / UNFREEZE                                              *
      *----------------------------------------------------------------*
       5000-FREEZE-TOGGLE.
           PERFORM 8000-READ-BY-INPUT-KEY
           IF NOT ACCT-FS-OK
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN ACCT-ACTIVE
                   SET ACCT-FROZEN TO TRUE
                   MOVE 'ACCOUNT FROZEN.' TO WS-MSG
               WHEN ACCT-FROZEN
                   SET ACCT-ACTIVE TO TRUE
                   MOVE 'ACCOUNT UNFROZEN.' TO WS-MSG
               WHEN OTHER
                   DISPLAY 'CLOSED ACCOUNT CANNOT BE CHANGED.'
                   EXIT PARAGRAPH
           END-EVALUATE
           PERFORM 8100-REWRITE-ACCOUNT
           IF ACCT-FS-OK
               DISPLAY WS-MSG
           END-IF.

      *----------------------------------------------------------------*
      * CLOSE - ONLY WHEN BALANCE IS ZERO                              *
      *----------------------------------------------------------------*
       6000-CLOSE-ACCOUNT.
           PERFORM 8000-READ-BY-INPUT-KEY
           IF NOT ACCT-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF ACCT-CLOSED
               DISPLAY 'ACCOUNT IS ALREADY CLOSED.'
               EXIT PARAGRAPH
           END-IF
           IF ACCT-BALANCE NOT = ZERO
               MOVE ACCT-BALANCE TO WS-DISP-BAL
               DISPLAY 'BALANCE MUST BE ZERO TO CLOSE. CURRENT: '
                       WS-DISP-BAL
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'CONFIRM CLOSE ACCOUNT ' ACCT-NO ' (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF CONFIRM-YES
               SET ACCT-CLOSED TO TRUE
               PERFORM 8100-REWRITE-ACCOUNT
               IF ACCT-FS-OK
                   DISPLAY 'ACCOUNT CLOSED.'
               END-IF
           ELSE
               DISPLAY 'CLOSE CANCELLED.'
           END-IF.

      *----------------------------------------------------------------*
      * REACTIVATE A DORMANT ACCOUNT - MANAGER, AFTER FRESH KYC        *
      *----------------------------------------------------------------*
       6500-REACTIVATE-DORMANT.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'REACTIVATION NEEDS A MANAGER OR ADMIN.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8000-READ-BY-INPUT-KEY
           IF NOT ACCT-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF NOT ACCT-DORMANT
               DISPLAY 'ACCOUNT IS NOT DORMANT.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'FRESH KYC DOCUMENTS SEEN AND VERIFIED (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF NOT CONFIRM-YES
               DISPLAY 'REACTIVATION NEEDS UPDATED KYC.'
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           SET ACCT-ACTIVE TO TRUE
           MOVE WS-CURR-DATE TO ACCT-LAST-TXN-DATE
           PERFORM 8100-REWRITE-ACCOUNT
           IF ACCT-FS-OK
               DISPLAY 'ACCOUNT ' ACCT-NO ' REACTIVATED.'
               MOVE 'REACTIVATE' TO AU-ACTION
               MOVE SPACES TO AU-DETAIL
               PERFORM 6900-AUDIT
           END-IF.

      *----------------------------------------------------------------*
      * OVERDRAFT LIMIT - CURRENT ACCOUNTS ONLY, MANAGER APPROVAL.     *
      * A LIMIT CANNOT BE CUT BELOW WHAT IS ALREADY OVERDRAWN.         *
      *----------------------------------------------------------------*
       6700-SET-OD-LIMIT.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'OVERDRAFT LIMITS NEED A MANAGER OR ADMIN.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8000-READ-BY-INPUT-KEY
           IF NOT ACCT-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF NOT ACCT-CURRENT OR NOT ACCT-ACTIVE
               DISPLAY 'OVERDRAFT IS ONLY FOR ACTIVE CURRENT ACCOUNTS.'
               EXIT PARAGRAPH
           END-IF
           MOVE ACCT-OD-LIMIT TO WS-DISP-BAL
           DISPLAY 'CURRENT OD LIMIT : ' WS-DISP-BAL
           DISPLAY 'NEW OD LIMIT     : ' WITH NO ADVANCING
           ACCEPT WS-IN-OD-LIMIT
           DISPLAY 'OD RATE % (BLANK = 12.00): ' WITH NO ADVANCING
           ACCEPT WS-IN-OD-RATE
           IF WS-IN-OD-RATE = ZERO
               MOVE WS-DEFAULT-OD-RATE TO WS-IN-OD-RATE
           END-IF
           IF WS-IN-OD-LIMIT > WS-MAX-OD-LIMIT
               DISPLAY 'LIMIT ABOVE BRANCH SANCTIONING POWER (50 LAKH).'
               EXIT PARAGRAPH
           END-IF
           IF ACCT-BALANCE < ZERO
              AND WS-IN-OD-LIMIT < 0 - ACCT-BALANCE
               DISPLAY 'ACCOUNT IS ALREADY OVERDRAWN BEYOND THAT LIMIT.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-OD-LIMIT TO ACCT-OD-LIMIT
           MOVE WS-IN-OD-RATE  TO ACCT-OD-RATE
           PERFORM 8100-REWRITE-ACCOUNT
           IF ACCT-FS-OK
               DISPLAY 'OVERDRAFT LIMIT SET.'
               MOVE 'OD-LIMIT' TO AU-ACTION
               MOVE SPACES TO AU-DETAIL
               MOVE WS-IN-OD-LIMIT TO WS-DISP-BAL
               STRING 'NEW LIMIT ' FUNCTION TRIM(WS-DISP-BAL)
                      DELIMITED BY SIZE INTO AU-DETAIL
               END-STRING
               PERFORM 6900-AUDIT
           END-IF.

       6900-AUDIT.
           MOVE 'ACCTMNT' TO AU-PROGRAM
           MOVE ACCT-NO   TO AU-KEY
           CALL 'AUDITLOG' USING AUD-PARMS.

      *----------------------------------------------------------------*
      * COMMON I/O ROUTINES                                            *
      *----------------------------------------------------------------*
       8000-READ-BY-INPUT-KEY.
           DISPLAY 'ACCOUNT NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           MOVE WS-IN-ACCT-NO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   DISPLAY 'ACCOUNT ' WS-IN-ACCT-NO ' NOT FOUND.'
           END-READ.

       8100-REWRITE-ACCOUNT.
           REWRITE ACCT-RECORD
               INVALID KEY
                   DISPLAY 'REWRITE FAILED, FS=' WS-ACCT-FS
           END-REWRITE.

      *    20 BLANK MENU INPUTS IN A ROW = INPUT EXHAUSTED, LEAVE MENU
       8990-GUARD-END-OF-INPUT.
           IF WS-CHOICE = SPACE
               ADD 1 TO WS-BLANK-INPUTS
               IF WS-BLANK-INPUTS > 20
                   MOVE '9' TO WS-CHOICE
                   MOVE ZERO TO WS-BLANK-INPUTS
               END-IF
           ELSE
               MOVE ZERO TO WS-BLANK-INPUTS
           END-IF.
