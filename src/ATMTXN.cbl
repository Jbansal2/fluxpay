       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMTXN.
      *================================================================*
      * PROGRAM  : ATMTXN                                              *
      * PURPOSE  : ATM SESSION SIMULATOR - CARD + PIN AUTHENTICATION,  *
      *            CASH WITHDRAWAL, BALANCE ENQUIRY, MINI STATEMENT.   *
      *            3 WRONG PINS LOCK THE CARD. WITHDRAWALS ARE IN      *
      *            MULTIPLES OF 100, MAX 10,000 PER TRANSACTION AND    *
      *            WITHIN THE CARD'S DAILY LIMIT.                      *
      * FILES    : CARDMAST (INDEXED, I-O)                             *
      *            TRANLOG  (SEQUENTIAL, INPUT - MINI STATEMENT)       *
      * CALLS    : ACCTPOST, CARDUTIL                                  *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CARD-FILE ASSIGN TO 'data/CARDMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CARD-NO
               FILE STATUS IS WS-CARD-FS.
           SELECT TRAN-FILE ASSIGN TO 'data/TRANLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRAN-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CARD-FILE.
           COPY CARDREC.
       FD  TRAN-FILE.
           COPY TRANREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY CARDPARM.

       01  WS-CARD-FS                  PIC X(02).
           88  CARD-FS-OK                        VALUE '00'.

       01  WS-IN-CARD-NO               PIC 9(16).
       01  WS-IN-PIN                   PIC X(04).
       01  WS-IN-AMOUNT                PIC 9(07).
       01  WS-CHOICE                   PIC X(01).
           88  CHOICE-EXIT                       VALUE '9'.

       01  WS-SESSION-SW               PIC X(01).
           88  SESSION-OPEN                      VALUE 'Y'.
           88  SESSION-CLOSED                    VALUE 'N'.
       01  WS-MAX-PIN-TRIES            PIC 9(01) VALUE 3.
       01  WS-MAX-PER-TXN              PIC 9(07) VALUE 10000.
       01  WS-REMAINING-LIMIT          PIC 9(07)V99.

      * LAST 5 TRANSACTIONS, KEPT AS A ROLLING WINDOW
       01  WS-MINI-TABLE.
           05  WS-MINI-ENTRY OCCURS 5 TIMES.
               10  MINI-DATE           PIC 9(08).
               10  MINI-TYPE           PIC X(02).
               10  MINI-AMOUNT         PIC 9(11)V99.
               10  MINI-CR-DR          PIC X(02).
       01  WS-MINI-COUNT               PIC 9(05).
       01  WS-MINI-IX                  PIC 9(01).
       01  WS-MINI-START               PIC 9(05).
       01  WS-MINI-SLOT                PIC 9(01).
       01  WS-K                        PIC 9(05).

       01  WS-DISP-AMT                 PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-DATE                PIC 9999/99/99.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN I-O CARD-FILE
           IF NOT CARD-FS-OK
               DISPLAY 'ATM OUT OF SERVICE (CARD FILE FS='
                       WS-CARD-FS ')'
               GOBACK
           END-IF
           DISPLAY ' '
           DISPLAY '*********** WELCOME TO CORE BANK ATM ***********'
           PERFORM 1000-AUTHENTICATE
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL SESSION-CLOSED OR CHOICE-EXIT
               PERFORM 1100-SHOW-MENU
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-WITHDRAW
                   WHEN '2' PERFORM 3000-BALANCE
                   WHEN '3' PERFORM 4000-MINI-STATEMENT
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID OPTION.'
               END-EVALUATE
           END-PERFORM
           CLOSE CARD-FILE
           DISPLAY 'PLEASE TAKE YOUR CARD. THANK YOU.'
           GOBACK.

      *----------------------------------------------------------------*
      * CARD AND PIN CHECK                                             *
      *----------------------------------------------------------------*
       1000-AUTHENTICATE.
           SET SESSION-CLOSED TO TRUE
           DISPLAY 'INSERT CARD (ENTER CARD NO): ' WITH NO ADVANCING
           ACCEPT WS-IN-CARD-NO
           MOVE 'LV'          TO CU-FUNCTION
           MOVE WS-IN-CARD-NO TO CU-CARD-NO
           CALL 'CARDUTIL' USING CU-PARMS
           IF CU-RC NOT = ZERO
               DISPLAY 'CARD NOT READABLE.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-CARD-NO TO CARD-NO
           READ CARD-FILE
               INVALID KEY
                   DISPLAY 'CARD NOT RECOGNISED.'
                   EXIT PARAGRAPH
           END-READ
           PERFORM 1200-CHECK-CARD-STATUS
           IF NOT CARD-ACTIVE
               EXIT PARAGRAPH
           END-IF
           PERFORM 1300-CHECK-PIN
               UNTIL SESSION-OPEN
                  OR CARD-WRONG-PIN-CNT >= WS-MAX-PIN-TRIES.

       1100-SHOW-MENU.
           DISPLAY ' '
           DISPLAY ' 1. CASH WITHDRAWAL'
           DISPLAY ' 2. BALANCE ENQUIRY'
           DISPLAY ' 3. MINI STATEMENT'
           DISPLAY ' 9. EXIT'
           DISPLAY 'SELECT: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT.

       1200-CHECK-CARD-STATUS.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           IF CARD-ACTIVE AND CARD-EXPIRY-YYYYMM < WS-CURR-YYYYMM
               SET CARD-EXPIRED TO TRUE
               REWRITE CARD-RECORD
           END-IF
           EVALUATE TRUE
               WHEN CARD-ACTIVE
                   CONTINUE
               WHEN CARD-PIN-LOCKED
                   DISPLAY 'CARD BLOCKED - WRONG PIN LIMIT REACHED.'
                   DISPLAY 'PLEASE CONTACT YOUR BRANCH.'
               WHEN CARD-HOTLISTED
                   DISPLAY 'CARD BLOCKED. PLEASE CONTACT YOUR BRANCH.'
               WHEN CARD-EXPIRED
                   DISPLAY 'CARD EXPIRED.'
           END-EVALUATE.

       1300-CHECK-PIN.
           DISPLAY 'ENTER PIN: ' WITH NO ADVANCING
           ACCEPT WS-IN-PIN
           MOVE ZERO TO CU-HASH
           IF WS-IN-PIN IS NUMERIC
               MOVE 'PH'      TO CU-FUNCTION
               MOVE CARD-NO   TO CU-CARD-NO
               MOVE WS-IN-PIN TO CU-PIN
               CALL 'CARDUTIL' USING CU-PARMS
           END-IF
           IF CU-HASH = CARD-PIN-HASH AND WS-IN-PIN IS NUMERIC
               MOVE ZERO TO CARD-WRONG-PIN-CNT
               SET SESSION-OPEN TO TRUE
           ELSE
               ADD 1 TO CARD-WRONG-PIN-CNT
               IF CARD-WRONG-PIN-CNT >= WS-MAX-PIN-TRIES
                   SET CARD-PIN-LOCKED TO TRUE
                   DISPLAY 'INCORRECT PIN. CARD HAS BEEN BLOCKED.'
               ELSE
                   COMPUTE WS-K = WS-MAX-PIN-TRIES
                                - CARD-WRONG-PIN-CNT
                   DISPLAY 'INCORRECT PIN. TRIES LEFT: ' WS-K(5:1)
               END-IF
           END-IF
           REWRITE CARD-RECORD
               INVALID KEY
                   DISPLAY 'ATM ERROR, FS=' WS-CARD-FS
           END-REWRITE.

      *----------------------------------------------------------------*
      * WITHDRAWAL                                                     *
      *----------------------------------------------------------------*
       2000-WITHDRAW.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
      *    DAILY USAGE RESETS ON FIRST USE OF A NEW DAY
           IF CARD-LAST-USE-DATE NOT = WS-CURR-DATE
               MOVE ZERO TO CARD-USED-TODAY
           END-IF
           COMPUTE WS-REMAINING-LIMIT =
                   CARD-DAILY-LIMIT - CARD-USED-TODAY
           DISPLAY 'AMOUNT (MULTIPLES OF 100): ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           EVALUATE TRUE
               WHEN WS-IN-AMOUNT = ZERO
                   DISPLAY 'INVALID AMOUNT.'
                   EXIT PARAGRAPH
               WHEN FUNCTION MOD(WS-IN-AMOUNT, 100) NOT = ZERO
                   DISPLAY 'ENTER AMOUNT IN MULTIPLES OF 100.'
                   EXIT PARAGRAPH
               WHEN WS-IN-AMOUNT > WS-MAX-PER-TXN
                   DISPLAY 'MAXIMUM 10,000 PER TRANSACTION.'
                   EXIT PARAGRAPH
               WHEN WS-IN-AMOUNT > WS-REMAINING-LIMIT
                   MOVE WS-REMAINING-LIMIT TO WS-DISP-AMT
                   DISPLAY 'EXCEEDS DAILY LIMIT. REMAINING TODAY: '
                           WS-DISP-AMT
                   EXIT PARAGRAPH
           END-EVALUATE

           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE CARD-ACCT-NO TO PP-ACCT-NO
           MOVE WS-IN-AMOUNT TO PP-AMOUNT
           MOVE 'AW'         TO PP-TRAN-TYPE
           MOVE ZERO         TO PP-REF-ACCT
           STRING 'ATM WDL CARD ' CARD-NO(13:4)
                  DELIMITED BY SIZE INTO PP-NARRATION
           END-STRING
           CALL 'ACCTPOST' USING POST-PARMS
           EVALUATE TRUE
               WHEN PP-OK
                   ADD WS-IN-AMOUNT TO CARD-USED-TODAY
                   MOVE WS-CURR-DATE TO CARD-LAST-USE-DATE
                   REWRITE CARD-RECORD
                   DISPLAY 'PLEASE COLLECT YOUR CASH.'
                   MOVE PP-BALANCE TO WS-DISP-AMT
                   DISPLAY 'AVAILABLE BALANCE: ' WS-DISP-AMT
               WHEN PP-INSUFFICIENT
                   DISPLAY 'INSUFFICIENT FUNDS.'
               WHEN OTHER
                   DISPLAY 'UNABLE TO PROCESS. RC=' PP-RETURN-CODE
           END-EVALUATE.

       3000-BALANCE.
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE CARD-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF PP-OK
               MOVE PP-BALANCE TO WS-DISP-AMT
               DISPLAY 'LEDGER BALANCE    : ' WS-DISP-AMT
               MOVE PP-AVAILABLE TO WS-DISP-AMT
               DISPLAY 'AVAILABLE BALANCE : ' WS-DISP-AMT
           ELSE
               DISPLAY 'UNABLE TO FETCH BALANCE.'
           END-IF.

      *----------------------------------------------------------------*
      * MINI STATEMENT - TWO PASSES: COUNT, THEN KEEP THE LAST 5       *
      *----------------------------------------------------------------*
       4000-MINI-STATEMENT.
           MOVE ZERO TO WS-MINI-COUNT
           PERFORM 4100-COUNT-PASS
           IF WS-MINI-COUNT = ZERO
               DISPLAY 'NO TRANSACTIONS.'
               EXIT PARAGRAPH
           END-IF
           IF WS-MINI-COUNT > 5
               COMPUTE WS-MINI-START = WS-MINI-COUNT - 4
           ELSE
               MOVE 1 TO WS-MINI-START
           END-IF
           PERFORM 4200-COLLECT-PASS
           DISPLAY '----------- MINI STATEMENT -----------'
           PERFORM VARYING WS-MINI-IX FROM 1 BY 1
                   UNTIL WS-MINI-IX > WS-MINI-SLOT
               MOVE MINI-DATE(WS-MINI-IX)   TO WS-DISP-DATE
               MOVE MINI-AMOUNT(WS-MINI-IX) TO WS-DISP-AMT
               DISPLAY WS-DISP-DATE '  ' MINI-TYPE(WS-MINI-IX) '  '
                       WS-DISP-AMT ' ' MINI-CR-DR(WS-MINI-IX)
           END-PERFORM
           PERFORM 3000-BALANCE.

       4100-COUNT-PASS.
           OPEN INPUT TRAN-FILE
           IF NOT TRAN-FS-OK
               EXIT PARAGRAPH
           END-IF
           PERFORM FOREVER
               READ TRAN-FILE
                   AT END EXIT PERFORM
               END-READ
               IF TRAN-ACCT-NO = CARD-ACCT-NO
                   ADD 1 TO WS-MINI-COUNT
               END-IF
           END-PERFORM
           CLOSE TRAN-FILE.

       4200-COLLECT-PASS.
           MOVE ZERO TO WS-K WS-MINI-SLOT
           OPEN INPUT TRAN-FILE
           PERFORM FOREVER
               READ TRAN-FILE
                   AT END EXIT PERFORM
               END-READ
               IF TRAN-ACCT-NO = CARD-ACCT-NO
                   ADD 1 TO WS-K
                   IF WS-K >= WS-MINI-START
                       ADD 1 TO WS-MINI-SLOT
                       MOVE TRAN-DATE   TO MINI-DATE(WS-MINI-SLOT)
                       MOVE TRAN-TYPE   TO MINI-TYPE(WS-MINI-SLOT)
                       MOVE TRAN-AMOUNT TO MINI-AMOUNT(WS-MINI-SLOT)
                       IF TRAN-CREDIT-TYPE
                           MOVE 'CR' TO MINI-CR-DR(WS-MINI-SLOT)
                       ELSE
                           MOVE 'DR' TO MINI-CR-DR(WS-MINI-SLOT)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           CLOSE TRAN-FILE.

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
