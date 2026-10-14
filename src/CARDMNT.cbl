       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARDMNT.
      *================================================================*
      * PROGRAM  : CARDMNT                                             *
      * PURPOSE  : DEBIT CARD MAINTENANCE (BRANCH SIDE) - ISSUE CARD,  *
      *            INQUIRY, PIN CHANGE, HOTLIST, UNLOCK AFTER WRONG    *
      *            PIN ATTEMPTS AND DAILY LIMIT CHANGE.                *
      * FILES    : CARDMAST (INDEXED, I-O)                             *
      * CALLS    : ACCTPOST (INQUIRY ONLY), CARDUTIL, INVUTIL          *
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
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CARD-NO
               FILE STATUS IS WS-CARD-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CARD-FILE.
           COPY CARDREC.

       WORKING-STORAGE SECTION.
           COPY INVPARM.
           COPY SESSION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY CARDPARM.

       01  WS-CARD-FS                  PIC X(02).
           88  CARD-FS-OK                        VALUE '00'.
           88  CARD-FS-NOFILE                    VALUE '35'.
           88  CARD-FS-EOF                       VALUE '10'.

       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
           88  CHOICE-BACK                       VALUE '9'.
       01  WS-CONFIRM                  PIC X(01).
           88  CONFIRM-YES                       VALUES 'Y' 'y'.

       01  WS-IN-CARD-NO               PIC 9(16).
       01  WS-IN-ACCT-NO               PIC 9(10).
       01  WS-IN-PIN                   PIC X(04).
       01  WS-IN-PIN-2                 PIC X(04).
       01  WS-IN-OLD-PIN               PIC X(04).
       01  WS-IN-LIMIT                 PIC 9(07)V99.
       01  WS-IN-REASON                PIC X(20).

       01  WS-NEW-CARD-NO              PIC 9(16).
       01  WS-NEW-CARD-R REDEFINES WS-NEW-CARD-NO.
           05  WS-NC-BIN               PIC 9(04).
           05  WS-NC-ACCT              PIC 9(10).
           05  WS-NC-SEQ               PIC 9(01).
           05  WS-NC-CHECK             PIC 9(01).
       01  WS-CARD-SEQ                 PIC 9(01).
       01  WS-ACTIVE-CARDS             PIC 9(02).
       01  WS-PIN-OK-SW                PIC X(01).
           88  PIN-OK                            VALUE 'Y'.

      * PARAMETERS
       01  WS-TEST-BIN                 PIC 9(04) VALUE 9990.
       01  WS-DEFAULT-LIMIT            PIC 9(07)V99 VALUE 40000.00.
       01  WS-MAX-LIMIT                PIC 9(07)V99 VALUE 200000.00.
       01  WS-VALIDITY-YEARS           PIC 9(02) VALUE 05.

       01  WS-DISP-AMT                 PIC Z,ZZZ,ZZ9.99.
       01  WS-MASKED-CARD              PIC X(19).

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN I-O CARD-FILE
           IF CARD-FS-NOFILE
               OPEN OUTPUT CARD-FILE
               CLOSE CARD-FILE
               OPEN I-O CARD-FILE
           END-IF
           IF NOT CARD-FS-OK
               DISPLAY 'CARD MASTER OPEN FAILED, FS=' WS-CARD-FS
               GOBACK
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL CHOICE-BACK
               PERFORM 1000-SHOW-MENU
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-ISSUE-CARD
                   WHEN '2' PERFORM 3000-INQUIRY
                   WHEN '3' PERFORM 4000-CHANGE-PIN
                   WHEN '4' PERFORM 5000-HOTLIST
                   WHEN '5' PERFORM 6000-UNLOCK
                   WHEN '6' PERFORM 7000-CHANGE-LIMIT
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE CARD-FILE
           GOBACK.

       1000-SHOW-MENU.
           DISPLAY ' '
           DISPLAY '------------- DEBIT CARD SERVICES -------------'
           DISPLAY ' 1. ISSUE NEW CARD'
           DISPLAY ' 2. CARD INQUIRY'
           DISPLAY ' 3. CHANGE PIN'
           DISPLAY ' 4. HOTLIST (LOST / STOLEN)'
           DISPLAY ' 5. UNLOCK AFTER WRONG PIN'
           DISPLAY ' 6. CHANGE DAILY LIMIT'
           DISPLAY ' 9. BACK TO MAIN MENU'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT.

      *----------------------------------------------------------------*
      * ISSUE - MAX 2 LIVE CARDS PER ACCOUNT. CARD NO = TEST BIN +     *
      * ACCOUNT NO + SEQUENCE + LUHN CHECK DIGIT.                      *
      *----------------------------------------------------------------*
       2000-ISSUE-CARD.
           DISPLAY 'ACCOUNT NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'ACCOUNT NOT FOUND OR UNAVAILABLE.'
               EXIT PARAGRAPH
           END-IF

           PERFORM 2100-FIND-FREE-SEQUENCE
           IF WS-ACTIVE-CARDS >= 2
               DISPLAY 'ACCOUNT ALREADY HAS 2 ACTIVE CARDS.'
               EXIT PARAGRAPH
           END-IF
           IF WS-CARD-SEQ = 9
               DISPLAY 'CARD SEQUENCE EXHAUSTED FOR THIS ACCOUNT.'
               EXIT PARAGRAPH
           END-IF

           PERFORM 8300-ACCEPT-NEW-PIN
           IF NOT PIN-OK
               EXIT PARAGRAPH
           END-IF
      *    A CARD BLANK COMES OUT OF THIS BRANCH'S STOCK
           MOVE SES-BRANCH    TO IV-BRANCH
           MOVE 'DCRD'        TO IV-ITEM
           MOVE 1             TO IV-QTY
           MOVE WS-IN-ACCT-NO TO IV-REF
           CALL 'INVUTIL' USING INV-PARMS
           IF IV-OUT-OF-STOCK
               DISPLAY 'NO DEBIT CARD BLANKS IN STOCK AT ' SES-BRANCH
                       '.'
               EXIT PARAGRAPH
           END-IF

           MOVE WS-TEST-BIN   TO WS-NC-BIN
           MOVE WS-IN-ACCT-NO TO WS-NC-ACCT
           MOVE WS-CARD-SEQ   TO WS-NC-SEQ
           MOVE ZERO          TO WS-NC-CHECK
           MOVE 'LU'          TO CU-FUNCTION
           MOVE WS-NEW-CARD-NO TO CU-CARD-NO
           CALL 'CARDUTIL' USING CU-PARMS
           MOVE CU-CARD-NO    TO WS-NEW-CARD-NO

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           INITIALIZE CARD-RECORD
           MOVE WS-NEW-CARD-NO    TO CARD-NO
           MOVE WS-IN-ACCT-NO     TO CARD-ACCT-NO
           MOVE PP-HOLDER-NAME    TO CARD-HOLDER-NAME
           SET CARD-ACTIVE        TO TRUE
           MOVE WS-DEFAULT-LIMIT  TO CARD-DAILY-LIMIT
           MOVE WS-CURR-DATE      TO CARD-ISSUE-DATE
           COMPUTE CARD-EXPIRY-YYYYMM =
                   WS-CURR-YYYYMM + WS-VALIDITY-YEARS * 100
           PERFORM 8400-HASH-NEW-PIN
           WRITE CARD-RECORD
               INVALID KEY
                   DISPLAY 'CARD WRITE FAILED, FS=' WS-CARD-FS
               NOT INVALID KEY
                   PERFORM 8500-MASK-CARD
                   DISPLAY 'CARD ISSUED: ' WS-MASKED-CARD
                   DISPLAY 'FULL NUMBER (PRINT ON CARD): ' CARD-NO
                   DISPLAY 'VALID THRU : ' CARD-EXPIRY-YYYYMM(5:2)
                           '/' CARD-EXPIRY-YYYYMM(1:4)
           END-WRITE.

      *    WALK ALL CARDS OF THIS ACCOUNT (KEY RANGE BIN+ACCT+0..9)
       2100-FIND-FREE-SEQUENCE.
           MOVE ZERO TO WS-ACTIVE-CARDS WS-CARD-SEQ
           MOVE WS-TEST-BIN   TO WS-NC-BIN
           MOVE WS-IN-ACCT-NO TO WS-NC-ACCT
           MOVE ZERO          TO WS-NC-SEQ WS-NC-CHECK
           MOVE WS-NEW-CARD-NO TO CARD-NO
           START CARD-FILE KEY IS NOT LESS THAN CARD-NO
               INVALID KEY EXIT PARAGRAPH
           END-START
           PERFORM FOREVER
               READ CARD-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF CARD-ACCT-NO NOT = WS-IN-ACCT-NO
                   EXIT PERFORM
               END-IF
               MOVE CARD-NO(15:1) TO WS-CARD-SEQ
               ADD 1 TO WS-CARD-SEQ
               IF CARD-ACTIVE OR CARD-PIN-LOCKED
                   ADD 1 TO WS-ACTIVE-CARDS
               END-IF
           END-PERFORM.

       3000-INQUIRY.
           PERFORM 8000-READ-CARD
           IF NOT CARD-FS-OK
               EXIT PARAGRAPH
           END-IF
           PERFORM 8500-MASK-CARD
           DISPLAY '----------------------------------------'
           DISPLAY 'CARD NO      : ' WS-MASKED-CARD
           DISPLAY 'ACCOUNT NO   : ' CARD-ACCT-NO
           DISPLAY 'NAME ON CARD : ' CARD-HOLDER-NAME
           EVALUATE TRUE
               WHEN CARD-ACTIVE     DISPLAY 'STATUS       : ACTIVE'
               WHEN CARD-PIN-LOCKED DISPLAY 'STATUS       : PIN LOCKED'
               WHEN CARD-HOTLISTED
                   DISPLAY 'STATUS       : HOTLISTED ('
                           FUNCTION TRIM(CARD-HOTLIST-REASON) ')'
               WHEN CARD-EXPIRED    DISPLAY 'STATUS       : EXPIRED'
           END-EVALUATE
           DISPLAY 'VALID THRU   : ' CARD-EXPIRY-YYYYMM(5:2) '/'
                   CARD-EXPIRY-YYYYMM(1:4)
           MOVE CARD-DAILY-LIMIT TO WS-DISP-AMT
           DISPLAY 'DAILY LIMIT  : ' WS-DISP-AMT
           MOVE CARD-USED-TODAY TO WS-DISP-AMT
           DISPLAY 'USED (' CARD-LAST-USE-DATE ') : ' WS-DISP-AMT
           DISPLAY '----------------------------------------'.

       4000-CHANGE-PIN.
           PERFORM 8000-READ-CARD
           IF NOT CARD-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF NOT CARD-ACTIVE
               DISPLAY 'PIN CAN ONLY BE CHANGED ON AN ACTIVE CARD.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'CURRENT PIN : ' WITH NO ADVANCING
           ACCEPT WS-IN-OLD-PIN
           IF WS-IN-OLD-PIN NOT NUMERIC
               DISPLAY 'INCORRECT PIN.'
               EXIT PARAGRAPH
           END-IF
           MOVE 'PH'          TO CU-FUNCTION
           MOVE CARD-NO       TO CU-CARD-NO
           MOVE WS-IN-OLD-PIN TO CU-PIN
           CALL 'CARDUTIL' USING CU-PARMS
           IF CU-HASH NOT = CARD-PIN-HASH
               DISPLAY 'INCORRECT PIN.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8300-ACCEPT-NEW-PIN
           IF NOT PIN-OK
               EXIT PARAGRAPH
           END-IF
           IF WS-IN-PIN = WS-IN-OLD-PIN
               DISPLAY 'NEW PIN MUST DIFFER FROM CURRENT PIN.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8400-HASH-NEW-PIN
           PERFORM 8100-REWRITE-CARD
           DISPLAY 'PIN CHANGED.'.

       5000-HOTLIST.
           PERFORM 8000-READ-CARD
           IF NOT CARD-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF CARD-HOTLISTED
               DISPLAY 'CARD IS ALREADY HOTLISTED.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'REASON (LOST/STOLEN/DAMAGED): ' WITH NO ADVANCING
           ACCEPT WS-IN-REASON
           DISPLAY 'HOTLISTING IS PERMANENT. CONFIRM (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF NOT CONFIRM-YES
               EXIT PARAGRAPH
           END-IF
           SET CARD-HOTLISTED TO TRUE
           MOVE FUNCTION UPPER-CASE(WS-IN-REASON)
                                  TO CARD-HOTLIST-REASON
           PERFORM 8100-REWRITE-CARD
           DISPLAY 'CARD HOTLISTED. ISSUE A NEW CARD IF NEEDED.'.

       6000-UNLOCK.
           PERFORM 8000-READ-CARD
           IF NOT CARD-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF NOT CARD-PIN-LOCKED
               DISPLAY 'CARD IS NOT PIN LOCKED.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'CUSTOMER MUST SET A NEW PIN.'
           PERFORM 8300-ACCEPT-NEW-PIN
           IF NOT PIN-OK
               EXIT PARAGRAPH
           END-IF
           PERFORM 8400-HASH-NEW-PIN
           SET CARD-ACTIVE TO TRUE
           MOVE ZERO TO CARD-WRONG-PIN-CNT
           PERFORM 8100-REWRITE-CARD
           DISPLAY 'CARD UNLOCKED WITH NEW PIN.'.

       7000-CHANGE-LIMIT.
           PERFORM 8000-READ-CARD
           IF NOT CARD-FS-OK
               EXIT PARAGRAPH
           END-IF
           MOVE CARD-DAILY-LIMIT TO WS-DISP-AMT
           DISPLAY 'CURRENT LIMIT: ' WS-DISP-AMT
           DISPLAY 'NEW LIMIT    : ' WITH NO ADVANCING
           ACCEPT WS-IN-LIMIT
           IF WS-IN-LIMIT < 1000 OR WS-IN-LIMIT > WS-MAX-LIMIT
               MOVE WS-MAX-LIMIT TO WS-DISP-AMT
               DISPLAY 'LIMIT MUST BE 1,000 TO ' WS-DISP-AMT
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-LIMIT TO CARD-DAILY-LIMIT
           PERFORM 8100-REWRITE-CARD
           DISPLAY 'DAILY LIMIT UPDATED.'.

      *----------------------------------------------------------------*
      * COMMON                                                         *
      *----------------------------------------------------------------*
       8000-READ-CARD.
           DISPLAY 'CARD NO (16 DIGITS): ' WITH NO ADVANCING
           ACCEPT WS-IN-CARD-NO
           MOVE WS-IN-CARD-NO TO CARD-NO
           READ CARD-FILE
               INVALID KEY
                   DISPLAY 'CARD NOT FOUND.'
           END-READ.

       8100-REWRITE-CARD.
           REWRITE CARD-RECORD
               INVALID KEY
                   DISPLAY 'CARD UPDATE FAILED, FS=' WS-CARD-FS
           END-REWRITE.

      *    PIN: 4 DIGITS, ENTERED TWICE, NOT ALL SAME, NOT 1234
       8300-ACCEPT-NEW-PIN.
           MOVE 'N' TO WS-PIN-OK-SW
           DISPLAY 'NEW 4-DIGIT PIN : ' WITH NO ADVANCING
           ACCEPT WS-IN-PIN
           DISPLAY 'RE-ENTER PIN    : ' WITH NO ADVANCING
           ACCEPT WS-IN-PIN-2
           EVALUATE TRUE
               WHEN WS-IN-PIN NOT NUMERIC
                   DISPLAY 'PIN MUST BE 4 DIGITS.'
               WHEN WS-IN-PIN NOT = WS-IN-PIN-2
                   DISPLAY 'PINS DO NOT MATCH.'
               WHEN WS-IN-PIN = '1234' OR '0000'
                   DISPLAY 'PIN TOO EASY TO GUESS.'
               WHEN WS-IN-PIN(1:1) = WS-IN-PIN(2:1)
                AND WS-IN-PIN(2:1) = WS-IN-PIN(3:1)
                AND WS-IN-PIN(3:1) = WS-IN-PIN(4:1)
                   DISPLAY 'PIN TOO EASY TO GUESS.'
               WHEN OTHER
                   MOVE 'Y' TO WS-PIN-OK-SW
           END-EVALUATE.

       8400-HASH-NEW-PIN.
           MOVE 'PH'      TO CU-FUNCTION
           MOVE CARD-NO   TO CU-CARD-NO
           MOVE WS-IN-PIN TO CU-PIN
           CALL 'CARDUTIL' USING CU-PARMS
           MOVE CU-HASH   TO CARD-PIN-HASH.

       8500-MASK-CARD.
           MOVE SPACES TO WS-MASKED-CARD
           STRING CARD-NO(1:4) ' XXXX XXXX ' CARD-NO(13:4)
                  DELIMITED BY SIZE INTO WS-MASKED-CARD
           END-STRING.

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
