       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHQMNT.
      *================================================================*
      * PROGRAM  : CHQMNT                                              *
      * PURPOSE  : CHEQUE SERVICES (BRANCH SIDE) - ISSUE CHEQUE BOOK,  *
      *            LEAF STATUS INQUIRY, STOP PAYMENT, REVOKE STOP,     *
      *            LIST LEAVES OF AN ACCOUNT, AND (MANAGER) RUN THE    *
      *            INWARD CLEARING BATCH.                              *
      * FILES    : CHQREG (INDEXED, I-O)                               *
      * CALLS    : ACCTPOST (INQUIRY), AUDITLOG, CLRBATCH, INVUTIL     *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHQ-FILE ASSIGN TO 'data/CHQREG.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CHQ-KEY
               FILE STATUS IS WS-CHQ-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CHQ-FILE.
           COPY CHQREC.

       WORKING-STORAGE SECTION.
           COPY INVPARM.
           COPY COMMWS.
           COPY POSTPARM.
           COPY SESSION.
           COPY AUDPARM.

       01  WS-CHQ-FS                   PIC X(02).
           88  CHQ-FS-OK                         VALUE '00'.
           88  CHQ-FS-NOFILE                     VALUE '35'.

       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
           88  CHOICE-BACK                       VALUE '9'.
       01  WS-CONFIRM                  PIC X(01).
           88  CONFIRM-YES                       VALUES 'Y' 'y'.

       01  WS-IN-ACCT-NO               PIC 9(10).
       01  WS-IN-CHQ-NO                PIC 9(06).
       01  WS-IN-LEAVES                PIC 9(02).
       01  WS-IN-REASON                PIC X(20).

       01  WS-NEXT-CHQ-NO              PIC 9(06).
       01  WS-NEXT-BOOK-NO             PIC 9(04).
       01  WS-FIRST-LEAF               PIC 9(06).
       01  WS-I                        PIC 9(02).
       01  WS-UNUSED-CNT               PIC 9(05).
       01  WS-LIST-CNT                 PIC 9(05).
       01  WS-CLR-RC                   PIC 9(02).
       01  WS-FIRST-CHQ-NO             PIC 9(06) VALUE 100001.
       01  WS-MAX-UNUSED               PIC 9(02) VALUE 50.

       01  WS-DISP-AMT                 PIC ZZZ,ZZZ,ZZ9.99.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 0100-OPEN-FILE
           IF NOT CHQ-FS-OK
               GOBACK
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL CHOICE-BACK
               PERFORM 1000-SHOW-MENU
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-ISSUE-BOOK
                   WHEN '2' PERFORM 3000-INQUIRY
                   WHEN '3' PERFORM 4000-STOP-PAYMENT
                   WHEN '4' PERFORM 4500-REVOKE-STOP
                   WHEN '5' PERFORM 5000-LIST-LEAVES
                   WHEN '6' PERFORM 6000-RUN-CLEARING
                   WHEN '7' CALL 'CTSMNT'
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE CHQ-FILE
           GOBACK.

       0100-OPEN-FILE.
           OPEN I-O CHQ-FILE
           IF CHQ-FS-NOFILE
               OPEN OUTPUT CHQ-FILE
               CLOSE CHQ-FILE
               OPEN I-O CHQ-FILE
           END-IF
           IF NOT CHQ-FS-OK
               DISPLAY 'CHEQUE REGISTER OPEN FAILED, FS=' WS-CHQ-FS
           END-IF.

       1000-SHOW-MENU.
           DISPLAY ' '
           DISPLAY '----------- CHEQUES AND CLEARING -----------'
           DISPLAY ' 1. ISSUE CHEQUE BOOK'
           DISPLAY ' 2. CHEQUE STATUS INQUIRY'
           DISPLAY ' 3. STOP PAYMENT'
           DISPLAY ' 4. REVOKE STOP PAYMENT'
           DISPLAY ' 5. LIST CHEQUE LEAVES OF AN ACCOUNT'
           IF SES-CAN-APPROVE
               DISPLAY ' 6. RUN INWARD CLEARING (MANAGER)'
           END-IF
           DISPLAY ' 7. OUTWARD CHEQUES (CTS) - DEPOSIT / TRACK'
           DISPLAY ' 9. BACK TO MAIN MENU'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT.

      *----------------------------------------------------------------*
      * ISSUE BOOK - SERIES CONTINUES FROM THE ACCOUNT'S LAST LEAF.    *
      * REFUSED IF THE ACCOUNT ALREADY HOLDS 50+ UNUSED LEAVES.        *
      *----------------------------------------------------------------*
       2000-ISSUE-BOOK.
           DISPLAY 'ACCOUNT NO        : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'LEAVES (10 OR 25) : ' WITH NO ADVANCING
           ACCEPT WS-IN-LEAVES
           IF WS-IN-LEAVES NOT = 10 AND WS-IN-LEAVES NOT = 25
               DISPLAY 'BOOKS COME WITH 10 OR 25 LEAVES.'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'ACCOUNT NOT FOUND.'
               EXIT PARAGRAPH
           END-IF

           PERFORM 2100-SCAN-ACCOUNT-LEAVES
           IF WS-UNUSED-CNT >= WS-MAX-UNUSED
               DISPLAY 'ACCOUNT ALREADY HAS ' WS-UNUSED-CNT
                       ' UNUSED LEAVES.'
               EXIT PARAGRAPH
           END-IF
           IF WS-NEXT-CHQ-NO + WS-IN-LEAVES > 999999
               DISPLAY 'CHEQUE NUMBER SERIES EXHAUSTED.'
               EXIT PARAGRAPH
           END-IF
      *    THE BOOK COMES OUT OF THIS BRANCH'S STOCK
           MOVE SES-BRANCH TO IV-BRANCH
           IF WS-IN-LEAVES = 10
               MOVE 'CB10' TO IV-ITEM
           ELSE
               MOVE 'CB25' TO IV-ITEM
           END-IF
           MOVE 1 TO IV-QTY
           MOVE WS-IN-ACCT-NO TO IV-REF
           CALL 'INVUTIL' USING INV-PARMS
           IF IV-OUT-OF-STOCK
               DISPLAY 'NO ' WS-IN-LEAVES '-LEAF CHEQUE BOOKS IN STOCK '
                       'AT ' SES-BRANCH '.'
               EXIT PARAGRAPH
           END-IF
           IF IV-AT-REORDER-LEVEL
               DISPLAY 'NOTE: ' IV-ITEM ' STOCK AT REORDER LEVEL ('
                       IV-ON-HAND ' LEFT).'
           END-IF

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE WS-NEXT-CHQ-NO TO WS-FIRST-LEAF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-IN-LEAVES
               INITIALIZE CHQ-RECORD
               MOVE WS-IN-ACCT-NO   TO CHQ-ACCT-NO
               MOVE WS-NEXT-CHQ-NO  TO CHQ-NO
               MOVE WS-NEXT-BOOK-NO TO CHQ-BOOK-NO
               SET CHQ-UNUSED       TO TRUE
               MOVE WS-CURR-DATE    TO CHQ-ISSUE-DATE
               WRITE CHQ-RECORD
                   INVALID KEY
                       DISPLAY 'LEAF WRITE FAILED ' WS-NEXT-CHQ-NO
                               ' FS=' WS-CHQ-FS
               END-WRITE
               ADD 1 TO WS-NEXT-CHQ-NO
           END-PERFORM
           SUBTRACT 1 FROM WS-NEXT-CHQ-NO
           DISPLAY 'BOOK ' WS-NEXT-BOOK-NO ' ISSUED: CHEQUES '
                   WS-FIRST-LEAF ' TO ' WS-NEXT-CHQ-NO
           MOVE 'CHQ-BOOK' TO AU-ACTION
           MOVE WS-IN-ACCT-NO TO AU-KEY
           STRING 'BOOK ' WS-NEXT-BOOK-NO ' LEAVES ' WS-FIRST-LEAF
                  '-' WS-NEXT-CHQ-NO DELIMITED BY SIZE INTO AU-DETAIL
           END-STRING
           PERFORM 9000-AUDIT.

      *    WALKS THIS ACCOUNT'S LEAVES (KEY RANGE ACCT+000000..999999)
       2100-SCAN-ACCOUNT-LEAVES.
           MOVE ZERO TO WS-UNUSED-CNT WS-NEXT-BOOK-NO
           MOVE WS-FIRST-CHQ-NO TO WS-NEXT-CHQ-NO
           MOVE WS-IN-ACCT-NO TO CHQ-ACCT-NO
           MOVE ZERO          TO CHQ-NO
           START CHQ-FILE KEY IS NOT LESS THAN CHQ-KEY
               INVALID KEY
                   ADD 1 TO WS-NEXT-BOOK-NO
                   EXIT PARAGRAPH
           END-START
           PERFORM FOREVER
               READ CHQ-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF CHQ-ACCT-NO NOT = WS-IN-ACCT-NO
                   EXIT PERFORM
               END-IF
               IF CHQ-UNUSED
                   ADD 1 TO WS-UNUSED-CNT
               END-IF
               COMPUTE WS-NEXT-CHQ-NO = CHQ-NO + 1
               MOVE CHQ-BOOK-NO TO WS-NEXT-BOOK-NO
           END-PERFORM
           ADD 1 TO WS-NEXT-BOOK-NO.

       3000-INQUIRY.
           PERFORM 8000-READ-LEAF
           IF NOT CHQ-FS-OK
               EXIT PARAGRAPH
           END-IF
           DISPLAY '----------------------------------------'
           DISPLAY 'ACCOUNT / CHEQUE : ' CHQ-ACCT-NO ' / ' CHQ-NO
           DISPLAY 'BOOK NO          : ' CHQ-BOOK-NO
           DISPLAY 'ISSUED ON        : ' CHQ-ISSUE-DATE
           EVALUATE TRUE
               WHEN CHQ-UNUSED
                   DISPLAY 'STATUS           : NOT YET PRESENTED'
               WHEN CHQ-STOPPED
                   DISPLAY 'STATUS           : STOPPED ('
                           FUNCTION TRIM(CHQ-STOP-REASON) ')'
               WHEN CHQ-PAID
                   MOVE CHQ-AMOUNT TO WS-DISP-AMT
                   DISPLAY 'STATUS           : PAID ' WS-DISP-AMT
                           ' ON ' CHQ-PRESENTED-DATE
                   DISPLAY 'PAYEE            : ' CHQ-PAYEE
               WHEN CHQ-RETURNED
                   MOVE CHQ-AMOUNT TO WS-DISP-AMT
                   DISPLAY 'STATUS           : RETURNED, REASON '
                           CHQ-REASON-CODE ', ' WS-DISP-AMT
                           ' ON ' CHQ-PRESENTED-DATE
           END-EVALUATE
           DISPLAY '----------------------------------------'.

       4000-STOP-PAYMENT.
           PERFORM 8000-READ-LEAF
           IF NOT CHQ-FS-OK
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN CHQ-PAID
                   DISPLAY 'CHEQUE IS ALREADY PAID - CANNOT STOP.'
                   EXIT PARAGRAPH
               WHEN CHQ-STOPPED
                   DISPLAY 'PAYMENT IS ALREADY STOPPED.'
                   EXIT PARAGRAPH
           END-EVALUATE
           DISPLAY 'REASON (LOST / DISPUTE / OTHER): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-REASON
           IF WS-IN-REASON = SPACES
               DISPLAY 'A REASON IS REQUIRED.'
               EXIT PARAGRAPH
           END-IF
           SET CHQ-STOPPED TO TRUE
           MOVE FUNCTION UPPER-CASE(WS-IN-REASON) TO CHQ-STOP-REASON
           PERFORM 8100-REWRITE-LEAF
           DISPLAY 'STOP PAYMENT RECORDED FOR CHEQUE ' CHQ-NO
           MOVE 'CHQ-STOP' TO AU-ACTION
           PERFORM 9100-AUDIT-LEAF.

       4500-REVOKE-STOP.
           PERFORM 8000-READ-LEAF
           IF NOT CHQ-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF NOT CHQ-STOPPED
               DISPLAY 'CHEQUE IS NOT STOPPED.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'REVOKE STOP ON CHEQUE ' CHQ-NO ' (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF NOT CONFIRM-YES
               EXIT PARAGRAPH
           END-IF
           SET CHQ-UNUSED TO TRUE
           MOVE SPACES TO CHQ-STOP-REASON
           PERFORM 8100-REWRITE-LEAF
           DISPLAY 'STOP REVOKED.'
           MOVE 'CHQ-UNSTOP' TO AU-ACTION
           PERFORM 9100-AUDIT-LEAF.

       5000-LIST-LEAVES.
           DISPLAY 'ACCOUNT NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           MOVE ZERO TO WS-LIST-CNT
           MOVE WS-IN-ACCT-NO TO CHQ-ACCT-NO
           MOVE ZERO TO CHQ-NO
           START CHQ-FILE KEY IS NOT LESS THAN CHQ-KEY
               INVALID KEY
                   DISPLAY 'NO CHEQUES FOR THIS ACCOUNT.'
                   EXIT PARAGRAPH
           END-START
           DISPLAY 'CHEQUE  BOOK ST         AMOUNT  DATE      REASON'
           PERFORM FOREVER
               READ CHQ-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF CHQ-ACCT-NO NOT = WS-IN-ACCT-NO
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-LIST-CNT
               MOVE CHQ-AMOUNT TO WS-DISP-AMT
               DISPLAY CHQ-NO '  ' CHQ-BOOK-NO ' ' CHQ-STATUS '  '
                       WS-DISP-AMT '  ' CHQ-PRESENTED-DATE '  '
                       CHQ-REASON-CODE
           END-PERFORM
           IF WS-LIST-CNT = ZERO
               DISPLAY 'NO CHEQUES FOR THIS ACCOUNT.'
           END-IF.

      *    THE BATCH OPENS THE REGISTER ITSELF, SO CLOSE IT AROUND THE
      *    CALL AND REOPEN AFTERWARDS
       6000-RUN-CLEARING.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'CLEARING NEEDS A MANAGER OR ADMIN.'
               EXIT PARAGRAPH
           END-IF
           CLOSE CHQ-FILE
           CALL 'CLRBATCH'
           MOVE RETURN-CODE TO WS-CLR-RC
           DISPLAY 'CLEARING ENDED WITH RC=' WS-CLR-RC
           PERFORM 0100-OPEN-FILE.

      *----------------------------------------------------------------*
      * COMMON                                                         *
      *----------------------------------------------------------------*
       8000-READ-LEAF.
           DISPLAY 'ACCOUNT NO : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'CHEQUE NO  : ' WITH NO ADVANCING
           ACCEPT WS-IN-CHQ-NO
           MOVE WS-IN-ACCT-NO TO CHQ-ACCT-NO
           MOVE WS-IN-CHQ-NO  TO CHQ-NO
           READ CHQ-FILE
               INVALID KEY
                   DISPLAY 'CHEQUE ' WS-IN-CHQ-NO
                           ' WAS NOT ISSUED TO THIS ACCOUNT.'
           END-READ.

       8100-REWRITE-LEAF.
           REWRITE CHQ-RECORD
               INVALID KEY
                   DISPLAY 'CHEQUE UPDATE FAILED, FS=' WS-CHQ-FS
           END-REWRITE.

       9000-AUDIT.
           MOVE 'CHQMNT' TO AU-PROGRAM
           CALL 'AUDITLOG' USING AUD-PARMS.

       9100-AUDIT-LEAF.
           MOVE SPACES TO AU-KEY AU-DETAIL
           STRING CHQ-ACCT-NO CHQ-NO DELIMITED BY SIZE INTO AU-KEY
           END-STRING
           MOVE CHQ-STOP-REASON TO AU-DETAIL
           PERFORM 9000-AUDIT.

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
