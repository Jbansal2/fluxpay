       IDENTIFICATION DIVISION.
       PROGRAM-ID. APPRVMNT.
      *================================================================*
      * PROGRAM  : APPRVMNT                                            *
      * PURPOSE  : CHECKER SCREEN FOR THE MAKER-CHECKER QUEUE.         *
      *              1 - LIST PENDING ITEMS                            *
      *              2 - APPROVE (POSTS THROUGH TXNEXEC)               *
      *              3 - REJECT WITH A REASON                          *
      *              4 - RECENT DECISIONS                              *
      *            RULES: ONLY MANAGER/ADMIN; NOBODY CHECKS HIS OWN    *
      *            ENTRY; ITEMS FROM AN EARLIER DAY ARE EXPIRED BY EOD *
      *            AND CANNOT BE APPROVED. IF POSTING FAILS (E.G. THE  *
      *            BALANCE HAS GONE DOWN SINCE) THE ITEM IS MARKED F   *
      *            WITH THE ACCTPOST RETURN CODE - NEVER LEFT HANGING. *
      * FILES    : PENDTXN (I-O)                                       *
      * CALLS    : TXNEXEC, AUDITLOG                                   *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PEND-FILE ASSIGN TO 'data/PENDTXN.DAT'
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS PND-ID FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  PEND-FILE.
           COPY PENDREC.

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-CHOICE                   PIC X(01).
       01  WS-EXIT-SW                  PIC X(01) VALUE 'N'.
           88  EXIT-REQUESTED                    VALUE 'Y'.
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-IN-ID                    PIC 9(08).
       01  WS-IN-REASON                PIC X(30).
       01  WS-CONFIRM                  PIC X(01).
       01  WS-COUNT                    PIC 9(04).
       01  WS-NOW.
           05  WS-NOW-DATE             PIC 9(08).
           05  WS-NOW-TIME             PIC 9(06).
           05  FILLER                  PIC X(07).
       01  ED-AMT                      PIC ZZ,ZZZ,ZZ9.99.
       01  WS-TYPE-NAME                PIC X(10).
       01  WS-STATUS-NAME              PIC X(10).
           COPY TXNPARM.
           COPY POSTPARM.
           COPY SESSION.
           COPY AUDPARM.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'APPROVALS NEED A MANAGER.'
               GOBACK
           END-IF
           OPEN I-O PEND-FILE
           IF WS-FS = '35'
               DISPLAY 'NOTHING HAS BEEN QUEUED FOR APPROVAL YET.'
               GOBACK
           END-IF
           IF WS-FS NOT = '00'
               DISPLAY 'APPROVAL QUEUE OPEN FAILED, FS=' WS-FS
               GOBACK
           END-IF
           MOVE 'N' TO WS-EXIT-SW
           PERFORM UNTIL EXIT-REQUESTED
               DISPLAY ' '
               DISPLAY '----------- APPROVALS (CHECKER) -----------'
               DISPLAY ' 1. LIST PENDING ITEMS'
               DISPLAY ' 2. APPROVE'
               DISPLAY ' 3. REJECT'
               DISPLAY ' 4. RECENT DECISIONS'
               DISPLAY ' 9. BACK TO MAIN MENU'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               PERFORM 8990-GUARD-END-OF-INPUT
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 1000-LIST-PENDING
                   WHEN '2' PERFORM 2000-APPROVE
                   WHEN '3' PERFORM 3000-REJECT
                   WHEN '4' PERFORM 4000-RECENT
                   WHEN '9' SET EXIT-REQUESTED TO TRUE
                   WHEN SPACE CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE PEND-FILE
           GOBACK.

       1000-LIST-PENDING.
           MOVE ZERO TO WS-COUNT
           DISPLAY 'REF      TYPE       FROM       TO              '
                   ' AMOUNT MAKER    DATE'
           PERFORM 8000-START-BROWSE
           PERFORM UNTIL END-OF-FILE
               READ PEND-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF PND-PENDING
                           ADD 1 TO WS-COUNT
                           PERFORM 8100-SHOW-LINE
                       END-IF
               END-READ
           END-PERFORM
           DISPLAY WS-COUNT ' ITEM(S) PENDING'.

       2000-APPROVE.
           PERFORM 7000-READ-PENDING
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8100-SHOW-LINE
           DISPLAY 'NARRATION: ' PND-NARRATION
           DISPLAY 'APPROVE AND POST (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
               DISPLAY 'NOT APPROVED.'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE TXN-PARMS
           MOVE PND-TYPE      TO TX-FUNCTION
           MOVE PND-FROM-ACCT TO TX-FROM-ACCT
           MOVE PND-TO-ACCT   TO TX-TO-ACCT
           MOVE PND-AMOUNT    TO TX-AMOUNT
           MOVE PND-NARRATION TO TX-NARRATION
      *    THE MAKER TOOK (OR PAYS) THE CASH, SO IT IS HIS DRAWER
           MOVE PND-MAKER        TO TX-CASH-USER
           MOVE PND-MAKER-BRANCH TO TX-CASH-BRANCH
           CALL 'TXNEXEC' USING TXN-PARMS POST-PARMS
           IF TX-OK
               SET PND-APPROVED TO TRUE
               MOVE ZERO TO PND-RESULT-RC
               MOVE 'APPROVED AND POSTED' TO PND-REMARK
               DISPLAY 'APPROVED AND POSTED. REF ' PND-ID
           ELSE
               SET PND-FAILED TO TRUE
               MOVE PP-RETURN-CODE TO PND-RESULT-RC
               MOVE SPACES TO PND-REMARK
               STRING 'POSTING FAILED TX ' TX-RC ' PP ' PP-RETURN-CODE
                   DELIMITED BY SIZE INTO PND-REMARK
               DISPLAY 'APPROVED BUT POSTING FAILED (TX RC ' TX-RC
                       ', POST RC ' PP-RETURN-CODE ') - MARKED FAILED.'
           END-IF
           PERFORM 7100-STAMP-AND-SAVE.

       3000-REJECT.
           PERFORM 7000-READ-PENDING
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'REASON : ' WITH NO ADVANCING
           ACCEPT WS-IN-REASON
           IF WS-IN-REASON = SPACES
               DISPLAY 'A REASON IS REQUIRED.'
               EXIT PARAGRAPH
           END-IF
           SET PND-REJECTED TO TRUE
           MOVE WS-IN-REASON TO PND-REMARK
           MOVE ZERO TO PND-RESULT-RC
           PERFORM 7100-STAMP-AND-SAVE
           DISPLAY 'REJECTED. REF ' PND-ID.

       4000-RECENT.
           MOVE ZERO TO WS-COUNT
           PERFORM 8000-START-BROWSE
           PERFORM UNTIL END-OF-FILE
               READ PEND-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF NOT PND-PENDING
                           ADD 1 TO WS-COUNT
                           PERFORM 8100-SHOW-LINE
                           DISPLAY '         ' WS-STATUS-NAME ' BY '
                                   PND-CHECKER ': ' PND-REMARK
                       END-IF
               END-READ
           END-PERFORM
           IF WS-COUNT = ZERO
               DISPLAY 'NO DECISIONS YET.'
           END-IF.

      *    READ ONE ITEM FOR A DECISION; WS-FS '00' = USABLE
       7000-READ-PENDING.
           DISPLAY 'REFERENCE NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-ID
           MOVE WS-IN-ID TO PND-ID
           READ PEND-FILE
               INVALID KEY
                   DISPLAY 'NO SUCH REFERENCE.'
                   MOVE '23' TO WS-FS
                   EXIT PARAGRAPH
           END-READ
           IF NOT PND-PENDING
               PERFORM 8200-STATUS-NAME
               DISPLAY 'ITEM IS NOT PENDING ('
                       FUNCTION TRIM(WS-STATUS-NAME) ').'
               MOVE '99' TO WS-FS
               EXIT PARAGRAPH
           END-IF
           IF PND-MAKER = SES-USER-ID
               DISPLAY 'MAKER CANNOT CHECK HIS OWN ENTRY.'
               MOVE 'SELF-CHECK' TO AU-ACTION
               MOVE 'APPRVMNT' TO AU-PROGRAM
               MOVE PND-ID TO AU-KEY
               MOVE 'SELF-APPROVAL REFUSED' TO AU-DETAIL
               CALL 'AUDITLOG' USING AUD-PARMS
               MOVE '99' TO WS-FS
           END-IF.

       7100-STAMP-AND-SAVE.
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           MOVE SES-USER-ID  TO PND-CHECKER
           MOVE WS-NOW-DATE  TO PND-CHECK-DATE
           MOVE WS-NOW-TIME  TO PND-CHECK-TIME
           REWRITE PEND-RECORD
               INVALID KEY
                   DISPLAY 'QUEUE UPDATE FAILED, FS=' WS-FS
           END-REWRITE
           PERFORM 8200-STATUS-NAME
           MOVE 'APPRVMNT'     TO AU-PROGRAM
           MOVE WS-STATUS-NAME TO AU-ACTION
           MOVE PND-ID         TO AU-KEY
           MOVE PND-REMARK     TO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS.

       8000-START-BROWSE.
           MOVE 'N' TO WS-EOF-SW
           MOVE ZERO TO PND-ID
           START PEND-FILE KEY IS NOT LESS THAN PND-ID
               INVALID KEY SET END-OF-FILE TO TRUE
           END-START.

       8100-SHOW-LINE.
           EVALUATE TRUE
               WHEN PND-DEPOSIT  MOVE 'DEPOSIT'  TO WS-TYPE-NAME
               WHEN PND-WITHDRAW MOVE 'WITHDRAW' TO WS-TYPE-NAME
               WHEN PND-TRANSFER MOVE 'TRANSFER' TO WS-TYPE-NAME
               WHEN OTHER        MOVE PND-TYPE   TO WS-TYPE-NAME
           END-EVALUATE
           PERFORM 8200-STATUS-NAME
           MOVE PND-AMOUNT TO ED-AMT
           DISPLAY PND-ID ' ' WS-TYPE-NAME ' ' PND-FROM-ACCT ' '
                   PND-TO-ACCT ' ' ED-AMT ' ' PND-MAKER ' '
                   PND-MAKE-DATE.

       8200-STATUS-NAME.
           EVALUATE TRUE
               WHEN PND-PENDING  MOVE 'PENDING'  TO WS-STATUS-NAME
               WHEN PND-APPROVED MOVE 'APPROVED' TO WS-STATUS-NAME
               WHEN PND-REJECTED MOVE 'REJECTED' TO WS-STATUS-NAME
               WHEN PND-FAILED   MOVE 'FAILED'   TO WS-STATUS-NAME
               WHEN PND-EXPIRED  MOVE 'EXPIRED'  TO WS-STATUS-NAME
               WHEN OTHER        MOVE 'UNKNOWN'  TO WS-STATUS-NAME
           END-EVALUATE.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       8990-GUARD-END-OF-INPUT.
           IF WS-CHOICE = SPACE
               ADD 1 TO WS-BLANK-INPUTS
               IF WS-BLANK-INPUTS > 20
                   SET EXIT-REQUESTED TO TRUE
               END-IF
           ELSE
               MOVE ZERO TO WS-BLANK-INPUTS
           END-IF.
