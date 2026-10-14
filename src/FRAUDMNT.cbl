       IDENTIFICATION DIVISION.
       PROGRAM-ID. FRAUDMNT.
      *================================================================*
      * PROGRAM  : FRAUDMNT                                            *
      * PURPOSE  : FRAUD ALERT REVIEW (MANAGER)                        *
      *              1 - OPEN ALERTS                                   *
      *              2 - REVIEW AN ALERT: GENUINE OR FRAUD             *
      *              3 - ALERT HISTORY OF AN ACCOUNT                   *
      *            CONFIRMING FRAUD FREEZES THE ACCOUNT AT ONCE (NO    *
      *            MORE DEBITS THROUGH ACCTPOST) - UNFREEZING IS A     *
      *            DELIBERATE STEP IN ACCOUNT MAINTENANCE.             *
      *            A REVIEW NEEDS REMARKS; EVERY DECISION IS AUDITED.  *
      * FILES    : FRAUDALT (I-O), ACCTMAST (I-O)                      *
      * CALLS    : AUDITLOG                                            *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALERT-FILE ASSIGN TO 'data/FRAUDALT.DAT'
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS FA-KEY FILE STATUS IS WS-FS.
           SELECT ACCT-FILE ASSIGN TO 'data/ACCTMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS RANDOM
               RECORD KEY IS ACCT-NO FILE STATUS IS WS-ACCT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  ALERT-FILE.
           COPY FRAUDREC.
       FD  ACCT-FILE.
           COPY ACCTREC.

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-ACCT-FS                  PIC X(02).
       01  WS-CHOICE                   PIC X(01).
       01  WS-EXIT-SW                  PIC X(01) VALUE 'N'.
           88  EXIT-REQUESTED                    VALUE 'Y'.
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-TODAY                    PIC 9(08).
       01  WS-IN-DATE                  PIC 9(08).
       01  WS-IN-ACCT                  PIC 9(10).
       01  WS-IN-RULE                  PIC X(02).
       01  WS-IN-VERDICT               PIC X(01).
       01  WS-IN-REMARKS               PIC X(30).
       01  WS-COUNT                    PIC 9(04).
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-RULE-NAME                PIC X(14).
       01  WS-STATUS-NAME              PIC X(08).
           COPY SESSION.
           COPY AUDPARM.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'FRAUD REVIEW NEEDS A MANAGER.'
               GOBACK
           END-IF
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           OPEN I-O ALERT-FILE
           IF WS-FS NOT = '00'
               DISPLAY 'NO FRAUD ALERTS HAVE BEEN RAISED YET.'
               GOBACK
           END-IF
           MOVE 'N' TO WS-EXIT-SW
           PERFORM UNTIL EXIT-REQUESTED
               DISPLAY ' '
               DISPLAY '------------- FRAUD ALERTS -------------'
               DISPLAY ' 1. OPEN ALERTS'
               DISPLAY ' 2. REVIEW AN ALERT'
               DISPLAY ' 3. ALERTS OF AN ACCOUNT'
               DISPLAY ' 9. BACK'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               PERFORM 8990-GUARD-END-OF-INPUT
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 1000-OPEN-ALERTS
                   WHEN '2' PERFORM 2000-REVIEW
                   WHEN '3' PERFORM 3000-ACCOUNT-ALERTS
                   WHEN '9' SET EXIT-REQUESTED TO TRUE
                   WHEN SPACE CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE ALERT-FILE
           GOBACK.

       1000-OPEN-ALERTS.
           MOVE ZERO TO WS-COUNT
           PERFORM 8000-START-BROWSE
           PERFORM UNTIL END-OF-FILE
               READ ALERT-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF FA-OPEN
                           ADD 1 TO WS-COUNT
                           PERFORM 8100-SHOW
                       END-IF
               END-READ
           END-PERFORM
           DISPLAY WS-COUNT ' OPEN ALERT(S)'.

       2000-REVIEW.
           DISPLAY 'ALERT DATE (YYYYMMDD) : ' WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           DISPLAY 'ACCOUNT NO            : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT
           DISPLAY 'RULE (VL/OH/NP/DW/PT) : ' WITH NO ADVANCING
           ACCEPT WS-IN-RULE
           MOVE WS-IN-DATE TO FA-DATE
           MOVE WS-IN-ACCT TO FA-ACCT-NO
           MOVE FUNCTION UPPER-CASE(WS-IN-RULE) TO FA-RULE
           READ ALERT-FILE
               INVALID KEY
                   DISPLAY 'NO SUCH ALERT.'
                   EXIT PARAGRAPH
           END-READ
           IF NOT FA-OPEN
               DISPLAY 'ALERT ALREADY REVIEWED BY ' FA-REVIEWED-BY
               EXIT PARAGRAPH
           END-IF
           PERFORM 8100-SHOW
           DISPLAY 'G=GENUINE  F=FRAUD (FREEZES THE ACCOUNT): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-VERDICT
           MOVE FUNCTION UPPER-CASE(WS-IN-VERDICT) TO WS-IN-VERDICT
           IF WS-IN-VERDICT NOT = 'G' AND WS-IN-VERDICT NOT = 'F'
               DISPLAY 'NO DECISION RECORDED.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'REMARKS : ' WITH NO ADVANCING
           ACCEPT WS-IN-REMARKS
           IF WS-IN-REMARKS = SPACES
               DISPLAY 'REMARKS ARE REQUIRED.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-VERDICT TO FA-STATUS
           MOVE WS-IN-REMARKS TO FA-REMARKS
           MOVE SES-USER-ID   TO FA-REVIEWED-BY
           MOVE WS-TODAY      TO FA-REVIEW-DATE
           REWRITE FRAUD-RECORD
           IF FA-FRAUD
               PERFORM 2100-FREEZE-ACCOUNT
           ELSE
               DISPLAY 'ALERT CLOSED AS GENUINE.'
           END-IF
           MOVE 'FRAUDMNT'  TO AU-PROGRAM
           MOVE 'ALERT ' TO AU-ACTION
           MOVE FA-RULE TO AU-ACTION(7:2)
           MOVE FA-ACCT-NO  TO AU-KEY
           MOVE SPACES TO AU-DETAIL
           STRING FA-STATUS ' ' FA-REMARKS DELIMITED BY SIZE
               INTO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS.

       2100-FREEZE-ACCOUNT.
           OPEN I-O ACCT-FILE
           IF WS-ACCT-FS NOT = '00'
               DISPLAY 'ACCOUNT MASTER NOT AVAILABLE - FREEZE MANUALLY.'
               EXIT PARAGRAPH
           END-IF
           MOVE FA-ACCT-NO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   DISPLAY 'ACCOUNT NOT FOUND.'
               NOT INVALID KEY
                   IF ACCT-ACTIVE OR ACCT-DORMANT
                       SET ACCT-FROZEN TO TRUE
                       REWRITE ACCT-RECORD
                       DISPLAY 'FRAUD CONFIRMED - ACCOUNT ' ACCT-NO
                               ' FROZEN.'
                       MOVE 'FRAUDMNT' TO AU-PROGRAM
                       MOVE 'FREEZE'   TO AU-ACTION
                       MOVE ACCT-NO    TO AU-KEY
                       MOVE 'FRAUD CONFIRMED ON ALERT REVIEW'
                           TO AU-DETAIL
                       CALL 'AUDITLOG' USING AUD-PARMS
                   ELSE
                       DISPLAY 'FRAUD CONFIRMED - ACCOUNT ALREADY '
                               'NOT ACTIVE (' ACCT-STATUS ').'
                   END-IF
           END-READ
           CLOSE ACCT-FILE.

       3000-ACCOUNT-ALERTS.
           DISPLAY 'ACCOUNT NO : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT
           MOVE ZERO TO WS-COUNT
           PERFORM 8000-START-BROWSE
           PERFORM UNTIL END-OF-FILE
               READ ALERT-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF FA-ACCT-NO = WS-IN-ACCT
                           ADD 1 TO WS-COUNT
                           PERFORM 8100-SHOW
                       END-IF
               END-READ
           END-PERFORM
           DISPLAY WS-COUNT ' ALERT(S)'.

       8000-START-BROWSE.
           MOVE 'N' TO WS-EOF-SW
           MOVE LOW-VALUES TO FA-KEY
           START ALERT-FILE KEY IS NOT LESS THAN FA-KEY
               INVALID KEY SET END-OF-FILE TO TRUE
           END-START.

       8100-SHOW.
           EVALUATE TRUE
               WHEN FA-VELOCITY     MOVE 'VELOCITY'      TO WS-RULE-NAME
               WHEN FA-ODD-HOURS    MOVE 'ODD HOURS'     TO WS-RULE-NAME
               WHEN FA-NEW-PAYEE    MOVE 'NEW PAYEE'     TO WS-RULE-NAME
               WHEN FA-DORMANT-WAKE MOVE 'DORMANT WAKE'  TO WS-RULE-NAME
               WHEN FA-PASS-THROUGH MOVE 'PASS-THROUGH'  TO WS-RULE-NAME
               WHEN OTHER           MOVE FA-RULE         TO WS-RULE-NAME
           END-EVALUATE
           EVALUATE TRUE
               WHEN FA-OPEN    MOVE 'OPEN'    TO WS-STATUS-NAME
               WHEN FA-GENUINE MOVE 'GENUINE' TO WS-STATUS-NAME
               WHEN FA-FRAUD   MOVE 'FRAUD'   TO WS-STATUS-NAME
           END-EVALUATE
           MOVE FA-AMOUNT TO ED-AMT
           DISPLAY FA-DATE ' ' FA-ACCT-NO ' ' FA-RULE ' '
                   WS-RULE-NAME ' ' ED-AMT ' ' WS-STATUS-NAME
           DISPLAY '         ' FA-DETAIL.

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
