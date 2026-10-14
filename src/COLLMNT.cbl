       IDENTIFICATION DIVISION.
       PROGRAM-ID. COLLMNT.
      *================================================================*
      * PROGRAM  : COLLMNT                                             *
      * PURPOSE  : COLLECTIONS AND RECOVERY ON OVERDUE LOANS           *
      *              1 - OVERDUE WORKLIST (OLDEST FIRST BY DPD)        *
      *              2 - RECORD A FOLLOW-UP (CALL / VISIT / LETTER /   *
      *                  PROMISE TO PAY)                               *
      *              3 - FOLLOW-UP HISTORY OF A LOAN                   *
      *              4 - ONE-TIME SETTLEMENT (OTS)          (MANAGER)  *
      *              5 - TECHNICAL WRITE-OFF                (MANAGER)  *
      *              6 - RECOVERY ON A WRITTEN-OFF LOAN                *
      *            OTS: ONLY FOR NPA LOANS, AT LEAST 60% OF THE        *
      *            OUTSTANDING, PAID FROM THE LINKED ACCOUNT. THE      *
      *            AMOUNT RECEIVED CLEARS THAT MUCH OF THE LOAN; THE   *
      *            SACRIFICE IS ABSORBED BY THE LOAN LOSS PROVISION.   *
      *              DR CUSTOMER  CR 1290 SUSPENSE    (ACCTPOST, LR)   *
      *              DR 1290      CR LOAN GL          (RECEIVED)       *
      *              DR 1299      CR LOAN GL          (SACRIFICE)      *
      *            WRITE-OFF: NPA LOAN TAKEN OFF THE BOOKS AGAINST THE *
      *            PROVISION (DR 1299 CR LOAN GL); THE LOAN STAYS ON   *
      *            FILE MARKED 'W' SO RECOVERIES CAN BE TRACKED.       *
      *            RECOVERY AFTER WRITE-OFF IS INCOME:                 *
      *              DR CUSTOMER  CR 4103 BAD DEBTS RECOVERED          *
      * FILES    : LOANMAST (I-O), COLLLOG (EXTEND / INPUT)            *
      * CALLS    : ACCTPOST, GLPOST, DATEUTIL, AUDITLOG                *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS LOAN-NO FILE STATUS IS WS-LOAN-FS.
           SELECT LOG-FILE ASSIGN TO 'data/COLLLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LOG-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  LOG-FILE.
           COPY COLLREC.

       WORKING-STORAGE SECTION.
       01  WS-LOAN-FS                  PIC X(02).
       01  WS-LOG-FS                   PIC X(02).
       01  WS-CHOICE                   PIC X(01).
       01  WS-EXIT-SW                  PIC X(01) VALUE 'N'.
           88  EXIT-REQUESTED                    VALUE 'Y'.
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-TODAY                    PIC 9(08).
       01  WS-NOW                      PIC X(21).
       01  WS-IN-LOAN-NO               PIC 9(10).
       01  WS-IN-ACTION                PIC X(01).
       01  WS-IN-REMARKS               PIC X(40).
       01  WS-IN-PTP-DATE              PIC 9(08).
       01  WS-IN-AMOUNT                PIC 9(11)V99.
       01  WS-CONFIRM                  PIC X(01).
       01  WS-COUNT                    PIC 9(04).
       01  WS-DPD                      PIC S9(05).
       01  WS-OVERDUE-AMT              PIC 9(11)V99.
       01  WS-MIN-OTS-PCT              PIC 9V99 VALUE 0.60.
       01  WS-MIN-OTS                  PIC 9(11)V99.
       01  WS-SACRIFICE                PIC 9(11)V99.
       01  WS-LOAN-GL                  PIC 9(04).
      * WORKLIST, SORTED BY DPD (SIMPLE INSERTION, MAX 500)
       01  WS-WL-COUNT                 PIC 9(04) VALUE ZERO.
       01  WS-WORKLIST.
           05  WS-WL OCCURS 500 TIMES.
               10  WL-DPD              PIC 9(05).
               10  WL-LOAN-NO          PIC 9(10).
               10  WL-ACCT-NO          PIC 9(10).
               10  WL-TYPE             PIC X(01).
               10  WL-EMIS             PIC 9(03).
               10  WL-AMOUNT           PIC 9(11)V99.
               10  WL-CLASS            PIC X(01).
       01  WS-I                        PIC 9(04).
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-AMT2                     PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-DPD                      PIC ZZZZ9.
           COPY POSTPARM.
           COPY GLPARM.
           COPY GLCODES.
           COPY DATEPARM.
           COPY SESSION.
           COPY AUDPARM.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           OPEN I-O LOAN-FILE
           IF WS-LOAN-FS NOT = '00'
               DISPLAY 'NO LOAN MASTER.'
               GOBACK
           END-IF
           MOVE 'N' TO WS-EXIT-SW
           PERFORM UNTIL EXIT-REQUESTED
               DISPLAY ' '
               DISPLAY '--------- COLLECTIONS AND RECOVERY ---------'
               DISPLAY ' 1. OVERDUE WORKLIST'
               DISPLAY ' 2. RECORD A FOLLOW-UP'
               DISPLAY ' 3. FOLLOW-UP HISTORY'
               IF SES-CAN-APPROVE
                   DISPLAY ' 4. ONE-TIME SETTLEMENT'
                   DISPLAY ' 5. WRITE OFF'
               END-IF
               DISPLAY ' 6. RECOVERY ON A WRITTEN-OFF LOAN'
               DISPLAY ' 9. BACK'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               PERFORM 8990-GUARD-END-OF-INPUT
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 1000-WORKLIST
                   WHEN '2' PERFORM 2000-FOLLOW-UP
                   WHEN '3' PERFORM 3000-HISTORY
                   WHEN '4' PERFORM 4000-SETTLEMENT
                   WHEN '5' PERFORM 5000-WRITE-OFF
                   WHEN '6' PERFORM 6000-RECOVERY
                   WHEN '9' SET EXIT-REQUESTED TO TRUE
                   WHEN SPACE CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE LOAN-FILE
           GOBACK.

      *================================================================*
      * 1 - WORKLIST                                                   *
      *================================================================*
       1000-WORKLIST.
           MOVE ZERO TO WS-WL-COUNT
           MOVE 'N' TO WS-EOF-SW
           MOVE ZERO TO LOAN-NO
           START LOAN-FILE KEY IS NOT LESS THAN LOAN-NO
               INVALID KEY SET END-OF-FILE TO TRUE
           END-START
           PERFORM UNTIL END-OF-FILE
               READ LOAN-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF LOAN-OVERDUE-EMIS > ZERO
                          AND NOT LOAN-CLOSED
                          AND NOT LOAN-WRITTEN-OFF
                          AND WS-WL-COUNT < 500
                           PERFORM 1100-ADD-TO-WORKLIST
                       END-IF
               END-READ
           END-PERFORM
           DISPLAY '  DPD  LOAN NO    ACCOUNT    T EMIS        '
                   ' OVERDUE C'
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-WL-COUNT
               MOVE WL-DPD(WS-I)    TO ED-DPD
               MOVE WL-AMOUNT(WS-I) TO ED-AMT
               DISPLAY ED-DPD ' ' WL-LOAN-NO(WS-I) ' '
                       WL-ACCT-NO(WS-I) ' ' WL-TYPE(WS-I) ' '
                       WL-EMIS(WS-I) ' ' ED-AMT ' ' WL-CLASS(WS-I)
           END-PERFORM
           DISPLAY WS-WL-COUNT ' OVERDUE LOAN(S)'.

       1100-ADD-TO-WORKLIST.
           PERFORM 7000-DPD
           COMPUTE WS-OVERDUE-AMT = LOAN-OVERDUE-EMIS * LOAN-EMI
                                    + LOAN-PENALTY-DUE
      *    INSERT KEEPING THE HIGHEST DPD FIRST
           ADD 1 TO WS-WL-COUNT
           MOVE WS-WL-COUNT TO WS-I
           PERFORM UNTIL WS-I = 1 OR WL-DPD(WS-I - 1) >= WS-DPD
               MOVE WS-WL(WS-I - 1) TO WS-WL(WS-I)
               SUBTRACT 1 FROM WS-I
           END-PERFORM
           MOVE WS-DPD            TO WL-DPD(WS-I)
           MOVE LOAN-NO           TO WL-LOAN-NO(WS-I)
           MOVE LOAN-ACCT-NO      TO WL-ACCT-NO(WS-I)
           MOVE LOAN-TYPE         TO WL-TYPE(WS-I)
           MOVE LOAN-OVERDUE-EMIS TO WL-EMIS(WS-I)
           MOVE WS-OVERDUE-AMT    TO WL-AMOUNT(WS-I)
           MOVE LOAN-ASSET-CLASS  TO WL-CLASS(WS-I).

      *================================================================*
      * 2 / 3 - FOLLOW-UPS                                             *
      *================================================================*
       2000-FOLLOW-UP.
           PERFORM 7100-READ-LOAN
           IF WS-LOAN-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'ACTION C=CALL V=VISIT L=LETTER P=PROMISE TO PAY: '
                   WITH NO ADVANCING
           ACCEPT WS-IN-ACTION
           MOVE FUNCTION UPPER-CASE(WS-IN-ACTION) TO WS-IN-ACTION
           IF WS-IN-ACTION NOT = 'C' AND NOT = 'V' AND NOT = 'L'
              AND NOT = 'P'
               DISPLAY 'ACTION MUST BE C, V, L OR P.'
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO WS-IN-PTP-DATE WS-IN-AMOUNT
           IF WS-IN-ACTION = 'P'
               DISPLAY 'PROMISED DATE (YYYYMMDD): ' WITH NO ADVANCING
               ACCEPT WS-IN-PTP-DATE
               DISPLAY 'PROMISED AMOUNT        : ' WITH NO ADVANCING
               ACCEPT WS-IN-AMOUNT
               SET DU-VALIDATE TO TRUE
               MOVE WS-IN-PTP-DATE TO DU-DATE-1
               CALL 'DATEUTIL' USING DATE-PARMS
               IF NOT DU-OK OR WS-IN-PTP-DATE < WS-TODAY
                  OR WS-IN-AMOUNT NOT > ZERO
                   DISPLAY 'PROMISE NEEDS A FUTURE DATE AND AN AMOUNT.'
                   EXIT PARAGRAPH
               END-IF
           END-IF
           DISPLAY 'REMARKS : ' WITH NO ADVANCING
           ACCEPT WS-IN-REMARKS
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           INITIALIZE COLL-RECORD
           MOVE LOAN-NO         TO CL-LOAN-NO
           MOVE WS-TODAY        TO CL-DATE
           MOVE WS-NOW(9:6)     TO CL-TIME
           MOVE WS-IN-ACTION    TO CL-ACTION
           MOVE WS-IN-PTP-DATE  TO CL-PTP-DATE
           MOVE WS-IN-AMOUNT    TO CL-PTP-AMOUNT
           MOVE WS-IN-REMARKS   TO CL-REMARKS
           MOVE SES-USER-ID     TO CL-USER
           PERFORM 7300-APPEND-LOG
           DISPLAY 'FOLLOW-UP RECORDED FOR LOAN ' LOAN-NO.

       3000-HISTORY.
           DISPLAY 'LOAN NO : ' WITH NO ADVANCING
           ACCEPT WS-IN-LOAN-NO
           MOVE ZERO TO WS-COUNT
           OPEN INPUT LOG-FILE
           IF WS-LOG-FS NOT = '00'
               DISPLAY 'NO FOLLOW-UPS RECORDED YET.'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ LOG-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF CL-LOAN-NO = WS-IN-LOAN-NO
                           ADD 1 TO WS-COUNT
                           MOVE CL-PTP-AMOUNT TO ED-AMT
                           DISPLAY CL-DATE ' ' CL-ACTION ' ' CL-USER
                                   ' ' CL-REMARKS
                           IF CL-ACTION = 'P'
                               DISPLAY '         PROMISED ' ED-AMT
                                       ' BY ' CL-PTP-DATE
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE LOG-FILE
           DISPLAY WS-COUNT ' FOLLOW-UP(S)'.

      *================================================================*
      * 4 - ONE-TIME SETTLEMENT                                        *
      *================================================================*
       4000-SETTLEMENT.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'NEEDS A MANAGER.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 7100-READ-LOAN
           IF WS-LOAN-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT LOAN-NPA OR LOAN-WRITTEN-OFF
               DISPLAY 'OTS IS ONLY FOR NPA LOANS STILL ON THE BOOKS.'
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-MIN-OTS ROUNDED =
                   LOAN-OUTSTANDING * WS-MIN-OTS-PCT
           MOVE LOAN-OUTSTANDING TO ED-AMT
           MOVE WS-MIN-OTS TO ED-AMT2
           DISPLAY 'OUTSTANDING : ' ED-AMT '  MINIMUM OTS : ' ED-AMT2
           DISPLAY 'SETTLEMENT AMOUNT : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           IF WS-IN-AMOUNT < WS-MIN-OTS
              OR WS-IN-AMOUNT > LOAN-OUTSTANDING
               DISPLAY 'SETTLEMENT MUST BE BETWEEN THE MINIMUM AND THE'
                       ' OUTSTANDING.'
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-SACRIFICE = LOAN-OUTSTANDING - WS-IN-AMOUNT
           MOVE WS-SACRIFICE TO ED-AMT
           DISPLAY 'BANK GIVES UP ' ED-AMT '. CONFIRM (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
               DISPLAY 'NOT SETTLED.'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE LOAN-ACCT-NO  TO PP-ACCT-NO
           MOVE WS-IN-AMOUNT  TO PP-AMOUNT
           MOVE 'LR'          TO PP-TRAN-TYPE
           MOVE LOAN-NO       TO PP-REF-ACCT
           MOVE 'ONE TIME SETTLEMENT' TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'SETTLEMENT NOT COLLECTED, RC=' PP-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           PERFORM 7200-LOAN-GL
           MOVE GLC-LOAN-REPAY-SUSP TO GP-DR-GL
           MOVE WS-LOAN-GL          TO GP-CR-GL
           MOVE WS-IN-AMOUNT        TO GP-AMOUNT
           MOVE 'OTS AMOUNT RECEIVED' TO GP-NARRATION
           PERFORM 7400-GL
           IF WS-SACRIFICE > ZERO
               MOVE GLC-LOAN-PROVISION TO GP-DR-GL
               MOVE WS-LOAN-GL         TO GP-CR-GL
               MOVE WS-SACRIFICE       TO GP-AMOUNT
               MOVE 'OTS LOSS AGAINST PROVISION' TO GP-NARRATION
               PERFORM 7400-GL
           END-IF
           MOVE ZERO TO LOAN-OUTSTANDING LOAN-OVERDUE-EMIS
                        LOAN-PENALTY-DUE
           SET LOAN-CLOSED TO TRUE
           MOVE WS-TODAY TO LOAN-CLOSE-DATE LOAN-LAST-PAY-DATE
           REWRITE LOAN-RECORD
           DISPLAY 'LOAN ' LOAN-NO ' SETTLED AND CLOSED.'
           MOVE 'OTS' TO AU-ACTION
           MOVE WS-IN-AMOUNT TO ED-AMT
           PERFORM 9000-AUDIT
           INITIALIZE COLL-RECORD
           MOVE LOAN-NO  TO CL-LOAN-NO
           MOVE WS-TODAY TO CL-DATE
           MOVE 'S'      TO CL-ACTION
           MOVE SES-USER-ID TO CL-USER
           STRING 'OTS SETTLED FOR ' ED-AMT DELIMITED BY SIZE
               INTO CL-REMARKS
           PERFORM 7300-APPEND-LOG.

      *================================================================*
      * 5 - WRITE-OFF                                                  *
      *================================================================*
       5000-WRITE-OFF.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'NEEDS A MANAGER.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 7100-READ-LOAN
           IF WS-LOAN-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT LOAN-NPA OR LOAN-WRITTEN-OFF
               DISPLAY 'ONLY AN NPA LOAN CAN BE WRITTEN OFF.'
               EXIT PARAGRAPH
           END-IF
           MOVE LOAN-OUTSTANDING TO ED-AMT
           DISPLAY 'WRITE OFF ' ED-AMT ' AGAINST PROVISION (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
               DISPLAY 'NOT WRITTEN OFF.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 7200-LOAN-GL
           MOVE GLC-LOAN-PROVISION TO GP-DR-GL
           MOVE WS-LOAN-GL         TO GP-CR-GL
           MOVE LOAN-OUTSTANDING   TO GP-AMOUNT
           MOVE 'TECHNICAL WRITE-OFF' TO GP-NARRATION
           PERFORM 7400-GL
           IF NOT GP-OK
               EXIT PARAGRAPH
           END-IF
           SET LOAN-WRITTEN-OFF TO TRUE
           REWRITE LOAN-RECORD
           DISPLAY 'LOAN ' LOAN-NO ' WRITTEN OFF - RECOVERY CONTINUES.'
           MOVE 'WRITE-OFF' TO AU-ACTION
           PERFORM 9000-AUDIT.

      *================================================================*
      * 6 - RECOVERY AFTER WRITE-OFF                                   *
      *================================================================*
       6000-RECOVERY.
           PERFORM 7100-READ-LOAN
           IF WS-LOAN-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT LOAN-WRITTEN-OFF OR LOAN-CLOSED
               DISPLAY 'LOAN IS NOT AN OPEN WRITTEN-OFF LOAN.'
               EXIT PARAGRAPH
           END-IF
           MOVE LOAN-OUTSTANDING TO ED-AMT
           DISPLAY 'STILL RECOVERABLE : ' ED-AMT
           DISPLAY 'AMOUNT RECOVERED  : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           IF WS-IN-AMOUNT NOT > ZERO
              OR WS-IN-AMOUNT > LOAN-OUTSTANDING
               DISPLAY 'AMOUNT MUST BE UP TO THE RECOVERABLE BALANCE.'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE LOAN-ACCT-NO TO PP-ACCT-NO
           MOVE WS-IN-AMOUNT TO PP-AMOUNT
           MOVE 'LR'         TO PP-TRAN-TYPE
           MOVE LOAN-NO      TO PP-REF-ACCT
           MOVE GLC-BAD-DEBT-RECOVERED TO PP-CONTRA-GL
           MOVE 'RECOVERY - WRITTEN-OFF LOAN' TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'RECOVERY NOT COLLECTED, RC=' PP-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           SUBTRACT WS-IN-AMOUNT FROM LOAN-OUTSTANDING
           MOVE WS-TODAY TO LOAN-LAST-PAY-DATE
           IF LOAN-OUTSTANDING = ZERO
               SET LOAN-CLOSED TO TRUE
               MOVE WS-TODAY TO LOAN-CLOSE-DATE
           END-IF
           REWRITE LOAN-RECORD
           MOVE WS-IN-AMOUNT TO ED-AMT
           DISPLAY 'RECOVERED ' ED-AMT ' - BOOKED AS INCOME.'
           MOVE 'RECOVERY' TO AU-ACTION
           PERFORM 9000-AUDIT.

      *================================================================*
      * COMMON                                                         *
      *================================================================*
       7000-DPD.
           MOVE ZERO TO WS-DPD
           SET DU-ADD-MONTHS TO TRUE
           MOVE LOAN-NEXT-DUE-DATE TO DU-DATE-1
           COMPUTE DU-NUMBER = 0 - LOAN-OVERDUE-EMIS
           CALL 'DATEUTIL' USING DATE-PARMS
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE DU-RESULT-DATE TO DU-DATE-1
           MOVE WS-TODAY       TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           IF DU-OK AND DU-RESULT-DAYS > ZERO
               MOVE DU-RESULT-DAYS TO WS-DPD
           END-IF.

       7100-READ-LOAN.
           DISPLAY 'LOAN NO : ' WITH NO ADVANCING
           ACCEPT WS-IN-LOAN-NO
           MOVE WS-IN-LOAN-NO TO LOAN-NO
           READ LOAN-FILE
               INVALID KEY
                   DISPLAY 'NO SUCH LOAN.'
           END-READ.

       7200-LOAN-GL.
           EVALUATE TRUE
               WHEN LOAN-GOLD       MOVE GLC-GOLD-LOANS    TO WS-LOAN-GL
               WHEN LOAN-AGAINST-FD MOVE GLC-DEPOSIT-LOANS TO WS-LOAN-GL
               WHEN OTHER           MOVE GLC-LOANS         TO WS-LOAN-GL
           END-EVALUATE.

       7300-APPEND-LOG.
           OPEN EXTEND LOG-FILE
           IF WS-LOG-FS = '35'
               OPEN OUTPUT LOG-FILE
           END-IF
           WRITE COLL-RECORD
           CLOSE LOG-FILE.

       7400-GL.
           MOVE SPACES TO GP-REF
           STRING 'LOAN' LOAN-NO DELIMITED BY SIZE INTO GP-REF
           CALL 'GLPOST' USING GL-PARMS
           IF NOT GP-OK
               DISPLAY 'GL POSTING FAILED, RC=' GP-RC
           END-IF.

       9000-AUDIT.
           MOVE 'COLLMNT' TO AU-PROGRAM
           MOVE LOAN-NO   TO AU-KEY
           MOVE SPACES    TO AU-DETAIL
           STRING 'ACCOUNT ' LOAN-ACCT-NO ' ' ED-AMT
               DELIMITED BY SIZE INTO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS.

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
