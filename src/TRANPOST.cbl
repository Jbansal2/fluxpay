       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANPOST.
      *================================================================*
      * PROGRAM  : TRANPOST                                            *
      * PURPOSE  : ONLINE TRANSACTION POSTING - CASH DEPOSIT, CASH     *
      *            WITHDRAWAL AND INTERNAL FUND TRANSFER. ALL MONEY    *
      *            MOVES THROUGH ACCTPOST, SO BALANCE RULES, OVERDRAFT *
      *            LIMITS, DORMANCY, GL ENTRIES AND AML SCREENING ARE  *
      *            APPLIED THE SAME WAY AS EVERYWHERE ELSE.            *
      *            TELLER ITEMS ABOVE 2,00,000 GO TO THE MAKER-CHECKER *
      *            QUEUE INSTEAD OF BEING POSTED (APPROVED IN APPRVMNT)*
      * CALLS    : TXNEXEC, MKRQUEUE                                   *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      * 2026-10-14 REWORKED TO POST THROUGH ACCTPOST INSTEAD OF        *
      *            UPDATING THE MASTER DIRECTLY                        *
      * 2026-10-14 POSTING MOVED TO TXNEXEC; MAKER-CHECKER LIMIT       *
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY POSTPARM.
           COPY TXNPARM.
           COPY MKRPARM.
           COPY SESSION.

       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
           88  CHOICE-BACK                       VALUE '9'.

       01  WS-IN-ACCT-NO               PIC 9(10).
       01  WS-IN-TO-ACCT-NO            PIC 9(10).
       01  WS-IN-AMOUNT                PIC 9(11)V99.
       01  WS-IN-NARRATION             PIC X(30).

       01  WS-OK-SW                    PIC X(01) VALUE 'Y'.
           88  INPUT-OK                          VALUE 'Y'.
           88  INPUT-BAD                         VALUE 'N'.

       01  WS-DISP-AMT                 PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-BAL                 PIC -,---,---,---,--9.99.

      * LIMITS
       01  WS-MAX-SINGLE-TXN           PIC 9(11)V99 VALUE 1000000.00.
       01  WS-PAN-CASH-LIMIT           PIC 9(11)V99 VALUE 50000.00.
      * A TELLER POSTS UP TO THIS ALONE; ABOVE IT NEEDS A CHECKER
       01  WS-MAKER-LIMIT              PIC 9(11)V99 VALUE 200000.00.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL CHOICE-BACK
               PERFORM 1000-SHOW-MENU
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-DEPOSIT
                   WHEN '2' PERFORM 3000-WITHDRAW
                   WHEN '3' PERFORM 4000-TRANSFER
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           GOBACK.

       1000-SHOW-MENU.
           DISPLAY ' '
           DISPLAY '---------- TRANSACTION POSTING ----------'
           DISPLAY ' 1. CASH DEPOSIT'
           DISPLAY ' 2. CASH WITHDRAWAL'
           DISPLAY ' 3. FUND TRANSFER'
           DISPLAY ' 9. BACK TO MAIN MENU'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT.

       2000-DEPOSIT.
           PERFORM 7000-ACCEPT-ACCT-AND-AMOUNT
           IF INPUT-BAD
               EXIT PARAGRAPH
           END-IF
           IF WS-IN-AMOUNT >= WS-PAN-CASH-LIMIT
               DISPLAY 'NOTE: CASH OF 50,000 OR MORE - OBTAIN PAN OR'
                       ' FORM 60 FROM THE DEPOSITOR.'
           END-IF
           SET TX-DEPOSIT TO TRUE
           PERFORM 6000-POST-OR-QUEUE.

       3000-WITHDRAW.
           PERFORM 7000-ACCEPT-ACCT-AND-AMOUNT
           IF INPUT-BAD
               EXIT PARAGRAPH
           END-IF
           SET TX-WITHDRAW TO TRUE
           PERFORM 6000-POST-OR-QUEUE.

       4000-TRANSFER.
           PERFORM 7000-ACCEPT-ACCT-AND-AMOUNT
           IF INPUT-BAD
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'TO ACCOUNT NO : ' WITH NO ADVANCING
           ACCEPT WS-IN-TO-ACCT-NO
           SET TX-TRANSFER TO TRUE
           PERFORM 6000-POST-OR-QUEUE.

      *----------------------------------------------------------------*
      * MAKER-CHECKER: A TELLER MAY POST UP TO WS-MAKER-LIMIT ON HIS   *
      * OWN. ANYTHING BIGGER GOES TO THE APPROVAL QUEUE (APPRVMNT) AND *
      * IS POSTED ONLY WHEN ANOTHER USER APPROVES IT.                  *
      *----------------------------------------------------------------*
       6000-POST-OR-QUEUE.
           MOVE WS-IN-ACCT-NO    TO TX-FROM-ACCT
           MOVE WS-IN-TO-ACCT-NO TO TX-TO-ACCT
           MOVE WS-IN-AMOUNT     TO TX-AMOUNT
           MOVE WS-IN-NARRATION  TO TX-NARRATION
           MOVE SPACES           TO TX-CASH-USER TX-CASH-BRANCH
           IF TX-TRANSFER AND TX-TO-ACCT = TX-FROM-ACCT
               DISPLAY 'CANNOT TRANSFER TO THE SAME ACCOUNT.'
               EXIT PARAGRAPH
           END-IF
           IF SES-TELLER AND WS-IN-AMOUNT > WS-MAKER-LIMIT
               PERFORM 6100-QUEUE
           ELSE
               CALL 'TXNEXEC' USING TXN-PARMS POST-PARMS
               PERFORM 7400-SHOW-TXN-RESULT
               IF TX-OK AND SES-TELLER AND TX-CASH-RC = 04
                   DISPLAY 'NOTE: NO CASH DRAWER OPEN - CASH NOT '
                           'TRACKED. ASK THE MANAGER TO ISSUE CASH.'
               END-IF
           END-IF.

       6100-QUEUE.
           INITIALIZE MKR-PARMS
           SET MKR-ADD TO TRUE
           MOVE TX-FUNCTION  TO MKR-TYPE
           MOVE TX-FROM-ACCT TO MKR-FROM-ACCT
           MOVE TX-TO-ACCT   TO MKR-TO-ACCT
           MOVE TX-AMOUNT    TO MKR-AMOUNT
           MOVE TX-NARRATION TO MKR-NARRATION
           CALL 'MKRQUEUE' USING MKR-PARMS
           IF MKR-OK
               DISPLAY 'ABOVE TELLER LIMIT - SENT FOR APPROVAL, REF '
                       MKR-ID
           ELSE
               DISPLAY 'COULD NOT QUEUE FOR APPROVAL, RC=' MKR-RC
           END-IF.

       7400-SHOW-TXN-RESULT.
           EVALUATE TRUE
               WHEN TX-OK AND TX-TRANSFER
                   MOVE TX-AMOUNT TO WS-DISP-AMT
                   DISPLAY 'TRANSFERRED ' WS-DISP-AMT ' TO '
                           TX-TO-ACCT
               WHEN TX-SAME-ACCOUNT
                   DISPLAY 'CANNOT TRANSFER TO THE SAME ACCOUNT.'
               WHEN TX-TARGET-MISSING
                   DISPLAY 'TARGET ACCOUNT ' TX-TO-ACCT ' NOT FOUND.'
               WHEN TX-REVERSED
                   DISPLAY 'CREDIT TO TARGET FAILED (RC '
                           PP-RETURN-CODE ') - DEBIT REVERSED.'
               WHEN TX-REVERSAL-FAILED
                   DISPLAY 'CRITICAL: REVERSAL FAILED, RC='
                           PP-RETURN-CODE ' - REFER TO OPERATIONS'
               WHEN OTHER
                   PERFORM 7500-SHOW-RESULT
           END-EVALUATE.

       7000-ACCEPT-ACCT-AND-AMOUNT.
           SET INPUT-OK TO TRUE
           DISPLAY 'ACCOUNT NO    : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'AMOUNT        : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           DISPLAY 'NARRATION     : ' WITH NO ADVANCING
           ACCEPT WS-IN-NARRATION
           IF WS-IN-AMOUNT NOT > ZERO
               DISPLAY 'AMOUNT MUST BE GREATER THAN ZERO.'
               SET INPUT-BAD TO TRUE
           END-IF
           IF WS-IN-AMOUNT > WS-MAX-SINGLE-TXN
               DISPLAY 'AMOUNT EXCEEDS SINGLE TRANSACTION LIMIT.'
               SET INPUT-BAD TO TRUE
           END-IF.

       7500-SHOW-RESULT.
           EVALUATE TRUE
               WHEN PP-OK
                   MOVE PP-AMOUNT  TO WS-DISP-AMT
                   MOVE PP-BALANCE TO WS-DISP-BAL
                   DISPLAY 'POSTED SUCCESSFULLY.'
                   DISPLAY 'AMOUNT      : ' WS-DISP-AMT
                   DISPLAY 'NEW BALANCE : ' WS-DISP-BAL
               WHEN PP-NOT-FOUND
                   DISPLAY 'ACCOUNT ' PP-ACCT-NO ' NOT FOUND.'
               WHEN PP-NOT-ACTIVE
                   DISPLAY 'ACCOUNT ' PP-ACCT-NO
                           ' IS NOT ACTIVE (FROZEN, CLOSED OR DORMANT).'
               WHEN PP-INSUFFICIENT
                   MOVE PP-AVAILABLE TO WS-DISP-BAL
                   DISPLAY 'INSUFFICIENT FUNDS. AVAILABLE: '
                           WS-DISP-BAL
               WHEN OTHER
                   DISPLAY 'POSTING FAILED, RC=' PP-RETURN-CODE
           END-EVALUATE.

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
