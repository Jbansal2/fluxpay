       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXNEXEC.
      *================================================================*
      * PROGRAM  : TXNEXEC                                             *
      * PURPOSE  : EXECUTES ONE TELLER TRANSACTION - CASH DEPOSIT,     *
      *            CASH WITHDRAWAL OR INTERNAL TRANSFER - THROUGH      *
      *            ACCTPOST. SHARED BY TRANPOST (DIRECT POSTING) AND   *
      *            APPRVMNT (POSTING AN APPROVED MAKER-CHECKER ITEM),  *
      *            SO BOTH PATHS FOLLOW EXACTLY THE SAME RULES.        *
      *            TRANSFER: TARGET CHECKED FIRST, SOURCE DEBITED,     *
      *            TARGET CREDITED; A FAILED CREDIT REVERSES THE DEBIT *
      *            SO THE TRANSFER SUSPENSE GL ALWAYS CLEARS.          *
      *            CASH ITEMS ALSO UPDATE THE TELLER'S DRAWER          *
      *            (CASHUTIL).                                         *
      * LINKAGE  : TXN-PARMS (IN/OUT), POST-PARMS (LAST ACCTPOST       *
      *            RESULT, FOR THE CALLER'S MESSAGES)                  *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION (LOGIC MOVED OUT OF TRANPOST)       *
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SAVE-RC                  PIC 9(02).
           COPY CSHPARM.

       LINKAGE SECTION.
           COPY TXNPARM.
           COPY POSTPARM.

       PROCEDURE DIVISION USING TXN-PARMS POST-PARMS.
       0000-MAIN.
           SET TX-OK TO TRUE
           EVALUATE TRUE
               WHEN TX-DEPOSIT
                   PERFORM 1000-DEPOSIT
               WHEN TX-WITHDRAW
                   PERFORM 2000-WITHDRAW
               WHEN TX-TRANSFER
                   PERFORM 3000-TRANSFER
               WHEN OTHER
                   SET TX-POST-FAILED TO TRUE
                   INITIALIZE POST-PARMS
                   SET PP-BAD-REQUEST TO TRUE
           END-EVALUATE
           GOBACK.

       1000-DEPOSIT.
           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE TX-FROM-ACCT TO PP-ACCT-NO
           MOVE TX-AMOUNT    TO PP-AMOUNT
           MOVE 'DP'         TO PP-TRAN-TYPE
           MOVE TX-NARRATION TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               SET TX-POST-FAILED TO TRUE
           ELSE
               SET CSH-RECEIPT TO TRUE
               PERFORM 4000-DRAWER
           END-IF.

       2000-WITHDRAW.
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE TX-FROM-ACCT TO PP-ACCT-NO
           MOVE TX-AMOUNT    TO PP-AMOUNT
           MOVE 'WD'         TO PP-TRAN-TYPE
           MOVE TX-NARRATION TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               SET TX-POST-FAILED TO TRUE
           ELSE
               SET CSH-PAYMENT TO TRUE
               PERFORM 4000-DRAWER
           END-IF.

       3000-TRANSFER.
           IF TX-TO-ACCT = TX-FROM-ACCT
               SET TX-SAME-ACCOUNT TO TRUE
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE TX-TO-ACCT TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               SET TX-TARGET-MISSING TO TRUE
               EXIT PARAGRAPH
           END-IF

           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE TX-FROM-ACCT TO PP-ACCT-NO
           MOVE TX-AMOUNT    TO PP-AMOUNT
           MOVE 'TO'         TO PP-TRAN-TYPE
           MOVE TX-TO-ACCT   TO PP-REF-ACCT
           MOVE TX-NARRATION TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               SET TX-POST-FAILED TO TRUE
               EXIT PARAGRAPH
           END-IF

           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE TX-TO-ACCT   TO PP-ACCT-NO
           MOVE TX-AMOUNT    TO PP-AMOUNT
           MOVE 'TI'         TO PP-TRAN-TYPE
           MOVE TX-FROM-ACCT TO PP-REF-ACCT
           MOVE TX-NARRATION TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               MOVE PP-RETURN-CODE TO WS-SAVE-RC
               PERFORM 3100-REVERSE-DEBIT
               MOVE WS-SAVE-RC TO PP-RETURN-CODE
           END-IF.

       3100-REVERSE-DEBIT.
           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE TX-FROM-ACCT TO PP-ACCT-NO
           MOVE TX-AMOUNT    TO PP-AMOUNT
           MOVE 'TI'         TO PP-TRAN-TYPE
           MOVE TX-TO-ACCT   TO PP-REF-ACCT
           MOVE 'REVERSAL OF FAILED TRANSFER' TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF PP-OK
               SET TX-REVERSED TO TRUE
           ELSE
               SET TX-REVERSAL-FAILED TO TRUE
           END-IF.

      *    CASH THROUGH THE COUNTER: KEEP THE TELLER'S DRAWER IN STEP
       4000-DRAWER.
           MOVE TX-CASH-USER   TO CSH-USER
           MOVE TX-CASH-BRANCH TO CSH-BRANCH
           MOVE TX-AMOUNT      TO CSH-AMOUNT
           CALL 'CASHUTIL' USING CSH-PARMS
           MOVE CSH-RC TO TX-CASH-RC.
