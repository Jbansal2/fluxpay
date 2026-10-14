       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNEMIPAY.
      *================================================================*
      * PROGRAM  : LNEMIPAY                                            *
      * PURPOSE  : LOAN REPAYMENT ENGINE, SHARED BY THE LOAN SCREEN    *
      *            (LOANMNT) AND STANDING INSTRUCTIONS (SIBATCH).      *
      *            QUOTES OR COLLECTS THE NEXT EMI OR A FORECLOSURE:   *
      *              - DEBITS THE LINKED ACCOUNT (TYPE LR, GL 1290)    *
      *              - SPLITS 1290 INTO PRINCIPAL (1201), INTEREST AND *
      *                PENALTY (4001) AND FORECLOSURE CHARGE (4101),   *
      *                SO THE REPAYMENT SUSPENSE ALWAYS CLEARS         *
      *              - UPDATES THE LOAN RECORD IN THE CALLER'S BUFFER  *
      *            EMI SPLIT: INTEREST = OUTSTANDING * RATE / 1200,    *
      *            PRINCIPAL = EMI - INTEREST; THE LAST EMI CLEARS ANY *
      *            ROUNDING RESIDUE. FORECLOSURE CHARGE IS 2% OF THE   *
      *            OUTSTANDING (NIL ON HOME LOANS).                    *
      * CALL     : CALL 'LNEMIPAY' USING LNP-PARMS LOAN-RECORD         *
      * CALLS    : ACCTPOST, GLPOST, DATEUTIL                          *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION (LOGIC MOVED OUT OF LOANMNT)        *
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY GLPARM.
           COPY GLCODES.
           COPY DATEPARM.

       01  WS-MONTHLY-RATE             COMP-2.
       01  WS-FORECLOSE-PCT            PIC 9V99 VALUE 0.02.
       01  WS-INCOME-PART              PIC 9(11)V99.

       LINKAGE SECTION.
           COPY LNPPARM.
           COPY LOANREC.

       PROCEDURE DIVISION USING LNP-PARMS LOAN-RECORD.
       0000-MAIN.
           SET LNP-OK TO TRUE
           MOVE ZERO TO LNP-PRINCIPAL LNP-INTEREST LNP-PENALTY
                        LNP-CHARGE LNP-AMOUNT-DUE LNP-AVAILABLE
           IF LOAN-CLOSED OR LOAN-OUTSTANDING = ZERO
               SET LNP-LOAN-CLOSED TO TRUE
               GOBACK
           END-IF
           EVALUATE TRUE
               WHEN LNP-QUOTE-EMI OR LNP-PAY-EMI
                   PERFORM 1000-QUOTE-EMI
               WHEN LNP-QUOTE-FORECLOSE OR LNP-PAY-FORECLOSE
                   PERFORM 1100-QUOTE-FORECLOSE
               WHEN OTHER
                   SET LNP-BAD-MODE TO TRUE
                   GOBACK
           END-EVALUATE
           COMPUTE LNP-AMOUNT-DUE = LNP-PRINCIPAL + LNP-INTEREST
                                  + LNP-PENALTY + LNP-CHARGE
           IF LNP-IS-PAYMENT
               PERFORM 2000-COLLECT
               IF LNP-OK
                   PERFORM 3000-SPLIT-IN-GL
                   PERFORM 4000-UPDATE-LOAN
               END-IF
           END-IF
           GOBACK.

       1000-QUOTE-EMI.
           COMPUTE WS-MONTHLY-RATE = LOAN-RATE / 1200
           COMPUTE LNP-INTEREST ROUNDED =
                   LOAN-OUTSTANDING * WS-MONTHLY-RATE
           IF LOAN-EMI > LNP-INTEREST
               COMPUTE LNP-PRINCIPAL = LOAN-EMI - LNP-INTEREST
           ELSE
               MOVE ZERO TO LNP-PRINCIPAL
           END-IF
           IF LNP-PRINCIPAL > LOAN-OUTSTANDING
              OR LOAN-OUTSTANDING - LNP-PRINCIPAL < 1.00
               MOVE LOAN-OUTSTANDING TO LNP-PRINCIPAL
           END-IF
           MOVE LOAN-PENALTY-DUE TO LNP-PENALTY.

       1100-QUOTE-FORECLOSE.
           MOVE LOAN-OUTSTANDING TO LNP-PRINCIPAL
           MOVE LOAN-PENALTY-DUE TO LNP-PENALTY
           IF LOAN-HOME
               MOVE ZERO TO LNP-CHARGE
           ELSE
               COMPUTE LNP-CHARGE ROUNDED =
                       LOAN-OUTSTANDING * WS-FORECLOSE-PCT
           END-IF.

       2000-COLLECT.
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE LOAN-ACCT-NO   TO PP-ACCT-NO
           MOVE LNP-AMOUNT-DUE TO PP-AMOUNT
           MOVE 'LR'           TO PP-TRAN-TYPE
           MOVE LOAN-NO        TO PP-REF-ACCT
           MOVE LNP-NARRATION  TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           MOVE PP-AVAILABLE TO LNP-AVAILABLE
           EVALUATE TRUE
               WHEN PP-OK           CONTINUE
               WHEN PP-INSUFFICIENT SET LNP-INSUFFICIENT TO TRUE
               WHEN OTHER           SET LNP-ACCT-PROBLEM TO TRUE
           END-EVALUATE.

      *    CLEAR THE REPAYMENT SUSPENSE INTO ITS REAL HOMES
       3000-SPLIT-IN-GL.
           MOVE GLC-LOAN-REPAY-SUSP TO GP-DR-GL
           MOVE LOAN-NO             TO GP-REF
           IF LNP-PRINCIPAL > ZERO
               EVALUATE TRUE
                   WHEN LOAN-GOLD       MOVE GLC-GOLD-LOANS TO GP-CR-GL
                   WHEN LOAN-AGAINST-FD MOVE GLC-DEPOSIT-LOANS
                                                            TO GP-CR-GL
                   WHEN OTHER           MOVE GLC-LOANS      TO GP-CR-GL
               END-EVALUATE
               MOVE LNP-PRINCIPAL  TO GP-AMOUNT
               MOVE 'LOAN PRINCIPAL REPAID' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
           END-IF
           COMPUTE WS-INCOME-PART = LNP-INTEREST + LNP-PENALTY
           IF WS-INCOME-PART > ZERO
               MOVE GLC-LOAN-INT-INCOME TO GP-CR-GL
               MOVE WS-INCOME-PART      TO GP-AMOUNT
               MOVE 'LOAN INTEREST AND PENALTY' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
           END-IF
           IF LNP-CHARGE > ZERO
               MOVE GLC-FEE-INCOME TO GP-CR-GL
               MOVE LNP-CHARGE     TO GP-AMOUNT
               MOVE 'LOAN FORECLOSURE CHARGE' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
           END-IF.

       4000-UPDATE-LOAN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           SUBTRACT LNP-PRINCIPAL FROM LOAN-OUTSTANDING
           ADD LNP-INTEREST TO LOAN-TOTAL-INT-PAID
           MOVE ZERO TO LOAN-PENALTY-DUE
           MOVE WS-CURR-DATE TO LOAN-LAST-PAY-DATE
           IF LNP-PAY-FORECLOSE
               MOVE ZERO TO LOAN-OVERDUE-EMIS
           ELSE
               ADD 1 TO LOAN-EMIS-PAID
               IF LOAN-OVERDUE-EMIS > ZERO
                   SUBTRACT 1 FROM LOAN-OVERDUE-EMIS
               END-IF
               SET DU-ADD-MONTHS TO TRUE
               MOVE LOAN-NEXT-DUE-DATE TO DU-DATE-1
               MOVE 1 TO DU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DU-RESULT-DATE TO LOAN-NEXT-DUE-DATE
           END-IF
      *    AN NPA IS UPGRADED ONCE ALL ARREARS ARE CLEARED
           IF LOAN-NPA AND LOAN-OVERDUE-EMIS = ZERO
               SET LOAN-ACTIVE TO TRUE
           END-IF
           IF LOAN-OUTSTANDING = ZERO
               SET LOAN-CLOSED TO TRUE
               MOVE WS-CURR-DATE TO LOAN-CLOSE-DATE
           END-IF.
