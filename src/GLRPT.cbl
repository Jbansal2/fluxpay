       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLRPT.
      *================================================================*
      * PROGRAM  : GLRPT                                               *
      * PURPOSE  : GENERAL LEDGER REPORTS                              *
      *              1 - TRIAL BALANCE (DEBITS MUST EQUAL CREDITS)     *
      *              2 - PROFIT AND LOSS                               *
      *              3 - BALANCE SHEET                                 *
      *              4 - SUB-LEDGER RECONCILIATION: GL CONTROL         *
      *                  ACCOUNTS AGAINST THE DETAIL MASTERS           *
      *                  (SAVINGS, CURRENT, LOANS, TERM DEPOSITS)      *
      *              5 - SUSPENSE CHECK: 1290 AND 2900 MUST BE ZERO    *
      *            ALL FIVE ARE PRODUCED ON EVERY RUN, TO THE SCREEN   *
      *            AND data/GL_<DATE>.TXT.                             *
      * RETURN   : 0 = BALANCED AND RECONCILED, 4 = SUSPENSE NOT ZERO, *
      *            8 = OUT OF BALANCE OR SUB-LEDGER MISMATCH           *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GL-FILE ASSIGN TO 'data/GLMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GL-CODE
               FILE STATUS IS WS-GL-FS.
           SELECT ACCT-FILE ASSIGN TO 'data/ACCTMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-ACCT-FS.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS LOAN-NO
               FILE STATUS IS WS-LOAN-FS.
           SELECT DEP-FILE ASSIGN TO 'data/DEPMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS DEP-NO
               FILE STATUS IS WS-DEP-FS.
           SELECT CC-FILE ASSIGN TO 'data/CCMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CC-NO
               FILE STATUS IS WS-CC-FS.
           SELECT RPT-FILE ASSIGN TO WS-RPT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  GL-FILE.
           COPY GLREC.
       FD  ACCT-FILE.
           COPY ACCTREC.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  DEP-FILE.
           COPY DEPREC.
       FD  CC-FILE.
           COPY CCREC.
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(90).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY GLCODES.

       01  WS-GL-FS                    PIC X(02).
           88  GL-FS-OK                          VALUE '00'.
       01  WS-LOAN-FS                  PIC X(02).
           88  LOAN-FS-OK                        VALUE '00'.
       01  WS-DEP-FS                   PIC X(02).
           88  DEP-FS-OK                         VALUE '00'.
       01  WS-RPT-FS                   PIC X(02).
       01  WS-CC-FS                    PIC X(02).
       01  WS-RPT-FILENAME             PIC X(40).
       01  WS-RPT-WORK                 PIC X(90).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-RETURN-CODE              PIC 9(02) VALUE ZERO.

      * GL TOTALS
       01  WS-TB.
           05  WS-TB-DEBIT             PIC S9(15)V99.
           05  WS-TB-CREDIT            PIC S9(15)V99.
           05  WS-INCOME               PIC S9(15)V99.
           05  WS-EXPENSE              PIC S9(15)V99.
           05  WS-ASSETS               PIC S9(15)V99.
           05  WS-LIABILITIES          PIC S9(15)V99.
           05  WS-CAPITAL              PIC S9(15)V99.
           05  WS-PROFIT               PIC S9(15)V99.

      * GL BALANCES PICKED OUT FOR THE RECONCILIATION
       01  WS-GL-PICK.
           05  WS-GL-SAVINGS           PIC S9(15)V99.
           05  WS-GL-CURRENT           PIC S9(15)V99.
           05  WS-GL-LOANS             PIC S9(15)V99.
           05  WS-GL-GOLD              PIC S9(15)V99.
           05  WS-GL-FDLOAN            PIC S9(15)V99.
           05  WS-GL-CARDS             PIC S9(15)V99.
           05  WS-GL-TERM              PIC S9(15)V99.
           05  WS-GL-REPAY-SUSP        PIC S9(15)V99.
           05  WS-GL-XFER-SUSP         PIC S9(15)V99.

      * SUB-LEDGER TOTALS
       01  WS-SUB.
           05  WS-SUB-SAVINGS          PIC S9(15)V99.
           05  WS-SUB-CURRENT          PIC S9(15)V99.
           05  WS-SUB-LOANS            PIC S9(15)V99.
           05  WS-SUB-GOLD             PIC S9(15)V99.
           05  WS-SUB-FDLOAN           PIC S9(15)V99.
           05  WS-SUB-CARDS            PIC S9(15)V99.
           05  WS-SUB-TERM             PIC S9(15)V99.

       01  WS-SIDE-AMT                 PIC S9(15)V99.
       01  WS-DIFF                     PIC S9(15)V99.
       01  WS-E-AMT                    PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-.
       01  WS-E-AMT2                   PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-.
       01  WS-E-AMT3                   PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-.

       01  RPT-TB-LINE.
           05  RPT-TB-CODE             PIC 9(04).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  RPT-TB-NAME             PIC X(30).
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  RPT-TB-TYPE             PIC X(01).
           05  RPT-TB-DEBIT            PIC ZZZ,ZZZ,ZZZ,ZZ9.99
                                       BLANK WHEN ZERO.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  RPT-TB-CREDIT           PIC ZZZ,ZZZ,ZZZ,ZZ9.99
                                       BLANK WHEN ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE ZERO TO WS-RETURN-CODE
           INITIALIZE WS-TB WS-GL-PICK WS-SUB
           STRING 'data/GL_' WS-CURR-DATE '.TXT'
                  DELIMITED BY SIZE INTO WS-RPT-FILENAME
           END-STRING
           OPEN OUTPUT RPT-FILE
           OPEN INPUT GL-FILE
           IF NOT GL-FS-OK
               MOVE 'NO GENERAL LEDGER YET - NOTHING HAS BEEN POSTED.'
                                                 TO WS-RPT-WORK
               PERFORM 8000-EMIT
               CLOSE RPT-FILE
               MOVE ZERO TO RETURN-CODE
               GOBACK
           END-IF
           PERFORM 1000-TRIAL-BALANCE
           CLOSE GL-FILE
           PERFORM 2000-PROFIT-AND-LOSS
           PERFORM 3000-BALANCE-SHEET
           PERFORM 4000-SUBLEDGER-RECON
           PERFORM 5000-SUSPENSE-CHECK
           MOVE SPACES TO WS-RPT-WORK
           PERFORM 8000-EMIT
           STRING 'GL REPORT COMPLETE. RETURN CODE ' WS-RETURN-CODE
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT
           CLOSE RPT-FILE
           DISPLAY 'REPORT SAVED TO ' WS-RPT-FILENAME
           MOVE WS-RETURN-CODE TO RETURN-CODE
           GOBACK.

      *================================================================*
      * 1 - TRIAL BALANCE                                              *
      *================================================================*
       1000-TRIAL-BALANCE.
           MOVE ALL '=' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           STRING 'TRIAL BALANCE AS ON ' WS-CURR-DATE
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT
           MOVE ALL '=' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE 'GL    NAME                           T'
                                                 TO WS-RPT-WORK
           MOVE 'DEBIT' TO WS-RPT-WORK(51:)
           MOVE 'CREDIT' TO WS-RPT-WORK(70:)
           PERFORM 8000-EMIT
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ GL-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END PERFORM 1100-TB-LINE
               END-READ
           END-PERFORM
           MOVE ALL '-' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE ZERO   TO RPT-TB-CODE
           MOVE 'TOTAL' TO RPT-TB-NAME
           MOVE SPACE  TO RPT-TB-TYPE
           MOVE WS-TB-DEBIT  TO RPT-TB-DEBIT
           MOVE WS-TB-CREDIT TO RPT-TB-CREDIT
           MOVE RPT-TB-LINE TO WS-RPT-WORK
           MOVE SPACES TO WS-RPT-WORK(1:4)
           PERFORM 8000-EMIT
           IF WS-TB-DEBIT = WS-TB-CREDIT
               MOVE 'TRIAL BALANCE AGREES.' TO WS-RPT-WORK
           ELSE
               COMPUTE WS-DIFF = WS-TB-DEBIT - WS-TB-CREDIT
               MOVE WS-DIFF TO WS-E-AMT
               STRING '*** OUT OF BALANCE BY ' FUNCTION TRIM(WS-E-AMT)
                      DELIMITED BY SIZE INTO WS-RPT-WORK
               END-STRING
               MOVE 8 TO WS-RETURN-CODE
           END-IF
           PERFORM 8000-EMIT.

       1100-TB-LINE.
           MOVE GL-CODE TO RPT-TB-CODE
           MOVE GL-NAME TO RPT-TB-NAME
           MOVE GL-TYPE TO RPT-TB-TYPE
           IF GL-BALANCE >= ZERO
               MOVE GL-BALANCE TO RPT-TB-DEBIT
               MOVE ZERO       TO RPT-TB-CREDIT
               ADD GL-BALANCE  TO WS-TB-DEBIT
           ELSE
               MOVE ZERO       TO RPT-TB-DEBIT
               COMPUTE RPT-TB-CREDIT = 0 - GL-BALANCE
               SUBTRACT GL-BALANCE FROM WS-TB-CREDIT
           END-IF
           EVALUATE TRUE
               WHEN GL-ASSET     ADD GL-BALANCE TO WS-ASSETS
               WHEN GL-LIABILITY SUBTRACT GL-BALANCE FROM WS-LIABILITIES
               WHEN GL-CAPITAL   SUBTRACT GL-BALANCE FROM WS-CAPITAL
               WHEN GL-INCOME    SUBTRACT GL-BALANCE FROM WS-INCOME
               WHEN GL-EXPENSE   ADD GL-BALANCE TO WS-EXPENSE
           END-EVALUATE
           EVALUATE GL-CODE
               WHEN GLC-SAVINGS-DEP
                   COMPUTE WS-GL-SAVINGS = 0 - GL-BALANCE
               WHEN GLC-CURRENT-DEP
                   COMPUTE WS-GL-CURRENT = 0 - GL-BALANCE
               WHEN GLC-TERM-DEP
                   COMPUTE WS-GL-TERM = 0 - GL-BALANCE
               WHEN GLC-LOANS
                   MOVE GL-BALANCE TO WS-GL-LOANS
               WHEN GLC-GOLD-LOANS
                   MOVE GL-BALANCE TO WS-GL-GOLD
               WHEN GLC-DEPOSIT-LOANS
                   MOVE GL-BALANCE TO WS-GL-FDLOAN
               WHEN GLC-CARD-RECEIVABLE
                   MOVE GL-BALANCE TO WS-GL-CARDS
               WHEN GLC-LOAN-REPAY-SUSP
                   MOVE GL-BALANCE TO WS-GL-REPAY-SUSP
               WHEN GLC-TRANSFER-SUSP
                   MOVE GL-BALANCE TO WS-GL-XFER-SUSP
           END-EVALUATE
           IF GL-BALANCE NOT = ZERO
               MOVE RPT-TB-LINE TO WS-RPT-WORK
               PERFORM 8000-EMIT
           END-IF.

      *================================================================*
      * 2 - PROFIT AND LOSS                                            *
      *================================================================*
       2000-PROFIT-AND-LOSS.
           COMPUTE WS-PROFIT = WS-INCOME - WS-EXPENSE
           MOVE SPACES TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE 'PROFIT AND LOSS (SINCE INCEPTION)' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-INCOME TO WS-E-AMT
           STRING '  TOTAL INCOME        ' WS-E-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-EXPENSE TO WS-E-AMT
           STRING '  TOTAL EXPENSES      ' WS-E-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-PROFIT TO WS-E-AMT
           STRING '  NET PROFIT / (LOSS) ' WS-E-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT.

      *================================================================*
      * 3 - BALANCE SHEET: ASSETS = LIABILITIES + CAPITAL + PROFIT     *
      *================================================================*
       3000-BALANCE-SHEET.
           MOVE SPACES TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE 'BALANCE SHEET' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-ASSETS TO WS-E-AMT
           STRING '  TOTAL ASSETS              ' WS-E-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-LIABILITIES TO WS-E-AMT
           STRING '  LIABILITIES               ' WS-E-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-CAPITAL TO WS-E-AMT
           STRING '  CAPITAL / OPENING CONTROL ' WS-E-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-PROFIT TO WS-E-AMT
           STRING '  RETAINED PROFIT           ' WS-E-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           COMPUTE WS-SIDE-AMT = WS-LIABILITIES + WS-CAPITAL
                               + WS-PROFIT
           MOVE WS-SIDE-AMT TO WS-E-AMT
           STRING '  LIABILITIES + EQUITY      ' WS-E-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           IF WS-SIDE-AMT = WS-ASSETS
               MOVE '  BALANCE SHEET BALANCES.' TO WS-RPT-WORK
           ELSE
               MOVE '  *** BALANCE SHEET DOES NOT BALANCE'
                                                 TO WS-RPT-WORK
               MOVE 8 TO WS-RETURN-CODE
           END-IF
           PERFORM 8000-EMIT.

      *================================================================*
      * 4 - SUB-LEDGER RECONCILIATION                                  *
      *================================================================*
       4000-SUBLEDGER-RECON.
           MOVE SPACES TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE 'SUB-LEDGER RECONCILIATION'  TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE 'CONTROL               GL BALANCE' TO WS-RPT-WORK
           MOVE 'DETAIL MASTER' TO WS-RPT-WORK(44:)
           MOVE 'DIFFERENCE' TO WS-RPT-WORK(70:)
           PERFORM 8000-EMIT

           OPEN INPUT ACCT-FILE
           IF ACCT-FS-OK
               MOVE 'N' TO WS-EOF-SW
               PERFORM UNTIL END-OF-FILE
                   READ ACCT-FILE NEXT RECORD
                       AT END SET END-OF-FILE TO TRUE
                       NOT AT END
                           IF ACCT-SAVINGS
                               ADD ACCT-BALANCE TO WS-SUB-SAVINGS
                           ELSE
                               ADD ACCT-BALANCE TO WS-SUB-CURRENT
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE ACCT-FILE
           END-IF
           OPEN INPUT LOAN-FILE
           IF LOAN-FS-OK
               MOVE 'N' TO WS-EOF-SW
               PERFORM UNTIL END-OF-FILE
                   READ LOAN-FILE NEXT RECORD
                       AT END SET END-OF-FILE TO TRUE
                       NOT AT END
      *                    A WRITTEN-OFF LOAN HAS LEFT THE BOOKS; ITS
      *                    OUTSTANDING IS KEPT ONLY FOR RECOVERY
                           EVALUATE TRUE
                               WHEN LOAN-WRITTEN-OFF
                                   CONTINUE
                               WHEN LOAN-GOLD
                                   ADD LOAN-OUTSTANDING TO WS-SUB-GOLD
                               WHEN LOAN-AGAINST-FD
                                   ADD LOAN-OUTSTANDING TO WS-SUB-FDLOAN
                               WHEN OTHER
                                   ADD LOAN-OUTSTANDING TO WS-SUB-LOANS
                           END-EVALUATE
                   END-READ
               END-PERFORM
               CLOSE LOAN-FILE
           END-IF
           OPEN INPUT DEP-FILE
           IF DEP-FS-OK
               MOVE 'N' TO WS-EOF-SW
               PERFORM UNTIL END-OF-FILE
                   READ DEP-FILE NEXT RECORD
                       AT END SET END-OF-FILE TO TRUE
                       NOT AT END
                           IF DEP-ACTIVE
                               ADD DEP-TOTAL-DEPOSITED TO WS-SUB-TERM
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE DEP-FILE
           END-IF

           MOVE 'SAVINGS DEPOSITS (2101)' TO WS-RPT-WORK
           MOVE WS-GL-SAVINGS TO WS-SIDE-AMT
           MOVE WS-SUB-SAVINGS TO WS-DIFF
           PERFORM 4100-RECON-LINE
           MOVE 'CURRENT DEPOSITS (2102)' TO WS-RPT-WORK
           MOVE WS-GL-CURRENT TO WS-SIDE-AMT
           MOVE WS-SUB-CURRENT TO WS-DIFF
           PERFORM 4100-RECON-LINE
           MOVE 'LOANS (1201)' TO WS-RPT-WORK
           MOVE WS-GL-LOANS TO WS-SIDE-AMT
           MOVE WS-SUB-LOANS TO WS-DIFF
           PERFORM 4100-RECON-LINE
           OPEN INPUT CC-FILE
           IF WS-CC-FS = '00'
               MOVE 'N' TO WS-EOF-SW
               PERFORM UNTIL END-OF-FILE
                   READ CC-FILE NEXT RECORD
                       AT END SET END-OF-FILE TO TRUE
                       NOT AT END
                           ADD CC-OUTSTANDING TO WS-SUB-CARDS
                   END-READ
               END-PERFORM
               CLOSE CC-FILE
           END-IF
           MOVE 'GOLD LOANS (1220)' TO WS-RPT-WORK
           MOVE WS-GL-GOLD TO WS-SIDE-AMT
           MOVE WS-SUB-GOLD TO WS-DIFF
           PERFORM 4100-RECON-LINE
           MOVE 'LOANS AGAINST FD (1230)' TO WS-RPT-WORK
           MOVE WS-GL-FDLOAN TO WS-SIDE-AMT
           MOVE WS-SUB-FDLOAN TO WS-DIFF
           PERFORM 4100-RECON-LINE
           MOVE 'CREDIT CARDS (1210)' TO WS-RPT-WORK
           MOVE WS-GL-CARDS TO WS-SIDE-AMT
           MOVE WS-SUB-CARDS TO WS-DIFF
           PERFORM 4100-RECON-LINE
           MOVE 'TERM DEPOSITS (2201)' TO WS-RPT-WORK
           MOVE WS-GL-TERM TO WS-SIDE-AMT
           MOVE WS-SUB-TERM TO WS-DIFF
           PERFORM 4100-RECON-LINE.

      *    IN: WS-SIDE-AMT = GL, WS-DIFF = DETAIL. LABEL ALREADY SET.
       4100-RECON-LINE.
           MOVE WS-SIDE-AMT TO WS-E-AMT
           MOVE WS-DIFF     TO WS-E-AMT2
           COMPUTE WS-DIFF = WS-SIDE-AMT - WS-DIFF
           MOVE WS-DIFF     TO WS-E-AMT3
           MOVE WS-E-AMT  TO WS-RPT-WORK(24:22)
           MOVE WS-E-AMT2 TO WS-RPT-WORK(46:22)
           MOVE WS-E-AMT3 TO WS-RPT-WORK(68:22)
           IF WS-DIFF NOT = ZERO
               MOVE 8 TO WS-RETURN-CODE
               MOVE '*' TO WS-RPT-WORK(90:1)
           END-IF
           PERFORM 8000-EMIT.

      *================================================================*
      * 5 - SUSPENSE ACCOUNTS MUST CLEAR TO ZERO                       *
      *================================================================*
       5000-SUSPENSE-CHECK.
           MOVE SPACES TO WS-RPT-WORK
           PERFORM 8000-EMIT
           IF WS-GL-REPAY-SUSP = ZERO AND WS-GL-XFER-SUSP = ZERO
               MOVE 'SUSPENSE ACCOUNTS 1290 AND 2900 ARE CLEAR.'
                                                 TO WS-RPT-WORK
           ELSE
               MOVE WS-GL-REPAY-SUSP TO WS-E-AMT
               MOVE WS-GL-XFER-SUSP  TO WS-E-AMT2
               STRING '*** SUSPENSE NOT CLEAR: 1290 '
                      FUNCTION TRIM(WS-E-AMT) '  2900 '
                      FUNCTION TRIM(WS-E-AMT2)
                      DELIMITED BY SIZE INTO WS-RPT-WORK
               END-STRING
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF
           PERFORM 8000-EMIT.

       8000-EMIT.
           MOVE WS-RPT-WORK TO RPT-LINE
           WRITE RPT-LINE
           DISPLAY FUNCTION TRIM(WS-RPT-WORK TRAILING)
           MOVE SPACES TO WS-RPT-WORK.
