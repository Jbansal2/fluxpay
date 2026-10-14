       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKMAIN.
      *================================================================*
      * PROGRAM  : BANKMAIN                                            *
      * PURPOSE  : MAIN MENU / DRIVER FOR THE CORE BANKING SYSTEM.     *
      *            SIGNS THE USER ON, THEN ROUTES TO EVERY MODULE.     *
      *            MENU OPTIONS ARE SHOWN AND ALLOWED BY ROLE:         *
      *              T (TELLER)  - DAY-TO-DAY CUSTOMER SERVICE         *
      *              M (MANAGER) - PLUS BATCHES AND MIS REPORTS        *
      *              A (ADMIN)   - PLUS USER ADMIN AND DATA UTILITIES  *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      * 2026-10-14 ADDED LOANS, CARDS, ATM, DEPOSITS, CIF/KYC, EOD     *
      * 2026-10-14 CREDIT CARDS, SECURED LOANS, CARD/GOLD BATCHES   *
      * 2026-10-14 SIGN-ON, ROLE-BASED MENU, CHEQUES, CHARGES, MIS,    *
      *            DATA UTILITIES, USER ADMIN                          *
      * 2026-10-14 PAYMENTS, SI, HOLDERS, LOCKERS, 360 SCREEN, GL,     *
      *            AML, TDS                                            *
      *================================================================*
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY SESSION.
           COPY AUDPARM.

       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
           88  CHOICE-ACCOUNTS                   VALUE '1'.
           88  CHOICE-POSTING                    VALUE '2'.
           88  CHOICE-INTEREST                   VALUE '3'.
           88  CHOICE-STATEMENT                  VALUE '4'.
           88  CHOICE-LOANS                      VALUE '5'.
           88  CHOICE-CARDS                      VALUE '6'.
           88  CHOICE-ATM                        VALUE '7'.
           88  CHOICE-DEPOSITS                   VALUE '8'.
           88  CHOICE-CUSTOMERS                  VALUE 'C'.
           88  CHOICE-CHEQUES                    VALUE 'Q'.
           88  CHOICE-EOD                        VALUE 'E'.
           88  CHOICE-CHARGES                    VALUE 'F'.
           88  CHOICE-REPORTS                    VALUE 'R'.
           88  CHOICE-DATA-UTIL                  VALUE 'D'.
           88  CHOICE-USER-ADMIN                 VALUE 'U'.
           88  CHOICE-PASSWORD                   VALUE 'P'.
           88  CHOICE-PAYMENTS                   VALUE 'Y'.
           88  CHOICE-SI                         VALUE 'I'.
           88  CHOICE-HOLDERS                    VALUE 'H'.
           88  CHOICE-LOCKERS                    VALUE 'L'.
           88  CHOICE-360-SCREEN                 VALUE 'V'.
           88  CHOICE-GL                         VALUE 'G'.
           88  CHOICE-AML                        VALUE 'A'.
           88  CHOICE-TDS                        VALUE 'T'.
           88  CHOICE-PAY-SETTLE                 VALUE 'B'.
           88  CHOICE-SI-RUN                     VALUE 'S'.
           88  CHOICE-CREDIT-CARDS               VALUE 'K'.
           88  CHOICE-SECURED-LOANS              VALUE 'O'.
           88  CHOICE-CC-BATCH                   VALUE 'X'.
           88  CHOICE-GOLD-REVAL                 VALUE 'J'.
           88  CHOICE-RATES                      VALUE 'N'.
           88  CHOICE-APPROVALS                  VALUE 'W'.
           88  CHOICE-CASH                       VALUE 'M'.
           88  CHOICE-NACH                       VALUE 'Z'.
           88  CHOICE-MORE                       VALUE '0'.
           88  CHOICE-EXIT                       VALUE '9'.
      *    OPTIONS THAT NEED A MANAGER (OR ADMIN)
           88  CHOICE-NEEDS-MANAGER              VALUES '3' 'E' 'F'
                                                        'R' 'G' 'A'
                                                        'T' 'B' 'S'
                                                        'X' 'J' 'N'
                                                        'W'.
      *    OPTIONS THAT NEED AN ADMIN
           88  CHOICE-NEEDS-ADMIN                VALUES 'D' 'U'.

       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.
       01  WS-SIGNON-RC                PIC 9(02).
       01  WS-USERMNT-MODE             PIC X(01).

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

           COPY MKRPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           CALL 'SIGNON' USING WS-SIGNON-RC
           IF WS-SIGNON-RC NOT = ZERO
               STOP RUN
           END-IF
           PERFORM UNTIL CHOICE-EXIT
               PERFORM 1000-SHOW-MENU
               PERFORM 2000-ROUTE-CHOICE
           END-PERFORM
           MOVE 'BANKMAIN' TO AU-PROGRAM
           MOVE 'LOGOUT'   TO AU-ACTION
           MOVE SES-USER-ID TO AU-KEY
           MOVE 'SIGNED OFF' TO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS
           DISPLAY 'SIGNED OFF. THANK YOU.'
           STOP RUN.

       1000-SHOW-MENU.
           DISPLAY ' '
           DISPLAY '=========================================='
           DISPLAY '        CORE BANKING SYSTEM - MAIN MENU    '
           DISPLAY '  USER: ' SES-USER-ID '  ROLE: ' SES-ROLE
                   '  BRANCH: ' SES-BRANCH
           DISPLAY '=========================================='
           DISPLAY ' 1. ACCOUNT MAINTENANCE'
           DISPLAY ' 2. TRANSACTION POSTING'
           DISPLAY ' 4. ACCOUNT STATEMENT'
           DISPLAY ' 5. LOANS'
           DISPLAY ' 6. DEBIT CARD SERVICES'
           DISPLAY ' 7. ATM (CUSTOMER SESSION)'
           DISPLAY ' 8. FIXED / RECURRING DEPOSITS'
           DISPLAY ' C. CUSTOMERS AND KYC'
           DISPLAY ' Q. CHEQUES AND CLEARING'
           DISPLAY ' Y. PAYMENTS (NEFT / RTGS / IMPS / UPI)'
           DISPLAY ' I. STANDING INSTRUCTIONS'
           DISPLAY ' H. JOINT HOLDERS AND NOMINEE'
           DISPLAY ' L. SAFE DEPOSIT LOCKERS'
           DISPLAY ' V. CUSTOMER 360 SCREEN'
           DISPLAY ' K. CREDIT CARDS'
           DISPLAY ' O. GOLD LOAN / LOAN AGAINST FD'
           DISPLAY ' M. BRANCH CASH (VAULT / DRAWER)'
           DISPLAY ' Z. NACH MANDATES'
           DISPLAY ' 0. MORE: FRAUD, TRADE FINANCE, FOREX, STATEMENTS'
           IF SES-CAN-APPROVE
               DISPLAY ' -- MANAGER --'
               DISPLAY ' 3. RUN MONTHLY INTEREST BATCH'
               DISPLAY ' E. RUN END-OF-DAY BATCH'
               DISPLAY ' F. RUN CHARGES BATCH'
               DISPLAY ' B. RUN PAYMENT SETTLEMENT'
               DISPLAY ' S. RUN STANDING INSTRUCTIONS'
               DISPLAY ' R. MIS REPORTS'
               DISPLAY ' G. GENERAL LEDGER REPORTS'
               DISPLAY ' A. AML REPORT (CTR / STR)'
               DISPLAY ' T. TDS CERTIFICATES'
               DISPLAY ' X. RUN CREDIT CARD BATCH'
               DISPLAY ' J. GOLD LOAN REVALUATION'
               DISPLAY ' N. INTEREST RATE MASTER'
               INITIALIZE MKR-PARMS
               SET MKR-COUNT TO TRUE
               CALL 'MKRQUEUE' USING MKR-PARMS
               DISPLAY ' W. APPROVALS (MAKER-CHECKER)  PENDING: '
                       MKR-PENDING
           END-IF
           IF SES-ADMIN
               DISPLAY ' -- ADMIN --'
               DISPLAY ' D. DATA UTILITIES'
               DISPLAY ' U. USER ADMINISTRATION'
           END-IF
           DISPLAY ' P. CHANGE MY PASSWORD'
           DISPLAY ' 9. SIGN OFF'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT
           MOVE FUNCTION UPPER-CASE(WS-CHOICE) TO WS-CHOICE.

       2000-ROUTE-CHOICE.
           IF (CHOICE-NEEDS-MANAGER AND NOT SES-CAN-APPROVE)
              OR (CHOICE-NEEDS-ADMIN AND NOT SES-ADMIN)
               DISPLAY 'YOU ARE NOT AUTHORISED FOR THIS OPTION.'
               MOVE 'BANKMAIN' TO AU-PROGRAM
               MOVE 'DENIED'   TO AU-ACTION
               MOVE WS-CHOICE  TO AU-KEY
               MOVE 'MENU OPTION NOT ALLOWED FOR ROLE' TO AU-DETAIL
               CALL 'AUDITLOG' USING AUD-PARMS
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN CHOICE-ACCOUNTS
                   CALL 'ACCTMNT'
               WHEN CHOICE-POSTING
                   CALL 'TRANPOST'
               WHEN CHOICE-INTEREST
                   CALL 'INTCALC' USING WS-RETURN-CODE
                   IF WS-RETURN-CODE NOT = ZERO
                       DISPLAY 'INTEREST BATCH ENDED WITH RC='
                               WS-RETURN-CODE
                   END-IF
               WHEN CHOICE-STATEMENT
                   CALL 'STMTRPT'
               WHEN CHOICE-LOANS
                   CALL 'LOANMNT'
               WHEN CHOICE-CARDS
                   CALL 'CARDMNT'
               WHEN CHOICE-ATM
                   CALL 'ATMTXN'
               WHEN CHOICE-DEPOSITS
                   CALL 'DEPMNT'
               WHEN CHOICE-CUSTOMERS
                   CALL 'CUSTMNT'
               WHEN CHOICE-CHEQUES
                   CALL 'CHQMNT'
               WHEN CHOICE-PAYMENTS
                   CALL 'PAYMNT'
               WHEN CHOICE-SI
                   CALL 'SIMNT'
               WHEN CHOICE-HOLDERS
                   CALL 'HLDMNT'
               WHEN CHOICE-LOCKERS
                   CALL 'LOCKMNT'
               WHEN CHOICE-360-SCREEN
                   CALL 'ACCTSCR'
               WHEN CHOICE-CREDIT-CARDS
                   CALL 'CCMNT'
               WHEN CHOICE-SECURED-LOANS
                   CALL 'SECLOAN'
               WHEN CHOICE-CC-BATCH
                   CALL 'CCBATCH'
               WHEN CHOICE-GOLD-REVAL
                   CALL 'GOLDREVL'
               WHEN CHOICE-MORE
                   CALL 'MOREMENU'
               WHEN CHOICE-NACH
                   CALL 'NACHMNT'
               WHEN CHOICE-CASH
                   CALL 'CASHMNT'
               WHEN CHOICE-APPROVALS
                   CALL 'APPRVMNT'
               WHEN CHOICE-RATES
                   CALL 'RATEMNT'
               WHEN CHOICE-GL
                   CALL 'GLRPT'
               WHEN CHOICE-AML
                   CALL 'AMLRPT'
               WHEN CHOICE-TDS
                   CALL 'TDSRPT'
               WHEN CHOICE-PAY-SETTLE
                   CALL 'PAYBATCH'
               WHEN CHOICE-SI-RUN
                   CALL 'SIBATCH'
               WHEN CHOICE-EOD
                   CALL 'EODBATCH'
               WHEN CHOICE-CHARGES
                   CALL 'CHGBATCH'
               WHEN CHOICE-REPORTS
                   CALL 'MISRPT'
               WHEN CHOICE-DATA-UTIL
                   CALL 'DATAUTIL'
               WHEN CHOICE-USER-ADMIN
                   MOVE 'A' TO WS-USERMNT-MODE
                   CALL 'USERMNT' USING WS-USERMNT-MODE
               WHEN CHOICE-PASSWORD
                   MOVE 'P' TO WS-USERMNT-MODE
                   CALL 'USERMNT' USING WS-USERMNT-MODE
               WHEN CHOICE-EXIT
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'INVALID CHOICE, TRY AGAIN.'
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
