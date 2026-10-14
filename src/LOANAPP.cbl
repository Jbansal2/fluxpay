       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOANAPP.
      *================================================================*
      * PROGRAM  : LOANAPP                                             *
      * PURPOSE  : LOAN ORIGINATION - APPLICATION TO DISBURSAL         *
      *              1 - NEW APPLICATION (ASSESSED AT ONCE)            *
      *              2 - APPLICATIONS UNDER REVIEW                     *
      *              3 - APPROVE / REJECT                              *
      *              4 - DISBURSE AN APPROVED APPLICATION              *
      *              5 - APPLICATION STATUS                            *
      *              6 - WITHDRAW AN APPLICATION                       *
      *            ASSESSMENT (AUTOMATIC):                             *
      *              HARD RULES - REJECTED STRAIGHT AWAY:              *
      *                ACCOUNT HOLDER NOT KYC-VERIFIED                 *
      *                BUREAU SCORE BELOW 650                          *
      *                FOIR ABOVE 50% (55% FOR HOME LOANS), WHERE      *
      *                FOIR = (EXISTING EMIS + NEW EMI) / INCOME       *
      *                AMOUNT / TENURE OUTSIDE THE PRODUCT LIMITS      *
      *              SCORE (0-100): BUREAU UP TO 50, FOIR UP TO 30,    *
      *                INCOME COVER UP TO 20 -> GRADE A (75+), B (60+),*
      *                C (BELOW 60, NEEDS THE CREDIT COMMITTEE)        *
      *            APPROVAL LEVEL: UP TO 5 LAKH AND GRADE A/B - BRANCH *
      *            MANAGER; ABOVE 5 LAKH OR GRADE C - CREDIT COMMITTEE *
      *            (ADMIN ROLE). THE PERSON WHO ENTERED AN APPLICATION *
      *            CANNOT DECIDE IT. A SANCTION IS VALID FOR 30 DAYS.  *
      *            DISBURSAL GOES THROUGH LNDISB, LIKE A DIRECT LOAN.  *
      * FILES    : LOANAPP (I-O)                                       *
      * CALLS    : ACCTPOST (INQUIRY), CUSTLKUP, RATEUTIL, LNDISB,     *
      *            DATEUTIL, AUDITLOG                                  *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT APP-FILE ASSIGN TO 'data/LOANAPP.DAT'
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS LA-APP-NO FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  APP-FILE.
           COPY LAPPREC.

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-CHOICE                   PIC X(01).
       01  WS-EXIT-SW                  PIC X(01) VALUE 'N'.
           88  EXIT-REQUESTED                    VALUE 'Y'.
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-TODAY                    PIC 9(08).
       01  WS-IN.
           05  WS-IN-ACCT-NO           PIC 9(10).
           05  WS-IN-TYPE              PIC X(01).
           05  WS-IN-AMOUNT            PIC 9(11)V99.
           05  WS-IN-TENURE            PIC 9(03).
           05  WS-IN-INCOME            PIC 9(09)V99.
           05  WS-IN-EXISTING          PIC 9(09)V99.
           05  WS-IN-BUREAU            PIC 9(03).
           05  WS-IN-APP-NO            PIC 9(08).
           05  WS-IN-DECISION          PIC X(01).
           05  WS-IN-REASON            PIC X(40).
       01  WS-CONFIRM                  PIC X(01).
       01  WS-NEXT-APP                 PIC 9(08).
       01  WS-COUNT                    PIC 9(04).
       01  WS-SPREAD                   PIC 9(02)V9(04).
       01  WS-FOIR-LIMIT               PIC 9(03)V99.
       01  WS-COVER                    PIC 9(05)V99.
       01  WS-BRANCH-LIMIT             PIC 9(11)V99 VALUE 500000.
      * EMI ARITHMETIC
       01  WS-R                        COMP-2.
       01  WS-POWER                    COMP-2.
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-EMI                      PIC ZZZ,ZZ9.99.
       01  ED-PCT                      PIC ZZ9.99.
       01  ED-RATE                     PIC Z9.99.
       01  WS-STATUS-NAME              PIC X(12).
           COPY LNPROD.
           COPY POSTPARM.
           COPY CLKPARM.
           COPY RATEPARM.
           COPY LNDPARM.
           COPY DATEPARM.
           COPY SESSION.
           COPY AUDPARM.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           OPEN I-O APP-FILE
           IF WS-FS = '35'
               OPEN OUTPUT APP-FILE
               CLOSE APP-FILE
               OPEN I-O APP-FILE
           END-IF
           IF WS-FS NOT = '00'
               DISPLAY 'APPLICATION FILE OPEN FAILED, FS=' WS-FS
               GOBACK
           END-IF
           MOVE 'N' TO WS-EXIT-SW
           PERFORM UNTIL EXIT-REQUESTED
               DISPLAY ' '
               DISPLAY '----------- LOAN APPLICATIONS -----------'
               DISPLAY ' 1. NEW APPLICATION'
               DISPLAY ' 2. APPLICATIONS UNDER REVIEW'
               IF SES-CAN-APPROVE
                   DISPLAY ' 3. APPROVE / REJECT'
               END-IF
               DISPLAY ' 4. DISBURSE AN APPROVED APPLICATION'
               DISPLAY ' 5. APPLICATION STATUS'
               DISPLAY ' 6. WITHDRAW AN APPLICATION'
               DISPLAY ' 9. BACK'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               PERFORM 8990-GUARD-END-OF-INPUT
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 1000-NEW-APPLICATION
                   WHEN '2' PERFORM 2000-REVIEW-LIST
                   WHEN '3' PERFORM 3000-DECIDE
                   WHEN '4' PERFORM 4000-DISBURSE
                   WHEN '5' PERFORM 5000-STATUS
                   WHEN '6' PERFORM 6000-WITHDRAW
                   WHEN '9' SET EXIT-REQUESTED TO TRUE
                   WHEN SPACE CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE APP-FILE
           MOVE 'Y' TO CLK-CLOSE
           CALL 'CUSTLKUP' USING CLK-PARMS
           GOBACK.

      *================================================================*
      * 1 - NEW APPLICATION                                            *
      *================================================================*
       1000-NEW-APPLICATION.
           DISPLAY 'ACCOUNT NO            : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'LOAN TYPE (H/P/V)     : ' WITH NO ADVANCING
           ACCEPT WS-IN-TYPE
           MOVE FUNCTION UPPER-CASE(WS-IN-TYPE) TO WS-IN-TYPE
           DISPLAY 'AMOUNT                : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           DISPLAY 'TENURE (MONTHS)       : ' WITH NO ADVANCING
           ACCEPT WS-IN-TENURE
           DISPLAY 'NET MONTHLY INCOME    : ' WITH NO ADVANCING
           ACCEPT WS-IN-INCOME
           DISPLAY 'EXISTING EMIS / MONTH : ' WITH NO ADVANCING
           ACCEPT WS-IN-EXISTING
           DISPLAY 'BUREAU SCORE (300-900): ' WITH NO ADVANCING
           ACCEPT WS-IN-BUREAU

           SET PRD-IDX TO 1
           SEARCH WS-PRODUCT
               AT END
                   DISPLAY 'LOAN TYPE MUST BE H, P OR V.'
                   EXIT PARAGRAPH
               WHEN PRD-TYPE(PRD-IDX) = WS-IN-TYPE
                   CONTINUE
           END-SEARCH
           IF WS-IN-INCOME NOT > ZERO
              OR WS-IN-BUREAU < 300 OR WS-IN-BUREAU > 900
               DISPLAY 'INCOME AND A BUREAU SCORE (300-900) ARE NEEDED.'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'ACCOUNT NOT FOUND OR NOT ACTIVE.'
               EXIT PARAGRAPH
           END-IF

           PERFORM 1100-NEXT-APP-NO
           INITIALIZE LAPP-RECORD
           MOVE WS-NEXT-APP     TO LA-APP-NO
           MOVE WS-IN-ACCT-NO   TO LA-ACCT-NO
           MOVE WS-IN-TYPE      TO LA-TYPE
           MOVE WS-IN-AMOUNT    TO LA-AMOUNT
           MOVE WS-IN-TENURE    TO LA-TENURE
           MOVE WS-IN-INCOME    TO LA-MONTHLY-INCOME
           MOVE WS-IN-EXISTING  TO LA-EXISTING-EMI
           MOVE WS-IN-BUREAU    TO LA-BUREAU-SCORE
           MOVE SES-USER-ID     TO LA-ENTERED-BY
           MOVE WS-TODAY        TO LA-ENTRY-DATE
           SET LA-UNDER-REVIEW  TO TRUE
           PERFORM 1200-PRICE
           PERFORM 1300-ASSESS
           WRITE LAPP-RECORD
               INVALID KEY
                   DISPLAY 'APPLICATION WRITE FAILED, FS=' WS-FS
                   EXIT PARAGRAPH
           END-WRITE
           PERFORM 1400-SHOW-ASSESSMENT
           MOVE 'APPLY' TO AU-ACTION
           PERFORM 9000-AUDIT.

       1100-NEXT-APP-NO.
           MOVE ZERO TO WS-NEXT-APP
           MOVE 99999999 TO LA-APP-NO
           START APP-FILE KEY IS LESS THAN OR EQUAL LA-APP-NO
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ APP-FILE PREVIOUS RECORD
                       NOT AT END MOVE LA-APP-NO TO WS-NEXT-APP
                   END-READ
           END-START
           ADD 1 TO WS-NEXT-APP.

      *    RATE: PRODUCT RATE, OR REPO + SPREAD FOR HOME LOANS
       1200-PRICE.
           MOVE PRD-RATE(PRD-IDX) TO LA-RATE
           IF LA-TYPE = 'H'
               MOVE 'HLSPREAD' TO RTP-CODE
               CALL 'RATEUTIL' USING RATE-PARMS
               MOVE RTP-VALUE TO WS-SPREAD
               MOVE 'REPO' TO RTP-CODE
               CALL 'RATEUTIL' USING RATE-PARMS
               COMPUTE LA-RATE = RTP-VALUE + WS-SPREAD
           END-IF
           MOVE ZERO TO LA-EMI
           IF LA-TENURE > ZERO
               COMPUTE WS-R = LA-RATE / 1200
               COMPUTE WS-POWER = (1 + WS-R) ** LA-TENURE
               COMPUTE LA-EMI ROUNDED =
                       LA-AMOUNT * WS-R * WS-POWER / (WS-POWER - 1)
           END-IF.

       1300-ASSESS.
           MOVE LA-ACCT-NO TO CLK-ACCT-NO
           MOVE 'N' TO CLK-CLOSE
           CALL 'CUSTLKUP' USING CLK-PARMS
           MOVE CLK-CUST-ID TO LA-CUST-ID
           COMPUTE LA-FOIR-PCT ROUNDED =
                   (LA-EXISTING-EMI + LA-EMI) * 100 / LA-MONTHLY-INCOME
               ON SIZE ERROR MOVE 999.99 TO LA-FOIR-PCT
           END-COMPUTE
           MOVE 50 TO WS-FOIR-LIMIT
           IF LA-TYPE = 'H'
               MOVE 55 TO WS-FOIR-LIMIT
           END-IF
      *    HARD RULES, IN THE ORDER A CREDIT OFFICER WOULD CHECK THEM
           EVALUATE TRUE
               WHEN NOT CLK-OK OR NOT CLK-KYC-VERIFIED
                   MOVE 'ACCOUNT HOLDER NOT KYC-VERIFIED' TO LA-REASON
               WHEN LA-AMOUNT < 10000
                    OR LA-AMOUNT > PRD-MAX-AMOUNT(PRD-IDX)
                   MOVE 'AMOUNT OUTSIDE PRODUCT LIMITS' TO LA-REASON
               WHEN LA-TENURE < 6 OR LA-TENURE > PRD-MAX-TENURE(PRD-IDX)
                   MOVE 'TENURE OUTSIDE PRODUCT LIMITS' TO LA-REASON
               WHEN LA-BUREAU-SCORE < 650
                   MOVE 'BUREAU SCORE BELOW 650' TO LA-REASON
               WHEN LA-FOIR-PCT > WS-FOIR-LIMIT
                   MOVE 'REPAYMENT CAPACITY: FOIR ABOVE LIMIT'
                       TO LA-REASON
           END-EVALUATE
           IF LA-REASON NOT = SPACES
               SET LA-REJECTED TO TRUE
               MOVE 'SYSTEM'   TO LA-DECIDED-BY
               MOVE WS-TODAY   TO LA-DECISION-DATE
               EXIT PARAGRAPH
           END-IF
      *    SCORE: BUREAU 50, FOIR 30, INCOME COVER (INCOME / EMI) 20
           MOVE ZERO TO LA-SCORE
           EVALUATE TRUE
               WHEN LA-BUREAU-SCORE >= 800 ADD 50 TO LA-SCORE
               WHEN LA-BUREAU-SCORE >= 750 ADD 40 TO LA-SCORE
               WHEN LA-BUREAU-SCORE >= 700 ADD 30 TO LA-SCORE
               WHEN OTHER                  ADD 15 TO LA-SCORE
           END-EVALUATE
           EVALUATE TRUE
               WHEN LA-FOIR-PCT <= 30 ADD 30 TO LA-SCORE
               WHEN LA-FOIR-PCT <= 40 ADD 20 TO LA-SCORE
               WHEN OTHER             ADD 10 TO LA-SCORE
           END-EVALUATE
           COMPUTE WS-COVER = LA-MONTHLY-INCOME / LA-EMI
           EVALUATE TRUE
               WHEN WS-COVER >= 5 ADD 20 TO LA-SCORE
               WHEN WS-COVER >= 3 ADD 12 TO LA-SCORE
               WHEN OTHER         ADD 5  TO LA-SCORE
           END-EVALUATE
           EVALUATE TRUE
               WHEN LA-SCORE >= 75 MOVE 'A' TO LA-GRADE
               WHEN LA-SCORE >= 60 MOVE 'B' TO LA-GRADE
               WHEN OTHER          MOVE 'C' TO LA-GRADE
           END-EVALUATE
           IF LA-AMOUNT > WS-BRANCH-LIMIT OR LA-GRADE = 'C'
               SET LA-LEVEL-CREDIT TO TRUE
           ELSE
               SET LA-LEVEL-BRANCH TO TRUE
           END-IF.

       1400-SHOW-ASSESSMENT.
           MOVE LA-EMI      TO ED-EMI
           MOVE LA-RATE     TO ED-RATE
           MOVE LA-FOIR-PCT TO ED-PCT
           DISPLAY 'APPLICATION ' LA-APP-NO '  RATE ' ED-RATE
                   '%  EMI ' ED-EMI '  FOIR ' ED-PCT '%'
           IF LA-REJECTED
               DISPLAY 'REJECTED BY RULE: ' LA-REASON
           ELSE
               DISPLAY 'SCORE ' LA-SCORE ' GRADE ' LA-GRADE
               IF LA-LEVEL-CREDIT
                   DISPLAY 'NEEDS CREDIT COMMITTEE APPROVAL.'
               ELSE
                   DISPLAY 'NEEDS BRANCH MANAGER APPROVAL.'
               END-IF
           END-IF.

      *================================================================*
      * 2 - REVIEW LIST                                                *
      *================================================================*
       2000-REVIEW-LIST.
           MOVE ZERO TO WS-COUNT
           PERFORM 8000-START-BROWSE
           PERFORM UNTIL END-OF-FILE
               READ APP-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF LA-UNDER-REVIEW OR LA-APPROVED
                           ADD 1 TO WS-COUNT
                           PERFORM 8100-STATUS-NAME
                           MOVE LA-AMOUNT TO ED-AMT
                           DISPLAY LA-APP-NO ' ' LA-ACCT-NO ' '
                                   LA-TYPE ' ' ED-AMT ' GRADE '
                                   LA-GRADE ' LEVEL ' LA-LEVEL ' '
                                   WS-STATUS-NAME
                       END-IF
               END-READ
           END-PERFORM
           DISPLAY WS-COUNT ' OPEN APPLICATION(S)'.

      *================================================================*
      * 3 - DECIDE                                                     *
      *================================================================*
       3000-DECIDE.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'NEEDS A MANAGER.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 7000-READ-APP
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT LA-UNDER-REVIEW
               PERFORM 8100-STATUS-NAME
               DISPLAY 'APPLICATION IS ' WS-STATUS-NAME
               EXIT PARAGRAPH
           END-IF
           IF LA-ENTERED-BY = SES-USER-ID
               DISPLAY 'YOU ENTERED THIS APPLICATION - ANOTHER OFFICER '
                       'MUST DECIDE IT.'
               EXIT PARAGRAPH
           END-IF
           IF LA-LEVEL-CREDIT AND NOT SES-ADMIN
               DISPLAY 'ABOVE BRANCH POWERS - CREDIT COMMITTEE ONLY.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 1400-SHOW-ASSESSMENT
           DISPLAY 'A=APPROVE R=REJECT : ' WITH NO ADVANCING
           ACCEPT WS-IN-DECISION
           MOVE FUNCTION UPPER-CASE(WS-IN-DECISION) TO WS-IN-DECISION
           EVALUATE WS-IN-DECISION
               WHEN 'A'
                   SET LA-APPROVED TO TRUE
                   SET DU-ADD-DAYS TO TRUE
                   MOVE WS-TODAY TO DU-DATE-1
                   MOVE 30 TO DU-NUMBER
                   CALL 'DATEUTIL' USING DATE-PARMS
                   MOVE DU-RESULT-DATE TO LA-SANCTION-VALID
                   MOVE 'SANCTIONED' TO LA-REASON
                   DISPLAY 'SANCTIONED. VALID TILL ' LA-SANCTION-VALID
               WHEN 'R'
                   DISPLAY 'REASON : ' WITH NO ADVANCING
                   ACCEPT WS-IN-REASON
                   IF WS-IN-REASON = SPACES
                       DISPLAY 'A REASON IS REQUIRED.'
                       EXIT PARAGRAPH
                   END-IF
                   SET LA-REJECTED TO TRUE
                   MOVE WS-IN-REASON TO LA-REASON
                   DISPLAY 'APPLICATION REJECTED.'
               WHEN OTHER
                   DISPLAY 'NO DECISION RECORDED.'
                   EXIT PARAGRAPH
           END-EVALUATE
           MOVE SES-USER-ID TO LA-DECIDED-BY
           MOVE WS-TODAY    TO LA-DECISION-DATE
           REWRITE LAPP-RECORD
           MOVE 'DECIDE' TO AU-ACTION
           PERFORM 9000-AUDIT.

      *================================================================*
      * 4 - DISBURSE                                                   *
      *================================================================*
       4000-DISBURSE.
           PERFORM 7000-READ-APP
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT LA-APPROVED
               PERFORM 8100-STATUS-NAME
               DISPLAY 'ONLY AN APPROVED APPLICATION CAN BE DISBURSED ('
                       FUNCTION TRIM(WS-STATUS-NAME) ').'
               EXIT PARAGRAPH
           END-IF
           IF WS-TODAY > LA-SANCTION-VALID
               DISPLAY 'SANCTION EXPIRED ON ' LA-SANCTION-VALID
                       ' - A FRESH APPLICATION IS NEEDED.'
               EXIT PARAGRAPH
           END-IF
           MOVE LA-AMOUNT TO ED-AMT
           DISPLAY 'DISBURSE ' ED-AMT ' TO ' LA-ACCT-NO ' (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
               DISPLAY 'NOT DISBURSED.'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE LND-PARMS
           MOVE LA-ACCT-NO TO LND-ACCT-NO
           MOVE LA-TYPE    TO LND-TYPE
           MOVE LA-AMOUNT  TO LND-AMOUNT
           MOVE LA-TENURE  TO LND-TENURE
           MOVE LA-RATE    TO LND-RATE
           MOVE LA-EMI     TO LND-EMI
           MOVE 'F'        TO LND-RATE-TYPE
           IF LA-TYPE = 'H'
               MOVE 'R'    TO LND-RATE-TYPE
               MOVE 'HLSPREAD' TO RTP-CODE
               CALL 'RATEUTIL' USING RATE-PARMS
               MOVE RTP-VALUE TO LND-SPREAD
           END-IF
           STRING 'LOAN DISBURSAL APP ' LA-APP-NO DELIMITED BY SIZE
               INTO LND-NARRATION
           CALL 'LNDISB' USING LND-PARMS
           IF NOT LND-OK
               DISPLAY 'DISBURSAL FAILED, RC=' LND-RC ' POST RC '
                       LND-POST-RC
               EXIT PARAGRAPH
           END-IF
           SET LA-DISBURSED TO TRUE
           MOVE LND-LOAN-NO TO LA-LOAN-NO
           REWRITE LAPP-RECORD
           DISPLAY 'LOAN ' LND-LOAN-NO ' DISBURSED. FIRST EMI DUE '
                   LND-FIRST-DUE
           MOVE 'DISBURSE' TO AU-ACTION
           PERFORM 9000-AUDIT.

      *================================================================*
      * 5 / 6                                                          *
      *================================================================*
       5000-STATUS.
           PERFORM 7000-READ-APP
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8100-STATUS-NAME
           MOVE LA-AMOUNT TO ED-AMT
           DISPLAY 'APPLICATION : ' LA-APP-NO '  ' WS-STATUS-NAME
           DISPLAY 'ACCOUNT     : ' LA-ACCT-NO '  CUSTOMER ' LA-CUST-ID
           DISPLAY 'LOAN        : ' LA-TYPE ' ' ED-AMT ' FOR '
                   LA-TENURE ' MONTHS'
           PERFORM 1400-SHOW-ASSESSMENT
           DISPLAY 'ENTERED BY  : ' LA-ENTERED-BY ' ON '
                   LA-ENTRY-DATE
           IF LA-DECIDED-BY NOT = SPACES
               DISPLAY 'DECIDED BY  : ' LA-DECIDED-BY ' ON '
                       LA-DECISION-DATE ' - ' LA-REASON
           END-IF
           IF LA-DISBURSED
               DISPLAY 'LOAN NO     : ' LA-LOAN-NO
           END-IF.

       6000-WITHDRAW.
           PERFORM 7000-READ-APP
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT LA-UNDER-REVIEW AND NOT LA-APPROVED
               DISPLAY 'ONLY AN OPEN APPLICATION CAN BE WITHDRAWN.'
               EXIT PARAGRAPH
           END-IF
           SET LA-WITHDRAWN TO TRUE
           MOVE 'WITHDRAWN BY CUSTOMER' TO LA-REASON
           MOVE SES-USER-ID TO LA-DECIDED-BY
           MOVE WS-TODAY    TO LA-DECISION-DATE
           REWRITE LAPP-RECORD
           DISPLAY 'APPLICATION ' LA-APP-NO ' WITHDRAWN.'
           MOVE 'WITHDRAW' TO AU-ACTION
           PERFORM 9000-AUDIT.

      *================================================================*
      * COMMON                                                         *
      *================================================================*
       7000-READ-APP.
           DISPLAY 'APPLICATION NO : ' WITH NO ADVANCING
           ACCEPT WS-IN-APP-NO
           MOVE WS-IN-APP-NO TO LA-APP-NO
           READ APP-FILE
               INVALID KEY
                   DISPLAY 'NO SUCH APPLICATION.'
                   MOVE '23' TO WS-FS
                   EXIT PARAGRAPH
           END-READ
      *    THE PRODUCT ROW IS NEEDED BY THE ASSESSMENT DISPLAY
           SET PRD-IDX TO 1
           SEARCH WS-PRODUCT
               WHEN PRD-TYPE(PRD-IDX) = LA-TYPE
                   CONTINUE
           END-SEARCH.

       8000-START-BROWSE.
           MOVE 'N' TO WS-EOF-SW
           MOVE ZERO TO LA-APP-NO
           START APP-FILE KEY IS NOT LESS THAN LA-APP-NO
               INVALID KEY SET END-OF-FILE TO TRUE
           END-START.

       8100-STATUS-NAME.
           EVALUATE TRUE
               WHEN LA-UNDER-REVIEW
                   MOVE 'UNDER REVIEW' TO WS-STATUS-NAME
               WHEN LA-APPROVED     MOVE 'APPROVED'  TO WS-STATUS-NAME
               WHEN LA-REJECTED     MOVE 'REJECTED'  TO WS-STATUS-NAME
               WHEN LA-DISBURSED    MOVE 'DISBURSED' TO WS-STATUS-NAME
               WHEN LA-WITHDRAWN    MOVE 'WITHDRAWN' TO WS-STATUS-NAME
               WHEN OTHER           MOVE 'UNKNOWN'   TO WS-STATUS-NAME
           END-EVALUATE.

       9000-AUDIT.
           MOVE 'LOANAPP'  TO AU-PROGRAM
           MOVE LA-APP-NO  TO AU-KEY
           MOVE LA-AMOUNT  TO ED-AMT
           MOVE SPACES     TO AU-DETAIL
           STRING LA-ACCT-NO ' ' LA-TYPE ' ' ED-AMT
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
