       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOANMNT.
      *================================================================*
      * PROGRAM  : LOANMNT                                             *
      * PURPOSE  : LOAN MANAGEMENT - SANCTION AND DISBURSE A LOAN TO   *
      *            A LINKED SAVINGS/CURRENT ACCOUNT, LOAN INQUIRY,     *
      *            AMORTISATION SCHEDULE, EMI PAYMENT AND FORECLOSURE. *
      *            EMI USES THE STANDARD REDUCING-BALANCE FORMULA:     *
      *              EMI = P * R * (1+R)**N / ((1+R)**N - 1)           *
      *            WHERE R = ANNUAL RATE / 12 / 100, N = MONTHS.       *
      * FILES    : LOANMAST (INDEXED, I-O)                             *
      *            LNSCHED  (PRINT FILE, OUTPUT)                       *
      * CALLS    : ACCTPOST, DATEUTIL                                  *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      * 2026-10-14 BOOKING MOVED TO LNDISB (SHARED WITH LOANAPP)     *
      * 2026-10-14 HOME LOANS REPO-LINKED (RATE MASTER, LNRESET)  *
      * 2026-10-14 EMI / FORECLOSURE MOVED TO LNEMIPAY (WITH GL SPLIT) *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LOAN-NO
               FILE STATUS IS WS-LOAN-FS.
           SELECT SCHED-FILE ASSIGN TO WS-SCHED-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SCHED-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  SCHED-FILE.
       01  SCHED-LINE                  PIC X(100).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY DATEPARM.
           COPY LNPPARM.

       01  WS-LOAN-FS                  PIC X(02).
           88  LOAN-FS-OK                        VALUE '00'.
           88  LOAN-FS-NOFILE                    VALUE '35'.
       01  WS-SCHED-FS                 PIC X(02).
       01  WS-SCHED-FILENAME           PIC X(40).

       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
           88  CHOICE-BACK                       VALUE '9'.
       01  WS-CONFIRM                  PIC X(01).
           88  CONFIRM-YES                       VALUES 'Y' 'y'.
       01  WS-VALID-SW                 PIC X(01).
           88  INPUT-VALID                       VALUE 'Y'.
           88  INPUT-INVALID                     VALUE 'N'.

       01  WS-INPUT.
           05  WS-IN-LOAN-NO           PIC 9(10).
           05  WS-IN-ACCT-NO           PIC 9(10).
           05  WS-IN-TYPE              PIC X(01).
           05  WS-IN-AMOUNT            PIC 9(11)V99.
           05  WS-IN-TENURE            PIC 9(03).

           COPY LNPROD.
       01  WS-PRD-FOUND-SW             PIC X(01).
           88  PRODUCT-FOUND                     VALUE 'Y'.

      * EMI ARITHMETIC (FLOATING POINT FOR THE POWER TERM ONLY)
       01  WS-MONTHLY-RATE             COMP-2.
       01  WS-POWER-FACTOR             COMP-2.
       01  WS-EMI-CALC                 PIC 9(09)V99.
       01  WS-INT-PORTION              PIC 9(09)V99.
       01  WS-PRIN-PORTION             PIC 9(11)V99.
       01  WS-AMOUNT-DUE               PIC 9(11)V99.
       01  WS-SIM-BALANCE              PIC 9(11)V99.
       01  WS-SIM-DATE                 PIC 9(08).
       01  WS-SIM-NO                   PIC 9(03).
       01  WS-FORECLOSE-CHG            PIC 9(09)V99.
       01  WS-FORECLOSE-PCT            PIC 9V99 VALUE 0.02.
           COPY RATEPARM.
           COPY LNDPARM.
       01  WS-SPREAD                   PIC 9(02)V9(04).
       01  WS-LOAN-RATE                PIC 9(02)V9(04).

       01  WS-DISP-AMT                 PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-RATE                PIC Z9.99.

       01  RPT-SCHED-HEAD.
           05  FILLER  PIC X(06) VALUE ' NO.'.
           05  FILLER  PIC X(13) VALUE 'DUE DATE'.
           05  FILLER  PIC X(17) VALUE '             EMI'.
           05  FILLER  PIC X(17) VALUE '       PRINCIPAL'.
           05  FILLER  PIC X(17) VALUE '        INTEREST'.
           05  FILLER  PIC X(17) VALUE '         BALANCE'.
       01  RPT-SCHED-LINE.
           05  RPT-NO                  PIC ZZZ9.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  RPT-DUE                 PIC 9999/99/99.
           05  FILLER                  PIC X(03) VALUE SPACES.
           05  RPT-EMI                 PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(04) VALUE SPACES.
           05  RPT-PRIN                PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(04) VALUE SPACES.
           05  RPT-INT                 PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  RPT-BAL                 PIC ZZ,ZZZ,ZZZ,ZZ9.99.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-OPEN-FILE
           IF NOT LOAN-FS-OK
               GOBACK
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL CHOICE-BACK
               PERFORM 1100-SHOW-MENU
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-NEW-LOAN
                   WHEN '2' PERFORM 3000-INQUIRY
                   WHEN '3' PERFORM 4000-SCHEDULE
                   WHEN '4' PERFORM 5000-PAY-EMI
                   WHEN '5' PERFORM 6000-FORECLOSE
                   WHEN '6'
                       CLOSE LOAN-FILE
                       CALL 'LOANAPP'
                       PERFORM 1000-OPEN-FILE
                   WHEN '7'
                       CLOSE LOAN-FILE
                       CALL 'COLLMNT'
                       PERFORM 1000-OPEN-FILE
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE LOAN-FILE
           GOBACK.

       1000-OPEN-FILE.
           OPEN I-O LOAN-FILE
           IF LOAN-FS-NOFILE
               OPEN OUTPUT LOAN-FILE
               CLOSE LOAN-FILE
               OPEN I-O LOAN-FILE
           END-IF
           IF NOT LOAN-FS-OK
               DISPLAY 'LOAN MASTER OPEN FAILED, FS=' WS-LOAN-FS
           END-IF.

       1100-SHOW-MENU.
           DISPLAY ' '
           DISPLAY '------------- LOAN MANAGEMENT -------------'
           DISPLAY ' 1. SANCTION AND DISBURSE NEW LOAN'
           DISPLAY ' 2. LOAN INQUIRY'
           DISPLAY ' 3. REPAYMENT SCHEDULE'
           DISPLAY ' 4. PAY EMI'
           DISPLAY ' 5. FORECLOSE LOAN'
           DISPLAY ' 6. LOAN APPLICATIONS (ORIGINATION)'
           DISPLAY ' 7. COLLECTIONS AND RECOVERY'
           DISPLAY ' 9. BACK TO MAIN MENU'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT.

      *----------------------------------------------------------------*
      * NEW LOAN                                                       *
      *----------------------------------------------------------------*
       2000-NEW-LOAN.
           SET INPUT-VALID TO TRUE
           DISPLAY 'LINKED ACCOUNT NO     : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'TYPE (H/P/V)          : ' WITH NO ADVANCING
           ACCEPT WS-IN-TYPE
           MOVE FUNCTION UPPER-CASE(WS-IN-TYPE) TO WS-IN-TYPE
           DISPLAY 'LOAN AMOUNT           : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           DISPLAY 'TENURE (MONTHS)       : ' WITH NO ADVANCING
           ACCEPT WS-IN-TENURE

           PERFORM 2100-VALIDATE-REQUEST
           IF INPUT-INVALID
               EXIT PARAGRAPH
           END-IF

           MOVE WS-IN-AMOUNT     TO LOAN-PRINCIPAL
           MOVE PRD-RATE(PRD-IDX) TO LOAN-RATE
           IF WS-IN-TYPE = 'H'
               PERFORM 2050-REPO-LINKED-RATE
           END-IF
           MOVE LOAN-RATE        TO WS-LOAN-RATE
           MOVE WS-IN-TENURE     TO LOAN-TENURE
           PERFORM 7000-CALC-EMI
           MOVE WS-EMI-CALC TO WS-DISP-AMT
           MOVE LOAN-RATE   TO WS-DISP-RATE
           DISPLAY 'RATE ' WS-DISP-RATE '% P.A.  EMI: ' WS-DISP-AMT
           DISPLAY 'CONFIRM DISBURSAL (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF NOT CONFIRM-YES
               DISPLAY 'LOAN NOT DISBURSED.'
               EXIT PARAGRAPH
           END-IF

           PERFORM 2200-BOOK-LOAN.

      *    HOME LOANS ARE REPO-LINKED: RATE = REPO + HOME LOAN SPREAD,
      *    BOTH FROM THE RATE MASTER (RBI EXTERNAL BENCHMARK RULE).
       2050-REPO-LINKED-RATE.
           MOVE 'HLSPREAD' TO RTP-CODE
           CALL 'RATEUTIL' USING RATE-PARMS
           MOVE RTP-VALUE TO WS-SPREAD
           MOVE 'REPO' TO RTP-CODE
           CALL 'RATEUTIL' USING RATE-PARMS
           COMPUTE LOAN-RATE = RTP-VALUE + WS-SPREAD
           MOVE LOAN-RATE TO WS-DISP-RATE
           DISPLAY 'REPO-LINKED: REPO + SPREAD = ' WS-DISP-RATE '%'.

       2100-VALIDATE-REQUEST.
           MOVE 'N' TO WS-PRD-FOUND-SW
           SET PRD-IDX TO 1
           SEARCH WS-PRODUCT
               AT END
                   DISPLAY 'LOAN TYPE MUST BE H, P OR V.'
                   SET INPUT-INVALID TO TRUE
                   EXIT PARAGRAPH
               WHEN PRD-TYPE(PRD-IDX) = WS-IN-TYPE
                   SET PRODUCT-FOUND TO TRUE
           END-SEARCH
           IF WS-IN-AMOUNT < 10000
               DISPLAY 'MINIMUM LOAN AMOUNT IS 10,000.'
               SET INPUT-INVALID TO TRUE
           END-IF
           IF WS-IN-AMOUNT > PRD-MAX-AMOUNT(PRD-IDX)
               MOVE PRD-MAX-AMOUNT(PRD-IDX) TO WS-DISP-AMT
               DISPLAY 'AMOUNT ABOVE PRODUCT LIMIT OF ' WS-DISP-AMT
               SET INPUT-INVALID TO TRUE
           END-IF
           IF WS-IN-TENURE < 6
              OR WS-IN-TENURE > PRD-MAX-TENURE(PRD-IDX)
               DISPLAY 'TENURE MUST BE 6 TO '
                       PRD-MAX-TENURE(PRD-IDX) ' MONTHS.'
               SET INPUT-INVALID TO TRUE
           END-IF
           IF INPUT-INVALID
               EXIT PARAGRAPH
           END-IF
      *    LINKED ACCOUNT MUST EXIST AND BE ACTIVE
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           EVALUATE TRUE
               WHEN PP-NOT-FOUND
                   DISPLAY 'LINKED ACCOUNT NOT FOUND.'
                   SET INPUT-INVALID TO TRUE
               WHEN NOT PP-OK
                   DISPLAY 'LINKED ACCOUNT CHECK FAILED, RC='
                           PP-RETURN-CODE
                   SET INPUT-INVALID TO TRUE
               WHEN OTHER
                   DISPLAY 'BORROWER: ' PP-HOLDER-NAME
           END-EVALUATE.

      *    LNDISB OPENS THE LOAN MASTER ITSELF, SO RELEASE IT FIRST
       2200-BOOK-LOAN.
           INITIALIZE LND-PARMS
           MOVE WS-IN-ACCT-NO TO LND-ACCT-NO
           MOVE WS-IN-TYPE    TO LND-TYPE
           MOVE WS-IN-AMOUNT  TO LND-AMOUNT
           MOVE WS-IN-TENURE  TO LND-TENURE
           MOVE WS-LOAN-RATE  TO LND-RATE
           MOVE WS-EMI-CALC   TO LND-EMI
           MOVE 'F'           TO LND-RATE-TYPE
           IF WS-IN-TYPE = 'H'
               MOVE 'R'       TO LND-RATE-TYPE
               MOVE WS-SPREAD TO LND-SPREAD
           END-IF
           CLOSE LOAN-FILE
           CALL 'LNDISB' USING LND-PARMS
           PERFORM 1000-OPEN-FILE
           EVALUATE TRUE
               WHEN LND-OK
                   DISPLAY 'LOAN ' LND-LOAN-NO ' DISBURSED. FIRST EMI '
                           'DUE ' LND-FIRST-DUE
               WHEN LND-CREDIT-FAILED
                   DISPLAY 'DISBURSAL FAILED, RC=' LND-POST-RC
               WHEN LND-WRITE-FAILED
                   DISPLAY 'CRITICAL: LOAN WRITE FAILED - AMOUNT '
                           'ALREADY CREDITED TO ' WS-IN-ACCT-NO
               WHEN OTHER
                   DISPLAY 'LOAN MASTER NOT AVAILABLE, RC=' LND-RC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INQUIRY                                                        *
      *----------------------------------------------------------------*
       3000-INQUIRY.
           PERFORM 8000-READ-LOAN
           IF NOT LOAN-FS-OK
               EXIT PARAGRAPH
           END-IF
           DISPLAY '----------------------------------------'
           DISPLAY 'LOAN NO        : ' LOAN-NO
           DISPLAY 'LINKED ACCOUNT : ' LOAN-ACCT-NO
           EVALUATE TRUE
               WHEN LOAN-HOME     DISPLAY 'TYPE           : HOME'
               WHEN LOAN-PERSONAL DISPLAY 'TYPE           : PERSONAL'
               WHEN LOAN-VEHICLE  DISPLAY 'TYPE           : VEHICLE'
               WHEN LOAN-GOLD     DISPLAY 'TYPE           : GOLD'
               WHEN LOAN-AGAINST-FD
                   DISPLAY 'TYPE           : AGAINST FD '
                           LOAN-LIEN-DEP-NO
           END-EVALUATE
           EVALUATE TRUE
               WHEN LOAN-ACTIVE   DISPLAY 'STATUS         : ACTIVE'
               WHEN LOAN-CLOSED   DISPLAY 'STATUS         : CLOSED'
               WHEN LOAN-NPA      DISPLAY 'STATUS         : NPA'
           END-EVALUATE
           MOVE LOAN-PRINCIPAL TO WS-DISP-AMT
           DISPLAY 'PRINCIPAL      : ' WS-DISP-AMT
           MOVE LOAN-RATE TO WS-DISP-RATE
           DISPLAY 'RATE (% P.A.)  : ' WS-DISP-RATE
           IF LOAN-REPO-LINKED
               MOVE LOAN-SPREAD TO WS-DISP-RATE
               DISPLAY 'RATE TYPE      : REPO-LINKED, SPREAD '
                       WS-DISP-RATE '%, RESET ' LOAN-RESET-PREF
           ELSE
               DISPLAY 'RATE TYPE      : FIXED'
           END-IF
           MOVE LOAN-EMI TO WS-DISP-AMT
           DISPLAY 'EMI            : ' WS-DISP-AMT
           DISPLAY 'EMIS PAID      : ' LOAN-EMIS-PAID ' OF '
                   LOAN-TENURE
           MOVE LOAN-OUTSTANDING TO WS-DISP-AMT
           DISPLAY 'OUTSTANDING    : ' WS-DISP-AMT
           DISPLAY 'NEXT DUE DATE  : ' LOAN-NEXT-DUE-DATE
           IF LOAN-OVERDUE-EMIS > ZERO
               DISPLAY 'OVERDUE EMIS   : ' LOAN-OVERDUE-EMIS
               MOVE LOAN-PENALTY-DUE TO WS-DISP-AMT
               DISPLAY 'PENALTY DUE    : ' WS-DISP-AMT
           END-IF
           MOVE LOAN-TOTAL-INT-PAID TO WS-DISP-AMT
           DISPLAY 'INTEREST PAID  : ' WS-DISP-AMT
           DISPLAY '----------------------------------------'.

      *----------------------------------------------------------------*
      * SCHEDULE - SIMULATES THE REMAINING EMIS FROM CURRENT STATE     *
      *----------------------------------------------------------------*
       4000-SCHEDULE.
           PERFORM 8000-READ-LOAN
           IF NOT LOAN-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF NOT LOAN-ACTIVE AND NOT LOAN-NPA
               DISPLAY 'LOAN IS CLOSED - NO SCHEDULE.'
               EXIT PARAGRAPH
           END-IF
           STRING 'data/SCHED_' DELIMITED BY SIZE
                  LOAN-NO       DELIMITED BY SIZE
                  '.TXT'        DELIMITED BY SIZE
                  INTO WS-SCHED-FILENAME
           END-STRING
           OPEN OUTPUT SCHED-FILE
           MOVE RPT-SCHED-HEAD TO SCHED-LINE
           PERFORM 4900-EMIT

           COMPUTE WS-MONTHLY-RATE = LOAN-RATE / 1200
           MOVE LOAN-OUTSTANDING   TO WS-SIM-BALANCE
           MOVE LOAN-NEXT-DUE-DATE TO WS-SIM-DATE
           MOVE LOAN-EMIS-PAID     TO WS-SIM-NO
           PERFORM UNTIL WS-SIM-BALANCE = ZERO
                      OR WS-SIM-NO >= 600
               ADD 1 TO WS-SIM-NO
               PERFORM 7200-SPLIT-EMI
               SUBTRACT WS-PRIN-PORTION FROM WS-SIM-BALANCE
               MOVE WS-SIM-NO       TO RPT-NO
               MOVE WS-SIM-DATE     TO RPT-DUE
               COMPUTE RPT-EMI = WS-PRIN-PORTION + WS-INT-PORTION
               MOVE WS-PRIN-PORTION TO RPT-PRIN
               MOVE WS-INT-PORTION  TO RPT-INT
               MOVE WS-SIM-BALANCE  TO RPT-BAL
               MOVE RPT-SCHED-LINE  TO SCHED-LINE
               PERFORM 4900-EMIT
               MOVE WS-SIM-DATE TO DU-DATE-1
               MOVE 1           TO DU-NUMBER
               PERFORM 7100-ADD-MONTHS
               MOVE DU-RESULT-DATE TO WS-SIM-DATE
           END-PERFORM
           CLOSE SCHED-FILE
           DISPLAY 'SCHEDULE SAVED TO ' WS-SCHED-FILENAME.

       4900-EMIT.
           WRITE SCHED-LINE
           DISPLAY SCHED-LINE.

      *----------------------------------------------------------------*
      * PAY EMI / FORECLOSE - QUOTE FIRST, CONFIRM, THEN COLLECT. THE  *
      * ARITHMETIC AND POSTINGS LIVE IN LNEMIPAY (SHARED WITH THE      *
      * STANDING INSTRUCTION BATCH).                                   *
      *----------------------------------------------------------------*
       5000-PAY-EMI.
           PERFORM 8000-READ-LOAN
           IF NOT LOAN-FS-OK
               EXIT PARAGRAPH
           END-IF
           MOVE 'QE' TO LNP-MODE
           PERFORM 5100-QUOTE-AND-COLLECT
           IF LNP-OK
               MOVE LOAN-OUTSTANDING TO WS-DISP-AMT
               DISPLAY 'EMI PAID. OUTSTANDING NOW: ' WS-DISP-AMT
               IF LOAN-CLOSED
                   DISPLAY 'LOAN FULLY REPAID AND CLOSED.'
               END-IF
           END-IF.

       5100-QUOTE-AND-COLLECT.
           MOVE 'LOAN EMI' TO LNP-NARRATION
           IF LNP-MODE = 'QF'
               MOVE 'LOAN FORECLOSURE' TO LNP-NARRATION
           END-IF
           CALL 'LNEMIPAY' USING LNP-PARMS LOAN-RECORD
           IF LNP-LOAN-CLOSED
               DISPLAY 'LOAN IS ALREADY CLOSED.'
               EXIT PARAGRAPH
           END-IF
           MOVE LNP-PRINCIPAL TO WS-DISP-AMT
           DISPLAY 'PRINCIPAL          : ' WS-DISP-AMT
           MOVE LNP-INTEREST TO WS-DISP-AMT
           DISPLAY 'INTEREST           : ' WS-DISP-AMT
           IF LNP-PENALTY > ZERO
               MOVE LNP-PENALTY TO WS-DISP-AMT
               DISPLAY 'PENALTY            : ' WS-DISP-AMT
           END-IF
           IF LNP-CHARGE > ZERO
               MOVE LNP-CHARGE TO WS-DISP-AMT
               DISPLAY 'FORECLOSURE CHARGE : ' WS-DISP-AMT
           END-IF
           MOVE LNP-AMOUNT-DUE TO WS-DISP-AMT
           DISPLAY 'AMOUNT TO DEBIT FROM ' LOAN-ACCT-NO ': '
                   WS-DISP-AMT
           DISPLAY 'CONFIRM (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF NOT CONFIRM-YES
               MOVE 99 TO LNP-RC
               EXIT PARAGRAPH
           END-IF
           IF LNP-MODE = 'QE'
               MOVE 'PE' TO LNP-MODE
           ELSE
               MOVE 'PF' TO LNP-MODE
           END-IF
           CALL 'LNEMIPAY' USING LNP-PARMS LOAN-RECORD
           EVALUATE TRUE
               WHEN LNP-OK
                   PERFORM 8100-REWRITE-LOAN
               WHEN LNP-INSUFFICIENT
                   MOVE LNP-AVAILABLE TO WS-DISP-AMT
                   DISPLAY 'INSUFFICIENT BALANCE. AVAILABLE: '
                           WS-DISP-AMT
               WHEN OTHER
                   DISPLAY 'LINKED ACCOUNT CANNOT BE DEBITED, RC='
                           LNP-RC
           END-EVALUATE.

       6000-FORECLOSE.
           PERFORM 8000-READ-LOAN
           IF NOT LOAN-FS-OK
               EXIT PARAGRAPH
           END-IF
           MOVE 'QF' TO LNP-MODE
           PERFORM 5100-QUOTE-AND-COLLECT
           IF LNP-OK
               DISPLAY 'LOAN ' LOAN-NO ' FORECLOSED AND CLOSED.'
           END-IF.

      *----------------------------------------------------------------*
      * CALCULATIONS                                                   *
      *----------------------------------------------------------------*
       7000-CALC-EMI.
           COMPUTE WS-MONTHLY-RATE = LOAN-RATE / 1200
           COMPUTE WS-POWER-FACTOR =
                   (1 + WS-MONTHLY-RATE) ** LOAN-TENURE
           COMPUTE WS-EMI-CALC ROUNDED =
                   LOAN-PRINCIPAL * WS-MONTHLY-RATE * WS-POWER-FACTOR
                   / (WS-POWER-FACTOR - 1).

       7100-ADD-MONTHS.
           SET DU-ADD-MONTHS TO TRUE
           CALL 'DATEUTIL' USING DATE-PARMS.

      *    SPLITS ONE EMI ON WS-SIM-BALANCE INTO INTEREST + PRINCIPAL.
      *    THE LAST EMI CLEARS WHATEVER IS LEFT (ROUNDING RESIDUE).
       7200-SPLIT-EMI.
           COMPUTE WS-INT-PORTION ROUNDED =
                   WS-SIM-BALANCE * WS-MONTHLY-RATE
           IF LOAN-EMI > WS-INT-PORTION
               COMPUTE WS-PRIN-PORTION = LOAN-EMI - WS-INT-PORTION
           ELSE
               MOVE ZERO TO WS-PRIN-PORTION
           END-IF
           IF WS-PRIN-PORTION > WS-SIM-BALANCE
              OR WS-SIM-BALANCE - WS-PRIN-PORTION < 1.00
               MOVE WS-SIM-BALANCE TO WS-PRIN-PORTION
           END-IF.

      *----------------------------------------------------------------*
      * I/O                                                            *
      *----------------------------------------------------------------*
       8000-READ-LOAN.
           DISPLAY 'LOAN NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-LOAN-NO
           MOVE WS-IN-LOAN-NO TO LOAN-NO
           READ LOAN-FILE
               INVALID KEY
                   DISPLAY 'LOAN ' WS-IN-LOAN-NO ' NOT FOUND.'
           END-READ.

       8100-REWRITE-LOAN.
           REWRITE LOAN-RECORD
               INVALID KEY
                   DISPLAY 'CRITICAL: LOAN REWRITE FAILED FS='
                           WS-LOAN-FS
           END-REWRITE.

       8200-SHOW-POST-ERROR.
           EVALUATE TRUE
               WHEN PP-INSUFFICIENT
                   MOVE PP-AVAILABLE TO WS-DISP-AMT
                   DISPLAY 'INSUFFICIENT BALANCE. AVAILABLE: '
                           WS-DISP-AMT
               WHEN PP-NOT-ACTIVE
                   DISPLAY 'LINKED ACCOUNT IS NOT ACTIVE.'
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
