       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFMNT.
      *================================================================*
      * PROGRAM  : MFMNT                                               *
      * PURPOSE  : THIRD-PARTY PRODUCT DISTRIBUTION AT THE COUNTER -   *
      *            MUTUAL FUNDS AND INSURANCE.                         *
      *            1 PRODUCT MASTER / NAV UPDATE (MANAGER)             *
      *            2 BUY FUND UNITS (LUMPSUM)                          *
      *            3 REDEEM UNITS                                      *
      *            4 REGISTER SIP                                      *
      *            5 CANCEL SIP                                        *
      *            6 PORTFOLIO OF AN ACCOUNT                           *
      *            7 ISSUE INSURANCE POLICY (FIRST PREMIUM NOW)        *
      *            SUITABILITY: KYC MUST BE VERIFIED; A HIGH-RISK FUND *
      *            FOR A CUSTOMER AGED 60+ NEEDS AN EXPLICIT RISK      *
      *            ACKNOWLEDGEMENT, WHICH IS AUDITED.                  *
      * FILES    : MFSCHEME (I-O), MFHOLD (INPUT), MFSIP (I-O) - EACH  *
      *            OPENED ONLY INSIDE THE OPTION THAT NEEDS IT, SO     *
      *            MFUTIL CAN OPEN THEM FOR THE MONEY SIDE             *
      * CALLS    : MFUTIL, CUSTLKUP, DATEUTIL, AUDITLOG                *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MFS-FILE ASSIGN TO 'data/MFSCHEME.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MS-CODE
               FILE STATUS IS WS-MFS-FS.
           SELECT MFH-FILE ASSIGN TO 'data/MFHOLD.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MH-KEY
               FILE STATUS IS WS-MFH-FS.
           SELECT SIP-FILE ASSIGN TO 'data/MFSIP.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SP-NO
               FILE STATUS IS WS-SIP-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  MFS-FILE.
           COPY MFSREC.
       FD  MFH-FILE.
           COPY MFHREC.
       FD  SIP-FILE.
           COPY SIPREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY SESSION.
           COPY AUDPARM.
           COPY MFUPARM.
           COPY CLKPARM.
           COPY DATEPARM.
       01  WS-MFS-FS                   PIC X(02).
       01  WS-MFH-FS                   PIC X(02).
       01  WS-SIP-FS                   PIC X(02).
       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.
       01  WS-EOF-SW                   PIC X(01).
       01  WS-CONFIRM                  PIC X(01).
       01  WS-OK-SW                    PIC X(01).
           88  CHECKS-OK                         VALUE 'Y'.
       01  WS-IN-ACCT                  PIC 9(10).
       01  WS-IN-CODE                  PIC X(06).
       01  WS-IN-TEXT                  PIC X(30).
       01  WS-IN-X1                    PIC X(01).
       01  WS-IN-NUM                   PIC X(14).
       01  WS-IN-SIP                   PIC 9(08).
       01  WS-NUM                      PIC 9(11)V9(04).
       01  WS-OLD-NAV                  PIC 9(05)V9(04).
       01  WS-NAV-MOVE-PCT             PIC S9(05)V99.
       01  WS-MAX-NAV-MOVE             PIC 9(03)V99 VALUE 10.00.
       01  WS-AGE                      PIC 9(03).
       01  WS-SENIOR-AGE               PIC 9(03) VALUE 60.
       01  WS-MIN-SIP                  PIC 9(09)V99 VALUE 500.00.
       01  WS-SIP-DAY                  PIC 9(02).
       01  WS-SIP-MONTHS               PIC 9(03).
       01  WS-NEXT-SIP                 PIC 9(08).
       01  WS-CNT                      PIC 9(05).
       01  WS-VALUE                    PIC 9(11)V99.
       01  WS-TOT-VALUE                PIC 9(13)V99.
       01  WS-TOT-COST                 PIC 9(13)V99.
       01  WS-GAIN                     PIC S9(11)V99.
       01  WS-SUM-ASSURED              PIC 9(11)V99.
       01  WS-PREMIUM                  PIC 9(09)V99.
       01  WS-SAVE-NAME                PIC X(30).
       01  WS-SAVE-NAV                 PIC 9(05)V9(04).
       01  WS-DISP-AMT                 PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-AMT2                PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-GAIN                PIC -Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-UNITS               PIC ZZZ,ZZZ,ZZ9.999.
       01  WS-DISP-NAV                 PIC ZZ,ZZ9.9999.
       01  WS-DISP-PCT                 PIC -ZZZZ9.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE = '9'
               DISPLAY ' '
               DISPLAY '------ MUTUAL FUNDS AND INSURANCE ------'
               DISPLAY ' 1. PRODUCT MASTER / NAV UPDATE (MANAGER)'
               DISPLAY ' 2. BUY FUND UNITS'
               DISPLAY ' 3. REDEEM UNITS'
               DISPLAY ' 4. REGISTER SIP'
               DISPLAY ' 5. CANCEL SIP'
               DISPLAY ' 6. PORTFOLIO OF AN ACCOUNT'
               DISPLAY ' 7. ISSUE INSURANCE POLICY'
               DISPLAY ' 9. BACK'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 1000-PRODUCT
                   WHEN '2' PERFORM 2000-BUY
                   WHEN '3' PERFORM 3000-REDEEM
                   WHEN '4' PERFORM 4000-SIP-REGISTER
                   WHEN '5' PERFORM 5000-SIP-CANCEL
                   WHEN '6' PERFORM 6000-PORTFOLIO
                   WHEN '7' PERFORM 7000-POLICY
                   WHEN '9' CONTINUE
                   WHEN SPACE
                       ADD 1 TO WS-BLANK-INPUTS
                       IF WS-BLANK-INPUTS > 20
                           MOVE '9' TO WS-CHOICE
                       END-IF
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
               IF WS-CHOICE NOT = SPACE
                   MOVE ZERO TO WS-BLANK-INPUTS
               END-IF
           END-PERFORM
           MOVE 'Y' TO CLK-CLOSE
           CALL 'CUSTLKUP' USING CLK-PARMS
           GOBACK.

      *----------------------------------------------------------------*
      * 1. PRODUCT MASTER - NEW PRODUCT, OR NAV / STATUS OF AN OLD ONE *
      *----------------------------------------------------------------*
       1000-PRODUCT.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'ONLY A MANAGER CAN MAINTAIN PRODUCTS.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8000-OPEN-PRODUCTS
           DISPLAY 'PRODUCT CODE         : ' WITH NO ADVANCING
           ACCEPT WS-IN-CODE
           MOVE FUNCTION UPPER-CASE(WS-IN-CODE) TO MS-CODE
           READ MFS-FILE
               INVALID KEY
                   PERFORM 1100-NEW-PRODUCT
               NOT INVALID KEY
                   PERFORM 1200-UPDATE-PRODUCT
           END-READ
           CLOSE MFS-FILE.

       1100-NEW-PRODUCT.
           MOVE MS-CODE TO WS-IN-CODE
           INITIALIZE MFS-RECORD
           MOVE WS-IN-CODE TO MS-CODE
           DISPLAY 'NEW PRODUCT - NAME   : ' WITH NO ADVANCING
           ACCEPT MS-NAME
           MOVE FUNCTION UPPER-CASE(MS-NAME) TO MS-NAME
           DISPLAY 'TYPE M=FUND I=INSURANCE: ' WITH NO ADVANCING
           ACCEPT WS-IN-X1
           MOVE FUNCTION UPPER-CASE(WS-IN-X1) TO MS-TYPE
           DISPLAY 'CATEGORY (EQTY DEBT HYBR TERM HLTH ULIP): '
                   WITH NO ADVANCING
           ACCEPT MS-CATEGORY
           MOVE FUNCTION UPPER-CASE(MS-CATEGORY) TO MS-CATEGORY
           DISPLAY 'RISK L/M/H           : ' WITH NO ADVANCING
           ACCEPT WS-IN-X1
           MOVE FUNCTION UPPER-CASE(WS-IN-X1) TO MS-RISK
           MOVE 'Y' TO WS-OK-SW
           IF MS-MUTUAL-FUND
               DISPLAY 'NAV                  : ' WITH NO ADVANCING
               PERFORM 8100-ACCEPT-NUMBER
               MOVE WS-NUM TO MS-NAV
               DISPLAY 'TRAIL COMMISSION % PA: ' WITH NO ADVANCING
               PERFORM 8100-ACCEPT-NUMBER
               MOVE WS-NUM TO MS-COMM-PCT
               DISPLAY 'MINIMUM INVESTMENT   : ' WITH NO ADVANCING
               PERFORM 8100-ACCEPT-NUMBER
               MOVE WS-NUM TO MS-MIN-AMT
               DISPLAY 'EXIT LOAD %          : ' WITH NO ADVANCING
               PERFORM 8100-ACCEPT-NUMBER
               MOVE WS-NUM TO MS-EXIT-LOAD-PCT
               DISPLAY 'EXIT LOAD PERIOD DAYS: ' WITH NO ADVANCING
               PERFORM 8100-ACCEPT-NUMBER
               MOVE WS-NUM TO MS-LOAD-DAYS
               IF MS-NAV = ZERO
                   DISPLAY 'NAV MUST BE ABOVE ZERO.'
                   MOVE 'N' TO WS-OK-SW
               END-IF
           ELSE
               DISPLAY 'COMMISSION % OF PREMIUM: ' WITH NO ADVANCING
               PERFORM 8100-ACCEPT-NUMBER
               MOVE WS-NUM TO MS-COMM-PCT
               DISPLAY 'MINIMUM PREMIUM      : ' WITH NO ADVANCING
               PERFORM 8100-ACCEPT-NUMBER
               MOVE WS-NUM TO MS-MIN-AMT
           END-IF
           EVALUATE TRUE
               WHEN NOT CHECKS-OK
                   CONTINUE
               WHEN NOT MS-MUTUAL-FUND AND NOT MS-INSURANCE
                   DISPLAY 'TYPE MUST BE M OR I.'
               WHEN MS-NAME = SPACES
                   DISPLAY 'NAME IS REQUIRED.'
               WHEN MS-RISK NOT = 'L' AND MS-RISK NOT = 'M'
                    AND MS-RISK NOT = 'H'
                   DISPLAY 'RISK MUST BE L, M OR H.'
               WHEN MS-COMM-PCT > 35
                   DISPLAY 'COMMISSION ABOVE 35% IS NOT ALLOWED.'
               WHEN OTHER
                   SET MS-OPEN       TO TRUE
                   MOVE WS-CURR-DATE TO MS-NAV-DATE
                   MOVE SES-USER-ID  TO MS-SET-BY
                   WRITE MFS-RECORD
                   MOVE 'PRODUCT NEW' TO AU-ACTION
                   MOVE MS-CODE TO AU-KEY
                   MOVE MS-NAME TO AU-DETAIL
                   PERFORM 8900-AUDIT
                   DISPLAY 'PRODUCT ' MS-CODE ' ADDED.'
           END-EVALUATE.

      *    NAV MOVES OVER 10% IN ONE UPDATE NEED A SECOND LOOK
       1200-UPDATE-PRODUCT.
           DISPLAY 'PRODUCT ' MS-CODE ' ' MS-NAME ' STATUS ' MS-STATUS
           IF MS-INSURANCE
               DISPLAY 'C=CLOSE TO NEW BUSINESS  A=REOPEN  BLANK=NONE: '
                       WITH NO ADVANCING
               ACCEPT WS-IN-X1
               PERFORM 1300-STATUS-CHANGE
               EXIT PARAGRAPH
           END-IF
           MOVE MS-NAV TO WS-DISP-NAV WS-OLD-NAV
           DISPLAY 'CURRENT NAV ' WS-DISP-NAV ' OF ' MS-NAV-DATE
           DISPLAY 'NEW NAV (BLANK = NO CHANGE): ' WITH NO ADVANCING
           MOVE 'Y' TO WS-OK-SW
           PERFORM 8100-ACCEPT-NUMBER
           IF WS-IN-NUM NOT = SPACES AND CHECKS-OK AND WS-NUM > ZERO
               COMPUTE WS-NAV-MOVE-PCT ROUNDED =
                       (WS-NUM - WS-OLD-NAV) * 100 / WS-OLD-NAV
               IF FUNCTION ABS(WS-NAV-MOVE-PCT) > WS-MAX-NAV-MOVE
                   MOVE WS-NAV-MOVE-PCT TO WS-DISP-PCT
                   DISPLAY 'NAV MOVES ' FUNCTION TRIM(WS-DISP-PCT)
                           '% - CONFIRM (Y/N): ' WITH NO ADVANCING
                   ACCEPT WS-CONFIRM
                   IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
                       DISPLAY 'NAV NOT CHANGED.'
                       EXIT PARAGRAPH
                   END-IF
               END-IF
               MOVE WS-NUM       TO MS-NAV
               MOVE WS-CURR-DATE TO MS-NAV-DATE
               MOVE SES-USER-ID  TO MS-SET-BY
               REWRITE MFS-RECORD
               MOVE MS-NAV TO WS-DISP-NAV
               MOVE 'NAV UPDATE' TO AU-ACTION
               MOVE MS-CODE TO AU-KEY
               MOVE SPACES TO AU-DETAIL
               STRING 'NAV ' WS-DISP-NAV DELIMITED BY SIZE
                      INTO AU-DETAIL
               PERFORM 8900-AUDIT
               DISPLAY 'NAV OF ' MS-CODE ' NOW ' WS-DISP-NAV '.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'C=CLOSE TO NEW BUSINESS  A=REOPEN  BLANK=NONE: '
                   WITH NO ADVANCING
           ACCEPT WS-IN-X1
           PERFORM 1300-STATUS-CHANGE.

       1300-STATUS-CHANGE.
           MOVE FUNCTION UPPER-CASE(WS-IN-X1) TO WS-IN-X1
           IF WS-IN-X1 = 'C' OR WS-IN-X1 = 'A'
               MOVE WS-IN-X1 TO MS-STATUS
               REWRITE MFS-RECORD
               DISPLAY 'PRODUCT ' MS-CODE ' STATUS ' MS-STATUS '.'
           END-IF.

      *----------------------------------------------------------------*
      * 2. BUY                                                         *
      *----------------------------------------------------------------*
       2000-BUY.
           PERFORM 8200-ASK-ACCT-AND-PRODUCT
           IF NOT CHECKS-OK
               EXIT PARAGRAPH
           END-IF
           IF NOT MS-MUTUAL-FUND
               DISPLAY MS-CODE ' IS NOT A FUND.'
               CLOSE MFS-FILE
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'AMOUNT               : ' WITH NO ADVANCING
           PERFORM 8100-ACCEPT-NUMBER
           IF NOT CHECKS-OK
               CLOSE MFS-FILE
               EXIT PARAGRAPH
           END-IF
           IF WS-NUM < MS-MIN-AMT
               MOVE MS-MIN-AMT TO WS-DISP-AMT
               DISPLAY 'AMOUNT BELOW THE PRODUCT MINIMUM ('
                       FUNCTION TRIM(WS-DISP-AMT) ').'
               CLOSE MFS-FILE
               EXIT PARAGRAPH
           END-IF
           MOVE MS-NAV TO WS-DISP-NAV
           COMPUTE WS-DISP-UNITS = WS-NUM / MS-NAV
           DISPLAY 'NAV ' FUNCTION TRIM(WS-DISP-NAV) ' - ABOUT '
                   FUNCTION TRIM(WS-DISP-UNITS)
                   ' UNITS. CONFIRM (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           CLOSE MFS-FILE
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE MFU-PARMS
           SET MU-BUY TO TRUE
           MOVE WS-IN-ACCT TO MU-ACCT-NO
           MOVE MS-CODE    TO MU-CODE
           MOVE 'P'        TO MU-SOURCE
           MOVE WS-NUM     TO MU-AMOUNT
           MOVE WS-CURR-DATE TO MU-RUN-DATE
           MOVE 'COUNTER'  TO MU-REF
           CALL 'MFUTIL' USING MFU-PARMS
           IF MU-OK
               MOVE MU-UNITS TO WS-DISP-UNITS
               MOVE MU-NAV   TO WS-DISP-NAV
               DISPLAY FUNCTION TRIM(WS-DISP-UNITS) ' UNITS OF '
                       MU-CODE ' ALLOTTED AT NAV '
                       FUNCTION TRIM(WS-DISP-NAV) '.'
           ELSE
               PERFORM 8300-SHOW-MFU-ERROR
           END-IF.

      *----------------------------------------------------------------*
      * 3. REDEEM                                                      *
      *----------------------------------------------------------------*
       3000-REDEEM.
           DISPLAY 'ACCOUNT NO           : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT
           DISPLAY 'FUND CODE            : ' WITH NO ADVANCING
           ACCEPT WS-IN-CODE
           MOVE FUNCTION UPPER-CASE(WS-IN-CODE) TO WS-IN-CODE
           DISPLAY 'UNITS (BLANK = ALL)  : ' WITH NO ADVANCING
           MOVE 'Y' TO WS-OK-SW
           PERFORM 8100-ACCEPT-NUMBER
           IF NOT CHECKS-OK
               EXIT PARAGRAPH
           END-IF
           INITIALIZE MFU-PARMS
           SET MU-REDEEM TO TRUE
           MOVE WS-IN-ACCT TO MU-ACCT-NO
           MOVE WS-IN-CODE TO MU-CODE
           MOVE WS-NUM     TO MU-UNITS
           MOVE WS-CURR-DATE TO MU-RUN-DATE
           MOVE 'COUNTER'  TO MU-REF
           CALL 'MFUTIL' USING MFU-PARMS
           IF MU-OK
               MOVE MU-UNITS TO WS-DISP-UNITS
               MOVE MU-VALUE TO WS-DISP-AMT
               MOVE MU-LOAD  TO WS-DISP-AMT2
               DISPLAY FUNCTION TRIM(WS-DISP-UNITS) ' UNITS REDEEMED, '
                       'VALUE ' FUNCTION TRIM(WS-DISP-AMT)
                       ' EXIT LOAD ' FUNCTION TRIM(WS-DISP-AMT2)
               MOVE MU-NET TO WS-DISP-AMT
               DISPLAY 'NET CREDITED TO ' WS-IN-ACCT ': '
                       FUNCTION TRIM(WS-DISP-AMT)
           ELSE
               PERFORM 8300-SHOW-MFU-ERROR
           END-IF.

      *----------------------------------------------------------------*
      * 4. SIP                                                         *
      *----------------------------------------------------------------*
       4000-SIP-REGISTER.
           PERFORM 8200-ASK-ACCT-AND-PRODUCT
           IF NOT CHECKS-OK
               EXIT PARAGRAPH
           END-IF
           CLOSE MFS-FILE
           IF NOT MS-MUTUAL-FUND
               DISPLAY 'A SIP IS ONLY FOR FUNDS.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'MONTHLY AMOUNT       : ' WITH NO ADVANCING
           PERFORM 8100-ACCEPT-NUMBER
           MOVE WS-NUM TO WS-PREMIUM
           DISPLAY 'DAY OF MONTH (1-28)  : ' WITH NO ADVANCING
           PERFORM 8100-ACCEPT-NUMBER
           MOVE WS-NUM TO WS-SIP-DAY
           DISPLAY 'NUMBER OF MONTHS     : ' WITH NO ADVANCING
           PERFORM 8100-ACCEPT-NUMBER
           MOVE WS-NUM TO WS-SIP-MONTHS
           EVALUATE TRUE
               WHEN NOT CHECKS-OK
                   EXIT PARAGRAPH
               WHEN WS-PREMIUM < WS-MIN-SIP
                   DISPLAY 'MINIMUM SIP IS 500 A MONTH.'
                   EXIT PARAGRAPH
               WHEN WS-SIP-DAY < 1 OR WS-SIP-DAY > 28
                   DISPLAY 'SIP DAY MUST BE 1 - 28.'
                   EXIT PARAGRAPH
               WHEN WS-SIP-MONTHS < 6 OR WS-SIP-MONTHS > 360
                   DISPLAY 'SIP MUST RUN 6 - 360 MONTHS.'
                   EXIT PARAGRAPH
           END-EVALUATE
      *    FIRST INSTALMENT: THE SIP DAY OF THIS MONTH IF STILL AHEAD,
      *    ELSE OF NEXT MONTH
           COMPUTE WS-NEXT-SIP = WS-CURR-YYYYMM * 100 + WS-SIP-DAY
           IF WS-NEXT-SIP <= WS-CURR-DATE
               SET DU-ADD-MONTHS TO TRUE
               MOVE WS-NEXT-SIP TO DU-DATE-1
               MOVE 1 TO DU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DU-RESULT-DATE TO WS-NEXT-SIP
           END-IF
           PERFORM 8400-OPEN-SIPS
           PERFORM 8500-NEXT-SIP-NO
           INITIALIZE SIP-RECORD
           MOVE WS-IN-SIP    TO SP-NO
           MOVE WS-IN-ACCT   TO SP-ACCT-NO
           MOVE MS-CODE      TO SP-CODE
           MOVE WS-PREMIUM   TO SP-AMOUNT
           MOVE WS-SIP-DAY   TO SP-DAY
           MOVE WS-NEXT-SIP  TO SP-NEXT-DATE
           SET SP-ACTIVE     TO TRUE
           MOVE WS-CURR-DATE TO SP-REG-DATE
           SET DU-ADD-MONTHS TO TRUE
           MOVE WS-NEXT-SIP  TO DU-DATE-1
           COMPUTE DU-NUMBER = WS-SIP-MONTHS - 1
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO SP-END-DATE
           WRITE SIP-RECORD
           CLOSE SIP-FILE
           MOVE 'SIP NEW' TO AU-ACTION
           MOVE SP-NO     TO AU-KEY
           MOVE SPACES    TO AU-DETAIL
           STRING WS-IN-ACCT ' ' MS-CODE DELIMITED BY SIZE
                  INTO AU-DETAIL
           PERFORM 8900-AUDIT
           DISPLAY 'SIP ' SP-NO ' REGISTERED - FIRST ON ' SP-NEXT-DATE
                   ', LAST ON ' SP-END-DATE '.'.

       5000-SIP-CANCEL.
           DISPLAY 'SIP NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-SIP
           PERFORM 8400-OPEN-SIPS
           MOVE WS-IN-SIP TO SP-NO
           READ SIP-FILE
               INVALID KEY
                   DISPLAY 'SIP ' WS-IN-SIP ' NOT FOUND.'
                   CLOSE SIP-FILE
                   EXIT PARAGRAPH
           END-READ
           IF NOT SP-ACTIVE
               DISPLAY 'SIP IS NOT ACTIVE (STATUS ' SP-STATUS ').'
           ELSE
               SET SP-CANCELLED TO TRUE
               REWRITE SIP-RECORD
               MOVE 'SIP CANCEL' TO AU-ACTION
               MOVE SP-NO TO AU-KEY
               MOVE SPACES TO AU-DETAIL
               PERFORM 8900-AUDIT
               DISPLAY 'SIP ' SP-NO ' CANCELLED AFTER ' SP-PAID-CNT
                       ' INSTALMENT(S).'
           END-IF
           CLOSE SIP-FILE.

      *----------------------------------------------------------------*
      * 6. PORTFOLIO                                                   *
      *----------------------------------------------------------------*
       6000-PORTFOLIO.
           DISPLAY 'ACCOUNT NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT
           PERFORM 8000-OPEN-PRODUCTS
           OPEN INPUT MFH-FILE
           IF WS-MFH-FS NOT = '00'
               DISPLAY 'NO HOLDINGS ON FILE.'
               CLOSE MFS-FILE
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO WS-CNT WS-TOT-VALUE WS-TOT-COST
           MOVE WS-IN-ACCT TO MH-ACCT-NO
           MOVE LOW-VALUES TO MH-CODE
           START MFH-FILE KEY >= MH-KEY
               INVALID KEY MOVE 'Y' TO WS-EOF-SW
               NOT INVALID KEY MOVE 'N' TO WS-EOF-SW
           END-START
           DISPLAY 'PORTFOLIO OF ' WS-IN-ACCT
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ MFH-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N'
                   IF MH-ACCT-NO NOT = WS-IN-ACCT
                       MOVE 'Y' TO WS-EOF-SW
                   ELSE
                       IF NOT MH-CLOSED
                           ADD 1 TO WS-CNT
                           PERFORM 6100-HOLDING-LINE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           CLOSE MFH-FILE MFS-FILE
           IF WS-CNT = ZERO
               DISPLAY '  NO LIVE HOLDINGS.'
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-GAIN = WS-TOT-VALUE - WS-TOT-COST
           MOVE WS-TOT-COST  TO WS-DISP-AMT
           MOVE WS-TOT-VALUE TO WS-DISP-AMT2
           MOVE WS-GAIN      TO WS-DISP-GAIN
           DISPLAY '  FUNDS: COST ' FUNCTION TRIM(WS-DISP-AMT)
                   '  VALUE ' FUNCTION TRIM(WS-DISP-AMT2)
                   '  GAIN ' FUNCTION TRIM(WS-DISP-GAIN).

       6100-HOLDING-LINE.
           MOVE MH-CODE TO MS-CODE
           READ MFS-FILE
               INVALID KEY MOVE ZERO TO MS-NAV
           END-READ
           IF MS-INSURANCE
               MOVE MH-SUM-ASSURED TO WS-DISP-AMT
               MOVE MH-PREMIUM     TO WS-DISP-AMT2
               DISPLAY '  ' MH-CODE ' POLICY ' MH-POLICY-NO ' COVER '
                       FUNCTION TRIM(WS-DISP-AMT) ' PREMIUM '
                       FUNCTION TRIM(WS-DISP-AMT2) '/' MH-FREQUENCY
                       ' NEXT ' MH-NEXT-DUE ' STATUS ' MH-STATUS
           ELSE
               COMPUTE WS-VALUE ROUNDED = MH-UNITS * MS-NAV
               ADD WS-VALUE TO WS-TOT-VALUE
               ADD MH-COST  TO WS-TOT-COST
               MOVE MH-UNITS TO WS-DISP-UNITS
               MOVE WS-VALUE TO WS-DISP-AMT
               DISPLAY '  ' MH-CODE ' ' MS-NAME(1:20) ' UNITS '
                       FUNCTION TRIM(WS-DISP-UNITS) ' VALUE '
                       FUNCTION TRIM(WS-DISP-AMT)
           END-IF.

      *----------------------------------------------------------------*
      * 7. INSURANCE POLICY - ONE POLICY PER PLAN PER ACCOUNT          *
      *----------------------------------------------------------------*
       7000-POLICY.
           PERFORM 8200-ASK-ACCT-AND-PRODUCT
           IF NOT CHECKS-OK
               EXIT PARAGRAPH
           END-IF
           IF NOT MS-INSURANCE
               DISPLAY MS-CODE ' IS NOT AN INSURANCE PLAN.'
               CLOSE MFS-FILE
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'SUM ASSURED          : ' WITH NO ADVANCING
           PERFORM 8100-ACCEPT-NUMBER
           MOVE WS-NUM TO WS-SUM-ASSURED
           DISPLAY 'PREMIUM              : ' WITH NO ADVANCING
           PERFORM 8100-ACCEPT-NUMBER
           MOVE WS-NUM TO WS-PREMIUM
           DISPLAY 'FREQUENCY Y/H/Q/M    : ' WITH NO ADVANCING
           ACCEPT WS-IN-X1
           MOVE FUNCTION UPPER-CASE(WS-IN-X1) TO WS-IN-X1
           MOVE MS-NAME TO WS-SAVE-NAME
           MOVE MS-MIN-AMT TO WS-VALUE
           CLOSE MFS-FILE
           EVALUATE TRUE
               WHEN NOT CHECKS-OK
                   EXIT PARAGRAPH
               WHEN WS-PREMIUM < WS-VALUE
                   MOVE WS-VALUE TO WS-DISP-AMT
                   DISPLAY 'MINIMUM PREMIUM IS '
                           FUNCTION TRIM(WS-DISP-AMT) '.'
                   EXIT PARAGRAPH
               WHEN WS-SUM-ASSURED < WS-PREMIUM * 10
                   DISPLAY 'SUM ASSURED MUST BE AT LEAST 10 x PREMIUM.'
                   EXIT PARAGRAPH
               WHEN WS-IN-X1 NOT = 'Y' AND WS-IN-X1 NOT = 'H'
                    AND WS-IN-X1 NOT = 'Q' AND WS-IN-X1 NOT = 'M'
                   DISPLAY 'FREQUENCY MUST BE Y, H, Q OR M.'
                   EXIT PARAGRAPH
           END-EVALUATE
      *    WRITE THE POLICY, THEN COLLECT THE FIRST PREMIUM THROUGH
      *    MFUTIL; IF THAT FAILS THE POLICY IS REMOVED AGAIN
           OPEN I-O MFH-FILE
           IF WS-MFH-FS = '35'
               OPEN OUTPUT MFH-FILE
               CLOSE MFH-FILE
               OPEN I-O MFH-FILE
           END-IF
           MOVE WS-IN-ACCT TO MH-ACCT-NO
           MOVE WS-IN-CODE TO MH-CODE
           READ MFH-FILE
               INVALID KEY CONTINUE
           END-READ
           IF WS-MFH-FS = '00' AND NOT MH-CLOSED
               DISPLAY 'ACCOUNT ALREADY HOLDS POLICY ' MH-POLICY-NO '.'
               CLOSE MFH-FILE
               EXIT PARAGRAPH
           END-IF
           INITIALIZE MFH-RECORD
           MOVE WS-IN-ACCT     TO MH-ACCT-NO
           MOVE WS-IN-CODE     TO MH-CODE
           SET MH-ACTIVE       TO TRUE
           MOVE WS-CURR-DATE   TO MH-FIRST-DATE
           STRING WS-IN-CODE(1:2) WS-CURR-DATE(3:6)
                  WS-IN-ACCT(7:4) DELIMITED BY SIZE INTO MH-POLICY-NO
           MOVE WS-SUM-ASSURED TO MH-SUM-ASSURED
           MOVE WS-PREMIUM     TO MH-PREMIUM
           MOVE WS-IN-X1       TO MH-FREQUENCY
           MOVE WS-CURR-DATE   TO MH-NEXT-DUE
           WRITE MFH-RECORD
               INVALID KEY REWRITE MFH-RECORD
           END-WRITE
           CLOSE MFH-FILE
           INITIALIZE MFU-PARMS
           SET MU-PREMIUM TO TRUE
           MOVE WS-IN-ACCT TO MU-ACCT-NO
           MOVE WS-IN-CODE TO MU-CODE
           MOVE WS-CURR-DATE TO MU-RUN-DATE
           MOVE 'FIRST PREM' TO MU-REF
           CALL 'MFUTIL' USING MFU-PARMS
           IF NOT MU-OK
               PERFORM 8300-SHOW-MFU-ERROR
               OPEN I-O MFH-FILE
               MOVE WS-IN-ACCT TO MH-ACCT-NO
               MOVE WS-IN-CODE TO MH-CODE
               READ MFH-FILE
                   NOT INVALID KEY DELETE MFH-FILE RECORD
               END-READ
               CLOSE MFH-FILE
               DISPLAY 'POLICY NOT ISSUED.'
               EXIT PARAGRAPH
           END-IF
           MOVE 'POLICY ISSUE' TO AU-ACTION
           MOVE WS-IN-ACCT TO AU-KEY
           MOVE WS-SAVE-NAME TO AU-DETAIL
           PERFORM 8900-AUDIT
           MOVE MU-NET TO WS-DISP-AMT
           MOVE MU-COMMISSION TO WS-DISP-AMT2
           DISPLAY 'POLICY ' FUNCTION TRIM(WS-IN-CODE) ' ISSUED, FIRST '
                   'PREMIUM ' FUNCTION TRIM(WS-DISP-AMT) ' COLLECTED '
                   '(COMMISSION ' FUNCTION TRIM(WS-DISP-AMT2) ').'.

      *----------------------------------------------------------------*
      * COMMON                                                         *
      *----------------------------------------------------------------*
       8000-OPEN-PRODUCTS.
           OPEN I-O MFS-FILE
           IF WS-MFS-FS = '35'
               OPEN OUTPUT MFS-FILE
               CLOSE MFS-FILE
               OPEN I-O MFS-FILE
           END-IF.

       8100-ACCEPT-NUMBER.
           MOVE ZERO TO WS-NUM
           ACCEPT WS-IN-NUM
           IF WS-IN-NUM = SPACES
               EXIT PARAGRAPH
           END-IF
           IF FUNCTION TEST-NUMVAL(WS-IN-NUM) = ZERO
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-IN-NUM)
           ELSE
               DISPLAY 'NOT A NUMBER: ' WS-IN-NUM
               MOVE 'N' TO WS-OK-SW
           END-IF.

      *    ACCOUNT + PRODUCT, KYC AND SUITABILITY. LEAVES MFS-FILE OPEN
      *    WITH THE PRODUCT IN THE BUFFER WHEN WS-OK-SW = 'Y'; OTHERWISE
      *    CLOSED. THE CALLER CLOSES IT.
       8200-ASK-ACCT-AND-PRODUCT.
           MOVE 'N' TO WS-OK-SW
           DISPLAY 'ACCOUNT NO           : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT
           DISPLAY 'PRODUCT CODE         : ' WITH NO ADVANCING
           ACCEPT WS-IN-CODE
           MOVE FUNCTION UPPER-CASE(WS-IN-CODE) TO WS-IN-CODE
           MOVE WS-IN-ACCT TO CLK-ACCT-NO
           MOVE 'N' TO CLK-CLOSE
           CALL 'CUSTLKUP' USING CLK-PARMS
           IF NOT CLK-OK
               DISPLAY 'NO CUSTOMER LINKED TO ACCOUNT ' WS-IN-ACCT '.'
               EXIT PARAGRAPH
           END-IF
           IF NOT CLK-KYC-VERIFIED
               DISPLAY 'KYC OF ' FUNCTION TRIM(CLK-NAME)
                       ' IS NOT VERIFIED - NO THIRD-PARTY PRODUCTS.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8000-OPEN-PRODUCTS
           MOVE WS-IN-CODE TO MS-CODE
           READ MFS-FILE
               INVALID KEY
                   DISPLAY 'PRODUCT ' WS-IN-CODE ' NOT FOUND.'
                   CLOSE MFS-FILE
                   EXIT PARAGRAPH
           END-READ
           IF NOT MS-OPEN
               DISPLAY 'PRODUCT ' WS-IN-CODE ' IS CLOSED TO NEW '
                       'BUSINESS.'
               CLOSE MFS-FILE
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-AGE = (WS-CURR-DATE - CLK-DOB) / 10000
           IF MS-HIGH-RISK AND WS-AGE >= WS-SENIOR-AGE
               DISPLAY 'HIGH-RISK PRODUCT FOR A CUSTOMER AGED ' WS-AGE
                       '. CUSTOMER ACKNOWLEDGES RISK (Y/N): '
                       WITH NO ADVANCING
               ACCEPT WS-CONFIRM
               IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
                   DISPLAY 'NOT SUITABLE - SALE STOPPED.'
                   CLOSE MFS-FILE
                   EXIT PARAGRAPH
               END-IF
               MOVE 'RISK ACK' TO AU-ACTION
               MOVE WS-IN-ACCT TO AU-KEY
               MOVE WS-IN-CODE TO AU-DETAIL
               PERFORM 8900-AUDIT
           END-IF
           MOVE 'Y' TO WS-OK-SW
           CLOSE MFS-FILE
           OPEN INPUT MFS-FILE
           READ MFS-FILE.

       8300-SHOW-MFU-ERROR.
           EVALUATE TRUE
               WHEN MU-NO-PRODUCT
                   DISPLAY 'PRODUCT NOT AVAILABLE.'
               WHEN MU-BELOW-MINIMUM
                   DISPLAY 'AMOUNT BELOW THE PRODUCT MINIMUM.'
               WHEN MU-NO-FUNDS
                   DISPLAY 'INSUFFICIENT BALANCE IN ' MU-ACCT-NO '.'
               WHEN MU-NO-HOLDING
                   DISPLAY 'NO SUCH HOLDING / NOT ENOUGH UNITS.'
               WHEN MU-POST-FAILED
                   DISPLAY 'ACCOUNT POSTING FAILED, RC=' MU-POST-RC
               WHEN OTHER
                   DISPLAY 'FAILED, RC=' MU-RC
           END-EVALUATE.

       8400-OPEN-SIPS.
           OPEN I-O SIP-FILE
           IF WS-SIP-FS = '35'
               OPEN OUTPUT SIP-FILE
               CLOSE SIP-FILE
               OPEN I-O SIP-FILE
           END-IF.

       8500-NEXT-SIP-NO.
           MOVE ZERO TO WS-IN-SIP
           MOVE 99999999 TO SP-NO
           START SIP-FILE KEY IS LESS THAN OR EQUAL SP-NO
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ SIP-FILE PREVIOUS RECORD
                       AT END CONTINUE
                       NOT AT END MOVE SP-NO TO WS-IN-SIP
                   END-READ
           END-START
           ADD 1 TO WS-IN-SIP.

       8900-AUDIT.
           MOVE 'MFMNT' TO AU-PROGRAM
           CALL 'AUDITLOG' USING AUD-PARMS.
