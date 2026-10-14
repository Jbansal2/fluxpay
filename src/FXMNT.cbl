       IDENTIFICATION DIVISION.
       PROGRAM-ID. FXMNT.
      *================================================================*
      * PROGRAM  : FXMNT                                               *
      * PURPOSE  : FOREIGN EXCHANGE AND NRI REMITTANCES                *
      *              1 - CARD RATES                                    *
      *              2 - SET A CARD RATE                     (MANAGER) *
      *              3 - MARK AN ACCOUNT RESIDENT / NRE / NRO(MANAGER) *
      *              4 - INWARD REMITTANCE (CREDIT IN RUPEES)          *
      *              5 - OUTWARD REMITTANCE                            *
      *              6 - REMITTANCES OF AN ACCOUNT                     *
      *            RULES:                                              *
      *              INWARD  - CONVERTED AT THE BUY RATE.              *
      *              OUTWARD - CONVERTED AT THE SELL RATE.             *
      *                RESIDENT: LIBERALISED REMITTANCE SCHEME, USD    *
      *                2,50,000 PER FINANCIAL YEAR; TCS ON THE PART OF *
      *                THE YEAR'S REMITTANCES ABOVE RS 10 LAKH - 5% FOR*
      *                EDUCATION / MEDICAL (PURPOSE S0305 / S1108), 20%*
      *                OTHERWISE.                                      *
      *                NRO: REPATRIATION UP TO USD 10 LAKH PER YEAR.   *
      *                NRE: FREELY REPATRIABLE.                        *
      *              THE LIMITS AND TCS RATES ARE PARAMETERS BELOW;    *
      *              CHECK THEM AGAINST THE CURRENT FEMA / FINANCE ACT *
      *              BEFORE ANY REAL USE.                              *
      *            GL (MID = AVERAGE OF BUY AND SELL):                 *
      *              INWARD  DR 1305 NOSTRO  CR CUSTOMER (BUY)  +      *
      *                      CR 4104 EXCHANGE INCOME (MID - BUY)       *
      *              OUTWARD DR CUSTOMER (SELL) CR 1305 NOSTRO, THEN   *
      *                      DR 1305 CR 4104 (SELL - MID); TCS:        *
      *                      DR CUSTOMER CR 2503 TCS PAYABLE           *
      * FILES    : FXRATE (I-O), FXLOG (EXTEND / INPUT),               *
      *            ACCTMAST (I-O, RESIDENCY ONLY)                      *
      * CALLS    : ACCTPOST, GLPOST, AUDITLOG                          *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FX-FILE ASSIGN TO 'data/FXRATE.DAT'
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS FX-CCY FILE STATUS IS WS-FX-FS.
           SELECT LOG-FILE ASSIGN TO 'data/FXLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LOG-FS.
           SELECT ACCT-FILE ASSIGN TO 'data/ACCTMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS RANDOM
               RECORD KEY IS ACCT-NO FILE STATUS IS WS-ACCT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  FX-FILE.
           COPY FXREC.
       FD  LOG-FILE.
           COPY FXLOG.
       FD  ACCT-FILE.
           COPY ACCTREC.

       WORKING-STORAGE SECTION.
       01  WS-FX-FS                    PIC X(02).
       01  WS-LOG-FS                   PIC X(02).
       01  WS-ACCT-FS                  PIC X(02).
       01  WS-CHOICE                   PIC X(01).
       01  WS-EXIT-SW                  PIC X(01) VALUE 'N'.
           88  EXIT-REQUESTED                    VALUE 'Y'.
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-TODAY                    PIC 9(08).
       01  WS-TODAY-R REDEFINES WS-TODAY.
           05  WS-TODAY-YYYY           PIC 9(04).
           05  WS-TODAY-MM             PIC 9(02).
           05  WS-TODAY-DD             PIC 9(02).
       01  WS-FY                       PIC 9(04).
      * PARAMETERS (SEE HEADER)
       01  WS-LRS-LIMIT-USD            PIC 9(11)V99 VALUE 250000.
       01  WS-NRO-LIMIT-USD            PIC 9(11)V99 VALUE 1000000.
       01  WS-TCS-THRESHOLD            PIC 9(11)V99 VALUE 1000000.
       01  WS-TCS-RATE-LOW             PIC 9V99 VALUE 0.05.
       01  WS-TCS-RATE                 PIC 9V99 VALUE 0.20.
       01  WS-TCS-PCT                  PIC 9V99.
      * INPUT
       01  WS-IN-CCY                   PIC X(03).
       01  WS-IN-ACCT-NO               PIC 9(10).
       01  WS-IN-FC                    PIC 9(11)V99.
       01  WS-IN-PURPOSE               PIC X(05).
       01  WS-IN-PARTY                 PIC X(30).
       01  WS-IN-BUY                   PIC 9(03)V9(04).
       01  WS-IN-SELL                  PIC 9(03)V9(04).
       01  WS-IN-RESIDENCY             PIC X(01).
       01  WS-CONFIRM                  PIC X(01).
      * ARITHMETIC
       01  WS-MID                      PIC 9(03)V9(04).
       01  WS-INR                      PIC 9(13)V99.
       01  WS-INR-MID                  PIC 9(13)V99.
       01  WS-MARGIN-INCOME            PIC S9(13)V99.
       01  WS-USD                      PIC 9(11)V99.
       01  WS-YEAR-USD                 PIC 9(13)V99.
       01  WS-YEAR-INR                 PIC 9(13)V99.
       01  WS-TAXABLE                  PIC S9(13)V99.
       01  WS-TCS                      PIC 9(11)V99.
       01  WS-COUNT                    PIC 9(04).
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-AMT2                     PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-RATE                     PIC ZZ9.9999.
       01  ED-RATE2                    PIC ZZ9.9999.
           COPY FXDFLT.
           COPY POSTPARM.
           COPY GLPARM.
           COPY GLCODES.
           COPY SESSION.
           COPY AUDPARM.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
      *    INDIAN FINANCIAL YEAR STARTS IN APRIL
           MOVE WS-TODAY-YYYY TO WS-FY
           IF WS-TODAY-MM < 4
               SUBTRACT 1 FROM WS-FY
           END-IF
           PERFORM 1000-OPEN-OR-SEED
           IF WS-FX-FS NOT = '00'
               GOBACK
           END-IF
           MOVE 'N' TO WS-EXIT-SW
           PERFORM UNTIL EXIT-REQUESTED
               DISPLAY ' '
               DISPLAY '--------- FOREX AND NRI REMITTANCES ---------'
               DISPLAY ' 1. CARD RATES'
               IF SES-CAN-APPROVE
                   DISPLAY ' 2. SET A CARD RATE'
                   DISPLAY ' 3. MARK ACCOUNT RESIDENT / NRE / NRO'
               END-IF
               DISPLAY ' 4. INWARD REMITTANCE'
               DISPLAY ' 5. OUTWARD REMITTANCE'
               DISPLAY ' 6. REMITTANCES OF AN ACCOUNT'
               DISPLAY ' 9. BACK'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               PERFORM 8990-GUARD-END-OF-INPUT
               IF (WS-CHOICE = '2' OR WS-CHOICE = '3')
                  AND NOT SES-CAN-APPROVE
                   DISPLAY 'NEEDS A MANAGER.'
                   MOVE SPACE TO WS-CHOICE
               END-IF
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-SHOW-RATES
                   WHEN '2' PERFORM 2100-SET-RATE
                   WHEN '3' PERFORM 3000-SET-RESIDENCY
                   WHEN '4' PERFORM 4000-INWARD
                   WHEN '5' PERFORM 5000-OUTWARD
                   WHEN '6' PERFORM 6000-HISTORY
                   WHEN '9' SET EXIT-REQUESTED TO TRUE
                   WHEN SPACE CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE FX-FILE
           GOBACK.

       1000-OPEN-OR-SEED.
           OPEN I-O FX-FILE
           IF WS-FX-FS = '35'
               OPEN OUTPUT FX-FILE
               PERFORM VARYING FXD-IDX FROM 1 BY 1 UNTIL FXD-IDX > 5
                   INITIALIZE FX-RECORD
                   MOVE FXD-CCY(FXD-IDX)     TO FX-CCY
                   MOVE FXD-NAME(FXD-IDX)    TO FX-NAME
                   MOVE FXD-BUY(FXD-IDX)     TO FX-BUY
                   MOVE FXD-SELL(FXD-IDX)    TO FX-SELL
                   MOVE FXD-PER-USD(FXD-IDX) TO FX-PER-USD
                   MOVE WS-TODAY             TO FX-SET-DATE
                   MOVE 'SYSTEM'             TO FX-SET-BY
                   WRITE FX-RECORD
               END-PERFORM
               CLOSE FX-FILE
               OPEN I-O FX-FILE
           END-IF
           IF WS-FX-FS NOT = '00'
               DISPLAY 'FOREX RATE FILE OPEN FAILED, FS=' WS-FX-FS
           END-IF.

      *================================================================*
      * 1 / 2 - RATES                                                  *
      *================================================================*
       2000-SHOW-RATES.
           DISPLAY 'CCY NAME                      BUY      SELL  SET ON'
           MOVE LOW-VALUES TO FX-CCY
           MOVE 'N' TO WS-EOF-SW
           START FX-FILE KEY IS NOT LESS THAN FX-CCY
               INVALID KEY SET END-OF-FILE TO TRUE
           END-START
           PERFORM UNTIL END-OF-FILE
               READ FX-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       MOVE FX-BUY  TO ED-RATE
                       MOVE FX-SELL TO ED-RATE2
                       DISPLAY FX-CCY ' ' FX-NAME ' ' ED-RATE ' '
                               ED-RATE2 '  ' FX-SET-DATE
               END-READ
           END-PERFORM.

       2100-SET-RATE.
           PERFORM 7000-READ-CCY
           IF WS-FX-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'NEW BUY RATE  : ' WITH NO ADVANCING
           ACCEPT WS-IN-BUY
           DISPLAY 'NEW SELL RATE : ' WITH NO ADVANCING
           ACCEPT WS-IN-SELL
           IF WS-IN-BUY = ZERO OR WS-IN-SELL NOT > WS-IN-BUY
               DISPLAY 'SELL MUST BE ABOVE BUY, BOTH ABOVE ZERO.'
               EXIT PARAGRAPH
           END-IF
      *    A MOVE OF MORE THAN 5% IN ONE GO IS ALMOST SURELY A TYPO
           IF WS-IN-BUY > FX-BUY * 1.05 OR WS-IN-BUY < FX-BUY * 0.95
               DISPLAY 'RATE MOVES MORE THAN 5% - REFUSED, CHECK INPUT.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-BUY   TO FX-BUY
           MOVE WS-IN-SELL  TO FX-SELL
           MOVE WS-TODAY    TO FX-SET-DATE
           MOVE SES-USER-ID TO FX-SET-BY
           REWRITE FX-RECORD
           DISPLAY 'RATE FOR ' FX-CCY ' UPDATED.'
           MOVE 'FXMNT'     TO AU-PROGRAM
           MOVE 'SET RATE'  TO AU-ACTION
           MOVE FX-CCY      TO AU-KEY
           MOVE WS-IN-BUY   TO ED-RATE
           MOVE WS-IN-SELL  TO ED-RATE2
           MOVE SPACES TO AU-DETAIL
           STRING ED-RATE ' / ' ED-RATE2 DELIMITED BY SIZE
               INTO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS.

      *================================================================*
      * 3 - RESIDENCY                                                  *
      *================================================================*
       3000-SET-RESIDENCY.
           DISPLAY 'ACCOUNT NO                 : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'R=RESIDENT  E=NRE  O=NRO   : ' WITH NO ADVANCING
           ACCEPT WS-IN-RESIDENCY
           MOVE FUNCTION UPPER-CASE(WS-IN-RESIDENCY) TO WS-IN-RESIDENCY
           IF WS-IN-RESIDENCY NOT = 'R' AND NOT = 'E' AND NOT = 'O'
               DISPLAY 'MUST BE R, E OR O.'
               EXIT PARAGRAPH
           END-IF
           OPEN I-O ACCT-FILE
           IF WS-ACCT-FS NOT = '00'
               DISPLAY 'ACCOUNT MASTER NOT AVAILABLE.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-ACCT-NO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   DISPLAY 'ACCOUNT NOT FOUND.'
               NOT INVALID KEY
                   MOVE WS-IN-RESIDENCY TO ACCT-RESIDENCY
                   REWRITE ACCT-RECORD
                   DISPLAY 'ACCOUNT ' ACCT-NO ' RESIDENCY SET TO '
                           WS-IN-RESIDENCY
                   MOVE 'FXMNT'     TO AU-PROGRAM
                   MOVE 'RESIDENCY' TO AU-ACTION
                   MOVE ACCT-NO     TO AU-KEY
                   MOVE WS-IN-RESIDENCY TO AU-DETAIL
                   CALL 'AUDITLOG' USING AUD-PARMS
           END-READ
           CLOSE ACCT-FILE.

      *================================================================*
      * 4 - INWARD                                                     *
      *================================================================*
       4000-INWARD.
           PERFORM 7000-READ-CCY
           IF WS-FX-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'CREDIT ACCOUNT       : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'FOREIGN AMOUNT       : ' WITH NO ADVANCING
           ACCEPT WS-IN-FC
           DISPLAY 'REMITTER             : ' WITH NO ADVANCING
           ACCEPT WS-IN-PARTY
           DISPLAY 'PURPOSE CODE (PXXXX) : ' WITH NO ADVANCING
           ACCEPT WS-IN-PURPOSE
           IF WS-IN-FC NOT > ZERO OR WS-IN-PARTY = SPACES
               DISPLAY 'AMOUNT AND REMITTER ARE REQUIRED.'
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-MID = (FX-BUY + FX-SELL) / 2
           COMPUTE WS-INR ROUNDED     = WS-IN-FC * FX-BUY
           COMPUTE WS-INR-MID ROUNDED = WS-IN-FC * WS-MID
           COMPUTE WS-MARGIN-INCOME   = WS-INR-MID - WS-INR
           MOVE WS-INR TO ED-AMT
           MOVE FX-BUY TO ED-RATE
           DISPLAY 'AT ' ED-RATE ' = RS ' ED-AMT '. CREDIT (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
               DISPLAY 'NOT CREDITED.'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           MOVE WS-INR        TO PP-AMOUNT
           MOVE 'XI'          TO PP-TRAN-TYPE
           MOVE SPACES        TO PP-NARRATION
           STRING 'FX IN ' FX-CCY ' ' WS-IN-PARTY
               DELIMITED BY SIZE INTO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'CREDIT FAILED, RC=' PP-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           IF WS-MARGIN-INCOME > ZERO
               MOVE GLC-NOSTRO          TO GP-DR-GL
               MOVE GLC-EXCHANGE-INCOME TO GP-CR-GL
               MOVE WS-MARGIN-INCOME    TO GP-AMOUNT
               MOVE 'EXCHANGE MARGIN - INWARD' TO GP-NARRATION
               PERFORM 7400-GL
           END-IF
           MOVE 'I' TO FXL-DIRECTION
           MOVE FX-BUY TO FXL-RATE
           MOVE ZERO TO WS-TCS
           PERFORM 7200-LOG
           DISPLAY 'INWARD REMITTANCE CREDITED: RS ' ED-AMT.

      *================================================================*
      * 5 - OUTWARD                                                    *
      *================================================================*
       5000-OUTWARD.
           PERFORM 7000-READ-CCY
           IF WS-FX-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'DEBIT ACCOUNT        : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'FOREIGN AMOUNT       : ' WITH NO ADVANCING
           ACCEPT WS-IN-FC
           DISPLAY 'BENEFICIARY ABROAD   : ' WITH NO ADVANCING
           ACCEPT WS-IN-PARTY
           DISPLAY 'PURPOSE CODE (SXXXX) : ' WITH NO ADVANCING
           ACCEPT WS-IN-PURPOSE
           MOVE FUNCTION UPPER-CASE(WS-IN-PURPOSE) TO WS-IN-PURPOSE
           IF WS-IN-FC NOT > ZERO OR WS-IN-PARTY = SPACES
               DISPLAY 'AMOUNT AND BENEFICIARY ARE REQUIRED.'
               EXIT PARAGRAPH
           END-IF
           OPEN INPUT ACCT-FILE
           MOVE WS-IN-ACCT-NO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY MOVE '23' TO WS-ACCT-FS
           END-READ
           CLOSE ACCT-FILE
           IF WS-ACCT-FS NOT = '00'
               DISPLAY 'ACCOUNT NOT FOUND.'
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-MID = (FX-BUY + FX-SELL) / 2
           COMPUTE WS-INR ROUNDED     = WS-IN-FC * FX-SELL
           COMPUTE WS-INR-MID ROUNDED = WS-IN-FC * WS-MID
           COMPUTE WS-MARGIN-INCOME   = WS-INR - WS-INR-MID
           COMPUTE WS-USD ROUNDED     = WS-IN-FC / FX-PER-USD
           PERFORM 5100-YEAR-TO-DATE
           MOVE ZERO TO WS-TCS
           EVALUATE TRUE
               WHEN ACCT-NRE
                   CONTINUE
               WHEN ACCT-NRO
                   IF WS-YEAR-USD + WS-USD > WS-NRO-LIMIT-USD
                       DISPLAY 'NRO REPATRIATION LIMIT OF USD 10 LAKH '
                               'FOR THE YEAR WOULD BE CROSSED.'
                       EXIT PARAGRAPH
                   END-IF
               WHEN OTHER
                   IF WS-YEAR-USD + WS-USD > WS-LRS-LIMIT-USD
                       MOVE WS-YEAR-USD TO ED-AMT
                       DISPLAY 'LRS LIMIT USD 2,50,000 EXCEEDED - USED '
                               'THIS YEAR: USD ' ED-AMT
                       EXIT PARAGRAPH
                   END-IF
                   PERFORM 5200-TCS
           END-EVALUATE
           MOVE WS-INR TO ED-AMT
           MOVE WS-TCS TO ED-AMT2
           MOVE FX-SELL TO ED-RATE
           DISPLAY 'AT ' ED-RATE ' = RS ' ED-AMT '  TCS ' ED-AMT2
           DISPLAY 'DEBIT AND REMIT (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
               DISPLAY 'NOT REMITTED.'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           MOVE WS-INR        TO PP-AMOUNT
           MOVE 'XO'          TO PP-TRAN-TYPE
           MOVE SPACES        TO PP-NARRATION
           STRING 'FX OUT ' FX-CCY ' ' WS-IN-PARTY
               DELIMITED BY SIZE INTO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'DEBIT FAILED, RC=' PP-RETURN-CODE
                       ' - NOT REMITTED.'
               EXIT PARAGRAPH
           END-IF
           IF WS-MARGIN-INCOME > ZERO
               MOVE GLC-NOSTRO          TO GP-DR-GL
               MOVE GLC-EXCHANGE-INCOME TO GP-CR-GL
               MOVE WS-MARGIN-INCOME    TO GP-AMOUNT
               MOVE 'EXCHANGE MARGIN - OUTWARD' TO GP-NARRATION
               PERFORM 7400-GL
           END-IF
           IF WS-TCS > ZERO
               INITIALIZE POST-PARMS
               SET PP-DEBIT TO TRUE
               MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
               MOVE WS-TCS        TO PP-AMOUNT
               MOVE 'CH'          TO PP-TRAN-TYPE
               MOVE GLC-TCS-PAYABLE TO PP-CONTRA-GL
               MOVE 'TCS ON FOREIGN REMITTANCE' TO PP-NARRATION
               MOVE 'Y'           TO PP-IGNORE-MIN-BAL
               CALL 'ACCTPOST' USING POST-PARMS
               IF NOT PP-OK
                   DISPLAY 'WARNING: TCS NOT COLLECTED, RC='
                           PP-RETURN-CODE ' - REFER TO OPERATIONS'
                   MOVE ZERO TO WS-TCS
               END-IF
           END-IF
           MOVE 'O' TO FXL-DIRECTION
           MOVE FX-SELL TO FXL-RATE
           PERFORM 7200-LOG
           DISPLAY 'OUTWARD REMITTANCE SENT: RS ' ED-AMT.

      *    THIS FINANCIAL YEAR'S OUTWARD REMITTANCES OF THE ACCOUNT
       5100-YEAR-TO-DATE.
           MOVE ZERO TO WS-YEAR-USD WS-YEAR-INR
           OPEN INPUT LOG-FILE
           IF WS-LOG-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ LOG-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF FXL-ACCT-NO = WS-IN-ACCT-NO
                          AND FXL-FY = WS-FY AND FXL-OUTWARD
                           ADD FXL-USD-AMOUNT TO WS-YEAR-USD
                           ADD FXL-INR-AMOUNT TO WS-YEAR-INR
                       END-IF
               END-READ
           END-PERFORM
           CLOSE LOG-FILE.

      *    TCS ONLY ON THE PART OF THE YEAR'S TOTAL ABOVE THE THRESHOLD
       5200-TCS.
           IF WS-IN-PURPOSE = 'S0305' OR WS-IN-PURPOSE = 'S1108'
               MOVE WS-TCS-RATE-LOW TO WS-TCS-PCT
           ELSE
               MOVE WS-TCS-RATE TO WS-TCS-PCT
           END-IF
           IF WS-YEAR-INR >= WS-TCS-THRESHOLD
               MOVE WS-INR TO WS-TAXABLE
           ELSE
               COMPUTE WS-TAXABLE =
                       WS-YEAR-INR + WS-INR - WS-TCS-THRESHOLD
           END-IF
           IF WS-TAXABLE > ZERO
               COMPUTE WS-TCS ROUNDED = WS-TAXABLE * WS-TCS-PCT
           END-IF.

      *================================================================*
      * 6 - HISTORY                                                    *
      *================================================================*
       6000-HISTORY.
           DISPLAY 'ACCOUNT NO : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           MOVE ZERO TO WS-COUNT
           OPEN INPUT LOG-FILE
           IF WS-LOG-FS NOT = '00'
               DISPLAY 'NO REMITTANCES YET.'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ LOG-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF FXL-ACCT-NO = WS-IN-ACCT-NO
                           ADD 1 TO WS-COUNT
                           MOVE FXL-FC-AMOUNT  TO ED-AMT
                           MOVE FXL-INR-AMOUNT TO ED-AMT2
                           DISPLAY FXL-DATE ' ' FXL-DIRECTION ' '
                                   FXL-CCY ED-AMT ' RS' ED-AMT2 ' '
                                   FXL-PURPOSE ' ' FXL-PARTY(1:20)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE LOG-FILE
           DISPLAY WS-COUNT ' REMITTANCE(S)'.

      *================================================================*
      * COMMON                                                         *
      *================================================================*
       7000-READ-CCY.
           DISPLAY 'CURRENCY (USD/EUR/GBP/AED/SGD): ' WITH NO ADVANCING
           ACCEPT WS-IN-CCY
           MOVE FUNCTION UPPER-CASE(WS-IN-CCY) TO FX-CCY
           READ FX-FILE
               INVALID KEY
                   DISPLAY 'CURRENCY NOT DEALT IN.'
                   MOVE '23' TO WS-FX-FS
           END-READ.

       7200-LOG.
           MOVE WS-TODAY      TO FXL-DATE
           MOVE WS-FY         TO FXL-FY
           MOVE WS-IN-ACCT-NO TO FXL-ACCT-NO
           MOVE FX-CCY        TO FXL-CCY
           MOVE WS-IN-FC      TO FXL-FC-AMOUNT
           MOVE WS-INR        TO FXL-INR-AMOUNT
           COMPUTE FXL-USD-AMOUNT ROUNDED = WS-IN-FC / FX-PER-USD
           MOVE WS-TCS        TO FXL-TCS
           MOVE WS-IN-PURPOSE TO FXL-PURPOSE
           MOVE WS-IN-PARTY   TO FXL-PARTY
           MOVE SES-USER-ID   TO FXL-USER
           OPEN EXTEND LOG-FILE
           IF WS-LOG-FS = '35'
               OPEN OUTPUT LOG-FILE
           END-IF
           WRITE FXL-RECORD
           CLOSE LOG-FILE
           MOVE 'FXMNT' TO AU-PROGRAM
           MOVE 'REMIT ' TO AU-ACTION
           MOVE FXL-DIRECTION TO AU-ACTION(7:1)
           MOVE WS-IN-ACCT-NO TO AU-KEY
           MOVE SPACES TO AU-DETAIL
           STRING FX-CCY ' ' WS-IN-FC ' ' WS-IN-PURPOSE
               DELIMITED BY SIZE INTO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS.

       7400-GL.
           MOVE SPACES TO GP-REF
           STRING 'FX' WS-IN-ACCT-NO DELIMITED BY SIZE INTO GP-REF
           CALL 'GLPOST' USING GL-PARMS
           IF NOT GP-OK
               DISPLAY 'GL POSTING FAILED, RC=' GP-RC
           END-IF.

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
