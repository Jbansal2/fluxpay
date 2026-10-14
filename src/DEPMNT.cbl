       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEPMNT.
      *================================================================*
      * PROGRAM  : DEPMNT                                              *
      * PURPOSE  : TERM DEPOSITS - OPEN FD / RD FUNDED FROM A SAVINGS  *
      *            OR CURRENT ACCOUNT, RD INSTALMENT PAYMENT, INQUIRY, *
      *            PREMATURE CLOSURE AND LIST BY ACCOUNT. NORMAL       *
      *            MATURITY PAYOUT IS DONE BY THE EOD BATCH (EODBATCH).*
      * FILES    : DEPMAST (INDEXED, I-O)                              *
      * CALLS    : ACCTPOST, DATEUTIL, DEPCALC                         *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      * 2026-10-14 PAYOUT THROUGH DEPPAY (TDS AND GL)                  *
      * 2026-10-14 RATE FROM THE VERSIONED CARD IN FORCE TODAY         *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DEP-FILE ASSIGN TO 'data/DEPMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DEP-NO
               FILE STATUS IS WS-DEP-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  DEP-FILE.
           COPY DEPREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY DATEPARM.
           COPY DEPCPARM.
           COPY DPPPARM.

       01  WS-DEP-FS                   PIC X(02).
           88  DEP-FS-OK                         VALUE '00'.
           88  DEP-FS-NOFILE                     VALUE '35'.

       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
           88  CHOICE-BACK                       VALUE '9'.
       01  WS-CONFIRM                  PIC X(01).
           88  CONFIRM-YES                       VALUES 'Y' 'y'.
       01  WS-VALID-SW                 PIC X(01).
           88  INPUT-VALID                       VALUE 'Y'.
           88  INPUT-INVALID                     VALUE 'N'.

       01  WS-IN-DEP-NO                PIC 9(10).
       01  WS-IN-ACCT-NO               PIC 9(10).
       01  WS-IN-AMOUNT                PIC 9(11)V99.
       01  WS-IN-TENURE                PIC 9(03).
       01  WS-IN-SENIOR                PIC X(01).
       01  WS-NEW-TYPE                 PIC X(01).

       01  WS-NEXT-DEP-NO              PIC 9(10).
       01  WS-DAYS-HELD                PIC S9(07).
       01  WS-MONTHS-HELD              PIC 9(03).
       01  WS-PENAL-RATE               PIC S9(02)V9(04).
       01  WS-PAYOUT                   PIC 9(11)V99.
       01  WS-LIST-CNT                 PIC 9(03).

      * LIMITS / RULES
       01  WS-FD-MIN                   PIC 9(11)V99 VALUE 5000.00.
       01  WS-RD-MIN                   PIC 9(11)V99 VALUE 500.00.
       01  WS-MIN-TENURE               PIC 9(03) VALUE 6.
       01  WS-MAX-TENURE               PIC 9(03) VALUE 120.
       01  WS-PENALTY-PCT              PIC 9V9(04) VALUE 1.0000.
       01  WS-NO-INT-DAYS              PIC 9(03) VALUE 7.

       01  WS-DISP-AMT                 PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-RATE                PIC Z9.99.
       01  WS-DISP-DAYS                PIC Z,ZZ9.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN I-O DEP-FILE
           IF DEP-FS-NOFILE
               OPEN OUTPUT DEP-FILE
               CLOSE DEP-FILE
               OPEN I-O DEP-FILE
           END-IF
           IF NOT DEP-FS-OK
               DISPLAY 'DEPOSIT MASTER OPEN FAILED, FS=' WS-DEP-FS
               GOBACK
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL CHOICE-BACK
               PERFORM 1000-SHOW-MENU
               EVALUATE WS-CHOICE
                   WHEN '1' MOVE 'F' TO WS-NEW-TYPE
                            PERFORM 2000-OPEN-DEPOSIT
                   WHEN '2' MOVE 'R' TO WS-NEW-TYPE
                            PERFORM 2000-OPEN-DEPOSIT
                   WHEN '3' PERFORM 3000-RD-INSTALMENT
                   WHEN '4' PERFORM 4000-INQUIRY
                   WHEN '5' PERFORM 5000-PREMATURE-CLOSE
                   WHEN '6' PERFORM 6000-LIST-BY-ACCOUNT
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE DEP-FILE
           GOBACK.

       1000-SHOW-MENU.
           DISPLAY ' '
           DISPLAY '------------- TERM DEPOSITS -------------'
           DISPLAY ' 1. OPEN FIXED DEPOSIT'
           DISPLAY ' 2. OPEN RECURRING DEPOSIT'
           DISPLAY ' 3. PAY RD INSTALMENT'
           DISPLAY ' 4. DEPOSIT INQUIRY'
           DISPLAY ' 5. PREMATURE CLOSURE'
           DISPLAY ' 6. LIST DEPOSITS OF AN ACCOUNT'
           DISPLAY ' 9. BACK TO MAIN MENU'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT.

      *----------------------------------------------------------------*
      * OPEN FD / RD                                                   *
      *----------------------------------------------------------------*
       2000-OPEN-DEPOSIT.
           SET INPUT-VALID TO TRUE
           DISPLAY 'FUNDING ACCOUNT NO  : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           IF WS-NEW-TYPE = 'F'
               DISPLAY 'FD AMOUNT           : ' WITH NO ADVANCING
           ELSE
               DISPLAY 'MONTHLY INSTALMENT  : ' WITH NO ADVANCING
           END-IF
           ACCEPT WS-IN-AMOUNT
           DISPLAY 'TENURE (MONTHS)     : ' WITH NO ADVANCING
           ACCEPT WS-IN-TENURE
           DISPLAY 'SENIOR CITIZEN (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-IN-SENIOR
           MOVE FUNCTION UPPER-CASE(WS-IN-SENIOR) TO WS-IN-SENIOR

           PERFORM 2100-VALIDATE
           IF INPUT-INVALID
               EXIT PARAGRAPH
           END-IF

           MOVE 'RT'          TO DC-FUNCTION
           MOVE WS-IN-TENURE  TO DC-TENURE
           MOVE WS-IN-SENIOR  TO DC-SENIOR
           MOVE ZERO          TO DC-AS-OF-DATE
           CALL 'DEPCALC' USING DEPC-PARMS
           IF DC-RC NOT = ZERO
               DISPLAY 'NO RATE ON THE CURRENT CARD FOR THIS TENURE.'
               EXIT PARAGRAPH
           END-IF
           IF DC-RATE-EFF-DATE > ZERO
               DISPLAY 'RATE CARD EFFECTIVE ' DC-RATE-EFF-DATE
           END-IF
           MOVE DC-RESULT-RATE TO DC-RATE
           MOVE WS-IN-AMOUNT  TO DC-AMOUNT
           MOVE WS-IN-TENURE  TO DC-MONTHS DC-COUNT
           IF WS-NEW-TYPE = 'F'
               MOVE 'FV' TO DC-FUNCTION
           ELSE
               MOVE 'RV' TO DC-FUNCTION
           END-IF
           CALL 'DEPCALC' USING DEPC-PARMS

           MOVE DC-RATE TO WS-DISP-RATE
           MOVE DC-RESULT-AMT TO WS-DISP-AMT
           DISPLAY 'RATE ' WS-DISP-RATE '% P.A.  MATURITY VALUE: '
                   WS-DISP-AMT
           DISPLAY 'CONFIRM (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF NOT CONFIRM-YES
               EXIT PARAGRAPH
           END-IF

           PERFORM 2200-GET-NEXT-DEP-NO
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE WS-IN-ACCT-NO  TO PP-ACCT-NO
           MOVE WS-IN-AMOUNT   TO PP-AMOUNT
           MOVE WS-NEXT-DEP-NO TO PP-REF-ACCT
           IF WS-NEW-TYPE = 'F'
               MOVE 'FO'              TO PP-TRAN-TYPE
               MOVE 'FIXED DEPOSIT OPENED' TO PP-NARRATION
           ELSE
               MOVE 'RI'              TO PP-TRAN-TYPE
               MOVE 'RD INSTALMENT 1' TO PP-NARRATION
           END-IF
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               PERFORM 8200-SHOW-POST-ERROR
               EXIT PARAGRAPH
           END-IF
           PERFORM 2300-WRITE-DEPOSIT.

       2100-VALIDATE.
           IF WS-IN-TENURE < WS-MIN-TENURE
              OR WS-IN-TENURE > WS-MAX-TENURE
               DISPLAY 'TENURE MUST BE 6 TO 120 MONTHS.'
               SET INPUT-INVALID TO TRUE
           END-IF
           IF WS-NEW-TYPE = 'F' AND WS-IN-AMOUNT < WS-FD-MIN
               DISPLAY 'MINIMUM FD AMOUNT IS 5,000.'
               SET INPUT-INVALID TO TRUE
           END-IF
           IF WS-NEW-TYPE = 'R'
               IF WS-IN-AMOUNT < WS-RD-MIN
                  OR FUNCTION MOD(WS-IN-AMOUNT, 100) NOT = ZERO
                   DISPLAY 'RD INSTALMENT: MIN 500, MULTIPLE OF 100.'
                   SET INPUT-INVALID TO TRUE
               END-IF
           END-IF
           IF WS-IN-SENIOR NOT = 'Y'
               MOVE 'N' TO WS-IN-SENIOR
           END-IF.

       2200-GET-NEXT-DEP-NO.
           MOVE 7000000000 TO WS-NEXT-DEP-NO
           MOVE 9999999999 TO DEP-NO
           START DEP-FILE KEY IS LESS THAN OR EQUAL DEP-NO
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ DEP-FILE PREVIOUS RECORD
                       AT END CONTINUE
                       NOT AT END MOVE DEP-NO TO WS-NEXT-DEP-NO
                   END-READ
           END-START
           ADD 1 TO WS-NEXT-DEP-NO.

       2300-WRITE-DEPOSIT.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           INITIALIZE DEP-RECORD
           MOVE WS-NEXT-DEP-NO  TO DEP-NO
           MOVE WS-IN-ACCT-NO   TO DEP-ACCT-NO
           MOVE WS-NEW-TYPE     TO DEP-TYPE
           SET DEP-ACTIVE       TO TRUE
           MOVE WS-IN-SENIOR    TO DEP-SENIOR-FLAG
           MOVE WS-IN-AMOUNT    TO DEP-AMOUNT DEP-TOTAL-DEPOSITED
           MOVE DC-RATE         TO DEP-RATE
           MOVE WS-IN-TENURE    TO DEP-TENURE
           MOVE DC-RESULT-AMT   TO DEP-MATURITY-AMT
           MOVE WS-CURR-DATE    TO DEP-OPEN-DATE
           MOVE WS-CURR-DATE    TO DU-DATE-1
           MOVE WS-IN-TENURE    TO DU-NUMBER
           SET DU-ADD-MONTHS    TO TRUE
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE  TO DEP-MATURITY-DATE
           IF DEP-RD
               MOVE 1 TO DEP-RD-PAID-CNT
               MOVE 1 TO DU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DU-RESULT-DATE TO DEP-RD-NEXT-DUE
           END-IF
           WRITE DEP-RECORD
               INVALID KEY
                   DISPLAY 'CRITICAL: DEPOSIT WRITE FAILED FS='
                           WS-DEP-FS ' - ACCOUNT ALREADY DEBITED'
               NOT INVALID KEY
                   DISPLAY 'DEPOSIT ' DEP-NO ' OPENED. MATURES ON '
                           DEP-MATURITY-DATE
           END-WRITE.

      *----------------------------------------------------------------*
      * RD INSTALMENT                                                  *
      *----------------------------------------------------------------*
       3000-RD-INSTALMENT.
           PERFORM 8000-READ-DEPOSIT
           IF NOT DEP-FS-OK
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN NOT DEP-RD
                   DISPLAY 'NOT A RECURRING DEPOSIT.'
                   EXIT PARAGRAPH
               WHEN NOT DEP-ACTIVE
                   DISPLAY 'DEPOSIT IS NOT ACTIVE.'
                   EXIT PARAGRAPH
               WHEN DEP-RD-PAID-CNT >= DEP-TENURE
                   DISPLAY 'ALL INSTALMENTS ALREADY PAID.'
                   EXIT PARAGRAPH
           END-EVALUATE

           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE DEP-ACCT-NO TO PP-ACCT-NO
           MOVE DEP-AMOUNT  TO PP-AMOUNT
           MOVE 'RI'        TO PP-TRAN-TYPE
           MOVE DEP-NO      TO PP-REF-ACCT
           MOVE 'RD INSTALMENT' TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               PERFORM 8200-SHOW-POST-ERROR
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO DEP-RD-PAID-CNT
           ADD DEP-AMOUNT TO DEP-TOTAL-DEPOSITED
           IF DEP-RD-MISSED-CNT > ZERO
               SUBTRACT 1 FROM DEP-RD-MISSED-CNT
           END-IF
           MOVE DEP-RD-NEXT-DUE TO DU-DATE-1
           MOVE 1               TO DU-NUMBER
           SET DU-ADD-MONTHS    TO TRUE
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE  TO DEP-RD-NEXT-DUE
           PERFORM 8100-REWRITE-DEPOSIT
           DISPLAY 'INSTALMENT ' DEP-RD-PAID-CNT ' OF ' DEP-TENURE
                   ' RECEIVED.'.

      *----------------------------------------------------------------*
      * INQUIRY                                                        *
      *----------------------------------------------------------------*
       4000-INQUIRY.
           PERFORM 8000-READ-DEPOSIT
           IF NOT DEP-FS-OK
               EXIT PARAGRAPH
           END-IF
           DISPLAY '----------------------------------------'
           DISPLAY 'DEPOSIT NO     : ' DEP-NO
           IF DEP-FD
               DISPLAY 'TYPE           : FIXED DEPOSIT'
               MOVE DEP-AMOUNT TO WS-DISP-AMT
               DISPLAY 'PRINCIPAL      : ' WS-DISP-AMT
           ELSE
               DISPLAY 'TYPE           : RECURRING DEPOSIT'
               MOVE DEP-AMOUNT TO WS-DISP-AMT
               DISPLAY 'INSTALMENT     : ' WS-DISP-AMT
               DISPLAY 'PAID           : ' DEP-RD-PAID-CNT ' OF '
                       DEP-TENURE
               IF DEP-ACTIVE AND DEP-RD-PAID-CNT < DEP-TENURE
                   DISPLAY 'NEXT DUE       : ' DEP-RD-NEXT-DUE
               END-IF
               IF DEP-RD-MISSED-CNT > ZERO
                   DISPLAY 'MISSED         : ' DEP-RD-MISSED-CNT
               END-IF
           END-IF
           DISPLAY 'FUNDING ACCT   : ' DEP-ACCT-NO
           MOVE DEP-RATE TO WS-DISP-RATE
           DISPLAY 'RATE (% P.A.)  : ' WS-DISP-RATE
           IF DEP-SENIOR
               DISPLAY '(INCL. SENIOR CITIZEN +0.50%)'
           END-IF
           DISPLAY 'OPENED / MATURE: ' DEP-OPEN-DATE ' / '
                   DEP-MATURITY-DATE
           MOVE DEP-TOTAL-DEPOSITED TO WS-DISP-AMT
           DISPLAY 'TOTAL DEPOSITED: ' WS-DISP-AMT
           MOVE DEP-MATURITY-AMT TO WS-DISP-AMT
           DISPLAY 'MATURITY VALUE : ' WS-DISP-AMT
           EVALUATE TRUE
               WHEN DEP-ACTIVE
                   DISPLAY 'STATUS         : ACTIVE'
               WHEN DEP-MATURED
                   MOVE DEP-PAYOUT-AMT TO WS-DISP-AMT
                   DISPLAY 'STATUS         : MATURED, PAID '
                           WS-DISP-AMT ' ON ' DEP-CLOSE-DATE
               WHEN DEP-PREMATURE
                   MOVE DEP-PAYOUT-AMT TO WS-DISP-AMT
                   DISPLAY 'STATUS         : CLOSED EARLY, PAID '
                           WS-DISP-AMT ' ON ' DEP-CLOSE-DATE
           END-EVALUATE
           DISPLAY '----------------------------------------'.

      *----------------------------------------------------------------*
      * PREMATURE CLOSURE - RATE FOR THE PERIOD ACTUALLY HELD, LESS    *
      * 1% PENALTY. NO INTEREST IF HELD UNDER 7 DAYS.                  *
      *----------------------------------------------------------------*
       5000-PREMATURE-CLOSE.
           PERFORM 8000-READ-DEPOSIT
           IF NOT DEP-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF NOT DEP-ACTIVE
               DISPLAY 'DEPOSIT IS NOT ACTIVE.'
               EXIT PARAGRAPH
           END-IF
           IF DEP-LIEN-LOAN-NO > ZERO
               IF DEP-LIEN-FOR-OD
                   DISPLAY 'DEPOSIT SECURES THE OVERDRAFT OF '
                           DEP-LIEN-LOAN-NO ' - RELEASE IT FIRST.'
               ELSE
                   DISPLAY 'DEPOSIT IS PLEDGED FOR LOAN '
                           DEP-LIEN-LOAN-NO ' - CLOSE THE LOAN FIRST.'
               END-IF
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE DEP-OPEN-DATE  TO DU-DATE-1
           MOVE WS-CURR-DATE   TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DAYS TO WS-DAYS-HELD
           COMPUTE WS-MONTHS-HELD = WS-DAYS-HELD / 30

           IF WS-DAYS-HELD < WS-NO-INT-DAYS OR WS-MONTHS-HELD = ZERO
               MOVE ZERO TO WS-PENAL-RATE
               MOVE DEP-TOTAL-DEPOSITED TO WS-PAYOUT
           ELSE
               MOVE 'RT'           TO DC-FUNCTION
               MOVE WS-MONTHS-HELD TO DC-TENURE
               MOVE DEP-SENIOR-FLAG TO DC-SENIOR
      *        THE CARD THAT WAS IN FORCE WHEN THE DEPOSIT WAS BOOKED
               MOVE DEP-OPEN-DATE  TO DC-AS-OF-DATE
               CALL 'DEPCALC' USING DEPC-PARMS
               COMPUTE WS-PENAL-RATE = DC-RESULT-RATE - WS-PENALTY-PCT
               IF WS-PENAL-RATE < ZERO
                   MOVE ZERO TO WS-PENAL-RATE
               END-IF
               MOVE WS-PENAL-RATE  TO DC-RATE
               MOVE DEP-AMOUNT     TO DC-AMOUNT
               MOVE WS-MONTHS-HELD TO DC-MONTHS
               MOVE DEP-RD-PAID-CNT TO DC-COUNT
               IF DEP-FD
                   MOVE 'FV' TO DC-FUNCTION
               ELSE
                   MOVE 'RV' TO DC-FUNCTION
               END-IF
               CALL 'DEPCALC' USING DEPC-PARMS
               MOVE DC-RESULT-AMT TO WS-PAYOUT
           END-IF

           MOVE WS-PENAL-RATE TO WS-DISP-RATE
           MOVE WS-DAYS-HELD  TO WS-DISP-DAYS
           DISPLAY 'HELD ' WS-DISP-DAYS ' DAYS. APPLIED RATE: '
                   WS-DISP-RATE '%'
           MOVE WS-PAYOUT TO WS-DISP-AMT
           DISPLAY 'PAYOUT: ' WS-DISP-AMT
           DISPLAY 'CONFIRM CLOSURE (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF NOT CONFIRM-YES
               EXIT PARAGRAPH
           END-IF

           MOVE WS-PAYOUT    TO DPP-GROSS
           MOVE 'DEPOSIT PREMATURE CLOSURE' TO DPP-NARRATION
           MOVE WS-CURR-DATE TO DPP-RUN-DATE
           CALL 'DEPPAY' USING DPP-PARMS DEP-RECORD
           IF NOT DPP-OK
               DISPLAY 'PAYOUT FAILED, RC=' DPP-RC
               EXIT PARAGRAPH
           END-IF
           SET DEP-PREMATURE TO TRUE
           MOVE WS-PAYOUT    TO DEP-PAYOUT-AMT
           MOVE WS-CURR-DATE TO DEP-CLOSE-DATE
           PERFORM 8100-REWRITE-DEPOSIT
           IF DPP-TDS > ZERO
               MOVE DPP-TDS TO WS-DISP-AMT
               DISPLAY 'TDS DEDUCTED : ' WS-DISP-AMT
           END-IF
           MOVE DPP-NET TO WS-DISP-AMT
           DISPLAY 'NET CREDITED : ' WS-DISP-AMT
           DISPLAY 'DEPOSIT CLOSED. AMOUNT CREDITED TO ' DEP-ACCT-NO.

      *----------------------------------------------------------------*
      * LIST ALL DEPOSITS FUNDED FROM ONE ACCOUNT (FULL SCAN)          *
      *----------------------------------------------------------------*
       6000-LIST-BY-ACCOUNT.
           DISPLAY 'ACCOUNT NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           MOVE ZERO TO WS-LIST-CNT DEP-NO
           START DEP-FILE KEY IS NOT LESS THAN DEP-NO
               INVALID KEY
                   DISPLAY 'NO DEPOSITS ON FILE.'
                   EXIT PARAGRAPH
           END-START
           DISPLAY 'DEPOSIT NO  TYPE STATUS      AMOUNT  MATURES'
           PERFORM FOREVER
               READ DEP-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF DEP-ACCT-NO = WS-IN-ACCT-NO
                   ADD 1 TO WS-LIST-CNT
                   MOVE DEP-AMOUNT TO WS-DISP-AMT
                   DISPLAY DEP-NO '  ' DEP-TYPE '    ' DEP-STATUS
                           '  ' WS-DISP-AMT '  ' DEP-MATURITY-DATE
               END-IF
           END-PERFORM
           IF WS-LIST-CNT = ZERO
               DISPLAY 'NO DEPOSITS FOR THIS ACCOUNT.'
           END-IF.

      *----------------------------------------------------------------*
      * COMMON                                                         *
      *----------------------------------------------------------------*
       8000-READ-DEPOSIT.
           DISPLAY 'DEPOSIT NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-DEP-NO
           MOVE WS-IN-DEP-NO TO DEP-NO
           READ DEP-FILE
               INVALID KEY
                   DISPLAY 'DEPOSIT ' WS-IN-DEP-NO ' NOT FOUND.'
           END-READ.

       8100-REWRITE-DEPOSIT.
           REWRITE DEP-RECORD
               INVALID KEY
                   DISPLAY 'CRITICAL: DEPOSIT REWRITE FAILED FS='
                           WS-DEP-FS
           END-REWRITE.

       8200-SHOW-POST-ERROR.
           EVALUATE TRUE
               WHEN PP-INSUFFICIENT
                   MOVE PP-AVAILABLE TO WS-DISP-AMT
                   DISPLAY 'INSUFFICIENT BALANCE. AVAILABLE: '
                           WS-DISP-AMT
               WHEN PP-NOT-FOUND
                   DISPLAY 'ACCOUNT NOT FOUND.'
               WHEN PP-NOT-ACTIVE
                   DISPLAY 'ACCOUNT IS NOT ACTIVE.'
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
