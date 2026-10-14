       IDENTIFICATION DIVISION.
       PROGRAM-ID. SWEEPMNT.
      *================================================================*
      * PROGRAM  : SWEEPMNT                                            *
      * PURPOSE  : AUTO-SWEEP MANDATES AND OVERDRAFT AGAINST FD.       *
      *            1 REGISTER SWEEP  - THRESHOLD / FLOOR / FD UNIT /   *
      *                                TENURE (RUN BY SWEEPBAT AT EOD) *
      *            2 VIEW SWEEP      - MANDATE, LIVE SWEEP FDS         *
      *            3 CANCEL SWEEP    - EXISTING SWEEP FDS RUN TO       *
      *                                MATURITY                        *
      *            4 OD AGAINST FD   - CURRENT ACCOUNT: LIMIT GOES UP  *
      *                                BY 90% OF THE FD, FD UNDER LIEN *
      *                                (OD RATE = FD RATE + 2%)        *
      *            5 RELEASE FD      - ONLY IF THE OD IN USE STILL     *
      *                                FITS IN THE REDUCED LIMIT       *
      * FILES    : SWPMAST (I-O), DEPMAST (I-O), ACCTMAST (I-O)        *
      * CALLS    : AUDITLOG                                            *
      * NOTE     : ACCTMAST IS UPDATED DIRECTLY (LIMIT, RATE ONLY - NO *
      *            MONEY MOVES), SO ACCTPOST IS NOT CALLED HERE.       *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SWP-FILE ASSIGN TO 'data/SWPMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SW-ACCT-NO
               FILE STATUS IS WS-SWP-FS.
           SELECT DEP-FILE ASSIGN TO 'data/DEPMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DEP-NO
               FILE STATUS IS WS-DEP-FS.
           SELECT ACCT-FILE ASSIGN TO 'data/ACCTMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-ACCT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  SWP-FILE.
           COPY SWPREC.
       FD  DEP-FILE.
           COPY DEPREC.
       FD  ACCT-FILE.
           COPY ACCTREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY SESSION.
           COPY AUDPARM.
       01  WS-SWP-FS                   PIC X(02).
       01  WS-DEP-FS                   PIC X(02).
       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.
       01  WS-CONFIRM                  PIC X(01).
       01  WS-EOF-SW                   PIC X(01).
       01  WS-IN-ACCT                  PIC 9(10).
       01  WS-IN-DEP                   PIC 9(10).
       01  WS-IN-AMT                   PIC X(15).
       01  WS-IN-TENURE                PIC X(03).
       01  WS-THRESHOLD                PIC 9(11)V99.
       01  WS-FLOOR                    PIC 9(11)V99.
       01  WS-UNIT                     PIC 9(11)V99.
       01  WS-TENURE                   PIC 9(03).
       01  WS-OK-SW                    PIC X(01).
           88  INPUT-OK                          VALUE 'Y'.
       01  WS-LIVE-CNT                 PIC 9(05).
       01  WS-LIVE-AMT                 PIC 9(13)V99.
       01  WS-OD-ADD                   PIC 9(11)V99.
       01  WS-NEW-LIMIT                PIC S9(11)V99.
       01  WS-NEW-RATE                 PIC 9(02)V9(04).
       01  WS-DISP-AMT                 PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-AMT2                PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-RATE                PIC Z9.99.

      * RULES
       01  WS-MIN-THRESHOLD            PIC 9(11)V99 VALUE 25000.00.
       01  WS-MIN-UNIT                 PIC 9(11)V99 VALUE 5000.00.
       01  WS-UNIT-MULTIPLE            PIC 9(05)    VALUE 1000.
       01  WS-OD-MARGIN-PCT            PIC 9(03)    VALUE 90.
       01  WS-OD-SPREAD                PIC 9V9(04)  VALUE 2.0000.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           OPEN I-O SWP-FILE
           IF WS-SWP-FS = '35'
               OPEN OUTPUT SWP-FILE
               CLOSE SWP-FILE
               OPEN I-O SWP-FILE
           END-IF
           OPEN I-O DEP-FILE
           IF WS-DEP-FS = '35'
               OPEN OUTPUT DEP-FILE
               CLOSE DEP-FILE
               OPEN I-O DEP-FILE
           END-IF
           OPEN I-O ACCT-FILE
           IF WS-SWP-FS NOT = '00' OR WS-DEP-FS NOT = '00'
              OR NOT ACCT-FS-OK
               DISPLAY 'SWEEPMNT: FILE OPEN FAILED ' WS-SWP-FS ' '
                       WS-DEP-FS ' ' WS-ACCT-FS
               GOBACK
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE = '9'
               DISPLAY ' '
               DISPLAY '------ SWEEP AND OVERDRAFT AGAINST FD ------'
               DISPLAY ' 1. REGISTER AUTO-SWEEP'
               DISPLAY ' 2. VIEW SWEEP'
               DISPLAY ' 3. CANCEL SWEEP'
               DISPLAY ' 4. OVERDRAFT AGAINST FD'
               DISPLAY ' 5. RELEASE FD FROM OVERDRAFT'
               DISPLAY ' 9. BACK'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 1000-REGISTER
                   WHEN '2' PERFORM 2000-VIEW
                   WHEN '3' PERFORM 3000-CANCEL
                   WHEN '4' PERFORM 4000-OD-SANCTION
                   WHEN '5' PERFORM 5000-OD-RELEASE
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
           CLOSE SWP-FILE DEP-FILE ACCT-FILE
           GOBACK.

      *----------------------------------------------------------------*
      * 1. REGISTER                                                    *
      *----------------------------------------------------------------*
       1000-REGISTER.
           DISPLAY 'ACCOUNT NO               : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT
           PERFORM 8000-READ-ACCOUNT
           IF NOT ACCT-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF NOT ACCT-ACTIVE
               DISPLAY 'ACCOUNT IS NOT ACTIVE.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-ACCT TO SW-ACCT-NO
           READ SWP-FILE
               INVALID KEY CONTINUE
           END-READ
           IF WS-SWP-FS = '00' AND SW-ACTIVE
               DISPLAY 'ACCOUNT ALREADY HAS AN ACTIVE SWEEP.'
               EXIT PARAGRAPH
           END-IF
           MOVE 'Y' TO WS-OK-SW
           DISPLAY 'SWEEP OUT ABOVE (THRESHOLD): ' WITH NO ADVANCING
           ACCEPT WS-IN-AMT
           PERFORM 8100-AMOUNT
           MOVE WS-OD-ADD TO WS-THRESHOLD
           DISPLAY 'SWEEP IN BELOW (FLOOR)     : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMT
           PERFORM 8100-AMOUNT
           MOVE WS-OD-ADD TO WS-FLOOR
           DISPLAY 'FD UNIT                    : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMT
           PERFORM 8100-AMOUNT
           MOVE WS-OD-ADD TO WS-UNIT
           DISPLAY 'FD TENURE MONTHS (6-60)    : ' WITH NO ADVANCING
           ACCEPT WS-IN-TENURE
           IF FUNCTION TEST-NUMVAL(WS-IN-TENURE) = ZERO
               COMPUTE WS-TENURE = FUNCTION NUMVAL(WS-IN-TENURE)
           ELSE
               MOVE ZERO TO WS-TENURE
           END-IF
           IF NOT INPUT-OK
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN WS-THRESHOLD < WS-MIN-THRESHOLD
                   DISPLAY 'THRESHOLD MUST BE AT LEAST 25,000.'
               WHEN WS-FLOOR < ACCT-MIN-BALANCE
                   DISPLAY 'FLOOR CANNOT BE BELOW THE MINIMUM BALANCE.'
               WHEN WS-FLOOR >= WS-THRESHOLD
                   DISPLAY 'FLOOR MUST BE BELOW THE THRESHOLD.'
               WHEN WS-UNIT < WS-MIN-UNIT
                    OR FUNCTION MOD(WS-UNIT, WS-UNIT-MULTIPLE) NOT = 0
                   DISPLAY 'FD UNIT MUST BE 5,000 OR MORE IN MULTIPLES '
                           'OF 1,000.'
               WHEN WS-TENURE < 6 OR WS-TENURE > 60
                   DISPLAY 'SWEEP FD TENURE MUST BE 6-60 MONTHS.'
               WHEN OTHER
                   PERFORM 1100-WRITE-MANDATE
           END-EVALUATE.

       1100-WRITE-MANDATE.
           INITIALIZE SWP-RECORD
           MOVE WS-IN-ACCT   TO SW-ACCT-NO
           SET SW-ACTIVE     TO TRUE
           MOVE WS-THRESHOLD TO SW-THRESHOLD
           MOVE WS-FLOOR     TO SW-FLOOR
           MOVE WS-UNIT      TO SW-UNIT
           MOVE WS-TENURE    TO SW-TENURE
           MOVE WS-CURR-DATE TO SW-CREATED-DATE
           MOVE SES-USER-ID  TO SW-CREATED-BY
      *    A CANCELLED MANDATE IS RE-USED (SAME KEY)
           WRITE SWP-RECORD
               INVALID KEY
                   REWRITE SWP-RECORD
           END-WRITE
           MOVE 'SWEEP ON' TO AU-ACTION
           MOVE WS-IN-ACCT TO AU-KEY
           MOVE WS-THRESHOLD TO WS-DISP-AMT
           MOVE WS-UNIT      TO WS-DISP-AMT2
           MOVE SPACES TO AU-DETAIL
           STRING 'ABOVE ' FUNCTION TRIM(WS-DISP-AMT) ' UNIT '
                  FUNCTION TRIM(WS-DISP-AMT2)
                  DELIMITED BY SIZE INTO AU-DETAIL
           PERFORM 8900-AUDIT
           DISPLAY 'AUTO-SWEEP REGISTERED FOR ' WS-IN-ACCT
                   ' - ABOVE ' FUNCTION TRIM(WS-DISP-AMT) ' IN FDS OF '
                   FUNCTION TRIM(WS-DISP-AMT2) '.'.

      *----------------------------------------------------------------*
      * 2. VIEW                                                        *
      *----------------------------------------------------------------*
       2000-VIEW.
           DISPLAY 'ACCOUNT NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT
           MOVE WS-IN-ACCT TO SW-ACCT-NO
           READ SWP-FILE
               INVALID KEY
                   DISPLAY 'NO SWEEP MANDATE ON ' WS-IN-ACCT '.'
                   EXIT PARAGRAPH
           END-READ
           MOVE SW-THRESHOLD TO WS-DISP-AMT
           MOVE SW-FLOOR     TO WS-DISP-AMT2
           DISPLAY 'SWEEP ' WS-IN-ACCT '  STATUS ' SW-STATUS
                   '  SINCE ' SW-CREATED-DATE
           DISPLAY '  OUT ABOVE ' WS-DISP-AMT '  IN BELOW '
                   WS-DISP-AMT2
           MOVE SW-UNIT TO WS-DISP-AMT
           DISPLAY '  FD UNIT   ' WS-DISP-AMT '  TENURE ' SW-TENURE
                   ' MONTHS'
           MOVE SW-OUT-AMOUNT TO WS-DISP-AMT
           MOVE SW-IN-AMOUNT  TO WS-DISP-AMT2
           DISPLAY '  SWEPT OUT ' SW-OUT-COUNT ' / ' WS-DISP-AMT
                   '  SWEPT IN ' SW-IN-COUNT ' / ' WS-DISP-AMT2
           MOVE ZERO TO WS-LIVE-CNT WS-LIVE-AMT
           MOVE ZERO TO DEP-NO
           START DEP-FILE KEY IS NOT LESS THAN DEP-NO
               INVALID KEY MOVE 'Y' TO WS-EOF-SW
               NOT INVALID KEY MOVE 'N' TO WS-EOF-SW
           END-START
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ DEP-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N' AND DEP-ACCT-NO = WS-IN-ACCT
                  AND DEP-IS-SWEEP AND DEP-ACTIVE
                   ADD 1 TO WS-LIVE-CNT
                   ADD DEP-AMOUNT TO WS-LIVE-AMT
                   MOVE DEP-AMOUNT TO WS-DISP-AMT
                   MOVE DEP-RATE   TO WS-DISP-RATE
                   DISPLAY '    FD ' DEP-NO ' ' WS-DISP-AMT ' @ '
                           WS-DISP-RATE '% MATURES '
                           DEP-MATURITY-DATE
               END-IF
           END-PERFORM
           MOVE WS-LIVE-AMT TO WS-DISP-AMT
           DISPLAY '  LIVE SWEEP FDS: ' WS-LIVE-CNT '  TOTAL '
                   WS-DISP-AMT.

      *----------------------------------------------------------------*
      * 3. CANCEL                                                      *
      *----------------------------------------------------------------*
       3000-CANCEL.
           DISPLAY 'ACCOUNT NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT
           MOVE WS-IN-ACCT TO SW-ACCT-NO
           READ SWP-FILE
               INVALID KEY
                   DISPLAY 'NO SWEEP MANDATE ON ' WS-IN-ACCT '.'
                   EXIT PARAGRAPH
           END-READ
           IF NOT SW-ACTIVE
               DISPLAY 'SWEEP IS ALREADY CANCELLED.'
               EXIT PARAGRAPH
           END-IF
           SET SW-CANCELLED TO TRUE
           REWRITE SWP-RECORD
           MOVE 'SWEEP OFF' TO AU-ACTION
           MOVE WS-IN-ACCT  TO AU-KEY
           MOVE SPACES      TO AU-DETAIL
           PERFORM 8900-AUDIT
           DISPLAY 'SWEEP CANCELLED - EXISTING SWEEP FDS RUN TO '
                   'MATURITY.'.

      *----------------------------------------------------------------*
      * 4. OVERDRAFT AGAINST FD                                        *
      *----------------------------------------------------------------*
       4000-OD-SANCTION.
           DISPLAY 'CURRENT ACCOUNT NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT
           DISPLAY 'FD NO TO PLEDGE   : ' WITH NO ADVANCING
           ACCEPT WS-IN-DEP
           PERFORM 8000-READ-ACCOUNT
           IF NOT ACCT-FS-OK
               EXIT PARAGRAPH
           END-IF
           PERFORM 8200-READ-DEPOSIT
           IF WS-DEP-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN NOT ACCT-CURRENT
                   DISPLAY 'OVERDRAFT IS ONLY FOR CURRENT ACCOUNTS.'
               WHEN NOT ACCT-ACTIVE
                   DISPLAY 'ACCOUNT IS NOT ACTIVE.'
               WHEN NOT DEP-FD OR NOT DEP-ACTIVE
                   DISPLAY 'ONLY A LIVE FIXED DEPOSIT CAN BE PLEDGED.'
               WHEN DEP-ACCT-NO NOT = WS-IN-ACCT
                   DISPLAY 'FD BELONGS TO ANOTHER ACCOUNT ('
                           DEP-ACCT-NO ').'
               WHEN DEP-LIEN-LOAN-NO > ZERO
                   DISPLAY 'FD IS ALREADY UNDER LIEN ('
                           DEP-LIEN-LOAN-NO ').'
               WHEN DEP-IS-SWEEP
                   DISPLAY 'A SWEEP FD CANNOT BE PLEDGED.'
               WHEN OTHER
                   PERFORM 4100-APPLY-OD
           END-EVALUATE.

       4100-APPLY-OD.
           COMPUTE WS-OD-ADD =
                   FUNCTION INTEGER(DEP-AMOUNT * WS-OD-MARGIN-PCT
                                    / 100 / 100) * 100
           COMPUTE WS-NEW-RATE = DEP-RATE + WS-OD-SPREAD
           IF ACCT-OD-RATE > WS-NEW-RATE
               MOVE ACCT-OD-RATE TO WS-NEW-RATE
           END-IF
           MOVE WS-OD-ADD   TO WS-DISP-AMT
           MOVE WS-NEW-RATE TO WS-DISP-RATE
           DISPLAY 'OD LIMIT +' FUNCTION TRIM(WS-DISP-AMT)
                   ' AT ' WS-DISP-RATE '% P.A. CONFIRM (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
               EXIT PARAGRAPH
           END-IF
           ADD WS-OD-ADD TO ACCT-OD-LIMIT
           MOVE WS-NEW-RATE TO ACCT-OD-RATE
           REWRITE ACCT-RECORD
               INVALID KEY
                   DISPLAY 'ACCOUNT UPDATE FAILED FS=' WS-ACCT-FS
                   EXIT PARAGRAPH
           END-REWRITE
           MOVE WS-IN-ACCT TO DEP-LIEN-LOAN-NO
           REWRITE DEP-RECORD
           MOVE 'OD VS FD' TO AU-ACTION
           MOVE WS-IN-ACCT TO AU-KEY
           MOVE SPACES     TO AU-DETAIL
           STRING 'FD ' WS-IN-DEP ' +' FUNCTION TRIM(WS-DISP-AMT)
                  DELIMITED BY SIZE INTO AU-DETAIL
           PERFORM 8900-AUDIT
           MOVE ACCT-OD-LIMIT TO WS-DISP-AMT
           DISPLAY 'FD ' WS-IN-DEP ' UNDER LIEN. OD LIMIT NOW '
                   FUNCTION TRIM(WS-DISP-AMT) '.'.

      *----------------------------------------------------------------*
      * 5. RELEASE - THE OD IN USE MUST FIT IN THE REDUCED LIMIT       *
      *----------------------------------------------------------------*
       5000-OD-RELEASE.
           DISPLAY 'FD NO TO RELEASE  : ' WITH NO ADVANCING
           ACCEPT WS-IN-DEP
           PERFORM 8200-READ-DEPOSIT
           IF WS-DEP-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT DEP-LIEN-FOR-OD
               DISPLAY 'FD IS NOT PLEDGED FOR AN OVERDRAFT.'
               EXIT PARAGRAPH
           END-IF
           MOVE DEP-LIEN-LOAN-NO TO WS-IN-ACCT
           PERFORM 8000-READ-ACCOUNT
           IF NOT ACCT-FS-OK
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-OD-ADD =
                   FUNCTION INTEGER(DEP-AMOUNT * WS-OD-MARGIN-PCT
                                    / 100 / 100) * 100
           COMPUTE WS-NEW-LIMIT = ACCT-OD-LIMIT - WS-OD-ADD
           IF WS-NEW-LIMIT < ZERO
               MOVE ZERO TO WS-NEW-LIMIT
           END-IF
           IF ACCT-BALANCE + WS-NEW-LIMIT < ZERO
               COMPUTE WS-DISP-AMT = 0 - ACCT-BALANCE - WS-NEW-LIMIT
               DISPLAY 'OVERDRAFT IN USE EXCEEDS THE REDUCED LIMIT - '
                       'REPAY ' FUNCTION TRIM(WS-DISP-AMT) ' FIRST.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-NEW-LIMIT TO ACCT-OD-LIMIT
           REWRITE ACCT-RECORD
           MOVE ZERO TO DEP-LIEN-LOAN-NO
           REWRITE DEP-RECORD
           MOVE 'OD RELEASE' TO AU-ACTION
           MOVE WS-IN-ACCT   TO AU-KEY
           MOVE SPACES       TO AU-DETAIL
           STRING 'FD ' WS-IN-DEP DELIMITED BY SIZE INTO AU-DETAIL
           PERFORM 8900-AUDIT
           MOVE ACCT-OD-LIMIT TO WS-DISP-AMT
           DISPLAY 'FD ' WS-IN-DEP ' RELEASED. OD LIMIT NOW '
                   FUNCTION TRIM(WS-DISP-AMT) '.'.

      *----------------------------------------------------------------*
      * COMMON                                                         *
      *----------------------------------------------------------------*
       8000-READ-ACCOUNT.
           MOVE WS-IN-ACCT TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   DISPLAY 'ACCOUNT ' WS-IN-ACCT ' NOT FOUND.'
           END-READ.

      *    AMOUNT IN WS-IN-AMT -> WS-OD-ADD; BAD INPUT CLEARS WS-OK-SW
       8100-AMOUNT.
           IF FUNCTION TEST-NUMVAL(WS-IN-AMT) = ZERO
              AND WS-IN-AMT NOT = SPACES
               COMPUTE WS-OD-ADD = FUNCTION NUMVAL(WS-IN-AMT)
           ELSE
               DISPLAY 'AMOUNT MUST BE A NUMBER.'
               MOVE ZERO TO WS-OD-ADD
               MOVE 'N' TO WS-OK-SW
           END-IF.

       8200-READ-DEPOSIT.
           MOVE WS-IN-DEP TO DEP-NO
           READ DEP-FILE
               INVALID KEY
                   DISPLAY 'DEPOSIT ' WS-IN-DEP ' NOT FOUND.'
           END-READ.

       8900-AUDIT.
           MOVE 'SWEEPMNT' TO AU-PROGRAM
           CALL 'AUDITLOG' USING AUD-PARMS.
