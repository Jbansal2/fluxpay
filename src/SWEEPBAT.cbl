       IDENTIFICATION DIVISION.
       PROGRAM-ID. SWEEPBAT.
      *================================================================*
      * PROGRAM  : SWEEPBAT                                            *
      * PURPOSE  : END-OF-DAY AUTO-SWEEP FOR EVERY ACTIVE MANDATE.     *
      *            SWEEP-OUT : BALANCE ABOVE THE THRESHOLD IS MOVED    *
      *                        INTO NEW FDS OF ONE UNIT EACH (AT MOST  *
      *                        20 A DAY PER ACCOUNT) AT THE RATE CARD  *
      *                        IN FORCE ON THE RUN DATE.               *
      *            SWEEP-IN  : BALANCE BELOW THE FLOOR IS REFILLED BY  *
      *                        BREAKING THE NEWEST SWEEP FDS FIRST     *
      *                        (LIFO). A SWEEP FD BROKEN EARLY EARNS   *
      *                        THE RATE FOR THE PERIOD HELD WITH NO    *
      *                        PENALTY; NO INTEREST UNDER 7 DAYS.      *
      *                        PAYOUT GOES THROUGH DEPPAY (TDS, GL).   *
      * INPUT    : RUN DATE YYYYMMDD (BLANK = TODAY)                   *
      * OUTPUT   : data/SWEEP_<DATE>.TXT                               *
      * FILES    : SWPMAST (I-O), DEPMAST (I-O)                        *
      * CALLS    : ACCTPOST, DEPCALC, DEPPAY, DATEUTIL, BATCHCTL       *
      * RC       : 0 OK, 4 SOME POSTING REFUSED, 12 FILE ERROR         *
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
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS SW-ACCT-NO
               FILE STATUS IS WS-SWP-FS.
           SELECT DEP-FILE ASSIGN TO 'data/DEPMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DEP-NO
               FILE STATUS IS WS-DEP-FS.
           SELECT RPT-FILE ASSIGN TO WS-RPT-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  SWP-FILE.
           COPY SWPREC.
       FD  DEP-FILE.
           COPY DEPREC.
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(100).

       WORKING-STORAGE SECTION.
           COPY POSTPARM.
           COPY DEPCPARM.
           COPY DPPPARM.
           COPY DATEPARM.
       01  WS-SWP-FS                   PIC X(02).
       01  WS-DEP-FS                   PIC X(02).
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-NAME                 PIC X(40).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-EOF-SW                   PIC X(01).
       01  WS-DEP-EOF                  PIC X(01).
       01  WS-RC                       PIC 9(02) VALUE ZERO.

       01  WS-BALANCE                  PIC S9(13)V99.
       01  WS-UNITS                    PIC 9(05).
       01  WS-MAX-UNITS-PER-DAY        PIC 9(05) VALUE 20.
       01  WS-U                        PIC 9(05).
       01  WS-NEXT-DEP-NO              PIC 9(10).
       01  WS-DAYS-HELD                PIC S9(07).
       01  WS-MONTHS-HELD              PIC 9(03).
       01  WS-PAYOUT                   PIC 9(11)V99.
       01  WS-NO-INT-DAYS              PIC 9(03) VALUE 7.

      * LIVE SWEEP FDS OF THE CURRENT ACCOUNT, ASCENDING DEP-NO
       01  WS-LIVE-CNT                 PIC 9(04).
       01  WS-LIVE-TABLE.
           05  WS-LIVE-DEP OCCURS 500 TIMES PIC 9(10).
       01  WS-L                        PIC 9(04).

       01  WS-TOTALS.
           05  WS-MANDATES             PIC 9(07).
           05  WS-OUT-FDS              PIC 9(07).
           05  WS-OUT-AMT              PIC 9(13)V99.
           05  WS-IN-FDS               PIC 9(07).
           05  WS-IN-AMT               PIC 9(13)V99.
           05  WS-REFUSED              PIC 9(07).
       01  WS-DISP-AMT                 PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-AMT2                PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-CNT                 PIC ZZZ,ZZ9.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'RUN DATE (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           IF WS-IN-DATE = SPACES OR WS-IN-DATE IS NOT NUMERIC
               ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           ELSE
               MOVE WS-IN-DATE TO WS-RUN-DATE
           END-IF
           INITIALIZE WS-TOTALS
           OPEN I-O SWP-FILE
           IF WS-SWP-FS = '35'
               DISPLAY 'SWEEPBAT: NO SWEEP MANDATES - NOTHING TO DO.'
               MOVE ZERO TO RETURN-CODE
               GOBACK
           END-IF
           OPEN I-O DEP-FILE
           IF WS-DEP-FS = '35'
               OPEN OUTPUT DEP-FILE
               CLOSE DEP-FILE
               OPEN I-O DEP-FILE
           END-IF
           IF WS-SWP-FS NOT = '00' OR WS-DEP-FS NOT = '00'
               DISPLAY 'SWEEPBAT: FILE OPEN FAILED ' WS-SWP-FS ' '
                       WS-DEP-FS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           STRING 'data/SWEEP_' WS-RUN-DATE '.TXT'
                  DELIMITED BY SIZE INTO WS-RPT-NAME
           OPEN OUTPUT RPT-FILE
           MOVE SPACES TO RPT-LINE
           STRING 'AUTO-SWEEP RUN ' WS-RUN-DATE
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           MOVE 'ACCOUNT     DIR  FD NO        AMOUNT          NOTE'
             TO RPT-LINE
           WRITE RPT-LINE

           CALL 'BATCHCTL' USING 'S'
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ SWP-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N' AND SW-ACTIVE
                  AND SW-LAST-RUN-DATE NOT = WS-RUN-DATE
                   ADD 1 TO WS-MANDATES
                   PERFORM 1000-ONE-MANDATE
                   MOVE WS-RUN-DATE TO SW-LAST-RUN-DATE
                   REWRITE SWP-RECORD
               END-IF
           END-PERFORM
           CALL 'BATCHCTL' USING 'E'

           PERFORM 9000-TOTALS
           CLOSE SWP-FILE DEP-FILE RPT-FILE
           MOVE WS-RC TO RETURN-CODE
           GOBACK.

       1000-ONE-MANDATE.
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE SW-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               PERFORM 8100-REFUSED
               EXIT PARAGRAPH
           END-IF
           MOVE PP-BALANCE TO WS-BALANCE
           EVALUATE TRUE
               WHEN WS-BALANCE >= SW-THRESHOLD + SW-UNIT
                   PERFORM 2000-SWEEP-OUT
               WHEN WS-BALANCE < SW-FLOOR
                   PERFORM 3000-SWEEP-IN
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SWEEP-OUT: WHOLE UNITS OF THE SURPLUS ABOVE THE THRESHOLD      *
      *----------------------------------------------------------------*
       2000-SWEEP-OUT.
           COMPUTE WS-UNITS =
                   FUNCTION INTEGER((WS-BALANCE - SW-THRESHOLD)
                                    / SW-UNIT)
           IF WS-UNITS > WS-MAX-UNITS-PER-DAY
               MOVE WS-MAX-UNITS-PER-DAY TO WS-UNITS
           END-IF
           MOVE 'RT'        TO DC-FUNCTION
           MOVE SW-TENURE   TO DC-TENURE
           MOVE 'N'         TO DC-SENIOR
           MOVE WS-RUN-DATE TO DC-AS-OF-DATE
           CALL 'DEPCALC' USING DEPC-PARMS
           IF DC-RC NOT = ZERO
               MOVE SPACES TO RPT-LINE
               STRING SW-ACCT-NO '  OUT  NO RATE FOR TENURE '
                      SW-TENURE DELIMITED BY SIZE INTO RPT-LINE
               WRITE RPT-LINE
               MOVE 4 TO WS-RC
               EXIT PARAGRAPH
           END-IF
           MOVE DC-RESULT-RATE TO DC-RATE
           PERFORM VARYING WS-U FROM 1 BY 1 UNTIL WS-U > WS-UNITS
               PERFORM 2100-OPEN-ONE-FD
           END-PERFORM.

       2100-OPEN-ONE-FD.
           PERFORM 8000-NEXT-DEP-NO
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE SW-ACCT-NO     TO PP-ACCT-NO
           MOVE SW-UNIT        TO PP-AMOUNT
           MOVE 'FO'           TO PP-TRAN-TYPE
           MOVE WS-NEXT-DEP-NO TO PP-REF-ACCT
           MOVE 'AUTO-SWEEP TO FD' TO PP-NARRATION
           MOVE 'Y'            TO PP-SYSTEM-TXN
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               PERFORM 8100-REFUSED
               MOVE WS-UNITS TO WS-U
               EXIT PARAGRAPH
           END-IF
           INITIALIZE DEP-RECORD
           MOVE WS-NEXT-DEP-NO TO DEP-NO
           MOVE SW-ACCT-NO     TO DEP-ACCT-NO
           SET DEP-FD          TO TRUE
           SET DEP-ACTIVE      TO TRUE
           MOVE 'N'            TO DEP-SENIOR-FLAG
           MOVE 'Y'            TO DEP-SWEEP-FLAG
           MOVE SW-UNIT        TO DEP-AMOUNT DEP-TOTAL-DEPOSITED
           MOVE DC-RATE        TO DEP-RATE
           MOVE SW-TENURE      TO DEP-TENURE
           MOVE WS-RUN-DATE    TO DEP-OPEN-DATE
           MOVE 'FV'           TO DC-FUNCTION
           MOVE SW-UNIT        TO DC-AMOUNT
           MOVE SW-TENURE      TO DC-MONTHS
           CALL 'DEPCALC' USING DEPC-PARMS
           MOVE DC-RESULT-AMT  TO DEP-MATURITY-AMT
           SET DU-ADD-MONTHS   TO TRUE
           MOVE WS-RUN-DATE    TO DU-DATE-1
           MOVE SW-TENURE      TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO DEP-MATURITY-DATE
           WRITE DEP-RECORD
               INVALID KEY
                   DISPLAY 'SWEEPBAT: CRITICAL - FD WRITE FAILED '
                           WS-NEXT-DEP-NO ' (ACCOUNT DEBITED)'
                   MOVE 12 TO WS-RC
           END-WRITE
           ADD 1 TO SW-OUT-COUNT WS-OUT-FDS
           ADD SW-UNIT TO SW-OUT-AMOUNT WS-OUT-AMT
           MOVE SW-UNIT TO WS-DISP-AMT
           MOVE SPACES TO RPT-LINE
           STRING SW-ACCT-NO '  OUT  ' WS-NEXT-DEP-NO ' '
                  WS-DISP-AMT '  MATURES ' DEP-MATURITY-DATE
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE.

      *----------------------------------------------------------------*
      * SWEEP-IN: BREAK NEWEST SWEEP FDS UNTIL BALANCE >= FLOOR        *
      *----------------------------------------------------------------*
       3000-SWEEP-IN.
           PERFORM 3100-LOAD-LIVE-FDS
           IF WS-LIVE-CNT = ZERO
               MOVE SPACES TO RPT-LINE
               STRING SW-ACCT-NO '  IN   -           BELOW FLOOR, NO '
                      'SWEEP FDS LEFT' DELIMITED BY SIZE INTO RPT-LINE
               WRITE RPT-LINE
               EXIT PARAGRAPH
           END-IF
           MOVE WS-LIVE-CNT TO WS-L
           PERFORM UNTIL WS-L = ZERO OR WS-BALANCE >= SW-FLOOR
               MOVE WS-LIVE-DEP(WS-L) TO DEP-NO
               READ DEP-FILE
                   INVALID KEY CONTINUE
               END-READ
               IF WS-DEP-FS = '00'
                   PERFORM 3200-BREAK-ONE-FD
               END-IF
               SUBTRACT 1 FROM WS-L
           END-PERFORM.

       3100-LOAD-LIVE-FDS.
           MOVE ZERO TO WS-LIVE-CNT
           MOVE ZERO TO DEP-NO
           START DEP-FILE KEY IS NOT LESS THAN DEP-NO
               INVALID KEY EXIT PARAGRAPH
           END-START
           MOVE 'N' TO WS-DEP-EOF
           PERFORM UNTIL WS-DEP-EOF = 'Y'
               READ DEP-FILE NEXT
                   AT END MOVE 'Y' TO WS-DEP-EOF
               END-READ
               IF WS-DEP-EOF = 'N' AND DEP-ACCT-NO = SW-ACCT-NO
                  AND DEP-IS-SWEEP AND DEP-ACTIVE
                  AND DEP-LIEN-LOAN-NO = ZERO
                  AND WS-LIVE-CNT < 500
                   ADD 1 TO WS-LIVE-CNT
                   MOVE DEP-NO TO WS-LIVE-DEP(WS-LIVE-CNT)
               END-IF
           END-PERFORM.

       3200-BREAK-ONE-FD.
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE DEP-OPEN-DATE  TO DU-DATE-1
           MOVE WS-RUN-DATE    TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DAYS TO WS-DAYS-HELD
           IF WS-DAYS-HELD < ZERO
               MOVE ZERO TO WS-DAYS-HELD
           END-IF
           COMPUTE WS-MONTHS-HELD = WS-DAYS-HELD / 30
           IF WS-DAYS-HELD < WS-NO-INT-DAYS OR WS-MONTHS-HELD = ZERO
               MOVE DEP-AMOUNT TO WS-PAYOUT
           ELSE
      *        RATE FOR THE PERIOD HELD, ON THE CARD OF THE BOOKING DAY
               MOVE 'RT'           TO DC-FUNCTION
               MOVE WS-MONTHS-HELD TO DC-TENURE
               MOVE 'N'            TO DC-SENIOR
               MOVE DEP-OPEN-DATE  TO DC-AS-OF-DATE
               CALL 'DEPCALC' USING DEPC-PARMS
               MOVE DC-RESULT-RATE TO DC-RATE
               MOVE DEP-AMOUNT     TO DC-AMOUNT
               MOVE WS-MONTHS-HELD TO DC-MONTHS
               MOVE 'FV'           TO DC-FUNCTION
               CALL 'DEPCALC' USING DEPC-PARMS
               MOVE DC-RESULT-AMT  TO WS-PAYOUT
           END-IF
           MOVE WS-PAYOUT    TO DPP-GROSS
           MOVE 'AUTO SWEEP-IN FROM FD' TO DPP-NARRATION
           MOVE WS-RUN-DATE  TO DPP-RUN-DATE
           CALL 'DEPPAY' USING DPP-PARMS DEP-RECORD
           IF NOT DPP-OK
               PERFORM 8100-REFUSED
               MOVE 1 TO WS-L
               EXIT PARAGRAPH
           END-IF
           SET DEP-PREMATURE TO TRUE
           MOVE WS-PAYOUT    TO DEP-PAYOUT-AMT
           MOVE WS-RUN-DATE  TO DEP-CLOSE-DATE
           REWRITE DEP-RECORD
           ADD DPP-NET TO WS-BALANCE
           ADD 1 TO SW-IN-COUNT WS-IN-FDS
           ADD DPP-NET TO SW-IN-AMOUNT WS-IN-AMT
           MOVE DPP-NET      TO WS-DISP-AMT
           MOVE DPP-INTEREST TO WS-DISP-AMT2
           MOVE SPACES TO RPT-LINE
           STRING SW-ACCT-NO '  IN   ' DEP-NO ' ' WS-DISP-AMT
                  '  INTEREST ' FUNCTION TRIM(WS-DISP-AMT2)
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE.

      *----------------------------------------------------------------*
      * COMMON                                                         *
      *----------------------------------------------------------------*
       8000-NEXT-DEP-NO.
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

       8100-REFUSED.
           ADD 1 TO WS-REFUSED
           IF WS-RC < 4
               MOVE 4 TO WS-RC
           END-IF
           MOVE SPACES TO RPT-LINE
           STRING SW-ACCT-NO '  --   POSTING REFUSED RC='
                  PP-RETURN-CODE DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE.

       9000-TOTALS.
           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-MANDATES TO WS-DISP-CNT
           MOVE SPACES TO RPT-LINE
           STRING 'MANDATES PROCESSED : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           DISPLAY FUNCTION TRIM(RPT-LINE TRAILING)
           MOVE WS-OUT-FDS TO WS-DISP-CNT
           MOVE WS-OUT-AMT TO WS-DISP-AMT
           MOVE SPACES TO RPT-LINE
           STRING 'SWEPT OUT          : ' WS-DISP-CNT ' FDS '
                  WS-DISP-AMT DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           DISPLAY FUNCTION TRIM(RPT-LINE TRAILING)
           MOVE WS-IN-FDS TO WS-DISP-CNT
           MOVE WS-IN-AMT TO WS-DISP-AMT
           MOVE SPACES TO RPT-LINE
           STRING 'SWEPT IN           : ' WS-DISP-CNT ' FDS '
                  WS-DISP-AMT DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           DISPLAY FUNCTION TRIM(RPT-LINE TRAILING)
           MOVE WS-REFUSED TO WS-DISP-CNT
           MOVE SPACES TO RPT-LINE
           STRING 'REFUSED            : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           DISPLAY FUNCTION TRIM(RPT-LINE TRAILING).
