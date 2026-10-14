       IDENTIFICATION DIVISION.
       PROGRAM-ID. FDRTMNT.
      *================================================================*
      * PROGRAM  : FDRTMNT                                             *
      * PURPOSE  : TERM DEPOSIT RATE CARD MAINTENANCE WITH VERSIONS.   *
      *            A RATE CHANGE IS A NEW CARD WITH AN EFFECTIVE DATE  *
      *            (TODAY OR LATER). DEPOSITS ALREADY BOOKED KEEP THE  *
      *            RATE STORED ON THEM; ONLY DEPOSITS OPENED ON OR     *
      *            AFTER THE EFFECTIVE DATE GET THE NEW RATE. EACH     *
      *            SLAB HAS ITS OWN SENIOR CITIZEN RATE (0 - 1% ABOVE  *
      *            THE GENERAL RATE).                                  *
      *            1 CARD IN FORCE ON A DATE                           *
      *            2 NEW CARD (MANAGER)                                *
      *            3 CARD HISTORY                                      *
      *            4 RATE QUOTE (TENURE, SENIOR, DATE)                 *
      *            5 WITHDRAW A FUTURE CARD NOT YET IN FORCE (MANAGER) *
      * FILES    : FDRATE (INDEXED, I-O)                               *
      * CALLS    : DEPCALC, AUDITLOG                                   *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATE-FILE ASSIGN TO 'data/FDRATE.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FR-KEY
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  RATE-FILE.
           COPY FDRTREC.

       WORKING-STORAGE SECTION.
           COPY SESSION.
           COPY DEPCPARM.
           COPY AUDPARM.
       01  WS-FS                       PIC X(02).
       01  WS-TODAY                    PIC 9(08).
       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.
       01  WS-EOF-SW                   PIC X(01).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-DATE                     PIC 9(08).
       01  WS-CARD-DATE                PIC 9(08).
       01  WS-PREV-DATE                PIC 9(08).
       01  WS-SLAB-CNT                 PIC 9(02).
       01  WS-I                        PIC 9(02).
       01  WS-IN-MONTHS                PIC X(03).
       01  WS-IN-GEN                   PIC X(08).
       01  WS-IN-SNR                   PIC X(08).
       01  WS-IN-TENURE                PIC 9(03).
       01  WS-IN-SENIOR                PIC X(01).
       01  WS-CONFIRM                  PIC X(01).
       01  WS-DONE-SW                  PIC X(01).
       01  WS-OK-SW                    PIC X(01).
           88  CARD-OK                           VALUE 'Y'.
       01  WS-DISP-RATE                PIC Z9.9999.
       01  WS-DISP-RATE2               PIC Z9.9999.
       01  WS-DISP-MON                 PIC ZZ9.

      * A CARD BEING KEYED: UP TO 8 SLABS, ASCENDING, LAST = 120
       01  WS-NEW-CARD.
           05  WS-NEW-SLAB OCCURS 8 TIMES.
               10  NS-MAX-MONTHS       PIC 9(03).
               10  NS-GEN-RATE         PIC 9(02)V9(04).
               10  NS-SNR-RATE         PIC 9(02)V9(04).
       01  WS-MAX-TENURE               PIC 9(03) VALUE 120.
       01  WS-MAX-RATE                 PIC 9(02)V9(04) VALUE 12.0000.
       01  WS-MAX-SENIOR-EXTRA         PIC 9V9(04) VALUE 1.0000.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           PERFORM 0100-OPEN-FILE
           IF WS-FS NOT = '00'
               DISPLAY 'RATE CARD FILE OPEN FAILED, FS=' WS-FS
               GOBACK
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE = '9'
               DISPLAY ' '
               DISPLAY '---------- DEPOSIT RATE CARD ----------'
               DISPLAY ' 1. CARD IN FORCE ON A DATE'
               DISPLAY ' 2. NEW CARD (MANAGER)'
               DISPLAY ' 3. CARD HISTORY'
               DISPLAY ' 4. RATE QUOTE'
               DISPLAY ' 5. WITHDRAW A FUTURE CARD (MANAGER)'
               DISPLAY ' 9. BACK'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 1000-SHOW-CARD
                   WHEN '2' PERFORM 2000-NEW-CARD
                   WHEN '3' PERFORM 3000-HISTORY
                   WHEN '4' PERFORM 4000-QUOTE
                   WHEN '5' PERFORM 5000-WITHDRAW
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
           CLOSE RATE-FILE
           GOBACK.

       0100-OPEN-FILE.
           OPEN I-O RATE-FILE
           IF WS-FS = '35'
               OPEN OUTPUT RATE-FILE
               CLOSE RATE-FILE
               OPEN I-O RATE-FILE
           END-IF.

      *----------------------------------------------------------------*
      * 1. THE CARD IN FORCE ON A DATE = LATEST EFFECTIVE DATE <= IT   *
      *----------------------------------------------------------------*
       1000-SHOW-CARD.
           DISPLAY 'AS OF DATE (BLANK=TODAY): ' WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           PERFORM 8000-DATE-OR-TODAY
           PERFORM 8100-FIND-CARD-IN-FORCE
           IF WS-CARD-DATE = ZERO
               DISPLAY 'NO CARD SET UP FOR THAT DATE - BUILT-IN CARD '
                       'APPLIES (6.50/7.00/7.25/6.75 +0.50 SENIOR).'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'CARD EFFECTIVE ' WS-CARD-DATE
           DISPLAY '  UP TO MONTHS   GENERAL    SENIOR'
           PERFORM 8200-LIST-CARD.

      *----------------------------------------------------------------*
      * 2. NEW CARD                                                    *
      *----------------------------------------------------------------*
       2000-NEW-CARD.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'ONLY A MANAGER CAN SET A RATE CARD.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'EFFECTIVE DATE (YYYYMMDD) : ' WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           PERFORM 8000-DATE-OR-TODAY
           IF WS-DATE < WS-TODAY
               DISPLAY 'A CARD CANNOT BE BACK-DATED.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-DATE TO FR-EFF-DATE
           MOVE ZERO    TO FR-MAX-MONTHS
           START RATE-FILE KEY >= FR-KEY
               INVALID KEY MOVE '23' TO WS-FS
           END-START
           IF WS-FS = '00'
               READ RATE-FILE NEXT
                   AT END MOVE '10' TO WS-FS
               END-READ
               IF WS-FS = '00' AND FR-EFF-DATE = WS-DATE
                   DISPLAY 'A CARD ALREADY EXISTS FOR ' WS-DATE
                           ' - WITHDRAW IT FIRST.'
                   EXIT PARAGRAPH
               END-IF
           END-IF

           DISPLAY 'ENTER SLABS IN ASCENDING ORDER, LAST = 120 MONTHS'
                   ' (BLANK MONTHS ENDS).'
           INITIALIZE WS-NEW-CARD
           MOVE ZERO TO WS-SLAB-CNT
           MOVE 'Y' TO WS-OK-SW
           MOVE 'N' TO WS-DONE-SW
           PERFORM UNTIL WS-SLAB-CNT = 8 OR NOT CARD-OK
                      OR WS-DONE-SW = 'Y'
                      OR (WS-SLAB-CNT > 0
                          AND NS-MAX-MONTHS(WS-SLAB-CNT)
                              = WS-MAX-TENURE)
               PERFORM 2100-KEY-SLAB
           END-PERFORM
           IF NOT CARD-OK
               EXIT PARAGRAPH
           END-IF
           IF WS-SLAB-CNT = 0
              OR NS-MAX-MONTHS(WS-SLAB-CNT) NOT = WS-MAX-TENURE
               DISPLAY 'LAST SLAB MUST END AT 120 MONTHS - CARD '
                       'NOT SAVED.'
               EXIT PARAGRAPH
           END-IF

           DISPLAY 'SAVE CARD EFFECTIVE ' WS-DATE ' (' WS-SLAB-CNT
                   ' SLABS)? (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
               DISPLAY 'CARD NOT SAVED.'
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-SLAB-CNT
               INITIALIZE FDRT-RECORD
               MOVE WS-DATE           TO FR-EFF-DATE
               MOVE NS-MAX-MONTHS(WS-I) TO FR-MAX-MONTHS
               MOVE NS-GEN-RATE(WS-I) TO FR-GEN-RATE
               MOVE NS-SNR-RATE(WS-I) TO FR-SNR-RATE
               MOVE SES-USER-ID       TO FR-SET-BY
               MOVE WS-TODAY          TO FR-SET-DATE
               WRITE FDRT-RECORD
                   INVALID KEY
                       DISPLAY 'WRITE FAILED FS=' WS-FS
               END-WRITE
           END-PERFORM
           MOVE 'NEW CARD' TO AU-ACTION
           MOVE WS-DATE    TO AU-KEY
           MOVE SPACES     TO AU-DETAIL
           STRING 'SLABS ' WS-SLAB-CNT DELIMITED BY SIZE INTO AU-DETAIL
           PERFORM 8900-AUDIT
           DISPLAY 'RATE CARD EFFECTIVE ' WS-DATE ' SAVED - EXISTING '
                   'DEPOSITS KEEP THEIR RATE.'.

       2100-KEY-SLAB.
           ADD 1 TO WS-SLAB-CNT
           DISPLAY 'SLAB ' WS-SLAB-CNT ' UP TO MONTHS : '
                   WITH NO ADVANCING
           ACCEPT WS-IN-MONTHS
           IF WS-IN-MONTHS = SPACES
               SUBTRACT 1 FROM WS-SLAB-CNT
               MOVE 'Y' TO WS-DONE-SW
               EXIT PARAGRAPH
           END-IF
           DISPLAY '        GENERAL RATE % : ' WITH NO ADVANCING
           ACCEPT WS-IN-GEN
           DISPLAY '        SENIOR RATE %  : ' WITH NO ADVANCING
           ACCEPT WS-IN-SNR
           IF FUNCTION TEST-NUMVAL(WS-IN-MONTHS) NOT = ZERO
              OR FUNCTION TEST-NUMVAL(WS-IN-GEN) NOT = ZERO
              OR FUNCTION TEST-NUMVAL(WS-IN-SNR) NOT = ZERO
               DISPLAY 'MONTHS AND RATES MUST BE NUMBERS - CARD '
                       'NOT SAVED.'
               MOVE 'N' TO WS-OK-SW
               EXIT PARAGRAPH
           END-IF
           COMPUTE NS-MAX-MONTHS(WS-SLAB-CNT) =
                   FUNCTION NUMVAL(WS-IN-MONTHS)
           COMPUTE NS-GEN-RATE(WS-SLAB-CNT) = FUNCTION NUMVAL(WS-IN-GEN)
           COMPUTE NS-SNR-RATE(WS-SLAB-CNT) = FUNCTION NUMVAL(WS-IN-SNR)
           EVALUATE TRUE
               WHEN NS-MAX-MONTHS(WS-SLAB-CNT) > WS-MAX-TENURE
               WHEN NS-MAX-MONTHS(WS-SLAB-CNT) = ZERO
                   DISPLAY 'SLAB MUST END AT 1 - 120 MONTHS.'
                   MOVE 'N' TO WS-OK-SW
               WHEN WS-SLAB-CNT > 1 AND NS-MAX-MONTHS(WS-SLAB-CNT)
                    NOT > NS-MAX-MONTHS(WS-SLAB-CNT - 1)
                   DISPLAY 'SLABS MUST BE IN ASCENDING ORDER.'
                   MOVE 'N' TO WS-OK-SW
               WHEN NS-GEN-RATE(WS-SLAB-CNT) = ZERO
               WHEN NS-GEN-RATE(WS-SLAB-CNT) > WS-MAX-RATE
                   DISPLAY 'GENERAL RATE MUST BE ABOVE 0 AND AT MOST '
                           '12%.'
                   MOVE 'N' TO WS-OK-SW
               WHEN NS-SNR-RATE(WS-SLAB-CNT)
                    < NS-GEN-RATE(WS-SLAB-CNT)
               WHEN NS-SNR-RATE(WS-SLAB-CNT)
                    > NS-GEN-RATE(WS-SLAB-CNT) + WS-MAX-SENIOR-EXTRA
                   DISPLAY 'SENIOR RATE MUST BE 0 - 1% ABOVE GENERAL.'
                   MOVE 'N' TO WS-OK-SW
           END-EVALUATE
           IF NOT CARD-OK
               DISPLAY 'CARD NOT SAVED.'
           END-IF.

      *----------------------------------------------------------------*
      * 3. HISTORY - ONE LINE PER CARD                                 *
      *----------------------------------------------------------------*
       3000-HISTORY.
           MOVE LOW-VALUES TO FR-KEY
           START RATE-FILE KEY >= FR-KEY
               INVALID KEY
                   DISPLAY 'NO RATE CARDS SET UP.'
                   EXIT PARAGRAPH
           END-START
           DISPLAY 'EFFECTIVE  SLABS  SET BY    ON        STATUS'
           MOVE ZERO TO WS-PREV-DATE WS-SLAB-CNT
           MOVE 'N' TO WS-EOF-SW
           PERFORM 8100-FIND-CARD-IN-FORCE-TODAY
           MOVE LOW-VALUES TO FR-KEY
           START RATE-FILE KEY >= FR-KEY
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ RATE-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'Y' OR FR-EFF-DATE NOT = WS-PREV-DATE
                   IF WS-PREV-DATE > ZERO
                       PERFORM 3100-HISTORY-LINE
                   END-IF
                   MOVE ZERO TO WS-SLAB-CNT
               END-IF
               IF WS-EOF-SW = 'N'
                   MOVE FR-EFF-DATE TO WS-PREV-DATE
                   ADD 1 TO WS-SLAB-CNT
                   MOVE FR-SET-BY   TO AU-KEY
                   MOVE FR-SET-DATE TO WS-DATE
               END-IF
           END-PERFORM.

       3100-HISTORY-LINE.
           EVALUATE TRUE
               WHEN WS-PREV-DATE = WS-CARD-DATE
                   MOVE 'IN FORCE' TO AU-DETAIL
               WHEN WS-PREV-DATE > WS-TODAY
                   MOVE 'FUTURE'   TO AU-DETAIL
               WHEN OTHER
                   MOVE 'SUPERSEDED' TO AU-DETAIL
           END-EVALUATE
           DISPLAY WS-PREV-DATE '   ' WS-SLAB-CNT '    '
                   AU-KEY(1:8) '  ' WS-DATE '  '
                   FUNCTION TRIM(AU-DETAIL).

      *----------------------------------------------------------------*
      * 4. QUOTE THROUGH DEPCALC - THE SAME LOOKUP DEPMNT USES         *
      *----------------------------------------------------------------*
       4000-QUOTE.
           DISPLAY 'TENURE (MONTHS)           : ' WITH NO ADVANCING
           ACCEPT WS-IN-TENURE
           DISPLAY 'SENIOR CITIZEN (Y/N)      : ' WITH NO ADVANCING
           ACCEPT WS-IN-SENIOR
           DISPLAY 'BOOKING DATE (BLANK=TODAY): ' WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           PERFORM 8000-DATE-OR-TODAY
      *    DEPCALC OPENS THE CARD ITSELF
           CLOSE RATE-FILE
           MOVE 'RT'          TO DC-FUNCTION
           MOVE WS-IN-TENURE  TO DC-TENURE
           MOVE FUNCTION UPPER-CASE(WS-IN-SENIOR) TO DC-SENIOR
           MOVE WS-DATE       TO DC-AS-OF-DATE
           CALL 'DEPCALC' USING DEPC-PARMS
           PERFORM 0100-OPEN-FILE
           IF DC-RC NOT = ZERO
               DISPLAY 'NO RATE FOR A TENURE OF ' WS-IN-TENURE
                       ' MONTHS.'
               EXIT PARAGRAPH
           END-IF
           MOVE DC-RESULT-RATE TO WS-DISP-RATE
           IF DC-RATE-EFF-DATE = ZERO
               DISPLAY 'RATE ' WS-DISP-RATE '% P.A. (BUILT-IN CARD)'
           ELSE
               DISPLAY 'RATE ' WS-DISP-RATE '% P.A. (CARD EFFECTIVE '
                       DC-RATE-EFF-DATE ')'
           END-IF.

      *----------------------------------------------------------------*
      * 5. A CARD NOT YET IN FORCE CAN BE WITHDRAWN; ONE IN FORCE      *
      *    (OR PAST) CANNOT, BECAUSE DEPOSITS MAY HAVE BEEN BOOKED ON  *
      *    IT AND ITS HISTORY MUST STAY.                               *
      *----------------------------------------------------------------*
       5000-WITHDRAW.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'ONLY A MANAGER CAN WITHDRAW A RATE CARD.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'EFFECTIVE DATE OF CARD    : ' WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           PERFORM 8000-DATE-OR-TODAY
           IF WS-DATE NOT > WS-TODAY
               DISPLAY 'ONLY A FUTURE CARD CAN BE WITHDRAWN.'
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO WS-SLAB-CNT
           MOVE WS-DATE TO FR-EFF-DATE
           MOVE ZERO    TO FR-MAX-MONTHS
           START RATE-FILE KEY >= FR-KEY
               INVALID KEY MOVE '23' TO WS-FS
           END-START
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y' OR WS-FS NOT = '00'
               READ RATE-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N'
                   IF FR-EFF-DATE NOT = WS-DATE
                       MOVE 'Y' TO WS-EOF-SW
                   ELSE
                       DELETE RATE-FILE RECORD
                       ADD 1 TO WS-SLAB-CNT
                   END-IF
               END-IF
           END-PERFORM
           MOVE '00' TO WS-FS
           IF WS-SLAB-CNT = ZERO
               DISPLAY 'NO CARD WITH EFFECTIVE DATE ' WS-DATE '.'
               EXIT PARAGRAPH
           END-IF
           MOVE 'WITHDRAW CARD' TO AU-ACTION
           MOVE WS-DATE TO AU-KEY
           MOVE SPACES  TO AU-DETAIL
           PERFORM 8900-AUDIT
           DISPLAY 'CARD ' WS-DATE ' WITHDRAWN (' WS-SLAB-CNT
                   ' SLABS).'.

      *----------------------------------------------------------------*
      * COMMON                                                         *
      *----------------------------------------------------------------*
       8000-DATE-OR-TODAY.
           IF WS-IN-DATE = SPACES
               MOVE WS-TODAY TO WS-DATE
           ELSE
               IF WS-IN-DATE IS NUMERIC
                   MOVE WS-IN-DATE TO WS-DATE
               ELSE
                   DISPLAY 'BAD DATE - USING TODAY.'
                   MOVE WS-TODAY TO WS-DATE
               END-IF
           END-IF.

       8100-FIND-CARD-IN-FORCE-TODAY.
           MOVE WS-TODAY TO WS-DATE
           PERFORM 8100-FIND-CARD-IN-FORCE.

      *    LATEST EFFECTIVE DATE NOT AFTER WS-DATE (ZERO = NONE)
       8100-FIND-CARD-IN-FORCE.
           MOVE ZERO TO WS-CARD-DATE
           MOVE LOW-VALUES TO FR-KEY
           START RATE-FILE KEY >= FR-KEY
               INVALID KEY
                   EXIT PARAGRAPH
           END-START
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ RATE-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N'
                   IF FR-EFF-DATE > WS-DATE
                       MOVE 'Y' TO WS-EOF-SW
                   ELSE
                       MOVE FR-EFF-DATE TO WS-CARD-DATE
                   END-IF
               END-IF
           END-PERFORM
           MOVE 'N' TO WS-EOF-SW.

       8200-LIST-CARD.
           MOVE WS-CARD-DATE TO FR-EFF-DATE
           MOVE ZERO         TO FR-MAX-MONTHS
           START RATE-FILE KEY >= FR-KEY
               INVALID KEY EXIT PARAGRAPH
           END-START
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ RATE-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N'
                   IF FR-EFF-DATE NOT = WS-CARD-DATE
                       MOVE 'Y' TO WS-EOF-SW
                   ELSE
                       MOVE FR-MAX-MONTHS TO WS-DISP-MON
                       MOVE FR-GEN-RATE   TO WS-DISP-RATE
                       MOVE FR-SNR-RATE   TO WS-DISP-RATE2
                       DISPLAY '      ' WS-DISP-MON '      '
                               WS-DISP-RATE '   ' WS-DISP-RATE2
                   END-IF
               END-IF
           END-PERFORM.

       8900-AUDIT.
           MOVE 'FDRTMNT' TO AU-PROGRAM
           CALL 'AUDITLOG' USING AUD-PARMS.
