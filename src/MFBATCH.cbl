       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFBATCH.
      *================================================================*
      * PROGRAM  : MFBATCH                                             *
      * PURPOSE  : END-OF-DAY RUN FOR THIRD-PARTY PRODUCTS.            *
      *            1. SIPS DUE ON OR BEFORE THE RUN DATE ARE BOUGHT    *
      *               THROUGH MFUTIL. A FAILED DEBIT IS A MISSED       *
      *               INSTALMENT; 3 IN A ROW CANCEL THE SIP. AFTER THE *
      *               LAST INSTALMENT THE SIP IS COMPLETED.            *
      *            2. INSURANCE PREMIUMS DUE ARE COLLECTED; A POLICY   *
      *               STILL UNPAID 30 DAYS AFTER ITS DUE DATE LAPSES.  *
      *            3. COLLECTIONS HELD IN GL 2906 ARE REMITTED TO THE  *
      *               AMCS / INSURERS (DR 2906, CR 1302 SETTLEMENT).   *
      *            4. ON THE LAST DAY OF A MONTH, TRAIL COMMISSION ON  *
      *               FUND ASSETS (VALUE x TRAIL % / 12) IS BOOKED AS  *
      *               RECEIVABLE FROM THE AMCS (DR 1302, CR 4106).     *
      * INPUT    : RUN DATE YYYYMMDD (BLANK = TODAY)                   *
      * OUTPUT   : data/MF_<DATE>.TXT                                  *
      * FILES    : MFSIP (I-O), MFHOLD (INPUT, PREMIUM SCAN AND TRAIL),*
      *            MFSCHEME (INPUT), GLMAST (INPUT, FOR 2906)          *
      * CALLS    : MFUTIL, GLPOST, DATEUTIL                            *
      * RC       : 0 OK, 4 MISSED SIPS / PREMIUMS OR LAPSES, 12 ERROR  *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SIP-FILE ASSIGN TO 'data/MFSIP.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS SP-NO
               FILE STATUS IS WS-SIP-FS.
           SELECT MFH-FILE ASSIGN TO 'data/MFHOLD.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MH-KEY
               FILE STATUS IS WS-MFH-FS.
           SELECT MFS-FILE ASSIGN TO 'data/MFSCHEME.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MS-CODE
               FILE STATUS IS WS-MFS-FS.
           SELECT GL-FILE ASSIGN TO 'data/GLMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS GL-CODE
               FILE STATUS IS WS-GL-FS.
           SELECT RPT-FILE ASSIGN TO WS-RPT-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  SIP-FILE.
           COPY SIPREC.
       FD  MFH-FILE.
           COPY MFHREC.
       FD  MFS-FILE.
           COPY MFSREC.
       FD  GL-FILE.
           COPY GLREC.
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(100).

       WORKING-STORAGE SECTION.
           COPY MFUPARM.
           COPY GLPARM.
           COPY GLCODES.
           COPY DATEPARM.
       01  WS-SIP-FS                   PIC X(02).
       01  WS-MFH-FS                   PIC X(02).
       01  WS-MFS-FS                   PIC X(02).
       01  WS-GL-FS                    PIC X(02).
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-NAME                 PIC X(40).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-EOF-SW                   PIC X(01).
       01  WS-RC                       PIC 9(02) VALUE ZERO.
       01  WS-MONTH-END-SW             PIC X(01).
           88  RUN-IS-MONTH-END                  VALUE 'Y'.
       01  WS-GRACE-DAYS               PIC 9(03) VALUE 30.
       01  WS-LAPSE-DATE               PIC 9(08).
       01  WS-MAX-MISSES               PIC 9(02) VALUE 3.

      * POLICIES DUE, COLLECTED AFTER THE SCAN (MFUTIL OPENS MFHOLD)
       01  WS-DUE-CNT                  PIC 9(04) VALUE ZERO.
       01  WS-DUE-TABLE.
           05  WS-DUE OCCURS 2000 TIMES.
               10  DUE-ACCT            PIC 9(10).
               10  DUE-CODE            PIC X(06).
               10  DUE-DATE            PIC 9(08).
       01  WS-D                        PIC 9(04).

       01  WS-TOTALS.
           05  WS-SIP-OK               PIC 9(07).
           05  WS-SIP-AMT              PIC 9(13)V99.
           05  WS-SIP-MISSED           PIC 9(07).
           05  WS-SIP-STOPPED          PIC 9(07).
           05  WS-SIP-DONE             PIC 9(07).
           05  WS-PREM-OK              PIC 9(07).
           05  WS-PREM-AMT             PIC 9(13)V99.
           05  WS-PREM-COMM            PIC 9(11)V99.
           05  WS-PREM-MISSED          PIC 9(07).
           05  WS-LAPSED               PIC 9(07).
           05  WS-REMITTED             PIC 9(13)V99.
           05  WS-TRAIL                PIC 9(11)V99.
       01  WS-VALUE                    PIC 9(13)V99.
       01  WS-DISP-AMT                 PIC ZZ,ZZZ,ZZZ,ZZ9.99.
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
           SET DU-ADD-DAYS TO TRUE
           MOVE WS-RUN-DATE TO DU-DATE-1
           MOVE 1 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           IF DU-RESULT-DATE(7:2) = '01'
               MOVE 'Y' TO WS-MONTH-END-SW
           ELSE
               MOVE 'N' TO WS-MONTH-END-SW
           END-IF
           STRING 'data/MF_' WS-RUN-DATE '.TXT'
                  DELIMITED BY SIZE INTO WS-RPT-NAME
           OPEN OUTPUT RPT-FILE
           MOVE SPACES TO RPT-LINE
           STRING 'MUTUAL FUND / INSURANCE RUN ' WS-RUN-DATE
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE

           PERFORM 1000-RUN-SIPS
           PERFORM 2000-PREMIUMS
           PERFORM 3000-REMIT-COLLECTIONS
           IF RUN-IS-MONTH-END
               PERFORM 4000-TRAIL-COMMISSION
           END-IF
           PERFORM 9000-TOTALS
           CLOSE RPT-FILE
           MOVE WS-RC TO RETURN-CODE
           GOBACK.

      *----------------------------------------------------------------*
      * 1. SIPS                                                        *
      *----------------------------------------------------------------*
       1000-RUN-SIPS.
           OPEN I-O SIP-FILE
           IF WS-SIP-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ SIP-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N' AND SP-ACTIVE
                  AND SP-NEXT-DATE <= WS-RUN-DATE
                   PERFORM 1100-ONE-SIP
                   REWRITE SIP-RECORD
               END-IF
           END-PERFORM
           CLOSE SIP-FILE.

       1100-ONE-SIP.
           INITIALIZE MFU-PARMS
           SET MU-BUY TO TRUE
           MOVE SP-ACCT-NO  TO MU-ACCT-NO
           MOVE SP-CODE     TO MU-CODE
           MOVE 'S'         TO MU-SOURCE
           MOVE SP-AMOUNT   TO MU-AMOUNT
           MOVE WS-RUN-DATE TO MU-RUN-DATE
           MOVE SPACES      TO MU-REF
           STRING 'SIP ' SP-NO DELIMITED BY SIZE INTO MU-REF
           CALL 'MFUTIL' USING MFU-PARMS
           MOVE SPACES TO RPT-LINE
           IF MU-OK
               ADD 1 TO SP-PAID-CNT WS-SIP-OK
               ADD SP-AMOUNT TO WS-SIP-AMT
               MOVE ZERO TO SP-MISSED-CNT
               MOVE SP-AMOUNT TO WS-DISP-AMT
               STRING 'SIP ' SP-NO ' ' SP-ACCT-NO ' ' SP-CODE ' '
                      WS-DISP-AMT ' UNITS ' MU-UNITS
                      DELIMITED BY SIZE INTO RPT-LINE
           ELSE
               ADD 1 TO SP-MISSED-CNT WS-SIP-MISSED
               MOVE 4 TO WS-RC
               STRING 'SIP ' SP-NO ' ' SP-ACCT-NO ' ' SP-CODE
                      '  MISSED (RC ' MU-RC ') - ' SP-MISSED-CNT
                      ' IN A ROW' DELIMITED BY SIZE INTO RPT-LINE
           END-IF
           WRITE RPT-LINE
           IF SP-MISSED-CNT >= WS-MAX-MISSES
               SET SP-CANCELLED TO TRUE
               ADD 1 TO WS-SIP-STOPPED
               MOVE SPACES TO RPT-LINE
               STRING 'SIP ' SP-NO ' CANCELLED AFTER 3 MISSED '
                      'INSTALMENTS' DELIMITED BY SIZE INTO RPT-LINE
               WRITE RPT-LINE
               EXIT PARAGRAPH
           END-IF
           SET DU-ADD-MONTHS TO TRUE
           MOVE SP-NEXT-DATE TO DU-DATE-1
           MOVE 1 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO SP-NEXT-DATE
           IF SP-NEXT-DATE > SP-END-DATE
               SET SP-COMPLETED TO TRUE
               ADD 1 TO WS-SIP-DONE
           END-IF.

      *----------------------------------------------------------------*
      * 2. PREMIUMS - SCAN FIRST, THEN COLLECT (MFUTIL UPDATES MFHOLD) *
      *----------------------------------------------------------------*
       2000-PREMIUMS.
           OPEN INPUT MFS-FILE
           OPEN I-O MFH-FILE
           IF WS-MFH-FS NOT = '00' OR WS-MFS-FS NOT = '00'
               CLOSE MFS-FILE MFH-FILE
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO WS-DUE-CNT
           MOVE LOW-VALUES TO MH-KEY
           START MFH-FILE KEY >= MH-KEY
               INVALID KEY MOVE 'Y' TO WS-EOF-SW
               NOT INVALID KEY MOVE 'N' TO WS-EOF-SW
           END-START
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ MFH-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N' AND MH-ACTIVE
                  AND MH-POLICY-NO NOT = SPACES
                  AND MH-NEXT-DUE <= WS-RUN-DATE
                  AND WS-DUE-CNT < 2000
                   ADD 1 TO WS-DUE-CNT
                   MOVE MH-ACCT-NO  TO DUE-ACCT(WS-DUE-CNT)
                   MOVE MH-CODE     TO DUE-CODE(WS-DUE-CNT)
                   MOVE MH-NEXT-DUE TO DUE-DATE(WS-DUE-CNT)
               END-IF
           END-PERFORM
           CLOSE MFS-FILE MFH-FILE
           PERFORM VARYING WS-D FROM 1 BY 1 UNTIL WS-D > WS-DUE-CNT
               PERFORM 2100-ONE-PREMIUM
           END-PERFORM.

       2100-ONE-PREMIUM.
           INITIALIZE MFU-PARMS
           SET MU-PREMIUM TO TRUE
           MOVE DUE-ACCT(WS-D) TO MU-ACCT-NO
           MOVE DUE-CODE(WS-D) TO MU-CODE
           MOVE WS-RUN-DATE    TO MU-RUN-DATE
           MOVE 'RENEWAL'      TO MU-REF
           CALL 'MFUTIL' USING MFU-PARMS
           MOVE SPACES TO RPT-LINE
           IF MU-OK
               ADD 1 TO WS-PREM-OK
               ADD MU-NET TO WS-PREM-AMT
               ADD MU-COMMISSION TO WS-PREM-COMM
               MOVE MU-NET TO WS-DISP-AMT
               STRING 'PREMIUM ' DUE-ACCT(WS-D) ' ' DUE-CODE(WS-D)
                      ' ' WS-DISP-AMT ' COLLECTED'
                      DELIMITED BY SIZE INTO RPT-LINE
               WRITE RPT-LINE
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-PREM-MISSED
           MOVE 4 TO WS-RC
      *    UNPAID PAST THE GRACE PERIOD -> LAPSE
           SET DU-ADD-DAYS TO TRUE
           MOVE DUE-DATE(WS-D) TO DU-DATE-1
           MOVE WS-GRACE-DAYS  TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO WS-LAPSE-DATE
           IF WS-RUN-DATE > WS-LAPSE-DATE
               OPEN I-O MFH-FILE
               MOVE DUE-ACCT(WS-D) TO MH-ACCT-NO
               MOVE DUE-CODE(WS-D) TO MH-CODE
               READ MFH-FILE
                   NOT INVALID KEY
                       SET MH-LAPSED TO TRUE
                       REWRITE MFH-RECORD
               END-READ
               CLOSE MFH-FILE
               ADD 1 TO WS-LAPSED
               STRING 'PREMIUM ' DUE-ACCT(WS-D) ' ' DUE-CODE(WS-D)
                      ' DUE ' DUE-DATE(WS-D) ' UNPAID - POLICY LAPSED'
                      DELIMITED BY SIZE INTO RPT-LINE
           ELSE
               STRING 'PREMIUM ' DUE-ACCT(WS-D) ' ' DUE-CODE(WS-D)
                      ' DUE ' DUE-DATE(WS-D) ' UNPAID (RC ' MU-RC
                      ') - GRACE TILL ' WS-LAPSE-DATE
                      DELIMITED BY SIZE INTO RPT-LINE
           END-IF
           WRITE RPT-LINE.

      *----------------------------------------------------------------*
      * 3. REMIT WHAT GL 2906 HOLDS FOR THE AMCS / INSURERS            *
      *----------------------------------------------------------------*
       3000-REMIT-COLLECTIONS.
           OPEN INPUT GL-FILE
           IF WS-GL-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           MOVE GLC-MF-COLLECTIONS TO GL-CODE
           READ GL-FILE
               INVALID KEY MOVE ZERO TO GL-BALANCE
           END-READ
           CLOSE GL-FILE
      *    A LIABILITY CARRIES A CREDIT (NEGATIVE) BALANCE
           IF GL-BALANCE < ZERO
               COMPUTE WS-REMITTED = 0 - GL-BALANCE
               MOVE GLC-MF-COLLECTIONS TO GP-DR-GL
               MOVE GLC-PAYMENT-SETTLE TO GP-CR-GL
               MOVE WS-REMITTED        TO GP-AMOUNT
               MOVE 'MFBATCH'          TO GP-REF
               MOVE 'REMIT TO AMC / INSURER' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
               IF NOT GP-OK
                   MOVE 12 TO WS-RC
                   MOVE ZERO TO WS-REMITTED
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * 4. MONTH-END TRAIL COMMISSION                                  *
      *----------------------------------------------------------------*
       4000-TRAIL-COMMISSION.
           OPEN INPUT MFS-FILE
           OPEN INPUT MFH-FILE
           IF WS-MFH-FS NOT = '00' OR WS-MFS-FS NOT = '00'
               CLOSE MFS-FILE MFH-FILE
               EXIT PARAGRAPH
           END-IF
           MOVE LOW-VALUES TO MH-KEY
           START MFH-FILE KEY >= MH-KEY
               INVALID KEY MOVE 'Y' TO WS-EOF-SW
               NOT INVALID KEY MOVE 'N' TO WS-EOF-SW
           END-START
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ MFH-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N' AND MH-ACTIVE AND MH-UNITS > ZERO
                   MOVE MH-CODE TO MS-CODE
                   READ MFS-FILE
                       INVALID KEY MOVE ZERO TO MS-NAV
                   END-READ
                   IF MS-MUTUAL-FUND
                       COMPUTE WS-VALUE ROUNDED = MH-UNITS * MS-NAV
                       COMPUTE WS-TRAIL ROUNDED = WS-TRAIL
                               + WS-VALUE * MS-COMM-PCT / 100 / 12
                   END-IF
               END-IF
           END-PERFORM
           CLOSE MFS-FILE MFH-FILE
           IF WS-TRAIL > ZERO
               MOVE GLC-PAYMENT-SETTLE TO GP-DR-GL
               MOVE GLC-MF-COMMISSION  TO GP-CR-GL
               MOVE WS-TRAIL           TO GP-AMOUNT
               MOVE 'MFBATCH'          TO GP-REF
               MOVE 'MF TRAIL COMMISSION' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
           END-IF.

       9000-TOTALS.
           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-SIP-OK  TO WS-DISP-CNT
           MOVE WS-SIP-AMT TO WS-DISP-AMT
           MOVE SPACES TO RPT-LINE
           STRING 'SIPS INVESTED      : ' WS-DISP-CNT ' '
                  WS-DISP-AMT DELIMITED BY SIZE INTO RPT-LINE
           PERFORM 9100-OUT
           MOVE WS-SIP-MISSED TO WS-DISP-CNT
           MOVE SPACES TO RPT-LINE
           STRING 'SIPS MISSED        : ' WS-DISP-CNT
                  '  CANCELLED ' WS-SIP-STOPPED
                  '  COMPLETED ' WS-SIP-DONE
                  DELIMITED BY SIZE INTO RPT-LINE
           PERFORM 9100-OUT
           MOVE WS-PREM-OK  TO WS-DISP-CNT
           MOVE WS-PREM-AMT TO WS-DISP-AMT
           MOVE SPACES TO RPT-LINE
           STRING 'PREMIUMS COLLECTED : ' WS-DISP-CNT ' '
                  WS-DISP-AMT DELIMITED BY SIZE INTO RPT-LINE
           PERFORM 9100-OUT
           MOVE WS-PREM-MISSED TO WS-DISP-CNT
           MOVE SPACES TO RPT-LINE
           STRING 'PREMIUMS UNPAID    : ' WS-DISP-CNT
                  '  LAPSED ' WS-LAPSED DELIMITED BY SIZE INTO RPT-LINE
           PERFORM 9100-OUT
           MOVE WS-REMITTED TO WS-DISP-AMT
           MOVE SPACES TO RPT-LINE
           STRING 'REMITTED TO AMC/INSURERS : ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO RPT-LINE
           PERFORM 9100-OUT
           COMPUTE WS-VALUE = WS-PREM-COMM + WS-TRAIL
           MOVE WS-VALUE TO WS-DISP-AMT
           MOVE SPACES TO RPT-LINE
           STRING 'COMMISSION EARNED        : ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO RPT-LINE
           PERFORM 9100-OUT.

       9100-OUT.
           WRITE RPT-LINE
           DISPLAY FUNCTION TRIM(RPT-LINE TRAILING).
