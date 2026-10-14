       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNRESET.
      *================================================================*
      * PROGRAM  : LNRESET                                           *
      * PURPOSE  : RESETS EVERY ACTIVE REPO-LINKED LOAN TO THE CURRENT *
      *            REPO RATE + ITS OWN SPREAD (SPREAD IS FIXED AT      *
      *            DISBURSAL, ONLY THE BENCHMARK MOVES).               *
      *            RESET PREFERENCE T (DEFAULT) KEEPS THE EMI AND      *
      *            RECALCULATES THE REMAINING TENURE:                  *
      *              N = -LN(1 - P*R/EMI) / LN(1+R), ROUNDED UP        *
      *            IF THE OLD EMI NO LONGER COVERS THE MONTHLY         *
      *            INTEREST (P*R >= EMI) OR THE NEW TENURE WOULD PASS  *
      *            THE 360-MONTH PRODUCT LIMIT, THE EMI IS RAISED      *
      *            INSTEAD - A LOAN MAY NEVER GO INTO NEGATIVE         *
      *            AMORTISATION. PREFERENCE E KEEPS THE TENURE AND     *
      *            RECALCULATES THE EMI.                               *
      *            NPA LOANS ARE REPRICED TOO (INTEREST STILL ACCRUES).*
      * INPUT    : RUN DATE YYYYMMDD (BLANK = TODAY)                   *
      * OUTPUT   : data/REPRICE_<DATE>.TXT                             *
      * FILES    : LOANMAST (I-O)                                      *
      * CALLS    : RATEUTIL, AUDITLOG                                  *
      * RETURN   : 0 = OK, 4 = SOME EMIS RAISED, 12 = FILE ERROR       *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS SEQUENTIAL
               RECORD KEY IS LOAN-NO FILE STATUS IS WS-LOAN-FS.
           SELECT RPT-FILE ASSIGN TO WS-RPT-NAME
               ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-LOAN-FS                  PIC X(02).
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-NAME                 PIC X(40).
       01  WS-IN-RUN-DATE              PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-EOF-SW                   PIC X(01) VALUE 'N'.
           88  END-OF-LOANS                      VALUE 'Y'.
           COPY RATEPARM.
           COPY AUDPARM.
       01  WS-REPO                     PIC 9(02)V9(04).
       01  WS-NEW-RATE                 PIC 9(02)V9(04).
       01  WS-OLD-RATE                 PIC 9(02)V9(04).
       01  WS-OLD-EMI                  PIC 9(09)V99.
       01  WS-OLD-TENURE               PIC 9(03).
       01  WS-REMAINING                PIC S9(05).
       01  WS-MAX-TENURE               PIC 9(03) VALUE 360.
       01  WS-R                        COMP-2.
       01  WS-PR                       COMP-2.
       01  WS-POWER                    COMP-2.
       01  WS-N-FLOAT                  COMP-2.
       01  WS-N                        PIC S9(05).
       01  WS-EMI                      PIC 9(09)V99.
       01  WS-ACTION                   PIC X(12).
       01  WS-COUNTS.
           05  WS-READ                 PIC 9(06) VALUE ZERO.
           05  WS-REPRICED             PIC 9(06) VALUE ZERO.
           05  WS-TENURE-CHANGED       PIC 9(06) VALUE ZERO.
           05  WS-EMI-CHANGED          PIC 9(06) VALUE ZERO.
           05  WS-EMI-FORCED           PIC 9(06) VALUE ZERO.
       01  WS-LINE                     PIC X(100).
       01  ED-RATE                     PIC Z9.9999.
       01  ED-RATE2                    PIC Z9.9999.
       01  ED-EMI                      PIC ZZZ,ZZ9.99.
       01  ED-EMI2                     PIC ZZZ,ZZ9.99.
       01  ED-AMT                      PIC ZZ,ZZZ,ZZ9.99.
       01  ED-TEN                      PIC ZZ9.
       01  ED-TEN2                     PIC ZZ9.
       01  ED-CNT                      PIC ZZZ,ZZ9.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'RUN DATE (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-RUN-DATE
           IF WS-IN-RUN-DATE = SPACES OR WS-IN-RUN-DATE NOT NUMERIC
               ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           ELSE
               MOVE WS-IN-RUN-DATE TO WS-RUN-DATE
           END-IF
           MOVE 'REPO' TO RTP-CODE
           CALL 'RATEUTIL' USING RATE-PARMS
           MOVE RTP-VALUE TO WS-REPO
           MOVE WS-REPO TO ED-RATE

           OPEN I-O LOAN-FILE
           IF WS-LOAN-FS = '35'
               DISPLAY 'LNRESET: NO LOAN MASTER - NOTHING TO DO'
               GOBACK
           END-IF
           IF WS-LOAN-FS NOT = '00'
               DISPLAY 'LNRESET: LOAN MASTER OPEN FAILED, FS='
                       WS-LOAN-FS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           STRING 'data/REPRICE_' WS-RUN-DATE '.TXT'
               DELIMITED BY SIZE INTO WS-RPT-NAME
           OPEN OUTPUT RPT-FILE
           MOVE SPACES TO WS-LINE
           STRING 'REPO-LINKED LOAN RESET  RUN ' WS-RUN-DATE
                  '  REPO ' ED-RATE '%' DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           STRING 'LOAN NO      OUTSTANDING OLD RATE  NEW RATE'
                  '     OLD EMI     NEW EMI OLD NEW ACTION'
                  DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT

           PERFORM UNTIL END-OF-LOANS
               READ LOAN-FILE NEXT RECORD
                   AT END SET END-OF-LOANS TO TRUE
                   NOT AT END
                       ADD 1 TO WS-READ
                       IF LOAN-REPO-LINKED AND NOT LOAN-CLOSED
                           PERFORM 2000-REPRICE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE LOAN-FILE

           PERFORM 9000-TOTALS
           CLOSE RPT-FILE
           IF WS-EMI-FORCED > ZERO
               MOVE 4 TO RETURN-CODE
           END-IF
           GOBACK.

       2000-REPRICE.
           COMPUTE WS-NEW-RATE = WS-REPO + LOAN-SPREAD
           IF WS-NEW-RATE = LOAN-RATE
               EXIT PARAGRAPH
           END-IF
           MOVE LOAN-RATE   TO WS-OLD-RATE
           MOVE LOAN-EMI    TO WS-OLD-EMI
           MOVE LOAN-TENURE TO WS-OLD-TENURE
           COMPUTE WS-REMAINING = LOAN-TENURE - LOAN-EMIS-PAID
           IF WS-REMAINING < 1
               MOVE 1 TO WS-REMAINING
           END-IF
           MOVE WS-NEW-RATE TO LOAN-RATE
           COMPUTE WS-R  = WS-NEW-RATE / 1200
           COMPUTE WS-PR = LOAN-OUTSTANDING * WS-R

           IF LOAN-RESET-EMI
               PERFORM 2100-NEW-EMI
               MOVE 'EMI' TO WS-ACTION
               ADD 1 TO WS-EMI-CHANGED
           ELSE
               PERFORM 2200-NEW-TENURE
           END-IF

           REWRITE LOAN-RECORD
               INVALID KEY
                   DISPLAY 'LNRESET: REWRITE FAILED ' LOAN-NO
                           ' FS=' WS-LOAN-FS
                   EXIT PARAGRAPH
           END-REWRITE
           ADD 1 TO WS-REPRICED
           PERFORM 3000-REPORT-LINE
           MOVE 'LNRESET' TO AU-PROGRAM
           MOVE 'RATE RESET'  TO AU-ACTION
           MOVE LOAN-NO       TO AU-KEY
           MOVE SPACES        TO AU-DETAIL
           MOVE WS-OLD-RATE   TO ED-RATE
           MOVE WS-NEW-RATE   TO ED-RATE2
           STRING ED-RATE ' TO ' ED-RATE2 ' ' WS-ACTION
               DELIMITED BY SIZE INTO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS.

      *    KEEP THE TENURE: EMI ON THE OUTSTANDING OVER THE MONTHS LEFT
       2100-NEW-EMI.
           COMPUTE WS-POWER = (1 + WS-R) ** WS-REMAINING
           COMPUTE WS-EMI ROUNDED =
                   WS-PR * WS-POWER / (WS-POWER - 1)
           MOVE WS-EMI TO LOAN-EMI.

      *    KEEP THE EMI: SOLVE FOR THE MONTHS LEFT
       2200-NEW-TENURE.
           IF LOAN-EMI > WS-PR
               COMPUTE WS-N-FLOAT =
                   - FUNCTION LOG(1 - WS-PR / LOAN-EMI)
                   / FUNCTION LOG(1 + WS-R)
               COMPUTE WS-N = FUNCTION INTEGER(WS-N-FLOAT)
               IF WS-N < WS-N-FLOAT
                   ADD 1 TO WS-N
               END-IF
           ELSE
               MOVE 99999 TO WS-N
           END-IF
           IF LOAN-EMIS-PAID + WS-N > WS-MAX-TENURE
      *        EMI NO LONGER ENOUGH - RAISE IT OVER THE MAXIMUM TENURE
               COMPUTE WS-REMAINING = WS-MAX-TENURE - LOAN-EMIS-PAID
               IF WS-REMAINING < 1
                   MOVE 1 TO WS-REMAINING
               END-IF
               PERFORM 2100-NEW-EMI
               COMPUTE LOAN-TENURE = LOAN-EMIS-PAID + WS-REMAINING
               MOVE 'EMI RAISED' TO WS-ACTION
               ADD 1 TO WS-EMI-FORCED
           ELSE
               COMPUTE LOAN-TENURE = LOAN-EMIS-PAID + WS-N
               MOVE 'TENURE' TO WS-ACTION
               ADD 1 TO WS-TENURE-CHANGED
           END-IF.

       3000-REPORT-LINE.
           MOVE LOAN-OUTSTANDING TO ED-AMT
           MOVE WS-OLD-RATE      TO ED-RATE
           MOVE LOAN-RATE        TO ED-RATE2
           MOVE WS-OLD-EMI       TO ED-EMI
           MOVE LOAN-EMI         TO ED-EMI2
           MOVE WS-OLD-TENURE    TO ED-TEN
           MOVE LOAN-TENURE      TO ED-TEN2
           MOVE SPACES TO WS-LINE
           STRING LOAN-NO ' ' ED-AMT ' ' ED-RATE '  ' ED-RATE2 ' '
                  ED-EMI ' ' ED-EMI2 ' ' ED-TEN ' ' ED-TEN2 ' '
                  WS-ACTION DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT.

       8000-PUT.
           DISPLAY FUNCTION TRIM(WS-LINE TRAILING)
           WRITE RPT-LINE FROM WS-LINE
           MOVE SPACES TO WS-LINE.

       9000-TOTALS.
           PERFORM 8000-PUT
           MOVE WS-READ TO ED-CNT
           STRING 'LOANS READ         : ' ED-CNT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-REPRICED TO ED-CNT
           STRING 'LOANS REPRICED     : ' ED-CNT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-TENURE-CHANGED TO ED-CNT
           STRING '  TENURE CHANGED   : ' ED-CNT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-EMI-CHANGED TO ED-CNT
           STRING '  EMI CHANGED      : ' ED-CNT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-EMI-FORCED TO ED-CNT
           STRING '  EMI RAISED (CAP) : ' ED-CNT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT.
