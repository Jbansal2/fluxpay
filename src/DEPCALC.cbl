       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEPCALC.
      *================================================================*
      * PROGRAM  : DEPCALC                                             *
      * PURPOSE  : TERM DEPOSIT CALCULATIONS SHARED BY DEPMNT AND THE  *
      *            END-OF-DAY BATCH - RATE CARD LOOKUP, FD VALUE AND   *
      *            RD VALUE WITH QUARTERLY COMPOUNDING.                *
      * CALL     : CALL 'DEPCALC' USING DEPC-PARMS  (COPY DEPCPARM)    *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      * 2026-10-14 VERSIONED RATE CARD (FDRATE.DAT, MAINTAINED BY      *
      *            FDRTMNT) WITH A SEPARATE SENIOR CITIZEN COLUMN. THE *
      *            BUILT-IN CARD BELOW IS USED UNTIL A CARD IS SET UP. *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATE-FILE ASSIGN TO 'data/FDRATE.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS FR-KEY
               FILE STATUS IS WS-RATE-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  RATE-FILE.
           COPY FDRTREC.

       WORKING-STORAGE SECTION.
       01  WS-RATE-FS                  PIC X(02).
       01  WS-AS-OF                    PIC 9(08).
      * LATEST CARD WITH EFFECTIVE DATE <= AS-OF, AND ITS ANSWER
       01  WS-GROUP-DATE               PIC 9(08).
       01  WS-GROUP-FOUND              PIC X(01).
       01  WS-GROUP-RATE               PIC 9(02)V9(04).
       01  WS-EOF-SW                   PIC X(01).
      * RATE CARD - UPPER TENURE BOUND (MONTHS) AND RATE % P.A.
       01  WS-RATE-CARD.
           05  FILLER PIC X(09) VALUE '011065000'.
           05  FILLER PIC X(09) VALUE '023070000'.
           05  FILLER PIC X(09) VALUE '060072500'.
           05  FILLER PIC X(09) VALUE '120067500'.
       01  WS-RATE-CARD-R REDEFINES WS-RATE-CARD.
           05  WS-SLAB OCCURS 4 TIMES INDEXED BY SLB-IDX.
               10  SLB-MAX-MONTHS      PIC 9(03).
               10  SLB-RATE            PIC 9(02)V9(04).
       01  WS-SENIOR-EXTRA             PIC 9V9(04) VALUE 0.5000.

       01  WS-QTR-RATE                 COMP-2.
       01  WS-PERIODS                  COMP-2.
       01  WS-VALUE                    COMP-2.
       01  WS-I                        PIC 9(03).

       LINKAGE SECTION.
           COPY DEPCPARM.

       PROCEDURE DIVISION USING DEPC-PARMS.
       0000-MAIN.
           MOVE ZERO TO DC-RC
           EVALUATE DC-FUNCTION
               WHEN 'RT' PERFORM 1000-RATE-LOOKUP
               WHEN 'FV' PERFORM 2000-FD-VALUE
               WHEN 'RV' PERFORM 3000-RD-VALUE
               WHEN OTHER MOVE 99 TO DC-RC
           END-EVALUATE
           GOBACK.

       1000-RATE-LOOKUP.
           MOVE ZERO TO DC-RESULT-RATE DC-RATE-EFF-DATE
           IF DC-AS-OF-DATE IS NUMERIC AND DC-AS-OF-DATE > ZERO
               MOVE DC-AS-OF-DATE TO WS-AS-OF
           ELSE
               ACCEPT WS-AS-OF FROM DATE YYYYMMDD
           END-IF
           PERFORM 1100-VERSIONED-CARD
           IF WS-GROUP-DATE > ZERO
               IF WS-GROUP-FOUND = 'Y'
                   MOVE WS-GROUP-RATE TO DC-RESULT-RATE
                   MOVE WS-GROUP-DATE TO DC-RATE-EFF-DATE
               ELSE
                   MOVE 10 TO DC-RC
               END-IF
               EXIT PARAGRAPH
           END-IF
           PERFORM 1200-BUILT-IN-CARD.

      *    ROWS ARE IN KEY ORDER (DATE, THEN SLAB), SO THE FIRST SLAB
      *    OF EACH CARD THAT COVERS THE TENURE IS THE ANSWER FOR THAT
      *    CARD; THE LAST CARD NOT AFTER THE AS-OF DATE WINS.
       1100-VERSIONED-CARD.
           MOVE ZERO TO WS-GROUP-DATE
           MOVE 'N'  TO WS-GROUP-FOUND
           OPEN INPUT RATE-FILE
           IF WS-RATE-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ RATE-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N'
                   IF FR-EFF-DATE > WS-AS-OF
                       MOVE 'Y' TO WS-EOF-SW
                   ELSE
                       IF FR-EFF-DATE NOT = WS-GROUP-DATE
                           MOVE FR-EFF-DATE TO WS-GROUP-DATE
                           MOVE 'N' TO WS-GROUP-FOUND
                       END-IF
                       IF WS-GROUP-FOUND = 'N'
                          AND DC-TENURE <= FR-MAX-MONTHS
                           MOVE 'Y' TO WS-GROUP-FOUND
                           IF DC-SENIOR = 'Y'
                               MOVE FR-SNR-RATE TO WS-GROUP-RATE
                           ELSE
                               MOVE FR-GEN-RATE TO WS-GROUP-RATE
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           CLOSE RATE-FILE.

       1200-BUILT-IN-CARD.
           SET SLB-IDX TO 1
           SEARCH WS-SLAB
               AT END
                   MOVE 10 TO DC-RC
               WHEN DC-TENURE <= SLB-MAX-MONTHS(SLB-IDX)
                   MOVE SLB-RATE(SLB-IDX) TO DC-RESULT-RATE
           END-SEARCH
           IF DC-RC = ZERO AND DC-SENIOR = 'Y'
               ADD WS-SENIOR-EXTRA TO DC-RESULT-RATE
           END-IF.

      *    A = P * (1 + R/400) ** (MONTHS / 3)
       2000-FD-VALUE.
           COMPUTE WS-QTR-RATE = DC-RATE / 400
           COMPUTE WS-PERIODS  = DC-MONTHS / 3
           COMPUTE DC-RESULT-AMT ROUNDED =
                   DC-AMOUNT * (1 + WS-QTR-RATE) ** WS-PERIODS.

      *    EACH INSTALMENT I EARNS FOR (MONTHS - I + 1) MONTHS
       3000-RD-VALUE.
           COMPUTE WS-QTR-RATE = DC-RATE / 400
           MOVE ZERO TO WS-VALUE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > DC-COUNT
               IF DC-MONTHS >= WS-I
                   COMPUTE WS-PERIODS = (DC-MONTHS - WS-I + 1) / 3
               ELSE
                   MOVE ZERO TO WS-PERIODS
               END-IF
               COMPUTE WS-VALUE = WS-VALUE
                       + DC-AMOUNT * (1 + WS-QTR-RATE) ** WS-PERIODS
           END-PERFORM
           COMPUTE DC-RESULT-AMT ROUNDED = WS-VALUE.
