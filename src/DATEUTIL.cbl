       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATEUTIL.
      *================================================================*
      * PROGRAM  : DATEUTIL                                            *
      * PURPOSE  : DATE ARITHMETIC USED BY LOANS, DEPOSITS AND EOD.    *
      *            MONTH ADDITION CLAMPS TO THE LAST VALID DAY, E.G.   *
      *            31-JAN + 1 MONTH = 28/29-FEB.                       *
      * CALL     : CALL 'DATEUTIL' USING DATE-PARMS  (COPY DATEPARM)   *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-WORK-DATE                PIC 9(08).
       01  WS-WORK-DATE-R REDEFINES WS-WORK-DATE.
           05  WS-YYYY                 PIC 9(04).
           05  WS-MM                   PIC 9(02).
           05  WS-DD                   PIC 9(02).
       01  WS-TOTAL-MONTHS             PIC S9(07).
       01  WS-INT-1                    PIC S9(09).
       01  WS-INT-2                    PIC S9(09).
       01  WS-MAX-DAY                  PIC 9(02).

       01  WS-DAYS-IN-MONTH-TABLE.
           05  FILLER  PIC X(24) VALUE '312831303130313130313031'.
       01  WS-DIM-R REDEFINES WS-DAYS-IN-MONTH-TABLE.
           05  WS-DIM                  PIC 9(02) OCCURS 12 TIMES.

       LINKAGE SECTION.
           COPY DATEPARM.

       PROCEDURE DIVISION USING DATE-PARMS.
       0000-MAIN.
           SET DU-OK TO TRUE
           MOVE ZERO TO DU-RESULT-DATE DU-RESULT-DAYS
           EVALUATE TRUE
               WHEN DU-ADD-MONTHS    PERFORM 1000-ADD-MONTHS
               WHEN DU-ADD-DAYS      PERFORM 2000-ADD-DAYS
               WHEN DU-DAYS-BETWEEN  PERFORM 3000-DAYS-BETWEEN
               WHEN DU-VALIDATE
                   MOVE DU-DATE-1 TO WS-WORK-DATE
                   PERFORM 9000-CHECK-DATE
               WHEN OTHER            SET DU-INVALID-FUNCTION TO TRUE
           END-EVALUATE
           GOBACK.

       1000-ADD-MONTHS.
           MOVE DU-DATE-1 TO WS-WORK-DATE
           PERFORM 9000-CHECK-DATE
           IF NOT DU-OK
               EXIT PARAGRAPH
           END-IF
      *    WORK IN "MONTHS SINCE YEAR 0" SO NEGATIVE N ALSO WORKS
           COMPUTE WS-TOTAL-MONTHS =
                   WS-YYYY * 12 + (WS-MM - 1) + DU-NUMBER
           COMPUTE WS-YYYY = WS-TOTAL-MONTHS / 12
           COMPUTE WS-MM   = FUNCTION MOD(WS-TOTAL-MONTHS, 12) + 1
           PERFORM 9100-SET-MAX-DAY
           IF WS-DD > WS-MAX-DAY
               MOVE WS-MAX-DAY TO WS-DD
           END-IF
           MOVE WS-WORK-DATE TO DU-RESULT-DATE.

       2000-ADD-DAYS.
           MOVE DU-DATE-1 TO WS-WORK-DATE
           PERFORM 9000-CHECK-DATE
           IF NOT DU-OK
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-INT-1 = FUNCTION INTEGER-OF-DATE(DU-DATE-1)
                            + DU-NUMBER
           MOVE FUNCTION DATE-OF-INTEGER(WS-INT-1) TO DU-RESULT-DATE.

       3000-DAYS-BETWEEN.
           MOVE DU-DATE-1 TO WS-WORK-DATE
           PERFORM 9000-CHECK-DATE
           IF NOT DU-OK
               EXIT PARAGRAPH
           END-IF
           MOVE DU-DATE-2 TO WS-WORK-DATE
           PERFORM 9000-CHECK-DATE
           IF NOT DU-OK
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-INT-1 = FUNCTION INTEGER-OF-DATE(DU-DATE-1)
           COMPUTE WS-INT-2 = FUNCTION INTEGER-OF-DATE(DU-DATE-2)
           COMPUTE DU-RESULT-DAYS = WS-INT-2 - WS-INT-1.

       9000-CHECK-DATE.
           IF WS-YYYY < 1601 OR WS-MM < 1 OR WS-MM > 12
               SET DU-INVALID-DATE TO TRUE
               EXIT PARAGRAPH
           END-IF
           PERFORM 9100-SET-MAX-DAY
           IF WS-DD < 1 OR WS-DD > WS-MAX-DAY
               SET DU-INVALID-DATE TO TRUE
           END-IF.

       9100-SET-MAX-DAY.
           MOVE WS-DIM(WS-MM) TO WS-MAX-DAY
           IF WS-MM = 2
              AND FUNCTION MOD(WS-YYYY, 4) = 0
              AND (FUNCTION MOD(WS-YYYY, 100) NOT = 0
                   OR FUNCTION MOD(WS-YYYY, 400) = 0)
               MOVE 29 TO WS-MAX-DAY
           END-IF.
