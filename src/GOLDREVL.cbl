       IDENTIFICATION DIVISION.
       PROGRAM-ID. GOLDREVL.
      *================================================================*
      * PROGRAM  : GOLDREVL                                            *
      * PURPOSE  : GOLD LOAN REVALUATION. VALUES EVERY OPEN GOLD LOAN  *
      *            AT THE CURRENT GOLD RATE AND WORKS OUT ITS          *
      *            LOAN-TO-VALUE (LTV):                                *
      *              LTV <= 75%          - OK (A MARGIN CALL CLEARS)   *
      *              LTV  > 75%          - MARGIN CALL: BORROWER MUST  *
      *                                    PAY DOWN OR ADD GOLD        *
      *              NPA, ANY LTV        - AUCTION NOTICE              *
      *            AN OPTIONAL NEW RATE ON INPUT IS SAVED FIRST.       *
      * INPUT    : NEW 24K RATE PER GRAM (BLANK = KEEP CURRENT RATE)   *
      * FILES    : LOANMAST (I-O), GOLDRATE (I-O)                      *
      * RETURN   : 0 = ALL WITHIN LTV, 4 = MARGIN CALLS OR AUCTIONS    *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS LOAN-NO
               FILE STATUS IS WS-LOAN-FS.
           SELECT RATE-FILE ASSIGN TO 'data/GOLDRATE.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RATE-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  RATE-FILE.
       01  RATE-RECORD.
           05  RATE-PER-GRAM           PIC 9(07)V99.
           05  RATE-SET-DATE           PIC 9(08).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
       01  WS-LOAN-FS                  PIC X(02).
           88  LOAN-FS-OK                        VALUE '00'.
       01  WS-RATE-FS                  PIC X(02).
       01  WS-IN-RATE                  PIC X(10).
       01  WS-GOLD-RATE                PIC 9(07)V99 VALUE 7000.00.
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-LTV                      PIC 9(05)V99.
       01  WS-LTV-LIMIT                PIC 9(03)V99 VALUE 75.00.
       01  WS-RETURN-CODE              PIC 9(02) VALUE ZERO.
       01  WS-STATS.
           05  WS-GOLD-LOANS           PIC 9(05).
           05  WS-MARGIN-CALLS         PIC 9(05).
           05  WS-CLEARED              PIC 9(05).
           05  WS-AUCTIONS             PIC 9(05).
           05  WS-TOTAL-OUT            PIC 9(13)V99.
           05  WS-TOTAL-VALUE          PIC 9(13)V99.
       01  WS-E-AMT                    PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-E-AMT2                   PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-E-LTV                    PIC ZZZZ9.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           INITIALIZE WS-STATS
           MOVE ZERO TO WS-RETURN-CODE
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           PERFORM 1000-RATE
           OPEN I-O LOAN-FILE
           IF NOT LOAN-FS-OK
               DISPLAY 'NO LOANS ON FILE.'
               MOVE ZERO TO RETURN-CODE
               GOBACK
           END-IF
           MOVE WS-GOLD-RATE TO WS-E-AMT
           DISPLAY 'GOLD LOAN REVALUATION AT ' FUNCTION TRIM(WS-E-AMT)
                   ' PER GRAM (24K)'
           DISPLAY 'LOAN NO       GRAMS  K    OUTSTANDING'
                   '         GOLD VALUE     LTV%  ACTION'
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ LOAN-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF LOAN-GOLD AND NOT LOAN-CLOSED
                           PERFORM 2000-REVALUE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE LOAN-FILE
           PERFORM 9000-SUMMARY
           MOVE WS-RETURN-CODE TO RETURN-CODE
           GOBACK.

       1000-RATE.
           OPEN INPUT RATE-FILE
           IF WS-RATE-FS = '00'
               READ RATE-FILE
                   NOT AT END
                       IF RATE-PER-GRAM IS NUMERIC
                           MOVE RATE-PER-GRAM TO WS-GOLD-RATE
                       END-IF
               END-READ
               CLOSE RATE-FILE
           END-IF
           DISPLAY 'NEW 24K RATE PER GRAM (BLANK = KEEP): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-RATE
           IF WS-IN-RATE NOT = SPACES
              AND FUNCTION TEST-NUMVAL(WS-IN-RATE) = ZERO
               COMPUTE WS-GOLD-RATE = FUNCTION NUMVAL(WS-IN-RATE)
               MOVE WS-GOLD-RATE TO RATE-PER-GRAM
               MOVE WS-CURR-DATE TO RATE-SET-DATE
               OPEN OUTPUT RATE-FILE
               WRITE RATE-RECORD
               CLOSE RATE-FILE
           END-IF.

       2000-REVALUE.
           ADD 1 TO WS-GOLD-LOANS
           COMPUTE LOAN-COLLATERAL-VALUE ROUNDED =
                   LOAN-GOLD-GRAMS * WS-GOLD-RATE * LOAN-GOLD-CARAT / 24
           IF LOAN-COLLATERAL-VALUE > ZERO
               COMPUTE WS-LTV ROUNDED =
                       LOAN-OUTSTANDING * 100 / LOAN-COLLATERAL-VALUE
           ELSE
               MOVE 99999.99 TO WS-LTV
           END-IF
           ADD LOAN-OUTSTANDING      TO WS-TOTAL-OUT
           ADD LOAN-COLLATERAL-VALUE TO WS-TOTAL-VALUE
           MOVE LOAN-OUTSTANDING      TO WS-E-AMT
           MOVE LOAN-COLLATERAL-VALUE TO WS-E-AMT2
           MOVE WS-LTV TO WS-E-LTV
           EVALUATE TRUE
               WHEN LOAN-NPA
                   ADD 1 TO WS-AUCTIONS
                   PERFORM 8900-WARN
                   DISPLAY LOAN-NO ' ' LOAN-GOLD-GRAMS ' '
                           LOAN-GOLD-CARAT
                           WS-E-AMT WS-E-AMT2 ' ' WS-E-LTV
                           '  AUCTION NOTICE (NPA)'
               WHEN WS-LTV > WS-LTV-LIMIT
                   IF NOT LOAN-IN-MARGIN-CALL
                       ADD 1 TO WS-MARGIN-CALLS
                   END-IF
                   MOVE 'Y' TO LOAN-MARGIN-CALL
                   PERFORM 8900-WARN
                   DISPLAY LOAN-NO ' ' LOAN-GOLD-GRAMS ' '
                           LOAN-GOLD-CARAT
                           WS-E-AMT WS-E-AMT2 ' ' WS-E-LTV
                           '  MARGIN CALL'
               WHEN OTHER
                   IF LOAN-IN-MARGIN-CALL
                       ADD 1 TO WS-CLEARED
                       DISPLAY LOAN-NO ' ' LOAN-GOLD-GRAMS ' '
                               LOAN-GOLD-CARAT WS-E-AMT WS-E-AMT2 ' '
                               WS-E-LTV '  MARGIN CALL CLEARED'
                   END-IF
                   MOVE 'N' TO LOAN-MARGIN-CALL
           END-EVALUATE
           REWRITE LOAN-RECORD.

       8900-WARN.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.

       9000-SUMMARY.
           DISPLAY '------------------------------------------------'
           DISPLAY 'GOLD LOANS          : ' WS-GOLD-LOANS
           DISPLAY 'NEW MARGIN CALLS    : ' WS-MARGIN-CALLS
           DISPLAY 'MARGIN CALLS CLEARED: ' WS-CLEARED
           DISPLAY 'AUCTION NOTICES     : ' WS-AUCTIONS
           MOVE WS-TOTAL-OUT TO WS-E-AMT
           MOVE WS-TOTAL-VALUE TO WS-E-AMT2
           DISPLAY 'PORTFOLIO           : ' WS-E-AMT ' AGAINST GOLD '
                   WS-E-AMT2
           DISPLAY 'RETURN CODE         : ' WS-RETURN-CODE.
