       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCBATCH.
      *================================================================*
      * PROGRAM  : CCBATCH                                             *
      * PURPOSE  : CREDIT CARD BILLING. FOR EACH OPEN CARD, IN ORDER:  *
      *              A - AUTOPAY ON/AFTER THE DUE DATE (MINIMUM OR     *
      *                  FULL STATEMENT AMOUNT FROM THE LINKED ACCOUNT)*
      *              B - LATE FEE WHEN LESS THAN THE MINIMUM DUE WAS   *
      *                  PAID BY THE DUE DATE (ONCE PER CYCLE)         *
      *                  SLABS ON THE STATEMENT BALANCE: UP TO 500 NIL,*
      *                  5,000 -> 500, 10,000 -> 750, ABOVE -> 1,000   *
      *              C - STATEMENT ON THE STATEMENT DATE:              *
      *                  INTEREST 3.5% A MONTH ON THE PART OF THE LAST *
      *                  STATEMENT NOT PAID IN FULL, AND ON CASH       *
      *                  ADVANCES; 18% GST ON FEES AND INTEREST;       *
      *                  MINIMUM DUE = 5% (AT LEAST 200); DUE IN 20    *
      *                  DAYS; STATEMENT LINES TO data/CCSTMT_<YM>.TXT *
      *            EVERY STEP IS GUARDED SO A RE-RUN FOR THE SAME      *
      *            DATE CHANGES NOTHING.                               *
      * FILES    : CCMAST (I-O), CCTXN (INPUT), CCSTMT (EXTEND)        *
      * CALLS    : CCPOST, ACCTPOST, DATEUTIL                          *
      * RETURN   : 0 = CLEAN, 4 = LATE FEES OR FAILED AUTOPAYS         *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CC-FILE ASSIGN TO 'data/CCMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CC-NO
               FILE STATUS IS WS-CC-FS.
           SELECT CCT-FILE ASSIGN TO 'data/CCTXN.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CCT-FS.
           SELECT STMT-FILE ASSIGN TO WS-STMT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STMT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CC-FILE.
           COPY CCREC.
       FD  CCT-FILE.
           COPY CCTREC.
       FD  STMT-FILE.
       01  STMT-LINE                   PIC X(90).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY CCPPARM.
           COPY POSTPARM.
           COPY DATEPARM.

       01  WS-CC-FS                    PIC X(02).
           88  CC-FS-OK                          VALUE '00'.
       01  WS-CCT-FS                   PIC X(02).
       01  WS-STMT-FS                  PIC X(02).
       01  WS-STMT-FILENAME            PIC X(40).
       01  WS-STMT-OPEN-SW             PIC X(01) VALUE 'N'.

       01  WS-IN-RUN-DATE              PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-RETURN-CODE              PIC 9(02) VALUE ZERO.
       01  WS-CHANGED-SW               PIC X(01).
           88  CARD-CHANGED                      VALUE 'Y'.

       01  WS-AMOUNT                   PIC 9(11)V99.
       01  WS-PREV-STMT-DATE           PIC 9(08).
       01  WS-UNPAID                   PIC S9(11)V99.
       01  WS-INTEREST                 PIC 9(09)V99.
       01  WS-FEE                      PIC 9(07)V99.
       01  WS-GST                      PIC 9(07)V99.
       01  WS-INT-RATE                 PIC V9999 VALUE 0.0350.
       01  WS-GST-RATE                 PIC V99 VALUE 0.18.
       01  WS-MIN-PCT                  PIC V99 VALUE 0.05.
       01  WS-MIN-FLOOR                PIC 9(05)V99 VALUE 200.00.
       01  WS-DUE-DAYS                 PIC 9(03) VALUE 20.

       01  WS-STATS.
           05  WS-CARDS                PIC 9(05).
           05  WS-AUTOPAID             PIC 9(05).
           05  WS-AUTOPAY-FAILED       PIC 9(05).
           05  WS-LATE-FEES            PIC 9(05).
           05  WS-STATEMENTS           PIC 9(05).
           05  WS-INTEREST-TOT         PIC 9(11)V99.
           05  WS-FEE-TOT              PIC 9(11)V99.
           05  WS-BILLED-TOT           PIC S9(13)V99.

       01  WS-E-AMT                    PIC ZZ,ZZZ,ZZ9.99.
       01  WS-E-SAMT                   PIC -ZZ,ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           INITIALIZE WS-STATS
           MOVE ZERO TO WS-RETURN-CODE
           MOVE 'N' TO WS-STMT-OPEN-SW
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           DISPLAY 'BILLING RUN DATE YYYYMMDD (BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-RUN-DATE
           MOVE WS-CURR-DATE TO WS-RUN-DATE
           IF WS-IN-RUN-DATE IS NUMERIC
               MOVE WS-IN-RUN-DATE TO DU-DATE-1
               SET DU-VALIDATE TO TRUE
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DU-OK
                   MOVE WS-IN-RUN-DATE TO WS-RUN-DATE
               END-IF
           END-IF
           STRING 'data/CCSTMT_' WS-RUN-DATE(1:6) '.TXT'
                  DELIMITED BY SIZE INTO WS-STMT-FILENAME
           END-STRING
           OPEN I-O CC-FILE
           IF NOT CC-FS-OK
               DISPLAY 'NO CREDIT CARDS ON FILE.'
               MOVE ZERO TO RETURN-CODE
               GOBACK
           END-IF
           DISPLAY 'CREDIT CARD BILLING FOR ' WS-RUN-DATE
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ CC-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF NOT CC-CLOSED
                           PERFORM 1000-PROCESS-CARD
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CC-FILE
           IF WS-STMT-OPEN-SW = 'Y'
               CLOSE STMT-FILE
           END-IF
           PERFORM 9000-SUMMARY
           MOVE WS-RETURN-CODE TO RETURN-CODE
           GOBACK.

       1000-PROCESS-CARD.
           ADD 1 TO WS-CARDS
           MOVE 'N' TO WS-CHANGED-SW
           IF CC-DUE-DATE > ZERO AND WS-RUN-DATE >= CC-DUE-DATE
               PERFORM 2000-AUTOPAY
               PERFORM 3000-LATE-FEE
           END-IF
           IF WS-RUN-DATE >= CC-NEXT-STMT-DATE
               PERFORM 4000-STATEMENT
           END-IF
           IF CARD-CHANGED
               REWRITE CC-RECORD
           END-IF.

      *----------------------------------------------------------------*
      * A - AUTOPAY                                                    *
      *----------------------------------------------------------------*
       2000-AUTOPAY.
           IF CC-AUTOPAY-OFF OR CC-AUTOPAY-DONE = 'Y'
               EXIT PARAGRAPH
           END-IF
           MOVE 'Y' TO CC-AUTOPAY-DONE
           MOVE 'Y' TO WS-CHANGED-SW
           IF CC-AUTOPAY-FULL
               COMPUTE WS-UNPAID = CC-STMT-BALANCE - CC-PAID-SINCE-STMT
           ELSE
               COMPUTE WS-UNPAID = CC-MIN-DUE - CC-PAID-SINCE-STMT
           END-IF
           IF WS-UNPAID NOT > ZERO
               EXIT PARAGRAPH
           END-IF
           MOVE WS-UNPAID TO WS-AMOUNT
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE CC-PAY-ACCT TO PP-ACCT-NO
           MOVE WS-AMOUNT   TO PP-AMOUNT
           MOVE 'CP'        TO PP-TRAN-TYPE
           STRING 'CARD AUTOPAY ' CC-NO(13:4)
                  DELIMITED BY SIZE INTO PP-NARRATION
           END-STRING
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               ADD 1 TO WS-AUTOPAY-FAILED
               PERFORM 8900-WARN
               DISPLAY '  ' CC-NO ' AUTOPAY FAILED, RC=' PP-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           INITIALIZE CCP-PARMS
           MOVE 'Y'         TO CCP-TYPE CCP-FROM-ACCOUNT
           MOVE WS-AMOUNT   TO CCP-AMOUNT
           MOVE 'AUTOPAY'   TO CCP-MERCHANT
           MOVE WS-RUN-DATE TO CCP-RUN-DATE
           CALL 'CCPOST' USING CCP-PARMS CC-RECORD
           ADD 1 TO WS-AUTOPAID
           MOVE WS-AMOUNT TO WS-E-AMT
           DISPLAY '  ' CC-NO ' AUTOPAY ' WS-E-AMT.

      *----------------------------------------------------------------*
      * B - LATE FEE                                                   *
      *----------------------------------------------------------------*
       3000-LATE-FEE.
           IF CC-LATE-FEE-DONE = 'Y'
              OR WS-RUN-DATE <= CC-DUE-DATE
              OR CC-PAID-SINCE-STMT >= CC-MIN-DUE
               EXIT PARAGRAPH
           END-IF
           MOVE 'Y' TO CC-LATE-FEE-DONE
           MOVE 'Y' TO WS-CHANGED-SW
           EVALUATE TRUE
               WHEN CC-STMT-BALANCE <= 500    MOVE ZERO   TO WS-FEE
               WHEN CC-STMT-BALANCE <= 5000   MOVE 500.00 TO WS-FEE
               WHEN CC-STMT-BALANCE <= 10000  MOVE 750.00 TO WS-FEE
               WHEN OTHER                     MOVE 1000.00 TO WS-FEE
           END-EVALUATE
           IF WS-FEE = ZERO
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO CC-LATE-COUNT WS-LATE-FEES
           PERFORM 8900-WARN
           PERFORM 8000-POST-FEE-WITH-GST
           ADD WS-FEE TO WS-FEE-TOT
           MOVE WS-FEE TO WS-E-AMT
           DISPLAY '  ' CC-NO ' LATE FEE ' WS-E-AMT.

      *----------------------------------------------------------------*
      * C - STATEMENT                                                  *
      *----------------------------------------------------------------*
       4000-STATEMENT.
           MOVE 'Y' TO WS-CHANGED-SW
           ADD 1 TO WS-STATEMENTS
      *    INTEREST: UNPAID PART OF THE LAST BILL (REVOLVING CREDIT)
      *    PLUS CASH ADVANCES, WHICH NEVER GET AN INTEREST-FREE PERIOD
           COMPUTE WS-UNPAID = CC-STMT-BALANCE - CC-PAID-SINCE-STMT
           IF WS-UNPAID < ZERO
               MOVE ZERO TO WS-UNPAID
           END-IF
           COMPUTE WS-INTEREST ROUNDED =
                   (WS-UNPAID + CC-CASH-OUTSTANDING) * WS-INT-RATE
           IF WS-INTEREST > ZERO
               INITIALIZE CCP-PARMS
               MOVE 'I'           TO CCP-TYPE
               MOVE WS-INTEREST   TO CCP-AMOUNT
               MOVE 'FINANCE CHARGES' TO CCP-MERCHANT
               MOVE WS-RUN-DATE   TO CCP-RUN-DATE
               CALL 'CCPOST' USING CCP-PARMS CC-RECORD
               COMPUTE WS-GST ROUNDED = WS-INTEREST * WS-GST-RATE
               IF WS-GST > ZERO
                   INITIALIZE CCP-PARMS
                   MOVE 'G'         TO CCP-TYPE
                   MOVE WS-GST      TO CCP-AMOUNT
                   MOVE 'GST ON FINANCE CHARGES' TO CCP-MERCHANT
                   MOVE WS-RUN-DATE TO CCP-RUN-DATE
                   CALL 'CCPOST' USING CCP-PARMS CC-RECORD
               END-IF
               ADD WS-INTEREST TO WS-INTEREST-TOT
           END-IF

           MOVE CC-OUTSTANDING TO CC-STMT-BALANCE
           IF CC-OUTSTANDING NOT > ZERO
               MOVE ZERO TO CC-MIN-DUE
           ELSE
               COMPUTE CC-MIN-DUE ROUNDED = CC-OUTSTANDING * WS-MIN-PCT
               IF CC-MIN-DUE < WS-MIN-FLOOR
                   MOVE WS-MIN-FLOOR TO CC-MIN-DUE
               END-IF
               IF CC-MIN-DUE > CC-OUTSTANDING
                   MOVE CC-OUTSTANDING TO CC-MIN-DUE
               END-IF
           END-IF
           ADD CC-STMT-BALANCE TO WS-BILLED-TOT
           MOVE CC-LAST-STMT-DATE TO WS-PREV-STMT-DATE
           MOVE CC-NEXT-STMT-DATE TO CC-LAST-STMT-DATE
           SET DU-ADD-DAYS TO TRUE
           MOVE CC-LAST-STMT-DATE TO DU-DATE-1
           MOVE WS-DUE-DAYS TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO CC-DUE-DATE
           SET DU-ADD-MONTHS TO TRUE
           MOVE CC-NEXT-STMT-DATE TO DU-DATE-1
           MOVE 1 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO CC-NEXT-STMT-DATE
           MOVE ZERO TO CC-PAID-SINCE-STMT
           MOVE 'NN' TO CC-CYCLE-FLAGS
           IF CC-MIN-DUE = ZERO
      *        NOTHING TO PAY: NO LATE FEE OR AUTOPAY THIS CYCLE
               MOVE 'YY' TO CC-CYCLE-FLAGS
           END-IF
           PERFORM 4100-WRITE-STATEMENT.

       4100-WRITE-STATEMENT.
           IF WS-STMT-OPEN-SW NOT = 'Y'
               OPEN EXTEND STMT-FILE
               IF WS-STMT-FS = '35'
                   OPEN OUTPUT STMT-FILE
               END-IF
               MOVE 'Y' TO WS-STMT-OPEN-SW
           END-IF
           MOVE ALL '=' TO STMT-LINE
           WRITE STMT-LINE
           MOVE SPACES TO STMT-LINE
           STRING 'CREDIT CARD STATEMENT  ' CC-NO(1:4) ' XXXX XXXX '
                  CC-NO(13:4) '  ' CC-NAME
                  DELIMITED BY SIZE INTO STMT-LINE
           END-STRING
           WRITE STMT-LINE
           MOVE CC-STMT-BALANCE TO WS-E-SAMT
           MOVE CC-MIN-DUE TO WS-E-AMT
           MOVE SPACES TO STMT-LINE
           STRING 'STATEMENT DATE ' CC-LAST-STMT-DATE
                  '  TOTAL DUE ' FUNCTION TRIM(WS-E-SAMT)
                  '  MINIMUM ' FUNCTION TRIM(WS-E-AMT)
                  '  PAY BY ' CC-DUE-DATE
                  DELIMITED BY SIZE INTO STMT-LINE
           END-STRING
           WRITE STMT-LINE
      *    TRANSACTIONS OF THE CYCLE (ON OR BEFORE THE STATEMENT DATE)
           OPEN INPUT CCT-FILE
           IF WS-CCT-FS = '00'
               PERFORM FOREVER
                   READ CCT-FILE
                       AT END EXIT PERFORM
                   END-READ
                   IF CCT-CARD-NO = CC-NO
                      AND CCT-DATE <= WS-RUN-DATE
                      AND CCT-DATE >= WS-PREV-STMT-DATE
                       MOVE CCT-AMOUNT TO WS-E-AMT
                       MOVE SPACES TO STMT-LINE
                       STRING '  ' CCT-DATE '  ' CCT-TYPE '  '
                              WS-E-AMT '  ' CCT-MERCHANT
                              DELIMITED BY SIZE INTO STMT-LINE
                       END-STRING
                       WRITE STMT-LINE
                   END-IF
               END-PERFORM
               CLOSE CCT-FILE
           END-IF.

       8000-POST-FEE-WITH-GST.
           INITIALIZE CCP-PARMS
           MOVE 'F'         TO CCP-TYPE
           MOVE WS-FEE      TO CCP-AMOUNT
           MOVE 'LATE PAYMENT FEE' TO CCP-MERCHANT
           MOVE WS-RUN-DATE TO CCP-RUN-DATE
           CALL 'CCPOST' USING CCP-PARMS CC-RECORD
           COMPUTE WS-GST ROUNDED = WS-FEE * WS-GST-RATE
           INITIALIZE CCP-PARMS
           MOVE 'G'         TO CCP-TYPE
           MOVE WS-GST      TO CCP-AMOUNT
           MOVE 'GST ON LATE FEE' TO CCP-MERCHANT
           MOVE WS-RUN-DATE TO CCP-RUN-DATE
           CALL 'CCPOST' USING CCP-PARMS CC-RECORD.

       8900-WARN.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.

       9000-SUMMARY.
           DISPLAY '------------------------------------------------'
           DISPLAY 'CARDS          : ' WS-CARDS
           DISPLAY 'AUTOPAID       : ' WS-AUTOPAID '   FAILED '
                   WS-AUTOPAY-FAILED
           DISPLAY 'LATE FEES      : ' WS-LATE-FEES
           MOVE WS-INTEREST-TOT TO WS-E-AMT
           DISPLAY 'STATEMENTS     : ' WS-STATEMENTS
                   '   INTEREST ' WS-E-AMT
           MOVE WS-BILLED-TOT TO WS-E-SAMT
           DISPLAY 'TOTAL BILLED   : ' WS-E-SAMT
           DISPLAY 'RETURN CODE    : ' WS-RETURN-CODE.
