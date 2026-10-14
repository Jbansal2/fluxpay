       IDENTIFICATION DIVISION.
       PROGRAM-ID. YREND.
      *================================================================*
      * PROGRAM  : YREND                                               *
      * PURPOSE  : FINANCIAL YEAR-END CLOSING (YEAR ENDS 31 MARCH).    *
      *            1. THE LEDGER MUST BE IN BALANCE.                   *
      *            2. EVERY INCOME AND EXPENSE GL IS CLOSED TO 3003    *
      *               PROFIT AND LOSS ACCOUNT, SO THE NEW YEAR STARTS  *
      *               WITH ZERO INCOME AND EXPENSE.                    *
      *            3. A PROFIT IS APPROPRIATED: 25% TO 3004 STATUTORY  *
      *               RESERVE (BANKING REGULATION ACT S.17), THE REST  *
      *               TO 3002 GENERAL RESERVE. A LOSS IS ABSORBED BY   *
      *               THE GENERAL RESERVE. 3003 ENDS AT ZERO.          *
      *            4. PROFIT AND LOSS STATEMENT AND BALANCE SHEET      *
      *               SUMMARY ARE PRINTED; THE YEAR IS LOGGED AND      *
      *               CANNOT BE CLOSED TWICE (NOR AN EARLIER YEAR      *
      *               AFTER A LATER ONE).                              *
      * INPUT    : YEAR-END DATE YYYY0331                              *
      * OUTPUT   : data/YE_<YYYY>.TXT, data/YRENDLOG.DAT               *
      * FILES    : GLMAST (INPUT), YRENDLOG (EXTEND / INPUT)           *
      * CALLS    : GLPOST, AUDITLOG                                    *
      * RC       : 0 CLOSED, 8 REFUSED (DATE / ALREADY CLOSED),        *
      *            12 LEDGER OUT OF BALANCE OR FILE ERROR              *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GL-FILE ASSIGN TO 'data/GLMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS GL-CODE
               FILE STATUS IS WS-GL-FS.
           SELECT LOG-FILE ASSIGN TO 'data/YRENDLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LOG-FS.
           SELECT RPT-FILE ASSIGN TO WS-RPT-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  GL-FILE.
           COPY GLREC.
       FD  LOG-FILE.
       01  YE-LOG-RECORD.
           05  YL-YEAR-END             PIC 9(08).
           05  YL-RUN-DATE             PIC 9(08).
           05  YL-NET                  PIC S9(15)V99
                                       SIGN LEADING SEPARATE.
           05  YL-USER                 PIC X(08).
           05  FILLER                  PIC X(10).
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(90).

       WORKING-STORAGE SECTION.
           COPY SESSION.
           COPY GLPARM.
           COPY GLCODES.
           COPY AUDPARM.
       01  WS-GL-FS                    PIC X(02).
       01  WS-LOG-FS                   PIC X(02).
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-NAME                 PIC X(40).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-YEAR-END                 PIC 9(08).
       01  WS-YEAR-END-R REDEFINES WS-YEAR-END.
           05  WS-YE-YYYY              PIC 9(04).
           05  WS-YE-MMDD              PIC 9(04).
       01  WS-TODAY                    PIC 9(08).
       01  WS-EOF-SW                   PIC X(01).
       01  WS-RC                       PIC 9(02) VALUE ZERO.

      * INCOME / EXPENSE GLS TO CLOSE
       01  WS-PL-CNT                   PIC 9(03) VALUE ZERO.
       01  WS-PL-TABLE.
           05  WS-PL OCCURS 100 TIMES.
               10  PL-CODE             PIC 9(04).
               10  PL-NAME             PIC X(30).
               10  PL-TYPE             PIC X(01).
               10  PL-BALANCE          PIC S9(15)V99.
       01  WS-I                        PIC 9(03).

       01  WS-SUM-ALL                  PIC S9(15)V99.
       01  WS-INCOME                   PIC S9(15)V99.
       01  WS-EXPENSE                  PIC S9(15)V99.
       01  WS-NET                      PIC S9(15)V99.
       01  WS-STAT-RES                 PIC S9(15)V99.
       01  WS-GEN-RES                  PIC S9(15)V99.
       01  WS-STAT-PCT                 PIC 9(03) VALUE 25.
       01  WS-ASSETS                   PIC S9(15)V99.
       01  WS-LIABS                    PIC S9(15)V99.
       01  WS-CAPITAL                  PIC S9(15)V99.
       01  WS-AMOUNT                   PIC S9(15)V99.
       01  WS-DISP-AMT                 PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           DISPLAY 'FINANCIAL YEAR END (YYYY0331): ' WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           IF WS-IN-DATE IS NOT NUMERIC
               DISPLAY 'YREND: YEAR-END DATE REQUIRED.'
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF
           MOVE WS-IN-DATE TO WS-YEAR-END
           IF WS-YE-MMDD NOT = 0331
               DISPLAY 'YREND: THE FINANCIAL YEAR ENDS ON 31 MARCH.'
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF
           PERFORM 1000-CHECK-LOG
           IF WS-RC NOT = ZERO
               MOVE WS-RC TO RETURN-CODE
               GOBACK
           END-IF
           PERFORM 2000-LOAD-LEDGER
           IF WS-RC NOT = ZERO
               MOVE WS-RC TO RETURN-CODE
               GOBACK
           END-IF
           STRING 'data/YE_' WS-YE-YYYY '.TXT'
                  DELIMITED BY SIZE INTO WS-RPT-NAME
           OPEN OUTPUT RPT-FILE
           PERFORM 3000-PROFIT-AND-LOSS
           PERFORM 4000-CLOSE-TO-PL
           PERFORM 5000-APPROPRIATE
           PERFORM 6000-BALANCE-SHEET
           CLOSE RPT-FILE
           PERFORM 7000-LOG-YEAR
           MOVE WS-RC TO RETURN-CODE
           GOBACK.

      *----------------------------------------------------------------*
      * ONE CLOSING PER YEAR, IN ORDER                                 *
      *----------------------------------------------------------------*
       1000-CHECK-LOG.
           OPEN INPUT LOG-FILE
           IF WS-LOG-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ LOG-FILE
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N' AND YL-YEAR-END >= WS-YEAR-END
                   DISPLAY 'YREND: YEAR ENDING ' YL-YEAR-END
                           ' WAS ALREADY CLOSED ON ' YL-RUN-DATE
                           ' - ' WS-YEAR-END ' CANNOT BE CLOSED.'
                   MOVE 8 TO WS-RC
                   MOVE 'Y' TO WS-EOF-SW
               END-IF
           END-PERFORM
           CLOSE LOG-FILE.

      *----------------------------------------------------------------*
      * READ THE CHART: TRIAL BALANCE AND THE GLS TO CLOSE             *
      *----------------------------------------------------------------*
       2000-LOAD-LEDGER.
           OPEN INPUT GL-FILE
           IF WS-GL-FS NOT = '00'
               DISPLAY 'YREND: LEDGER OPEN FAILED FS=' WS-GL-FS
               MOVE 12 TO WS-RC
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO WS-SUM-ALL WS-INCOME WS-EXPENSE
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ GL-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N'
                   ADD GL-BALANCE TO WS-SUM-ALL
                   IF (GL-INCOME OR GL-EXPENSE) AND WS-PL-CNT < 100
                       ADD 1 TO WS-PL-CNT
                       MOVE GL-CODE    TO PL-CODE(WS-PL-CNT)
                       MOVE GL-NAME    TO PL-NAME(WS-PL-CNT)
                       MOVE GL-TYPE    TO PL-TYPE(WS-PL-CNT)
                       MOVE GL-BALANCE TO PL-BALANCE(WS-PL-CNT)
                       IF GL-INCOME
                           SUBTRACT GL-BALANCE FROM WS-INCOME
                       ELSE
                           ADD GL-BALANCE TO WS-EXPENSE
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           CLOSE GL-FILE
           IF WS-SUM-ALL NOT = ZERO
               MOVE WS-SUM-ALL TO WS-DISP-AMT
               DISPLAY 'YREND: TRIAL BALANCE OUT BY ' WS-DISP-AMT
                       ' - YEAR NOT CLOSED.'
               MOVE 12 TO WS-RC
           END-IF
           COMPUTE WS-NET = WS-INCOME - WS-EXPENSE.

      *----------------------------------------------------------------*
      * STATEMENT                                                      *
      *----------------------------------------------------------------*
       3000-PROFIT-AND-LOSS.
           MOVE SPACES TO RPT-LINE
           STRING 'PROFIT AND LOSS ACCOUNT - YEAR ENDED ' WS-YEAR-END
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE 'INCOME' TO RPT-LINE
           WRITE RPT-LINE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-PL-CNT
               IF PL-TYPE(WS-I) = 'I'
                   COMPUTE WS-AMOUNT = 0 - PL-BALANCE(WS-I)
                   PERFORM 3100-PL-LINE
               END-IF
           END-PERFORM
           MOVE WS-INCOME TO WS-AMOUNT
           MOVE 'TOTAL INCOME' TO PL-NAME(100)
           MOVE ZERO TO PL-CODE(100)
           MOVE 100 TO WS-I
           PERFORM 3100-PL-LINE
           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE 'EXPENDITURE' TO RPT-LINE
           WRITE RPT-LINE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-PL-CNT
               IF PL-TYPE(WS-I) = 'E'
                   MOVE PL-BALANCE(WS-I) TO WS-AMOUNT
                   PERFORM 3100-PL-LINE
               END-IF
           END-PERFORM
           MOVE WS-EXPENSE TO WS-AMOUNT
           MOVE 'TOTAL EXPENDITURE' TO PL-NAME(100)
           MOVE 100 TO WS-I
           PERFORM 3100-PL-LINE
           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-NET TO WS-AMOUNT
           IF WS-NET < ZERO
               MOVE 'NET LOSS FOR THE YEAR' TO PL-NAME(100)
           ELSE
               MOVE 'NET PROFIT FOR THE YEAR' TO PL-NAME(100)
           END-IF
           MOVE 100 TO WS-I
           PERFORM 3100-PL-LINE
           DISPLAY FUNCTION TRIM(RPT-LINE TRAILING).

       3100-PL-LINE.
           MOVE WS-AMOUNT TO WS-DISP-AMT
           MOVE SPACES TO RPT-LINE
           IF PL-CODE(WS-I) = ZERO
               STRING '      ' PL-NAME(WS-I) '      ' WS-DISP-AMT
                      DELIMITED BY SIZE INTO RPT-LINE
           ELSE
               STRING '  ' PL-CODE(WS-I) ' ' PL-NAME(WS-I) '      '
                      WS-DISP-AMT DELIMITED BY SIZE INTO RPT-LINE
           END-IF
           WRITE RPT-LINE.

      *----------------------------------------------------------------*
      * CLOSING ENTRIES: EVERY I / E GL TO ZERO AGAINST 3003           *
      *----------------------------------------------------------------*
       4000-CLOSE-TO-PL.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-PL-CNT
               IF PL-BALANCE(WS-I) NOT = ZERO
      *            CREDIT BALANCE (NORMAL INCOME): DEBIT THE GL
                   IF PL-BALANCE(WS-I) < ZERO
                       MOVE PL-CODE(WS-I)       TO GP-DR-GL
                       MOVE GLC-PROFIT-AND-LOSS TO GP-CR-GL
                       COMPUTE GP-AMOUNT = 0 - PL-BALANCE(WS-I)
                   ELSE
                       MOVE GLC-PROFIT-AND-LOSS TO GP-DR-GL
                       MOVE PL-CODE(WS-I)       TO GP-CR-GL
                       MOVE PL-BALANCE(WS-I)    TO GP-AMOUNT
                   END-IF
                   MOVE 'YEAR-END CLOSING' TO GP-NARRATION
                   PERFORM 8000-POST
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      * APPROPRIATION OF THE RESULT                                    *
      *----------------------------------------------------------------*
       5000-APPROPRIATE.
           MOVE ZERO TO WS-STAT-RES WS-GEN-RES
           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE 'APPROPRIATION' TO RPT-LINE
           WRITE RPT-LINE
           EVALUATE TRUE
               WHEN WS-NET > ZERO
                   COMPUTE WS-STAT-RES ROUNDED =
                           WS-NET * WS-STAT-PCT / 100
                   COMPUTE WS-GEN-RES = WS-NET - WS-STAT-RES
                   MOVE GLC-PROFIT-AND-LOSS   TO GP-DR-GL
                   MOVE GLC-STATUTORY-RESERVE TO GP-CR-GL
                   MOVE WS-STAT-RES           TO GP-AMOUNT
                   MOVE 'TO STATUTORY RESERVE 25%' TO GP-NARRATION
                   PERFORM 8000-POST
                   MOVE GLC-PROFIT-AND-LOSS TO GP-DR-GL
                   MOVE GLC-GENERAL-RESERVE TO GP-CR-GL
                   MOVE WS-GEN-RES          TO GP-AMOUNT
                   MOVE 'BALANCE TO GENERAL RESERVE' TO GP-NARRATION
                   PERFORM 8000-POST
                   MOVE WS-STAT-RES TO WS-DISP-AMT
                   MOVE SPACES TO RPT-LINE
                   STRING '  TRANSFER TO STATUTORY RESERVE (25%)  '
                          WS-DISP-AMT DELIMITED BY SIZE INTO RPT-LINE
                   WRITE RPT-LINE
                   DISPLAY FUNCTION TRIM(RPT-LINE)
                   MOVE WS-GEN-RES TO WS-DISP-AMT
                   MOVE SPACES TO RPT-LINE
                   STRING '  TRANSFER TO GENERAL RESERVE          '
                          WS-DISP-AMT DELIMITED BY SIZE INTO RPT-LINE
                   WRITE RPT-LINE
                   DISPLAY FUNCTION TRIM(RPT-LINE)
               WHEN WS-NET < ZERO
                   MOVE GLC-GENERAL-RESERVE TO GP-DR-GL
                   MOVE GLC-PROFIT-AND-LOSS TO GP-CR-GL
                   COMPUTE GP-AMOUNT = 0 - WS-NET
                   MOVE 'LOSS ABSORBED BY RESERVE' TO GP-NARRATION
                   PERFORM 8000-POST
                   MOVE GP-AMOUNT TO WS-DISP-AMT
                   MOVE SPACES TO RPT-LINE
                   STRING '  LOSS ABSORBED BY GENERAL RESERVE     '
                          WS-DISP-AMT DELIMITED BY SIZE INTO RPT-LINE
                   WRITE RPT-LINE
                   DISPLAY FUNCTION TRIM(RPT-LINE)
               WHEN OTHER
                   MOVE '  NO PROFIT OR LOSS' TO RPT-LINE
                   WRITE RPT-LINE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BALANCE SHEET SUMMARY AFTER CLOSING                            *
      *----------------------------------------------------------------*
       6000-BALANCE-SHEET.
           MOVE ZERO TO WS-ASSETS WS-LIABS WS-CAPITAL WS-SUM-ALL
                        WS-AMOUNT
           OPEN INPUT GL-FILE
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ GL-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N'
                   ADD GL-BALANCE TO WS-SUM-ALL
                   EVALUATE TRUE
                       WHEN GL-ASSET     ADD GL-BALANCE TO WS-ASSETS
                       WHEN GL-LIABILITY SUBTRACT GL-BALANCE
                                             FROM WS-LIABS
                       WHEN GL-CAPITAL   SUBTRACT GL-BALANCE
                                             FROM WS-CAPITAL
                       WHEN OTHER        ADD GL-BALANCE TO WS-AMOUNT
                   END-EVALUATE
               END-IF
           END-PERFORM
           CLOSE GL-FILE
           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE 'BALANCE SHEET SUMMARY AFTER CLOSING' TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-ASSETS TO WS-DISP-AMT
           MOVE SPACES TO RPT-LINE
           STRING '  ASSETS                               ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-LIABS TO WS-DISP-AMT
           MOVE SPACES TO RPT-LINE
           STRING '  LIABILITIES                          ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-CAPITAL TO WS-DISP-AMT
           MOVE SPACES TO RPT-LINE
           STRING '  CAPITAL AND RESERVES                 ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-AMOUNT TO WS-DISP-AMT
           MOVE SPACES TO RPT-LINE
           STRING '  INCOME / EXPENSE LEFT OPEN           ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           MOVE SPACES TO RPT-LINE
           IF WS-SUM-ALL = ZERO AND WS-AMOUNT = ZERO
               MOVE '  ASSETS = LIABILITIES + CAPITAL. NEW YEAR OPENS '
                 TO RPT-LINE
               MOVE 'WITH ZERO INCOME AND EXPENSE.' TO RPT-LINE(50:)
           ELSE
               MOVE '  *** LEDGER DOES NOT BALANCE AFTER CLOSING ***'
                 TO RPT-LINE
               MOVE 12 TO WS-RC
           END-IF
           WRITE RPT-LINE
           DISPLAY FUNCTION TRIM(RPT-LINE).

       7000-LOG-YEAR.
           IF WS-RC NOT = ZERO
               EXIT PARAGRAPH
           END-IF
           OPEN EXTEND LOG-FILE
           IF WS-LOG-FS = '35'
               OPEN OUTPUT LOG-FILE
           END-IF
           INITIALIZE YE-LOG-RECORD
           MOVE WS-YEAR-END TO YL-YEAR-END
           MOVE WS-TODAY    TO YL-RUN-DATE
           MOVE WS-NET      TO YL-NET
           MOVE SES-USER-ID TO YL-USER
           WRITE YE-LOG-RECORD
           CLOSE LOG-FILE
           MOVE 'YREND'     TO AU-PROGRAM
           MOVE 'YEAR CLOSED' TO AU-ACTION
           MOVE WS-YEAR-END TO AU-KEY
           MOVE WS-NET      TO WS-DISP-AMT
           MOVE SPACES      TO AU-DETAIL
           STRING 'NET ' FUNCTION TRIM(WS-DISP-AMT)
                  DELIMITED BY SIZE INTO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS
           DISPLAY 'YEAR ENDING ' WS-YEAR-END ' CLOSED.'.

       8000-POST.
           MOVE 'YREND' TO GP-REF
           CALL 'GLPOST' USING GL-PARMS
           IF NOT GP-OK
               DISPLAY 'YREND: CLOSING ENTRY FAILED RC=' GP-RC ' '
                       GP-DR-GL '/' GP-CR-GL
               MOVE 12 TO WS-RC
           END-IF.
