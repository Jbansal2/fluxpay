       IDENTIFICATION DIVISION.
       PROGRAM-ID. REGRPT.
      *================================================================*
      * PROGRAM  : REGRPT                                              *
      * PURPOSE  : REGULATORY RETURNS (SIMPLIFIED, FOR MANAGEMENT USE):*
      *            PART A - CRR / SLR REQUIREMENT ON NDTL              *
      *              DEMAND LIABILITIES = CURRENT + SAVINGS DEPOSITS   *
      *              TIME LIABILITIES   = TERM DEPOSITS                *
      *              OTHER LIABILITIES  = GST/TDS PAYABLE, SUNDRY      *
      *              CRR AND SLR PERCENTAGES COME FROM THE RATE MASTER *
      *              (CODES CRR, SLR) SO A POLICY CHANGE NEEDS NO CODE.*
      *            PART B - STRUCTURAL LIQUIDITY (ALM) IN 9 BUCKETS    *
      *              OUTFLOWS: TERM DEPOSITS AT MATURITY VALUE ON      *
      *              THEIR MATURITY DATE; SAVINGS/CURRENT BALANCES     *
      *              SPLIT BY A BEHAVIOURAL ASSUMPTION - 15% VOLATILE  *
      *              (1-14 DAYS, HALF EACH IN THE FIRST TWO BUCKETS),  *
      *              85% CORE (1-3 YEARS).                             *
      *              INFLOWS: LOAN PRINCIPAL AS IT FALLS DUE, PROJECTED*
      *              EMI BY EMI ON THE REDUCING BALANCE; OVERDUE EMIS  *
      *              OF STANDARD LOANS IN 3-6 MONTHS; NPA PRINCIPAL IN *
      *              OVER 5 YEARS (NOT COUNTED AS LIQUID).             *
      *              GAP, CUMULATIVE GAP AND GAP AS % OF OUTFLOWS. THE *
      *              FIRST FOUR BUCKETS ARE CHECKED AGAINST THE USUAL  *
      *              TOLERANCES (-5%, -10%, -15%, -20%).               *
      *            PART C - BSR-STYLE SIZE CLASSES                     *
      *              LOANS AND DEPOSITS BY TYPE AND AMOUNT BAND        *
      *              (UP TO 2 LAKH, 2-10 LAKH, 10-50 LAKH, ABOVE).     *
      * INPUT    : REPORTING DATE YYYYMMDD (BLANK = TODAY)             *
      * OUTPUT   : data/REGRPT_<DATE>.TXT                              *
      * FILES    : GLMAST, ACCTMAST, DEPMAST, LOANMAST (INPUT)         *
      * CALLS    : RATEUTIL, DATEUTIL                                  *
      * RETURN   : 0 = WITHIN TOLERANCES, 4 = A LIQUIDITY GAP BREACH   *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GL-FILE ASSIGN TO 'data/GLMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS SEQUENTIAL
               RECORD KEY IS GL-CODE FILE STATUS IS WS-GL-FS.
           SELECT ACCT-FILE ASSIGN TO 'data/ACCTMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS SEQUENTIAL
               RECORD KEY IS ACCT-NO FILE STATUS IS WS-ACCT-FS.
           SELECT DEP-FILE ASSIGN TO 'data/DEPMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS SEQUENTIAL
               RECORD KEY IS DEP-NO FILE STATUS IS WS-DEP-FS.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS SEQUENTIAL
               RECORD KEY IS LOAN-NO FILE STATUS IS WS-LOAN-FS.
           SELECT RPT-FILE ASSIGN TO WS-RPT-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  GL-FILE.
           COPY GLREC.
       FD  ACCT-FILE.
           COPY ACCTREC.
       FD  DEP-FILE.
           COPY DEPREC.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(110).

       WORKING-STORAGE SECTION.
       01  WS-GL-FS                    PIC X(02).
       01  WS-ACCT-FS                  PIC X(02).
       01  WS-DEP-FS                   PIC X(02).
       01  WS-LOAN-FS                  PIC X(02).
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-NAME                 PIC X(40).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-REPORT-DATE              PIC 9(08).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-RETURN-CODE              PIC 9(02) VALUE ZERO.
       01  WS-LINE                     PIC X(110).
      *----------------------------------------------------------------*
      * PART A                                                         *
      *----------------------------------------------------------------*
       01  WS-DEMAND                   PIC S9(15)V99 VALUE ZERO.
       01  WS-TIME                     PIC S9(15)V99 VALUE ZERO.
       01  WS-OTHER-LIAB               PIC S9(15)V99 VALUE ZERO.
       01  WS-NDTL                     PIC S9(15)V99 VALUE ZERO.
       01  WS-CRR-PCT                  PIC 9(02)V9(04).
       01  WS-SLR-PCT                  PIC 9(02)V9(04).
       01  WS-CRR-REQ                  PIC S9(15)V99.
       01  WS-SLR-REQ                  PIC S9(15)V99.
      *----------------------------------------------------------------*
      * PART B - BUCKETS (UPPER LIMIT IN DAYS; 99999 = OPEN ENDED)     *
      *----------------------------------------------------------------*
       01  WS-BUCKET-DEF.
           05  FILLER PIC X(15) VALUE '000071-7 DAYS  '.
           05  FILLER PIC X(15) VALUE '000148-14 DAYS '.
           05  FILLER PIC X(15) VALUE '0003015-30 DAYS'.
           05  FILLER PIC X(15) VALUE '000901-3 MONTHS'.
           05  FILLER PIC X(15) VALUE '001803-6 MONTHS'.
           05  FILLER PIC X(15) VALUE '003656-12 MNTHS'.
           05  FILLER PIC X(15) VALUE '010951-3 YEARS '.
           05  FILLER PIC X(15) VALUE '018253-5 YEARS '.
           05  FILLER PIC X(15) VALUE '99999OVER 5 YRS'.
       01  WS-BUCKET-R REDEFINES WS-BUCKET-DEF.
           05  WS-BUCKET OCCURS 9 TIMES.
               10  BK-MAX-DAYS         PIC 9(05).
               10  BK-NAME             PIC X(10).
       01  WS-FLOWS.
           05  WS-FLOW OCCURS 9 TIMES.
               10  FL-OUT              PIC S9(15)V99.
               10  FL-IN               PIC S9(15)V99.
      *    TOLERANCE FOR THE FIRST FOUR BUCKETS (NEGATIVE GAP / OUTFLOW)
       01  WS-TOLERANCES.
           05  FILLER                  PIC S9(03) VALUE -5.
           05  FILLER                  PIC S9(03) VALUE -10.
           05  FILLER                  PIC S9(03) VALUE -15.
           05  FILLER                  PIC S9(03) VALUE -20.
       01  WS-TOLERANCE-R REDEFINES WS-TOLERANCES.
           05  WS-TOLERANCE            PIC S9(03) OCCURS 4 TIMES.
       01  WS-B                        PIC 9(02).
       01  WS-DAYS                     PIC S9(07).
       01  WS-AMOUNT                   PIC S9(15)V99.
       01  WS-VOLATILE-PCT             PIC 9V99 VALUE 0.15.
       01  WS-SBCA-TOTAL               PIC S9(15)V99 VALUE ZERO.
       01  WS-GAP                      PIC S9(15)V99.
       01  WS-CUM-GAP                  PIC S9(15)V99 VALUE ZERO.
       01  WS-GAP-PCT                  PIC S9(05)V99.
       01  WS-FLAG                     PIC X(08).
      *    LOAN PROJECTION
       01  WS-BAL                      PIC S9(11)V99.
       01  WS-INT                      PIC S9(11)V99.
       01  WS-PRIN                     PIC S9(11)V99.
       01  WS-K                        PIC 9(03).
       01  WS-REMAINING                PIC S9(05).
       01  WS-DUE-DATE                 PIC 9(08).
      *----------------------------------------------------------------*
      * PART C - SIZE CLASSES: ROW 1-5 LOAN TYPES H P V G F,           *
      *          ROW 6-8 SAVINGS, CURRENT, TERM DEPOSITS               *
      *----------------------------------------------------------------*
       01  WS-BAND-LIMITS.
           05  FILLER                  PIC 9(09) VALUE 200000.
           05  FILLER                  PIC 9(09) VALUE 1000000.
           05  FILLER                  PIC 9(09) VALUE 5000000.
           05  FILLER                  PIC 9(09) VALUE 999999999.
       01  WS-BAND-R REDEFINES WS-BAND-LIMITS.
           05  WS-BAND-LIMIT           PIC 9(09) OCCURS 4 TIMES.
       01  WS-ROW-NAMES.
           05  FILLER                  PIC X(14) VALUE 'HOME LOANS'.
           05  FILLER                  PIC X(14) VALUE 'PERSONAL LOANS'.
           05  FILLER                  PIC X(14) VALUE 'VEHICLE LOANS'.
           05  FILLER                  PIC X(14) VALUE 'GOLD LOANS'.
           05  FILLER                  PIC X(14) VALUE 'LOANS AGNST FD'.
           05  FILLER                  PIC X(14) VALUE 'SAVINGS DEP.'.
           05  FILLER                  PIC X(14) VALUE 'CURRENT DEP.'.
           05  FILLER                  PIC X(14) VALUE 'TERM DEPOSITS'.
       01  WS-ROW-R REDEFINES WS-ROW-NAMES.
           05  WS-ROW-NAME             PIC X(14) OCCURS 8 TIMES.
       01  WS-BSR.
           05  WS-BSR-ROW OCCURS 8 TIMES.
               10  WS-BSR-CELL OCCURS 4 TIMES.
                   15  BS-COUNT        PIC 9(07).
                   15  BS-AMOUNT       PIC S9(15)V99.
       01  WS-ROW                      PIC 9(02).
       01  WS-BAND                     PIC 9(01).
       01  ED-AMT                      PIC ----,---,---,--9.
       01  ED-AMT2                     PIC ----,---,---,--9.
       01  ED-AMT3                     PIC ----,---,---,--9.
       01  ED-AMT4                     PIC ----,---,---,--9.
       01  ED-PCT                      PIC ----9.99.
       01  ED-RATE                     PIC Z9.99.
       01  ED-CNT                      PIC ZZZZ9.
           COPY RATEPARM.
           COPY DATEPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'REPORTING DATE (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           IF WS-IN-DATE = SPACES OR WS-IN-DATE NOT NUMERIC
               ACCEPT WS-REPORT-DATE FROM DATE YYYYMMDD
           ELSE
               MOVE WS-IN-DATE TO WS-REPORT-DATE
           END-IF
           INITIALIZE WS-FLOWS WS-BSR
           STRING 'data/REGRPT_' WS-REPORT-DATE '.TXT'
               DELIMITED BY SIZE INTO WS-RPT-NAME
           OPEN OUTPUT RPT-FILE
           STRING 'REGULATORY RETURNS AS ON ' WS-REPORT-DATE
                  '   (AMOUNTS IN RUPEES)' DELIMITED BY SIZE
                  INTO WS-LINE
           PERFORM 8000-PUT
           PERFORM 1000-PART-A
           PERFORM 2000-COLLECT-DEPOSIT-ACCOUNTS
           PERFORM 2100-COLLECT-TERM-DEPOSITS
           PERFORM 2200-COLLECT-LOANS
           PERFORM 3000-PRINT-ALM
           PERFORM 4000-PRINT-BSR
           CLOSE RPT-FILE
           MOVE WS-RETURN-CODE TO RETURN-CODE
           GOBACK.

      *================================================================*
      * PART A - NDTL FROM THE LEDGER (LIABILITIES CARRY A CREDIT, I.E.*
      * NEGATIVE, BALANCE IN GLMAST)                                   *
      *================================================================*
       1000-PART-A.
           OPEN INPUT GL-FILE
           IF WS-GL-FS = '00'
               MOVE 'N' TO WS-EOF-SW
               PERFORM UNTIL END-OF-FILE
                   READ GL-FILE NEXT RECORD
                       AT END SET END-OF-FILE TO TRUE
                       NOT AT END PERFORM 1100-CLASSIFY-GL
                   END-READ
               END-PERFORM
               CLOSE GL-FILE
           END-IF
           COMPUTE WS-NDTL = WS-DEMAND + WS-TIME + WS-OTHER-LIAB
           MOVE 'CRR' TO RTP-CODE
           CALL 'RATEUTIL' USING RATE-PARMS
           MOVE RTP-VALUE TO WS-CRR-PCT
           MOVE 'SLR' TO RTP-CODE
           CALL 'RATEUTIL' USING RATE-PARMS
           MOVE RTP-VALUE TO WS-SLR-PCT
           COMPUTE WS-CRR-REQ ROUNDED = WS-NDTL * WS-CRR-PCT / 100
           COMPUTE WS-SLR-REQ ROUNDED = WS-NDTL * WS-SLR-PCT / 100
           PERFORM 8100-BLANK
           MOVE 'PART A - CRR / SLR ON NET DEMAND AND TIME LIABILITIES'
               TO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-DEMAND TO ED-AMT
           STRING '  DEMAND LIABILITIES (SB + CA)  ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-TIME TO ED-AMT
           STRING '  TIME LIABILITIES (TERM DEP.)  ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-OTHER-LIAB TO ED-AMT
           STRING '  OTHER DEMAND AND TIME LIAB.   ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-NDTL TO ED-AMT
           STRING '  NDTL                          ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-CRR-PCT TO ED-RATE
           MOVE WS-CRR-REQ TO ED-AMT
           STRING '  CRR REQUIRED @ ' ED-RATE '%        ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-SLR-PCT TO ED-RATE
           MOVE WS-SLR-REQ TO ED-AMT
           STRING '  SLR REQUIRED @ ' ED-RATE '%        ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT.

       1100-CLASSIFY-GL.
           EVALUATE GL-CODE
               WHEN 2101
               WHEN 2102
                   SUBTRACT GL-BALANCE FROM WS-DEMAND
               WHEN 2201
                   SUBTRACT GL-BALANCE FROM WS-TIME
               WHEN 2501 THRU 2599
               WHEN 2900 THRU 2999
                   IF GL-BALANCE < ZERO
                       SUBTRACT GL-BALANCE FROM WS-OTHER-LIAB
                   END-IF
           END-EVALUATE.

      *================================================================*
      * PART B / C - COLLECTION                                        *
      *================================================================*
       2000-COLLECT-DEPOSIT-ACCOUNTS.
           OPEN INPUT ACCT-FILE
           IF WS-ACCT-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ ACCT-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF NOT ACCT-CLOSED AND ACCT-BALANCE > ZERO
                           ADD ACCT-BALANCE TO WS-SBCA-TOTAL
                           IF ACCT-SAVINGS
                               MOVE 6 TO WS-ROW
                           ELSE
                               MOVE 7 TO WS-ROW
                           END-IF
                           MOVE ACCT-BALANCE TO WS-AMOUNT
                           PERFORM 7200-ADD-BSR
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACCT-FILE
      *    BEHAVIOURAL SPLIT OF SAVINGS + CURRENT
           COMPUTE WS-AMOUNT ROUNDED =
                   WS-SBCA-TOTAL * WS-VOLATILE-PCT / 2
           ADD WS-AMOUNT TO FL-OUT(1) FL-OUT(2)
           COMPUTE FL-OUT(7) = FL-OUT(7) + WS-SBCA-TOTAL
                   - WS-AMOUNT - WS-AMOUNT.

       2100-COLLECT-TERM-DEPOSITS.
           OPEN INPUT DEP-FILE
           IF WS-DEP-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ DEP-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF DEP-ACTIVE
                           MOVE DEP-MATURITY-DATE TO WS-DUE-DATE
                           PERFORM 7000-BUCKET-OF-DATE
                           ADD DEP-MATURITY-AMT TO FL-OUT(WS-B)
                           MOVE 8 TO WS-ROW
                           MOVE DEP-TOTAL-DEPOSITED TO WS-AMOUNT
                           IF DEP-FD
                               MOVE DEP-AMOUNT TO WS-AMOUNT
                           END-IF
                           PERFORM 7200-ADD-BSR
                       END-IF
               END-READ
           END-PERFORM
           CLOSE DEP-FILE.

       2200-COLLECT-LOANS.
           OPEN INPUT LOAN-FILE
           IF WS-LOAN-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ LOAN-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF NOT LOAN-CLOSED
                           PERFORM 2300-ONE-LOAN
                       END-IF
               END-READ
           END-PERFORM
           CLOSE LOAN-FILE.

       2300-ONE-LOAN.
           EVALUATE TRUE
               WHEN LOAN-HOME       MOVE 1 TO WS-ROW
               WHEN LOAN-PERSONAL   MOVE 2 TO WS-ROW
               WHEN LOAN-VEHICLE    MOVE 3 TO WS-ROW
               WHEN LOAN-GOLD       MOVE 4 TO WS-ROW
               WHEN OTHER           MOVE 5 TO WS-ROW
           END-EVALUATE
           MOVE LOAN-OUTSTANDING TO WS-AMOUNT
           PERFORM 7200-ADD-BSR
           IF LOAN-NPA
               ADD LOAN-OUTSTANDING TO FL-IN(9)
               EXIT PARAGRAPH
           END-IF
      *    PROJECT THE REMAINING EMIS ON THE REDUCING BALANCE
           MOVE LOAN-OUTSTANDING TO WS-BAL
           COMPUTE WS-REMAINING = LOAN-TENURE - LOAN-EMIS-PAID
           IF LOAN-OVERDUE-EMIS > ZERO
               COMPUTE WS-PRIN = LOAN-OVERDUE-EMIS * LOAN-EMI
               IF WS-PRIN > WS-BAL
                   MOVE WS-BAL TO WS-PRIN
               END-IF
               ADD WS-PRIN TO FL-IN(5)
               SUBTRACT WS-PRIN FROM WS-BAL
           END-IF
           MOVE LOAN-NEXT-DUE-DATE TO WS-DUE-DATE
           PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-BAL NOT > ZERO OR WS-K > WS-REMAINING
               COMPUTE WS-INT ROUNDED = WS-BAL * LOAN-RATE / 1200
               COMPUTE WS-PRIN = LOAN-EMI - WS-INT
               IF WS-PRIN > WS-BAL OR WS-K = WS-REMAINING
                   MOVE WS-BAL TO WS-PRIN
               END-IF
               IF WS-PRIN < ZERO
                   MOVE ZERO TO WS-PRIN
               END-IF
               PERFORM 7000-BUCKET-OF-DATE
               ADD WS-PRIN TO FL-IN(WS-B)
               SUBTRACT WS-PRIN FROM WS-BAL
               SET DU-ADD-MONTHS TO TRUE
               MOVE WS-DUE-DATE TO DU-DATE-1
               MOVE 1 TO DU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DU-RESULT-DATE TO WS-DUE-DATE
           END-PERFORM
      *    ANYTHING LEFT (ROUNDING, ZERO EMI) AT THE LOAN'S END
           IF WS-BAL > ZERO
               ADD WS-BAL TO FL-IN(WS-B)
           END-IF.

      *================================================================*
      * PART B - PRINT                                                 *
      *================================================================*
       3000-PRINT-ALM.
           PERFORM 8100-BLANK
           MOVE 'PART B - STRUCTURAL LIQUIDITY (MATURITY BUCKETS)'
               TO WS-LINE
           PERFORM 8000-PUT
           STRING 'BUCKET             OUTFLOWS         INFLOWS'
                  '             GAP      CUMULATIVE    GAP %'
                  DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           PERFORM VARYING WS-B FROM 1 BY 1 UNTIL WS-B > 9
               COMPUTE WS-GAP = FL-IN(WS-B) - FL-OUT(WS-B)
               ADD WS-GAP TO WS-CUM-GAP
               MOVE SPACES TO WS-FLAG
               IF FL-OUT(WS-B) > ZERO
                   COMPUTE WS-GAP-PCT ROUNDED =
                           WS-GAP * 100 / FL-OUT(WS-B)
               ELSE
                   MOVE ZERO TO WS-GAP-PCT
               END-IF
               IF WS-B <= 4 AND WS-GAP-PCT < WS-TOLERANCE(WS-B)
                   MOVE 'BREACH' TO WS-FLAG
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               MOVE FL-OUT(WS-B) TO ED-AMT
               MOVE FL-IN(WS-B)  TO ED-AMT2
               MOVE WS-GAP       TO ED-AMT3
               MOVE WS-CUM-GAP   TO ED-AMT4
               MOVE WS-GAP-PCT   TO ED-PCT
               STRING BK-NAME(WS-B) ED-AMT ED-AMT2 ED-AMT3 ED-AMT4
                      ED-PCT ' ' WS-FLAG DELIMITED BY SIZE
                      INTO WS-LINE
               PERFORM 8000-PUT
           END-PERFORM.

      *================================================================*
      * PART C - PRINT                                                 *
      *================================================================*
       4000-PRINT-BSR.
           PERFORM 8100-BLANK
           MOVE 'PART C - SIZE CLASSES (COUNT / AMOUNT)' TO WS-LINE
           PERFORM 8000-PUT
           STRING 'TYPE                UP TO 2 LAKH'
                  '           2-10 LAKH          10-50 LAKH'
                  '       ABOVE 50 LAKH' DELIMITED BY SIZE
                  INTO WS-LINE
           PERFORM 8000-PUT
           PERFORM VARYING WS-ROW FROM 1 BY 1 UNTIL WS-ROW > 8
               MOVE WS-ROW-NAME(WS-ROW) TO WS-LINE
               MOVE 15 TO WS-K
               PERFORM VARYING WS-BAND FROM 1 BY 1 UNTIL WS-BAND > 4
                   MOVE BS-COUNT(WS-ROW, WS-BAND)  TO ED-CNT
                   MOVE BS-AMOUNT(WS-ROW, WS-BAND) TO ED-AMT
                   STRING ED-CNT ED-AMT DELIMITED BY SIZE
                       INTO WS-LINE(WS-K:21)
                   ADD 21 TO WS-K
               END-PERFORM
               PERFORM 8000-PUT
           END-PERFORM.

      *================================================================*
      * HELPERS                                                        *
      *================================================================*
      *    WS-DUE-DATE -> WS-B (PAST DATES COUNT AS DUE IN BUCKET 1)
       7000-BUCKET-OF-DATE.
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE WS-REPORT-DATE TO DU-DATE-1
           MOVE WS-DUE-DATE    TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DAYS TO WS-DAYS
           IF NOT DU-OK OR WS-DAYS < 1
               MOVE 1 TO WS-DAYS
           END-IF
           PERFORM VARYING WS-B FROM 1 BY 1
                   UNTIL WS-B = 9 OR WS-DAYS <= BK-MAX-DAYS(WS-B)
               CONTINUE
           END-PERFORM.

       7200-ADD-BSR.
           PERFORM VARYING WS-BAND FROM 1 BY 1
                   UNTIL WS-BAND = 4
                      OR WS-AMOUNT <= WS-BAND-LIMIT(WS-BAND)
               CONTINUE
           END-PERFORM
           ADD 1         TO BS-COUNT(WS-ROW, WS-BAND)
           ADD WS-AMOUNT TO BS-AMOUNT(WS-ROW, WS-BAND).

       8000-PUT.
           DISPLAY FUNCTION TRIM(WS-LINE TRAILING)
           WRITE RPT-LINE FROM WS-LINE
           MOVE SPACES TO WS-LINE.

       8100-BLANK.
           MOVE SPACES TO WS-LINE
           PERFORM 8000-PUT.
