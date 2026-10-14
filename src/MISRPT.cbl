       IDENTIFICATION DIVISION.
       PROGRAM-ID. MISRPT.
      *================================================================*
      * PROGRAM  : MISRPT                                              *
      * PURPOSE  : MANAGEMENT INFORMATION REPORTS                      *
      *              1 - BRANCH-WISE DEPOSIT SUMMARY (SORT + CONTROL   *
      *                  BREAK ON BRANCH)                              *
      *              2 - LOAN PORTFOLIO BY PRODUCT WITH GROSS NPA %    *
      *              3 - TERM DEPOSIT LIABILITY AND 30-DAY MATURITIES  *
      *              4 - TOP 10 ACCOUNTS BY BALANCE (SORT DESCENDING)  *
      *              A - ALL OF THE ABOVE                              *
      *            OUTPUT GOES TO THE SCREEN AND data/MIS_<DATE>.TXT.  *
      * FILES    : ACCTMAST, LOANMAST, DEPMAST (INPUT), SORTWK, MISRPT *
      * CALLS    : DATEUTIL                                            *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE ASSIGN TO 'data/ACCTMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-ACCT-FS.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS LOAN-NO
               FILE STATUS IS WS-LOAN-FS.
           SELECT DEP-FILE ASSIGN TO 'data/DEPMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS DEP-NO
               FILE STATUS IS WS-DEP-FS.
           SELECT SORT-FILE ASSIGN TO 'data/SORTWK.TMP'.
           SELECT RPT-FILE ASSIGN TO WS-RPT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
           COPY ACCTREC.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  DEP-FILE.
           COPY DEPREC.
       SD  SORT-FILE.
       01  SORT-RECORD.
           05  SR-BRANCH               PIC X(06).
           05  SR-BALANCE              PIC S9(13)V99.
           05  SR-ACCT-NO              PIC 9(10).
           05  SR-TYPE                 PIC X(01).
           05  SR-NAME                 PIC X(40).
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(100).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY DATEPARM.

       01  WS-LOAN-FS                  PIC X(02).
           88  LOAN-FS-OK                        VALUE '00'.
       01  WS-DEP-FS                   PIC X(02).
           88  DEP-FS-OK                         VALUE '00'.
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-FILENAME             PIC X(40).

       01  WS-CHOICE                   PIC X(01).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-SORT-EOF-SW              PIC X(01).
           88  END-OF-SORT                       VALUE 'Y'.
       01  WS-RPT-WORK                 PIC X(100).

      * BRANCH CONTROL BREAK
       01  WS-PREV-BRANCH              PIC X(06).
       01  WS-BR-TOTALS.
           05  WS-BR-SAV-CNT           PIC 9(07).
           05  WS-BR-CUR-CNT           PIC 9(07).
           05  WS-BR-SAV-BAL           PIC S9(15)V99.
           05  WS-BR-CUR-BAL           PIC S9(15)V99.
       01  WS-GRAND-TOTALS.
           05  WS-GT-SAV-CNT           PIC 9(07).
           05  WS-GT-CUR-CNT           PIC 9(07).
           05  WS-GT-SAV-BAL           PIC S9(15)V99.
           05  WS-GT-CUR-BAL           PIC S9(15)V99.
           05  WS-GT-BRANCHES          PIC 9(05).

      * LOAN PORTFOLIO - ONE ROW PER PRODUCT (H, P, V)
       01  WS-LOAN-TABLE.
           05  WS-LN-ROW OCCURS 5 TIMES.
               10  LN-COUNT            PIC 9(07).
               10  LN-SANCTIONED       PIC 9(15)V99.
               10  LN-OUTSTANDING      PIC 9(15)V99.
               10  LN-NPA-COUNT        PIC 9(07).
               10  LN-NPA-OUTSTANDING  PIC 9(15)V99.
       01  WS-LN-NAMES.
           05  FILLER PIC X(10) VALUE 'HOME'.
           05  FILLER PIC X(10) VALUE 'PERSONAL'.
           05  FILLER PIC X(10) VALUE 'VEHICLE'.
           05  FILLER PIC X(10) VALUE 'GOLD'.
           05  FILLER PIC X(10) VALUE 'AGAINST FD'.
       01  WS-LN-NAMES-R REDEFINES WS-LN-NAMES.
           05  WS-LN-NAME PIC X(10) OCCURS 5 TIMES.
       01  WS-LX                       PIC 9(01).
       01  WS-LN-TOT-OUT               PIC 9(15)V99.
       01  WS-LN-TOT-NPA               PIC 9(15)V99.
       01  WS-NPA-PCT                  PIC 9(03)V99.

      * DEPOSITS
       01  WS-DEP-TOTALS.
           05  WS-FD-COUNT             PIC 9(07).
           05  WS-FD-PRINCIPAL         PIC 9(15)V99.
           05  WS-FD-MATURITY          PIC 9(15)V99.
           05  WS-RD-COUNT             PIC 9(07).
           05  WS-RD-DEPOSITED         PIC 9(15)V99.
           05  WS-RD-MATURITY          PIC 9(15)V99.
           05  WS-DUE30-COUNT          PIC 9(07).
           05  WS-DUE30-AMOUNT         PIC 9(15)V99.
       01  WS-DATE-PLUS-30             PIC 9(08).

      * TOP N
       01  WS-TOP-N                    PIC 9(02) VALUE 10.
       01  WS-RANK                     PIC 9(02).

      * EDITED FIELDS
       01  WS-E-CNT                    PIC Z,ZZZ,ZZ9.
       01  WS-E-AMT                    PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-.
       01  WS-E-AMT2                   PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-.
       01  WS-E-PCT                    PIC ZZ9.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           DISPLAY ' '
           DISPLAY '------------- MIS REPORTS -------------'
           DISPLAY ' 1. BRANCH-WISE DEPOSIT SUMMARY'
           DISPLAY ' 2. LOAN PORTFOLIO AND NPA'
           DISPLAY ' 3. TERM DEPOSIT LIABILITY'
           DISPLAY ' 4. TOP 10 ACCOUNTS BY BALANCE'
           DISPLAY ' A. ALL REPORTS'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           MOVE FUNCTION UPPER-CASE(WS-CHOICE) TO WS-CHOICE
           IF WS-CHOICE = SPACE
               MOVE 'A' TO WS-CHOICE
           END-IF

           STRING 'data/MIS_' WS-CURR-DATE '.TXT'
                  DELIMITED BY SIZE INTO WS-RPT-FILENAME
           END-STRING
           OPEN OUTPUT RPT-FILE
           MOVE ALL '=' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           STRING 'CORE BANKING - MIS REPORT AS ON ' WS-CURR-DATE
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT
           MOVE ALL '=' TO WS-RPT-WORK
           PERFORM 8000-EMIT

           IF WS-CHOICE = '1' OR 'A'
               PERFORM 1000-BRANCH-SUMMARY
           END-IF
           IF WS-CHOICE = '2' OR 'A'
               PERFORM 2000-LOAN-PORTFOLIO
           END-IF
           IF WS-CHOICE = '3' OR 'A'
               PERFORM 3000-DEPOSIT-LIABILITY
           END-IF
           IF WS-CHOICE = '4' OR 'A'
               PERFORM 4000-TOP-ACCOUNTS
           END-IF

           CLOSE RPT-FILE
           DISPLAY 'REPORT SAVED TO ' WS-RPT-FILENAME
           MOVE ZERO TO RETURN-CODE
           GOBACK.

      *================================================================*
      * 1 - BRANCH SUMMARY                                             *
      *================================================================*
       1000-BRANCH-SUMMARY.
           PERFORM 8100-BLANK-LINE
           MOVE '1. BRANCH-WISE DEPOSIT SUMMARY (ACTIVE ACCOUNTS)'
                                                 TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE 'BRANCH   SAVINGS A/C       SAVINGS BALANCE'
                                                 TO WS-RPT-WORK
           MOVE 'CURRENT A/C' TO WS-RPT-WORK(47:)
           MOVE '       CURRENT BALANCE' TO WS-RPT-WORK(59:)
           PERFORM 8000-EMIT
           INITIALIZE WS-GRAND-TOTALS
           SORT SORT-FILE
               ON ASCENDING KEY SR-BRANCH
               INPUT PROCEDURE  IS 1100-RELEASE-ACCOUNTS
               OUTPUT PROCEDURE IS 1200-BRANCH-BREAK
           PERFORM 8200-DASH-LINE
           MOVE 'TOTAL' TO WS-RPT-WORK
           MOVE WS-GT-SAV-CNT TO WS-E-CNT
           MOVE WS-E-CNT TO WS-RPT-WORK(10:9)
           MOVE WS-GT-SAV-BAL TO WS-E-AMT
           MOVE WS-E-AMT TO WS-RPT-WORK(20:23)
           MOVE WS-GT-CUR-CNT TO WS-E-CNT
           MOVE WS-E-CNT TO WS-RPT-WORK(46:9)
           MOVE WS-GT-CUR-BAL TO WS-E-AMT
           MOVE WS-E-AMT TO WS-RPT-WORK(57:23)
           PERFORM 8000-EMIT
           MOVE WS-GT-BRANCHES TO WS-E-CNT
           STRING 'BRANCHES: ' FUNCTION TRIM(WS-E-CNT)
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT.

       1100-RELEASE-ACCOUNTS.
           OPEN INPUT ACCT-FILE
           IF NOT ACCT-FS-OK
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ ACCT-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF ACCT-ACTIVE OR ACCT-FROZEN
                           MOVE ACCT-BRANCH-CODE TO SR-BRANCH
                           MOVE ACCT-BALANCE     TO SR-BALANCE
                           MOVE ACCT-NO          TO SR-ACCT-NO
                           MOVE ACCT-TYPE        TO SR-TYPE
                           MOVE ACCT-HOLDER-NAME TO SR-NAME
                           RELEASE SORT-RECORD
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACCT-FILE.

       1200-BRANCH-BREAK.
           MOVE 'N' TO WS-SORT-EOF-SW
           MOVE LOW-VALUES TO WS-PREV-BRANCH
           INITIALIZE WS-BR-TOTALS
           PERFORM UNTIL END-OF-SORT
               RETURN SORT-FILE
                   AT END SET END-OF-SORT TO TRUE
                   NOT AT END
                       IF SR-BRANCH NOT = WS-PREV-BRANCH
                          AND WS-PREV-BRANCH NOT = LOW-VALUES
                           PERFORM 1300-PRINT-BRANCH
                       END-IF
                       MOVE SR-BRANCH TO WS-PREV-BRANCH
                       IF SR-TYPE = 'S'
                           ADD 1          TO WS-BR-SAV-CNT
                           ADD SR-BALANCE TO WS-BR-SAV-BAL
                       ELSE
                           ADD 1          TO WS-BR-CUR-CNT
                           ADD SR-BALANCE TO WS-BR-CUR-BAL
                       END-IF
               END-RETURN
           END-PERFORM
           IF WS-PREV-BRANCH NOT = LOW-VALUES
               PERFORM 1300-PRINT-BRANCH
           END-IF.

       1300-PRINT-BRANCH.
           MOVE WS-PREV-BRANCH TO WS-RPT-WORK
           MOVE WS-BR-SAV-CNT TO WS-E-CNT
           MOVE WS-E-CNT TO WS-RPT-WORK(10:9)
           MOVE WS-BR-SAV-BAL TO WS-E-AMT
           MOVE WS-E-AMT TO WS-RPT-WORK(20:23)
           MOVE WS-BR-CUR-CNT TO WS-E-CNT
           MOVE WS-E-CNT TO WS-RPT-WORK(46:9)
           MOVE WS-BR-CUR-BAL TO WS-E-AMT
           MOVE WS-E-AMT TO WS-RPT-WORK(57:23)
           PERFORM 8000-EMIT
           ADD WS-BR-SAV-CNT TO WS-GT-SAV-CNT
           ADD WS-BR-CUR-CNT TO WS-GT-CUR-CNT
           ADD WS-BR-SAV-BAL TO WS-GT-SAV-BAL
           ADD WS-BR-CUR-BAL TO WS-GT-CUR-BAL
           ADD 1 TO WS-GT-BRANCHES
           INITIALIZE WS-BR-TOTALS.

      *================================================================*
      * 2 - LOAN PORTFOLIO                                             *
      *================================================================*
       2000-LOAN-PORTFOLIO.
           PERFORM 8100-BLANK-LINE
           MOVE '2. LOAN PORTFOLIO AND ASSET QUALITY' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           INITIALIZE WS-LOAN-TABLE
           MOVE ZERO TO WS-LN-TOT-OUT WS-LN-TOT-NPA
           OPEN INPUT LOAN-FILE
           IF NOT LOAN-FS-OK
               MOVE '   NO LOANS ON FILE.' TO WS-RPT-WORK
               PERFORM 8000-EMIT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ LOAN-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF NOT LOAN-CLOSED
                           PERFORM 2100-ACCUMULATE-LOAN
                       END-IF
               END-READ
           END-PERFORM
           CLOSE LOAN-FILE

           MOVE 'PRODUCT     LOANS            OUTSTANDING   NPA'
                                                 TO WS-RPT-WORK
           MOVE '        NPA OUTSTANDING   NPA %' TO WS-RPT-WORK(48:)
           PERFORM 8000-EMIT
           PERFORM VARYING WS-LX FROM 1 BY 1 UNTIL WS-LX > 5
               MOVE WS-LN-NAME(WS-LX) TO WS-RPT-WORK
               MOVE LN-COUNT(WS-LX) TO WS-E-CNT
               MOVE WS-E-CNT TO WS-RPT-WORK(9:9)
               MOVE LN-OUTSTANDING(WS-LX) TO WS-E-AMT
               MOVE WS-E-AMT TO WS-RPT-WORK(18:23)
               MOVE LN-NPA-COUNT(WS-LX) TO WS-E-CNT
               MOVE WS-E-CNT TO WS-RPT-WORK(41:9)
               MOVE LN-NPA-OUTSTANDING(WS-LX) TO WS-E-AMT
               MOVE WS-E-AMT TO WS-RPT-WORK(51:23)
               IF LN-OUTSTANDING(WS-LX) > ZERO
                   COMPUTE WS-NPA-PCT ROUNDED =
                       LN-NPA-OUTSTANDING(WS-LX) * 100
                       / LN-OUTSTANDING(WS-LX)
               ELSE
                   MOVE ZERO TO WS-NPA-PCT
               END-IF
               MOVE WS-NPA-PCT TO WS-E-PCT
               MOVE WS-E-PCT TO WS-RPT-WORK(75:6)
               PERFORM 8000-EMIT
           END-PERFORM
           PERFORM 8200-DASH-LINE
           IF WS-LN-TOT-OUT > ZERO
               COMPUTE WS-NPA-PCT ROUNDED =
                       WS-LN-TOT-NPA * 100 / WS-LN-TOT-OUT
           ELSE
               MOVE ZERO TO WS-NPA-PCT
           END-IF
           MOVE WS-LN-TOT-OUT TO WS-E-AMT
           MOVE WS-NPA-PCT TO WS-E-PCT
           STRING 'TOTAL OUTSTANDING ' WS-E-AMT '   GROSS NPA % '
                  WS-E-PCT DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT.

       2100-ACCUMULATE-LOAN.
           EVALUATE TRUE
               WHEN LOAN-HOME     MOVE 1 TO WS-LX
               WHEN LOAN-PERSONAL MOVE 2 TO WS-LX
               WHEN LOAN-VEHICLE  MOVE 3 TO WS-LX
               WHEN LOAN-GOLD     MOVE 4 TO WS-LX
               WHEN OTHER         MOVE 5 TO WS-LX
           END-EVALUATE
           ADD 1                TO LN-COUNT(WS-LX)
           ADD LOAN-PRINCIPAL   TO LN-SANCTIONED(WS-LX)
           ADD LOAN-OUTSTANDING TO LN-OUTSTANDING(WS-LX)
           ADD LOAN-OUTSTANDING TO WS-LN-TOT-OUT
           IF LOAN-NPA
               ADD 1                TO LN-NPA-COUNT(WS-LX)
               ADD LOAN-OUTSTANDING TO LN-NPA-OUTSTANDING(WS-LX)
               ADD LOAN-OUTSTANDING TO WS-LN-TOT-NPA
           END-IF.

      *================================================================*
      * 3 - TERM DEPOSIT LIABILITY                                     *
      *================================================================*
       3000-DEPOSIT-LIABILITY.
           PERFORM 8100-BLANK-LINE
           MOVE '3. TERM DEPOSIT LIABILITY (ACTIVE DEPOSITS)'
                                                 TO WS-RPT-WORK
           PERFORM 8000-EMIT
           INITIALIZE WS-DEP-TOTALS
           SET DU-ADD-DAYS TO TRUE
           MOVE WS-CURR-DATE TO DU-DATE-1
           MOVE 30 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO WS-DATE-PLUS-30
           OPEN INPUT DEP-FILE
           IF NOT DEP-FS-OK
               MOVE '   NO DEPOSITS ON FILE.' TO WS-RPT-WORK
               PERFORM 8000-EMIT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ DEP-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF DEP-ACTIVE
                           PERFORM 3100-ACCUMULATE-DEPOSIT
                       END-IF
               END-READ
           END-PERFORM
           CLOSE DEP-FILE

           MOVE WS-FD-COUNT TO WS-E-CNT
           MOVE WS-FD-PRINCIPAL TO WS-E-AMT
           MOVE WS-FD-MATURITY TO WS-E-AMT2
           STRING 'FIXED DEPOSITS    ' WS-E-CNT '  PRINCIPAL '
                  WS-E-AMT '  AT MATURITY ' WS-E-AMT2
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT
           MOVE WS-RD-COUNT TO WS-E-CNT
           MOVE WS-RD-DEPOSITED TO WS-E-AMT
           MOVE WS-RD-MATURITY TO WS-E-AMT2
           STRING 'RECURRING DEPOSITS' WS-E-CNT '  DEPOSITED '
                  WS-E-AMT '  AT MATURITY ' WS-E-AMT2
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT
           MOVE WS-DUE30-COUNT TO WS-E-CNT
           MOVE WS-DUE30-AMOUNT TO WS-E-AMT
           STRING 'MATURING IN NEXT 30 DAYS: ' FUNCTION TRIM(WS-E-CNT)
                  ' DEPOSITS, PAYOUT ' FUNCTION TRIM(WS-E-AMT)
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT.

       3100-ACCUMULATE-DEPOSIT.
           IF DEP-FD
               ADD 1                TO WS-FD-COUNT
               ADD DEP-AMOUNT       TO WS-FD-PRINCIPAL
               ADD DEP-MATURITY-AMT TO WS-FD-MATURITY
           ELSE
               ADD 1                   TO WS-RD-COUNT
               ADD DEP-TOTAL-DEPOSITED TO WS-RD-DEPOSITED
               ADD DEP-MATURITY-AMT    TO WS-RD-MATURITY
           END-IF
           IF DEP-MATURITY-DATE <= WS-DATE-PLUS-30
               ADD 1                TO WS-DUE30-COUNT
               ADD DEP-MATURITY-AMT TO WS-DUE30-AMOUNT
           END-IF.

      *================================================================*
      * 4 - TOP ACCOUNTS                                               *
      *================================================================*
       4000-TOP-ACCOUNTS.
           PERFORM 8100-BLANK-LINE
           MOVE '4. TOP 10 ACCOUNTS BY BALANCE' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE 'RANK  ACCOUNT     T  BRANCH  HOLDER'
                                                 TO WS-RPT-WORK
           MOVE 'BALANCE' TO WS-RPT-WORK(78:)
           PERFORM 8000-EMIT
           SORT SORT-FILE
               ON DESCENDING KEY SR-BALANCE
               ON ASCENDING  KEY SR-ACCT-NO
               INPUT PROCEDURE  IS 1100-RELEASE-ACCOUNTS
               OUTPUT PROCEDURE IS 4100-PRINT-TOP.

       4100-PRINT-TOP.
           MOVE 'N' TO WS-SORT-EOF-SW
           MOVE ZERO TO WS-RANK
           PERFORM UNTIL END-OF-SORT OR WS-RANK >= WS-TOP-N
               RETURN SORT-FILE
                   AT END SET END-OF-SORT TO TRUE
                   NOT AT END
                       ADD 1 TO WS-RANK
                       MOVE SR-BALANCE TO WS-E-AMT
                       STRING WS-RANK '    ' SR-ACCT-NO '  ' SR-TYPE
                              '  ' SR-BRANCH '  ' SR-NAME(1:30)
                              WS-E-AMT
                              DELIMITED BY SIZE INTO WS-RPT-WORK
                       END-STRING
                       PERFORM 8000-EMIT
               END-RETURN
           END-PERFORM.

      *================================================================*
      * COMMON                                                         *
      *================================================================*
       8000-EMIT.
           MOVE WS-RPT-WORK TO RPT-LINE
           WRITE RPT-LINE
           DISPLAY FUNCTION TRIM(WS-RPT-WORK TRAILING)
           MOVE SPACES TO WS-RPT-WORK.

       8100-BLANK-LINE.
           MOVE SPACES TO WS-RPT-WORK
           PERFORM 8000-EMIT.

       8200-DASH-LINE.
           MOVE ALL '-' TO WS-RPT-WORK
           PERFORM 8000-EMIT.
