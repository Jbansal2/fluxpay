       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLRPT.
      *================================================================*
      * PROGRAM  : SQLRPT                                              *
      * PURPOSE  : MANAGEMENT REPORTS RUN AGAINST THE POSTGRESQL COPY  *
      *            LOADED BY SQLLOAD, USING EMBEDDED SQL CURSORS:      *
      *              1. DEPOSITS BY BRANCH   (VIEW BRANCH_DEPOSITS)    *
      *              2. LOAN BOOK QUALITY    (VIEW LOAN_QUALITY, NPA%) *
      *              3. TOP 10 DEPOSITORS                              *
      *              4. TRANSACTION VOLUME BY TYPE                     *
      *              5. GL 2101/2102 VS ACCOUNT BALANCES (RECON)       *
      *            OUTPUT GOES TO data/SQLRPT_<YYYYMMDD>.TXT AND THE   *
      *            CONSOLE.                                            *
      * CONNECT  : SAME ENVIRONMENT VARIABLES AS SQLLOAD               *
      * RETURN   : 0 = OK, 4 = RECON DIFFERENCE, 12 = SQL ERROR        *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO WS-RPT-NAME
               ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-RPT-NAME                 PIC X(40).
       01  WS-TODAY                    PIC X(08).
       01  WS-ENV                      PIC X(60).
       01  WS-FAILED-SW                PIC X(01) VALUE 'N'.
           88  RPT-FAILED                        VALUE 'Y'.
       01  WS-STEP                     PIC X(20).
       01  WS-RANK                     PIC 9(02).
       01  WS-LINE                     PIC X(100).
       01  WS-NPA-PCT                  PIC S9(03)V99.
       01  ED-AMT                      PIC Z,ZZZ,ZZZ,ZZ9.99-.
       01  ED-AMT2                     PIC Z,ZZZ,ZZZ,ZZ9.99-.
       01  ED-CNT                      PIC ZZZ,ZZ9.
       01  ED-CNT2                     PIC ZZZ,ZZ9.
       01  ED-PCT                      PIC ZZ9.99.

           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DB-NAME                     PIC X(60).
       01  DB-USER                     PIC X(30).
       01  DB-PASS                     PIC X(30).
       01  H-BRANCH                    PIC X(06).
       01  H-CNT1                      PIC S9(09).
       01  H-AMT1                      PIC S9(13)V99.
       01  H-CNT2                      PIC S9(09).
       01  H-AMT2                      PIC S9(13)V99.
       01  H-TYPE                      PIC X(02).
       01  H-NAME                      PIC X(40).
       01  H-ACCT                      PIC S9(18).
       01  H-GL-DEP                    PIC S9(15)V99.
       01  H-ACCT-DEP                  PIC S9(15)V99.
           EXEC SQL END DECLARE SECTION END-EXEC.

           EXEC SQL INCLUDE SQLCA END-EXEC.
      *----------------------------------------------------------------*

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           STRING 'data/SQLRPT_' WS-TODAY '.TXT' DELIMITED BY SIZE
               INTO WS-RPT-NAME
           PERFORM 0100-CONNECT
           IF RPT-FAILED
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           OPEN OUTPUT RPT-FILE
           MOVE SPACES TO WS-LINE
           STRING 'COREBANK SQL MANAGEMENT REPORTS  RUN DATE '
                  WS-TODAY DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           PERFORM 1000-BRANCH-DEPOSITS
           IF NOT RPT-FAILED PERFORM 2000-LOAN-QUALITY   END-IF
           IF NOT RPT-FAILED PERFORM 3000-TOP-DEPOSITORS END-IF
           IF NOT RPT-FAILED PERFORM 4000-TRAN-VOLUME    END-IF
           IF NOT RPT-FAILED PERFORM 5000-RECONCILE      END-IF
           CLOSE RPT-FILE
           EXEC SQL DISCONNECT ALL END-EXEC
           IF RPT-FAILED
               MOVE 12 TO RETURN-CODE
           END-IF
           DISPLAY 'SQLRPT: REPORT WRITTEN TO '
                   FUNCTION TRIM(WS-RPT-NAME)
           GOBACK.

       0100-CONNECT.
           MOVE SPACES TO WS-ENV
           ACCEPT WS-ENV FROM ENVIRONMENT 'CB_DB'
           IF WS-ENV = SPACES
               MOVE 'corebank@localhost:5432' TO DB-NAME
           ELSE
               MOVE WS-ENV TO DB-NAME
           END-IF
           MOVE SPACES TO WS-ENV
           ACCEPT WS-ENV FROM ENVIRONMENT 'CB_DB_USER'
           IF WS-ENV = SPACES
               MOVE 'corebank' TO DB-USER
           ELSE
               MOVE WS-ENV TO DB-USER
           END-IF
           MOVE SPACES TO WS-ENV
           ACCEPT WS-ENV FROM ENVIRONMENT 'CB_DB_PASS'
           IF WS-ENV = SPACES
               MOVE 'corebank' TO DB-PASS
           ELSE
               MOVE WS-ENV TO DB-PASS
           END-IF
           EXEC SQL
               CONNECT :DB-USER IDENTIFIED BY :DB-PASS USING :DB-NAME
           END-EXEC
           IF SQLCODE NOT = ZERO
               DISPLAY 'SQLRPT: CANNOT CONNECT TO '
                       FUNCTION TRIM(DB-NAME) ', SQLCODE ' SQLCODE
               SET RPT-FAILED TO TRUE
           END-IF.

      *================================================================*
      * 1. DEPOSITS BY BRANCH                                          *
      *================================================================*
       1000-BRANCH-DEPOSITS.
           MOVE 'BRANCH DEPOSITS' TO WS-STEP
           PERFORM 8100-BLANK
           MOVE '1. DEPOSITS BY BRANCH' TO WS-LINE
           PERFORM 8000-PUT
           MOVE 'BRANCH  SB A/C      SB BALANCE  CA A/C      CA BAL'
               TO WS-LINE
           PERFORM 8000-PUT
           EXEC SQL
               DECLARE C_BRANCH CURSOR FOR
               SELECT branch, savings_accts, savings_bal,
                      current_accts, current_bal
                 FROM branch_deposits ORDER BY branch
           END-EXEC
           EXEC SQL OPEN C_BRANCH END-EXEC
           PERFORM 9000-CHECK-SQL
           PERFORM UNTIL RPT-FAILED
               EXEC SQL
                   FETCH C_BRANCH INTO :H-BRANCH, :H-CNT1, :H-AMT1,
                                       :H-CNT2, :H-AMT2
               END-EXEC
               IF SQLCODE = 100
                   EXIT PERFORM
               END-IF
               PERFORM 9000-CHECK-SQL
               IF NOT RPT-FAILED
                   MOVE H-CNT1 TO ED-CNT
                   MOVE H-AMT1 TO ED-AMT
                   MOVE H-CNT2 TO ED-CNT2
                   MOVE H-AMT2 TO ED-AMT2
                   MOVE SPACES TO WS-LINE
                   STRING H-BRANCH ' ' ED-CNT ' ' ED-AMT ' '
                          ED-CNT2 ' ' ED-AMT2
                          DELIMITED BY SIZE INTO WS-LINE
                   PERFORM 8000-PUT
               END-IF
           END-PERFORM
           EXEC SQL CLOSE C_BRANCH END-EXEC.

      *================================================================*
      * 2. LOAN BOOK QUALITY WITH NPA PERCENTAGE                       *
      *================================================================*
       2000-LOAN-QUALITY.
           MOVE 'LOAN QUALITY' TO WS-STEP
           PERFORM 8100-BLANK
           MOVE '2. LOAN BOOK QUALITY' TO WS-LINE
           PERFORM 8000-PUT
           MOVE 'TYPE   LOANS       OUTSTANDING   NPA OUTSTAND   NPA%'
               TO WS-LINE
           PERFORM 8000-PUT
           EXEC SQL
               DECLARE C_LOANS CURSOR FOR
               SELECT loan_type, loans, outstanding, npa_outstanding
                 FROM loan_quality ORDER BY loan_type
           END-EXEC
           EXEC SQL OPEN C_LOANS END-EXEC
           PERFORM 9000-CHECK-SQL
           PERFORM UNTIL RPT-FAILED
               EXEC SQL
                   FETCH C_LOANS INTO :H-TYPE, :H-CNT1, :H-AMT1,
                                      :H-AMT2
               END-EXEC
               IF SQLCODE = 100
                   EXIT PERFORM
               END-IF
               PERFORM 9000-CHECK-SQL
               IF NOT RPT-FAILED
                   IF H-AMT1 > ZERO
                       COMPUTE WS-NPA-PCT ROUNDED =
                           H-AMT2 * 100 / H-AMT1
                   ELSE
                       MOVE ZERO TO WS-NPA-PCT
                   END-IF
                   MOVE H-CNT1 TO ED-CNT
                   MOVE H-AMT1 TO ED-AMT
                   MOVE H-AMT2 TO ED-AMT2
                   MOVE WS-NPA-PCT TO ED-PCT
                   MOVE SPACES TO WS-LINE
                   STRING H-TYPE(1:1) '    ' ED-CNT ' ' ED-AMT ' '
                          ED-AMT2 ' ' ED-PCT
                          DELIMITED BY SIZE INTO WS-LINE
                   PERFORM 8000-PUT
               END-IF
           END-PERFORM
           EXEC SQL CLOSE C_LOANS END-EXEC.

      *================================================================*
      * 3. TOP 10 DEPOSITORS (ACTIVE ACCOUNTS, CREDIT BALANCES)        *
      *================================================================*
       3000-TOP-DEPOSITORS.
           MOVE 'TOP DEPOSITORS' TO WS-STEP
           PERFORM 8100-BLANK
           MOVE '3. TOP 10 DEPOSITORS' TO WS-LINE
           PERFORM 8000-PUT
           EXEC SQL
               DECLARE C_TOP CURSOR FOR
               SELECT acct_no, holder_name, balance
                 FROM accounts
                WHERE status IN ('A','D','F') AND balance > 0
                ORDER BY balance DESC, acct_no
                LIMIT 10
           END-EXEC
           EXEC SQL OPEN C_TOP END-EXEC
           PERFORM 9000-CHECK-SQL
           MOVE ZERO TO WS-RANK
           PERFORM UNTIL RPT-FAILED
               EXEC SQL
                   FETCH C_TOP INTO :H-ACCT, :H-NAME, :H-AMT1
               END-EXEC
               IF SQLCODE = 100
                   EXIT PERFORM
               END-IF
               PERFORM 9000-CHECK-SQL
               IF NOT RPT-FAILED
                   ADD 1 TO WS-RANK
                   MOVE H-AMT1 TO ED-AMT
                   MOVE SPACES TO WS-LINE
                   STRING WS-RANK '. ' H-ACCT(9:10) ' ' H-NAME(1:30)
                          ' ' ED-AMT
                          DELIMITED BY SIZE INTO WS-LINE
                   PERFORM 8000-PUT
               END-IF
           END-PERFORM
           EXEC SQL CLOSE C_TOP END-EXEC.

      *================================================================*
      * 4. TRANSACTION VOLUME BY TYPE                                  *
      *================================================================*
       4000-TRAN-VOLUME.
           MOVE 'TRAN VOLUME' TO WS-STEP
           PERFORM 8100-BLANK
           MOVE '4. TRANSACTION VOLUME BY TYPE' TO WS-LINE
           PERFORM 8000-PUT
           MOVE 'TYPE   COUNT              AMOUNT' TO WS-LINE
           PERFORM 8000-PUT
           EXEC SQL
               DECLARE C_TRAN CURSOR FOR
               SELECT tran_type, count(*), sum(amount)
                 FROM transactions
                GROUP BY tran_type ORDER BY 3 DESC
           END-EXEC
           EXEC SQL OPEN C_TRAN END-EXEC
           PERFORM 9000-CHECK-SQL
           PERFORM UNTIL RPT-FAILED
               EXEC SQL
                   FETCH C_TRAN INTO :H-TYPE, :H-CNT1, :H-AMT1
               END-EXEC
               IF SQLCODE = 100
                   EXIT PERFORM
               END-IF
               PERFORM 9000-CHECK-SQL
               IF NOT RPT-FAILED
                   MOVE H-CNT1 TO ED-CNT
                   MOVE H-AMT1 TO ED-AMT
                   MOVE SPACES TO WS-LINE
                   STRING H-TYPE '   ' ED-CNT ' ' ED-AMT
                          DELIMITED BY SIZE INTO WS-LINE
                   PERFORM 8000-PUT
               END-IF
           END-PERFORM
           EXEC SQL CLOSE C_TRAN END-EXEC.

      *================================================================*
      * 5. GL DEPOSIT LIABILITIES (2101 SB + 2102 CA) VS THE NET SUM  *
      *    OF ACCOUNT BALANCES.      A SINGLETON SELECT, NO CURSOR.   *
      *================================================================*
       5000-RECONCILE.
           MOVE 'RECONCILE' TO WS-STEP
           PERFORM 8100-BLANK
           MOVE '5. GL VS ACCOUNTS RECONCILIATION' TO WS-LINE
           PERFORM 8000-PUT
           EXEC SQL
               SELECT coalesce(-sum(balance), 0)
                 INTO :H-GL-DEP
                 FROM gl_balances WHERE gl_code IN (2101, 2102)
           END-EXEC
           PERFORM 9000-CHECK-SQL
           EXEC SQL
               SELECT coalesce(sum(balance), 0)
                 INTO :H-ACCT-DEP
                 FROM accounts
           END-EXEC
           PERFORM 9000-CHECK-SQL
           IF NOT RPT-FAILED
               MOVE H-GL-DEP TO ED-AMT
               MOVE H-ACCT-DEP TO ED-AMT2
               MOVE SPACES TO WS-LINE
               STRING 'GL 2101+2102 ' ED-AMT '   ACCOUNTS ' ED-AMT2
                      DELIMITED BY SIZE INTO WS-LINE
               PERFORM 8000-PUT
               IF H-GL-DEP = H-ACCT-DEP
                   MOVE 'RESULT: BALANCED' TO WS-LINE
               ELSE
                   MOVE 'RESULT: DIFFERENCE - CHECK GLRPT' TO WS-LINE
                   MOVE 4 TO RETURN-CODE
               END-IF
               PERFORM 8000-PUT
           END-IF.

      *================================================================*
      * UTILITIES                                                      *
      *================================================================*
       8000-PUT.
           DISPLAY FUNCTION TRIM(WS-LINE TRAILING)
           WRITE RPT-LINE FROM WS-LINE
           MOVE SPACES TO WS-LINE.

       8100-BLANK.
           MOVE SPACES TO WS-LINE
           PERFORM 8000-PUT.

       9000-CHECK-SQL.
           IF SQLCODE < ZERO
               DISPLAY 'SQLRPT: SQL ERROR IN ' FUNCTION TRIM(WS-STEP)
                       ', SQLCODE ' SQLCODE
               DISPLAY '        ' SQLERRMC
               SET RPT-FAILED TO TRUE
           END-IF.
