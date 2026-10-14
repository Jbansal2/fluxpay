       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLLOAD.
      *================================================================*
      * PROGRAM  : SQLLOAD                                             *
      * PURPOSE  : NIGHTLY COPY OF THE INDEXED MASTERS INTO POSTGRESQL *
      *            (THE "VSAM TO DB2" FEED). FULL REFRESH: EVERY TABLE *
      *            IS EMPTIED AND RELOADED INSIDE ONE UNIT OF WORK,    *
      *            SO REPORT USERS NEVER SEE A HALF-LOADED DATABASE -  *
      *            ANY SQL ERROR ROLLS THE WHOLE LOAD BACK.            *
      *            TABLES: CUSTOMERS, ACCOUNTS, TRANSACTIONS, LOANS,   *
      *            DEPOSITS, GL_BALANCES (SEE sql/schema.sql)          *
      * CONNECT  : ENVIRONMENT CB_DB (DEFAULT corebank@localhost:5432),*
      *            CB_DB_USER, CB_DB_PASS (DEFAULT corebank/corebank)  *
      * BUILD    : PRECOMPILED WITH OCESQL, SEE THE MAKEFILE 'sql'     *
      *            TARGET                                              *
      * RETURN   : 0 = LOADED, 12 = CONNECT OR SQL ERROR (ROLLED BACK) *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-FILE ASSIGN TO 'data/CUSTMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CUST-ID FILE STATUS IS WS-FS.
           SELECT ACCT-FILE ASSIGN TO 'data/ACCTMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ACCT-NO FILE STATUS IS WS-FS.
           SELECT TRAN-FILE ASSIGN TO 'data/TRANLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS WS-FS.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS LOAN-NO FILE STATUS IS WS-FS.
           SELECT DEP-FILE ASSIGN TO 'data/DEPMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS DEP-NO FILE STATUS IS WS-FS.
           SELECT GL-FILE ASSIGN TO 'data/GLMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS GL-CODE FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUST-FILE.
           COPY CUSTREC.
       FD  ACCT-FILE.
           COPY ACCTREC.
       FD  TRAN-FILE.
           COPY TRANREC.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  DEP-FILE.
           COPY DEPREC.
       FD  GL-FILE.
           COPY GLREC.

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-FAILED-SW                PIC X(01) VALUE 'N'.
           88  LOAD-FAILED                       VALUE 'Y'.
       01  WS-STEP                     PIC X(20).
       01  WS-ROWS                     PIC 9(09).
       01  WS-ENV                      PIC X(60).

           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DB-NAME                     PIC X(60).
       01  DB-USER                     PIC X(30).
       01  DB-PASS                     PIC X(30).
       01  H-CUST.
           05  H-CUST-ID               PIC S9(09).
           05  H-CUST-NAME             PIC X(40).
           05  H-CUST-DOB              PIC X(08).
           05  H-CUST-PAN              PIC X(10).
           05  H-CUST-MOBILE           PIC X(10).
           05  H-CUST-CITY             PIC X(20).
           05  H-CUST-STATE            PIC X(20).
           05  H-CUST-KYC              PIC X(01).
           05  H-CUST-RISK             PIC X(01).
       01  H-ACCT.
           05  H-ACCT-NO               PIC S9(18).
           05  H-ACCT-NAME             PIC X(40).
           05  H-ACCT-TYPE             PIC X(01).
           05  H-ACCT-STATUS           PIC X(01).
           05  H-ACCT-BAL              PIC S9(13)V99.
           05  H-ACCT-OD               PIC S9(13)V99.
           05  H-ACCT-BRANCH           PIC X(06).
           05  H-ACCT-OPENED           PIC X(08).
           05  H-ACCT-LASTTXN          PIC X(08).
       01  H-TRAN.
           05  H-TRAN-ID               PIC S9(18).
           05  H-TRAN-ACCT             PIC S9(18).
           05  H-TRAN-DATE             PIC X(08).
           05  H-TRAN-TYPE             PIC X(02).
           05  H-TRAN-AMT              PIC S9(13)V99.
           05  H-TRAN-BAL              PIC S9(13)V99.
           05  H-TRAN-NARR             PIC X(30).
           05  H-TRAN-SEQ              PIC S9(09).
       01  H-LOAN.
           05  H-LOAN-NO               PIC S9(18).
           05  H-LOAN-ACCT             PIC S9(18).
           05  H-LOAN-TYPE             PIC X(01).
           05  H-LOAN-STATUS           PIC X(01).
           05  H-LOAN-PRIN             PIC S9(13)V99.
           05  H-LOAN-RATE             PIC S9(03)V9(04).
           05  H-LOAN-TENURE           PIC S9(04).
           05  H-LOAN-EMI              PIC S9(13)V99.
           05  H-LOAN-OUT              PIC S9(13)V99.
           05  H-LOAN-OVERDUE          PIC S9(04).
           05  H-LOAN-NEXTDUE          PIC X(08).
       01  H-DEP.
           05  H-DEP-NO                PIC S9(18).
           05  H-DEP-ACCT              PIC S9(18).
           05  H-DEP-TYPE              PIC X(01).
           05  H-DEP-STATUS            PIC X(01).
           05  H-DEP-AMT               PIC S9(13)V99.
           05  H-DEP-RATE              PIC S9(03)V9(04).
           05  H-DEP-TENURE            PIC S9(04).
           05  H-DEP-MATDATE           PIC X(08).
           05  H-DEP-MATVAL            PIC S9(13)V99.
           05  H-DEP-DEPOSITED         PIC S9(13)V99.
       01  H-GL.
           05  H-GL-CODE               PIC S9(09).
           05  H-GL-NAME               PIC X(30).
           05  H-GL-TYPE               PIC X(01).
           05  H-GL-BAL                PIC S9(15)V99.
           EXEC SQL END DECLARE SECTION END-EXEC.

           EXEC SQL INCLUDE SQLCA END-EXEC.
      *----------------------------------------------------------------*

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 0100-CONNECT
           IF LOAD-FAILED
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           MOVE 'EMPTY TABLES' TO WS-STEP
           EXEC SQL DELETE FROM transactions END-EXEC
           PERFORM 9000-CHECK-SQL
           EXEC SQL DELETE FROM loans END-EXEC
           PERFORM 9000-CHECK-SQL
           EXEC SQL DELETE FROM deposits END-EXEC
           PERFORM 9000-CHECK-SQL
           EXEC SQL DELETE FROM accounts END-EXEC
           PERFORM 9000-CHECK-SQL
           EXEC SQL DELETE FROM customers END-EXEC
           PERFORM 9000-CHECK-SQL
           EXEC SQL DELETE FROM gl_balances END-EXEC
           PERFORM 9000-CHECK-SQL

           IF NOT LOAD-FAILED PERFORM 1000-LOAD-CUSTOMERS    END-IF
           IF NOT LOAD-FAILED PERFORM 2000-LOAD-ACCOUNTS     END-IF
           IF NOT LOAD-FAILED PERFORM 3000-LOAD-TRANSACTIONS END-IF
           IF NOT LOAD-FAILED PERFORM 4000-LOAD-LOANS        END-IF
           IF NOT LOAD-FAILED PERFORM 5000-LOAD-DEPOSITS     END-IF
           IF NOT LOAD-FAILED PERFORM 6000-LOAD-GL           END-IF

           IF LOAD-FAILED
               EXEC SQL ROLLBACK WORK END-EXEC
               DISPLAY 'SQLLOAD: LOAD ROLLED BACK - DATABASE UNCHANGED'
               MOVE 12 TO RETURN-CODE
           ELSE
               EXEC SQL COMMIT WORK END-EXEC
               DISPLAY 'SQLLOAD: LOAD COMMITTED'
               MOVE ZERO TO RETURN-CODE
           END-IF
           EXEC SQL DISCONNECT ALL END-EXEC
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
               DISPLAY 'SQLLOAD: CANNOT CONNECT TO '
                       FUNCTION TRIM(DB-NAME) ', SQLCODE ' SQLCODE
               SET LOAD-FAILED TO TRUE
           ELSE
               DISPLAY 'SQLLOAD: CONNECTED TO ' FUNCTION TRIM(DB-NAME)
           END-IF.

      *================================================================*
      * ONE PARAGRAPH PER TABLE                                        *
      *================================================================*
       1000-LOAD-CUSTOMERS.
           MOVE 'CUSTOMERS' TO WS-STEP
           MOVE ZERO TO WS-ROWS
           OPEN INPUT CUST-FILE
           IF WS-FS NOT = '00'
               PERFORM 8000-REPORT-COUNT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE OR LOAD-FAILED
               READ CUST-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       MOVE CUST-ID         TO H-CUST-ID
                       MOVE CUST-NAME       TO H-CUST-NAME
                       MOVE CUST-DOB        TO H-CUST-DOB
                       MOVE CUST-PAN        TO H-CUST-PAN
                       MOVE CUST-MOBILE     TO H-CUST-MOBILE
                       MOVE CUST-CITY       TO H-CUST-CITY
                       MOVE CUST-STATE      TO H-CUST-STATE
                       MOVE CUST-KYC-STATUS TO H-CUST-KYC
                       MOVE CUST-RISK       TO H-CUST-RISK
                       EXEC SQL
                           INSERT INTO customers
                             (cust_id, name, dob, pan, mobile, city,
                              state, kyc_status, risk)
                           VALUES
                             (:H-CUST-ID, TRIM(:H-CUST-NAME),
                              CAST(NULLIF(:H-CUST-DOB,'00000000')
                                   AS DATE),
                              :H-CUST-PAN, :H-CUST-MOBILE,
                              TRIM(:H-CUST-CITY), TRIM(:H-CUST-STATE),
                              :H-CUST-KYC, :H-CUST-RISK)
                       END-EXEC
                       PERFORM 9000-CHECK-SQL
                       ADD 1 TO WS-ROWS
               END-READ
           END-PERFORM
           CLOSE CUST-FILE
           PERFORM 8000-REPORT-COUNT.

       2000-LOAD-ACCOUNTS.
           MOVE 'ACCOUNTS' TO WS-STEP
           MOVE ZERO TO WS-ROWS
           OPEN INPUT ACCT-FILE
           IF WS-FS NOT = '00'
               PERFORM 8000-REPORT-COUNT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE OR LOAD-FAILED
               READ ACCT-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       MOVE ACCT-NO            TO H-ACCT-NO
                       MOVE ACCT-HOLDER-NAME   TO H-ACCT-NAME
                       MOVE ACCT-TYPE          TO H-ACCT-TYPE
                       MOVE ACCT-STATUS        TO H-ACCT-STATUS
                       MOVE ACCT-BALANCE       TO H-ACCT-BAL
                       MOVE ACCT-OD-LIMIT      TO H-ACCT-OD
                       MOVE ACCT-BRANCH-CODE   TO H-ACCT-BRANCH
                       MOVE ACCT-OPEN-DATE     TO H-ACCT-OPENED
                       MOVE ACCT-LAST-TXN-DATE TO H-ACCT-LASTTXN
                       EXEC SQL
                           INSERT INTO accounts
                             (acct_no, holder_name, acct_type, status,
                              balance, od_limit, branch, opened_on,
                              last_txn_on)
                           VALUES
                             (:H-ACCT-NO, TRIM(:H-ACCT-NAME),
                              :H-ACCT-TYPE, :H-ACCT-STATUS,
                              :H-ACCT-BAL, :H-ACCT-OD, :H-ACCT-BRANCH,
                              CAST(NULLIF(:H-ACCT-OPENED,'00000000')
                                   AS DATE),
                              CAST(NULLIF(:H-ACCT-LASTTXN,'00000000')
                                   AS DATE))
                       END-EXEC
                       PERFORM 9000-CHECK-SQL
                       ADD 1 TO WS-ROWS
               END-READ
           END-PERFORM
           CLOSE ACCT-FILE
           PERFORM 8000-REPORT-COUNT.

       3000-LOAD-TRANSACTIONS.
           MOVE 'TRANSACTIONS' TO WS-STEP
           MOVE ZERO TO WS-ROWS
           OPEN INPUT TRAN-FILE
           IF WS-FS NOT = '00'
               PERFORM 8000-REPORT-COUNT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE OR LOAD-FAILED
               READ TRAN-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-ROWS
                       MOVE TRAN-ID        TO H-TRAN-ID
                       MOVE TRAN-ACCT-NO   TO H-TRAN-ACCT
                       MOVE TRAN-DATE      TO H-TRAN-DATE
                       MOVE TRAN-TYPE      TO H-TRAN-TYPE
                       MOVE TRAN-AMOUNT    TO H-TRAN-AMT
                       MOVE TRAN-BAL-AFTER TO H-TRAN-BAL
                       MOVE TRAN-NARRATION TO H-TRAN-NARR
                       MOVE WS-ROWS        TO H-TRAN-SEQ
                       EXEC SQL
                           INSERT INTO transactions
                             (tran_id, acct_no, tran_date, tran_type,
                              amount, balance_after, narration, seq)
                           VALUES
                             (:H-TRAN-ID, :H-TRAN-ACCT,
                              CAST(:H-TRAN-DATE AS DATE), :H-TRAN-TYPE,
                              :H-TRAN-AMT, :H-TRAN-BAL,
                              TRIM(:H-TRAN-NARR), :H-TRAN-SEQ)
                       END-EXEC
                       PERFORM 9000-CHECK-SQL
               END-READ
           END-PERFORM
           CLOSE TRAN-FILE
           PERFORM 8000-REPORT-COUNT.

       4000-LOAD-LOANS.
           MOVE 'LOANS' TO WS-STEP
           MOVE ZERO TO WS-ROWS
           OPEN INPUT LOAN-FILE
           IF WS-FS NOT = '00'
               PERFORM 8000-REPORT-COUNT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE OR LOAD-FAILED
               READ LOAN-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       MOVE LOAN-NO            TO H-LOAN-NO
                       MOVE LOAN-ACCT-NO       TO H-LOAN-ACCT
                       MOVE LOAN-TYPE          TO H-LOAN-TYPE
                       MOVE LOAN-STATUS        TO H-LOAN-STATUS
                       MOVE LOAN-PRINCIPAL     TO H-LOAN-PRIN
                       MOVE LOAN-RATE          TO H-LOAN-RATE
                       MOVE LOAN-TENURE        TO H-LOAN-TENURE
                       MOVE LOAN-EMI           TO H-LOAN-EMI
                       MOVE LOAN-OUTSTANDING   TO H-LOAN-OUT
                       MOVE LOAN-OVERDUE-EMIS  TO H-LOAN-OVERDUE
                       MOVE LOAN-NEXT-DUE-DATE TO H-LOAN-NEXTDUE
                       EXEC SQL
                           INSERT INTO loans
                             (loan_no, acct_no, loan_type, status,
                              principal, rate, tenure_months, emi,
                              outstanding, overdue_emis, next_due)
                           VALUES
                             (:H-LOAN-NO, :H-LOAN-ACCT, :H-LOAN-TYPE,
                              :H-LOAN-STATUS, :H-LOAN-PRIN,
                              :H-LOAN-RATE, :H-LOAN-TENURE,
                              :H-LOAN-EMI, :H-LOAN-OUT,
                              :H-LOAN-OVERDUE,
                              CAST(NULLIF(:H-LOAN-NEXTDUE,'00000000')
                                   AS DATE))
                       END-EXEC
                       PERFORM 9000-CHECK-SQL
                       ADD 1 TO WS-ROWS
               END-READ
           END-PERFORM
           CLOSE LOAN-FILE
           PERFORM 8000-REPORT-COUNT.

       5000-LOAD-DEPOSITS.
           MOVE 'DEPOSITS' TO WS-STEP
           MOVE ZERO TO WS-ROWS
           OPEN INPUT DEP-FILE
           IF WS-FS NOT = '00'
               PERFORM 8000-REPORT-COUNT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE OR LOAD-FAILED
               READ DEP-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       MOVE DEP-NO              TO H-DEP-NO
                       MOVE DEP-ACCT-NO         TO H-DEP-ACCT
                       MOVE DEP-TYPE            TO H-DEP-TYPE
                       MOVE DEP-STATUS          TO H-DEP-STATUS
                       MOVE DEP-AMOUNT          TO H-DEP-AMT
                       MOVE DEP-RATE            TO H-DEP-RATE
                       MOVE DEP-TENURE          TO H-DEP-TENURE
                       MOVE DEP-MATURITY-DATE   TO H-DEP-MATDATE
                       MOVE DEP-MATURITY-AMT    TO H-DEP-MATVAL
                       MOVE DEP-TOTAL-DEPOSITED TO H-DEP-DEPOSITED
                       EXEC SQL
                           INSERT INTO deposits
                             (dep_no, acct_no, dep_type, status,
                              amount, rate, tenure_months,
                              maturity_date, maturity_value, deposited)
                           VALUES
                             (:H-DEP-NO, :H-DEP-ACCT, :H-DEP-TYPE,
                              :H-DEP-STATUS, :H-DEP-AMT, :H-DEP-RATE,
                              :H-DEP-TENURE,
                              CAST(NULLIF(:H-DEP-MATDATE,'00000000')
                                   AS DATE),
                              :H-DEP-MATVAL, :H-DEP-DEPOSITED)
                       END-EXEC
                       PERFORM 9000-CHECK-SQL
                       ADD 1 TO WS-ROWS
               END-READ
           END-PERFORM
           CLOSE DEP-FILE
           PERFORM 8000-REPORT-COUNT.

       6000-LOAD-GL.
           MOVE 'GL_BALANCES' TO WS-STEP
           MOVE ZERO TO WS-ROWS
           OPEN INPUT GL-FILE
           IF WS-FS NOT = '00'
               PERFORM 8000-REPORT-COUNT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE OR LOAD-FAILED
               READ GL-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       MOVE GL-CODE    TO H-GL-CODE
                       MOVE GL-NAME    TO H-GL-NAME
                       MOVE GL-TYPE    TO H-GL-TYPE
                       MOVE GL-BALANCE TO H-GL-BAL
                       EXEC SQL
                           INSERT INTO gl_balances
                             (gl_code, gl_name, gl_type, balance)
                           VALUES
                             (:H-GL-CODE, TRIM(:H-GL-NAME),
                              :H-GL-TYPE, :H-GL-BAL)
                       END-EXEC
                       PERFORM 9000-CHECK-SQL
                       ADD 1 TO WS-ROWS
               END-READ
           END-PERFORM
           CLOSE GL-FILE
           PERFORM 8000-REPORT-COUNT.

       8000-REPORT-COUNT.
           DISPLAY 'SQLLOAD: ' WS-STEP ' ROWS ' WS-ROWS.

       9000-CHECK-SQL.
           IF SQLCODE NOT = ZERO
               DISPLAY 'SQLLOAD: SQL ERROR IN ' FUNCTION TRIM(WS-STEP)
                       ' SQLCODE ' SQLCODE
               DISPLAY '         ' SQLERRMC
               SET LOAD-FAILED TO TRUE
           END-IF.
