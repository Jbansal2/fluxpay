       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLINQ.
      *================================================================*
      * PROGRAM  : SQLINQ                                              *
      * PURPOSE  : ACCOUNT ENQUIRY AGAINST THE SQL REPORTING DATABASE  *
      *            (THE DB2-STYLE COPY LOADED BY SQLLOAD), SHOWING     *
      *            PARAMETERISED EMBEDDED SQL:                         *
      *              <10-DIGIT ACCOUNT>  ACCOUNT ROW (SINGLETON SELECT)*
      *                                  + LAST 5 TRANSACTIONS (CURSOR *
      *                                  WITH A HOST VARIABLE IN THE   *
      *                                  WHERE CLAUSE, NEWEST FIRST)   *
      *              ?<TEXT>             ACCOUNTS WHOSE HOLDER NAME    *
      *                                  CONTAINS THE TEXT (LIKE)      *
      *              BLANK LINE          END                           *
      *            THE DATA IS AS OF THE LAST SQLLOAD - IT IS FOR      *
      *            ENQUIRY ONLY, NEVER FOR POSTING.                    *
      * CONNECT  : SAME ENVIRONMENT VARIABLES AS SQLLOAD               *
      * RETURN   : 0 = OK, 12 = CONNECT FAILED                         *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-INPUT                    PIC X(40).
       01  WS-ENV                      PIC X(60).
       01  WS-DONE-SW                  PIC X(01) VALUE 'N'.
           88  DONE                              VALUE 'Y'.
       01  WS-ROWS                     PIC 9(04).
       01  ED-AMT                      PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-AMT2                     PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.

           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DB-NAME                     PIC X(60).
       01  DB-USER                     PIC X(30).
       01  DB-PASS                     PIC X(30).
       01  H-ACCT                      PIC S9(18).
       01  H-NAME                      PIC X(40).
       01  H-TYPE                      PIC X(01).
       01  H-STATUS                    PIC X(01).
       01  H-BAL                       PIC S9(13)V99.
       01  H-OD                        PIC S9(13)V99.
       01  H-BRANCH                    PIC X(06).
       01  H-PATTERN                   PIC X(42).
       01  H-T-DATE                    PIC X(10).
       01  H-T-TYPE                    PIC X(02).
       01  H-T-AMT                     PIC S9(13)V99.
       01  H-T-BAL                     PIC S9(13)V99.
       01  H-T-NARR                    PIC X(30).
           EXEC SQL END DECLARE SECTION END-EXEC.

           EXEC SQL INCLUDE SQLCA END-EXEC.
      *----------------------------------------------------------------*

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 0100-CONNECT
           IF SQLCODE NOT = ZERO
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           PERFORM UNTIL DONE
               DISPLAY 'ACCOUNT NO, ?NAME, OR BLANK TO END: '
                       WITH NO ADVANCING
               MOVE SPACES TO WS-INPUT
               ACCEPT WS-INPUT
               EVALUATE TRUE
                   WHEN WS-INPUT = SPACES
                       SET DONE TO TRUE
                   WHEN WS-INPUT(1:1) = '?'
                       PERFORM 2000-NAME-SEARCH
                   WHEN WS-INPUT(1:10) IS NUMERIC
                       PERFORM 1000-ACCOUNT
                   WHEN OTHER
                       DISPLAY 'ENTER 10 DIGITS OR ?TEXT.'
               END-EVALUATE
           END-PERFORM
           EXEC SQL DISCONNECT ALL END-EXEC
           GOBACK.

       0100-CONNECT.
           MOVE SPACES TO WS-ENV
           ACCEPT WS-ENV FROM ENVIRONMENT 'CB_DB'
           MOVE 'corebank@localhost:5432' TO DB-NAME
           IF WS-ENV NOT = SPACES
               MOVE WS-ENV TO DB-NAME
           END-IF
           MOVE SPACES TO WS-ENV
           ACCEPT WS-ENV FROM ENVIRONMENT 'CB_DB_USER'
           MOVE 'corebank' TO DB-USER
           IF WS-ENV NOT = SPACES
               MOVE WS-ENV TO DB-USER
           END-IF
           MOVE SPACES TO WS-ENV
           ACCEPT WS-ENV FROM ENVIRONMENT 'CB_DB_PASS'
           MOVE 'corebank' TO DB-PASS
           IF WS-ENV NOT = SPACES
               MOVE WS-ENV TO DB-PASS
           END-IF
           EXEC SQL
               CONNECT :DB-USER IDENTIFIED BY :DB-PASS USING :DB-NAME
           END-EXEC
           IF SQLCODE NOT = ZERO
               DISPLAY 'SQLINQ: CANNOT CONNECT, SQLCODE ' SQLCODE
           END-IF.

      *================================================================*
      * ONE ACCOUNT + ITS LAST FIVE TRANSACTIONS                       *
      *================================================================*
       1000-ACCOUNT.
           MOVE WS-INPUT(1:10) TO H-ACCT
           EXEC SQL
               SELECT holder_name, acct_type, status, balance,
                      od_limit, branch
                 INTO :H-NAME, :H-TYPE, :H-STATUS, :H-BAL, :H-OD,
                      :H-BRANCH
                 FROM accounts WHERE acct_no = :H-ACCT
           END-EXEC
           EVALUATE TRUE
               WHEN SQLCODE = 100
                   DISPLAY 'ACCOUNT NOT IN THE REPORTING DATABASE.'
                   EXIT PARAGRAPH
               WHEN SQLCODE < ZERO
                   DISPLAY 'SQL ERROR ' SQLCODE ' ' SQLERRMC
                   EXIT PARAGRAPH
           END-EVALUATE
           MOVE H-BAL TO ED-AMT
           MOVE H-OD  TO ED-AMT2
           DISPLAY 'ACCOUNT ' WS-INPUT(1:10) '  ' H-NAME
           DISPLAY 'TYPE ' H-TYPE '  STATUS ' H-STATUS '  BRANCH '
                   H-BRANCH
           DISPLAY 'BALANCE ' ED-AMT '  OD LIMIT ' ED-AMT2
           EXEC SQL
               DECLARE C_RECENT CURSOR FOR
               SELECT to_char(tran_date, 'DD/MM/YYYY'), tran_type,
                      amount, balance_after, coalesce(narration, ' ')
                 FROM transactions
                WHERE acct_no = :H-ACCT
                ORDER BY seq DESC
                LIMIT 5
           END-EXEC
           EXEC SQL OPEN C_RECENT END-EXEC
           MOVE ZERO TO WS-ROWS
           PERFORM UNTIL SQLCODE NOT = ZERO
               EXEC SQL
                   FETCH C_RECENT INTO :H-T-DATE, :H-T-TYPE, :H-T-AMT,
                                       :H-T-BAL, :H-T-NARR
               END-EXEC
               IF SQLCODE = ZERO
                   ADD 1 TO WS-ROWS
                   MOVE H-T-AMT TO ED-AMT
                   MOVE H-T-BAL TO ED-AMT2
                   DISPLAY '  ' H-T-DATE ' ' H-T-TYPE ED-AMT ED-AMT2
                           ' ' H-T-NARR
               END-IF
           END-PERFORM
           EXEC SQL CLOSE C_RECENT END-EXEC
           DISPLAY WS-ROWS ' RECENT TRANSACTION(S)'.

      *================================================================*
      * NAME SEARCH                                                    *
      *================================================================*
       2000-NAME-SEARCH.
           MOVE SPACES TO H-PATTERN
           STRING '%' FUNCTION TRIM(WS-INPUT(2:39)) '%'
               DELIMITED BY SIZE INTO H-PATTERN
           MOVE FUNCTION UPPER-CASE(H-PATTERN) TO H-PATTERN
           EXEC SQL
               DECLARE C_NAMES CURSOR FOR
               SELECT acct_no, holder_name, balance
                 FROM accounts
                WHERE upper(holder_name) LIKE trim(:H-PATTERN)
                ORDER BY holder_name, acct_no
                LIMIT 10
           END-EXEC
           EXEC SQL OPEN C_NAMES END-EXEC
           MOVE ZERO TO WS-ROWS
           PERFORM UNTIL SQLCODE NOT = ZERO
               EXEC SQL
                   FETCH C_NAMES INTO :H-ACCT, :H-NAME, :H-BAL
               END-EXEC
               IF SQLCODE = ZERO
                   ADD 1 TO WS-ROWS
                   MOVE H-BAL TO ED-AMT
                   DISPLAY '  ' H-ACCT(9:10) ' ' H-NAME ED-AMT
               END-IF
           END-PERFORM
           EXEC SQL CLOSE C_NAMES END-EXEC
           DISPLAY WS-ROWS ' MATCHING ACCOUNT(S)'.
