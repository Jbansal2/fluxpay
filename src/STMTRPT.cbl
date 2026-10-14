       IDENTIFICATION DIVISION.
       PROGRAM-ID. STMTRPT.
      *================================================================*
      * PROGRAM  : STMTRPT                                             *
      * PURPOSE  : ACCOUNT STATEMENT FOR A DATE RANGE. READS THE TRAN  *
      *            LOG, FILTERS BY ACCOUNT AND DATE, WRITES A PRINT    *
      *            FILE AND ECHOES IT TO THE SCREEN.                   *
      * FILES    : ACCTMAST (INDEXED, INPUT)                           *
      *            TRANLOG  (SEQUENTIAL, INPUT)                        *
      *            STMTOUT  (PRINT FILE, OUTPUT)                       *
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
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-ACCT-FS.
           SELECT TRAN-FILE ASSIGN TO 'data/TRANLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRAN-FS.
           SELECT STMT-FILE ASSIGN TO WS-STMT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STMT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
           COPY ACCTREC.
       FD  TRAN-FILE.
           COPY TRANREC.
       FD  STMT-FILE.
       01  STMT-LINE                   PIC X(100).

       WORKING-STORAGE SECTION.
           COPY COMMWS.

       01  WS-STMT-FS                  PIC X(02).
       01  WS-STMT-FILENAME            PIC X(40).

       01  WS-EOF-SW                   PIC X(01) VALUE 'N'.
           88  END-OF-TRANS                      VALUE 'Y'.

       01  WS-IN-ACCT-NO               PIC 9(10).
       01  WS-FROM-DATE                PIC 9(08).
       01  WS-TO-DATE                  PIC 9(08).

       01  WS-TOTALS.
           05  WS-TOT-CREDIT           PIC S9(13)V99 COMP-3.
           05  WS-TOT-DEBIT            PIC S9(13)V99 COMP-3.
           05  WS-LINE-CNT             PIC 9(05).

      * REPORT LINES
       01  RPT-TITLE.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(40)
               VALUE 'CORE BANKING SYSTEM - ACCOUNT STATEMENT'.
       01  RPT-ACCT-LINE.
           05  FILLER                  PIC X(12) VALUE 'ACCOUNT NO: '.
           05  RPT-ACCT-NO             PIC 9(10).
           05  FILLER                  PIC X(04) VALUE SPACES.
           05  FILLER                  PIC X(08) VALUE 'HOLDER: '.
           05  RPT-HOLDER              PIC X(40).
       01  RPT-PERIOD-LINE.
           05  FILLER                  PIC X(08) VALUE 'PERIOD: '.
           05  RPT-FROM                PIC 9999/99/99.
           05  FILLER                  PIC X(04) VALUE ' TO '.
           05  RPT-TO                  PIC 9999/99/99.
       01  RPT-COL-HEAD.
           05  FILLER                  PIC X(12) VALUE 'DATE'.
           05  FILLER                  PIC X(31) VALUE 'NARRATION'.
           05  FILLER                  PIC X(19) VALUE '       DEBIT'.
           05  FILLER                  PIC X(19) VALUE '      CREDIT'.
           05  FILLER                  PIC X(19) VALUE '     BALANCE'.
       01  RPT-DASHES                  PIC X(100) VALUE ALL '-'.
       01  RPT-DETAIL.
           05  RPT-DATE                PIC 9999/99/99.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  RPT-NARR                PIC X(30).
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  RPT-DEBIT               PIC Z,ZZZ,ZZZ,ZZ9.99
                                       BLANK WHEN ZERO.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  RPT-CREDIT              PIC Z,ZZZ,ZZZ,ZZ9.99
                                       BLANK WHEN ZERO.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  RPT-BALANCE             PIC -ZZZ,ZZZ,ZZ9.99.
       01  RPT-TOTAL-LINE.
           05  FILLER                  PIC X(43) VALUE 'TOTALS'.
           05  RPT-TOT-DEBIT           PIC Z,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  RPT-TOT-CREDIT          PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  RPT-NONE-LINE               PIC X(50)
               VALUE 'NO TRANSACTIONS FOUND FOR THIS PERIOD.'.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-ACCEPT-PARMS
           PERFORM 1100-VALIDATE-ACCOUNT
           IF NOT ACCT-FS-OK
               GOBACK
           END-IF
           PERFORM 1200-OPEN-REPORT-FILES
           IF NOT TRAN-FS-OK
               GOBACK
           END-IF
           PERFORM 2000-WRITE-HEADER
           PERFORM 8000-READ-TRAN
           PERFORM 3000-PROCESS-TRAN UNTIL END-OF-TRANS
           PERFORM 4000-WRITE-TOTALS
           CLOSE TRAN-FILE STMT-FILE
           DISPLAY 'STATEMENT SAVED TO ' WS-STMT-FILENAME
           GOBACK.

       1000-ACCEPT-PARMS.
           INITIALIZE WS-TOTALS
           MOVE 'N' TO WS-EOF-SW
           DISPLAY 'ACCOUNT NO          : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'FROM DATE (YYYYMMDD): ' WITH NO ADVANCING
           ACCEPT WS-FROM-DATE
           DISPLAY 'TO DATE   (YYYYMMDD): ' WITH NO ADVANCING
           ACCEPT WS-TO-DATE
           IF WS-TO-DATE = ZERO
               MOVE FUNCTION CURRENT-DATE(1:8) TO WS-TO-DATE
           END-IF
           IF WS-FROM-DATE > WS-TO-DATE
               DISPLAY 'FROM DATE AFTER TO DATE - SWAPPING.'
               MOVE WS-FROM-DATE TO WS-CURR-DATE
               MOVE WS-TO-DATE   TO WS-FROM-DATE
               MOVE WS-CURR-DATE TO WS-TO-DATE
           END-IF.

       1100-VALIDATE-ACCOUNT.
           OPEN INPUT ACCT-FILE
           IF NOT ACCT-FS-OK
               DISPLAY 'ACCOUNT MASTER NOT AVAILABLE, FS=' WS-ACCT-FS
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-ACCT-NO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   DISPLAY 'ACCOUNT ' WS-IN-ACCT-NO ' NOT FOUND.'
           END-READ
           IF ACCT-FS-OK
               MOVE ACCT-NO          TO RPT-ACCT-NO
               MOVE ACCT-HOLDER-NAME TO RPT-HOLDER
           END-IF
      *    KEEP THE READ STATUS - CLOSE WOULD OVERWRITE IT WITH 00
           MOVE WS-ACCT-FS TO WS-STMT-FS
           CLOSE ACCT-FILE
           MOVE WS-STMT-FS TO WS-ACCT-FS.

       1200-OPEN-REPORT-FILES.
           OPEN INPUT TRAN-FILE
           IF NOT TRAN-FS-OK
               DISPLAY 'NO TRANSACTIONS ON FILE YET.'
               EXIT PARAGRAPH
           END-IF
           STRING 'data/STMT_' DELIMITED BY SIZE
                  WS-IN-ACCT-NO DELIMITED BY SIZE
                  '.TXT'        DELIMITED BY SIZE
                  INTO WS-STMT-FILENAME
           END-STRING
           OPEN OUTPUT STMT-FILE.

       2000-WRITE-HEADER.
           MOVE WS-FROM-DATE TO RPT-FROM
           MOVE WS-TO-DATE   TO RPT-TO
           MOVE RPT-TITLE       TO STMT-LINE PERFORM 7000-EMIT
           MOVE RPT-DASHES      TO STMT-LINE PERFORM 7000-EMIT
           MOVE RPT-ACCT-LINE   TO STMT-LINE PERFORM 7000-EMIT
           MOVE RPT-PERIOD-LINE TO STMT-LINE PERFORM 7000-EMIT
           MOVE RPT-DASHES      TO STMT-LINE PERFORM 7000-EMIT
           MOVE RPT-COL-HEAD    TO STMT-LINE PERFORM 7000-EMIT
           MOVE RPT-DASHES      TO STMT-LINE PERFORM 7000-EMIT.

       3000-PROCESS-TRAN.
           IF TRAN-ACCT-NO = WS-IN-ACCT-NO
              AND TRAN-DATE >= WS-FROM-DATE
              AND TRAN-DATE <= WS-TO-DATE
               PERFORM 3100-FORMAT-DETAIL
           END-IF
           PERFORM 8000-READ-TRAN.

       3100-FORMAT-DETAIL.
           MOVE TRAN-DATE      TO RPT-DATE
           MOVE TRAN-NARRATION TO RPT-NARR
           IF TRAN-NARRATION = SPACES
               EVALUATE TRUE
                   WHEN TRAN-DEPOSIT  MOVE 'CASH DEPOSIT'   TO RPT-NARR
                   WHEN TRAN-WITHDRAW MOVE 'CASH WITHDRAWAL'
                                                            TO RPT-NARR
                   WHEN TRAN-XFER-OUT MOVE 'TRANSFER OUT'   TO RPT-NARR
                   WHEN TRAN-XFER-IN  MOVE 'TRANSFER IN'    TO RPT-NARR
                   WHEN TRAN-INTEREST MOVE 'INTEREST'       TO RPT-NARR
                   WHEN TRAN-LOAN-DISB MOVE 'LOAN DISBURSAL' TO RPT-NARR
                   WHEN TRAN-LOAN-REPAY
                                     MOVE 'LOAN REPAYMENT' TO RPT-NARR
                   WHEN TRAN-ATM-WITHDRAW
                                     MOVE 'ATM WITHDRAWAL' TO RPT-NARR
                   WHEN TRAN-DEP-OPEN MOVE 'DEPOSIT OPENED' TO RPT-NARR
                   WHEN TRAN-DEP-MATURITY
                                     MOVE 'DEPOSIT PAYOUT' TO RPT-NARR
                   WHEN TRAN-RD-INSTALMENT
                                     MOVE 'RD INSTALMENT'  TO RPT-NARR
                   WHEN TRAN-CHARGES  MOVE 'BANK CHARGES'   TO RPT-NARR
                   WHEN TRAN-CHEQUE-PAID
                                     MOVE 'CHEQUE PAID'    TO RPT-NARR
                   WHEN TRAN-PAYMENT-OUT
                                     MOVE 'PAYMENT SENT'   TO RPT-NARR
                   WHEN TRAN-PAYMENT-IN
                                     MOVE 'PAYMENT RECEIVED' TO RPT-NARR
                   WHEN TRAN-OD-INTEREST
                                     MOVE 'OD INTEREST'    TO RPT-NARR
                   WHEN TRAN-CARD-PAYMENT
                                     MOVE 'CREDIT CARD PAYMENT'
                                                           TO RPT-NARR
                   WHEN TRAN-NACH-DEBIT
                                     MOVE 'NACH DEBIT'     TO RPT-NARR
                   WHEN TRAN-OUTWARD-CHQ
                                     MOVE 'CHEQUE COLLECTED'
                                                           TO RPT-NARR
                   WHEN TRAN-TRADE-DEBIT
                                     MOVE 'TRADE FINANCE'  TO RPT-NARR
                   WHEN TRAN-TRADE-REFUND
                                     MOVE 'MARGIN REFUND'  TO RPT-NARR
                   WHEN TRAN-FX-INWARD
                                     MOVE 'FOREX INWARD'   TO RPT-NARR
                   WHEN TRAN-FX-OUTWARD
                                     MOVE 'FOREX OUTWARD'  TO RPT-NARR
               END-EVALUATE
           END-IF
           IF TRAN-CREDIT-TYPE
               MOVE ZERO        TO RPT-DEBIT
               MOVE TRAN-AMOUNT TO RPT-CREDIT
               ADD TRAN-AMOUNT  TO WS-TOT-CREDIT
           ELSE
               MOVE TRAN-AMOUNT TO RPT-DEBIT
               MOVE ZERO        TO RPT-CREDIT
               ADD TRAN-AMOUNT  TO WS-TOT-DEBIT
           END-IF
           MOVE TRAN-BAL-AFTER TO RPT-BALANCE
           ADD 1 TO WS-LINE-CNT
           MOVE RPT-DETAIL TO STMT-LINE
           PERFORM 7000-EMIT.

       4000-WRITE-TOTALS.
           MOVE RPT-DASHES TO STMT-LINE PERFORM 7000-EMIT
           IF WS-LINE-CNT = ZERO
               MOVE RPT-NONE-LINE TO STMT-LINE
           ELSE
               MOVE WS-TOT-DEBIT  TO RPT-TOT-DEBIT
               MOVE WS-TOT-CREDIT TO RPT-TOT-CREDIT
               MOVE RPT-TOTAL-LINE TO STMT-LINE
           END-IF
           PERFORM 7000-EMIT
           MOVE RPT-DASHES TO STMT-LINE PERFORM 7000-EMIT.

       7000-EMIT.
           WRITE STMT-LINE
           DISPLAY STMT-LINE.

       8000-READ-TRAN.
           READ TRAN-FILE
               AT END SET END-OF-TRANS TO TRUE
           END-READ.
