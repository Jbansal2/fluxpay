       IDENTIFICATION DIVISION.
       PROGRAM-ID. STMTBAT.
      *================================================================*
      * PROGRAM  : STMTBAT                                             *
      * PURPOSE  : CUSTOMER STATEMENTS                                 *
      *            1. MONTHLY: ONE STATEMENT FOR EVERY ACCOUNT WITH    *
      *               ACTIVITY IN THE MONTH. THE MONTH'S JOURNAL IS    *
      *               SORTED BY ACCOUNT AND POSTING ORDER (SORT WITH   *
      *               INPUT / OUTPUT PROCEDURES) AND WRITTEN ON A      *
      *               CONTROL BREAK PER ACCOUNT.                       *
      *            2. ON REQUEST: EVERY QUEUED STMTREQ REQUEST, FOR    *
      *               ITS OWN PERIOD; THE QUEUE IS THEN EMPTIED.       *
      *            OPENING BALANCE = BALANCE BEFORE THE FIRST ENTRY OF *
      *            THE PERIOD (BALANCE AFTER IT, LESS/PLUS ITS AMOUNT).*
      *            DELIVERY: IF THE PRIMARY HOLDER HAS AN E-MAIL       *
      *            ADDRESS A LINE GOES TO THE MAIL QUEUE (THE MAIL     *
      *            GATEWAY SENDS THE FILE AS AN ATTACHMENT), OTHERWISE *
      *            TO THE PRINT QUEUE FOR POSTING.                     *
      * INPUT    : MONTH YYYYMM (BLANK = LAST MONTH)                   *
      * OUTPUT   : data/STMT_<ACCOUNT>_<FROM>_<TO>.TXT,                *
      *            data/MAILQ.DAT, data/PRINTQ.DAT (APPENDED)          *
      * FILES    : TRANLOG, ACCTMAST, STMTREQ                          *
      * CALLS    : CUSTLKUP, DATEUTIL                                  *
      * RETURN   : 0 = OK, 12 = NO TRANSACTION LOG                     *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRAN-FILE ASSIGN TO 'data/TRANLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRAN-FS.
           SELECT SORT-FILE ASSIGN TO 'data/SORTWK4.TMP'.
           SELECT ACCT-FILE ASSIGN TO 'data/ACCTMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS RANDOM
               RECORD KEY IS ACCT-NO FILE STATUS IS WS-ACCT-FS.
           SELECT REQ-FILE ASSIGN TO 'data/STMTREQ.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-REQ-FS.
           SELECT STMT-FILE ASSIGN TO WS-STMT-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STMT-FS.
           SELECT MAIL-FILE ASSIGN TO 'data/MAILQ.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MAIL-FS.
           SELECT PRINT-FILE ASSIGN TO 'data/PRINTQ.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PRINT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRAN-FILE.
           COPY TRANREC.
       SD  SORT-FILE.
      *    ACCOUNT, THEN THE ORDER THE ENTRIES WERE POSTED IN (THE LOG
      *    POSITION - TRANSACTION IDS COME FROM SEVERAL PROGRAMS AND
      *    ARE NOT A RELIABLE ORDER ACROSS THEM)
       01  SORT-RECORD.
           05  SK-ACCT-NO              PIC 9(10).
           05  SK-SEQ                  PIC 9(09).
           05  SK-DATA                 PIC X(106).
       FD  ACCT-FILE.
           COPY ACCTREC.
       FD  REQ-FILE.
           COPY SREQREC.
       FD  STMT-FILE.
       01  STMT-LINE                   PIC X(100).
       FD  MAIL-FILE.
       01  MAIL-LINE                   PIC X(150).
       FD  PRINT-FILE.
       01  PRINT-LINE                  PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-TRAN-FS                  PIC X(02).
       01  WS-ACCT-FS                  PIC X(02).
       01  WS-REQ-FS                   PIC X(02).
       01  WS-STMT-FS                  PIC X(02).
       01  WS-MAIL-FS                  PIC X(02).
       01  WS-PRINT-FS                 PIC X(02).
       01  WS-STMT-NAME                PIC X(60).
       01  WS-IN-MONTH                 PIC X(06).
       01  WS-TODAY                    PIC 9(08).
       01  WS-FROM                     PIC 9(08).
       01  WS-TO                       PIC 9(08).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-SORT-EOF-SW              PIC X(01).
           88  END-OF-SORT                       VALUE 'Y'.
      * THE TRANSACTION BEING PRINTED (FROM THE SORT OR THE LOG)
           COPY TRANREC REPLACING LEADING ==TRAN-== BY ==WT-==.
      * CURRENT STATEMENT
       01  WS-CUR-ACCT                 PIC 9(10).
       01  WS-OPENING                  PIC S9(13)V99.
       01  WS-CLOSING                  PIC S9(13)V99.
       01  WS-TOT-DR                   PIC 9(13)V99.
       01  WS-TOT-CR                   PIC 9(13)V99.
       01  WS-ENTRIES                  PIC 9(05).
       01  WS-OPEN-SW                  PIC X(01) VALUE 'N'.
           88  STATEMENT-OPEN                    VALUE 'Y'.
       01  WS-COUNTS.
           05  WS-MONTHLY              PIC 9(05) VALUE ZERO.
           05  WS-REQUESTED            PIC 9(05) VALUE ZERO.
           05  WS-MAILED               PIC 9(05) VALUE ZERO.
           05  WS-PRINTED              PIC 9(05) VALUE ZERO.
       01  WS-LINE                     PIC X(100).
       01  WS-LOG-SEQ                  PIC 9(09) VALUE ZERO.
       01  ED-AMT                      PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  ED-AMT2                     PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  ED-BAL                      PIC -Z,ZZZ,ZZZ,ZZ9.99.
       01  ED-CNT                      PIC ZZ,ZZ9.
           COPY CLKPARM.
           COPY DATEPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           DISPLAY 'STATEMENT MONTH (YYYYMM, BLANK = LAST MONTH): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-MONTH
           PERFORM 0100-PERIOD
           OPEN INPUT TRAN-FILE
           IF WS-TRAN-FS NOT = '00'
               DISPLAY 'STMTBAT: NO TRANSACTION LOG'
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           CLOSE TRAN-FILE
           OPEN INPUT ACCT-FILE
           OPEN EXTEND MAIL-FILE
           IF WS-MAIL-FS = '35'
               OPEN OUTPUT MAIL-FILE
           END-IF
           OPEN EXTEND PRINT-FILE
           IF WS-PRINT-FS = '35'
               OPEN OUTPUT PRINT-FILE
           END-IF
           SORT SORT-FILE
               ON ASCENDING KEY SK-ACCT-NO SK-SEQ
               INPUT PROCEDURE  1000-SELECT-MONTH
               OUTPUT PROCEDURE 2000-WRITE-STATEMENTS
           PERFORM 3000-REQUESTS
           CLOSE ACCT-FILE MAIL-FILE PRINT-FILE
           MOVE 'Y' TO CLK-CLOSE
           CALL 'CUSTLKUP' USING CLK-PARMS
           PERFORM 9000-SUMMARY
           GOBACK.

      *    FIRST AND LAST DAY OF THE STATEMENT MONTH
       0100-PERIOD.
           IF WS-IN-MONTH IS NUMERIC
               STRING WS-IN-MONTH '01' DELIMITED BY SIZE INTO WS-FROM
           ELSE
               SET DU-ADD-MONTHS TO TRUE
               MOVE WS-TODAY TO DU-DATE-1
               MOVE -1 TO DU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DU-RESULT-DATE TO WS-FROM
               MOVE '01' TO WS-FROM(7:2)
           END-IF
           SET DU-ADD-MONTHS TO TRUE
           MOVE WS-FROM TO DU-DATE-1
           MOVE 1 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           SET DU-ADD-DAYS TO TRUE
           MOVE DU-RESULT-DATE TO DU-DATE-1
           MOVE -1 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO WS-TO.

      *================================================================*
      * 1 - MONTHLY STATEMENTS                                         *
      *================================================================*
       1000-SELECT-MONTH.
           OPEN INPUT TRAN-FILE
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ TRAN-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-LOG-SEQ
                       IF TRAN-DATE >= WS-FROM AND TRAN-DATE <= WS-TO
                           MOVE TRAN-ACCT-NO TO SK-ACCT-NO
                           MOVE WS-LOG-SEQ   TO SK-SEQ
                           MOVE TRAN-RECORD  TO SK-DATA
                           RELEASE SORT-RECORD
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TRAN-FILE.

       2000-WRITE-STATEMENTS.
           MOVE 'N' TO WS-SORT-EOF-SW
           MOVE ZERO TO WS-CUR-ACCT
           PERFORM UNTIL END-OF-SORT
               RETURN SORT-FILE
                   AT END SET END-OF-SORT TO TRUE
                   NOT AT END
                       MOVE SK-DATA TO WT-RECORD
                       IF WT-ACCT-NO NOT = WS-CUR-ACCT
                           IF STATEMENT-OPEN
                               PERFORM 5200-END-STATEMENT
                           END-IF
                           PERFORM 5000-START-STATEMENT
                           ADD 1 TO WS-MONTHLY
                       END-IF
                       PERFORM 5100-ADD-ENTRY
               END-RETURN
           END-PERFORM
           IF STATEMENT-OPEN
               PERFORM 5200-END-STATEMENT
           END-IF.

      *================================================================*
      * 2 - REQUESTED STATEMENTS                                       *
      *================================================================*
       3000-REQUESTS.
           OPEN INPUT REQ-FILE
           IF WS-REQ-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-REQ-FS NOT = '00'
               READ REQ-FILE
                   AT END MOVE '10' TO WS-REQ-FS
                   NOT AT END PERFORM 3100-ONE-REQUEST
               END-READ
           END-PERFORM
           CLOSE REQ-FILE
      *    ALL DONE - EMPTY THE QUEUE
           OPEN OUTPUT REQ-FILE
           CLOSE REQ-FILE.

       3100-ONE-REQUEST.
           ADD 1 TO WS-REQUESTED
           MOVE SR-FROM-DATE TO WS-FROM
           MOVE SR-TO-DATE   TO WS-TO
           MOVE SR-ACCT-NO   TO WS-CUR-ACCT
           OPEN INPUT TRAN-FILE
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ TRAN-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF TRAN-ACCT-NO = SR-ACCT-NO
                          AND TRAN-DATE >= WS-FROM
                          AND TRAN-DATE <= WS-TO
                           MOVE TRAN-RECORD TO WT-RECORD
                           IF NOT STATEMENT-OPEN
                               PERFORM 5000-START-STATEMENT
                           END-IF
                           PERFORM 5100-ADD-ENTRY
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TRAN-FILE
           IF NOT STATEMENT-OPEN
      *        NO ENTRIES IN THE PERIOD - STILL SEND A NIL STATEMENT
               MOVE SR-ACCT-NO TO WT-ACCT-NO
               MOVE ZERO TO WT-AMOUNT WT-BAL-AFTER
               PERFORM 5000-START-STATEMENT
               MOVE '  NO TRANSACTIONS IN THIS PERIOD' TO WS-LINE
               PERFORM 8000-PUT
           END-IF
           PERFORM 5200-END-STATEMENT.

      *================================================================*
      * STATEMENT WRITER                                               *
      *================================================================*
       5000-START-STATEMENT.
           MOVE WT-ACCT-NO TO WS-CUR-ACCT ACCT-NO
           MOVE SPACES TO WS-STMT-NAME
           STRING 'data/STMT_' WS-CUR-ACCT '_' WS-FROM '_' WS-TO
                  '.TXT' DELIMITED BY SIZE INTO WS-STMT-NAME
           OPEN OUTPUT STMT-FILE
           MOVE 'Y' TO WS-OPEN-SW
           MOVE ZERO TO WS-TOT-DR WS-TOT-CR WS-ENTRIES
      *    BALANCE BEFORE THE FIRST ENTRY
           IF WT-CREDIT-TYPE
               COMPUTE WS-OPENING = WT-BAL-AFTER - WT-AMOUNT
           ELSE
               COMPUTE WS-OPENING = WT-BAL-AFTER + WT-AMOUNT
           END-IF
           MOVE WS-OPENING TO WS-CLOSING
           READ ACCT-FILE
               INVALID KEY MOVE '** ACCOUNT NOT ON FILE **'
                               TO ACCT-HOLDER-NAME
           END-READ
           MOVE ALL '=' TO WS-LINE
           PERFORM 8000-PUT
           MOVE 'COREBANK - STATEMENT OF ACCOUNT' TO WS-LINE
           PERFORM 8000-PUT
           STRING 'ACCOUNT : ' WS-CUR-ACCT '   BRANCH ' ACCT-BRANCH-CODE
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           STRING 'NAME    : ' ACCT-HOLDER-NAME
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           STRING 'PERIOD  : ' WS-FROM(7:2) '/' WS-FROM(5:2) '/'
                  WS-FROM(1:4) ' TO ' WS-TO(7:2) '/' WS-TO(5:2) '/'
                  WS-TO(1:4) DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE ALL '-' TO WS-LINE
           PERFORM 8000-PUT
           STRING 'DATE       NARRATION                     '
                  '         DEBIT          CREDIT          BALANCE'
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-OPENING TO ED-BAL
           STRING '           OPENING BALANCE' DELIMITED BY SIZE
               INTO WS-LINE
           MOVE ED-BAL TO WS-LINE(84:17)
           PERFORM 8000-PUT.

       5100-ADD-ENTRY.
           ADD 1 TO WS-ENTRIES
           STRING WT-DATE(7:2) '/' WT-DATE(5:2) '/' WT-DATE(1:4) ' '
                  WT-NARRATION DELIMITED BY SIZE INTO WS-LINE
           MOVE WT-AMOUNT TO ED-AMT
           IF WT-CREDIT-TYPE
               MOVE ED-AMT TO WS-LINE(67:16)
               ADD WT-AMOUNT TO WS-TOT-CR
           ELSE
               MOVE ED-AMT TO WS-LINE(51:16)
               ADD WT-AMOUNT TO WS-TOT-DR
           END-IF
           MOVE WT-BAL-AFTER TO ED-BAL WS-CLOSING
           MOVE ED-BAL TO WS-LINE(84:17)
           PERFORM 8000-PUT.

       5200-END-STATEMENT.
           MOVE ALL '-' TO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-TOT-DR TO ED-AMT
           MOVE WS-TOT-CR TO ED-AMT2
           MOVE WS-CLOSING TO ED-BAL
           MOVE '           CLOSING BALANCE / TOTALS' TO WS-LINE
           MOVE ED-AMT  TO WS-LINE(51:16)
           MOVE ED-AMT2 TO WS-LINE(67:16)
           MOVE ED-BAL  TO WS-LINE(84:17)
           PERFORM 8000-PUT
           MOVE WS-ENTRIES TO ED-CNT
           STRING 'ENTRIES : ' ED-CNT
               '   THIS IS A COMPUTER GENERATED STATEMENT.'
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           CLOSE STMT-FILE
           MOVE 'N' TO WS-OPEN-SW
           PERFORM 5300-QUEUE-DELIVERY.

       5300-QUEUE-DELIVERY.
           MOVE WS-CUR-ACCT TO CLK-ACCT-NO
           MOVE 'N' TO CLK-CLOSE
           CALL 'CUSTLKUP' USING CLK-PARMS
           IF CLK-OK AND CLK-EMAIL NOT = SPACES
               MOVE SPACES TO MAIL-LINE
               STRING 'TO=' FUNCTION TRIM(CLK-EMAIL)
                      '|SUBJECT=STATEMENT ' WS-CUR-ACCT ' '
                      WS-FROM '-' WS-TO
                      '|ATTACH=' FUNCTION TRIM(WS-STMT-NAME)
                   DELIMITED BY SIZE INTO MAIL-LINE
               WRITE MAIL-LINE
               ADD 1 TO WS-MAILED
           ELSE
               MOVE SPACES TO PRINT-LINE
               STRING 'PRINT ' FUNCTION TRIM(WS-STMT-NAME)
                      ' FOR ' WS-CUR-ACCT
                   DELIMITED BY SIZE INTO PRINT-LINE
               WRITE PRINT-LINE
               ADD 1 TO WS-PRINTED
           END-IF.

       8000-PUT.
           WRITE STMT-LINE FROM WS-LINE
           MOVE SPACES TO WS-LINE.

       9000-SUMMARY.
           DISPLAY 'STATEMENT RUN'
           MOVE WS-MONTHLY TO ED-CNT
           DISPLAY '  MONTHLY STATEMENTS   : ' ED-CNT
           MOVE WS-REQUESTED TO ED-CNT
           DISPLAY '  REQUESTED STATEMENTS : ' ED-CNT
           MOVE WS-MAILED TO ED-CNT
           DISPLAY '  QUEUED FOR E-MAIL    : ' ED-CNT
           MOVE WS-PRINTED TO ED-CNT
           DISPLAY '  QUEUED FOR PRINTING  : ' ED-CNT.
