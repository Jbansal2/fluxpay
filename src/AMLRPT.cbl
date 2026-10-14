       IDENTIFICATION DIVISION.
       PROGRAM-ID. AMLRPT.
      *================================================================*
      * PROGRAM  : AMLRPT                                              *
      * PURPOSE  : MONTHLY AML REPORT. LISTS THE MONTH'S ALERTS,       *
      *            SPLIT INTO CTR (CASH THRESHOLD - FILED MONTHLY) AND *
      *            STR (SUSPICIOUS - FOR THE COMPLIANCE OFFICER), AND  *
      *            WRITES THE CTR EXTRACT TO data/CTR_<YYYYMM>.TXT AS  *
      *            THE INPUT FOR THE REGULATORY RETURN.                *
      * INPUT    : MONTH YYYYMM (BLANK = CURRENT MONTH)                *
      * RETURN   : 0 = NO STR, 4 = STR ALERTS NEED REVIEW              *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALERT-FILE ASSIGN TO 'data/AMLALERT.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ALERT-FS.
           SELECT CTR-FILE ASSIGN TO WS-CTR-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CTR-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  ALERT-FILE.
           COPY AMLAREC.
       FD  CTR-FILE.
       01  CTR-LINE                    PIC X(100).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
       01  WS-ALERT-FS                 PIC X(02).
       01  WS-CTR-FS                   PIC X(02).
       01  WS-CTR-FILENAME             PIC X(40).
       01  WS-IN-MONTH                 PIC X(06).
       01  WS-MONTH                    PIC 9(06).
       01  WS-PASS                     PIC X(03).
       01  WS-CTR-CNT                  PIC 9(05) VALUE ZERO.
       01  WS-STR-CNT                  PIC 9(05) VALUE ZERO.
       01  WS-CTR-AMT                  PIC 9(15)V99 VALUE ZERO.
       01  WS-E-AMT                    PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-E-CNT                    PIC ZZ,ZZ9.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE ZERO TO WS-CTR-CNT WS-STR-CNT WS-CTR-AMT
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           DISPLAY 'MONTH YYYYMM (BLANK = CURRENT): ' WITH NO ADVANCING
           ACCEPT WS-IN-MONTH
           IF WS-IN-MONTH IS NUMERIC
               MOVE WS-IN-MONTH TO WS-MONTH
           ELSE
               MOVE WS-CURR-YYYYMM TO WS-MONTH
           END-IF
           STRING 'data/CTR_' WS-MONTH '.TXT' DELIMITED BY SIZE
                  INTO WS-CTR-FILENAME
           END-STRING
           OPEN OUTPUT CTR-FILE
           MOVE 'DATE    |ACCOUNT   |RULE        |AMOUNT         |NAME'
                                                 TO CTR-LINE
           WRITE CTR-LINE

           DISPLAY '=================================================='
           DISPLAY 'AML ALERTS FOR ' WS-MONTH
           DISPLAY '=================================================='
           MOVE 'CTR' TO WS-PASS
           DISPLAY 'CASH TRANSACTION REPORTS (CTR)'
           PERFORM 1000-PASS
           DISPLAY ' '
           MOVE 'STR' TO WS-PASS
           DISPLAY 'SUSPICIOUS TRANSACTIONS (STR) - REVIEW REQUIRED'
           PERFORM 1000-PASS
           CLOSE CTR-FILE

           DISPLAY '--------------------------------------------------'
           MOVE WS-CTR-CNT TO WS-E-CNT
           MOVE WS-CTR-AMT TO WS-E-AMT
           DISPLAY 'CTR ALERTS : ' WS-E-CNT '   CASH VALUE ' WS-E-AMT
           MOVE WS-STR-CNT TO WS-E-CNT
           DISPLAY 'STR ALERTS : ' WS-E-CNT
           DISPLAY 'CTR EXTRACT: ' WS-CTR-FILENAME
           IF WS-STR-CNT > ZERO
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE ZERO TO RETURN-CODE
           END-IF
           GOBACK.

      *    ONE READ OF THE ALERT FILE PER ALERT KIND
       1000-PASS.
           OPEN INPUT ALERT-FILE
           IF WS-ALERT-FS NOT = '00'
               DISPLAY '  NO ALERTS ON FILE.'
               EXIT PARAGRAPH
           END-IF
           PERFORM FOREVER
               READ ALERT-FILE
                   AT END EXIT PERFORM
               END-READ
               IF AL-DATE(1:6) = WS-MONTH AND AL-KIND = WS-PASS
                   PERFORM 1100-SHOW-ALERT
               END-IF
           END-PERFORM
           CLOSE ALERT-FILE.

       1100-SHOW-ALERT.
           MOVE AL-AMOUNT TO WS-E-AMT
           DISPLAY '  ' AL-DATE ' ' AL-ACCT-NO ' ' AL-RULE ' '
                   WS-E-AMT ' ' AL-NAME(1:24)
           IF AL-IS-CTR
               ADD 1 TO WS-CTR-CNT
               ADD AL-AMOUNT TO WS-CTR-AMT
               MOVE SPACES TO CTR-LINE
               STRING AL-DATE '|' AL-ACCT-NO '|' AL-RULE '|'
                      WS-E-AMT '|' FUNCTION TRIM(AL-NAME)
                      DELIMITED BY SIZE INTO CTR-LINE
               END-STRING
               WRITE CTR-LINE
           ELSE
               ADD 1 TO WS-STR-CNT
           END-IF.
