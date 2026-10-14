       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIBATCH.
      *================================================================*
      * PROGRAM  : SIBATCH                                             *
      * PURPOSE  : EXECUTES STANDING INSTRUCTIONS DUE ON OR BEFORE THE *
      *            RUN DATE.                                           *
      *              T - TRANSFER  : DEBIT (TO) + CREDIT (TI), WITH A  *
      *                              REVERSAL IF THE CREDIT FAILS      *
      *              L - LOAN EMI  : LNEMIPAY, SAME AS THE LOAN SCREEN *
      *              R - RD        : INSTALMENT DEBIT + RD UPDATE      *
      *            NOT ENOUGH MONEY: RETRIED ON EACH OF THE NEXT TWO   *
      *            DAYS; AFTER THE THIRD FAILURE THE MONTH IS MARKED   *
      *            MISSED AND A 100 + GST FAILURE FEE IS CHARGED.      *
      *            A PAID-OFF LOAN, A COMPLETED RD OR A PASSED END     *
      *            DATE COMPLETES THE SI. A CLOSED / FROZEN ACCOUNT    *
      *            SUSPENDS IT FOR THE BRANCH TO LOOK AT.              *
      * FILES    : SIMAST, LOANMAST, DEPMAST (I-O)                     *
      * CALLS    : ACCTPOST, LNEMIPAY, GLPOST, DATEUTIL                *
      * RETURN   : 0 = ALL OK, 4 = RETRIES / MISSES / SUSPENSIONS      *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SI-FILE ASSIGN TO 'data/SIMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SI-ID
               FILE STATUS IS WS-SI-FS.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LOAN-NO
               FILE STATUS IS WS-LOAN-FS.
           SELECT DEP-FILE ASSIGN TO 'data/DEPMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS DEP-NO
               FILE STATUS IS WS-DEP-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  SI-FILE.
           COPY SIREC.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  DEP-FILE.
           COPY DEPREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY DATEPARM.
           COPY LNPPARM.
           COPY GLPARM.
           COPY GLCODES.

       01  WS-SI-FS                    PIC X(02).
           88  SI-FS-OK                          VALUE '00'.
       01  WS-LOAN-FS                  PIC X(02).
           88  LOAN-FS-OK                        VALUE '00'.
       01  WS-DEP-FS                   PIC X(02).
           88  DEP-FS-OK                         VALUE '00'.

       01  WS-IN-RUN-DATE              PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-RETURN-CODE              PIC 9(02) VALUE ZERO.

      * OUTCOME OF ONE EXECUTION
       01  WS-OUTCOME                  PIC X(01).
           88  OUTCOME-PAID                      VALUE 'P'.
           88  OUTCOME-NO-FUNDS                  VALUE 'F'.
           88  OUTCOME-COMPLETE                  VALUE 'C'.
           88  OUTCOME-SUSPEND                   VALUE 'S'.
       01  WS-OUTCOME-TEXT             PIC X(30).
       01  WS-PAID-AMOUNT              PIC 9(11)V99.

       01  WS-MAX-ATTEMPTS             PIC 9(01) VALUE 3.
       01  WS-FAIL-FEE                 PIC 9(07)V99 VALUE 100.00.
       01  WS-FAIL-GST                 PIC 9(07)V99 VALUE 18.00.

       01  WS-STATS.
           05  WS-DUE-CNT              PIC 9(05).
           05  WS-PAID-CNT             PIC 9(05).
           05  WS-RETRY-CNT            PIC 9(05).
           05  WS-MISSED-CNT           PIC 9(05).
           05  WS-DONE-CNT             PIC 9(05).
           05  WS-SUSP-CNT             PIC 9(05).
           05  WS-PAID-TOTAL           PIC 9(13)V99.
       01  WS-E-AMT                    PIC ZZ,ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           INITIALIZE WS-STATS
           MOVE ZERO TO WS-RETURN-CODE
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           DISPLAY 'SI RUN DATE YYYYMMDD (BLANK = TODAY): '
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
           OPEN I-O SI-FILE
           IF NOT SI-FS-OK
               DISPLAY 'NO STANDING INSTRUCTIONS ON FILE.'
               MOVE ZERO TO RETURN-CODE
               GOBACK
           END-IF
           OPEN I-O LOAN-FILE
           OPEN I-O DEP-FILE
           DISPLAY 'STANDING INSTRUCTIONS DUE BY ' WS-RUN-DATE
           MOVE ZERO TO SI-ID
           START SI-FILE KEY IS NOT LESS THAN SI-ID
               INVALID KEY CONTINUE
               NOT INVALID KEY PERFORM 1000-SCAN
           END-START
           CLOSE SI-FILE
           IF LOAN-FS-OK
               CLOSE LOAN-FILE
           END-IF
           IF DEP-FS-OK
               CLOSE DEP-FILE
           END-IF
           PERFORM 9000-SUMMARY
           MOVE WS-RETURN-CODE TO RETURN-CODE
           GOBACK.

       1000-SCAN.
           PERFORM FOREVER
               READ SI-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF SI-ACTIVE AND SI-NEXT-RUN-DATE <= WS-RUN-DATE
                   ADD 1 TO WS-DUE-CNT
                   PERFORM 2000-EXECUTE-ONE
               END-IF
           END-PERFORM.

       2000-EXECUTE-ONE.
           MOVE ZERO TO WS-PAID-AMOUNT
           MOVE SPACES TO WS-OUTCOME-TEXT
           IF SI-END-DATE < WS-RUN-DATE
               SET OUTCOME-COMPLETE TO TRUE
               MOVE 'END DATE REACHED' TO WS-OUTCOME-TEXT
           ELSE
               EVALUATE TRUE
                   WHEN SI-TRANSFER      PERFORM 3000-DO-TRANSFER
                   WHEN SI-LOAN-EMI      PERFORM 4000-DO-LOAN-EMI
                   WHEN SI-RD-INSTALMENT PERFORM 5000-DO-RD
               END-EVALUATE
           END-IF
           PERFORM 6000-RECORD-OUTCOME
           REWRITE SI-RECORD
           MOVE WS-PAID-AMOUNT TO WS-E-AMT
           DISPLAY '  SI ' SI-ID ' ' SI-TYPE ' ' SI-FROM-ACCT ' -> '
                   SI-TARGET ' ' WS-E-AMT ' '
                   FUNCTION TRIM(SI-LAST-RESULT).

      *----------------------------------------------------------------*
      * TRANSFER                                                       *
      *----------------------------------------------------------------*
       3000-DO-TRANSFER.
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE SI-FROM-ACCT TO PP-ACCT-NO
           MOVE SI-AMOUNT    TO PP-AMOUNT
           MOVE 'TO'         TO PP-TRAN-TYPE
           MOVE SI-TARGET    TO PP-REF-ACCT
           STRING 'STANDING INSTRUCTION ' SI-ID
                  DELIMITED BY SIZE INTO PP-NARRATION
           END-STRING
           CALL 'ACCTPOST' USING POST-PARMS
           EVALUATE TRUE
               WHEN PP-OK
                   CONTINUE
               WHEN PP-INSUFFICIENT
                   SET OUTCOME-NO-FUNDS TO TRUE
                   EXIT PARAGRAPH
               WHEN OTHER
                   SET OUTCOME-SUSPEND TO TRUE
                   MOVE 'DEBIT ACCOUNT BLOCKED' TO WS-OUTCOME-TEXT
                   EXIT PARAGRAPH
           END-EVALUATE
           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE SI-TARGET    TO PP-ACCT-NO
           MOVE SI-AMOUNT    TO PP-AMOUNT
           MOVE 'TI'         TO PP-TRAN-TYPE
           MOVE SI-FROM-ACCT TO PP-REF-ACCT
           STRING 'STANDING INSTRUCTION ' SI-ID
                  DELIMITED BY SIZE INTO PP-NARRATION
           END-STRING
           CALL 'ACCTPOST' USING POST-PARMS
           IF PP-OK
               SET OUTCOME-PAID TO TRUE
               MOVE SI-AMOUNT TO WS-PAID-AMOUNT
           ELSE
      *        PUT THE MONEY BACK SO THE TRANSFER SUSPENSE CLEARS
               INITIALIZE POST-PARMS
               SET PP-CREDIT TO TRUE
               MOVE SI-FROM-ACCT TO PP-ACCT-NO
               MOVE SI-AMOUNT    TO PP-AMOUNT
               MOVE 'TI'         TO PP-TRAN-TYPE
               MOVE SI-TARGET    TO PP-REF-ACCT
               MOVE 'REVERSAL - SI TARGET CLOSED' TO PP-NARRATION
               CALL 'ACCTPOST' USING POST-PARMS
               SET OUTCOME-SUSPEND TO TRUE
               MOVE 'TARGET ACCOUNT BLOCKED' TO WS-OUTCOME-TEXT
           END-IF.

      *----------------------------------------------------------------*
      * LOAN EMI                                                       *
      *----------------------------------------------------------------*
       4000-DO-LOAN-EMI.
           IF NOT LOAN-FS-OK AND WS-LOAN-FS NOT = '23'
               SET OUTCOME-SUSPEND TO TRUE
               MOVE 'LOAN FILE UNAVAILABLE' TO WS-OUTCOME-TEXT
               EXIT PARAGRAPH
           END-IF
           MOVE SI-TARGET TO LOAN-NO
           READ LOAN-FILE
               INVALID KEY
                   SET OUTCOME-COMPLETE TO TRUE
                   MOVE 'LOAN NOT FOUND' TO WS-OUTCOME-TEXT
                   EXIT PARAGRAPH
           END-READ
           MOVE 'PE' TO LNP-MODE
           STRING 'LOAN EMI SI ' SI-ID DELIMITED BY SIZE
                  INTO LNP-NARRATION
           END-STRING
           CALL 'LNEMIPAY' USING LNP-PARMS LOAN-RECORD
           EVALUATE TRUE
               WHEN LNP-OK
                   REWRITE LOAN-RECORD
                   SET OUTCOME-PAID TO TRUE
                   MOVE LNP-AMOUNT-DUE TO WS-PAID-AMOUNT
                   IF LOAN-CLOSED
                       SET OUTCOME-COMPLETE TO TRUE
                       MOVE 'LOAN FULLY REPAID' TO WS-OUTCOME-TEXT
                   END-IF
               WHEN LNP-LOAN-CLOSED
                   SET OUTCOME-COMPLETE TO TRUE
                   MOVE 'LOAN ALREADY CLOSED' TO WS-OUTCOME-TEXT
               WHEN LNP-INSUFFICIENT
                   SET OUTCOME-NO-FUNDS TO TRUE
               WHEN OTHER
                   SET OUTCOME-SUSPEND TO TRUE
                   MOVE 'DEBIT ACCOUNT BLOCKED' TO WS-OUTCOME-TEXT
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RD INSTALMENT                                                  *
      *----------------------------------------------------------------*
       5000-DO-RD.
           IF NOT DEP-FS-OK AND WS-DEP-FS NOT = '23'
               SET OUTCOME-SUSPEND TO TRUE
               MOVE 'DEPOSIT FILE UNAVAILABLE' TO WS-OUTCOME-TEXT
               EXIT PARAGRAPH
           END-IF
           MOVE SI-TARGET TO DEP-NO
           READ DEP-FILE
               INVALID KEY
                   SET OUTCOME-COMPLETE TO TRUE
                   MOVE 'RD NOT FOUND' TO WS-OUTCOME-TEXT
                   EXIT PARAGRAPH
           END-READ
           IF NOT DEP-ACTIVE OR DEP-RD-PAID-CNT >= DEP-TENURE
               SET OUTCOME-COMPLETE TO TRUE
               MOVE 'RD COMPLETE OR CLOSED' TO WS-OUTCOME-TEXT
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE SI-FROM-ACCT TO PP-ACCT-NO
           MOVE DEP-AMOUNT   TO PP-AMOUNT
           MOVE 'RI'         TO PP-TRAN-TYPE
           MOVE DEP-NO       TO PP-REF-ACCT
           STRING 'RD INSTALMENT SI ' SI-ID
                  DELIMITED BY SIZE INTO PP-NARRATION
           END-STRING
           CALL 'ACCTPOST' USING POST-PARMS
           EVALUATE TRUE
               WHEN PP-OK
                   ADD 1 TO DEP-RD-PAID-CNT
                   ADD DEP-AMOUNT TO DEP-TOTAL-DEPOSITED
                   IF DEP-RD-MISSED-CNT > ZERO
                       SUBTRACT 1 FROM DEP-RD-MISSED-CNT
                   END-IF
                   SET DU-ADD-MONTHS TO TRUE
                   MOVE DEP-RD-NEXT-DUE TO DU-DATE-1
                   MOVE 1 TO DU-NUMBER
                   CALL 'DATEUTIL' USING DATE-PARMS
                   MOVE DU-RESULT-DATE TO DEP-RD-NEXT-DUE
                   REWRITE DEP-RECORD
                   SET OUTCOME-PAID TO TRUE
                   MOVE DEP-AMOUNT TO WS-PAID-AMOUNT
                   IF DEP-RD-PAID-CNT >= DEP-TENURE
                       SET OUTCOME-COMPLETE TO TRUE
                       MOVE 'LAST RD INSTALMENT PAID' TO WS-OUTCOME-TEXT
                   END-IF
               WHEN PP-INSUFFICIENT
                   SET OUTCOME-NO-FUNDS TO TRUE
               WHEN OTHER
                   SET OUTCOME-SUSPEND TO TRUE
                   MOVE 'DEBIT ACCOUNT BLOCKED' TO WS-OUTCOME-TEXT
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SCHEDULE THE NEXT RUN                                          *
      *----------------------------------------------------------------*
       6000-RECORD-OUTCOME.
           MOVE WS-RUN-DATE TO SI-LAST-RUN-DATE
           EVALUATE TRUE
               WHEN OUTCOME-COMPLETE
                   IF WS-PAID-AMOUNT > ZERO
                       ADD 1 TO SI-EXECUTED-CNT WS-PAID-CNT
                       ADD WS-PAID-AMOUNT TO WS-PAID-TOTAL
                   END-IF
                   SET SI-COMPLETED TO TRUE
                   MOVE WS-OUTCOME-TEXT TO SI-LAST-RESULT
                   ADD 1 TO WS-DONE-CNT
               WHEN OUTCOME-PAID
                   ADD 1 TO SI-EXECUTED-CNT WS-PAID-CNT
                   ADD WS-PAID-AMOUNT TO WS-PAID-TOTAL
                   MOVE 'SUCCESS' TO SI-LAST-RESULT
                   PERFORM 6100-ADVANCE-CYCLE
               WHEN OUTCOME-NO-FUNDS
                   ADD 1 TO SI-ATTEMPTS
                   PERFORM 8900-WARN
                   IF SI-ATTEMPTS < WS-MAX-ATTEMPTS
                       MOVE SPACES TO SI-LAST-RESULT
                       STRING 'NO FUNDS - RETRY ' SI-ATTEMPTS
                              DELIMITED BY SIZE INTO SI-LAST-RESULT
                       END-STRING
                       SET DU-ADD-DAYS TO TRUE
                       MOVE WS-RUN-DATE TO DU-DATE-1
                       MOVE 1 TO DU-NUMBER
                       CALL 'DATEUTIL' USING DATE-PARMS
                       MOVE DU-RESULT-DATE TO SI-NEXT-RUN-DATE
                       ADD 1 TO WS-RETRY-CNT
                   ELSE
                       ADD 1 TO SI-MISSED-CNT WS-MISSED-CNT
                       MOVE 'MISSED - FEE CHARGED' TO SI-LAST-RESULT
                       PERFORM 6200-CHARGE-FAILURE-FEE
                       PERFORM 6100-ADVANCE-CYCLE
                   END-IF
               WHEN OUTCOME-SUSPEND
                   SET SI-SUSPENDED TO TRUE
                   MOVE WS-OUTCOME-TEXT TO SI-LAST-RESULT
                   ADD 1 TO WS-SUSP-CNT
                   PERFORM 8900-WARN
           END-EVALUATE.

      *    NEXT CYCLE = SAME DAY NEXT MONTH, COUNTED FROM THE DUE DATE
      *    (NOT THE RETRY DATE) SO RETRIES NEVER SHIFT THE SCHEDULE
       6100-ADVANCE-CYCLE.
           MOVE ZERO TO SI-ATTEMPTS
           SET DU-ADD-MONTHS TO TRUE
           MOVE SI-CYCLE-DUE-DATE TO DU-DATE-1
           MOVE 1 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO SI-CYCLE-DUE-DATE SI-NEXT-RUN-DATE.

       6200-CHARGE-FAILURE-FEE.
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE SI-FROM-ACCT TO PP-ACCT-NO
           COMPUTE PP-AMOUNT = WS-FAIL-FEE + WS-FAIL-GST
           MOVE 'CH'         TO PP-TRAN-TYPE
           MOVE 'Y'          TO PP-IGNORE-MIN-BAL PP-SYSTEM-TXN
           STRING 'SI FAILURE FEE ' SI-ID
                  DELIMITED BY SIZE INTO PP-NARRATION
           END-STRING
           CALL 'ACCTPOST' USING POST-PARMS
           IF PP-OK
               MOVE GLC-FEE-INCOME  TO GP-DR-GL
               MOVE GLC-GST-PAYABLE TO GP-CR-GL
               MOVE WS-FAIL-GST     TO GP-AMOUNT
               MOVE SI-FROM-ACCT    TO GP-REF
               MOVE 'GST ON SI FAILURE FEE' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
           ELSE
               MOVE 'MISSED - FEE PENDING' TO SI-LAST-RESULT
           END-IF.

       8900-WARN.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.

       9000-SUMMARY.
           DISPLAY '------------------------------------------------'
           DISPLAY 'DUE        : ' WS-DUE-CNT
           MOVE WS-PAID-TOTAL TO WS-E-AMT
           DISPLAY 'EXECUTED   : ' WS-PAID-CNT '   ' WS-E-AMT
           DISPLAY 'RETRY LATER: ' WS-RETRY-CNT
           DISPLAY 'MISSED     : ' WS-MISSED-CNT
           DISPLAY 'COMPLETED  : ' WS-DONE-CNT
           DISPLAY 'SUSPENDED  : ' WS-SUSP-CNT
           DISPLAY 'RETURN CODE: ' WS-RETURN-CODE.
