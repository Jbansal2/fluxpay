       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYBATCH.
      *================================================================*
      * PROGRAM  : PAYBATCH                                            *
      * PURPOSE  : PAYMENT SETTLEMENT BATCH, THREE STEPS:              *
      *              1 - OUTWARD RETURNS: data/PAYRET.DAT LISTS OUR    *
      *                  NEFT/RTGS PAYMENTS THAT THE OTHER BANK SENT   *
      *                  BACK. THE CUSTOMER IS REFUNDED (CHARGES ARE   *
      *                  NOT) AND THE PAYMENT IS MARKED RETURNED.      *
      *                  LAYOUT: REF(16) REASON(30)                    *
      *              2 - SETTLEMENT: EVERY OUTWARD PAYMENT STILL       *
      *                  PENDING AND CREATED ON OR BEFORE THE RUN DATE *
      *                  IS MARKED SETTLED WITH A UTR.                 *
      *              3 - INWARD CREDITS: data/PAYIN.DAT LISTS MONEY    *
      *                  SENT TO OUR CUSTOMERS. EACH IS CREDITED, OR   *
      *                  RETURNED (data/PAYINRET_<DATE>.DAT) WHEN THE  *
      *                  ACCOUNT IS UNKNOWN OR NOT ACTIVE. A UTR SEEN  *
      *                  BEFORE IS REJECTED AS A DUPLICATE.            *
      *                  LAYOUT: UTR(22) MODE(1) ACCOUNT(10)           *
      *                          AMOUNT(9V99, 11 DIGITS) REMITTER(35)  *
      * GL       : REFUNDS AND INWARD CREDITS: DR 1302 CR CUSTOMER     *
      * RETURN   : 0 = CLEAN, 4 = SOME RETURNS OR REJECTS, 12 = ERROR  *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAY-FILE ASSIGN TO 'data/PAYREG.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PAY-REF
               FILE STATUS IS WS-PAY-FS.
           SELECT RET-FILE ASSIGN TO 'data/PAYRET.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RET-FS.
           SELECT IN-FILE ASSIGN TO 'data/PAYIN.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-FS.
           SELECT INRET-FILE ASSIGN TO WS-INRET-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-INRET-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  PAY-FILE.
           COPY PAYREC.
       FD  RET-FILE.
       01  RET-RECORD.
           05  RET-REF                 PIC X(16).
           05  RET-REASON              PIC X(30).
       FD  IN-FILE.
       01  IN-RECORD.
           05  IN-UTR                  PIC X(22).
           05  IN-MODE                 PIC X(01).
           05  IN-ACCT-NO              PIC X(10).
           05  IN-AMOUNT               PIC X(11).
           05  IN-AMOUNT-N REDEFINES IN-AMOUNT PIC 9(09)V99.
           05  IN-REMITTER             PIC X(35).
       FD  INRET-FILE.
       01  INRET-LINE                  PIC X(100).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY DATEPARM.

       01  WS-PAY-FS                   PIC X(02).
           88  PAY-FS-OK                         VALUE '00'.
           88  PAY-FS-NOFILE                     VALUE '35'.
       01  WS-RET-FS                   PIC X(02).
       01  WS-IN-FS                    PIC X(02).
       01  WS-INRET-FS                 PIC X(02).
       01  WS-INRET-FILENAME           PIC X(40).
       01  WS-INRET-OPEN-SW            PIC X(01) VALUE 'N'.
           88  INRET-IS-OPEN                     VALUE 'Y'.

       01  WS-IN-RUN-DATE              PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-RETURN-CODE              PIC 9(02) VALUE ZERO.
       01  WS-REASON                   PIC X(30).
       01  WS-DUP-SW                   PIC X(01).
           88  DUPLICATE-UTR                     VALUE 'Y'.
       01  WS-NEXT-SEQ                 PIC 9(07).
       01  WS-SAVE-REF                 PIC X(16).

       01  WS-STATS.
           05  WS-RET-READ             PIC 9(05).
           05  WS-RET-DONE             PIC 9(05).
           05  WS-RET-REJECT           PIC 9(05).
           05  WS-SETTLED              PIC 9(05).
           05  WS-IN-READ              PIC 9(05).
           05  WS-IN-CREDITED          PIC 9(05).
           05  WS-IN-RETURNED          PIC 9(05).
           05  WS-IN-AMT               PIC 9(13)V99.
           05  WS-SETTLED-AMT          PIC 9(13)V99.

       01  WS-E-AMT                    PIC ZZ,ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           INITIALIZE WS-STATS
           MOVE ZERO TO WS-RETURN-CODE
           MOVE 'N' TO WS-INRET-OPEN-SW
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           DISPLAY 'SETTLEMENT DATE YYYYMMDD (BLANK = TODAY): '
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
           STRING 'data/PAYINRET_' WS-RUN-DATE '.DAT'
                  DELIMITED BY SIZE INTO WS-INRET-FILENAME
           END-STRING

           OPEN I-O PAY-FILE
           IF PAY-FS-NOFILE
               OPEN OUTPUT PAY-FILE
               CLOSE PAY-FILE
               OPEN I-O PAY-FILE
           END-IF
           IF NOT PAY-FS-OK
               DISPLAY 'PAYBATCH: REGISTER OPEN FAILED FS=' WS-PAY-FS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           DISPLAY 'PAYMENT SETTLEMENT FOR ' WS-RUN-DATE
           PERFORM 1000-OUTWARD-RETURNS
           PERFORM 2000-SETTLE-PENDING
           PERFORM 3000-INWARD-CREDITS
           CLOSE PAY-FILE
           IF INRET-IS-OPEN
               CLOSE INRET-FILE
           END-IF
           PERFORM 9000-SUMMARY
           MOVE WS-RETURN-CODE TO RETURN-CODE
           GOBACK.

      *================================================================*
      * STEP 1 - OUTWARD RETURNS                                       *
      *================================================================*
       1000-OUTWARD-RETURNS.
           DISPLAY 'STEP 1 - OUTWARD RETURNS'
           OPEN INPUT RET-FILE
           IF WS-RET-FS NOT = '00'
               DISPLAY '  NO RETURN FILE.'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ RET-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END PERFORM 1100-RETURN-ONE
               END-READ
           END-PERFORM
           CLOSE RET-FILE.

       1100-RETURN-ONE.
           ADD 1 TO WS-RET-READ
           MOVE FUNCTION UPPER-CASE(RET-REF) TO PAY-REF
           READ PAY-FILE
               INVALID KEY
                   DISPLAY '  ' RET-REF ' UNKNOWN REFERENCE - IGNORED'
                   ADD 1 TO WS-RET-REJECT
                   EXIT PARAGRAPH
           END-READ
           IF NOT PAY-OUTWARD OR PAY-RETURNED OR PAY-INSTANT
               DISPLAY '  ' RET-REF ' CANNOT BE RETURNED (STATUS '
                       PAY-STATUS ') - IGNORED'
               ADD 1 TO WS-RET-REJECT
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE PAY-OUR-ACCT TO PP-ACCT-NO
           MOVE PAY-AMOUNT   TO PP-AMOUNT
           MOVE 'PI'         TO PP-TRAN-TYPE
           MOVE 'Y'          TO PP-SYSTEM-TXN
           STRING 'RETURNED ' PAY-REF DELIMITED BY SIZE
                  INTO PP-NARRATION
           END-STRING
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY '  ' PAY-REF ' REFUND FAILED RC=' PP-RETURN-CODE
                       ' - REFER TO OPERATIONS'
               MOVE 12 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           SET PAY-RETURNED TO TRUE
           MOVE RET-REASON TO PAY-RETURN-REASON
           MOVE WS-RUN-DATE TO PAY-SETTLED-DATE
           REWRITE PAY-RECORD
           ADD 1 TO WS-RET-DONE
           PERFORM 8900-WARN
           MOVE PAY-AMOUNT TO WS-E-AMT
           DISPLAY '  ' PAY-REF ' RETURNED ' WS-E-AMT ' - '
                   FUNCTION TRIM(RET-REASON).

      *================================================================*
      * STEP 2 - SETTLE PENDING OUTWARD                                *
      *================================================================*
       2000-SETTLE-PENDING.
           DISPLAY 'STEP 2 - SETTLE PENDING OUTWARD PAYMENTS'
           MOVE LOW-VALUES TO PAY-REF
           START PAY-FILE KEY IS NOT LESS THAN PAY-REF
               INVALID KEY EXIT PARAGRAPH
           END-START
           PERFORM FOREVER
               READ PAY-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF PAY-OUTWARD AND PAY-PENDING
                  AND PAY-CREATED-DATE <= WS-RUN-DATE
                   SET PAY-SETTLED TO TRUE
                   MOVE WS-RUN-DATE TO PAY-SETTLED-DATE
                   MOVE SPACES TO PAY-UTR
                   STRING 'CB' PAY-REF DELIMITED BY SIZE INTO PAY-UTR
                   END-STRING
                   REWRITE PAY-RECORD
                   ADD 1 TO WS-SETTLED
                   ADD PAY-AMOUNT TO WS-SETTLED-AMT
               END-IF
           END-PERFORM.

      *================================================================*
      * STEP 3 - INWARD CREDITS                                        *
      *================================================================*
       3000-INWARD-CREDITS.
           DISPLAY 'STEP 3 - INWARD CREDITS'
           OPEN INPUT IN-FILE
           IF WS-IN-FS NOT = '00'
               DISPLAY '  NO INWARD FILE.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 3050-NEXT-INWARD-SEQ
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ IN-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END PERFORM 3100-CREDIT-ONE
               END-READ
           END-PERFORM
           CLOSE IN-FILE.

      *    INWARD REFERENCES: 'X' + RUN DATE + SEQUENCE
       3050-NEXT-INWARD-SEQ.
           MOVE 1 TO WS-NEXT-SEQ
           MOVE SPACES TO PAY-REF
           STRING 'X' WS-RUN-DATE '9999999' DELIMITED BY SIZE
                  INTO PAY-REF
           END-STRING
           START PAY-FILE KEY IS LESS THAN OR EQUAL PAY-REF
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ PAY-FILE PREVIOUS RECORD
                       AT END CONTINUE
                       NOT AT END
                           IF PAY-REF(1:1) = 'X'
                              AND PAY-REF(2:8) = WS-RUN-DATE
                               COMPUTE WS-NEXT-SEQ =
                                   FUNCTION NUMVAL(PAY-REF(10:7)) + 1
                           END-IF
                   END-READ
           END-START.

       3100-CREDIT-ONE.
           ADD 1 TO WS-IN-READ
           MOVE SPACES TO WS-REASON
           EVALUATE TRUE
               WHEN IN-ACCT-NO NOT NUMERIC
                 OR IN-AMOUNT NOT NUMERIC
                 OR IN-UTR = SPACES
                   MOVE 'INVALID RECORD' TO WS-REASON
               WHEN IN-AMOUNT-N = ZERO
                   MOVE 'ZERO AMOUNT' TO WS-REASON
           END-EVALUATE
           IF WS-REASON = SPACES
               PERFORM 3200-CHECK-DUPLICATE
               IF DUPLICATE-UTR
                   MOVE 'DUPLICATE UTR' TO WS-REASON
               END-IF
           END-IF
           IF WS-REASON = SPACES
               INITIALIZE POST-PARMS
               SET PP-CREDIT TO TRUE
               MOVE IN-ACCT-NO  TO PP-ACCT-NO
               MOVE IN-AMOUNT-N TO PP-AMOUNT
               MOVE 'PI'        TO PP-TRAN-TYPE
               STRING 'FROM ' IN-REMITTER DELIMITED BY SIZE
                      INTO PP-NARRATION
               END-STRING
               CALL 'ACCTPOST' USING POST-PARMS
               EVALUATE TRUE
                   WHEN PP-OK         CONTINUE
                   WHEN PP-NOT-FOUND  MOVE 'NO SUCH ACCOUNT'
                                      TO WS-REASON
                   WHEN PP-NOT-ACTIVE MOVE 'ACCOUNT CLOSED OR FROZEN'
                                      TO WS-REASON
                   WHEN OTHER         MOVE 'CREDIT FAILED'
                                      TO WS-REASON
               END-EVALUATE
           END-IF
           PERFORM 3300-REGISTER-INWARD
           IF WS-REASON = SPACES
               ADD 1 TO WS-IN-CREDITED
               ADD IN-AMOUNT-N TO WS-IN-AMT
           ELSE
               ADD 1 TO WS-IN-RETURNED
               PERFORM 3400-WRITE-INWARD-RETURN
               PERFORM 8900-WARN
               DISPLAY '  ' IN-UTR ' RETURNED - ' WS-REASON
           END-IF.

      *    FULL SCAN OF INWARD ENTRIES - VOLUMES ARE SMALL HERE; A REAL
      *    SYSTEM WOULD KEEP AN ALTERNATE INDEX ON THE UTR
       3200-CHECK-DUPLICATE.
           MOVE 'N' TO WS-DUP-SW
           MOVE PAY-REF TO WS-SAVE-REF
           MOVE 'X' TO PAY-REF
           START PAY-FILE KEY IS NOT LESS THAN PAY-REF
               INVALID KEY EXIT PARAGRAPH
           END-START
           PERFORM FOREVER
               READ PAY-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF PAY-REF(1:1) NOT = 'X'
                   EXIT PERFORM
               END-IF
               IF PAY-UTR = IN-UTR AND NOT PAY-RETURNED
                   MOVE 'Y' TO WS-DUP-SW
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       3300-REGISTER-INWARD.
           INITIALIZE PAY-RECORD
           MOVE SPACES TO PAY-REF
           STRING 'X' WS-RUN-DATE WS-NEXT-SEQ DELIMITED BY SIZE
                  INTO PAY-REF
           END-STRING
           ADD 1 TO WS-NEXT-SEQ
           MOVE IN-MODE     TO PAY-MODE
           SET PAY-INWARD   TO TRUE
           IF IN-ACCT-NO IS NUMERIC
               MOVE IN-ACCT-NO TO PAY-OUR-ACCT
           END-IF
           MOVE IN-REMITTER TO PAY-OTHER-NAME
           IF IN-AMOUNT IS NUMERIC
               MOVE IN-AMOUNT-N TO PAY-AMOUNT
           END-IF
           MOVE IN-UTR      TO PAY-UTR
           MOVE WS-RUN-DATE TO PAY-CREATED-DATE PAY-SETTLED-DATE
           MOVE WS-CURR-TIME TO PAY-CREATED-TIME
           IF WS-REASON = SPACES
               SET PAY-SETTLED TO TRUE
           ELSE
               SET PAY-RETURNED TO TRUE
               MOVE WS-REASON TO PAY-RETURN-REASON
           END-IF
           WRITE PAY-RECORD
               INVALID KEY
                   DISPLAY 'PAYBATCH: REGISTER WRITE FAILED ' PAY-REF
                   MOVE 12 TO WS-RETURN-CODE
           END-WRITE.

       3400-WRITE-INWARD-RETURN.
           IF NOT INRET-IS-OPEN
               OPEN OUTPUT INRET-FILE
               MOVE 'Y' TO WS-INRET-OPEN-SW
           END-IF
           MOVE SPACES TO INRET-LINE
           STRING IN-UTR '|' IN-ACCT-NO '|' IN-AMOUNT '|' WS-REASON
                  DELIMITED BY SIZE INTO INRET-LINE
           END-STRING
           WRITE INRET-LINE.

       8900-WARN.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.

       9000-SUMMARY.
           DISPLAY '------------------------------------------------'
           DISPLAY 'OUTWARD RETURNS PROCESSED : ' WS-RET-DONE
                   '  (IGNORED ' WS-RET-REJECT ')'
           MOVE WS-SETTLED-AMT TO WS-E-AMT
           DISPLAY 'OUTWARD SETTLED           : ' WS-SETTLED
                   '  ' WS-E-AMT
           MOVE WS-IN-AMT TO WS-E-AMT
           DISPLAY 'INWARD CREDITED           : ' WS-IN-CREDITED
                   '  ' WS-E-AMT
           DISPLAY 'INWARD RETURNED           : ' WS-IN-RETURNED
           DISPLAY 'RETURN CODE               : ' WS-RETURN-CODE.
