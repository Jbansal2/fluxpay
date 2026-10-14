       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTSRET.
      *================================================================*
      * PROGRAM  : CTSRET                                              *
      * PURPOSE  : OUTWARD CTS RESULTS. FIRST APPLIES THE RETURNS SENT *
      *            BACK BY THE DRAWEE BANKS (data/CTSRESP.DAT), THEN   *
      *            TREATS EVERY CHEQUE PRESENTED ON AN EARLIER DAY AND *
      *            NOT RETURNED AS PAID ("DEEMED CLEARED" - UNDER CTS  *
      *            A DRAWEE BANK THAT MISSES THE RETURN WINDOW HAS     *
      *            ACCEPTED THE CHEQUE) AND CREDITS THE CUSTOMER.      *
      *            A RETURNED CHEQUE COSTS THE DEPOSITOR 150 + 18% GST.*
      *            RESPONSE LAYOUT: REF 9(8) 'R' REASON 9(2)           *
      *            GL: CREDIT   DR 1301 CLEARING  CR CUSTOMER          *
      *                FUNDS    DR 1302 SETTLEMENT  CR 1301 CLEARING   *
      * INPUT    : RUN DATE (BLANK = TODAY)                            *
      * FILES    : OUTCHQ (I-O), CTSRESP (INPUT, OPTIONAL)             *
      * CALLS    : ACCTPOST, GLPOST, AUDITLOG                          *
      * RETURN   : 0 = OK, 4 = RETURNS OR CREDITS THAT COULD NOT POST, *
      *            12 = FILE ERROR                                     *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OCHQ-FILE ASSIGN TO 'data/OUTCHQ.DAT'
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS OC-ID FILE STATUS IS WS-FS.
           SELECT RESP-FILE ASSIGN TO 'data/CTSRESP.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RESP-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  OCHQ-FILE.
           COPY OCHQREC.
       FD  RESP-FILE.
       01  RESP-RECORD.
           05  RS-REF                  PIC 9(08).
           05  RS-STATUS               PIC X(01).
           05  RS-REASON               PIC 9(02).

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-RESP-FS                  PIC X(02).
       01  WS-IN-RUN-DATE              PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-RETURN-FEE               PIC 9(05)V99 VALUE 150.00.
       01  WS-GST-RATE                 PIC 9V99 VALUE 0.18.
       01  WS-GST                      PIC 9(05)V99.
       01  WS-COUNTS.
           05  WS-RETURNED             PIC 9(06) VALUE ZERO.
           05  WS-CLEARED              PIC 9(06) VALUE ZERO.
           05  WS-HELD                 PIC 9(06) VALUE ZERO.
           05  WS-BAD-RESP             PIC 9(06) VALUE ZERO.
           05  WS-CLEARED-AMT          PIC 9(13)V99 VALUE ZERO.
           05  WS-RETURNED-AMT         PIC 9(13)V99 VALUE ZERO.
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-CNT                      PIC ZZZ,ZZ9.
           COPY CTSRSN.
           COPY POSTPARM.
           COPY GLPARM.
           COPY GLCODES.
           COPY AUDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'RUN DATE (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-RUN-DATE
           IF WS-IN-RUN-DATE = SPACES OR WS-IN-RUN-DATE NOT NUMERIC
               ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           ELSE
               MOVE WS-IN-RUN-DATE TO WS-RUN-DATE
           END-IF
           OPEN I-O OCHQ-FILE
           IF WS-FS = '35'
               DISPLAY 'CTSRET: NO OUTWARD CHEQUES ON FILE.'
               GOBACK
           END-IF
           IF WS-FS NOT = '00'
               DISPLAY 'CTSRET: OPEN FAILED, FS=' WS-FS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           PERFORM 1000-APPLY-RETURNS
           PERFORM 2000-DEEMED-CLEARED
           CLOSE OCHQ-FILE
           PERFORM 3000-SETTLE
           PERFORM 9000-REPORT
           IF WS-RETURNED > ZERO OR WS-HELD > ZERO
              OR WS-BAD-RESP > ZERO
               MOVE 4 TO RETURN-CODE
           END-IF
           GOBACK.

      *================================================================*
      * RETURNS FROM DRAWEE BANKS                                      *
      *================================================================*
       1000-APPLY-RETURNS.
           OPEN INPUT RESP-FILE
           IF WS-RESP-FS NOT = '00'
               DISPLAY 'CTSRET: NO RETURN FILE TODAY.'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ RESP-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END PERFORM 1100-ONE-RETURN
               END-READ
           END-PERFORM
           CLOSE RESP-FILE.

       1100-ONE-RETURN.
           IF RS-STATUS NOT = 'R'
               EXIT PARAGRAPH
           END-IF
           MOVE RS-REF TO OC-ID
           READ OCHQ-FILE
               INVALID KEY
                   DISPLAY '  RETURN FOR UNKNOWN REF ' RS-REF
                   ADD 1 TO WS-BAD-RESP
                   EXIT PARAGRAPH
           END-READ
           IF NOT OC-SENT
               DISPLAY '  RETURN FOR REF ' RS-REF
                       ' WHICH IS NOT IN CLEARING - IGNORED'
               ADD 1 TO WS-BAD-RESP
               EXIT PARAGRAPH
           END-IF
           SET OC-RETURNED TO TRUE
           MOVE WS-RUN-DATE TO OC-RESULT-DATE
           MOVE RS-REASON   TO OC-RETURN-REASON
           MOVE 'OTHER REASON' TO OC-RETURN-TEXT
           SET CRS-IDX TO 1
           SEARCH CTS-REASON
               WHEN CRS-CODE(CRS-IDX) = RS-REASON
                   MOVE CRS-TEXT(CRS-IDX) TO OC-RETURN-TEXT
           END-SEARCH
           REWRITE OCHQ-RECORD
           ADD 1 TO WS-RETURNED
           ADD OC-AMOUNT TO WS-RETURNED-AMT
           MOVE OC-AMOUNT TO ED-AMT
           DISPLAY '  RETURNED REF ' OC-ID ' ' ED-AMT ' - '
                   OC-RETURN-TEXT
           PERFORM 1200-RETURN-FEE.

       1200-RETURN-FEE.
           COMPUTE WS-GST ROUNDED = WS-RETURN-FEE * WS-GST-RATE
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE OC-ACCT-NO TO PP-ACCT-NO
           COMPUTE PP-AMOUNT = WS-RETURN-FEE + WS-GST
           MOVE 'CH'       TO PP-TRAN-TYPE
           MOVE 'CHEQUE RETURN CHARGES' TO PP-NARRATION
           MOVE 'Y'        TO PP-IGNORE-MIN-BAL
           MOVE 'Y'        TO PP-SYSTEM-TXN
           CALL 'ACCTPOST' USING POST-PARMS
           IF PP-OK
               MOVE GLC-FEE-INCOME  TO GP-DR-GL
               MOVE GLC-GST-PAYABLE TO GP-CR-GL
               MOVE WS-GST          TO GP-AMOUNT
               MOVE SPACES          TO GP-REF
               STRING 'OCHQ' OC-ID DELIMITED BY SIZE INTO GP-REF
               MOVE 'GST ON CHEQUE RETURN FEE' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
           ELSE
               DISPLAY '  RETURN FEE NOT RECOVERED FROM ' OC-ACCT-NO
           END-IF.

      *================================================================*
      * NOT RETURNED BY THE END OF THE RETURN WINDOW = PAID            *
      *================================================================*
       2000-DEEMED-CLEARED.
           MOVE 'N' TO WS-EOF-SW
           MOVE ZERO TO OC-ID
           START OCHQ-FILE KEY IS NOT LESS THAN OC-ID
               INVALID KEY SET END-OF-FILE TO TRUE
           END-START
           PERFORM UNTIL END-OF-FILE
               READ OCHQ-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF OC-SENT AND OC-BATCH-DATE < WS-RUN-DATE
                           PERFORM 2100-CREDIT-CUSTOMER
                       END-IF
               END-READ
           END-PERFORM.

       2100-CREDIT-CUSTOMER.
           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE OC-ACCT-NO TO PP-ACCT-NO
           MOVE OC-AMOUNT  TO PP-AMOUNT
           MOVE 'OC'       TO PP-TRAN-TYPE
           MOVE SPACES     TO PP-NARRATION
           STRING 'CHQ ' OC-CHEQUE-NO ' ' OC-DRAWER-NAME
               DELIMITED BY SIZE INTO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
      *        E.G. ACCOUNT FROZEN SINCE LODGEMENT - LEAVE IN CLEARING
      *        SO OPERATIONS CAN DECIDE; IT IS PICKED UP NEXT RUN
               ADD 1 TO WS-HELD
               DISPLAY '  CREDIT HELD FOR REF ' OC-ID ' - ACCOUNT '
                       OC-ACCT-NO ' RC ' PP-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           SET OC-CLEARED TO TRUE
           MOVE WS-RUN-DATE TO OC-RESULT-DATE
           REWRITE OCHQ-RECORD
           ADD 1 TO WS-CLEARED
           ADD OC-AMOUNT TO WS-CLEARED-AMT.

      *    THE CLEARING HOUSE PAYS US FOR WHAT CLEARED
       3000-SETTLE.
           IF WS-CLEARED-AMT > ZERO
               MOVE GLC-PAYMENT-SETTLE TO GP-DR-GL
               MOVE GLC-CLEARING       TO GP-CR-GL
               MOVE WS-CLEARED-AMT     TO GP-AMOUNT
               MOVE SPACES TO GP-REF
               STRING 'CTS' WS-RUN-DATE DELIMITED BY SIZE INTO GP-REF
               MOVE 'CTS OUTWARD SETTLEMENT' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
               IF NOT GP-OK
                   DISPLAY 'CTSRET: SETTLEMENT GL FAILED, RC=' GP-RC
                   MOVE 12 TO RETURN-CODE
               END-IF
           END-IF.

       9000-REPORT.
           DISPLAY ' '
           DISPLAY 'OUTWARD CLEARING RESULTS FOR ' WS-RUN-DATE
           MOVE WS-CLEARED TO ED-CNT
           MOVE WS-CLEARED-AMT TO ED-AMT
           DISPLAY 'CLEARED AND CREDITED : ' ED-CNT ' ' ED-AMT
           MOVE WS-RETURNED TO ED-CNT
           MOVE WS-RETURNED-AMT TO ED-AMT
           DISPLAY 'RETURNED             : ' ED-CNT ' ' ED-AMT
           MOVE WS-HELD TO ED-CNT
           DISPLAY 'CREDITS HELD         : ' ED-CNT
           MOVE WS-BAD-RESP TO ED-CNT
           DISPLAY 'BAD RETURN RECORDS   : ' ED-CNT.
