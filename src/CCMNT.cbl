       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCMNT.
      *================================================================*
      * PROGRAM  : CCMNT                                               *
      * PURPOSE  : CREDIT CARDS - ONLINE                               *
      *              1 ISSUE (MANAGER)    5 RECENT TRANSACTIONS        *
      *              2 AUTHORISE PURCHASE 6 BLOCK / UNBLOCK            *
      *                / CASH ADVANCE     7 MERCHANT REFUND            *
      *              3 PAYMENT            8 CHANGE LIMIT (MANAGER)     *
      *              4 CARD INQUIRY                                    *
      *            RULES: KYC-VERIFIED CUSTOMER, ONE CARD EACH, LIMIT  *
      *            10,000 - 5,00,000, CASH LIMIT 30% OF LIMIT, CASH    *
      *            ADVANCE FEE 2.5% (MIN 300) + 18% GST, STATEMENT ON  *
      *            THE 1ST OF EACH MONTH (CCBATCH).                    *
      * FILES    : CCMAST (INDEXED, I-O), CUSTMAST (INPUT), CCTXN      *
      * CALLS    : CCPOST, ACCTPOST, CARDUTIL, DATEUTIL, AUDITLOG      *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CC-FILE ASSIGN TO 'data/CCMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CC-NO
               FILE STATUS IS WS-CC-FS.
           SELECT CUST-FILE ASSIGN TO 'data/CUSTMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-FS.
           SELECT CCT-FILE ASSIGN TO 'data/CCTXN.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CCT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CC-FILE.
           COPY CCREC.
       FD  CUST-FILE.
           COPY CUSTREC.
       FD  CCT-FILE.
           COPY CCTREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY CCPPARM.
           COPY POSTPARM.
           COPY CARDPARM.
           COPY DATEPARM.
           COPY SESSION.
           COPY AUDPARM.

       01  WS-CC-FS                    PIC X(02).
           88  CC-FS-OK                          VALUE '00'.
           88  CC-FS-NOFILE                      VALUE '35'.
       01  WS-CUST-FS                  PIC X(02).
           88  CUST-FS-OK                        VALUE '00'.
       01  WS-CCT-FS                   PIC X(02).

       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
           88  CHOICE-BACK                       VALUE '9'.
       01  WS-CONFIRM                  PIC X(01).
           88  CONFIRM-YES                       VALUES 'Y' 'y'.

       01  WS-IN.
           05  WS-IN-CARD-NO           PIC 9(16).
           05  WS-IN-CUST-ID           PIC 9(08).
           05  WS-IN-ACCT-NO           PIC 9(10).
           05  WS-IN-LIMIT             PIC 9(09)V99.
           05  WS-IN-AMOUNT            PIC 9(09)V99.
           05  WS-IN-TYPE              PIC X(01).
           05  WS-IN-MERCHANT          PIC X(25).
           05  WS-IN-AUTOPAY           PIC X(01).
           05  WS-IN-SOURCE            PIC X(01).

       01  WS-NEW-CARD                 PIC 9(16).
       01  WS-NEW-CARD-R REDEFINES WS-NEW-CARD.
           05  WS-NC-BIN               PIC 9(04).
           05  WS-NC-CUST              PIC 9(08).
           05  WS-NC-SEQ               PIC 9(03).
           05  WS-NC-CHECK             PIC 9(01).
       01  WS-CC-BIN                   PIC 9(04) VALUE 9991.
       01  WS-FOUND-SW                 PIC X(01).
           88  CUSTOMER-HAS-CARD                 VALUE 'Y'.

       01  WS-MIN-LIMIT                PIC 9(09)V99 VALUE 10000.00.
       01  WS-MAX-LIMIT                PIC 9(09)V99 VALUE 500000.00.
       01  WS-CASH-PCT                 PIC V99 VALUE 0.30.
       01  WS-CASH-FEE-PCT             PIC V999 VALUE 0.025.
       01  WS-CASH-FEE-MIN             PIC 9(05)V99 VALUE 300.00.
       01  WS-GST-RATE                 PIC V99 VALUE 0.18.
       01  WS-FEE                      PIC 9(07)V99.
       01  WS-GST                      PIC 9(07)V99.

      * RECENT TRANSACTIONS RING
       01  WS-RING.
           05  WS-RING-LINE            PIC X(78) OCCURS 10 TIMES.
       01  WS-RING-NEXT                PIC 9(02).
       01  WS-RING-CNT                 PIC 9(05).
       01  WS-I                        PIC 9(02).
       01  WS-J                        PIC 9(02).

       01  WS-E-AMT                    PIC ZZ,ZZZ,ZZ9.99.
       01  WS-E-SAMT                   PIC -ZZ,ZZZ,ZZ9.99.
       01  WS-MASKED                   PIC X(19).

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN I-O CC-FILE
           IF CC-FS-NOFILE
               OPEN OUTPUT CC-FILE
               CLOSE CC-FILE
               OPEN I-O CC-FILE
           END-IF
           IF NOT CC-FS-OK
               DISPLAY 'CREDIT CARD MASTER OPEN FAILED, FS=' WS-CC-FS
               GOBACK
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL CHOICE-BACK
               PERFORM 1000-SHOW-MENU
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-ISSUE
                   WHEN '2' PERFORM 3000-AUTHORISE
                   WHEN '3' PERFORM 4000-PAYMENT
                   WHEN '4' PERFORM 5000-INQUIRY
                   WHEN '5' PERFORM 5500-RECENT
                   WHEN '6' PERFORM 6000-BLOCK-TOGGLE
                   WHEN '7' PERFORM 6500-REFUND
                   WHEN '8' PERFORM 7000-CHANGE-LIMIT
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE CC-FILE
           GOBACK.

       1000-SHOW-MENU.
           DISPLAY ' '
           DISPLAY '------------- CREDIT CARDS -------------'
           IF SES-CAN-APPROVE
               DISPLAY ' 1. ISSUE CREDIT CARD (MANAGER)'
           END-IF
           DISPLAY ' 2. AUTHORISE PURCHASE / CASH ADVANCE'
           DISPLAY ' 3. CARD PAYMENT'
           DISPLAY ' 4. CARD INQUIRY'
           DISPLAY ' 5. RECENT TRANSACTIONS'
           DISPLAY ' 6. BLOCK / UNBLOCK'
           DISPLAY ' 7. MERCHANT REFUND'
           IF SES-CAN-APPROVE
               DISPLAY ' 8. CHANGE LIMIT (MANAGER)'
           END-IF
           DISPLAY ' 9. BACK TO MAIN MENU'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT.

      *================================================================*
      * ISSUE                                                          *
      *================================================================*
       2000-ISSUE.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'CARD ISSUE NEEDS A MANAGER OR ADMIN.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'CUSTOMER ID          : ' WITH NO ADVANCING
           ACCEPT WS-IN-CUST-ID
           DISPLAY 'PAYMENT ACCOUNT NO   : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'CREDIT LIMIT         : ' WITH NO ADVANCING
           ACCEPT WS-IN-LIMIT
           DISPLAY 'AUTOPAY (N/M=MIN/F=FULL): ' WITH NO ADVANCING
           ACCEPT WS-IN-AUTOPAY
           MOVE FUNCTION UPPER-CASE(WS-IN-AUTOPAY) TO WS-IN-AUTOPAY
           IF WS-IN-AUTOPAY NOT = 'M' AND WS-IN-AUTOPAY NOT = 'F'
               MOVE 'N' TO WS-IN-AUTOPAY
           END-IF

           IF WS-IN-LIMIT < WS-MIN-LIMIT OR WS-IN-LIMIT > WS-MAX-LIMIT
               DISPLAY 'LIMIT MUST BE 10,000 TO 5,00,000.'
               EXIT PARAGRAPH
           END-IF
           OPEN INPUT CUST-FILE
           IF NOT CUST-FS-OK
               DISPLAY 'NO CUSTOMER FILE.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-CUST-ID TO CUST-ID
           READ CUST-FILE
               INVALID KEY
                   DISPLAY 'CUSTOMER NOT FOUND.'
                   CLOSE CUST-FILE
                   EXIT PARAGRAPH
           END-READ
           CLOSE CUST-FILE
           IF NOT KYC-VERIFIED
               DISPLAY 'CUSTOMER KYC IS NOT VERIFIED.'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'PAYMENT ACCOUNT NOT FOUND.'
               EXIT PARAGRAPH
           END-IF

      *    ONE CARD PER CUSTOMER: KEY RANGE BIN + CUSTOMER
           MOVE WS-CC-BIN     TO WS-NC-BIN
           MOVE WS-IN-CUST-ID TO WS-NC-CUST
           MOVE ZERO          TO WS-NC-SEQ WS-NC-CHECK
           MOVE WS-NEW-CARD   TO CC-NO
           MOVE 'N' TO WS-FOUND-SW
           START CC-FILE KEY IS NOT LESS THAN CC-NO
               NOT INVALID KEY
                   READ CC-FILE NEXT RECORD
                       NOT AT END
                           IF CC-CUST-ID = WS-IN-CUST-ID
                              AND NOT CC-CLOSED
                               MOVE 'Y' TO WS-FOUND-SW
                           END-IF
                   END-READ
           END-START
           IF CUSTOMER-HAS-CARD
               DISPLAY 'CUSTOMER ALREADY HOLDS A CREDIT CARD.'
               EXIT PARAGRAPH
           END-IF

           MOVE 'LU'        TO CU-FUNCTION
           MOVE WS-NEW-CARD TO CU-CARD-NO
           CALL 'CARDUTIL' USING CU-PARMS
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           INITIALIZE CC-RECORD
           MOVE CU-CARD-NO      TO CC-NO
           MOVE WS-IN-CUST-ID   TO CC-CUST-ID
           MOVE CUST-NAME       TO CC-NAME
           MOVE WS-IN-ACCT-NO   TO CC-PAY-ACCT
           SET CC-ACTIVE        TO TRUE
           COMPUTE CC-EXPIRY-YYYYMM = WS-CURR-YYYYMM + 500
           MOVE WS-IN-LIMIT     TO CC-LIMIT
           COMPUTE CC-CASH-LIMIT = WS-IN-LIMIT * WS-CASH-PCT
           MOVE WS-IN-AUTOPAY   TO CC-AUTOPAY
           MOVE 'NN'            TO CC-CYCLE-FLAGS
           MOVE WS-CURR-DATE    TO CC-ISSUE-DATE CC-LAST-STMT-DATE
      *    FIRST STATEMENT: THE 1ST OF NEXT MONTH
           MOVE WS-CURR-DATE    TO DU-DATE-1
           MOVE '01'            TO DU-DATE-1(7:2)
           SET DU-ADD-MONTHS    TO TRUE
           MOVE 1               TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE  TO CC-NEXT-STMT-DATE
           WRITE CC-RECORD
               INVALID KEY
                   DISPLAY 'CARD WRITE FAILED, FS=' WS-CC-FS
                   EXIT PARAGRAPH
           END-WRITE
           PERFORM 8100-MASK
           DISPLAY 'CREDIT CARD ISSUED: ' WS-MASKED
           DISPLAY 'FULL NUMBER        : ' CC-NO
           MOVE CC-LIMIT TO WS-E-AMT
           DISPLAY 'LIMIT              : ' WS-E-AMT
           MOVE CC-CASH-LIMIT TO WS-E-AMT
           DISPLAY 'CASH LIMIT         : ' WS-E-AMT
           DISPLAY 'FIRST STATEMENT ON : ' CC-NEXT-STMT-DATE
           MOVE 'CC-ISSUE' TO AU-ACTION
           PERFORM 9000-AUDIT.

      *================================================================*
      * AUTHORISE PURCHASE OR CASH ADVANCE                             *
      *================================================================*
       3000-AUTHORISE.
           PERFORM 8000-READ-CARD
           IF NOT CC-FS-OK
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'TYPE (P=PURCHASE C=CASH): ' WITH NO ADVANCING
           ACCEPT WS-IN-TYPE
           MOVE FUNCTION UPPER-CASE(WS-IN-TYPE) TO WS-IN-TYPE
           DISPLAY 'AMOUNT                  : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           DISPLAY 'MERCHANT / ATM          : ' WITH NO ADVANCING
           ACCEPT WS-IN-MERCHANT
           IF WS-IN-TYPE NOT = 'P' AND WS-IN-TYPE NOT = 'C'
               DISPLAY 'TYPE MUST BE P OR C.'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE CCP-PARMS
           MOVE WS-IN-TYPE     TO CCP-TYPE
           MOVE WS-IN-AMOUNT   TO CCP-AMOUNT
           MOVE FUNCTION UPPER-CASE(WS-IN-MERCHANT) TO CCP-MERCHANT
           CALL 'CCPOST' USING CCP-PARMS CC-RECORD
           EVALUATE TRUE
               WHEN CCP-OK
                   CONTINUE
               WHEN CCP-CARD-BLOCKED
                   DISPLAY 'DECLINED - CARD BLOCKED.'
                   EXIT PARAGRAPH
               WHEN CCP-CARD-EXPIRED
                   DISPLAY 'DECLINED - CARD EXPIRED.'
                   EXIT PARAGRAPH
               WHEN CCP-OVER-LIMIT
                   MOVE CCP-AVAILABLE TO WS-E-SAMT
                   DISPLAY 'DECLINED - OVER LIMIT. AVAILABLE: '
                           WS-E-SAMT
                   EXIT PARAGRAPH
               WHEN CCP-OVER-CASH-LIMIT
                   DISPLAY 'DECLINED - OVER CASH LIMIT.'
                   EXIT PARAGRAPH
               WHEN OTHER
                   DISPLAY 'DECLINED, RC=' CCP-RC
                   EXIT PARAGRAPH
           END-EVALUATE
           DISPLAY 'APPROVED. AUTH CODE ' CCP-AUTH-CODE
           IF WS-IN-TYPE = 'C'
               PERFORM 3100-CASH-ADVANCE-FEE
           END-IF
           REWRITE CC-RECORD
           MOVE CCP-AVAILABLE TO WS-E-SAMT
           DISPLAY 'AVAILABLE LIMIT NOW: ' WS-E-SAMT.

       3100-CASH-ADVANCE-FEE.
           COMPUTE WS-FEE ROUNDED = WS-IN-AMOUNT * WS-CASH-FEE-PCT
           IF WS-FEE < WS-CASH-FEE-MIN
               MOVE WS-CASH-FEE-MIN TO WS-FEE
           END-IF
           COMPUTE WS-GST ROUNDED = WS-FEE * WS-GST-RATE
           INITIALIZE CCP-PARMS
           MOVE 'F'    TO CCP-TYPE
           MOVE WS-FEE TO CCP-AMOUNT
           MOVE 'CASH ADVANCE FEE' TO CCP-MERCHANT
           CALL 'CCPOST' USING CCP-PARMS CC-RECORD
           INITIALIZE CCP-PARMS
           MOVE 'G'    TO CCP-TYPE
           MOVE WS-GST TO CCP-AMOUNT
           MOVE 'GST ON CASH ADVANCE FEE' TO CCP-MERCHANT
           CALL 'CCPOST' USING CCP-PARMS CC-RECORD
           MOVE WS-FEE TO WS-E-AMT
           DISPLAY 'CASH ADVANCE FEE   : ' WS-E-AMT ' + GST'.

      *================================================================*
      * PAYMENT                                                        *
      *================================================================*
       4000-PAYMENT.
           PERFORM 8000-READ-CARD
           IF NOT CC-FS-OK
               EXIT PARAGRAPH
           END-IF
           MOVE CC-OUTSTANDING TO WS-E-SAMT
           DISPLAY 'OUTSTANDING: ' WS-E-SAMT
           DISPLAY 'AMOUNT                        : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           DISPLAY 'FROM A=ACCOUNT ' CC-PAY-ACCT ' / C=CASH: '
                   WITH NO ADVANCING
           ACCEPT WS-IN-SOURCE
           MOVE FUNCTION UPPER-CASE(WS-IN-SOURCE) TO WS-IN-SOURCE
           IF WS-IN-AMOUNT NOT > ZERO
               DISPLAY 'AMOUNT MUST BE GREATER THAN ZERO.'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE CCP-PARMS
           EVALUATE WS-IN-SOURCE
               WHEN 'A'
                   INITIALIZE POST-PARMS
                   SET PP-DEBIT TO TRUE
                   MOVE CC-PAY-ACCT  TO PP-ACCT-NO
                   MOVE WS-IN-AMOUNT TO PP-AMOUNT
                   MOVE 'CP'         TO PP-TRAN-TYPE
                   STRING 'CREDIT CARD ' CC-NO(13:4)
                          DELIMITED BY SIZE INTO PP-NARRATION
                   END-STRING
                   CALL 'ACCTPOST' USING POST-PARMS
                   IF NOT PP-OK
                       DISPLAY 'ACCOUNT DEBIT FAILED, RC='
                               PP-RETURN-CODE
                       EXIT PARAGRAPH
                   END-IF
                   MOVE 'Y' TO CCP-FROM-ACCOUNT
                   MOVE 'PAYMENT FROM ACCOUNT' TO CCP-MERCHANT
               WHEN 'C'
                   MOVE 'N' TO CCP-FROM-ACCOUNT
                   MOVE 'CASH PAYMENT AT BRANCH' TO CCP-MERCHANT
               WHEN OTHER
                   DISPLAY 'SOURCE MUST BE A OR C.'
                   EXIT PARAGRAPH
           END-EVALUATE
           MOVE 'Y'          TO CCP-TYPE
           MOVE WS-IN-AMOUNT TO CCP-AMOUNT
           CALL 'CCPOST' USING CCP-PARMS CC-RECORD
           REWRITE CC-RECORD
           MOVE CC-OUTSTANDING TO WS-E-SAMT
           DISPLAY 'PAYMENT RECEIVED. OUTSTANDING NOW: ' WS-E-SAMT.

      *================================================================*
      * INQUIRY / RECENT                                               *
      *================================================================*
       5000-INQUIRY.
           PERFORM 8000-READ-CARD
           IF NOT CC-FS-OK
               EXIT PARAGRAPH
           END-IF
           PERFORM 8100-MASK
           DISPLAY '----------------------------------------'
           DISPLAY 'CARD          : ' WS-MASKED '  STATUS ' CC-STATUS
           DISPLAY 'NAME          : ' CC-NAME
           MOVE CC-LIMIT TO WS-E-AMT
           DISPLAY 'LIMIT         : ' WS-E-AMT
           MOVE CC-OUTSTANDING TO WS-E-SAMT
           DISPLAY 'OUTSTANDING   : ' WS-E-SAMT
           COMPUTE WS-E-SAMT = CC-LIMIT - CC-OUTSTANDING
           DISPLAY 'AVAILABLE     : ' WS-E-SAMT
           MOVE CC-STMT-BALANCE TO WS-E-SAMT
           DISPLAY 'LAST STATEMENT: ' WS-E-SAMT ' ON '
                   CC-LAST-STMT-DATE
           MOVE CC-MIN-DUE TO WS-E-AMT
           DISPLAY 'MINIMUM DUE   : ' WS-E-AMT ' BY ' CC-DUE-DATE
           MOVE CC-PAID-SINCE-STMT TO WS-E-AMT
           DISPLAY 'PAID SINCE    : ' WS-E-AMT
           DISPLAY 'AUTOPAY       : ' CC-AUTOPAY '   LATE PAYMENTS '
                   CC-LATE-COUNT
           DISPLAY 'NEXT STATEMENT: ' CC-NEXT-STMT-DATE
           DISPLAY '----------------------------------------'.

       5500-RECENT.
           PERFORM 8000-READ-CARD
           IF NOT CC-FS-OK
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-RING
           MOVE 1 TO WS-RING-NEXT
           MOVE ZERO TO WS-RING-CNT
           OPEN INPUT CCT-FILE
           IF WS-CCT-FS NOT = '00'
               DISPLAY 'NO CARD TRANSACTIONS YET.'
               EXIT PARAGRAPH
           END-IF
           PERFORM FOREVER
               READ CCT-FILE
                   AT END EXIT PERFORM
               END-READ
               IF CCT-CARD-NO = CC-NO
                   MOVE CCT-AMOUNT TO WS-E-AMT
                   MOVE SPACES TO WS-RING-LINE(WS-RING-NEXT)
                   STRING CCT-DATE '  ' CCT-TYPE '  ' WS-E-AMT '  '
                          CCT-MERCHANT '  ' CCT-AUTH-CODE
                          DELIMITED BY SIZE
                          INTO WS-RING-LINE(WS-RING-NEXT)
                   END-STRING
                   ADD 1 TO WS-RING-CNT
                   IF WS-RING-NEXT = 10
                       MOVE 1 TO WS-RING-NEXT
                   ELSE
                       ADD 1 TO WS-RING-NEXT
                   END-IF
               END-IF
           END-PERFORM
           CLOSE CCT-FILE
           DISPLAY 'DATE      T        AMOUNT  DETAIL'
      *    OLDEST TO NEWEST OF THE LAST 10
           IF WS-RING-CNT < 10
               MOVE 1 TO WS-J
           ELSE
               MOVE WS-RING-NEXT TO WS-J
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               IF WS-RING-LINE(WS-J) NOT = SPACES
                   DISPLAY WS-RING-LINE(WS-J)
               END-IF
               IF WS-J = 10
                   MOVE 1 TO WS-J
               ELSE
                   ADD 1 TO WS-J
               END-IF
           END-PERFORM.

      *================================================================*
      * BLOCK / REFUND / LIMIT                                         *
      *================================================================*
       6000-BLOCK-TOGGLE.
           PERFORM 8000-READ-CARD
           IF NOT CC-FS-OK
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN CC-ACTIVE
                   SET CC-BLOCKED TO TRUE
                   MOVE 'CC-BLOCK' TO AU-ACTION
               WHEN CC-BLOCKED
                   SET CC-ACTIVE TO TRUE
                   MOVE 'CC-UNBLOCK' TO AU-ACTION
               WHEN OTHER
                   DISPLAY 'CARD IS CLOSED.'
                   EXIT PARAGRAPH
           END-EVALUATE
           REWRITE CC-RECORD
           DISPLAY 'CARD IS NOW STATUS ' CC-STATUS
           PERFORM 9000-AUDIT.

       6500-REFUND.
           PERFORM 8000-READ-CARD
           IF NOT CC-FS-OK
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'REFUND AMOUNT : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           DISPLAY 'MERCHANT      : ' WITH NO ADVANCING
           ACCEPT WS-IN-MERCHANT
           INITIALIZE CCP-PARMS
           MOVE 'R'          TO CCP-TYPE
           MOVE WS-IN-AMOUNT TO CCP-AMOUNT
           MOVE FUNCTION UPPER-CASE(WS-IN-MERCHANT) TO CCP-MERCHANT
           CALL 'CCPOST' USING CCP-PARMS CC-RECORD
           IF NOT CCP-OK
               DISPLAY 'REFUND NOT POSTED, RC=' CCP-RC
               EXIT PARAGRAPH
           END-IF
           REWRITE CC-RECORD
           MOVE CC-OUTSTANDING TO WS-E-SAMT
           DISPLAY 'REFUND POSTED. OUTSTANDING NOW: ' WS-E-SAMT.

       7000-CHANGE-LIMIT.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'LIMIT CHANGES NEED A MANAGER OR ADMIN.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8000-READ-CARD
           IF NOT CC-FS-OK
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'NEW LIMIT: ' WITH NO ADVANCING
           ACCEPT WS-IN-LIMIT
           IF WS-IN-LIMIT < WS-MIN-LIMIT OR WS-IN-LIMIT > WS-MAX-LIMIT
               DISPLAY 'LIMIT MUST BE 10,000 TO 5,00,000.'
               EXIT PARAGRAPH
           END-IF
           IF WS-IN-LIMIT < CC-OUTSTANDING
               DISPLAY 'LIMIT CANNOT BE BELOW THE OUTSTANDING BALANCE.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-LIMIT TO CC-LIMIT
           COMPUTE CC-CASH-LIMIT = WS-IN-LIMIT * WS-CASH-PCT
           REWRITE CC-RECORD
           DISPLAY 'LIMIT CHANGED.'
           MOVE 'CC-LIMIT' TO AU-ACTION
           PERFORM 9000-AUDIT.

      *================================================================*
      * COMMON                                                         *
      *================================================================*
       8000-READ-CARD.
           DISPLAY 'CARD NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-CARD-NO
           MOVE WS-IN-CARD-NO TO CC-NO
           READ CC-FILE
               INVALID KEY
                   DISPLAY 'CARD NOT FOUND.'
           END-READ.

       8100-MASK.
           MOVE SPACES TO WS-MASKED
           STRING CC-NO(1:4) ' XXXX XXXX ' CC-NO(13:4)
                  DELIMITED BY SIZE INTO WS-MASKED
           END-STRING.

       9000-AUDIT.
           MOVE 'CCMNT'   TO AU-PROGRAM
           MOVE CC-NO     TO AU-KEY
           MOVE SPACES    TO AU-DETAIL
           MOVE CC-LIMIT  TO WS-E-AMT
           STRING 'LIMIT ' FUNCTION TRIM(WS-E-AMT) ' STATUS '
                  CC-STATUS DELIMITED BY SIZE INTO AU-DETAIL
           END-STRING
           CALL 'AUDITLOG' USING AUD-PARMS.

      *    20 BLANK MENU INPUTS IN A ROW = INPUT EXHAUSTED, LEAVE MENU
       8990-GUARD-END-OF-INPUT.
           IF WS-CHOICE = SPACE
               ADD 1 TO WS-BLANK-INPUTS
               IF WS-BLANK-INPUTS > 20
                   MOVE '9' TO WS-CHOICE
                   MOVE ZERO TO WS-BLANK-INPUTS
               END-IF
           ELSE
               MOVE ZERO TO WS-BLANK-INPUTS
           END-IF.
