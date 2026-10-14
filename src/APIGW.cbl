       IDENTIFICATION DIVISION.
       PROGRAM-ID. APIGW.
      *================================================================*
      * PROGRAM  : APIGW                                               *
      * PURPOSE  : JSON API GATEWAY - THE COBOL SIDE OF THE MOBILE /   *
      *            NET BANKING API (THE ROLE z/OS CONNECT PLAYS ON A   *
      *            MAINFRAME). READS ONE JSON REQUEST PER LINE FROM    *
      *            STDIN AND WRITES ONE JSON RESPONSE PER LINE.        *
      *            OPERATIONS ("op"):                                  *
      *              health       - LIVENESS CHECK                     *
      *              balance      - "account"                          *
      *              ministatement- "account", LAST 5 TRANSACTIONS     *
      *              transfer     - "from","to","amount","narration"   *
      *                             (WRITE SCOPE, MAX 2,00,000)        *
      *            EVERY REQUEST CARRIES "apikey", CHECKED AGAINST     *
      *            data/APIKEYS.DAT (KEY, CLIENT, SCOPE R OR W).       *
      *            REQUESTS ARE AUDITED UNDER USER 'API' + CLIENT.     *
      *            THE PARSER HANDLES FLAT OBJECTS OF STRING / NUMBER  *
      *            VALUES, WHICH IS ALL THIS API ACCEPTS.              *
      * FILES    : APIKEYS (INPUT), TRANLOG (INPUT)                    *
      * CALLS    : ACCTPOST, AUDITLOG                                  *
      * RETURN   : 0 ALWAYS (ERRORS ARE REPORTED IN THE JSON)          *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KEY-FILE ASSIGN TO 'data/APIKEYS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-KEY-FS.
           SELECT TRAN-FILE ASSIGN TO 'data/TRANLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRAN-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  KEY-FILE.
       01  KEY-RECORD.
           05  KEY-VALUE               PIC X(32).
           05  FILLER                  PIC X(01).
           05  KEY-CLIENT              PIC X(20).
           05  FILLER                  PIC X(01).
           05  KEY-SCOPE               PIC X(01).
       FD  TRAN-FILE.
           COPY TRANREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY SESSION.
           COPY AUDPARM.

       01  WS-KEY-FS                   PIC X(02).
       01  WS-REQUEST                  PIC X(1000).
       01  WS-RESPONSE                 PIC X(2000).
       01  WS-RESP-PTR                 PIC 9(04).
       01  WS-EOF-SW                   PIC X(01) VALUE 'N'.
           88  NO-MORE-REQUESTS                  VALUE 'Y'.
       01  WS-REQ-COUNT                PIC 9(07) VALUE ZERO.

      * PARSER
       01  WS-KEY-NAME                 PIC X(20).
       01  WS-PATTERN                  PIC X(24).
       01  WS-PAT-LEN                  PIC 9(02).
       01  WS-VALUE                    PIC X(100).
       01  WS-FOUND-SW                 PIC X(01).
           88  KEY-FOUND                         VALUE 'Y'.
       01  WS-POS                      PIC 9(04).
       01  WS-START                    PIC 9(04).
       01  WS-END                      PIC 9(04).
       01  WS-REQ-LEN                  PIC 9(04).

      * REQUEST FIELDS
       01  WS-OP                       PIC X(20).
       01  WS-APIKEY                   PIC X(32).
       01  WS-CLIENT                   PIC X(20).
       01  WS-SCOPE                    PIC X(01).
       01  WS-ACCOUNT                  PIC 9(10).
       01  WS-TO-ACCOUNT               PIC 9(10).
       01  WS-AMOUNT                   PIC 9(11)V99.
       01  WS-NARRATION                PIC X(30).
       01  WS-MAX-API-TRANSFER         PIC 9(11)V99 VALUE 200000.00.

      * RESPONSE PIECES
       01  WS-E-AMT                    PIC -(13)9.99.
       01  WS-E-TXT                    PIC X(30).
       01  WS-ERR-CODE                 PIC X(20).
       01  WS-ERR-TEXT                 PIC X(60).

      * MINI STATEMENT RING
       01  WS-RING.
           05  WS-RING-ENTRY OCCURS 5 TIMES.
               10  RG-DATE             PIC 9(08).
               10  RG-TYPE             PIC X(02).
               10  RG-AMOUNT           PIC 9(11)V99.
               10  RG-CRDR             PIC X(02).
               10  RG-NARR             PIC X(30).
       01  WS-RING-NEXT                PIC 9(01).
       01  WS-RING-CNT                 PIC 9(05).
       01  WS-I                        PIC 9(01).
       01  WS-J                        PIC 9(01).
       01  WS-SHOWN                    PIC 9(01).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL NO-MORE-REQUESTS
               MOVE SPACES TO WS-REQUEST
               ACCEPT WS-REQUEST
               IF WS-REQUEST = SPACES OR WS-REQUEST(1:1) NOT = '{'
                   SET NO-MORE-REQUESTS TO TRUE
               ELSE
                   ADD 1 TO WS-REQ-COUNT
                   PERFORM 1000-HANDLE-REQUEST
               END-IF
           END-PERFORM
           MOVE ZERO TO RETURN-CODE
           GOBACK.

       1000-HANDLE-REQUEST.
           MOVE SPACES TO WS-RESPONSE WS-ERR-CODE WS-ERR-TEXT
           MOVE ZERO TO WS-ACCOUNT WS-TO-ACCOUNT WS-AMOUNT
           MOVE 1 TO WS-RESP-PTR
           COMPUTE WS-REQ-LEN =
                   FUNCTION LENGTH(FUNCTION TRIM(WS-REQUEST TRAILING))
           MOVE 'op' TO WS-KEY-NAME
           PERFORM 8000-GET-VALUE
           MOVE FUNCTION LOWER-CASE(WS-VALUE) TO WS-OP
           IF WS-OP = 'health'
               STRING '{"status":"ok","service":"corebank-api"}'
                      DELIMITED BY SIZE INTO WS-RESPONSE
               END-STRING
               PERFORM 9000-SEND
               EXIT PARAGRAPH
           END-IF
           PERFORM 1100-AUTHENTICATE
           IF WS-ERR-CODE = SPACES
               EVALUATE WS-OP
                   WHEN 'balance'       PERFORM 2000-BALANCE
                   WHEN 'ministatement' PERFORM 3000-MINI-STATEMENT
                   WHEN 'transfer'      PERFORM 4000-TRANSFER
                   WHEN OTHER
                       MOVE 'UNKNOWN_OP' TO WS-ERR-CODE
                       MOVE 'unknown op' TO WS-ERR-TEXT
               END-EVALUATE
           END-IF
           IF WS-ERR-CODE NOT = SPACES
               PERFORM 8800-ERROR-RESPONSE
           END-IF
           PERFORM 9000-SEND
           PERFORM 9100-AUDIT.

      *    THE KEY FILE IS SMALL; READ IT FOR EACH REQUEST SO A REVOKED
      *    KEY STOPS WORKING AT ONCE
       1100-AUTHENTICATE.
           MOVE 'apikey' TO WS-KEY-NAME
           PERFORM 8000-GET-VALUE
           MOVE WS-VALUE TO WS-APIKEY
           MOVE SPACES TO WS-CLIENT WS-SCOPE
           IF WS-APIKEY = SPACES
               MOVE 'UNAUTHORISED' TO WS-ERR-CODE
               MOVE 'apikey is required' TO WS-ERR-TEXT
               EXIT PARAGRAPH
           END-IF
           OPEN INPUT KEY-FILE
           IF WS-KEY-FS = '00'
               PERFORM FOREVER
                   READ KEY-FILE
                       AT END EXIT PERFORM
                   END-READ
                   IF KEY-VALUE = WS-APIKEY
                       MOVE KEY-CLIENT TO WS-CLIENT
                       MOVE KEY-SCOPE  TO WS-SCOPE
                       EXIT PERFORM
                   END-IF
               END-PERFORM
               CLOSE KEY-FILE
           END-IF
           IF WS-CLIENT = SPACES
               MOVE 'UNAUTHORISED' TO WS-ERR-CODE
               MOVE 'apikey not recognised' TO WS-ERR-TEXT
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO SES-USER-ID
           STRING 'API' WS-CLIENT(1:5) DELIMITED BY SIZE
                  INTO SES-USER-ID
           END-STRING
           MOVE 'T' TO SES-ROLE.

      *================================================================*
      * OPERATIONS                                                     *
      *================================================================*
       2000-BALANCE.
           PERFORM 8100-GET-ACCOUNT
           IF WS-ERR-CODE NOT = SPACES
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-ACCOUNT TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               MOVE 'NOT_FOUND' TO WS-ERR-CODE
               MOVE 'account not found' TO WS-ERR-TEXT
               EXIT PARAGRAPH
           END-IF
           STRING '{"status":"ok","account":"' WS-ACCOUNT '"'
                  DELIMITED BY SIZE INTO WS-RESPONSE
                  WITH POINTER WS-RESP-PTR
           END-STRING
           MOVE 'name' TO WS-KEY-NAME
           MOVE PP-HOLDER-NAME TO WS-VALUE
           PERFORM 8700-ADD-STRING-FIELD
           MOVE 'balance' TO WS-KEY-NAME
           MOVE PP-BALANCE TO WS-E-AMT
           PERFORM 8710-ADD-NUMBER-FIELD
           MOVE 'available' TO WS-KEY-NAME
           MOVE PP-AVAILABLE TO WS-E-AMT
           PERFORM 8710-ADD-NUMBER-FIELD
           STRING '}' DELIMITED BY SIZE INTO WS-RESPONSE
                  WITH POINTER WS-RESP-PTR
           END-STRING.

       3000-MINI-STATEMENT.
           PERFORM 8100-GET-ACCOUNT
           IF WS-ERR-CODE NOT = SPACES
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-RING
           MOVE 1 TO WS-RING-NEXT
           MOVE ZERO TO WS-RING-CNT
           OPEN INPUT TRAN-FILE
           IF TRAN-FS-OK
               PERFORM FOREVER
                   READ TRAN-FILE
                       AT END EXIT PERFORM
                   END-READ
                   IF TRAN-ACCT-NO = WS-ACCOUNT
                       PERFORM 3100-ADD-TO-RING
                   END-IF
               END-PERFORM
               CLOSE TRAN-FILE
           END-IF
           STRING '{"status":"ok","account":"' WS-ACCOUNT
                  '","transactions":[' DELIMITED BY SIZE
                  INTO WS-RESPONSE WITH POINTER WS-RESP-PTR
           END-STRING
      *    NEWEST FIRST
           MOVE WS-RING-NEXT TO WS-J
           MOVE ZERO TO WS-SHOWN
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               IF WS-J = 1
                   MOVE 5 TO WS-J
               ELSE
                   SUBTRACT 1 FROM WS-J
               END-IF
               IF RG-DATE(WS-J) NOT = ZERO
                   IF WS-SHOWN > ZERO
                       STRING ',' DELIMITED BY SIZE INTO WS-RESPONSE
                              WITH POINTER WS-RESP-PTR
                       END-STRING
                   END-IF
                   ADD 1 TO WS-SHOWN
                   MOVE RG-AMOUNT(WS-J) TO WS-E-AMT
                   MOVE RG-NARR(WS-J) TO WS-E-TXT
                   INSPECT WS-E-TXT REPLACING ALL '"' BY "'"
                   STRING '{"date":"' RG-DATE(WS-J) '","type":"'
                          RG-TYPE(WS-J) '","drcr":"' RG-CRDR(WS-J)
                          '","amount":' FUNCTION TRIM(WS-E-AMT)
                          ',"narration":"' FUNCTION TRIM(WS-E-TXT)
                          '"}' DELIMITED BY SIZE INTO WS-RESPONSE
                          WITH POINTER WS-RESP-PTR
                   END-STRING
               END-IF
           END-PERFORM
           STRING ']}' DELIMITED BY SIZE INTO WS-RESPONSE
                  WITH POINTER WS-RESP-PTR
           END-STRING.

       3100-ADD-TO-RING.
           MOVE TRAN-DATE      TO RG-DATE(WS-RING-NEXT)
           MOVE TRAN-TYPE      TO RG-TYPE(WS-RING-NEXT)
           MOVE TRAN-AMOUNT    TO RG-AMOUNT(WS-RING-NEXT)
           MOVE TRAN-NARRATION TO RG-NARR(WS-RING-NEXT)
           IF TRAN-CREDIT-TYPE
               MOVE 'CR' TO RG-CRDR(WS-RING-NEXT)
           ELSE
               MOVE 'DR' TO RG-CRDR(WS-RING-NEXT)
           END-IF
           IF WS-RING-NEXT = 5
               MOVE 1 TO WS-RING-NEXT
           ELSE
               ADD 1 TO WS-RING-NEXT
           END-IF.

       4000-TRANSFER.
           IF WS-SCOPE NOT = 'W'
               MOVE 'FORBIDDEN' TO WS-ERR-CODE
               MOVE 'this apikey is read-only' TO WS-ERR-TEXT
               EXIT PARAGRAPH
           END-IF
           MOVE 'from' TO WS-KEY-NAME
           PERFORM 8000-GET-VALUE
           IF WS-VALUE(1:10) IS NOT NUMERIC
              OR WS-VALUE(11:1) NOT = SPACE
               MOVE 'BAD_REQUEST' TO WS-ERR-CODE
               MOVE 'from must be a 10-digit account' TO WS-ERR-TEXT
               EXIT PARAGRAPH
           END-IF
           MOVE WS-VALUE(1:10) TO WS-ACCOUNT
           MOVE 'to' TO WS-KEY-NAME
           PERFORM 8000-GET-VALUE
           IF WS-VALUE(1:10) IS NOT NUMERIC
              OR WS-VALUE(11:1) NOT = SPACE
               MOVE 'BAD_REQUEST' TO WS-ERR-CODE
               MOVE 'to must be a 10-digit account' TO WS-ERR-TEXT
               EXIT PARAGRAPH
           END-IF
           MOVE WS-VALUE(1:10) TO WS-TO-ACCOUNT
           MOVE 'amount' TO WS-KEY-NAME
           PERFORM 8000-GET-VALUE
           IF FUNCTION TEST-NUMVAL(WS-VALUE) NOT = ZERO
              OR WS-VALUE = SPACES
               MOVE 'BAD_REQUEST' TO WS-ERR-CODE
               MOVE 'amount must be a number' TO WS-ERR-TEXT
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-AMOUNT = FUNCTION NUMVAL(WS-VALUE)
           MOVE 'narration' TO WS-KEY-NAME
           PERFORM 8000-GET-VALUE
           MOVE WS-VALUE TO WS-NARRATION
           IF WS-NARRATION = SPACES
               MOVE 'API TRANSFER' TO WS-NARRATION
           END-IF
           EVALUATE TRUE
               WHEN WS-AMOUNT NOT > ZERO
                   MOVE 'BAD_REQUEST' TO WS-ERR-CODE
                   MOVE 'amount must be positive' TO WS-ERR-TEXT
               WHEN WS-AMOUNT > WS-MAX-API-TRANSFER
                   MOVE 'LIMIT_EXCEEDED' TO WS-ERR-CODE
                   MOVE 'api transfers are limited to 200000'
                                                 TO WS-ERR-TEXT
               WHEN WS-ACCOUNT = WS-TO-ACCOUNT
                   MOVE 'BAD_REQUEST' TO WS-ERR-CODE
                   MOVE 'from and to are the same account'
                                                 TO WS-ERR-TEXT
           END-EVALUATE
           IF WS-ERR-CODE NOT = SPACES
               EXIT PARAGRAPH
           END-IF

           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-TO-ACCOUNT TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               MOVE 'NOT_FOUND' TO WS-ERR-CODE
               MOVE 'destination account not found' TO WS-ERR-TEXT
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE WS-ACCOUNT    TO PP-ACCT-NO
           MOVE WS-AMOUNT     TO PP-AMOUNT
           MOVE 'TO'          TO PP-TRAN-TYPE
           MOVE WS-TO-ACCOUNT TO PP-REF-ACCT
           MOVE WS-NARRATION  TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           EVALUATE TRUE
               WHEN PP-OK
                   CONTINUE
               WHEN PP-INSUFFICIENT
                   MOVE 'INSUFFICIENT_FUNDS' TO WS-ERR-CODE
                   MOVE 'not enough available balance' TO WS-ERR-TEXT
                   EXIT PARAGRAPH
               WHEN PP-NOT-FOUND
                   MOVE 'NOT_FOUND' TO WS-ERR-CODE
                   MOVE 'source account not found' TO WS-ERR-TEXT
                   EXIT PARAGRAPH
               WHEN OTHER
                   MOVE 'ACCOUNT_BLOCKED' TO WS-ERR-CODE
                   MOVE 'source account cannot be debited'
                                                 TO WS-ERR-TEXT
                   EXIT PARAGRAPH
           END-EVALUATE
           MOVE PP-BALANCE TO WS-E-AMT
           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE WS-TO-ACCOUNT TO PP-ACCT-NO
           MOVE WS-AMOUNT     TO PP-AMOUNT
           MOVE 'TI'          TO PP-TRAN-TYPE
           MOVE WS-ACCOUNT    TO PP-REF-ACCT
           MOVE WS-NARRATION  TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
      *        PUT IT BACK SO NOTHING IS LOST
               INITIALIZE POST-PARMS
               SET PP-CREDIT TO TRUE
               MOVE WS-ACCOUNT    TO PP-ACCT-NO
               MOVE WS-AMOUNT     TO PP-AMOUNT
               MOVE 'TI'          TO PP-TRAN-TYPE
               MOVE WS-TO-ACCOUNT TO PP-REF-ACCT
               MOVE 'API TRANSFER REVERSED' TO PP-NARRATION
               CALL 'ACCTPOST' USING POST-PARMS
               MOVE 'ACCOUNT_BLOCKED' TO WS-ERR-CODE
               MOVE 'destination cannot be credited; reversed'
                                                 TO WS-ERR-TEXT
               EXIT PARAGRAPH
           END-IF
           STRING '{"status":"ok","from":"' WS-ACCOUNT '","to":"'
                  WS-TO-ACCOUNT '"' DELIMITED BY SIZE
                  INTO WS-RESPONSE WITH POINTER WS-RESP-PTR
           END-STRING
           MOVE 'from_balance' TO WS-KEY-NAME
           PERFORM 8710-ADD-NUMBER-FIELD
           MOVE 'amount' TO WS-KEY-NAME
           MOVE WS-AMOUNT TO WS-E-AMT
           PERFORM 8710-ADD-NUMBER-FIELD
           STRING '}' DELIMITED BY SIZE INTO WS-RESPONSE
                  WITH POINTER WS-RESP-PTR
           END-STRING.

      *================================================================*
      * JSON HELPERS                                                   *
      *================================================================*
      *    FINDS "KEY" : VALUE IN WS-REQUEST. VALUE IS THE TEXT BETWEEN
      *    QUOTES, OR UP TO THE NEXT , OR } FOR A BARE NUMBER.
       8000-GET-VALUE.
           MOVE SPACES TO WS-VALUE WS-PATTERN
           MOVE 'N' TO WS-FOUND-SW
           STRING '"' FUNCTION TRIM(WS-KEY-NAME) '"'
                  DELIMITED BY SIZE INTO WS-PATTERN
           END-STRING
           COMPUTE WS-PAT-LEN =
                   FUNCTION LENGTH(FUNCTION TRIM(WS-PATTERN))
           PERFORM VARYING WS-POS FROM 1 BY 1
                   UNTIL WS-POS > WS-REQ-LEN - WS-PAT-LEN OR KEY-FOUND
               IF WS-REQUEST(WS-POS:WS-PAT-LEN) =
                  WS-PATTERN(1:WS-PAT-LEN)
                   MOVE 'Y' TO WS-FOUND-SW
               END-IF
           END-PERFORM
           IF NOT KEY-FOUND
               EXIT PARAGRAPH
           END-IF
      *    WS-POS IS ONE PAST THE MATCH START; SKIP KEY, SPACES, COLON
           COMPUTE WS-START = WS-POS - 1 + WS-PAT-LEN
           PERFORM UNTIL WS-START > WS-REQ-LEN
                   OR (WS-REQUEST(WS-START:1) NOT = SPACE
                       AND WS-REQUEST(WS-START:1) NOT = ':')
               ADD 1 TO WS-START
           END-PERFORM
           IF WS-START > WS-REQ-LEN
               EXIT PARAGRAPH
           END-IF
           IF WS-REQUEST(WS-START:1) = '"'
               ADD 1 TO WS-START
               MOVE WS-START TO WS-END
               PERFORM UNTIL WS-END > WS-REQ-LEN
                       OR WS-REQUEST(WS-END:1) = '"'
                   ADD 1 TO WS-END
               END-PERFORM
           ELSE
               MOVE WS-START TO WS-END
               PERFORM UNTIL WS-END > WS-REQ-LEN
                       OR WS-REQUEST(WS-END:1) = ','
                       OR WS-REQUEST(WS-END:1) = '}'
                       OR WS-REQUEST(WS-END:1) = SPACE
                   ADD 1 TO WS-END
               END-PERFORM
           END-IF
           IF WS-END > WS-START
              AND WS-END - WS-START <= 100
               MOVE WS-REQUEST(WS-START:WS-END - WS-START) TO WS-VALUE
           END-IF.

       8100-GET-ACCOUNT.
           MOVE 'account' TO WS-KEY-NAME
           PERFORM 8000-GET-VALUE
           IF WS-VALUE(1:10) IS NOT NUMERIC
              OR WS-VALUE(11:1) NOT = SPACE
               MOVE 'BAD_REQUEST' TO WS-ERR-CODE
               MOVE 'account must be a 10-digit number' TO WS-ERR-TEXT
           ELSE
               MOVE WS-VALUE(1:10) TO WS-ACCOUNT
           END-IF.

       8700-ADD-STRING-FIELD.
           INSPECT WS-VALUE REPLACING ALL '"' BY "'"
           STRING ',"' FUNCTION TRIM(WS-KEY-NAME) '":"'
                  FUNCTION TRIM(WS-VALUE) '"'
                  DELIMITED BY SIZE INTO WS-RESPONSE
                  WITH POINTER WS-RESP-PTR
           END-STRING.

       8710-ADD-NUMBER-FIELD.
           STRING ',"' FUNCTION TRIM(WS-KEY-NAME) '":'
                  FUNCTION TRIM(WS-E-AMT)
                  DELIMITED BY SIZE INTO WS-RESPONSE
                  WITH POINTER WS-RESP-PTR
           END-STRING.

       8800-ERROR-RESPONSE.
           MOVE SPACES TO WS-RESPONSE
           MOVE 1 TO WS-RESP-PTR
           STRING '{"status":"error","code":"'
                  FUNCTION TRIM(WS-ERR-CODE)
                  '","message":"' FUNCTION TRIM(WS-ERR-TEXT) '"}'
                  DELIMITED BY SIZE INTO WS-RESPONSE
                  WITH POINTER WS-RESP-PTR
           END-STRING.

       9000-SEND.
           DISPLAY FUNCTION TRIM(WS-RESPONSE TRAILING).

       9100-AUDIT.
           MOVE 'APIGW'  TO AU-PROGRAM
           MOVE WS-OP    TO AU-ACTION
           MOVE WS-ACCOUNT TO AU-KEY
           MOVE SPACES TO AU-DETAIL
           IF WS-ERR-CODE = SPACES
               MOVE 'OK' TO AU-DETAIL
           ELSE
               MOVE WS-ERR-CODE TO AU-DETAIL
           END-IF
           CALL 'AUDITLOG' USING AUD-PARMS.
