       IDENTIFICATION DIVISION.
       PROGRAM-ID. CMPMNT.
      *================================================================*
      * PROGRAM  : CMPMNT                                              *
      * PURPOSE  : CUSTOMER COMPLAINTS AND SERVICE REQUESTS.           *
      *            1 LOG A TICKET (SLA DUE DATE FROM THE CATEGORY)     *
      *            2 VIEW A TICKET                                     *
      *            3 ASSIGN                                            *
      *            4 RESOLVE / REJECT - FOR A FAILED ATM DEBIT THE     *
      *              DISPUTED AMOUNT IS REFUNDED AND, PAST T+5 DAYS,   *
      *              RS 100 A DAY COMPENSATION IS PAID AUTOMATICALLY   *
      *              (RBI TURN-AROUND-TIME RULE). OTHER CATEGORIES:    *
      *              OPTIONAL GOODWILL COMPENSATION (MANAGER, MAX      *
      *              10,000).                                          *
      *            5 CUSTOMER FEEDBACK - CLOSE, OR REOPEN ONE LEVEL UP *
      *            6 PENDING TICKETS WITH OVERDUE / OMBUDSMAN FLAGS    *
      *            ESCALATION AND THE OMBUDSMAN RETURN ARE IN CMPBATCH.*
      * FILES    : CMPMAST (INDEXED, I-O)                              *
      * CALLS    : ACCTPOST, CUSTLKUP, DATEUTIL, AUDITLOG              *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMP-FILE ASSIGN TO 'data/CMPMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CMP-NO
               FILE STATUS IS WS-CMP-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CMP-FILE.
           COPY CMPREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY SESSION.
           COPY AUDPARM.
           COPY POSTPARM.
           COPY CLKPARM.
           COPY DATEPARM.
           COPY GLCODES.
       01  WS-CMP-FS                   PIC X(02).
       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.
       01  WS-EOF-SW                   PIC X(01).
       01  WS-CONFIRM                  PIC X(01).
       01  WS-IN-NO                    PIC 9(08).
       01  WS-IN-ACCT                  PIC 9(10).
       01  WS-IN-TYPE                  PIC X(01).
       01  WS-IN-CAT                   PIC X(04).
       01  WS-IN-CHANNEL               PIC X(01).
       01  WS-IN-TEXT                  PIC X(60).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-IN-AMT                   PIC X(12).
       01  WS-IN-USER                  PIC X(08).
       01  WS-IN-ACTION                PIC X(01).
       01  WS-AMOUNT                   PIC 9(09)V99.
       01  WS-NEXT-NO                  PIC 9(08).
       01  WS-DAYS                     PIC S9(07).
       01  WS-LATE-DAYS                PIC S9(07).
       01  WS-CNT                      PIC 9(05).
       01  WS-FLAGS                    PIC X(20).
       01  WS-DISP-AMT                 PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-AMT2                PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-DAYS                PIC Z,ZZ9.

      * RULES
       01  WS-ATM-TAT-DAYS             PIC 9(03) VALUE 5.
       01  WS-ATM-COMP-PER-DAY         PIC 9(05)V99 VALUE 100.00.
       01  WS-MAX-GOODWILL             PIC 9(09)V99 VALUE 10000.00.
       01  WS-OMBUDSMAN-DAYS           PIC 9(03) VALUE 30.
       01  WS-REOPEN-SLA               PIC 9(03) VALUE 7.

      * CATEGORY, SLA DAYS, DESCRIPTION
       01  WS-CAT-TABLE.
           05  FILLER PIC X(31) VALUE 'ATMF005ATM / CARD DEBIT FAILED'.
           05  FILLER PIC X(31) VALUE 'CARD007CARD SERVICES          '.
           05  FILLER PIC X(31) VALUE 'FRAD010UNAUTHORISED / FRAUD   '.
           05  FILLER PIC X(31) VALUE 'CHRG007CHARGES / INTEREST     '.
           05  FILLER PIC X(31) VALUE 'ACCT015ACCOUNT OPERATIONS     '.
           05  FILLER PIC X(31) VALUE 'LOAN021LOANS AND ADVANCES     '.
           05  FILLER PIC X(31) VALUE 'SERV007SERVICE REQUEST        '.
           05  FILLER PIC X(31) VALUE 'OTHR030OTHERS                 '.
       01  WS-CAT-TABLE-R REDEFINES WS-CAT-TABLE.
           05  WS-CAT OCCURS 8 TIMES INDEXED BY CAT-IDX.
               10  CAT-CODE            PIC X(04).
               10  CAT-SLA             PIC 9(03).
               10  CAT-DESC            PIC X(24).

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN I-O CMP-FILE
           IF WS-CMP-FS = '35'
               OPEN OUTPUT CMP-FILE
               CLOSE CMP-FILE
               OPEN I-O CMP-FILE
           END-IF
           IF WS-CMP-FS NOT = '00'
               DISPLAY 'COMPLAINT FILE OPEN FAILED, FS=' WS-CMP-FS
               GOBACK
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE = '9'
               DISPLAY ' '
               DISPLAY '------ COMPLAINTS AND SERVICE REQUESTS ------'
               DISPLAY ' 1. LOG A TICKET'
               DISPLAY ' 2. VIEW A TICKET'
               DISPLAY ' 3. ASSIGN'
               DISPLAY ' 4. RESOLVE / REJECT'
               DISPLAY ' 5. CUSTOMER FEEDBACK (CLOSE / REOPEN)'
               DISPLAY ' 6. PENDING TICKETS'
               DISPLAY ' 9. BACK'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 1000-LOG
                   WHEN '2' PERFORM 2000-VIEW
                   WHEN '3' PERFORM 3000-ASSIGN
                   WHEN '4' PERFORM 4000-RESOLVE
                   WHEN '5' PERFORM 5000-FEEDBACK
                   WHEN '6' PERFORM 6000-PENDING
                   WHEN '9' CONTINUE
                   WHEN SPACE
                       ADD 1 TO WS-BLANK-INPUTS
                       IF WS-BLANK-INPUTS > 20
                           MOVE '9' TO WS-CHOICE
                       END-IF
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
               IF WS-CHOICE NOT = SPACE
                   MOVE ZERO TO WS-BLANK-INPUTS
               END-IF
           END-PERFORM
           CLOSE CMP-FILE
           MOVE 'Y' TO CLK-CLOSE
           CALL 'CUSTLKUP' USING CLK-PARMS
           GOBACK.

      *----------------------------------------------------------------*
      * 1. LOG                                                         *
      *----------------------------------------------------------------*
       1000-LOG.
           DISPLAY 'ACCOUNT NO                    : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-ACCT TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF PP-NOT-FOUND
               DISPLAY 'ACCOUNT NOT FOUND.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'C=COMPLAINT S=SERVICE REQUEST : ' WITH NO ADVANCING
           ACCEPT WS-IN-TYPE
           MOVE FUNCTION UPPER-CASE(WS-IN-TYPE) TO WS-IN-TYPE
           DISPLAY 'CATEGORY (ATMF CARD FRAD CHRG ACCT LOAN SERV '
                   'OTHR): ' WITH NO ADVANCING
           ACCEPT WS-IN-CAT
           MOVE FUNCTION UPPER-CASE(WS-IN-CAT) TO WS-IN-CAT
           DISPLAY 'CHANNEL B/P/E/W               : ' WITH NO ADVANCING
           ACCEPT WS-IN-CHANNEL
           MOVE FUNCTION UPPER-CASE(WS-IN-CHANNEL) TO WS-IN-CHANNEL
           DISPLAY 'DESCRIPTION                   : ' WITH NO ADVANCING
           ACCEPT WS-IN-TEXT
           IF WS-IN-TYPE NOT = 'C' AND WS-IN-TYPE NOT = 'S'
               DISPLAY 'TYPE MUST BE C OR S.'
               EXIT PARAGRAPH
           END-IF
           SET CAT-IDX TO 1
           SEARCH WS-CAT
               AT END
                   DISPLAY 'UNKNOWN CATEGORY ' WS-IN-CAT '.'
                   EXIT PARAGRAPH
               WHEN CAT-CODE(CAT-IDX) = WS-IN-CAT
                   CONTINUE
           END-SEARCH
           IF WS-IN-TEXT = SPACES
               DISPLAY 'DESCRIPTION IS REQUIRED.'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE CMP-RECORD
           IF WS-IN-CAT = 'ATMF' OR WS-IN-CAT = 'FRAD'
               DISPLAY 'TRANSACTION DATE (YYYYMMDD)   : '
                       WITH NO ADVANCING
               ACCEPT WS-IN-DATE
               DISPLAY 'AMOUNT DISPUTED               : '
                       WITH NO ADVANCING
               ACCEPT WS-IN-AMT
               IF WS-IN-DATE IS NOT NUMERIC
                  OR FUNCTION TEST-NUMVAL(WS-IN-AMT) NOT = ZERO
                   DISPLAY 'DATE AND AMOUNT ARE REQUIRED FOR '
                           WS-IN-CAT '.'
                   EXIT PARAGRAPH
               END-IF
               MOVE WS-IN-DATE TO CMP-TXN-DATE
               COMPUTE CMP-AMOUNT = FUNCTION NUMVAL(WS-IN-AMT)
               IF CMP-TXN-DATE > WS-CURR-DATE OR CMP-AMOUNT = ZERO
                   DISPLAY 'TRANSACTION DATE CANNOT BE IN THE FUTURE '
                           'AND AMOUNT MUST BE ABOVE ZERO.'
                   EXIT PARAGRAPH
               END-IF
           END-IF

           MOVE WS-IN-ACCT TO CLK-ACCT-NO
           MOVE 'N' TO CLK-CLOSE
           CALL 'CUSTLKUP' USING CLK-PARMS
           IF CLK-OK
               MOVE CLK-CUST-ID TO CMP-CUST-ID
           END-IF
           PERFORM 8000-NEXT-NO
           MOVE WS-NEXT-NO      TO CMP-NO
           MOVE WS-IN-TYPE      TO CMP-TYPE
           MOVE WS-IN-CAT       TO CMP-CATEGORY
           MOVE WS-IN-CHANNEL   TO CMP-CHANNEL
           MOVE WS-IN-ACCT      TO CMP-ACCT-NO
           MOVE SES-BRANCH      TO CMP-BRANCH
           MOVE WS-IN-TEXT      TO CMP-DESC
           MOVE WS-CURR-DATE    TO CMP-OPEN-DATE
           MOVE WS-CURR-TIME    TO CMP-OPEN-TIME
           MOVE CAT-SLA(CAT-IDX) TO CMP-SLA-DAYS
           MOVE 1               TO CMP-LEVEL
           SET CMP-OPEN         TO TRUE
           MOVE SES-USER-ID     TO CMP-LOGGED-BY
      *    ATM FAILURES RUN FROM THE TRANSACTION DATE (T+5), THE REST
      *    FROM THE DAY THE TICKET IS LOGGED
           IF CMP-ATM-FAILED
               MOVE CMP-TXN-DATE TO DU-DATE-1
           ELSE
               MOVE WS-CURR-DATE TO DU-DATE-1
           END-IF
           SET DU-ADD-DAYS      TO TRUE
           MOVE CMP-SLA-DAYS    TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE  TO CMP-DUE-DATE
           WRITE CMP-RECORD
               INVALID KEY
                   DISPLAY 'TICKET WRITE FAILED FS=' WS-CMP-FS
                   EXIT PARAGRAPH
           END-WRITE
           MOVE 'TICKET OPEN' TO AU-ACTION
           MOVE CMP-CATEGORY  TO AU-DETAIL
           PERFORM 8900-AUDIT
           DISPLAY 'TICKET ' CMP-NO ' LOGGED (' CAT-DESC(CAT-IDX)
                   ') - DUE ' CMP-DUE-DATE '.'.

      *----------------------------------------------------------------*
      * 2. VIEW                                                        *
      *----------------------------------------------------------------*
       2000-VIEW.
           PERFORM 8100-READ-TICKET
           IF WS-CMP-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'TICKET ' CMP-NO '  ' CMP-TYPE '/' CMP-CATEGORY
                   '  STATUS ' CMP-STATUS '  LEVEL ' CMP-LEVEL
           DISPLAY '  ACCOUNT ' CMP-ACCT-NO '  CUSTOMER ' CMP-CUST-ID
                   '  BRANCH ' CMP-BRANCH
           DISPLAY '  ' CMP-DESC
           DISPLAY '  OPENED ' CMP-OPEN-DATE ' BY ' CMP-LOGGED-BY
                   '  DUE ' CMP-DUE-DATE '  ASSIGNED ' CMP-ASSIGNED-TO
           IF CMP-AMOUNT > ZERO
               MOVE CMP-AMOUNT TO WS-DISP-AMT
               DISPLAY '  DISPUTED ' WS-DISP-AMT ' ON ' CMP-TXN-DATE
           END-IF
           IF CMP-DISPOSED
               DISPLAY '  RESOLVED ' CMP-RESOLVED-DATE ' BY '
                       CMP-RESOLVED-BY ': ' CMP-RESOLUTION
               MOVE CMP-REFUND-AMT TO WS-DISP-AMT
               MOVE CMP-COMP-AMT   TO WS-DISP-AMT2
               DISPLAY '  REFUND ' WS-DISP-AMT '  COMPENSATION '
                       WS-DISP-AMT2
           END-IF.

      *----------------------------------------------------------------*
      * 3. ASSIGN                                                      *
      *----------------------------------------------------------------*
       3000-ASSIGN.
           PERFORM 8100-READ-TICKET
           IF WS-CMP-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT CMP-PENDING
               DISPLAY 'TICKET IS NOT PENDING.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'ASSIGN TO USER: ' WITH NO ADVANCING
           ACCEPT WS-IN-USER
           MOVE FUNCTION UPPER-CASE(WS-IN-USER) TO CMP-ASSIGNED-TO
           REWRITE CMP-RECORD
           MOVE 'TICKET ASSGN' TO AU-ACTION
           MOVE CMP-ASSIGNED-TO TO AU-DETAIL
           PERFORM 8900-AUDIT
           DISPLAY 'TICKET ' CMP-NO ' ASSIGNED TO ' CMP-ASSIGNED-TO '.'.

      *----------------------------------------------------------------*
      * 4. RESOLVE / REJECT                                            *
      *----------------------------------------------------------------*
       4000-RESOLVE.
           PERFORM 8100-READ-TICKET
           IF WS-CMP-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT CMP-PENDING
               DISPLAY 'TICKET IS NOT PENDING.'
               EXIT PARAGRAPH
           END-IF
           IF CMP-LOGGED-BY = SES-USER-ID AND NOT SES-CAN-APPROVE
               DISPLAY 'A TELLER CANNOT RESOLVE A TICKET THEY LOGGED.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'R=RESOLVE J=REJECT : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACTION
           MOVE FUNCTION UPPER-CASE(WS-IN-ACTION) TO WS-IN-ACTION
           DISPLAY 'RESOLUTION NOTE    : ' WITH NO ADVANCING
           ACCEPT WS-IN-TEXT
           IF (WS-IN-ACTION NOT = 'R' AND WS-IN-ACTION NOT = 'J')
              OR WS-IN-TEXT = SPACES
               DISPLAY 'NEED R OR J AND A RESOLUTION NOTE.'
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO CMP-REFUND-AMT CMP-COMP-AMT
           IF WS-IN-ACTION = 'R'
               IF CMP-ATM-FAILED
                   PERFORM 4100-ATM-REFUND
               ELSE
                   PERFORM 4200-GOODWILL
               END-IF
               IF WS-CONFIRM = 'X'
                   EXIT PARAGRAPH
               END-IF
               SET CMP-RESOLVED TO TRUE
               MOVE 'TICKET RSLVD' TO AU-ACTION
           ELSE
               SET CMP-REJECTED TO TRUE
               MOVE 'TICKET REJCT' TO AU-ACTION
           END-IF
           MOVE WS-IN-TEXT   TO CMP-RESOLUTION
           MOVE WS-CURR-DATE TO CMP-RESOLVED-DATE
           MOVE SES-USER-ID  TO CMP-RESOLVED-BY
           REWRITE CMP-RECORD
           MOVE CMP-COMP-AMT TO WS-DISP-AMT
           MOVE SPACES TO AU-DETAIL
           STRING 'COMPENSATION ' FUNCTION TRIM(WS-DISP-AMT)
                  DELIMITED BY SIZE INTO AU-DETAIL
           PERFORM 8900-AUDIT
           IF CMP-RESOLVED
               DISPLAY 'TICKET ' CMP-NO ' RESOLVED.'
           ELSE
               DISPLAY 'TICKET ' CMP-NO ' REJECTED.'
           END-IF.

      *    REFUND THE DISPUTED AMOUNT (CONTRA ATM CASH, WHERE THE
      *    UNDISPENSED NOTES SHOW UP) PLUS RS 100 FOR EVERY DAY PAST
      *    T+5 (CONTRA COMPENSATION EXPENSE). WS-CONFIRM = 'X' ON
      *    FAILURE.
       4100-ATM-REFUND.
           MOVE SPACE TO WS-CONFIRM
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE CMP-TXN-DATE  TO DU-DATE-1
           MOVE WS-CURR-DATE  TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           COMPUTE WS-LATE-DAYS = DU-RESULT-DAYS - WS-ATM-TAT-DAYS
           IF WS-LATE-DAYS < ZERO
               MOVE ZERO TO WS-LATE-DAYS
           END-IF
           COMPUTE CMP-COMP-AMT = WS-LATE-DAYS * WS-ATM-COMP-PER-DAY
           MOVE CMP-AMOUNT    TO WS-DISP-AMT
           MOVE CMP-COMP-AMT  TO WS-DISP-AMT2
           MOVE WS-LATE-DAYS  TO WS-DISP-DAYS
           DISPLAY 'REFUND ' FUNCTION TRIM(WS-DISP-AMT) ' + '
                   FUNCTION TRIM(WS-DISP-DAYS) ' DAY(S) LATE x 100 = '
                   FUNCTION TRIM(WS-DISP-AMT2)
           DISPLAY 'CONFIRM (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
               MOVE 'X' TO WS-CONFIRM
               EXIT PARAGRAPH
           END-IF
           MOVE CMP-AMOUNT TO WS-AMOUNT
           MOVE GLC-ATM-CASH TO PP-CONTRA-GL
           MOVE 'ATM FAILED TXN REFUND' TO PP-NARRATION
           PERFORM 8200-CREDIT
           IF WS-CONFIRM = 'X'
               EXIT PARAGRAPH
           END-IF
           MOVE CMP-AMOUNT TO CMP-REFUND-AMT
           IF CMP-COMP-AMT > ZERO
               MOVE CMP-COMP-AMT TO WS-AMOUNT
               MOVE ZERO TO PP-CONTRA-GL
               MOVE 'COMPENSATION - DELAYED REFUND' TO PP-NARRATION
               PERFORM 8200-CREDIT
               IF WS-CONFIRM = 'X'
                   MOVE ZERO TO CMP-COMP-AMT
                   MOVE SPACE TO WS-CONFIRM
               END-IF
           END-IF.

       4200-GOODWILL.
           MOVE SPACE TO WS-CONFIRM
           IF NOT SES-CAN-APPROVE
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'GOODWILL COMPENSATION (BLANK = NONE): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-AMT
           IF WS-IN-AMT = SPACES
               EXIT PARAGRAPH
           END-IF
           IF FUNCTION TEST-NUMVAL(WS-IN-AMT) NOT = ZERO
               DISPLAY 'AMOUNT MUST BE A NUMBER.'
               MOVE 'X' TO WS-CONFIRM
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-AMOUNT = FUNCTION NUMVAL(WS-IN-AMT)
           IF WS-AMOUNT > WS-MAX-GOODWILL
               DISPLAY 'GOODWILL COMPENSATION IS CAPPED AT 10,000.'
               MOVE 'X' TO WS-CONFIRM
               EXIT PARAGRAPH
           END-IF
           IF WS-AMOUNT = ZERO
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO PP-CONTRA-GL
           MOVE 'GOODWILL COMPENSATION' TO PP-NARRATION
           PERFORM 8200-CREDIT
           IF WS-CONFIRM NOT = 'X'
               MOVE WS-AMOUNT TO CMP-COMP-AMT
           END-IF.

      *----------------------------------------------------------------*
      * 5. FEEDBACK                                                    *
      *----------------------------------------------------------------*
       5000-FEEDBACK.
           PERFORM 8100-READ-TICKET
           IF WS-CMP-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT CMP-RESOLVED AND NOT CMP-REJECTED
               DISPLAY 'TICKET IS NOT AWAITING CUSTOMER FEEDBACK.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'CUSTOMER SATISFIED (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) = 'Y'
               SET CMP-CLOSED TO TRUE
               MOVE 'TICKET CLOSE' TO AU-ACTION
               DISPLAY 'TICKET ' CMP-NO ' CLOSED.'
           ELSE
      *        NOT SATISFIED: BACK OPEN ONE LEVEL UP, 7 MORE DAYS
               SET CMP-ESCALATED TO TRUE
               IF CMP-LEVEL < 3
                   ADD 1 TO CMP-LEVEL
               END-IF
               ADD 1 TO CMP-REOPEN-CNT
               MOVE WS-CURR-DATE TO CMP-ESCALATED-DATE DU-DATE-1
               SET DU-ADD-DAYS TO TRUE
               MOVE WS-REOPEN-SLA TO DU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DU-RESULT-DATE TO CMP-DUE-DATE
               MOVE 'TICKET REOPN' TO AU-ACTION
               DISPLAY 'TICKET ' CMP-NO ' REOPENED AT LEVEL '
                       CMP-LEVEL ' - DUE ' CMP-DUE-DATE '.'
           END-IF
           REWRITE CMP-RECORD
           MOVE SPACES TO AU-DETAIL
           PERFORM 8900-AUDIT.

      *----------------------------------------------------------------*
      * 6. PENDING LIST                                                *
      *----------------------------------------------------------------*
       6000-PENDING.
           MOVE ZERO TO CMP-NO WS-CNT
           START CMP-FILE KEY IS NOT LESS THAN CMP-NO
               INVALID KEY
                   DISPLAY 'NO TICKETS.'
                   EXIT PARAGRAPH
           END-START
           DISPLAY 'TICKET   CAT  LVL OPENED   DUE      AGE  FLAGS'
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ CMP-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N' AND CMP-PENDING
                   ADD 1 TO WS-CNT
                   PERFORM 6100-PENDING-LINE
               END-IF
           END-PERFORM
           DISPLAY WS-CNT ' PENDING TICKET(S).'.

       6100-PENDING-LINE.
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE CMP-OPEN-DATE TO DU-DATE-1
           MOVE WS-CURR-DATE  TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DAYS TO WS-DISP-DAYS
           MOVE SPACES TO WS-FLAGS
           IF CMP-DUE-DATE < WS-CURR-DATE
               MOVE 'OVERDUE' TO WS-FLAGS
           END-IF
           IF DU-RESULT-DAYS > WS-OMBUDSMAN-DAYS AND CMP-COMPLAINT
               MOVE 'OMBUDSMAN' TO WS-FLAGS(9:)
           END-IF
           DISPLAY CMP-NO ' ' CMP-CATEGORY '  ' CMP-LEVEL '  '
                   CMP-OPEN-DATE ' ' CMP-DUE-DATE ' ' WS-DISP-DAYS
                   '  ' WS-FLAGS.

      *----------------------------------------------------------------*
      * COMMON                                                         *
      *----------------------------------------------------------------*
       8000-NEXT-NO.
           MOVE ZERO TO WS-NEXT-NO
           MOVE 99999999 TO CMP-NO
           START CMP-FILE KEY IS LESS THAN OR EQUAL CMP-NO
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ CMP-FILE PREVIOUS RECORD
                       AT END CONTINUE
                       NOT AT END MOVE CMP-NO TO WS-NEXT-NO
                   END-READ
           END-START
           ADD 1 TO WS-NEXT-NO.

       8100-READ-TICKET.
           DISPLAY 'TICKET NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-NO
           MOVE WS-IN-NO TO CMP-NO
           READ CMP-FILE
               INVALID KEY
                   DISPLAY 'TICKET ' WS-IN-NO ' NOT FOUND.'
           END-READ.

      *    CREDIT WS-AMOUNT TO THE TICKET'S ACCOUNT (TYPE CM); THE
      *    CALLER SETS PP-CONTRA-GL AND PP-NARRATION FIRST.
       8200-CREDIT.
           SET PP-CREDIT     TO TRUE
           MOVE CMP-ACCT-NO  TO PP-ACCT-NO
           MOVE WS-AMOUNT    TO PP-AMOUNT
           MOVE 'CM'         TO PP-TRAN-TYPE
           MOVE ZERO         TO PP-REF-ACCT
           MOVE 'Y'          TO PP-SYSTEM-TXN
           MOVE SPACE        TO PP-IGNORE-MIN-BAL
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'CREDIT TO ' CMP-ACCT-NO ' FAILED, RC='
                       PP-RETURN-CODE
               MOVE 'X' TO WS-CONFIRM
           END-IF.

       8900-AUDIT.
           MOVE 'CMPMNT' TO AU-PROGRAM
           MOVE CMP-NO   TO AU-KEY
           CALL 'AUDITLOG' USING AUD-PARMS.
