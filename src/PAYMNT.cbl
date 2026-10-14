       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYMNT.
      *================================================================*
      * PROGRAM  : PAYMNT                                              *
      * PURPOSE  : OUTWARD PAYMENTS AT THE BRANCH - NEFT, RTGS, IMPS   *
      *            AND UPI - PLUS STATUS INQUIRY AND A PAYMENT LIST.   *
      *            MODE RULES:                                         *
      *              NEFT - ANY AMOUNT, SETTLED IN BATCH (PAYBATCH)    *
      *              RTGS - MIN 2 LAKH, 07:00-18:00 ONLY, BATCH SETTLED*
      *              IMPS - MAX 5 LAKH, INSTANT                        *
      *              UPI  - MAX 1 LAKH, VPA INSTEAD OF ACCOUNT, INSTANT*
      *            CHARGES (PLUS 18% GST) BY MODE AND SLAB, SEE        *
      *            3000-WORK-OUT-CHARGE. FUNDS FOR AMOUNT + CHARGES    *
      *            ARE CHECKED BEFORE ANYTHING IS POSTED.              *
      * GL       : DR CUSTOMER CR 1302 PAYMENT SETTLEMENT (AMOUNT)     *
      *            DR CUSTOMER CR 4101, THEN DR 4101 CR 2501 (GST)     *
      * FILES    : PAYREG (INDEXED, I-O), PAYOUT_<DATE> (EXTEND)       *
      * CALLS    : ACCTPOST, GLPOST, AUDITLOG                          *
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
           SELECT OUT-FILE ASSIGN TO WS-OUT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  PAY-FILE.
           COPY PAYREC.
       FD  OUT-FILE.
       01  OUT-LINE                    PIC X(120).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY GLPARM.
           COPY GLCODES.
           COPY AUDPARM.

       01  WS-PAY-FS                   PIC X(02).
           88  PAY-FS-OK                         VALUE '00'.
           88  PAY-FS-NOFILE                     VALUE '35'.
       01  WS-OUT-FS                   PIC X(02).
       01  WS-OUT-FILENAME             PIC X(40).

       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
           88  CHOICE-BACK                       VALUE '9'.
       01  WS-CONFIRM                  PIC X(01).
           88  CONFIRM-YES                       VALUES 'Y' 'y'.
       01  WS-VALID-SW                 PIC X(01).
           88  INPUT-VALID                       VALUE 'Y'.
           88  INPUT-INVALID                     VALUE 'N'.

       01  WS-IN.
           05  WS-IN-MODE              PIC X(01).
           05  WS-IN-ACCT-NO           PIC 9(10).
           05  WS-IN-BENE-NAME         PIC X(35).
           05  WS-IN-BENE-ACCT         PIC X(18).
           05  WS-IN-IFSC              PIC X(11).
           05  WS-IN-VPA               PIC X(30).
           05  WS-IN-AMOUNT            PIC 9(11)V99.
           05  WS-IN-REMARKS           PIC X(30).
           05  WS-IN-REF               PIC X(16).

       01  WS-CHARGE                   PIC 9(05)V99.
       01  WS-GST                      PIC 9(05)V99.
       01  WS-TOTAL-DEBIT              PIC 9(11)V99.
       01  WS-GST-RATE                 PIC 9V99 VALUE 0.18.
       01  WS-I                        PIC 9(02).
       01  WS-AT-POS                   PIC 9(02).
       01  WS-AT-COUNT                 PIC 9(02).
       01  WS-LEN                      PIC 9(02).
       01  WS-NEXT-SEQ                 PIC 9(07).
       01  WS-REF-PREFIX               PIC X(09).
       01  WS-LIST-CNT                 PIC 9(05).

      * MODE LIMITS
       01  WS-RTGS-MIN                 PIC 9(11)V99 VALUE 200000.00.
       01  WS-IMPS-MAX                 PIC 9(11)V99 VALUE 500000.00.
       01  WS-UPI-MAX                  PIC 9(11)V99 VALUE 100000.00.
       01  WS-RTGS-OPEN                PIC 9(06) VALUE 070000.
       01  WS-RTGS-CLOSE               PIC 9(06) VALUE 180000.

       01  WS-DISP-AMT                 PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-CHG                 PIC ZZ,ZZ9.99.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN I-O PAY-FILE
           IF PAY-FS-NOFILE
               OPEN OUTPUT PAY-FILE
               CLOSE PAY-FILE
               OPEN I-O PAY-FILE
           END-IF
           IF NOT PAY-FS-OK
               DISPLAY 'PAYMENT REGISTER OPEN FAILED, FS=' WS-PAY-FS
               GOBACK
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL CHOICE-BACK
               PERFORM 1000-SHOW-MENU
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-SEND-PAYMENT
                   WHEN '2' PERFORM 5000-INQUIRY
                   WHEN '3' PERFORM 6000-LIST-FOR-ACCOUNT
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE PAY-FILE
           GOBACK.

       1000-SHOW-MENU.
           DISPLAY ' '
           DISPLAY '------------- PAYMENTS -------------'
           DISPLAY ' 1. SEND PAYMENT (NEFT / RTGS / IMPS / UPI)'
           DISPLAY ' 2. PAYMENT STATUS'
           DISPLAY ' 3. PAYMENTS OF AN ACCOUNT'
           DISPLAY ' 9. BACK TO MAIN MENU'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT.

      *================================================================*
      * SEND                                                           *
      *================================================================*
       2000-SEND-PAYMENT.
           SET INPUT-VALID TO TRUE
           INITIALIZE WS-IN
           DISPLAY 'MODE (N=NEFT R=RTGS I=IMPS U=UPI): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-MODE
           MOVE FUNCTION UPPER-CASE(WS-IN-MODE) TO WS-IN-MODE
           MOVE WS-IN-MODE TO PAY-MODE
           IF NOT PAY-MODE-VALID
               DISPLAY 'MODE MUST BE N, R, I OR U.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'DEBIT ACCOUNT NO      : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'BENEFICIARY NAME      : ' WITH NO ADVANCING
           ACCEPT WS-IN-BENE-NAME
           IF PAY-UPI
               DISPLAY 'BENEFICIARY UPI ID    : ' WITH NO ADVANCING
               ACCEPT WS-IN-VPA
           ELSE
               DISPLAY 'BENEFICIARY ACCOUNT   : ' WITH NO ADVANCING
               ACCEPT WS-IN-BENE-ACCT
               DISPLAY 'BENEFICIARY IFSC      : ' WITH NO ADVANCING
               ACCEPT WS-IN-IFSC
           END-IF
           DISPLAY 'AMOUNT                : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           DISPLAY 'REMARKS               : ' WITH NO ADVANCING
           ACCEPT WS-IN-REMARKS
           MOVE FUNCTION UPPER-CASE(WS-IN-BENE-NAME) TO WS-IN-BENE-NAME
           MOVE FUNCTION UPPER-CASE(WS-IN-IFSC)      TO WS-IN-IFSC
           MOVE FUNCTION LOWER-CASE(WS-IN-VPA)       TO WS-IN-VPA

           PERFORM 2100-VALIDATE
           IF INPUT-INVALID
               DISPLAY 'PAYMENT NOT SENT.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 3000-WORK-OUT-CHARGE
           COMPUTE WS-TOTAL-DEBIT = WS-IN-AMOUNT + WS-CHARGE + WS-GST

           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'DEBIT ACCOUNT NOT FOUND.'
               EXIT PARAGRAPH
           END-IF
           IF WS-TOTAL-DEBIT > PP-AVAILABLE
               MOVE PP-AVAILABLE TO WS-DISP-AMT
               DISPLAY 'INSUFFICIENT FUNDS FOR AMOUNT + CHARGES. '
                       'AVAILABLE: ' WS-DISP-AMT
               EXIT PARAGRAPH
           END-IF

           MOVE WS-IN-AMOUNT TO WS-DISP-AMT
           DISPLAY 'AMOUNT  : ' WS-DISP-AMT
           MOVE WS-CHARGE TO WS-DISP-CHG
           DISPLAY 'CHARGES : ' WS-DISP-CHG ' + GST ' WITH NO ADVANCING
           MOVE WS-GST TO WS-DISP-CHG
           DISPLAY WS-DISP-CHG
           MOVE WS-TOTAL-DEBIT TO WS-DISP-AMT
           DISPLAY 'TOTAL   : ' WS-DISP-AMT
           DISPLAY 'CONFIRM (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF NOT CONFIRM-YES
               DISPLAY 'PAYMENT CANCELLED.'
               EXIT PARAGRAPH
           END-IF

           PERFORM 2200-NEXT-REFERENCE
           PERFORM 4000-POST-DEBITS
           IF NOT PP-OK
               EXIT PARAGRAPH
           END-IF
           PERFORM 4500-REGISTER-PAYMENT.

       2100-VALIDATE.
           IF WS-IN-BENE-NAME = SPACES
               DISPLAY '- BENEFICIARY NAME IS REQUIRED.'
               SET INPUT-INVALID TO TRUE
           END-IF
           IF WS-IN-AMOUNT NOT > ZERO
               DISPLAY '- AMOUNT MUST BE GREATER THAN ZERO.'
               SET INPUT-INVALID TO TRUE
           END-IF
           EVALUATE TRUE
               WHEN PAY-RTGS AND WS-IN-AMOUNT < WS-RTGS-MIN
                   DISPLAY '- RTGS MINIMUM IS 2,00,000. USE NEFT.'
                   SET INPUT-INVALID TO TRUE
               WHEN PAY-IMPS AND WS-IN-AMOUNT > WS-IMPS-MAX
                   DISPLAY '- IMPS MAXIMUM IS 5,00,000.'
                   SET INPUT-INVALID TO TRUE
               WHEN PAY-UPI AND WS-IN-AMOUNT > WS-UPI-MAX
                   DISPLAY '- UPI MAXIMUM IS 1,00,000.'
                   SET INPUT-INVALID TO TRUE
           END-EVALUATE
           IF PAY-RTGS
               MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
               IF WS-CURR-TIME < WS-RTGS-OPEN
                  OR WS-CURR-TIME >= WS-RTGS-CLOSE
                   DISPLAY '- RTGS RUNS 07:00 TO 18:00. USE NEFT.'
                   SET INPUT-INVALID TO TRUE
               END-IF
           END-IF
           IF PAY-UPI
               PERFORM 2120-VALIDATE-VPA
           ELSE
               PERFORM 2110-VALIDATE-ACCOUNT-IFSC
           END-IF.

      *    ACCOUNT: 9-18 DIGITS. IFSC: 4 LETTERS, '0', 6 LETTERS/DIGITS
       2110-VALIDATE-ACCOUNT-IFSC.
           MOVE ZERO TO WS-LEN
           INSPECT WS-IN-BENE-ACCT TALLYING WS-LEN
                   FOR CHARACTERS BEFORE INITIAL SPACE
           IF WS-LEN < 9
              OR WS-IN-BENE-ACCT(1:WS-LEN) IS NOT NUMERIC
               DISPLAY '- BENEFICIARY ACCOUNT MUST BE 9-18 DIGITS.'
               SET INPUT-INVALID TO TRUE
           END-IF
           IF WS-IN-IFSC(1:4) IS NOT ALPHABETIC-UPPER
              OR WS-IN-IFSC(1:4) = SPACES
              OR WS-IN-IFSC(5:1) NOT = '0'
               DISPLAY '- IFSC MUST LOOK LIKE ABCD0123456.'
               SET INPUT-INVALID TO TRUE
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 11
               IF WS-IN-IFSC(WS-I:1) = SPACE
                   DISPLAY '- IFSC MUST BE 11 CHARACTERS.'
                   SET INPUT-INVALID TO TRUE
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.

      *    VPA: NAME@HANDLE, EXACTLY ONE '@', NO SPACES INSIDE
       2120-VALIDATE-VPA.
           MOVE ZERO TO WS-AT-COUNT WS-AT-POS WS-LEN
           INSPECT WS-IN-VPA TALLYING WS-AT-COUNT FOR ALL '@'
           INSPECT WS-IN-VPA TALLYING WS-LEN
                   FOR CHARACTERS BEFORE INITIAL SPACE
           INSPECT WS-IN-VPA TALLYING WS-AT-POS
                   FOR CHARACTERS BEFORE INITIAL '@'
           IF WS-AT-COUNT NOT = 1
              OR WS-AT-POS < 2
              OR WS-LEN - WS-AT-POS < 3
               DISPLAY '- UPI ID MUST LOOK LIKE NAME@BANK.'
               SET INPUT-INVALID TO TRUE
           END-IF.

       2200-NEXT-REFERENCE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE SPACES TO WS-REF-PREFIX
           STRING WS-IN-MODE WS-CURR-DATE DELIMITED BY SIZE
                  INTO WS-REF-PREFIX
           END-STRING
           MOVE 1 TO WS-NEXT-SEQ
           MOVE SPACES TO PAY-REF
           STRING WS-REF-PREFIX '9999999' DELIMITED BY SIZE
                  INTO PAY-REF
           END-STRING
           START PAY-FILE KEY IS LESS THAN OR EQUAL PAY-REF
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ PAY-FILE PREVIOUS RECORD
                       AT END CONTINUE
                       NOT AT END
                           IF PAY-REF(1:9) = WS-REF-PREFIX
                               COMPUTE WS-NEXT-SEQ =
                                   FUNCTION NUMVAL(PAY-REF(10:7)) + 1
                           END-IF
                   END-READ
           END-START
           MOVE SPACES TO WS-IN-REF
           STRING WS-REF-PREFIX WS-NEXT-SEQ DELIMITED BY SIZE
                  INTO WS-IN-REF
           END-STRING.

      *================================================================*
      * CHARGES (BRANCH TARIFF, BEFORE GST)                            *
      *   NEFT : UP TO 10K 2.50, 1L 5, 2L 15, ABOVE 25                 *
      *   RTGS : UP TO 5L 25, ABOVE 50                                 *
      *   IMPS : UP TO 1K NIL, 1L 5, 5L 15                             *
      *   UPI  : NIL                                                   *
      *================================================================*
       3000-WORK-OUT-CHARGE.
           EVALUATE TRUE
               WHEN PAY-NEFT
                   EVALUATE TRUE
                       WHEN WS-IN-AMOUNT <= 10000
                           MOVE 2.50 TO WS-CHARGE
                       WHEN WS-IN-AMOUNT <= 100000
                           MOVE 5.00 TO WS-CHARGE
                       WHEN WS-IN-AMOUNT <= 200000 MOVE 15.00
                                                   TO WS-CHARGE
                       WHEN OTHER                  MOVE 25.00
                                                   TO WS-CHARGE
                   END-EVALUATE
               WHEN PAY-RTGS
                   IF WS-IN-AMOUNT <= 500000
                       MOVE 25.00 TO WS-CHARGE
                   ELSE
                       MOVE 50.00 TO WS-CHARGE
                   END-IF
               WHEN PAY-IMPS
                   EVALUATE TRUE
                       WHEN WS-IN-AMOUNT <= 1000
                           MOVE ZERO TO WS-CHARGE
                       WHEN WS-IN-AMOUNT <= 100000
                           MOVE 5.00 TO WS-CHARGE
                       WHEN OTHER                  MOVE 15.00
                                                   TO WS-CHARGE
                   END-EVALUATE
               WHEN OTHER
                   MOVE ZERO TO WS-CHARGE
           END-EVALUATE
           COMPUTE WS-GST ROUNDED = WS-CHARGE * WS-GST-RATE.

      *================================================================*
      * POSTINGS                                                       *
      *================================================================*
       4000-POST-DEBITS.
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           MOVE WS-IN-AMOUNT  TO PP-AMOUNT
           MOVE 'PO'          TO PP-TRAN-TYPE
           STRING PAY-MODE ' ' WS-IN-REF(10:7) ' '
                  WS-IN-BENE-NAME
                  DELIMITED BY SIZE INTO PP-NARRATION
           END-STRING
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'DEBIT FAILED, RC=' PP-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           IF WS-CHARGE = ZERO
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           COMPUTE PP-AMOUNT = WS-CHARGE + WS-GST
           MOVE 'CH'          TO PP-TRAN-TYPE
           MOVE 'Y'           TO PP-SYSTEM-TXN
           STRING 'PAYMENT CHARGES ' WS-IN-REF(10:7)
                  DELIMITED BY SIZE INTO PP-NARRATION
           END-STRING
           CALL 'ACCTPOST' USING POST-PARMS
           IF PP-OK
               MOVE GLC-FEE-INCOME  TO GP-DR-GL
               MOVE GLC-GST-PAYABLE TO GP-CR-GL
               MOVE WS-GST          TO GP-AMOUNT
               MOVE WS-IN-ACCT-NO   TO GP-REF
               MOVE 'GST ON PAYMENT CHARGES' TO GP-NARRATION
               IF WS-GST > ZERO
                   CALL 'GLPOST' USING GL-PARMS
               END-IF
           ELSE
      *        FUNDS WERE CHECKED FOR AMOUNT + CHARGES, SO THIS IS
      *        UNEXPECTED; THE PAYMENT ITSELF STILL GOES
               DISPLAY 'WARNING: CHARGES NOT RECOVERED, RC='
                       PP-RETURN-CODE
               MOVE ZERO TO WS-CHARGE WS-GST
               SET PP-OK TO TRUE
           END-IF.

       4500-REGISTER-PAYMENT.
           INITIALIZE PAY-RECORD
           MOVE WS-IN-REF       TO PAY-REF
           MOVE WS-IN-MODE      TO PAY-MODE
           SET PAY-OUTWARD      TO TRUE
           MOVE WS-IN-ACCT-NO   TO PAY-OUR-ACCT
           MOVE WS-IN-BENE-NAME TO PAY-OTHER-NAME
           MOVE WS-IN-BENE-ACCT TO PAY-OTHER-ACCT
           MOVE WS-IN-IFSC      TO PAY-OTHER-IFSC
           MOVE WS-IN-VPA       TO PAY-OTHER-VPA
           MOVE WS-IN-AMOUNT    TO PAY-AMOUNT
           MOVE WS-CHARGE       TO PAY-CHARGE
           MOVE WS-GST          TO PAY-GST
           MOVE WS-IN-REMARKS   TO PAY-REMARKS
           MOVE WS-CURR-DATE    TO PAY-CREATED-DATE
           MOVE WS-CURR-TIME    TO PAY-CREATED-TIME
           IF PAY-INSTANT
               SET PAY-SETTLED  TO TRUE
               MOVE WS-CURR-DATE TO PAY-SETTLED-DATE
               STRING 'CB' WS-IN-REF DELIMITED BY SIZE INTO PAY-UTR
               END-STRING
           ELSE
               SET PAY-PENDING  TO TRUE
               PERFORM 4600-WRITE-OUTWARD-FILE
           END-IF
           WRITE PAY-RECORD
               INVALID KEY
                   DISPLAY 'CRITICAL: PAYMENT REGISTER WRITE FAILED FS='
                           WS-PAY-FS ' REF ' WS-IN-REF
                   EXIT PARAGRAPH
           END-WRITE
           MOVE 'PAYMNT'    TO AU-PROGRAM
           MOVE 'PAY-OUT'   TO AU-ACTION
           MOVE WS-IN-REF   TO AU-KEY
           MOVE WS-IN-AMOUNT TO WS-DISP-AMT
           STRING PAY-MODE ' ' FUNCTION TRIM(WS-DISP-AMT) ' FROM '
                  WS-IN-ACCT-NO DELIMITED BY SIZE INTO AU-DETAIL
           END-STRING
           CALL 'AUDITLOG' USING AUD-PARMS
           IF PAY-INSTANT
               DISPLAY 'PAYMENT SENT. REFERENCE ' PAY-REF
                       '  UTR ' PAY-UTR
           ELSE
               DISPLAY 'PAYMENT QUEUED FOR SETTLEMENT. REFERENCE '
                       PAY-REF
           END-IF.

      *    NEFT / RTGS GO TO THE DAY'S OUTWARD FILE FOR THE CLEARING
      *    HOUSE: REF|MODE|AMOUNT|IFSC|ACCOUNT|NAME
       4600-WRITE-OUTWARD-FILE.
           MOVE SPACES TO WS-OUT-FILENAME
           STRING 'data/PAYOUT_' WS-CURR-DATE '.DAT'
                  DELIMITED BY SIZE INTO WS-OUT-FILENAME
           END-STRING
           OPEN EXTEND OUT-FILE
           IF WS-OUT-FS = '35'
               OPEN OUTPUT OUT-FILE
           END-IF
           MOVE WS-IN-AMOUNT TO WS-DISP-AMT
           MOVE SPACES TO OUT-LINE
           STRING WS-IN-REF '|' WS-IN-MODE '|'
                  FUNCTION TRIM(WS-DISP-AMT) '|' WS-IN-IFSC '|'
                  FUNCTION TRIM(WS-IN-BENE-ACCT) '|'
                  FUNCTION TRIM(WS-IN-BENE-NAME)
                  DELIMITED BY SIZE INTO OUT-LINE
           END-STRING
           WRITE OUT-LINE
           CLOSE OUT-FILE.

      *================================================================*
      * INQUIRY / LIST                                                 *
      *================================================================*
       5000-INQUIRY.
           DISPLAY 'PAYMENT REFERENCE: ' WITH NO ADVANCING
           ACCEPT WS-IN-REF
           MOVE FUNCTION UPPER-CASE(WS-IN-REF) TO PAY-REF
           READ PAY-FILE
               INVALID KEY
                   DISPLAY 'NO SUCH PAYMENT.'
                   EXIT PARAGRAPH
           END-READ
           DISPLAY '----------------------------------------'
           DISPLAY 'REFERENCE   : ' PAY-REF '   MODE ' PAY-MODE
                   '   ' PAY-DIRECTION
           DISPLAY 'OUR ACCOUNT : ' PAY-OUR-ACCT
           DISPLAY 'OTHER PARTY : ' FUNCTION TRIM(PAY-OTHER-NAME)
           IF PAY-UPI
               DISPLAY 'UPI ID      : ' PAY-OTHER-VPA
           ELSE
               DISPLAY 'ACCOUNT/IFSC: ' FUNCTION TRIM(PAY-OTHER-ACCT)
                       ' / ' PAY-OTHER-IFSC
           END-IF
           MOVE PAY-AMOUNT TO WS-DISP-AMT
           DISPLAY 'AMOUNT      : ' WS-DISP-AMT
           EVALUATE TRUE
               WHEN PAY-PENDING
                   DISPLAY 'STATUS      : PENDING SETTLEMENT'
               WHEN PAY-SETTLED
                   DISPLAY 'STATUS      : SETTLED ' PAY-SETTLED-DATE
                           '  UTR ' PAY-UTR
               WHEN PAY-RETURNED
                   DISPLAY 'STATUS      : RETURNED - '
                           FUNCTION TRIM(PAY-RETURN-REASON)
           END-EVALUATE
           DISPLAY '----------------------------------------'.

       6000-LIST-FOR-ACCOUNT.
           DISPLAY 'ACCOUNT NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           MOVE ZERO TO WS-LIST-CNT
           MOVE LOW-VALUES TO PAY-REF
           START PAY-FILE KEY IS NOT LESS THAN PAY-REF
               INVALID KEY
                   DISPLAY 'NO PAYMENTS.'
                   EXIT PARAGRAPH
           END-START
           DISPLAY 'REFERENCE        M D S          AMOUNT  OTHER PARTY'
           PERFORM FOREVER
               READ PAY-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF PAY-OUR-ACCT = WS-IN-ACCT-NO
                   ADD 1 TO WS-LIST-CNT
                   MOVE PAY-AMOUNT TO WS-DISP-AMT
                   DISPLAY PAY-REF ' ' PAY-MODE ' ' PAY-DIRECTION ' '
                           PAY-STATUS ' ' WS-DISP-AMT '  '
                           PAY-OTHER-NAME(1:20)
               END-IF
           END-PERFORM
           IF WS-LIST-CNT = ZERO
               DISPLAY 'NO PAYMENTS FOR THIS ACCOUNT.'
           END-IF.

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
