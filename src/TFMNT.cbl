       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFMNT.
      *================================================================*
      * PROGRAM  : TFMNT                                               *
      * PURPOSE  : TRADE FINANCE - BANK GUARANTEES AND LETTERS OF      *
      *            CREDIT                                              *
      *              1 - ISSUE A BG OR LC                  (MANAGER)   *
      *              2 - LC DRAWING (DOCUMENTS PAID)       (MANAGER)   *
      *              3 - BG INVOCATION (CLAIM PAID)        (MANAGER)   *
      *              4 - CANCEL / BG RETURNED UNUSED       (MANAGER)   *
      *              5 - INSTRUMENT INQUIRY                            *
      *              6 - INSTRUMENTS OF AN ACCOUNT                     *
      *            ISSUE: MARGIN (MIN 10% FOR A BG, ANY FOR AN LC) IS  *
      *            MOVED FROM THE APPLICANT'S ACCOUNT TO GL 2905;      *
      *            COMMISSION 1.5% P.A. (BG) / 2% P.A. (LC) ON THE     *
      *            PERIOD, MINIMUM 1,000, PLUS 18% GST, TO GL 4105.    *
      *            A BG STAYS CLAIMABLE FOR 30 DAYS AFTER EXPIRY.      *
      *            PAYMENT (DRAWING / INVOCATION) USES THE MARGIN      *
      *            FIRST AND DEBITS THE ACCOUNT FOR THE REST; IF THE   *
      *            ACCOUNT CANNOT COVER IT THE PAYMENT IS REFUSED      *
      *            HERE (A DEVOLVEMENT IS A CREDIT DECISION).          *
      *              MARGIN PART   DR 2905      CR 1302 SETTLEMENT     *
      *              ACCOUNT PART  DR CUSTOMER  CR 1302 (ACCTPOST, TF) *
      * FILES    : TFMAST (I-O)                                        *
      * CALLS    : ACCTPOST, GLPOST, DATEUTIL, AUDITLOG                *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TF-FILE ASSIGN TO 'data/TFMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS TF-NO FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  TF-FILE.
           COPY TFREC.

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-CHOICE                   PIC X(01).
       01  WS-EXIT-SW                  PIC X(01) VALUE 'N'.
           88  EXIT-REQUESTED                    VALUE 'Y'.
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-OK-SW                    PIC X(01).
           88  INPUT-OK                          VALUE 'Y'.
       01  WS-TODAY                    PIC 9(08).
       01  WS-IN.
           05  WS-IN-ACCT-NO           PIC 9(10).
           05  WS-IN-TYPE              PIC X(01).
           05  WS-IN-BENEFICIARY       PIC X(40).
           05  WS-IN-AMOUNT            PIC 9(11)V99.
           05  WS-IN-EXPIRY            PIC 9(08).
           05  WS-IN-MARGIN-PCT        PIC 9(03).
           05  WS-IN-TF-NO             PIC 9(08).
       01  WS-CONFIRM                  PIC X(01).
       01  WS-NEXT-NO                  PIC 9(08).
       01  WS-COUNT                    PIC 9(04).
       01  WS-DAYS                     PIC S9(07).
       01  WS-MIN-EXPIRY               PIC 9(08).
       01  WS-MAX-EXPIRY               PIC 9(08).
       01  WS-RATE                     PIC 9V9(04).
       01  WS-COMMISSION               PIC 9(09)V99.
       01  WS-MIN-COMMISSION           PIC 9(09)V99 VALUE 1000.
       01  WS-GST                      PIC 9(09)V99.
       01  WS-GST-RATE                 PIC 9V99 VALUE 0.18.
       01  WS-MARGIN                   PIC 9(11)V99.
       01  WS-AVAILABLE                PIC 9(11)V99.
       01  WS-FROM-MARGIN              PIC 9(11)V99.
       01  WS-FROM-ACCOUNT             PIC 9(11)V99.
       01  WS-CLAIM-DAYS               PIC 9(03) VALUE 30.
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-AMT2                     PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-TYPE-NAME                PIC X(09).
       01  WS-STATUS-NAME              PIC X(11).
           COPY POSTPARM.
           COPY GLPARM.
           COPY GLCODES.
           COPY DATEPARM.
           COPY SESSION.
           COPY AUDPARM.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           OPEN I-O TF-FILE
           IF WS-FS = '35'
               OPEN OUTPUT TF-FILE
               CLOSE TF-FILE
               OPEN I-O TF-FILE
           END-IF
           IF WS-FS NOT = '00'
               DISPLAY 'TRADE FINANCE FILE OPEN FAILED, FS=' WS-FS
               GOBACK
           END-IF
           MOVE 'N' TO WS-EXIT-SW
           PERFORM UNTIL EXIT-REQUESTED
               DISPLAY ' '
               DISPLAY '------- TRADE FINANCE (BG / LC) -------'
               IF SES-CAN-APPROVE
                   DISPLAY ' 1. ISSUE A BANK GUARANTEE / LC'
                   DISPLAY ' 2. LC DRAWING'
                   DISPLAY ' 3. BG INVOCATION'
                   DISPLAY ' 4. CANCEL / BG RETURNED UNUSED'
               END-IF
               DISPLAY ' 5. INSTRUMENT INQUIRY'
               DISPLAY ' 6. INSTRUMENTS OF AN ACCOUNT'
               DISPLAY ' 9. BACK'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               PERFORM 8990-GUARD-END-OF-INPUT
               IF WS-CHOICE >= '1' AND WS-CHOICE <= '4'
                  AND NOT SES-CAN-APPROVE
                   DISPLAY 'NEEDS A MANAGER.'
                   MOVE SPACE TO WS-CHOICE
               END-IF
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 1000-ISSUE
                   WHEN '2' PERFORM 2000-LC-DRAWING
                   WHEN '3' PERFORM 3000-BG-INVOCATION
                   WHEN '4' PERFORM 4000-CANCEL
                   WHEN '5' PERFORM 5000-INQUIRY
                   WHEN '6' PERFORM 6000-ACCOUNT-LIST
                   WHEN '9' SET EXIT-REQUESTED TO TRUE
                   WHEN SPACE CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE TF-FILE
           GOBACK.

      *================================================================*
      * 1 - ISSUE                                                      *
      *================================================================*
       1000-ISSUE.
           MOVE 'Y' TO WS-OK-SW
           DISPLAY 'TYPE (G=GUARANTEE L=LC) : ' WITH NO ADVANCING
           ACCEPT WS-IN-TYPE
           MOVE FUNCTION UPPER-CASE(WS-IN-TYPE) TO WS-IN-TYPE
           DISPLAY 'APPLICANT ACCOUNT       : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'BENEFICIARY             : ' WITH NO ADVANCING
           ACCEPT WS-IN-BENEFICIARY
           DISPLAY 'AMOUNT                  : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           DISPLAY 'EXPIRY DATE (YYYYMMDD)  : ' WITH NO ADVANCING
           ACCEPT WS-IN-EXPIRY
           DISPLAY 'MARGIN %                : ' WITH NO ADVANCING
           ACCEPT WS-IN-MARGIN-PCT
           PERFORM 1100-VALIDATE
           IF NOT INPUT-OK
               EXIT PARAGRAPH
           END-IF
           PERFORM 1200-PRICE
           COMPUTE WS-MARGIN ROUNDED =
                   WS-IN-AMOUNT * WS-IN-MARGIN-PCT / 100
           MOVE WS-MARGIN TO ED-AMT
           COMPUTE ED-AMT2 = WS-COMMISSION + WS-GST
           DISPLAY 'MARGIN ' ED-AMT '  COMMISSION + GST ' ED-AMT2
           DISPLAY 'DEBIT ACCOUNT AND ISSUE (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
               DISPLAY 'NOT ISSUED.'
               EXIT PARAGRAPH
           END-IF
      *    MARGIN FIRST - IF THE ACCOUNT CANNOT PAY, NOTHING IS ISSUED
           IF WS-MARGIN > ZERO
               INITIALIZE POST-PARMS
               SET PP-DEBIT TO TRUE
               MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
               MOVE WS-MARGIN     TO PP-AMOUNT
               MOVE 'TF'          TO PP-TRAN-TYPE
               MOVE 'MARGIN FOR BG / LC' TO PP-NARRATION
               CALL 'ACCTPOST' USING POST-PARMS
               IF NOT PP-OK
                   DISPLAY 'MARGIN NOT AVAILABLE, RC=' PP-RETURN-CODE
                           ' - NOT ISSUED.'
                   EXIT PARAGRAPH
               END-IF
           END-IF
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           COMPUTE PP-AMOUNT = WS-COMMISSION + WS-GST
           MOVE 'CH'          TO PP-TRAN-TYPE
           MOVE GLC-TRADE-COMMISSION TO PP-CONTRA-GL
           MOVE 'BG / LC COMMISSION + GST' TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
      *        GIVE THE MARGIN BACK BEFORE GIVING UP
               PERFORM 1300-REFUND-MARGIN
               DISPLAY 'COMMISSION NOT RECOVERABLE, RC=' PP-RETURN-CODE
                       ' - NOT ISSUED.'
               EXIT PARAGRAPH
           END-IF
           MOVE GLC-TRADE-COMMISSION TO GP-DR-GL
           MOVE GLC-GST-PAYABLE      TO GP-CR-GL
           MOVE WS-GST               TO GP-AMOUNT
           MOVE 'GST ON BG / LC COMMISSION' TO GP-NARRATION
           PERFORM 7400-GL
           PERFORM 1400-NEXT-NO
           INITIALIZE TF-RECORD
           MOVE WS-NEXT-NO        TO TF-NO
           MOVE WS-IN-TYPE        TO TF-TYPE
           MOVE WS-IN-ACCT-NO     TO TF-ACCT-NO
           MOVE WS-IN-BENEFICIARY TO TF-BENEFICIARY
           MOVE WS-IN-AMOUNT      TO TF-AMOUNT
           MOVE WS-TODAY          TO TF-ISSUE-DATE
           MOVE WS-IN-EXPIRY      TO TF-EXPIRY-DATE TF-CLAIM-END
           IF TF-GUARANTEE
               SET DU-ADD-DAYS TO TRUE
               MOVE WS-IN-EXPIRY  TO DU-DATE-1
               MOVE WS-CLAIM-DAYS TO DU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DU-RESULT-DATE TO TF-CLAIM-END
           END-IF
           MOVE WS-IN-MARGIN-PCT  TO TF-MARGIN-PCT
           MOVE WS-MARGIN         TO TF-MARGIN-HELD
           MOVE WS-COMMISSION     TO TF-COMMISSION
           SET TF-ACTIVE          TO TRUE
           MOVE SES-USER-ID       TO TF-ISSUED-BY
           WRITE TF-RECORD
               INVALID KEY
                   DISPLAY 'CRITICAL: WRITE FAILED, FS=' WS-FS
                   EXIT PARAGRAPH
           END-WRITE
           PERFORM 8100-NAMES
           DISPLAY FUNCTION TRIM(WS-TYPE-NAME) ' ' TF-NO ' ISSUED.'
                   ' VALID TILL ' TF-EXPIRY-DATE
           IF TF-GUARANTEE
               DISPLAY 'CLAIMS ACCEPTED UNTIL ' TF-CLAIM-END
           END-IF
           MOVE 'ISSUE' TO AU-ACTION
           PERFORM 9000-AUDIT.

       1100-VALIDATE.
           IF WS-IN-TYPE NOT = 'G' AND WS-IN-TYPE NOT = 'L'
               DISPLAY 'TYPE MUST BE G OR L.'
               MOVE 'N' TO WS-OK-SW
           END-IF
           IF WS-IN-BENEFICIARY = SPACES
               DISPLAY 'BENEFICIARY IS REQUIRED.'
               MOVE 'N' TO WS-OK-SW
           END-IF
           IF WS-IN-AMOUNT < 10000
               DISPLAY 'MINIMUM AMOUNT IS 10,000.'
               MOVE 'N' TO WS-OK-SW
           END-IF
           IF WS-IN-MARGIN-PCT > 100
              OR (WS-IN-TYPE = 'G' AND WS-IN-MARGIN-PCT < 10)
               DISPLAY 'MARGIN MUST BE 10-100% FOR A BG, 0-100% FOR '
                       'AN LC.'
               MOVE 'N' TO WS-OK-SW
           END-IF
           SET DU-ADD-DAYS TO TRUE
           MOVE WS-TODAY TO DU-DATE-1
           MOVE 30 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO WS-MIN-EXPIRY
           SET DU-ADD-MONTHS TO TRUE
           MOVE 120 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO WS-MAX-EXPIRY
           SET DU-VALIDATE TO TRUE
           MOVE WS-IN-EXPIRY TO DU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DU-OK OR WS-IN-EXPIRY < WS-MIN-EXPIRY
              OR WS-IN-EXPIRY > WS-MAX-EXPIRY
               DISPLAY 'EXPIRY MUST BE 30 DAYS TO 10 YEARS AHEAD.'
               MOVE 'N' TO WS-OK-SW
           END-IF
           IF NOT INPUT-OK
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'APPLICANT ACCOUNT NOT FOUND OR NOT ACTIVE.'
               MOVE 'N' TO WS-OK-SW
           END-IF.

      *    COMMISSION ON THE NUMBER OF DAYS TO EXPIRY (PRO-RATA YEAR)
       1200-PRICE.
           IF WS-IN-TYPE = 'G'
               MOVE 0.015 TO WS-RATE
           ELSE
               MOVE 0.02 TO WS-RATE
           END-IF
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE WS-TODAY     TO DU-DATE-1
           MOVE WS-IN-EXPIRY TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DAYS TO WS-DAYS
           COMPUTE WS-COMMISSION ROUNDED =
                   WS-IN-AMOUNT * WS-RATE * WS-DAYS / 365
           IF WS-COMMISSION < WS-MIN-COMMISSION
               MOVE WS-MIN-COMMISSION TO WS-COMMISSION
           END-IF
           COMPUTE WS-GST ROUNDED = WS-COMMISSION * WS-GST-RATE.

       1300-REFUND-MARGIN.
           IF WS-MARGIN = ZERO
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           MOVE WS-MARGIN     TO PP-AMOUNT
           MOVE 'TR'          TO PP-TRAN-TYPE
           MOVE 'MARGIN REFUND' TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS.

       1400-NEXT-NO.
           MOVE ZERO TO WS-NEXT-NO
           MOVE 99999999 TO TF-NO
           START TF-FILE KEY IS LESS THAN OR EQUAL TF-NO
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ TF-FILE PREVIOUS RECORD
                       NOT AT END MOVE TF-NO TO WS-NEXT-NO
                   END-READ
           END-START
           ADD 1 TO WS-NEXT-NO.

      *================================================================*
      * 2 / 3 - PAYMENTS UNDER AN INSTRUMENT                           *
      *================================================================*
       2000-LC-DRAWING.
           PERFORM 7000-READ-TF
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT TF-LC OR NOT TF-ACTIVE
               DISPLAY 'NOT AN ACTIVE LETTER OF CREDIT.'
               EXIT PARAGRAPH
           END-IF
           IF WS-TODAY > TF-EXPIRY-DATE
               DISPLAY 'LC EXPIRED ON ' TF-EXPIRY-DATE
                       ' - DOCUMENTS CANNOT BE PAID.'
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-AVAILABLE = TF-AMOUNT - TF-UTILISED
           MOVE WS-AVAILABLE TO ED-AMT
           DISPLAY 'AVAILABLE UNDER LC : ' ED-AMT
           DISPLAY 'DOCUMENT AMOUNT    : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           IF WS-IN-AMOUNT NOT > ZERO OR WS-IN-AMOUNT > WS-AVAILABLE
               DISPLAY 'AMOUNT MUST BE WITHIN THE AVAILABLE BALANCE.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 7100-PAY-BENEFICIARY
           IF NOT INPUT-OK
               EXIT PARAGRAPH
           END-IF
           ADD WS-IN-AMOUNT TO TF-UTILISED
           IF TF-UTILISED = TF-AMOUNT
               SET TF-FULLY-DRAWN TO TRUE
               MOVE WS-TODAY TO TF-CLOSE-DATE
               PERFORM 7200-RELEASE-MARGIN
           END-IF
           REWRITE TF-RECORD
           MOVE WS-IN-AMOUNT TO ED-AMT
           DISPLAY 'LC DRAWING OF ' ED-AMT ' PAID TO '
                   FUNCTION TRIM(TF-BENEFICIARY)
           MOVE 'LC DRAWING' TO AU-ACTION
           PERFORM 9000-AUDIT.

       3000-BG-INVOCATION.
           PERFORM 7000-READ-TF
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT TF-GUARANTEE OR NOT TF-ACTIVE
               DISPLAY 'NOT AN ACTIVE BANK GUARANTEE.'
               EXIT PARAGRAPH
           END-IF
           IF WS-TODAY > TF-CLAIM-END
               DISPLAY 'CLAIM PERIOD ENDED ON ' TF-CLAIM-END
                       ' - CLAIM REJECTED.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'AMOUNT CLAIMED : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           IF WS-IN-AMOUNT NOT > ZERO OR WS-IN-AMOUNT > TF-AMOUNT
               DISPLAY 'CLAIM CANNOT EXCEED THE GUARANTEE AMOUNT.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 7100-PAY-BENEFICIARY
           IF NOT INPUT-OK
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-AMOUNT TO TF-UTILISED
           SET TF-INVOKED TO TRUE
           MOVE WS-TODAY TO TF-CLOSE-DATE
      *    A GUARANTEE IS INVOKED ONCE; ANY UNUSED MARGIN GOES BACK
           PERFORM 7200-RELEASE-MARGIN
           REWRITE TF-RECORD
           MOVE WS-IN-AMOUNT TO ED-AMT
           DISPLAY 'BG INVOKED - ' ED-AMT ' PAID TO '
                   FUNCTION TRIM(TF-BENEFICIARY)
           MOVE 'BG INVOKED' TO AU-ACTION
           PERFORM 9000-AUDIT.

      *================================================================*
      * 4 - CANCEL                                                     *
      *================================================================*
       4000-CANCEL.
           PERFORM 7000-READ-TF
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT TF-ACTIVE
               DISPLAY 'INSTRUMENT IS NOT ACTIVE.'
               EXIT PARAGRAPH
           END-IF
           IF TF-UTILISED > ZERO
               DISPLAY 'LC ALREADY PARTLY DRAWN - CANNOT CANCEL.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'ORIGINAL RETURNED BY BENEFICIARY - CANCEL (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
               DISPLAY 'NOT CANCELLED.'
               EXIT PARAGRAPH
           END-IF
           SET TF-CANCELLED TO TRUE
           MOVE WS-TODAY TO TF-CLOSE-DATE
           PERFORM 7200-RELEASE-MARGIN
           REWRITE TF-RECORD
           DISPLAY 'INSTRUMENT ' TF-NO ' CANCELLED, MARGIN RELEASED.'
           MOVE 'CANCEL' TO AU-ACTION
           PERFORM 9000-AUDIT.

      *================================================================*
      * 5 / 6 - ENQUIRIES                                              *
      *================================================================*
       5000-INQUIRY.
           PERFORM 7000-READ-TF
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8100-NAMES
           MOVE TF-AMOUNT TO ED-AMT
           MOVE TF-UTILISED TO ED-AMT2
           DISPLAY WS-TYPE-NAME ' ' TF-NO '   ' WS-STATUS-NAME
           DISPLAY 'APPLICANT   : ' TF-ACCT-NO
           DISPLAY 'BENEFICIARY : ' TF-BENEFICIARY
           DISPLAY 'AMOUNT      : ' ED-AMT '  UTILISED ' ED-AMT2
           DISPLAY 'ISSUED      : ' TF-ISSUE-DATE '  EXPIRY '
                   TF-EXPIRY-DATE '  CLAIMS TILL ' TF-CLAIM-END
           MOVE TF-MARGIN-HELD TO ED-AMT
           DISPLAY 'MARGIN HELD : ' ED-AMT ' (' TF-MARGIN-PCT '%)'.

       6000-ACCOUNT-LIST.
           DISPLAY 'ACCOUNT NO : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           MOVE ZERO TO WS-COUNT
           MOVE 'N' TO WS-EOF-SW
           MOVE ZERO TO TF-NO
           START TF-FILE KEY IS NOT LESS THAN TF-NO
               INVALID KEY SET END-OF-FILE TO TRUE
           END-START
           PERFORM UNTIL END-OF-FILE
               READ TF-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF TF-ACCT-NO = WS-IN-ACCT-NO
                           ADD 1 TO WS-COUNT
                           PERFORM 8100-NAMES
                           MOVE TF-AMOUNT TO ED-AMT
                           DISPLAY TF-NO ' ' WS-TYPE-NAME ' ' ED-AMT
                                   ' ' TF-EXPIRY-DATE ' '
                                   WS-STATUS-NAME
                       END-IF
               END-READ
           END-PERFORM
           DISPLAY WS-COUNT ' INSTRUMENT(S)'.

      *================================================================*
      * COMMON                                                         *
      *================================================================*
       7000-READ-TF.
           DISPLAY 'INSTRUMENT NO : ' WITH NO ADVANCING
           ACCEPT WS-IN-TF-NO
           MOVE WS-IN-TF-NO TO TF-NO
           READ TF-FILE
               INVALID KEY
                   DISPLAY 'NO SUCH INSTRUMENT.'
                   MOVE '23' TO WS-FS
           END-READ.

      *    MARGIN FIRST, THEN THE APPLICANT'S ACCOUNT. WS-OK-SW = 'N'
      *    IF THE ACCOUNT CANNOT COVER ITS PART (NOTHING IS PAID).
       7100-PAY-BENEFICIARY.
           MOVE 'Y' TO WS-OK-SW
           IF WS-IN-AMOUNT > TF-MARGIN-HELD
               MOVE TF-MARGIN-HELD TO WS-FROM-MARGIN
           ELSE
               MOVE WS-IN-AMOUNT TO WS-FROM-MARGIN
           END-IF
           COMPUTE WS-FROM-ACCOUNT = WS-IN-AMOUNT - WS-FROM-MARGIN
           IF WS-FROM-ACCOUNT > ZERO
               INITIALIZE POST-PARMS
               SET PP-DEBIT TO TRUE
               MOVE TF-ACCT-NO      TO PP-ACCT-NO
               MOVE WS-FROM-ACCOUNT TO PP-AMOUNT
               MOVE 'TF'            TO PP-TRAN-TYPE
               MOVE GLC-PAYMENT-SETTLE TO PP-CONTRA-GL
               MOVE 'PAYMENT UNDER BG / LC' TO PP-NARRATION
               CALL 'ACCTPOST' USING POST-PARMS
               IF NOT PP-OK
                   MOVE 'N' TO WS-OK-SW
                   MOVE WS-FROM-ACCOUNT TO ED-AMT
                   DISPLAY 'ACCOUNT CANNOT FUND ' ED-AMT
                           ' - DEVOLVEMENT, REFER TO CREDIT.'
                   EXIT PARAGRAPH
               END-IF
           END-IF
           IF WS-FROM-MARGIN > ZERO
               MOVE GLC-MARGIN-MONEY   TO GP-DR-GL
               MOVE GLC-PAYMENT-SETTLE TO GP-CR-GL
               MOVE WS-FROM-MARGIN     TO GP-AMOUNT
               MOVE 'PAYMENT FROM MARGIN' TO GP-NARRATION
               PERFORM 7400-GL
               SUBTRACT WS-FROM-MARGIN FROM TF-MARGIN-HELD
           END-IF.

       7200-RELEASE-MARGIN.
           IF TF-MARGIN-HELD = ZERO
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE TF-ACCT-NO     TO PP-ACCT-NO
           MOVE TF-MARGIN-HELD TO PP-AMOUNT
           MOVE 'TR'           TO PP-TRAN-TYPE
           MOVE 'MARGIN RELEASED' TO PP-NARRATION
           CALL 'ACCTPOST' USING POST-PARMS
           IF PP-OK
               MOVE TF-MARGIN-HELD TO ED-AMT
               DISPLAY 'MARGIN OF ' ED-AMT ' RELEASED TO ' TF-ACCT-NO
               MOVE ZERO TO TF-MARGIN-HELD
           ELSE
               DISPLAY 'MARGIN RELEASE FAILED, RC=' PP-RETURN-CODE
                       ' - STILL HELD'
           END-IF.

       7400-GL.
           MOVE SPACES TO GP-REF
           STRING 'TF' TF-NO DELIMITED BY SIZE INTO GP-REF
           CALL 'GLPOST' USING GL-PARMS
           IF NOT GP-OK
               DISPLAY 'GL POSTING FAILED, RC=' GP-RC
           END-IF.

       8100-NAMES.
           IF TF-GUARANTEE
               MOVE 'GUARANTEE' TO WS-TYPE-NAME
           ELSE
               MOVE 'LC' TO WS-TYPE-NAME
           END-IF
           EVALUATE TRUE
               WHEN TF-ACTIVE      MOVE 'ACTIVE'      TO WS-STATUS-NAME
               WHEN TF-INVOKED     MOVE 'INVOKED'     TO WS-STATUS-NAME
               WHEN TF-FULLY-DRAWN MOVE 'FULLY DRAWN' TO WS-STATUS-NAME
               WHEN TF-EXPIRED     MOVE 'EXPIRED'     TO WS-STATUS-NAME
               WHEN TF-CANCELLED   MOVE 'CANCELLED'   TO WS-STATUS-NAME
               WHEN OTHER          MOVE 'UNKNOWN'     TO WS-STATUS-NAME
           END-EVALUATE.

       9000-AUDIT.
           MOVE 'TFMNT'   TO AU-PROGRAM
           MOVE TF-NO     TO AU-KEY
           MOVE SPACES    TO AU-DETAIL
           MOVE TF-AMOUNT TO ED-AMT
           STRING TF-TYPE ' ' TF-ACCT-NO ' ' ED-AMT
               DELIMITED BY SIZE INTO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       8990-GUARD-END-OF-INPUT.
           IF WS-CHOICE = SPACE
               ADD 1 TO WS-BLANK-INPUTS
               IF WS-BLANK-INPUTS > 20
                   SET EXIT-REQUESTED TO TRUE
               END-IF
           ELSE
               MOVE ZERO TO WS-BLANK-INPUTS
           END-IF.
