       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMNT.
      *================================================================*
      * PROGRAM  : CUSTMNT                                             *
      * PURPOSE  : CUSTOMER INFORMATION FILE (CIF) AND KYC.            *
      *            CREATE CUSTOMER WITH PAN / MOBILE / EMAIL / PIN     *
      *            CODE VALIDATION AND DUPLICATE PAN CHECK, INQUIRY,   *
      *            CONTACT UPDATE, KYC VERIFY / REJECT, LINK ACCOUNTS, *
      *            RE-KYC DUE LIST AND SEARCH BY PAN.                  *
      *            RE-KYC PERIODS (RBI MASTER DIRECTION ON KYC):       *
      *              HIGH RISK 2 YEARS, MEDIUM 8 YEARS, LOW 10 YEARS.  *
      * FILES    : CUSTMAST (INDEXED, I-O)                             *
      * CALLS    : ACCTPOST (INQUIRY ONLY), DATEUTIL                   *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      * 2026-10-14 KYC DECISION RESTRICTED TO MANAGER/ADMIN, AUDITED   *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-FILE ASSIGN TO 'data/CUSTMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUST-FILE.
           COPY CUSTREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY DATEPARM.
           COPY SESSION.
           COPY AUDPARM.

       01  WS-CUST-FS                  PIC X(02).
           88  CUST-FS-OK                        VALUE '00'.
           88  CUST-FS-NOFILE                    VALUE '35'.

       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
           88  CHOICE-BACK                       VALUE '9'.
       01  WS-VALID-SW                 PIC X(01).
           88  INPUT-VALID                       VALUE 'Y'.
           88  INPUT-INVALID                     VALUE 'N'.
       01  WS-FOUND-SW                 PIC X(01).
           88  RECORD-FOUND                      VALUE 'Y'.

      * INPUT WORK AREA - SAME SHAPE AS THE MASTER SO IT CAN BE MOVED
       01  WS-IN.
           05  WS-IN-CUST-ID           PIC 9(08).
           05  WS-IN-NAME              PIC X(40).
           05  WS-IN-DOB               PIC X(08).
           05  WS-IN-GENDER            PIC X(01).
           05  WS-IN-PAN               PIC X(10).
           05  WS-IN-AADHAAR           PIC X(12).
           05  WS-IN-MOBILE            PIC X(10).
           05  WS-IN-EMAIL             PIC X(40).
           05  WS-IN-ADDR1             PIC X(40).
           05  WS-IN-ADDR2             PIC X(40).
           05  WS-IN-CITY              PIC X(20).
           05  WS-IN-STATE             PIC X(20).
           05  WS-IN-PINCODE           PIC X(06).
           05  WS-IN-RISK              PIC X(01).
           05  WS-IN-ACCT-NO           PIC 9(10).
           05  WS-IN-DECISION          PIC X(01).
           05  WS-IN-REMARKS           PIC X(30).

       01  WS-NEXT-CUST-ID             PIC 9(08).
       01  WS-AGE-DAYS                 PIC S9(07).
       01  WS-I                        PIC 9(02).
       01  WS-AT-POS                   PIC 9(02).
       01  WS-DOT-AFTER-AT             PIC 9(02).
       01  WS-RE-KYC-YEARS             PIC 9(02).
       01  WS-RE-KYC-DATE              PIC 9(08).
       01  WS-LIST-CNT                 PIC 9(05).
       01  WS-SAVE-CUST-ID             PIC 9(08).

       01  WS-PAN-WORK                 PIC X(10).
       01  WS-PAN-R REDEFINES WS-PAN-WORK.
           05  WS-PAN-ALPHA1           PIC X(05).
           05  WS-PAN-DIGITS           PIC X(04).
           05  WS-PAN-ALPHA2           PIC X(01).

       01  WS-MASKED-MOBILE            PIC X(10).

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN I-O CUST-FILE
           IF CUST-FS-NOFILE
               OPEN OUTPUT CUST-FILE
               CLOSE CUST-FILE
               OPEN I-O CUST-FILE
           END-IF
           IF NOT CUST-FS-OK
               DISPLAY 'CUSTOMER MASTER OPEN FAILED, FS=' WS-CUST-FS
               GOBACK
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL CHOICE-BACK
               PERFORM 1000-SHOW-MENU
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-CREATE-CUSTOMER
                   WHEN '2' PERFORM 3000-INQUIRY
                   WHEN '3' PERFORM 4000-UPDATE-CONTACT
                   WHEN '4' PERFORM 5000-KYC-DECISION
                   WHEN '5' PERFORM 6000-LINK-ACCOUNT
                   WHEN '6' PERFORM 7000-RE-KYC-DUE-LIST
                   WHEN '7' PERFORM 7500-SEARCH-BY-PAN
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE CUST-FILE
           GOBACK.

       1000-SHOW-MENU.
           DISPLAY ' '
           DISPLAY '----------- CUSTOMER / KYC -----------'
           DISPLAY ' 1. CREATE CUSTOMER'
           DISPLAY ' 2. CUSTOMER INQUIRY'
           DISPLAY ' 3. UPDATE MOBILE / EMAIL / ADDRESS'
           DISPLAY ' 4. KYC VERIFY / REJECT'
           DISPLAY ' 5. LINK ACCOUNT TO CUSTOMER'
           DISPLAY ' 6. RE-KYC DUE LIST'
           DISPLAY ' 7. SEARCH BY PAN'
           DISPLAY ' 9. BACK TO MAIN MENU'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT.

      *----------------------------------------------------------------*
      * CREATE                                                         *
      *----------------------------------------------------------------*
       2000-CREATE-CUSTOMER.
           SET INPUT-VALID TO TRUE
           PERFORM 2100-ACCEPT-DETAILS
           PERFORM 2200-VALIDATE-DETAILS
           IF INPUT-INVALID
               DISPLAY 'CUSTOMER NOT CREATED.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 2300-CHECK-DUPLICATE-PAN
           IF RECORD-FOUND
               DISPLAY 'PAN ALREADY REGISTERED TO CUSTOMER ' CUST-ID
               EXIT PARAGRAPH
           END-IF
           PERFORM 2400-GET-NEXT-CUST-ID
           PERFORM 2500-WRITE-CUSTOMER.

       2100-ACCEPT-DETAILS.
           DISPLAY 'FULL NAME            : ' WITH NO ADVANCING
           ACCEPT WS-IN-NAME
           DISPLAY 'DATE OF BIRTH (YYYYMMDD): ' WITH NO ADVANCING
           ACCEPT WS-IN-DOB
           DISPLAY 'GENDER (M/F/T)       : ' WITH NO ADVANCING
           ACCEPT WS-IN-GENDER
           DISPLAY 'PAN                  : ' WITH NO ADVANCING
           ACCEPT WS-IN-PAN
           DISPLAY 'AADHAAR (12 DIGITS)  : ' WITH NO ADVANCING
           ACCEPT WS-IN-AADHAAR
           DISPLAY 'MOBILE               : ' WITH NO ADVANCING
           ACCEPT WS-IN-MOBILE
           DISPLAY 'EMAIL                : ' WITH NO ADVANCING
           ACCEPT WS-IN-EMAIL
           DISPLAY 'ADDRESS LINE 1       : ' WITH NO ADVANCING
           ACCEPT WS-IN-ADDR1
           DISPLAY 'ADDRESS LINE 2       : ' WITH NO ADVANCING
           ACCEPT WS-IN-ADDR2
           DISPLAY 'CITY                 : ' WITH NO ADVANCING
           ACCEPT WS-IN-CITY
           DISPLAY 'STATE                : ' WITH NO ADVANCING
           ACCEPT WS-IN-STATE
           DISPLAY 'PIN CODE             : ' WITH NO ADVANCING
           ACCEPT WS-IN-PINCODE
           DISPLAY 'RISK CATEGORY (L/M/H): ' WITH NO ADVANCING
           ACCEPT WS-IN-RISK
           MOVE FUNCTION UPPER-CASE(WS-IN-NAME)   TO WS-IN-NAME
           MOVE FUNCTION UPPER-CASE(WS-IN-GENDER) TO WS-IN-GENDER
           MOVE FUNCTION UPPER-CASE(WS-IN-PAN)    TO WS-IN-PAN
           MOVE FUNCTION UPPER-CASE(WS-IN-RISK)   TO WS-IN-RISK
           MOVE FUNCTION UPPER-CASE(WS-IN-CITY)   TO WS-IN-CITY
           MOVE FUNCTION UPPER-CASE(WS-IN-STATE)  TO WS-IN-STATE
           MOVE FUNCTION LOWER-CASE(WS-IN-EMAIL)  TO WS-IN-EMAIL.

       2200-VALIDATE-DETAILS.
           IF WS-IN-NAME = SPACES
               DISPLAY '- NAME IS REQUIRED.'
               SET INPUT-INVALID TO TRUE
           END-IF
           PERFORM 2210-VALIDATE-DOB
           MOVE WS-IN-GENDER TO CUST-GENDER
           IF NOT CUST-GENDER-VALID
               DISPLAY '- GENDER MUST BE M, F OR T.'
               SET INPUT-INVALID TO TRUE
           END-IF
           PERFORM 2220-VALIDATE-PAN
           IF WS-IN-AADHAAR NOT NUMERIC
               DISPLAY '- AADHAAR MUST BE 12 DIGITS.'
               SET INPUT-INVALID TO TRUE
           END-IF
           PERFORM 2230-VALIDATE-MOBILE
           PERFORM 2240-VALIDATE-EMAIL
           IF WS-IN-ADDR1 = SPACES OR WS-IN-CITY = SPACES
              OR WS-IN-STATE = SPACES
               DISPLAY '- ADDRESS LINE 1, CITY AND STATE ARE REQUIRED.'
               SET INPUT-INVALID TO TRUE
           END-IF
           IF WS-IN-PINCODE NOT NUMERIC OR WS-IN-PINCODE(1:1) = '0'
               DISPLAY '- PIN CODE MUST BE 6 DIGITS, NOT STARTING 0.'
               SET INPUT-INVALID TO TRUE
           END-IF
           MOVE WS-IN-RISK TO CUST-RISK
           IF NOT RISK-VALID
               DISPLAY '- RISK CATEGORY MUST BE L, M OR H.'
               SET INPUT-INVALID TO TRUE
           END-IF.

      *    VALID DATE, NOT IN THE FUTURE, CUSTOMER AT LEAST 18
       2210-VALIDATE-DOB.
           IF WS-IN-DOB NOT NUMERIC
               DISPLAY '- DATE OF BIRTH MUST BE YYYYMMDD.'
               SET INPUT-INVALID TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-DOB TO DU-DATE-1
           SET DU-VALIDATE TO TRUE
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DU-OK
               DISPLAY '- DATE OF BIRTH IS NOT A VALID DATE.'
               SET INPUT-INVALID TO TRUE
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE WS-CURR-DATE TO DU-DATE-1
           MOVE -18          TO DU-NUMBER
           MULTIPLY 12 BY DU-NUMBER
           SET DU-ADD-MONTHS TO TRUE
           CALL 'DATEUTIL' USING DATE-PARMS
           IF WS-IN-DOB > DU-RESULT-DATE
               DISPLAY '- CUSTOMER MUST BE AT LEAST 18 YEARS OLD.'
               SET INPUT-INVALID TO TRUE
           END-IF.

      *    PAN FORMAT: 5 LETTERS, 4 DIGITS, 1 LETTER. THE 4TH LETTER
      *    IS THE HOLDER TYPE - 'P' FOR AN INDIVIDUAL.
       2220-VALIDATE-PAN.
           MOVE WS-IN-PAN TO WS-PAN-WORK
           IF WS-PAN-ALPHA1 IS NOT ALPHABETIC-UPPER
              OR WS-PAN-ALPHA1 = SPACES
              OR WS-PAN-DIGITS IS NOT NUMERIC
              OR WS-PAN-ALPHA2 IS NOT ALPHABETIC-UPPER
              OR WS-PAN-ALPHA2 = SPACE
               DISPLAY '- PAN FORMAT MUST BE AAAAA9999A.'
               SET INPUT-INVALID TO TRUE
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               IF WS-PAN-ALPHA1(WS-I:1) = SPACE
                   DISPLAY '- PAN FORMAT MUST BE AAAAA9999A.'
                   SET INPUT-INVALID TO TRUE
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM
           IF WS-PAN-ALPHA1(4:1) NOT = 'P'
               DISPLAY '- PAN IS NOT AN INDIVIDUAL PAN (4TH CHAR P).'
               SET INPUT-INVALID TO TRUE
           END-IF.

      *    INDIAN MOBILE NUMBERS START WITH 6, 7, 8 OR 9
       2230-VALIDATE-MOBILE.
           IF WS-IN-MOBILE NOT NUMERIC
              OR WS-IN-MOBILE(1:1) < '6'
               DISPLAY '- MOBILE MUST BE 10 DIGITS STARTING 6-9.'
               SET INPUT-INVALID TO TRUE
           END-IF.

      *    LIGHT CHECK: ONE '@', SOMETHING BEFORE IT, A '.' AFTER IT
       2240-VALIDATE-EMAIL.
           IF WS-IN-EMAIL = SPACES
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO WS-AT-POS WS-DOT-AFTER-AT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 40
               EVALUATE TRUE
                   WHEN WS-IN-EMAIL(WS-I:1) = '@'
                       IF WS-AT-POS = ZERO
                           MOVE WS-I TO WS-AT-POS
                       ELSE
                           MOVE 99 TO WS-AT-POS
                       END-IF
                   WHEN WS-IN-EMAIL(WS-I:1) = '.'
                    AND WS-AT-POS > ZERO AND WS-I > WS-AT-POS + 1
                       MOVE WS-I TO WS-DOT-AFTER-AT
               END-EVALUATE
           END-PERFORM
           IF WS-AT-POS < 2 OR WS-AT-POS = 99 OR WS-DOT-AFTER-AT = ZERO
               DISPLAY '- EMAIL ADDRESS IS NOT VALID.'
               SET INPUT-INVALID TO TRUE
           END-IF.

      *    FULL SCAN - FINE FOR A SMALL CIF. A REAL SYSTEM WOULD USE
      *    AN ALTERNATE KEY ON PAN.
       2300-CHECK-DUPLICATE-PAN.
           MOVE 'N' TO WS-FOUND-SW
           MOVE ZERO TO CUST-ID
           START CUST-FILE KEY IS NOT LESS THAN CUST-ID
               INVALID KEY EXIT PARAGRAPH
           END-START
           PERFORM FOREVER
               READ CUST-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF CUST-PAN = WS-IN-PAN
                   SET RECORD-FOUND TO TRUE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       2400-GET-NEXT-CUST-ID.
           MOVE 20000000 TO WS-NEXT-CUST-ID
           MOVE 99999999 TO CUST-ID
           START CUST-FILE KEY IS LESS THAN OR EQUAL CUST-ID
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ CUST-FILE PREVIOUS RECORD
                       AT END CONTINUE
                       NOT AT END MOVE CUST-ID TO WS-NEXT-CUST-ID
                   END-READ
           END-START
           ADD 1 TO WS-NEXT-CUST-ID.

       2500-WRITE-CUSTOMER.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           INITIALIZE CUST-RECORD
           MOVE WS-NEXT-CUST-ID    TO CUST-ID
           MOVE WS-IN-NAME         TO CUST-NAME
           MOVE WS-IN-DOB          TO CUST-DOB
           MOVE WS-IN-GENDER       TO CUST-GENDER
           MOVE WS-IN-PAN          TO CUST-PAN
           MOVE WS-IN-AADHAAR(9:4) TO CUST-AADHAAR-LAST4
           MOVE WS-IN-MOBILE       TO CUST-MOBILE
           MOVE WS-IN-EMAIL        TO CUST-EMAIL
           MOVE WS-IN-ADDR1        TO CUST-ADDR-LINE1
           MOVE WS-IN-ADDR2        TO CUST-ADDR-LINE2
           MOVE WS-IN-CITY         TO CUST-CITY
           MOVE WS-IN-STATE        TO CUST-STATE
           MOVE WS-IN-PINCODE      TO CUST-PINCODE
           MOVE WS-IN-RISK         TO CUST-RISK
           SET KYC-PENDING         TO TRUE
           MOVE WS-CURR-DATE       TO CUST-CREATED-DATE
           WRITE CUST-RECORD
               INVALID KEY
                   DISPLAY 'CUSTOMER WRITE FAILED, FS=' WS-CUST-FS
               NOT INVALID KEY
                   DISPLAY 'CUSTOMER CREATED. CUSTOMER ID: ' CUST-ID
                   DISPLAY 'KYC STATUS: PENDING VERIFICATION'
           END-WRITE.

      *----------------------------------------------------------------*
      * INQUIRY                                                        *
      *----------------------------------------------------------------*
       3000-INQUIRY.
           PERFORM 8000-READ-CUSTOMER
           IF NOT CUST-FS-OK
               EXIT PARAGRAPH
           END-IF
           MOVE ALL 'X' TO WS-MASKED-MOBILE
           MOVE CUST-MOBILE(7:4) TO WS-MASKED-MOBILE(7:4)
           DISPLAY '----------------------------------------'
           DISPLAY 'CUSTOMER ID  : ' CUST-ID
           DISPLAY 'NAME         : ' CUST-NAME
           DISPLAY 'DOB / GENDER : ' CUST-DOB ' / ' CUST-GENDER
           DISPLAY 'PAN          : ' CUST-PAN
           DISPLAY 'AADHAAR      : XXXX XXXX ' CUST-AADHAAR-LAST4
           DISPLAY 'MOBILE       : ' WS-MASKED-MOBILE
           DISPLAY 'EMAIL        : ' FUNCTION TRIM(CUST-EMAIL)
           DISPLAY 'ADDRESS      : ' FUNCTION TRIM(CUST-ADDR-LINE1)
           IF CUST-ADDR-LINE2 NOT = SPACES
               DISPLAY '               ' FUNCTION TRIM(CUST-ADDR-LINE2)
           END-IF
           DISPLAY '               ' FUNCTION TRIM(CUST-CITY) ', '
                   FUNCTION TRIM(CUST-STATE) ' - ' CUST-PINCODE
           EVALUATE TRUE
               WHEN KYC-PENDING  DISPLAY 'KYC          : PENDING'
               WHEN KYC-VERIFIED DISPLAY 'KYC          : VERIFIED ON '
                                         CUST-KYC-DATE
               WHEN KYC-REJECTED DISPLAY 'KYC          : REJECTED - '
                                         CUST-KYC-REMARKS
           END-EVALUATE
           DISPLAY 'RISK         : ' CUST-RISK
           IF CUST-ACCT-COUNT = ZERO
               DISPLAY 'ACCOUNTS     : NONE LINKED'
           ELSE
               PERFORM VARYING WS-I FROM 1 BY 1
                       UNTIL WS-I > CUST-ACCT-COUNT
                   DISPLAY 'ACCOUNT ' WS-I '   : ' CUST-ACCOUNTS(WS-I)
               END-PERFORM
           END-IF
           DISPLAY '----------------------------------------'.

      *----------------------------------------------------------------*
      * UPDATE CONTACT - BLANK INPUT KEEPS THE CURRENT VALUE.          *
      * AN ADDRESS CHANGE SENDS KYC BACK TO PENDING.                   *
      *----------------------------------------------------------------*
       4000-UPDATE-CONTACT.
           PERFORM 8000-READ-CUSTOMER
           IF NOT CUST-FS-OK
               EXIT PARAGRAPH
           END-IF
           SET INPUT-VALID TO TRUE
           DISPLAY '(LEAVE BLANK TO KEEP CURRENT VALUE)'
           DISPLAY 'NEW MOBILE    : ' WITH NO ADVANCING
           ACCEPT WS-IN-MOBILE
           DISPLAY 'NEW EMAIL     : ' WITH NO ADVANCING
           ACCEPT WS-IN-EMAIL
           DISPLAY 'NEW ADDRESS 1 : ' WITH NO ADVANCING
           ACCEPT WS-IN-ADDR1
           MOVE FUNCTION LOWER-CASE(WS-IN-EMAIL) TO WS-IN-EMAIL

           IF WS-IN-MOBILE NOT = SPACES
               PERFORM 2230-VALIDATE-MOBILE
           END-IF
           PERFORM 2240-VALIDATE-EMAIL
           IF WS-IN-ADDR1 NOT = SPACES
               DISPLAY 'NEW ADDRESS 2 : ' WITH NO ADVANCING
               ACCEPT WS-IN-ADDR2
               DISPLAY 'NEW CITY      : ' WITH NO ADVANCING
               ACCEPT WS-IN-CITY
               DISPLAY 'NEW STATE     : ' WITH NO ADVANCING
               ACCEPT WS-IN-STATE
               DISPLAY 'NEW PIN CODE  : ' WITH NO ADVANCING
               ACCEPT WS-IN-PINCODE
               IF WS-IN-PINCODE NOT NUMERIC
                  OR WS-IN-PINCODE(1:1) = '0'
                   DISPLAY '- PIN CODE MUST BE 6 DIGITS.'
                   SET INPUT-INVALID TO TRUE
               END-IF
           END-IF
           IF INPUT-INVALID
               DISPLAY 'NO CHANGES SAVED.'
               EXIT PARAGRAPH
           END-IF

           IF WS-IN-MOBILE NOT = SPACES
               MOVE WS-IN-MOBILE TO CUST-MOBILE
           END-IF
           IF WS-IN-EMAIL NOT = SPACES
               MOVE WS-IN-EMAIL TO CUST-EMAIL
           END-IF
           IF WS-IN-ADDR1 NOT = SPACES
               MOVE WS-IN-ADDR1   TO CUST-ADDR-LINE1
               MOVE WS-IN-ADDR2   TO CUST-ADDR-LINE2
               MOVE FUNCTION UPPER-CASE(WS-IN-CITY)  TO CUST-CITY
               MOVE FUNCTION UPPER-CASE(WS-IN-STATE) TO CUST-STATE
               MOVE WS-IN-PINCODE TO CUST-PINCODE
               SET KYC-PENDING    TO TRUE
               MOVE 'ADDRESS CHANGED' TO CUST-KYC-REMARKS
               DISPLAY 'ADDRESS CHANGED - KYC RESET TO PENDING.'
           END-IF
           PERFORM 8100-REWRITE-CUSTOMER
           DISPLAY 'CUSTOMER UPDATED.'.

      *----------------------------------------------------------------*
      * KYC DECISION                                                   *
      *----------------------------------------------------------------*
       5000-KYC-DECISION.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'KYC DECISIONS NEED A MANAGER OR ADMIN.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8000-READ-CUSTOMER
           IF NOT CUST-FS-OK
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'DECISION (V=VERIFY / R=REJECT): ' WITH NO ADVANCING
           ACCEPT WS-IN-DECISION
           MOVE FUNCTION UPPER-CASE(WS-IN-DECISION) TO WS-IN-DECISION
           DISPLAY 'REMARKS                     : ' WITH NO ADVANCING
           ACCEPT WS-IN-REMARKS
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           EVALUATE WS-IN-DECISION
               WHEN 'V'
                   SET KYC-VERIFIED TO TRUE
                   MOVE WS-CURR-DATE TO CUST-KYC-DATE
               WHEN 'R'
                   IF WS-IN-REMARKS = SPACES
                       DISPLAY 'REMARKS ARE MANDATORY FOR REJECTION.'
                       EXIT PARAGRAPH
                   END-IF
                   SET KYC-REJECTED TO TRUE
               WHEN OTHER
                   DISPLAY 'INVALID DECISION.'
                   EXIT PARAGRAPH
           END-EVALUATE
           MOVE FUNCTION UPPER-CASE(WS-IN-REMARKS) TO CUST-KYC-REMARKS
           PERFORM 8100-REWRITE-CUSTOMER
           MOVE 'CUSTMNT'  TO AU-PROGRAM
           MOVE 'KYC-' TO AU-ACTION
           MOVE CUST-KYC-STATUS TO AU-ACTION(5:1)
           MOVE CUST-ID    TO AU-KEY
           MOVE CUST-KYC-REMARKS TO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS
           DISPLAY 'KYC STATUS UPDATED.'.

      *----------------------------------------------------------------*
      * LINK ACCOUNT - KYC MUST BE VERIFIED, MAX 5 ACCOUNTS            *
      *----------------------------------------------------------------*
       6000-LINK-ACCOUNT.
           PERFORM 8000-READ-CUSTOMER
           IF NOT CUST-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF NOT KYC-VERIFIED
               DISPLAY 'KYC MUST BE VERIFIED BEFORE LINKING ACCOUNTS.'
               EXIT PARAGRAPH
           END-IF
           IF CUST-ACCT-COUNT >= 5
               DISPLAY 'MAXIMUM 5 ACCOUNTS PER CUSTOMER.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'ACCOUNT NO TO LINK: ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > CUST-ACCT-COUNT
               IF CUST-ACCOUNTS(WS-I) = WS-IN-ACCT-NO
                   DISPLAY 'ACCOUNT ALREADY LINKED.'
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'ACCOUNT NOT FOUND.'
               EXIT PARAGRAPH
           END-IF
           IF PP-HOLDER-NAME NOT = CUST-NAME
               DISPLAY 'WARNING: ACCOUNT NAME ' PP-HOLDER-NAME
               DISPLAY '         DIFFERS FROM CUSTOMER NAME.'
           END-IF
           ADD 1 TO CUST-ACCT-COUNT
           MOVE WS-IN-ACCT-NO TO CUST-ACCOUNTS(CUST-ACCT-COUNT)
           PERFORM 8100-REWRITE-CUSTOMER
           DISPLAY 'ACCOUNT LINKED.'.

      *----------------------------------------------------------------*
      * RE-KYC DUE LIST - VERIFIED CUSTOMERS WHOSE PERIODIC UPDATE IS  *
      * DUE WITHIN 30 DAYS OR OVERDUE, PLUS ALL PENDING ONES.          *
      *----------------------------------------------------------------*
       7000-RE-KYC-DUE-LIST.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE ZERO TO WS-LIST-CNT CUST-ID
           START CUST-FILE KEY IS NOT LESS THAN CUST-ID
               INVALID KEY
                   DISPLAY 'NO CUSTOMERS ON FILE.'
                   EXIT PARAGRAPH
           END-START
           DISPLAY 'CUST ID   NAME                      STATUS  DUE'
           PERFORM FOREVER
               READ CUST-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               EVALUATE TRUE
                   WHEN KYC-PENDING OR KYC-REJECTED
                       ADD 1 TO WS-LIST-CNT
                       DISPLAY CUST-ID '  ' CUST-NAME(1:24) '  '
                               CUST-KYC-STATUS '       NOW'
                   WHEN KYC-VERIFIED
                       PERFORM 7100-CHECK-RE-KYC
               END-EVALUATE
           END-PERFORM
           IF WS-LIST-CNT = ZERO
               DISPLAY 'NO CUSTOMERS DUE FOR KYC.'
           ELSE
               DISPLAY WS-LIST-CNT ' CUSTOMER(S) LISTED.'
           END-IF.

       7100-CHECK-RE-KYC.
           EVALUATE TRUE
               WHEN RISK-HIGH   MOVE 02 TO WS-RE-KYC-YEARS
               WHEN RISK-MEDIUM MOVE 08 TO WS-RE-KYC-YEARS
               WHEN OTHER       MOVE 10 TO WS-RE-KYC-YEARS
           END-EVALUATE
           SET DU-ADD-MONTHS TO TRUE
           MOVE CUST-KYC-DATE TO DU-DATE-1
           COMPUTE DU-NUMBER = WS-RE-KYC-YEARS * 12
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO WS-RE-KYC-DATE
      *    DUE WITHIN THE NEXT 30 DAYS?
           SET DU-ADD-DAYS TO TRUE
           MOVE WS-CURR-DATE TO DU-DATE-1
           MOVE 30           TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           IF WS-RE-KYC-DATE <= DU-RESULT-DATE
               ADD 1 TO WS-LIST-CNT
               DISPLAY CUST-ID '  ' CUST-NAME(1:24) '  '
                       CUST-KYC-STATUS '       ' WS-RE-KYC-DATE
           END-IF.

       7500-SEARCH-BY-PAN.
           DISPLAY 'PAN: ' WITH NO ADVANCING
           ACCEPT WS-IN-PAN
           MOVE FUNCTION UPPER-CASE(WS-IN-PAN) TO WS-IN-PAN
           PERFORM 2300-CHECK-DUPLICATE-PAN
           IF RECORD-FOUND
               DISPLAY 'FOUND: CUSTOMER ' CUST-ID '  ' CUST-NAME
           ELSE
               DISPLAY 'NO CUSTOMER WITH THIS PAN.'
           END-IF.

      *----------------------------------------------------------------*
      * I/O                                                            *
      *----------------------------------------------------------------*
       8000-READ-CUSTOMER.
           DISPLAY 'CUSTOMER ID: ' WITH NO ADVANCING
           ACCEPT WS-IN-CUST-ID
           MOVE WS-IN-CUST-ID TO CUST-ID
           READ CUST-FILE
               INVALID KEY
                   DISPLAY 'CUSTOMER ' WS-IN-CUST-ID ' NOT FOUND.'
           END-READ.

       8100-REWRITE-CUSTOMER.
           REWRITE CUST-RECORD
               INVALID KEY
                   DISPLAY 'CUSTOMER UPDATE FAILED, FS=' WS-CUST-FS
           END-REWRITE.

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
