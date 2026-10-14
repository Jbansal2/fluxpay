       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOCKMNT.
      *================================================================*
      * PROGRAM  : LOCKMNT                                             *
      * PURPOSE  : SAFE DEPOSIT LOCKERS                                *
      *              1 - ADD LOCKERS TO A BRANCH (MANAGER)             *
      *              2 - ALLOT TO A KYC-VERIFIED CUSTOMER; THE FIRST   *
      *                  YEAR'S RENT IS DEBITED UP FRONT               *
      *              3 - RECORD A VISIT (REFUSED IF RENT IS OVERDUE)   *
      *              4 - SURRENDER (RENT MUST BE UP TO DATE)           *
      *              5 - VACANCY LIST                                  *
      *              6 - LOCKER INQUIRY                                *
      *            ANNUAL RENT: SMALL 2,000  MEDIUM 4,000  LARGE 8,000 *
      *            PLUS 18% GST. RENEWALS ARE COLLECTED BY CHGBATCH    *
      *            (CHARGE CODE LKR) IN THE MONTH RENT FALLS DUE.      *
      *            ONE LOCKER PER CUSTOMER.                            *
      * GL       : DR CUSTOMER CR 4102 (RENT + GST), DR 4102 CR 2501   *
      * FILES    : LOCKMAST (I-O), CUSTMAST (INPUT), LOCKVISIT (EXTEND)*
      * CALLS    : ACCTPOST, GLPOST, DATEUTIL, AUDITLOG                *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOCK-FILE ASSIGN TO 'data/LOCKMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LOCK-NO
               FILE STATUS IS WS-LOCK-FS.
           SELECT CUST-FILE ASSIGN TO 'data/CUSTMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-FS.
           SELECT VISIT-FILE ASSIGN TO 'data/LOCKVISIT.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-VISIT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOCK-FILE.
           COPY LOCKREC.
       FD  CUST-FILE.
           COPY CUSTREC.
       FD  VISIT-FILE.
       01  VISIT-LINE.
           05  VL-DATE                 PIC 9(08).
           05  VL-TIME                 PIC 9(06).
           05  VL-LOCK-NO              PIC 9(05).
           05  VL-CUST-ID              PIC 9(08).
           05  VL-USER-ID              PIC X(08).
           05  VL-EVENT                PIC X(10).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY GLPARM.
           COPY GLCODES.
           COPY DATEPARM.
           COPY SESSION.
           COPY AUDPARM.

       01  WS-LOCK-FS                  PIC X(02).
           88  LOCK-FS-OK                        VALUE '00'.
           88  LOCK-FS-NOFILE                    VALUE '35'.
       01  WS-CUST-FS                  PIC X(02).
           88  CUST-FS-OK                        VALUE '00'.
       01  WS-VISIT-FS                 PIC X(02).
       01  WS-CUST-OPEN-SW             PIC X(01).

       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
           88  CHOICE-BACK                       VALUE '9'.
       01  WS-CONFIRM                  PIC X(01).
           88  CONFIRM-YES                       VALUES 'Y' 'y'.

       01  WS-IN-LOCK-NO               PIC 9(05).
       01  WS-IN-BRANCH                PIC X(06).
       01  WS-IN-SIZE                  PIC X(01).
       01  WS-IN-COUNT                 PIC 9(03).
       01  WS-IN-CUST-ID               PIC 9(08).
       01  WS-IN-ACCT-NO               PIC 9(10).

       01  WS-NEXT-LOCK-NO             PIC 9(05).
       01  WS-I                        PIC 9(03).
       01  WS-RENT                     PIC 9(07)V99.
       01  WS-GST                      PIC 9(07)V99.
       01  WS-FOUND-SW                 PIC X(01).
           88  CUSTOMER-HAS-LOCKER               VALUE 'Y'.
       01  WS-SAVE-LOCK                PIC X(100).
       01  WS-LIST-CNT                 PIC 9(05).
       01  WS-DISP-AMT                 PIC ZZ,ZZ9.99.

      * RENT TABLE (ANNUAL, BEFORE GST)
       01  WS-RENT-SMALL               PIC 9(07)V99 VALUE 2000.00.
       01  WS-RENT-MEDIUM              PIC 9(07)V99 VALUE 4000.00.
       01  WS-RENT-LARGE               PIC 9(07)V99 VALUE 8000.00.
       01  WS-GST-RATE                 PIC 9V99 VALUE 0.18.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN I-O LOCK-FILE
           IF LOCK-FS-NOFILE
               OPEN OUTPUT LOCK-FILE
               CLOSE LOCK-FILE
               OPEN I-O LOCK-FILE
           END-IF
           IF NOT LOCK-FS-OK
               DISPLAY 'LOCKER MASTER OPEN FAILED, FS=' WS-LOCK-FS
               GOBACK
           END-IF
           OPEN INPUT CUST-FILE
           MOVE 'N' TO WS-CUST-OPEN-SW
           IF CUST-FS-OK
               MOVE 'Y' TO WS-CUST-OPEN-SW
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL CHOICE-BACK
               PERFORM 1000-SHOW-MENU
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-ADD-LOCKERS
                   WHEN '2' PERFORM 3000-ALLOT
                   WHEN '3' PERFORM 4000-VISIT
                   WHEN '4' PERFORM 5000-SURRENDER
                   WHEN '5' PERFORM 6000-VACANCIES
                   WHEN '6' PERFORM 7000-INQUIRY
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE LOCK-FILE
           IF WS-CUST-OPEN-SW = 'Y'
               CLOSE CUST-FILE
           END-IF
           GOBACK.

       1000-SHOW-MENU.
           DISPLAY ' '
           DISPLAY '------------- SAFE DEPOSIT LOCKERS -------------'
           IF SES-CAN-APPROVE
               DISPLAY ' 1. ADD LOCKERS (MANAGER)'
           END-IF
           DISPLAY ' 2. ALLOT LOCKER'
           DISPLAY ' 3. RECORD A VISIT'
           DISPLAY ' 4. SURRENDER LOCKER'
           DISPLAY ' 5. VACANCY LIST'
           DISPLAY ' 6. LOCKER INQUIRY'
           DISPLAY ' 9. BACK TO MAIN MENU'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT.

      *----------------------------------------------------------------*
      * ADD LOCKERS - NUMBERED ON FROM THE HIGHEST EXISTING            *
      *----------------------------------------------------------------*
       2000-ADD-LOCKERS.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'ADDING LOCKERS NEEDS A MANAGER OR ADMIN.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'BRANCH CODE      : ' WITH NO ADVANCING
           ACCEPT WS-IN-BRANCH
           DISPLAY 'SIZE (S/M/L)     : ' WITH NO ADVANCING
           ACCEPT WS-IN-SIZE
           DISPLAY 'HOW MANY (1-100) : ' WITH NO ADVANCING
           ACCEPT WS-IN-COUNT
           MOVE FUNCTION UPPER-CASE(WS-IN-SIZE)   TO LOCK-SIZE
           MOVE FUNCTION UPPER-CASE(WS-IN-BRANCH) TO WS-IN-BRANCH
           IF NOT LOCK-SIZE-VALID OR WS-IN-COUNT < 1
              OR WS-IN-COUNT > 100 OR WS-IN-BRANCH = SPACES
               DISPLAY 'BRANCH, SIZE S/M/L AND COUNT 1-100 NEEDED.'
               EXIT PARAGRAPH
           END-IF
           MOVE LOCK-SIZE TO WS-IN-SIZE
           MOVE ZERO TO WS-NEXT-LOCK-NO
           MOVE 99999 TO LOCK-NO
           START LOCK-FILE KEY IS LESS THAN OR EQUAL LOCK-NO
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ LOCK-FILE PREVIOUS RECORD
                       AT END CONTINUE
                       NOT AT END MOVE LOCK-NO TO WS-NEXT-LOCK-NO
                   END-READ
           END-START
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-IN-COUNT
               ADD 1 TO WS-NEXT-LOCK-NO
               INITIALIZE LOCK-RECORD
               MOVE WS-NEXT-LOCK-NO TO LOCK-NO
               MOVE WS-IN-BRANCH    TO LOCK-BRANCH
               MOVE WS-IN-SIZE      TO LOCK-SIZE
               SET LOCK-VACANT      TO TRUE
               WRITE LOCK-RECORD
           END-PERFORM
           DISPLAY WS-IN-COUNT ' LOCKERS ADDED, LAST NUMBER '
                   WS-NEXT-LOCK-NO.

      *----------------------------------------------------------------*
      * ALLOT                                                          *
      *----------------------------------------------------------------*
       3000-ALLOT.
           PERFORM 8000-READ-LOCKER
           IF NOT LOCK-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF NOT LOCK-VACANT
               DISPLAY 'LOCKER IS NOT VACANT.'
               EXIT PARAGRAPH
           END-IF
           MOVE LOCK-RECORD TO WS-SAVE-LOCK
           DISPLAY 'CUSTOMER ID      : ' WITH NO ADVANCING
           ACCEPT WS-IN-CUST-ID
           DISPLAY 'RENT ACCOUNT NO  : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           IF WS-CUST-OPEN-SW NOT = 'Y'
               DISPLAY 'NO CUSTOMER FILE.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-CUST-ID TO CUST-ID
           READ CUST-FILE
               INVALID KEY
                   DISPLAY 'CUSTOMER NOT FOUND.'
                   EXIT PARAGRAPH
           END-READ
           IF NOT KYC-VERIFIED
               DISPLAY 'CUSTOMER KYC IS NOT VERIFIED.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 3100-CHECK-ONE-PER-CUSTOMER
           IF CUSTOMER-HAS-LOCKER
               DISPLAY 'CUSTOMER ALREADY HAS LOCKER ' LOCK-NO '.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-SAVE-LOCK TO LOCK-RECORD
           PERFORM 8100-WORK-OUT-RENT
           MOVE WS-RENT TO WS-DISP-AMT
           DISPLAY 'ANNUAL RENT ' WS-DISP-AMT ' + GST '
                   WITH NO ADVANCING
           MOVE WS-GST TO WS-DISP-AMT
           DISPLAY WS-DISP-AMT ' WILL BE DEBITED NOW. CONFIRM (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF NOT CONFIRM-YES
               EXIT PARAGRAPH
           END-IF
           PERFORM 8200-COLLECT-RENT
           IF NOT PP-OK
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           SET LOCK-ALLOTTED   TO TRUE
           MOVE WS-IN-CUST-ID  TO LOCK-CUST-ID
           MOVE WS-IN-ACCT-NO  TO LOCK-RENT-ACCT
           MOVE WS-CURR-DATE   TO LOCK-ALLOT-DATE
           SET DU-ADD-MONTHS TO TRUE
           MOVE WS-CURR-DATE TO DU-DATE-1
           MOVE 12 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO LOCK-PAID-UNTIL
           MOVE SPACES TO LOCK-KEY-NO
           STRING 'K' LOCK-NO WS-CURR-DATE(7:2)
                  DELIMITED BY SIZE INTO LOCK-KEY-NO
           END-STRING
           MOVE ZERO TO LOCK-VISITS LOCK-LAST-VISIT
           REWRITE LOCK-RECORD
           DISPLAY 'LOCKER ' LOCK-NO ' ALLOTTED. KEY ' LOCK-KEY-NO
                   ', RENT PAID UNTIL ' LOCK-PAID-UNTIL
           MOVE 'ALLOT' TO AU-ACTION
           PERFORM 9000-AUDIT.

       3100-CHECK-ONE-PER-CUSTOMER.
           MOVE 'N' TO WS-FOUND-SW
           MOVE ZERO TO LOCK-NO
           START LOCK-FILE KEY IS NOT LESS THAN LOCK-NO
               INVALID KEY EXIT PARAGRAPH
           END-START
           PERFORM FOREVER
               READ LOCK-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF LOCK-ALLOTTED AND LOCK-CUST-ID = WS-IN-CUST-ID
                   MOVE 'Y' TO WS-FOUND-SW
                   EXIT PERFORM
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      * VISIT                                                          *
      *----------------------------------------------------------------*
       4000-VISIT.
           PERFORM 8000-READ-LOCKER
           IF NOT LOCK-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF NOT LOCK-ALLOTTED
               DISPLAY 'LOCKER IS NOT ALLOTTED.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'CUSTOMER ID: ' WITH NO ADVANCING
           ACCEPT WS-IN-CUST-ID
           IF WS-IN-CUST-ID NOT = LOCK-CUST-ID
               DISPLAY 'ACCESS DENIED - NOT THE LOCKER HOLDER.'
               MOVE 'DENIED' TO VL-EVENT
               PERFORM 4100-LOG-VISIT
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           IF LOCK-PAID-UNTIL < WS-CURR-DATE
               DISPLAY 'ACCESS DENIED - RENT OVERDUE SINCE '
                       LOCK-PAID-UNTIL
               MOVE 'RENT-DUE' TO VL-EVENT
               PERFORM 4100-LOG-VISIT
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO LOCK-VISITS
           MOVE WS-CURR-DATE TO LOCK-LAST-VISIT
           REWRITE LOCK-RECORD
           MOVE 'ACCESS' TO VL-EVENT
           PERFORM 4100-LOG-VISIT
           DISPLAY 'ACCESS GRANTED AT ' WS-CURR-TIME(1:2) ':'
                   WS-CURR-TIME(3:2) '. VISIT ' LOCK-VISITS '.'.

       4100-LOG-VISIT.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE WS-CURR-DATE  TO VL-DATE
           MOVE WS-CURR-TIME  TO VL-TIME
           MOVE LOCK-NO       TO VL-LOCK-NO
           MOVE WS-IN-CUST-ID TO VL-CUST-ID
           MOVE SES-USER-ID   TO VL-USER-ID
           OPEN EXTEND VISIT-FILE
           IF WS-VISIT-FS = '35'
               OPEN OUTPUT VISIT-FILE
           END-IF
           WRITE VISIT-LINE
           CLOSE VISIT-FILE.

      *----------------------------------------------------------------*
      * SURRENDER - NO REFUND OF RENT ALREADY PAID                     *
      *----------------------------------------------------------------*
       5000-SURRENDER.
           PERFORM 8000-READ-LOCKER
           IF NOT LOCK-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF NOT LOCK-ALLOTTED
               DISPLAY 'LOCKER IS NOT ALLOTTED.'
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           IF LOCK-PAID-UNTIL < WS-CURR-DATE
               DISPLAY 'RENT IS OVERDUE - CLEAR IT BEFORE SURRENDER.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'KEY RETURNED AND LOCKER EMPTIED (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF NOT CONFIRM-YES
               EXIT PARAGRAPH
           END-IF
           MOVE LOCK-CUST-ID TO WS-IN-CUST-ID
           SET LOCK-VACANT TO TRUE
           MOVE ZERO TO LOCK-CUST-ID LOCK-RENT-ACCT LOCK-PAID-UNTIL
           MOVE SPACES TO LOCK-KEY-NO
           REWRITE LOCK-RECORD
           DISPLAY 'LOCKER ' LOCK-NO ' SURRENDERED AND NOW VACANT.'
           MOVE 'SURRENDER' TO AU-ACTION
           PERFORM 9000-AUDIT.

       6000-VACANCIES.
           DISPLAY 'BRANCH (BLANK = ALL): ' WITH NO ADVANCING
           ACCEPT WS-IN-BRANCH
           MOVE FUNCTION UPPER-CASE(WS-IN-BRANCH) TO WS-IN-BRANCH
           MOVE ZERO TO WS-LIST-CNT LOCK-NO
           START LOCK-FILE KEY IS NOT LESS THAN LOCK-NO
               INVALID KEY
                   DISPLAY 'NO LOCKERS DEFINED.'
                   EXIT PARAGRAPH
           END-START
           DISPLAY 'LOCKER BRANCH SIZE'
           PERFORM FOREVER
               READ LOCK-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF LOCK-VACANT
                  AND (WS-IN-BRANCH = SPACES
                       OR LOCK-BRANCH = WS-IN-BRANCH)
                   ADD 1 TO WS-LIST-CNT
                   DISPLAY LOCK-NO '  ' LOCK-BRANCH '  ' LOCK-SIZE
               END-IF
           END-PERFORM
           DISPLAY WS-LIST-CNT ' VACANT LOCKER(S).'.

       7000-INQUIRY.
           PERFORM 8000-READ-LOCKER
           IF NOT LOCK-FS-OK
               EXIT PARAGRAPH
           END-IF
           DISPLAY '----------------------------------------'
           DISPLAY 'LOCKER      : ' LOCK-NO '  BRANCH ' LOCK-BRANCH
                   '  SIZE ' LOCK-SIZE
           EVALUATE TRUE
               WHEN LOCK-VACANT
                   DISPLAY 'STATUS      : VACANT'
               WHEN LOCK-OUT-OF-SERVICE
                   DISPLAY 'STATUS      : OUT OF SERVICE'
               WHEN LOCK-ALLOTTED
                   DISPLAY 'STATUS      : ALLOTTED TO ' LOCK-CUST-ID
                           ' ON ' LOCK-ALLOT-DATE
                   DISPLAY 'RENT ACCOUNT: ' LOCK-RENT-ACCT
                   DISPLAY 'PAID UNTIL  : ' LOCK-PAID-UNTIL
                   DISPLAY 'KEY NO      : ' LOCK-KEY-NO
                   DISPLAY 'VISITS      : ' LOCK-VISITS '  LAST '
                           LOCK-LAST-VISIT
           END-EVALUATE
           DISPLAY '----------------------------------------'.

      *----------------------------------------------------------------*
      * COMMON                                                         *
      *----------------------------------------------------------------*
       8000-READ-LOCKER.
           DISPLAY 'LOCKER NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-LOCK-NO
           MOVE WS-IN-LOCK-NO TO LOCK-NO
           READ LOCK-FILE
               INVALID KEY
                   DISPLAY 'LOCKER ' WS-IN-LOCK-NO ' NOT FOUND.'
           END-READ.

       8100-WORK-OUT-RENT.
           EVALUATE TRUE
               WHEN LOCK-SMALL  MOVE WS-RENT-SMALL  TO WS-RENT
               WHEN LOCK-MEDIUM MOVE WS-RENT-MEDIUM TO WS-RENT
               WHEN OTHER       MOVE WS-RENT-LARGE  TO WS-RENT
           END-EVALUATE
           COMPUTE WS-GST ROUNDED = WS-RENT * WS-GST-RATE.

       8200-COLLECT-RENT.
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           COMPUTE PP-AMOUNT = WS-RENT + WS-GST
           MOVE 'CH'          TO PP-TRAN-TYPE
           MOVE GLC-LOCKER-INCOME TO PP-CONTRA-GL
           STRING 'LOCKER RENT ' LOCK-NO DELIMITED BY SIZE
                  INTO PP-NARRATION
           END-STRING
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'RENT COULD NOT BE DEBITED, RC=' PP-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           MOVE GLC-LOCKER-INCOME TO GP-DR-GL
           MOVE GLC-GST-PAYABLE   TO GP-CR-GL
           MOVE WS-GST            TO GP-AMOUNT
           MOVE WS-IN-ACCT-NO     TO GP-REF
           MOVE 'GST ON LOCKER RENT' TO GP-NARRATION
           CALL 'GLPOST' USING GL-PARMS.

       9000-AUDIT.
           MOVE 'LOCKMNT'  TO AU-PROGRAM
           MOVE LOCK-NO    TO AU-KEY
           MOVE SPACES     TO AU-DETAIL
           STRING 'CUSTOMER ' WS-IN-CUST-ID DELIMITED BY SIZE
                  INTO AU-DETAIL
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
