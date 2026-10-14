       IDENTIFICATION DIVISION.
       PROGRAM-ID. SECLOAN.
      *================================================================*
      * PROGRAM  : SECLOAN                                             *
      * PURPOSE  : SECURED LOANS                                       *
      *              1 GOLD LOAN - VALUE = GRAMS x RATE PER GRAM (24K) *
      *                x CARAT / 24; UP TO 75% OF VALUE (RBI LTV CAP); *
      *                9% P.A., 3-36 MONTHS                            *
      *              2 LOAN AGAINST FD - UP TO 90% OF THE AMOUNT       *
      *                DEPOSITED; RATE = FD RATE + 2%; TENURE ENDS     *
      *                BY THE FD'S MATURITY; THE FD IS MARKED UNDER    *
      *                LIEN AND CANNOT BE CLOSED WHILE PLEDGED         *
      *              3 RELEASE SECURITY ONCE THE LOAN IS CLOSED        *
      *              4 SET TODAY'S GOLD RATE (MANAGER)                 *
      *            REPAYMENT USES THE NORMAL LOAN SCREEN (LNEMIPAY).   *
      * GL       : GOLD DR 1220, AGAINST FD DR 1230, CR CUSTOMER       *
      * FILES    : LOANMAST, DEPMAST (I-O), GOLDRATE (I-O)             *
      * CALLS    : ACCTPOST, DATEUTIL, AUDITLOG                        *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LOAN-NO
               FILE STATUS IS WS-LOAN-FS.
           SELECT DEP-FILE ASSIGN TO 'data/DEPMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS DEP-NO
               FILE STATUS IS WS-DEP-FS.
           SELECT RATE-FILE ASSIGN TO 'data/GOLDRATE.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RATE-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  DEP-FILE.
           COPY DEPREC.
       FD  RATE-FILE.
       01  RATE-RECORD.
           05  RATE-PER-GRAM           PIC 9(07)V99.
           05  RATE-SET-DATE           PIC 9(08).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY DATEPARM.
           COPY GLCODES.
           COPY SESSION.
           COPY AUDPARM.

       01  WS-LOAN-FS                  PIC X(02).
           88  LOAN-FS-OK                        VALUE '00'.
           88  LOAN-FS-NOFILE                    VALUE '35'.
       01  WS-DEP-FS                   PIC X(02).
           88  DEP-FS-OK                         VALUE '00'.
       01  WS-RATE-FS                  PIC X(02).
       01  WS-DEP-OPEN-SW              PIC X(01).

       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
           88  CHOICE-BACK                       VALUE '9'.
       01  WS-CONFIRM                  PIC X(01).
           88  CONFIRM-YES                       VALUES 'Y' 'y'.

       01  WS-IN.
           05  WS-IN-ACCT-NO           PIC 9(10).
           05  WS-IN-GRAMS             PIC 9(05)V999.
           05  WS-IN-CARAT             PIC 9(02).
           05  WS-IN-AMOUNT            PIC 9(11)V99.
           05  WS-IN-TENURE            PIC 9(03).
           05  WS-IN-DEP-NO            PIC 9(10).
           05  WS-IN-LOAN-NO           PIC 9(10).
           05  WS-IN-RATE              PIC 9(07)V99.

       01  WS-GOLD-RATE                PIC 9(07)V99 VALUE 7000.00.
       01  WS-RATE-DATE                PIC 9(08) VALUE ZERO.
       01  WS-COLLATERAL               PIC 9(11)V99.
       01  WS-MAX-LOAN                 PIC 9(11)V99.
       01  WS-LTV-GOLD                 PIC V99 VALUE 0.75.
       01  WS-LTV-FD                   PIC V99 VALUE 0.90.
       01  WS-GOLD-INT-RATE            PIC 9(02)V9(04) VALUE 09.0000.
       01  WS-FD-SPREAD                PIC 9(02)V9(04) VALUE 02.0000.
       01  WS-MONTHS-LEFT              PIC S9(05).
       01  WS-NEXT-LOAN-NO             PIC 9(10).
       01  WS-MONTHLY-RATE             COMP-2.
       01  WS-POWER                    COMP-2.
       01  WS-NEW-LOAN.
           05  WS-NL-TYPE              PIC X(01).
           05  WS-NL-RATE              PIC 9(02)V9(04).
           05  WS-NL-CONTRA            PIC 9(04).

       01  WS-E-AMT                    PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-E-RATE                   PIC Z9.99.
       01  WS-E-GRAMS                  PIC ZZ,ZZ9.999.
       01  WS-E-MONTHS                 PIC ZZ9.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN I-O LOAN-FILE
           IF LOAN-FS-NOFILE
               OPEN OUTPUT LOAN-FILE
               CLOSE LOAN-FILE
               OPEN I-O LOAN-FILE
           END-IF
           IF NOT LOAN-FS-OK
               DISPLAY 'LOAN MASTER OPEN FAILED, FS=' WS-LOAN-FS
               GOBACK
           END-IF
           OPEN I-O DEP-FILE
           MOVE 'N' TO WS-DEP-OPEN-SW
           IF DEP-FS-OK
               MOVE 'Y' TO WS-DEP-OPEN-SW
           END-IF
           PERFORM 8000-LOAD-GOLD-RATE
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL CHOICE-BACK
               PERFORM 1000-SHOW-MENU
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-GOLD-LOAN
                   WHEN '2' PERFORM 3000-FD-LOAN
                   WHEN '3' PERFORM 4000-RELEASE-SECURITY
                   WHEN '4' PERFORM 5000-SET-GOLD-RATE
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE LOAN-FILE
           IF WS-DEP-OPEN-SW = 'Y'
               CLOSE DEP-FILE
           END-IF
           GOBACK.

       1000-SHOW-MENU.
           DISPLAY ' '
           DISPLAY '------------- SECURED LOANS -------------'
           MOVE WS-GOLD-RATE TO WS-E-AMT
           DISPLAY ' GOLD RATE (24K, PER GRAM): ' WS-E-AMT
           DISPLAY ' 1. GOLD LOAN'
           DISPLAY ' 2. LOAN AGAINST FIXED DEPOSIT'
           DISPLAY ' 3. RELEASE SECURITY (CLOSED LOAN)'
           IF SES-CAN-APPROVE
               DISPLAY ' 4. SET GOLD RATE (MANAGER)'
           END-IF
           DISPLAY ' 9. BACK TO MAIN MENU'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT.

      *================================================================*
      * GOLD LOAN                                                      *
      *================================================================*
       2000-GOLD-LOAN.
           DISPLAY 'LINKED ACCOUNT NO     : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'GOLD WEIGHT (GRAMS)   : ' WITH NO ADVANCING
           ACCEPT WS-IN-GRAMS
           DISPLAY 'PURITY (18/22/24 CARAT): ' WITH NO ADVANCING
           ACCEPT WS-IN-CARAT
           IF WS-IN-CARAT NOT = 18 AND WS-IN-CARAT NOT = 22
              AND WS-IN-CARAT NOT = 24
               DISPLAY 'PURITY MUST BE 18, 22 OR 24 CARAT.'
               EXIT PARAGRAPH
           END-IF
           IF WS-IN-GRAMS < 1
               DISPLAY 'MINIMUM 1 GRAM.'
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-COLLATERAL ROUNDED =
                   WS-IN-GRAMS * WS-GOLD-RATE * WS-IN-CARAT / 24
           COMPUTE WS-MAX-LOAN ROUNDED = WS-COLLATERAL * WS-LTV-GOLD
           MOVE WS-COLLATERAL TO WS-E-AMT
           DISPLAY 'GOLD VALUE            : ' WS-E-AMT
           MOVE WS-MAX-LOAN TO WS-E-AMT
           DISPLAY 'MAXIMUM LOAN (75%)    : ' WS-E-AMT
           DISPLAY 'LOAN AMOUNT           : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           DISPLAY 'TENURE (3-36 MONTHS)  : ' WITH NO ADVANCING
           ACCEPT WS-IN-TENURE
           IF WS-IN-AMOUNT < 5000 OR WS-IN-AMOUNT > WS-MAX-LOAN
               DISPLAY 'AMOUNT MUST BE 5,000 TO THE MAXIMUM SHOWN.'
               EXIT PARAGRAPH
           END-IF
           IF WS-IN-TENURE < 3 OR WS-IN-TENURE > 36
               DISPLAY 'TENURE MUST BE 3 TO 36 MONTHS.'
               EXIT PARAGRAPH
           END-IF
           MOVE 'G'              TO WS-NL-TYPE
           MOVE WS-GOLD-INT-RATE TO WS-NL-RATE
           MOVE GLC-GOLD-LOANS   TO WS-NL-CONTRA
           PERFORM 7000-DISBURSE
           IF NOT PP-OK
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-GRAMS   TO LOAN-GOLD-GRAMS
           MOVE WS-IN-CARAT   TO LOAN-GOLD-CARAT
           PERFORM 7100-WRITE-LOAN.

      *================================================================*
      * LOAN AGAINST FD                                                *
      *================================================================*
       3000-FD-LOAN.
           IF WS-DEP-OPEN-SW NOT = 'Y'
               DISPLAY 'NO DEPOSITS ON FILE.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'FIXED DEPOSIT NO      : ' WITH NO ADVANCING
           ACCEPT WS-IN-DEP-NO
           MOVE WS-IN-DEP-NO TO DEP-NO
           READ DEP-FILE
               INVALID KEY
                   DISPLAY 'DEPOSIT NOT FOUND.'
                   EXIT PARAGRAPH
           END-READ
           EVALUATE TRUE
               WHEN NOT DEP-FD OR NOT DEP-ACTIVE
                   DISPLAY 'ONLY AN ACTIVE FD CAN BE PLEDGED.'
                   EXIT PARAGRAPH
               WHEN DEP-LIEN-LOAN-NO > ZERO
                   DISPLAY 'DEPOSIT IS ALREADY PLEDGED FOR LOAN '
                           DEP-LIEN-LOAN-NO
                   EXIT PARAGRAPH
           END-EVALUATE
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE WS-CURR-DATE TO DU-DATE-1
           MOVE DEP-MATURITY-DATE TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           COMPUTE WS-MONTHS-LEFT = DU-RESULT-DAYS / 30
           IF WS-MONTHS-LEFT < 3
               DISPLAY 'DEPOSIT MATURES TOO SOON TO LEND AGAINST.'
               EXIT PARAGRAPH
           END-IF
           MOVE DEP-ACCT-NO TO WS-IN-ACCT-NO
           MOVE DEP-TOTAL-DEPOSITED TO WS-COLLATERAL
           COMPUTE WS-MAX-LOAN ROUNDED = WS-COLLATERAL * WS-LTV-FD
           COMPUTE WS-NL-RATE = DEP-RATE + WS-FD-SPREAD
           MOVE WS-MAX-LOAN TO WS-E-AMT
           DISPLAY 'MAXIMUM LOAN (90%)    : ' WS-E-AMT
           MOVE WS-NL-RATE TO WS-E-RATE
           DISPLAY 'INTEREST RATE         : ' WS-E-RATE '%'
           DISPLAY 'LOAN AMOUNT           : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           MOVE WS-MONTHS-LEFT TO WS-E-MONTHS
           DISPLAY 'TENURE (MAX ' WS-E-MONTHS ' MONTHS)     : '
                   WITH NO ADVANCING
           ACCEPT WS-IN-TENURE
           IF WS-IN-AMOUNT < 5000 OR WS-IN-AMOUNT > WS-MAX-LOAN
               DISPLAY 'AMOUNT MUST BE 5,000 TO THE MAXIMUM SHOWN.'
               EXIT PARAGRAPH
           END-IF
           IF WS-IN-TENURE < 1 OR WS-IN-TENURE > WS-MONTHS-LEFT
               DISPLAY 'TENURE MUST END BEFORE THE DEPOSIT MATURES.'
               EXIT PARAGRAPH
           END-IF
           MOVE 'F'               TO WS-NL-TYPE
           MOVE GLC-DEPOSIT-LOANS TO WS-NL-CONTRA
           PERFORM 7000-DISBURSE
           IF NOT PP-OK
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-DEP-NO TO LOAN-LIEN-DEP-NO
           PERFORM 7100-WRITE-LOAN
      *    MARK THE LIEN
           MOVE WS-IN-DEP-NO TO DEP-NO
           READ DEP-FILE
           MOVE LOAN-NO TO DEP-LIEN-LOAN-NO
           REWRITE DEP-RECORD
           DISPLAY 'DEPOSIT ' DEP-NO ' MARKED UNDER LIEN.'.

      *================================================================*
      * RELEASE SECURITY                                               *
      *================================================================*
       4000-RELEASE-SECURITY.
           DISPLAY 'LOAN NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-LOAN-NO
           MOVE WS-IN-LOAN-NO TO LOAN-NO
           READ LOAN-FILE
               INVALID KEY
                   DISPLAY 'LOAN NOT FOUND.'
                   EXIT PARAGRAPH
           END-READ
           IF NOT LOAN-SECURED
               DISPLAY 'NOT A SECURED LOAN.'
               EXIT PARAGRAPH
           END-IF
           IF NOT LOAN-CLOSED
               MOVE LOAN-OUTSTANDING TO WS-E-AMT
               DISPLAY 'LOAN STILL OWES ' WS-E-AMT
                       ' - SECURITY CANNOT BE RELEASED.'
               EXIT PARAGRAPH
           END-IF
           IF LOAN-GOLD
               IF LOAN-GOLD-GRAMS = ZERO
                   DISPLAY 'GOLD ALREADY RETURNED.'
                   EXIT PARAGRAPH
               END-IF
               MOVE LOAN-GOLD-GRAMS TO WS-E-GRAMS
               DISPLAY 'RETURN ' WS-E-GRAMS ' GRAMS OF '
                       LOAN-GOLD-CARAT 'K GOLD TO THE BORROWER.'
               MOVE ZERO TO LOAN-GOLD-GRAMS LOAN-COLLATERAL-VALUE
               REWRITE LOAN-RECORD
               MOVE 'GOLD-RETURN' TO AU-ACTION
           ELSE
               IF LOAN-LIEN-DEP-NO = ZERO
                   DISPLAY 'LIEN ALREADY RELEASED.'
                   EXIT PARAGRAPH
               END-IF
               MOVE LOAN-LIEN-DEP-NO TO DEP-NO
               READ DEP-FILE
                   INVALID KEY
                       DISPLAY 'PLEDGED DEPOSIT NOT FOUND.'
                       EXIT PARAGRAPH
               END-READ
               MOVE ZERO TO DEP-LIEN-LOAN-NO
               REWRITE DEP-RECORD
               MOVE ZERO TO LOAN-LIEN-DEP-NO
               REWRITE LOAN-RECORD
               DISPLAY 'LIEN ON DEPOSIT ' DEP-NO ' RELEASED.'
               MOVE 'LIEN-RELEASE' TO AU-ACTION
           END-IF
           PERFORM 9000-AUDIT.

      *================================================================*
      * GOLD RATE                                                      *
      *================================================================*
       5000-SET-GOLD-RATE.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'GOLD RATE CHANGES NEED A MANAGER OR ADMIN.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'NEW 24K RATE PER GRAM: ' WITH NO ADVANCING
           ACCEPT WS-IN-RATE
           IF WS-IN-RATE < 1000 OR WS-IN-RATE > 99999
               DISPLAY 'RATE LOOKS WRONG (1,000 - 99,999 PER GRAM).'
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE WS-IN-RATE   TO WS-GOLD-RATE RATE-PER-GRAM
           MOVE WS-CURR-DATE TO RATE-SET-DATE
           OPEN OUTPUT RATE-FILE
           WRITE RATE-RECORD
           CLOSE RATE-FILE
           DISPLAY 'GOLD RATE UPDATED. RUN GOLDREVL TO REVALUE LOANS.'
           MOVE ZERO TO LOAN-NO
           MOVE 'GOLD-RATE' TO AU-ACTION
           PERFORM 9000-AUDIT.

      *================================================================*
      * COMMON                                                         *
      *================================================================*
       7000-DISBURSE.
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'LINKED ACCOUNT NOT FOUND.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 7200-NEXT-LOAN-NO
           DISPLAY 'DISBURSE TO ' WS-IN-ACCT-NO ' (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF NOT CONFIRM-YES
               MOVE 99 TO PP-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE WS-IN-ACCT-NO   TO PP-ACCT-NO
           MOVE WS-IN-AMOUNT    TO PP-AMOUNT
           MOVE 'LD'            TO PP-TRAN-TYPE
           MOVE WS-NEXT-LOAN-NO TO PP-REF-ACCT
           MOVE WS-NL-CONTRA    TO PP-CONTRA-GL
           IF WS-NL-TYPE = 'G'
               MOVE 'GOLD LOAN DISBURSAL' TO PP-NARRATION
           ELSE
               MOVE 'LOAN AGAINST FD' TO PP-NARRATION
           END-IF
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'DISBURSAL FAILED, RC=' PP-RETURN-CODE
           END-IF.

       7100-WRITE-LOAN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE WS-NEXT-LOAN-NO TO LOAN-NO
           MOVE WS-IN-ACCT-NO   TO LOAN-ACCT-NO
           MOVE WS-NL-TYPE      TO LOAN-TYPE
           SET LOAN-ACTIVE      TO TRUE
           MOVE WS-IN-AMOUNT    TO LOAN-PRINCIPAL LOAN-OUTSTANDING
           MOVE WS-NL-RATE      TO LOAN-RATE
           MOVE WS-IN-TENURE    TO LOAN-TENURE
           MOVE WS-COLLATERAL   TO LOAN-COLLATERAL-VALUE
           MOVE 'N'             TO LOAN-MARGIN-CALL
           COMPUTE WS-MONTHLY-RATE = LOAN-RATE / 1200
           COMPUTE WS-POWER = (1 + WS-MONTHLY-RATE) ** LOAN-TENURE
           COMPUTE LOAN-EMI ROUNDED = LOAN-PRINCIPAL * WS-MONTHLY-RATE
                   * WS-POWER / (WS-POWER - 1)
           MOVE WS-CURR-DATE    TO LOAN-DISB-DATE
           SET DU-ADD-MONTHS TO TRUE
           MOVE WS-CURR-DATE TO DU-DATE-1
           MOVE 1 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO LOAN-NEXT-DUE-DATE
           WRITE LOAN-RECORD
               INVALID KEY
                   DISPLAY 'CRITICAL: LOAN WRITE FAILED FS=' WS-LOAN-FS
                   EXIT PARAGRAPH
           END-WRITE
           MOVE LOAN-EMI TO WS-E-AMT
           DISPLAY 'LOAN ' LOAN-NO ' DISBURSED. EMI ' WS-E-AMT
                   ', FIRST DUE ' LOAN-NEXT-DUE-DATE
           MOVE 'SEC-LOAN' TO AU-ACTION
           PERFORM 9000-AUDIT.

       7200-NEXT-LOAN-NO.
           MOVE 5000000000 TO WS-NEXT-LOAN-NO
           MOVE 9999999999 TO LOAN-NO
           START LOAN-FILE KEY IS LESS THAN OR EQUAL LOAN-NO
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ LOAN-FILE PREVIOUS RECORD
                       AT END CONTINUE
                       NOT AT END MOVE LOAN-NO TO WS-NEXT-LOAN-NO
                   END-READ
           END-START
           ADD 1 TO WS-NEXT-LOAN-NO
           INITIALIZE LOAN-RECORD.

       8000-LOAD-GOLD-RATE.
           OPEN INPUT RATE-FILE
           IF WS-RATE-FS = '00'
               READ RATE-FILE
                   NOT AT END
                       IF RATE-PER-GRAM IS NUMERIC
                           MOVE RATE-PER-GRAM TO WS-GOLD-RATE
                           MOVE RATE-SET-DATE TO WS-RATE-DATE
                       END-IF
               END-READ
               CLOSE RATE-FILE
           END-IF.

       9000-AUDIT.
           MOVE 'SECLOAN' TO AU-PROGRAM
           MOVE LOAN-NO   TO AU-KEY
           MOVE SPACES    TO AU-DETAIL
           MOVE WS-IN-AMOUNT TO WS-E-AMT
           STRING 'AMOUNT ' FUNCTION TRIM(WS-E-AMT)
                  DELIMITED BY SIZE INTO AU-DETAIL
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
