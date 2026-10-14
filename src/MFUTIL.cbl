       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFUTIL.
      *================================================================*
      * PROGRAM  : MFUTIL                                              *
      * PURPOSE  : MONEY SIDE OF THIRD-PARTY PRODUCTS, SHARED BY MFMNT *
      *            (COUNTER) AND MFBATCH (SIP / PREMIUM RUN).          *
      *            BUY     - DEBIT ACCOUNT (MP, CONTRA 2906 COLLECTIONS*
      *                      PAYABLE TO THE AMC), ALLOT UNITS AT NAV,  *
      *                      TRUNCATED TO 3 DECIMALS                   *
      *            REDEEM  - UNITS x NAV, LESS EXIT LOAD INSIDE THE    *
      *                      LOAD PERIOD, CREDIT ACCOUNT (MR, CONTRA   *
      *                      1302 SETTLEMENT); COST REDUCED PRO RATA   *
      *            PREMIUM - DEBIT ACCOUNT (IP, CONTRA 2906), MOVE THE *
      *                      NEXT DUE DATE, BOOK COMMISSION RECEIVABLE *
      *                      FROM THE INSURER (DR 1302 CR 4106)        *
      * CALL     : CALL 'MFUTIL' USING MFU-PARMS  (COPY MFUPARM)       *
      * FILES    : MFSCHEME (INPUT), MFHOLD (I-O), MFTXN (EXTEND)      *
      * CALLS    : ACCTPOST, GLPOST, DATEUTIL                          *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MFS-FILE ASSIGN TO 'data/MFSCHEME.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MS-CODE
               FILE STATUS IS WS-MFS-FS.
           SELECT MFH-FILE ASSIGN TO 'data/MFHOLD.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MH-KEY
               FILE STATUS IS WS-MFH-FS.
           SELECT MFT-FILE ASSIGN TO 'data/MFTXN.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MFT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  MFS-FILE.
           COPY MFSREC.
       FD  MFH-FILE.
           COPY MFHREC.
       FD  MFT-FILE.
           COPY MFTREC.

       WORKING-STORAGE SECTION.
           COPY SESSION.
           COPY POSTPARM.
           COPY GLPARM.
           COPY GLCODES.
           COPY DATEPARM.
       01  WS-MFS-FS                   PIC X(02).
       01  WS-MFH-FS                   PIC X(02).
       01  WS-MFT-FS                   PIC X(02).
       01  WS-NEW-HOLDING              PIC X(01).
       01  WS-UNITS                    PIC 9(09)V9(03).
       01  WS-COST-OUT                 PIC 9(11)V99.
       01  WS-DAYS-HELD                PIC S9(07).
       01  WS-MONTHS                   PIC 9(02).

       LINKAGE SECTION.
           COPY MFUPARM.

       PROCEDURE DIVISION USING MFU-PARMS.
       0000-MAIN.
           SET MU-OK TO TRUE
           MOVE ZERO TO MU-NAV MU-VALUE MU-LOAD MU-NET MU-COMMISSION
                        MU-POST-RC
           IF MU-RUN-DATE NOT NUMERIC OR MU-RUN-DATE = ZERO
               ACCEPT MU-RUN-DATE FROM DATE YYYYMMDD
           END-IF
           OPEN INPUT MFS-FILE
           IF WS-MFS-FS NOT = '00'
               SET MU-NO-PRODUCT TO TRUE
               GOBACK
           END-IF
           MOVE MU-CODE TO MS-CODE
           READ MFS-FILE
               INVALID KEY SET MU-NO-PRODUCT TO TRUE
           END-READ
           CLOSE MFS-FILE
           IF NOT MU-OK
               GOBACK
           END-IF
           OPEN I-O MFH-FILE
           IF WS-MFH-FS = '35'
               OPEN OUTPUT MFH-FILE
               CLOSE MFH-FILE
               OPEN I-O MFH-FILE
           END-IF
           IF WS-MFH-FS NOT = '00'
               SET MU-FILE-ERROR TO TRUE
               GOBACK
           END-IF
           MOVE MU-ACCT-NO TO MH-ACCT-NO
           MOVE MU-CODE    TO MH-CODE
           MOVE 'N' TO WS-NEW-HOLDING
           READ MFH-FILE
               INVALID KEY MOVE 'Y' TO WS-NEW-HOLDING
           END-READ
           EVALUATE TRUE
               WHEN MU-BUY     PERFORM 1000-BUY
               WHEN MU-REDEEM  PERFORM 2000-REDEEM
               WHEN MU-PREMIUM PERFORM 3000-PREMIUM
               WHEN OTHER      SET MU-NO-PRODUCT TO TRUE
           END-EVALUATE
           CLOSE MFH-FILE
           GOBACK.

      *----------------------------------------------------------------*
      * BUY                                                            *
      *----------------------------------------------------------------*
       1000-BUY.
           IF NOT MS-MUTUAL-FUND OR NOT MS-OPEN OR MS-NAV = ZERO
               SET MU-NO-PRODUCT TO TRUE
               EXIT PARAGRAPH
           END-IF
           IF MU-AMOUNT < MS-MIN-AMT
               SET MU-BELOW-MINIMUM TO TRUE
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-UNITS = MU-AMOUNT / MS-NAV
           IF WS-UNITS = ZERO
               SET MU-BELOW-MINIMUM TO TRUE
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE MU-ACCT-NO TO PP-ACCT-NO
           MOVE MU-AMOUNT  TO PP-AMOUNT
           MOVE 'MP'       TO PP-TRAN-TYPE
           IF MU-SOURCE = 'S'
               STRING 'SIP ' MS-NAME DELIMITED BY SIZE
                      INTO PP-NARRATION
           ELSE
               STRING 'MF ' MS-NAME DELIMITED BY SIZE
                      INTO PP-NARRATION
           END-IF
           PERFORM 8000-POST
           IF NOT MU-OK
               EXIT PARAGRAPH
           END-IF
           IF WS-NEW-HOLDING = 'Y'
               PERFORM 8100-NEW-HOLDING
           END-IF
           SET MH-ACTIVE TO TRUE
           ADD WS-UNITS  TO MH-UNITS
           ADD MU-AMOUNT TO MH-COST
           MOVE MU-RUN-DATE TO MH-LAST-TXN-DATE
           PERFORM 8200-SAVE-HOLDING
           MOVE WS-UNITS  TO MU-UNITS
           MOVE MS-NAV    TO MU-NAV
           MOVE MU-AMOUNT TO MU-NET
           MOVE MU-SOURCE TO MT-TYPE
           PERFORM 8300-LOG.

      *----------------------------------------------------------------*
      * REDEEM                                                         *
      *----------------------------------------------------------------*
       2000-REDEEM.
           IF WS-NEW-HOLDING = 'Y' OR NOT MS-MUTUAL-FUND
              OR MH-UNITS = ZERO
               SET MU-NO-HOLDING TO TRUE
               EXIT PARAGRAPH
           END-IF
           IF MU-UNITS = ZERO
               MOVE MH-UNITS TO MU-UNITS
           END-IF
           IF MU-UNITS > MH-UNITS
               SET MU-NO-HOLDING TO TRUE
               EXIT PARAGRAPH
           END-IF
           COMPUTE MU-VALUE ROUNDED = MU-UNITS * MS-NAV
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE MH-FIRST-DATE TO DU-DATE-1
           MOVE MU-RUN-DATE   TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DAYS TO WS-DAYS-HELD
           IF WS-DAYS-HELD < MS-LOAD-DAYS
               COMPUTE MU-LOAD ROUNDED =
                       MU-VALUE * MS-EXIT-LOAD-PCT / 100
           END-IF
           COMPUTE MU-NET = MU-VALUE - MU-LOAD
           IF MU-NET = ZERO
               SET MU-BELOW-MINIMUM TO TRUE
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE MU-ACCT-NO TO PP-ACCT-NO
           MOVE MU-NET     TO PP-AMOUNT
           MOVE 'MR'       TO PP-TRAN-TYPE
           STRING 'REDEEM ' MS-NAME DELIMITED BY SIZE
                  INTO PP-NARRATION
           PERFORM 8000-POST
           IF NOT MU-OK
               EXIT PARAGRAPH
           END-IF
      *    COST OF THE UNITS SOLD, PRO RATA
           COMPUTE WS-COST-OUT ROUNDED =
                   MH-COST * MU-UNITS / MH-UNITS
           SUBTRACT MU-UNITS    FROM MH-UNITS
           SUBTRACT WS-COST-OUT FROM MH-COST
           ADD MU-NET TO MH-REDEEMED-VALUE
           IF MH-UNITS = ZERO
               MOVE ZERO TO MH-COST
               SET MH-CLOSED TO TRUE
           END-IF
           MOVE MU-RUN-DATE TO MH-LAST-TXN-DATE
           PERFORM 8200-SAVE-HOLDING
           MOVE MS-NAV TO MU-NAV
           MOVE 'R'    TO MT-TYPE
           PERFORM 8300-LOG.

      *----------------------------------------------------------------*
      * PREMIUM                                                        *
      *----------------------------------------------------------------*
       3000-PREMIUM.
           IF WS-NEW-HOLDING = 'Y' OR NOT MS-INSURANCE
              OR MH-CLOSED
               SET MU-NO-HOLDING TO TRUE
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE MU-ACCT-NO  TO PP-ACCT-NO
           MOVE MH-PREMIUM  TO PP-AMOUNT
           MOVE 'IP'        TO PP-TRAN-TYPE
           STRING 'PREMIUM ' MH-POLICY-NO DELIMITED BY SIZE
                  INTO PP-NARRATION
           PERFORM 8000-POST
           IF NOT MU-OK
               EXIT PARAGRAPH
           END-IF
           EVALUATE MH-FREQUENCY
               WHEN 'M' MOVE 1  TO WS-MONTHS
               WHEN 'Q' MOVE 3  TO WS-MONTHS
               WHEN 'H' MOVE 6  TO WS-MONTHS
               WHEN OTHER MOVE 12 TO WS-MONTHS
           END-EVALUATE
           IF MH-NEXT-DUE = ZERO
               MOVE MU-RUN-DATE TO MH-NEXT-DUE
           END-IF
           SET DU-ADD-MONTHS TO TRUE
           MOVE MH-NEXT-DUE  TO DU-DATE-1
           MOVE WS-MONTHS    TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO MH-NEXT-DUE
           ADD 1 TO MH-PREMIUMS-PAID
           SET MH-ACTIVE TO TRUE
           MOVE MU-RUN-DATE TO MH-LAST-TXN-DATE
           PERFORM 8200-SAVE-HOLDING
           MOVE MH-PREMIUM TO MU-NET
           MOVE ZERO TO MU-UNITS
           COMPUTE MU-COMMISSION ROUNDED =
                   MH-PREMIUM * MS-COMM-PCT / 100
           IF MU-COMMISSION > ZERO
               MOVE GLC-PAYMENT-SETTLE TO GP-DR-GL
               MOVE GLC-MF-COMMISSION  TO GP-CR-GL
               MOVE MU-COMMISSION      TO GP-AMOUNT
               MOVE MH-POLICY-NO       TO GP-REF
               MOVE 'INSURANCE COMMISSION' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
           END-IF
           MOVE 'I' TO MT-TYPE
           PERFORM 8300-LOG.

      *----------------------------------------------------------------*
      * COMMON                                                         *
      *----------------------------------------------------------------*
       8000-POST.
           MOVE MU-ACCT-NO TO PP-REF-ACCT
           CALL 'ACCTPOST' USING POST-PARMS
           MOVE PP-RETURN-CODE TO MU-POST-RC
           EVALUATE TRUE
               WHEN PP-OK          CONTINUE
               WHEN PP-INSUFFICIENT SET MU-NO-FUNDS TO TRUE
               WHEN OTHER          SET MU-POST-FAILED TO TRUE
           END-EVALUATE.

       8100-NEW-HOLDING.
           INITIALIZE MFH-RECORD
           MOVE MU-ACCT-NO  TO MH-ACCT-NO
           MOVE MU-CODE     TO MH-CODE
           MOVE MU-RUN-DATE TO MH-FIRST-DATE.

       8200-SAVE-HOLDING.
           IF WS-NEW-HOLDING = 'Y'
               WRITE MFH-RECORD
                   INVALID KEY SET MU-FILE-ERROR TO TRUE
               END-WRITE
               MOVE 'N' TO WS-NEW-HOLDING
           ELSE
               REWRITE MFH-RECORD
                   INVALID KEY SET MU-FILE-ERROR TO TRUE
               END-REWRITE
           END-IF.

       8300-LOG.
           OPEN EXTEND MFT-FILE
           IF WS-MFT-FS = '35'
               OPEN OUTPUT MFT-FILE
           END-IF
           MOVE MU-RUN-DATE TO MT-DATE
           MOVE MU-ACCT-NO  TO MT-ACCT-NO
           MOVE MU-CODE     TO MT-CODE
           MOVE MU-NET      TO MT-AMOUNT
           MOVE MU-UNITS    TO MT-UNITS
           MOVE MS-NAV      TO MT-NAV
           MOVE MU-LOAD     TO MT-LOAD
           MOVE MU-REF      TO MT-REF
           IF SES-USER-ID = SPACES OR LOW-VALUES
               MOVE 'SYSTEM' TO MT-USER
           ELSE
               MOVE SES-USER-ID TO MT-USER
           END-IF
           WRITE MFT-RECORD
           CLOSE MFT-FILE.
