       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEPPAY.
      *================================================================*
      * PROGRAM  : DEPPAY                                              *
      * PURPOSE  : TERM DEPOSIT PAYOUT WITH TAX DEDUCTED AT SOURCE.    *
      *            SHARED BY DEPMNT (PREMATURE CLOSURE) AND EODBATCH   *
      *            (MATURITY). GIVEN THE GROSS PAYOUT:                 *
      *              INTEREST = GROSS - AMOUNT DEPOSITED               *
      *              TDS      = 10% OF INTEREST WHEN INTEREST EXCEEDS  *
      *                         40,000 (50,000 FOR SENIOR CITIZENS)    *
      *              NET      = GROSS - TDS, CREDITED TO THE ACCOUNT   *
      *            GL: DR 2201 CR CUSTOMER (NET)                       *
      *                DR 5002 CR 2201 (INTEREST EXPENSE)              *
      *                DR 2201 CR 2502 (TDS PAYABLE)                   *
      *            SO TERM DEPOSITS (2201) RETURN TO ZERO FOR THE      *
      *            DEPOSIT. THE CUSTOMER CREDIT IS POSTED FIRST; IF IT *
      *            FAILS NOTHING ELSE IS POSTED.                       *
      * NOTE     : THRESHOLD IS APPLIED PER DEPOSIT FOR SIMPLICITY. A  *
      *            REAL SYSTEM AGGREGATES INTEREST PER PAN PER YEAR    *
      *            AND HONOURS FORM 15G / 15H.                         *
      * CALL     : CALL 'DEPPAY' USING DPP-PARMS DEP-RECORD            *
      * CALLS    : ACCTPOST, GLPOST                                    *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TDS-FILE ASSIGN TO 'data/TDSLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TDS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  TDS-FILE.
           COPY TDSREC.

       WORKING-STORAGE SECTION.
           COPY POSTPARM.
           COPY GLPARM.
           COPY GLCODES.

       01  WS-TDS-FS                   PIC X(02).
       01  WS-THRESHOLD                PIC 9(09)V99.
       01  WS-THRESHOLD-NORMAL         PIC 9(09)V99 VALUE 40000.00.
       01  WS-THRESHOLD-SENIOR         PIC 9(09)V99 VALUE 50000.00.
       01  WS-TDS-RATE                 PIC 9(02)V99 VALUE 10.00.
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-RUN-DATE-R REDEFINES WS-RUN-DATE.
           05  WS-RUN-YYYY             PIC 9(04).
           05  WS-RUN-MM               PIC 9(02).
           05  WS-RUN-DD               PIC 9(02).

       LINKAGE SECTION.
           COPY DPPPARM.
           COPY DEPREC.

       PROCEDURE DIVISION USING DPP-PARMS DEP-RECORD.
       0000-MAIN.
           MOVE ZERO TO DPP-RC DPP-INTEREST DPP-TDS DPP-NET
           IF DPP-GROSS > DEP-TOTAL-DEPOSITED
               COMPUTE DPP-INTEREST = DPP-GROSS - DEP-TOTAL-DEPOSITED
           END-IF
           IF DEP-SENIOR
               MOVE WS-THRESHOLD-SENIOR TO WS-THRESHOLD
           ELSE
               MOVE WS-THRESHOLD-NORMAL TO WS-THRESHOLD
           END-IF
           IF DPP-INTEREST > WS-THRESHOLD
               COMPUTE DPP-TDS ROUNDED =
                       DPP-INTEREST * WS-TDS-RATE / 100
           END-IF
           COMPUTE DPP-NET = DPP-GROSS - DPP-TDS

           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE DEP-ACCT-NO   TO PP-ACCT-NO
           MOVE DPP-NET       TO PP-AMOUNT
           MOVE 'FM'          TO PP-TRAN-TYPE
           MOVE DEP-NO        TO PP-REF-ACCT
           MOVE DPP-NARRATION TO PP-NARRATION
           MOVE 'Y'           TO PP-SYSTEM-TXN
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               MOVE PP-RETURN-CODE TO DPP-RC
               GOBACK
           END-IF

           MOVE DEP-NO TO GP-REF
           IF DPP-INTEREST > ZERO
               MOVE GLC-TD-INT-EXPENSE TO GP-DR-GL
               MOVE GLC-TERM-DEP       TO GP-CR-GL
               MOVE DPP-INTEREST       TO GP-AMOUNT
               MOVE 'TERM DEPOSIT INTEREST' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
           END-IF
           IF DPP-TDS > ZERO
               MOVE GLC-TERM-DEP       TO GP-DR-GL
               MOVE GLC-TDS-PAYABLE    TO GP-CR-GL
               MOVE DPP-TDS            TO GP-AMOUNT
               MOVE 'TDS ON DEPOSIT INTEREST' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
           END-IF
           IF DPP-INTEREST > ZERO
               PERFORM 1000-LOG-TDS
           END-IF
           GOBACK.

      *    EVERY INTEREST PAYOUT IS LOGGED (EVEN WITH NIL TDS) SO THE
      *    CERTIFICATE SHOWS THE FULL INTEREST FOR THE YEAR
       1000-LOG-TDS.
           MOVE DPP-RUN-DATE TO WS-RUN-DATE
           OPEN EXTEND TDS-FILE
           IF WS-TDS-FS = '35'
               OPEN OUTPUT TDS-FILE
           END-IF
           IF WS-TDS-FS NOT = '00'
               DISPLAY 'DEPPAY: WARNING - TDS LOG UNAVAILABLE'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-RUN-DATE TO TDS-DATE
      *    INDIAN FINANCIAL YEAR RUNS APRIL TO MARCH
           IF WS-RUN-MM >= 4
               MOVE WS-RUN-YYYY TO TDS-FY-START
           ELSE
               COMPUTE TDS-FY-START = WS-RUN-YYYY - 1
           END-IF
           MOVE DEP-ACCT-NO  TO TDS-ACCT-NO
           MOVE DEP-NO       TO TDS-DEP-NO
           MOVE DPP-INTEREST TO TDS-INTEREST
           IF DPP-TDS > ZERO
               MOVE WS-TDS-RATE TO TDS-RATE
           ELSE
               MOVE ZERO TO TDS-RATE
           END-IF
           MOVE DPP-TDS      TO TDS-AMOUNT
           MOVE DPP-NET      TO TDS-NET-PAID
           WRITE TDS-RECORD
           CLOSE TDS-FILE.
