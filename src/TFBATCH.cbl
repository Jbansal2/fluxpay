       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFBATCH.
      *================================================================*
      * PROGRAM  : TFBATCH                                             *
      * PURPOSE  : DAILY TRADE FINANCE RUN                             *
      *            1. EXPIRY: AN ACTIVE LC PAST ITS EXPIRY DATE, OR AN *
      *               ACTIVE BG PAST ITS CLAIM PERIOD, IS CLOSED AS    *
      *               EXPIRED AND ITS REMAINING MARGIN IS RETURNED TO  *
      *               THE APPLICANT (DR 2905, CR CUSTOMER - TYPE TR).  *
      *            2. CONTINGENT LIABILITY REGISTER: WHAT THE BANK     *
      *               COULD STILL BE CALLED ON TO PAY (BG AMOUNT, LC   *
      *               UNDRAWN BALANCE), LESS MARGIN HELD, AND THE      *
      *               INSTRUMENTS EXPIRING IN THE NEXT 30 DAYS.        *
      * INPUT    : RUN DATE YYYYMMDD (BLANK = TODAY)                   *
      * FILES    : TFMAST (I-O)                                        *
      * CALLS    : ACCTPOST, DATEUTIL, AUDITLOG                        *
      * RETURN   : 0 = OK, 4 = A MARGIN COULD NOT BE RETURNED          *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TF-FILE ASSIGN TO 'data/TFMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS SEQUENTIAL
               RECORD KEY IS TF-NO FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  TF-FILE.
           COPY TFREC.

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-SOON-DATE                PIC 9(08).
       01  WS-EOF-SW                   PIC X(01) VALUE 'N'.
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-RETURN-CODE              PIC 9(02) VALUE ZERO.
       01  WS-EXPOSURE                 PIC 9(11)V99.
       01  WS-TOTALS.
           05  WS-EXPIRED              PIC 9(05) VALUE ZERO.
           05  WS-LIVE-BG              PIC 9(05) VALUE ZERO.
           05  WS-LIVE-LC              PIC 9(05) VALUE ZERO.
           05  WS-BG-EXPOSURE          PIC 9(13)V99 VALUE ZERO.
           05  WS-LC-EXPOSURE          PIC 9(13)V99 VALUE ZERO.
           05  WS-MARGIN-TOTAL         PIC 9(13)V99 VALUE ZERO.
           05  WS-NET-EXPOSURE         PIC S9(13)V99.
           05  WS-EXPIRING-SOON        PIC 9(05) VALUE ZERO.
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-CNT                      PIC ZZ,ZZ9.
           COPY POSTPARM.
           COPY DATEPARM.
           COPY AUDPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'RUN DATE (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           IF WS-IN-DATE = SPACES OR WS-IN-DATE NOT NUMERIC
               ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           ELSE
               MOVE WS-IN-DATE TO WS-RUN-DATE
           END-IF
           SET DU-ADD-DAYS TO TRUE
           MOVE WS-RUN-DATE TO DU-DATE-1
           MOVE 30 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO WS-SOON-DATE
           OPEN I-O TF-FILE
           IF WS-FS NOT = '00'
               DISPLAY 'TFBATCH: NO TRADE FINANCE INSTRUMENTS'
               GOBACK
           END-IF
           PERFORM UNTIL END-OF-FILE
               READ TF-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF TF-ACTIVE
                           PERFORM 1000-ONE-INSTRUMENT
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TF-FILE
           PERFORM 9000-REPORT
           MOVE WS-RETURN-CODE TO RETURN-CODE
           GOBACK.

       1000-ONE-INSTRUMENT.
           IF WS-RUN-DATE > TF-CLAIM-END
               PERFORM 2000-EXPIRE
               EXIT PARAGRAPH
           END-IF
           IF TF-GUARANTEE
               MOVE TF-AMOUNT TO WS-EXPOSURE
               ADD 1 TO WS-LIVE-BG
               ADD WS-EXPOSURE TO WS-BG-EXPOSURE
           ELSE
               COMPUTE WS-EXPOSURE = TF-AMOUNT - TF-UTILISED
               ADD 1 TO WS-LIVE-LC
               ADD WS-EXPOSURE TO WS-LC-EXPOSURE
           END-IF
           ADD TF-MARGIN-HELD TO WS-MARGIN-TOTAL
           IF TF-EXPIRY-DATE <= WS-SOON-DATE
               ADD 1 TO WS-EXPIRING-SOON
               MOVE WS-EXPOSURE TO ED-AMT
               DISPLAY '  EXPIRES ' TF-EXPIRY-DATE ': ' TF-TYPE ' '
                       TF-NO ' ' ED-AMT ' ' TF-BENEFICIARY(1:25)
           END-IF.

       2000-EXPIRE.
           IF TF-MARGIN-HELD > ZERO
               INITIALIZE POST-PARMS
               SET PP-CREDIT TO TRUE
               MOVE TF-ACCT-NO     TO PP-ACCT-NO
               MOVE TF-MARGIN-HELD TO PP-AMOUNT
               MOVE 'TR'           TO PP-TRAN-TYPE
               MOVE 'MARGIN RELEASED ON EXPIRY' TO PP-NARRATION
               CALL 'ACCTPOST' USING POST-PARMS
               IF NOT PP-OK
                   DISPLAY '  MARGIN NOT RETURNED FOR ' TF-NO
                           ' (RC ' PP-RETURN-CODE ') - STAYS ACTIVE'
                   MOVE 4 TO WS-RETURN-CODE
                   EXIT PARAGRAPH
               END-IF
               MOVE ZERO TO TF-MARGIN-HELD
           END-IF
           SET TF-EXPIRED TO TRUE
           MOVE WS-RUN-DATE TO TF-CLOSE-DATE
           REWRITE TF-RECORD
           ADD 1 TO WS-EXPIRED
           DISPLAY '  EXPIRED ' TF-TYPE ' ' TF-NO ' (VALID TILL '
                   TF-EXPIRY-DATE ')'
           MOVE 'TFBATCH' TO AU-PROGRAM
           MOVE 'EXPIRED' TO AU-ACTION
           MOVE TF-NO     TO AU-KEY
           MOVE 'MARGIN RETURNED ON EXPIRY' TO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS.

       9000-REPORT.
           DISPLAY ' '
           DISPLAY 'TRADE FINANCE - CONTINGENT LIABILITIES AS ON '
                   WS-RUN-DATE
           MOVE WS-LIVE-BG TO ED-CNT
           MOVE WS-BG-EXPOSURE TO ED-AMT
           DISPLAY 'BANK GUARANTEES  : ' ED-CNT '  ' ED-AMT
           MOVE WS-LIVE-LC TO ED-CNT
           MOVE WS-LC-EXPOSURE TO ED-AMT
           DISPLAY 'LCS (UNDRAWN)    : ' ED-CNT '  ' ED-AMT
           MOVE WS-MARGIN-TOTAL TO ED-AMT
           DISPLAY 'LESS MARGIN HELD :         ' ED-AMT
           COMPUTE WS-NET-EXPOSURE =
                   WS-BG-EXPOSURE + WS-LC-EXPOSURE - WS-MARGIN-TOTAL
           MOVE WS-NET-EXPOSURE TO ED-AMT
           DISPLAY 'NET EXPOSURE     :         ' ED-AMT
           MOVE WS-EXPIRING-SOON TO ED-CNT
           DISPLAY 'EXPIRING IN 30 DAYS : ' ED-CNT
           MOVE WS-EXPIRED TO ED-CNT
           DISPLAY 'EXPIRED TODAY       : ' ED-CNT.
