       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLRBATCH.
      *================================================================*
      * PROGRAM  : CLRBATCH                                            *
      * PURPOSE  : INWARD CHEQUE CLEARING. READS CHEQUES PRESENTED ON  *
      *            OUR ACCOUNTS (data/CLRIN.DAT), VALIDATES EACH ONE,  *
      *            DEBITS THE DRAWER OR RETURNS IT WITH A REASON CODE. *
      *            CHECKS, IN ORDER:                                   *
      *              99 BAD RECORD     31 LEAF NOT ISSUED TO ACCOUNT   *
      *              32 ALREADY PAID   12 PAYMENT STOPPED              *
      *              05 POST-DATED     04 STALE (OVER 3 MONTHS OLD)    *
      *              22 NO ACCOUNT     21 ACCOUNT NOT ACTIVE           *
      *              01 FUNDS INSUFFICIENT                             *
      *            A FUNDS RETURN ALSO CHARGES THE DRAWER 250 + GST.   *
      *            RE-PRESENTING THE SAME FILE IS SAFE: PAID LEAVES    *
      *            COME BACK AS REASON 32 INSTEAD OF PAYING TWICE.     *
      * FILES    : CLRIN (INPUT), CHQREG (I-O), CLROUT_<DATE> (OUT),   *
      *            CLR_<DATE>.TXT (REPORT)                             *
      * CALLS    : ACCTPOST, DATEUTIL                                  *
      * RETURN   : 0 = ALL PROCESSED, 4 = SOME RETURNS, 12 = FILE ERROR*
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLR-IN-FILE ASSIGN TO 'data/CLRIN.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-FS.
           SELECT CLR-OUT-FILE ASSIGN TO WS-OUT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUT-FS.
           SELECT CHQ-FILE ASSIGN TO 'data/CHQREG.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CHQ-KEY
               FILE STATUS IS WS-CHQ-FS.
           SELECT RPT-FILE ASSIGN TO WS-RPT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CLR-IN-FILE.
           COPY CLRREC.
       FD  CLR-OUT-FILE.
       01  CLR-OUT-RECORD              PIC X(72).
       FD  CHQ-FILE.
           COPY CHQREC.
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(90).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY DATEPARM.
           COPY GLPARM.
           COPY GLCODES.

       01  WS-IN-FS                    PIC X(02).
       01  WS-OUT-FS                   PIC X(02).
       01  WS-CHQ-FS                   PIC X(02).
           88  CHQ-FS-OK                         VALUE '00'.
       01  WS-RPT-FS                   PIC X(02).
       01  WS-OUT-FILENAME             PIC X(40).
       01  WS-RPT-FILENAME             PIC X(40).

       01  WS-IN-RUN-DATE              PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-INPUT                      VALUE 'Y'.
       01  WS-LEAF-SW                  PIC X(01).
           88  LEAF-FOUND                        VALUE 'Y'.
       01  WS-REASON                   PIC X(02).
       01  WS-RETURN-CODE              PIC 9(02) VALUE ZERO.

       01  WS-RETURN-CHARGE            PIC 9(07)V99 VALUE 250.00.
       01  WS-GST-RATE                 PIC 9V99 VALUE 0.18.
       01  WS-CHARGE-TOTAL             PIC 9(07)V99.

      * REASON CODE TABLE (FOR THE REPORT)
       01  WS-REASON-TABLE.
           05  FILLER PIC X(26) VALUE '01FUNDS INSUFFICIENT      '.
           05  FILLER PIC X(26) VALUE '04CHEQUE STALE            '.
           05  FILLER PIC X(26) VALUE '05CHEQUE POST-DATED       '.
           05  FILLER PIC X(26) VALUE '12PAYMENT STOPPED         '.
           05  FILLER PIC X(26) VALUE '21ACCOUNT NOT ACTIVE      '.
           05  FILLER PIC X(26) VALUE '22ACCOUNT NOT FOUND       '.
           05  FILLER PIC X(26) VALUE '31CHEQUE NOT ISSUED TO A/C'.
           05  FILLER PIC X(26) VALUE '32CHEQUE ALREADY PAID     '.
           05  FILLER PIC X(26) VALUE '99INVALID CLEARING RECORD '.
       01  WS-REASON-TABLE-R REDEFINES WS-REASON-TABLE.
           05  WS-RSN OCCURS 9 TIMES INDEXED BY RSN-IDX.
               10  RSN-CODE            PIC X(02).
               10  RSN-TEXT            PIC X(24).
       01  WS-REASON-TEXT              PIC X(24).

       01  WS-STATS.
           05  WS-READ-CNT             PIC 9(07).
           05  WS-PAID-CNT             PIC 9(07).
           05  WS-RETURN-CNT           PIC 9(07).
           05  WS-PAID-AMT             PIC 9(13)V99.
           05  WS-RETURN-AMT           PIC 9(13)V99.

       01  WS-RPT-WORK                 PIC X(90).
       01  WS-DISP-CNT                 PIC Z,ZZZ,ZZ9.
       01  WS-DISP-AMT                 PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  RPT-DETAIL.
           05  RPT-SEQ                 PIC 9(06).
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  RPT-ACCT                PIC 9(10).
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  RPT-CHQ                 PIC 9(06).
           05  RPT-AMT                 PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  RPT-RESULT              PIC X(06).
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  RPT-REASON              PIC X(02).
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  RPT-REASON-TEXT         PIC X(24).

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 0100-INITIALIZE
           IF WS-RETURN-CODE < 12
               PERFORM 1000-PROCESS-ONE UNTIL END-OF-INPUT
               CLOSE CLR-IN-FILE CLR-OUT-FILE CHQ-FILE
           END-IF
           PERFORM 9000-FINISH
           GOBACK.

       0100-INITIALIZE.
           INITIALIZE WS-STATS
           MOVE ZERO TO WS-RETURN-CODE
           MOVE 'N' TO WS-EOF-SW
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           DISPLAY 'CLEARING DATE YYYYMMDD (BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-RUN-DATE
           MOVE WS-CURR-DATE TO WS-RUN-DATE
           IF WS-IN-RUN-DATE IS NUMERIC
               MOVE WS-IN-RUN-DATE TO DU-DATE-1
               SET DU-VALIDATE TO TRUE
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DU-OK
                   MOVE WS-IN-RUN-DATE TO WS-RUN-DATE
               END-IF
           END-IF
           STRING 'data/CLR_' WS-RUN-DATE '.TXT'
                  DELIMITED BY SIZE INTO WS-RPT-FILENAME
           END-STRING
           STRING 'data/CLROUT_' WS-RUN-DATE '.DAT'
                  DELIMITED BY SIZE INTO WS-OUT-FILENAME
           END-STRING
           OPEN OUTPUT RPT-FILE
           STRING 'INWARD CLEARING - ' WS-RUN-DATE
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT
           MOVE ALL '-' TO WS-RPT-WORK
           PERFORM 8000-EMIT

           OPEN INPUT CLR-IN-FILE
           IF WS-IN-FS NOT = '00'
               MOVE 'NO INWARD CLEARING FILE (data/CLRIN.DAT).'
                                                 TO WS-RPT-WORK
               PERFORM 8000-EMIT
               MOVE 12 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           OPEN I-O CHQ-FILE
           IF NOT CHQ-FS-OK
               MOVE 'CHEQUE REGISTER NOT AVAILABLE.' TO WS-RPT-WORK
               PERFORM 8000-EMIT
               CLOSE CLR-IN-FILE
               MOVE 12 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           OPEN OUTPUT CLR-OUT-FILE
           MOVE 'SEQ    ACCOUNT    CHEQUE        AMOUNT  RESULT RC'
                                                 TO WS-RPT-WORK
           PERFORM 8000-EMIT
           PERFORM 8100-READ-INPUT.

      *----------------------------------------------------------------*
      * ONE PRESENTED CHEQUE                                           *
      *----------------------------------------------------------------*
       1000-PROCESS-ONE.
           ADD 1 TO WS-READ-CNT
           MOVE SPACES TO WS-REASON
           PERFORM 2000-VALIDATE
           IF WS-REASON = SPACES
               PERFORM 3000-DEBIT-DRAWER
           END-IF
           IF WS-REASON = SPACES
               PERFORM 4000-MARK-PAID
           ELSE
               PERFORM 5000-MARK-RETURNED
           END-IF
           PERFORM 8100-READ-INPUT.

       2000-VALIDATE.
           IF CLR-ACCT-NO NOT NUMERIC OR CLR-CHQ-NO NOT NUMERIC
              OR CLR-AMOUNT NOT NUMERIC OR CLR-CHQ-DATE NOT NUMERIC
              OR CLR-AMOUNT = ZERO
               MOVE '99' TO WS-REASON
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-LEAF-SW
           MOVE CLR-ACCT-NO TO CHQ-ACCT-NO
           MOVE CLR-CHQ-NO  TO CHQ-NO
           READ CHQ-FILE
               INVALID KEY
                   MOVE '31' TO WS-REASON
                   EXIT PARAGRAPH
           END-READ
           MOVE 'Y' TO WS-LEAF-SW
           EVALUATE TRUE
               WHEN CHQ-PAID    MOVE '32' TO WS-REASON
               WHEN CHQ-STOPPED MOVE '12' TO WS-REASON
           END-EVALUATE
           IF WS-REASON NOT = SPACES
               EXIT PARAGRAPH
           END-IF
           MOVE CLR-CHQ-DATE TO DU-DATE-1
           SET DU-VALIDATE TO TRUE
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DU-OK
               MOVE '99' TO WS-REASON
               EXIT PARAGRAPH
           END-IF
           IF CLR-CHQ-DATE > WS-RUN-DATE
               MOVE '05' TO WS-REASON
               EXIT PARAGRAPH
           END-IF
      *    CTS RULE: VALID FOR 3 MONTHS FROM THE DATE ON THE CHEQUE
           SET DU-ADD-MONTHS TO TRUE
           MOVE 3 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           IF DU-RESULT-DATE < WS-RUN-DATE
               MOVE '04' TO WS-REASON
           END-IF.

       3000-DEBIT-DRAWER.
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE CLR-ACCT-NO TO PP-ACCT-NO
           MOVE CLR-AMOUNT  TO PP-AMOUNT
           MOVE 'CQ'        TO PP-TRAN-TYPE
           MOVE ZERO        TO PP-REF-ACCT
           STRING 'CHQ ' CLR-CHQ-NO ' ' CLR-PAYEE
                  DELIMITED BY SIZE INTO PP-NARRATION
           END-STRING
           CALL 'ACCTPOST' USING POST-PARMS
           EVALUATE TRUE
               WHEN PP-OK           CONTINUE
               WHEN PP-NOT-FOUND    MOVE '22' TO WS-REASON
               WHEN PP-NOT-ACTIVE   MOVE '21' TO WS-REASON
               WHEN PP-INSUFFICIENT MOVE '01' TO WS-REASON
               WHEN OTHER           MOVE '99' TO WS-REASON
           END-EVALUATE.

       4000-MARK-PAID.
           SET CHQ-PAID        TO TRUE
           MOVE WS-RUN-DATE    TO CHQ-PRESENTED-DATE
           MOVE CLR-AMOUNT     TO CHQ-AMOUNT
           MOVE CLR-PAYEE      TO CHQ-PAYEE
           MOVE SPACES         TO CHQ-REASON-CODE
           REWRITE CHQ-RECORD
           IF NOT CHQ-FS-OK
      *        MONEY HAS MOVED BUT THE LEAF STILL LOOKS UNUSED
               DISPLAY 'CLRBATCH: SEVERE - LEAF ' CLR-CHQ-NO
                       ' PAID BUT NOT UPDATED FS=' WS-CHQ-FS
               MOVE 12 TO WS-RETURN-CODE
           END-IF
           ADD 1 TO WS-PAID-CNT
           ADD CLR-AMOUNT TO WS-PAID-AMT
           MOVE 'PAID' TO RPT-RESULT
           MOVE SPACES TO RPT-REASON RPT-REASON-TEXT
           PERFORM 6000-REPORT-LINE.

       5000-MARK-RETURNED.
           ADD 1 TO WS-RETURN-CNT
           IF CLR-AMOUNT IS NUMERIC
               ADD CLR-AMOUNT TO WS-RETURN-AMT
           END-IF
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF
      *    UPDATE THE LEAF ONLY IF IT IS OURS AND NOT ALREADY FINAL
           IF LEAF-FOUND AND NOT CHQ-PAID AND NOT CHQ-STOPPED
               SET CHQ-RETURNED  TO TRUE
               MOVE WS-RUN-DATE  TO CHQ-PRESENTED-DATE
               MOVE CLR-AMOUNT   TO CHQ-AMOUNT
               MOVE CLR-PAYEE    TO CHQ-PAYEE
               MOVE WS-REASON    TO CHQ-REASON-CODE
               REWRITE CHQ-RECORD
           END-IF
           IF WS-REASON = '01'
               PERFORM 5100-CHARGE-DRAWER
           END-IF
           MOVE WS-REASON TO CLR-REASON-CODE
           SET CLR-RETURN TO TRUE
           MOVE CLR-RECORD TO CLR-OUT-RECORD
           WRITE CLR-OUT-RECORD
           MOVE 'RETURN' TO RPT-RESULT
           MOVE WS-REASON TO RPT-REASON
           PERFORM 7000-LOOKUP-REASON
           MOVE WS-REASON-TEXT TO RPT-REASON-TEXT
           PERFORM 6000-REPORT-LINE.

       5100-CHARGE-DRAWER.
           COMPUTE WS-CHARGE-TOTAL ROUNDED =
                   WS-RETURN-CHARGE * (1 + WS-GST-RATE)
           INITIALIZE POST-PARMS
           SET PP-DEBIT TO TRUE
           MOVE CLR-ACCT-NO     TO PP-ACCT-NO
           MOVE WS-CHARGE-TOTAL TO PP-AMOUNT
           MOVE 'CH'            TO PP-TRAN-TYPE
           MOVE 'Y'             TO PP-IGNORE-MIN-BAL
           MOVE 'Y'             TO PP-SYSTEM-TXN
           STRING 'CHQ RETURN CHG ' CLR-CHQ-NO
                  DELIMITED BY SIZE INTO PP-NARRATION
           END-STRING
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'CLRBATCH: RETURN CHARGE NOT RECOVERED FROM '
                       CLR-ACCT-NO
               EXIT PARAGRAPH
           END-IF
           MOVE GLC-FEE-INCOME  TO GP-DR-GL
           MOVE GLC-GST-PAYABLE TO GP-CR-GL
           COMPUTE GP-AMOUNT = WS-CHARGE-TOTAL - WS-RETURN-CHARGE
           MOVE CLR-ACCT-NO     TO GP-REF
           MOVE 'GST ON CHEQUE RETURN CHARGE' TO GP-NARRATION
           CALL 'GLPOST' USING GL-PARMS.

       6000-REPORT-LINE.
           MOVE CLR-SEQ     TO RPT-SEQ
           MOVE CLR-ACCT-NO TO RPT-ACCT
           MOVE CLR-CHQ-NO  TO RPT-CHQ
           IF CLR-AMOUNT IS NUMERIC
               MOVE CLR-AMOUNT TO RPT-AMT
           ELSE
               MOVE ZERO TO RPT-AMT
           END-IF
           MOVE RPT-DETAIL TO WS-RPT-WORK
           PERFORM 8000-EMIT.

       7000-LOOKUP-REASON.
           MOVE 'UNKNOWN' TO WS-REASON-TEXT
           SET RSN-IDX TO 1
           SEARCH WS-RSN
               WHEN RSN-CODE(RSN-IDX) = WS-REASON
                   MOVE RSN-TEXT(RSN-IDX) TO WS-REASON-TEXT
           END-SEARCH.

       8000-EMIT.
           MOVE WS-RPT-WORK TO RPT-LINE
           WRITE RPT-LINE
           DISPLAY WS-RPT-WORK
           MOVE SPACES TO WS-RPT-WORK.

       8100-READ-INPUT.
           READ CLR-IN-FILE
               AT END SET END-OF-INPUT TO TRUE
           END-READ.

       9000-FINISH.
           MOVE ALL '-' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-READ-CNT TO WS-DISP-CNT
           STRING 'CHEQUES PRESENTED : ' WS-DISP-CNT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-PAID-CNT TO WS-DISP-CNT
           MOVE WS-PAID-AMT TO WS-DISP-AMT
           STRING 'PAID              : ' WS-DISP-CNT '  ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-RETURN-CNT TO WS-DISP-CNT
           MOVE WS-RETURN-AMT TO WS-DISP-AMT
           STRING 'RETURNED          : ' WS-DISP-CNT '  ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           STRING 'RETURN CODE       : ' WS-RETURN-CODE
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           PERFORM 8000-EMIT
           CLOSE RPT-FILE
           IF WS-RETURN-CNT > ZERO
               DISPLAY 'RETURN FILE: ' WS-OUT-FILENAME
           END-IF
           DISPLAY 'REPORT SAVED TO ' WS-RPT-FILENAME
           MOVE WS-RETURN-CODE TO RETURN-CODE.
