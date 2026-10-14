       IDENTIFICATION DIVISION.
       PROGRAM-ID. COLLBAT.
      *================================================================*
      * PROGRAM  : COLLBAT                                             *
      * PURPOSE  : DAILY COLLECTIONS RUN                               *
      *            1. REMINDERS BY DAYS PAST DUE, ESCALATING:          *
      *                 1-30  SMS REMINDER                             *
      *                31-60  REMINDER LETTER                          *
      *                61-90  FINAL NOTICE BEFORE NPA                  *
      *                 >90   RECALL NOTICE (HOME LOANS: SARFAESI      *
      *                       SEC 13(2) NOTICE, 60 DAYS TO PAY)        *
      *               A NOTICE IS SENT ONCE PER STAGE - THE LOG IS     *
      *               CHECKED SO A DAILY RUN DOES NOT REPEAT IT.       *
      *            2. BROKEN PROMISES: A PROMISE TO PAY WHOSE DATE HAS *
      *               PASSED WITH NO PAYMENT SINCE IT WAS MADE.        *
      * INPUT    : RUN DATE YYYYMMDD (BLANK = TODAY)                   *
      * OUTPUT   : data/REMIND_<DATE>.TXT (NOTICES TO PRINT / SEND),   *
      *            ONE 'R' RECORD PER NOTICE IN COLLLOG                *
      * FILES    : LOANMAST, COLLLOG                                   *
      * CALLS    : DATEUTIL, CUSTLKUP                                  *
      * RETURN   : 0 = OK, 4 = BROKEN PROMISES FOUND                   *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS RANDOM
               RECORD KEY IS LOAN-NO FILE STATUS IS WS-LOAN-FS.
           SELECT LOAN-SEQ ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS SEQUENTIAL
               RECORD KEY IS LS-LOAN-NO FILE STATUS IS WS-SEQ-FS.
           SELECT LOG-FILE ASSIGN TO 'data/COLLLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LOG-FS.
           SELECT OUT-FILE ASSIGN TO WS-OUT-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  LOAN-SEQ.
       01  LS-RECORD.
           05  LS-LOAN-NO              PIC 9(10).
           05  FILLER                  PIC X(132).
       FD  LOG-FILE.
           COPY COLLREC.
       FD  OUT-FILE.
       01  OUT-LINE                    PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-LOAN-FS                  PIC X(02).
       01  WS-SEQ-FS                   PIC X(02).
       01  WS-LOG-FS                   PIC X(02).
       01  WS-OUT-FS                   PIC X(02).
       01  WS-OUT-NAME                 PIC X(40).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-DPD                      PIC S9(05).
       01  WS-STAGE                    PIC 9(01).
       01  WS-LINE                     PIC X(100).
       01  WS-OVERDUE-AMT              PIC 9(11)V99.
      * NOTICES ALREADY SENT: LOAN + STAGE (FROM THE LOG)
       01  WS-SENT-COUNT               PIC 9(05) VALUE ZERO.
       01  WS-SENT-TABLE.
           05  WS-SENT OCCURS 3000 TIMES.
               10  ST-LOAN-NO          PIC 9(10).
               10  ST-STAGE            PIC 9(01).
      * PROMISES TO PAY (LATEST PER LOAN)
       01  WS-PTP-COUNT                PIC 9(04) VALUE ZERO.
       01  WS-PTP-TABLE.
           05  WS-PTP OCCURS 1000 TIMES.
               10  PT-LOAN-NO          PIC 9(10).
               10  PT-MADE             PIC 9(08).
               10  PT-DUE              PIC 9(08).
               10  PT-AMOUNT           PIC 9(11)V99.
       01  WS-I                        PIC 9(05).
       01  WS-FOUND-SW                 PIC X(01).
           88  FOUND                             VALUE 'Y'.
       01  WS-STAGE-NAMES.
           05  FILLER PIC X(30) VALUE 'SMS REMINDER'.
           05  FILLER PIC X(30) VALUE 'REMINDER LETTER'.
           05  FILLER PIC X(30) VALUE 'FINAL NOTICE BEFORE NPA'.
           05  FILLER PIC X(30) VALUE 'LOAN RECALL NOTICE'.
       01  WS-STAGE-R REDEFINES WS-STAGE-NAMES.
           05  WS-STAGE-NAME           PIC X(30) OCCURS 4 TIMES.
       01  WS-COUNTS.
           05  WS-NOTICES              PIC 9(05) VALUE ZERO.
           05  WS-STAGE-CNT            PIC 9(05) OCCURS 4 TIMES.
           05  WS-BROKEN               PIC 9(05) VALUE ZERO.
           05  WS-ALREADY              PIC 9(05) VALUE ZERO.
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-CNT                      PIC ZZ,ZZ9.
       01  ED-DPD                      PIC ZZZZ9.
           COPY DATEPARM.
           COPY CLKPARM.

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
           PERFORM VARYING WS-STAGE FROM 1 BY 1 UNTIL WS-STAGE > 4
               MOVE ZERO TO WS-STAGE-CNT(WS-STAGE)
           END-PERFORM
           PERFORM 1000-LOAD-LOG
           OPEN INPUT LOAN-SEQ
           IF WS-SEQ-FS NOT = '00'
               DISPLAY 'COLLBAT: NO LOAN MASTER - NOTHING TO DO'
               GOBACK
           END-IF
           OPEN INPUT LOAN-FILE
           STRING 'data/REMIND_' WS-RUN-DATE '.TXT' DELIMITED BY SIZE
               INTO WS-OUT-NAME
           OPEN OUTPUT OUT-FILE
           OPEN EXTEND LOG-FILE
           IF WS-LOG-FS = '35'
               OPEN OUTPUT LOG-FILE
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ LOAN-SEQ NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       MOVE LS-LOAN-NO TO LOAN-NO
                       READ LOAN-FILE
                           NOT INVALID KEY PERFORM 2000-ONE-LOAN
                       END-READ
               END-READ
           END-PERFORM
           CLOSE LOAN-SEQ LOG-FILE
           PERFORM 3000-BROKEN-PROMISES
           CLOSE LOAN-FILE OUT-FILE
           MOVE 'Y' TO CLK-CLOSE
           CALL 'CUSTLKUP' USING CLK-PARMS
           PERFORM 9000-SUMMARY
           IF WS-BROKEN > ZERO
               MOVE 4 TO RETURN-CODE
           END-IF
           GOBACK.

      *    WHAT WAS ALREADY SENT, AND THE LATEST PROMISE PER LOAN
       1000-LOAD-LOG.
           OPEN INPUT LOG-FILE
           IF WS-LOG-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ LOG-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       EVALUATE CL-ACTION
                           WHEN 'R'
                               IF WS-SENT-COUNT < 3000
                                   ADD 1 TO WS-SENT-COUNT
                                   MOVE CL-LOAN-NO
                                     TO ST-LOAN-NO(WS-SENT-COUNT)
                                   MOVE CL-REMARKS(1:1)
                                     TO ST-STAGE(WS-SENT-COUNT)
                               END-IF
                           WHEN 'P'
                               PERFORM 1100-KEEP-PROMISE
                       END-EVALUATE
               END-READ
           END-PERFORM
           CLOSE LOG-FILE.

       1100-KEEP-PROMISE.
           MOVE 'N' TO WS-FOUND-SW
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-PTP-COUNT OR FOUND
               IF PT-LOAN-NO(WS-I) = CL-LOAN-NO
                   SET FOUND TO TRUE
                   MOVE CL-DATE       TO PT-MADE(WS-I)
                   MOVE CL-PTP-DATE   TO PT-DUE(WS-I)
                   MOVE CL-PTP-AMOUNT TO PT-AMOUNT(WS-I)
               END-IF
           END-PERFORM
           IF NOT FOUND AND WS-PTP-COUNT < 1000
               ADD 1 TO WS-PTP-COUNT
               MOVE CL-LOAN-NO    TO PT-LOAN-NO(WS-PTP-COUNT)
               MOVE CL-DATE       TO PT-MADE(WS-PTP-COUNT)
               MOVE CL-PTP-DATE   TO PT-DUE(WS-PTP-COUNT)
               MOVE CL-PTP-AMOUNT TO PT-AMOUNT(WS-PTP-COUNT)
           END-IF.

      *================================================================*
      * REMINDERS                                                      *
      *================================================================*
       2000-ONE-LOAN.
           IF LOAN-CLOSED OR LOAN-WRITTEN-OFF
              OR LOAN-OVERDUE-EMIS = ZERO
               EXIT PARAGRAPH
           END-IF
           PERFORM 7000-DPD
           EVALUATE TRUE
               WHEN WS-DPD <= 0  EXIT PARAGRAPH
               WHEN WS-DPD <= 30 MOVE 1 TO WS-STAGE
               WHEN WS-DPD <= 60 MOVE 2 TO WS-STAGE
               WHEN WS-DPD <= 90 MOVE 3 TO WS-STAGE
               WHEN OTHER        MOVE 4 TO WS-STAGE
           END-EVALUATE
           MOVE 'N' TO WS-FOUND-SW
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-SENT-COUNT OR FOUND
               IF ST-LOAN-NO(WS-I) = LOAN-NO
                  AND ST-STAGE(WS-I) = WS-STAGE
                   SET FOUND TO TRUE
               END-IF
           END-PERFORM
           IF FOUND
               ADD 1 TO WS-ALREADY
               EXIT PARAGRAPH
           END-IF
           PERFORM 2100-WRITE-NOTICE.

       2100-WRITE-NOTICE.
           MOVE LOAN-ACCT-NO TO CLK-ACCT-NO
           MOVE 'N' TO CLK-CLOSE
           CALL 'CUSTLKUP' USING CLK-PARMS
           IF NOT CLK-OK
               MOVE 'THE BORROWER' TO CLK-NAME
           END-IF
           COMPUTE WS-OVERDUE-AMT = LOAN-OVERDUE-EMIS * LOAN-EMI
                                    + LOAN-PENALTY-DUE
           MOVE WS-OVERDUE-AMT TO ED-AMT
           MOVE WS-DPD TO ED-DPD
           MOVE ALL '-' TO WS-LINE
           PERFORM 8000-PUT
           STRING WS-STAGE-NAME(WS-STAGE) '  DATE ' WS-RUN-DATE
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           STRING 'TO     : ' CLK-NAME DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           STRING 'LOAN   : ' LOAN-NO '  ACCOUNT ' LOAN-ACCT-NO
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           STRING 'OVERDUE: ' ED-AMT ' (' LOAN-OVERDUE-EMIS
                  ' EMIS, ' FUNCTION TRIM(ED-DPD) ' DAYS)'
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           EVALUATE WS-STAGE
               WHEN 1
                   MOVE 'PLEASE PAY AT THE EARLIEST TO AVOID CHARGES.'
                       TO WS-LINE
               WHEN 2
                   MOVE
                       'PAY WITHIN 7 DAYS OR CONTACT YOUR BRANCH.'
                       TO WS-LINE
               WHEN 3
                   MOVE
                       'IF NOT PAID THE LOAN WILL BE CLASSIFIED AS NPA.'
                       TO WS-LINE
               WHEN 4
                   IF LOAN-HOME
                       MOVE
                           'SARFAESI SEC 13(2) NOTICE: PAY ALL DUES'
                           TO WS-LINE
                       PERFORM 8000-PUT
                       MOVE
                           'IN 60 DAYS OR THE SECURITY IS TAKEN.'
                           TO WS-LINE
                   ELSE
                       MOVE
                           'WHOLE LOAN RECALLED. PAY IN 15 DAYS.'
                           TO WS-LINE
                   END-IF
           END-EVALUATE
           PERFORM 8000-PUT
           ADD 1 TO WS-NOTICES WS-STAGE-CNT(WS-STAGE)
           INITIALIZE COLL-RECORD
           MOVE LOAN-NO     TO CL-LOAN-NO
           MOVE WS-RUN-DATE TO CL-DATE
           MOVE 'R'         TO CL-ACTION
           MOVE 'COLLBAT'   TO CL-USER
           STRING WS-STAGE ' ' WS-STAGE-NAME(WS-STAGE)
               DELIMITED BY SIZE INTO CL-REMARKS
           WRITE COLL-RECORD.

      *================================================================*
      * BROKEN PROMISES                                                *
      *================================================================*
       3000-BROKEN-PROMISES.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-PTP-COUNT
               IF PT-DUE(WS-I) < WS-RUN-DATE
                   MOVE PT-LOAN-NO(WS-I) TO LOAN-NO
                   READ LOAN-FILE
                       INVALID KEY CONTINUE
                       NOT INVALID KEY
                           IF NOT LOAN-CLOSED
                              AND LOAN-LAST-PAY-DATE < PT-MADE(WS-I)
                               ADD 1 TO WS-BROKEN
                               MOVE PT-AMOUNT(WS-I) TO ED-AMT
                               DISPLAY '  BROKEN PROMISE: LOAN '
                                       LOAN-NO ' ' ED-AMT ' DUE '
                                       PT-DUE(WS-I)
                           END-IF
                   END-READ
               END-IF
           END-PERFORM.

       7000-DPD.
           MOVE ZERO TO WS-DPD
           SET DU-ADD-MONTHS TO TRUE
           MOVE LOAN-NEXT-DUE-DATE TO DU-DATE-1
           COMPUTE DU-NUMBER = 0 - LOAN-OVERDUE-EMIS
           CALL 'DATEUTIL' USING DATE-PARMS
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE DU-RESULT-DATE TO DU-DATE-1
           MOVE WS-RUN-DATE    TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           IF DU-OK
               MOVE DU-RESULT-DAYS TO WS-DPD
           END-IF.

       8000-PUT.
           WRITE OUT-LINE FROM WS-LINE
           MOVE SPACES TO WS-LINE.

       9000-SUMMARY.
           DISPLAY 'COLLECTIONS RUN FOR ' WS-RUN-DATE
           PERFORM VARYING WS-STAGE FROM 1 BY 1 UNTIL WS-STAGE > 4
               MOVE WS-STAGE-CNT(WS-STAGE) TO ED-CNT
               DISPLAY '  ' WS-STAGE-NAME(WS-STAGE) ' : ' ED-CNT
           END-PERFORM
           MOVE WS-ALREADY TO ED-CNT
           DISPLAY '  ALREADY NOTIFIED AT THIS STAGE : ' ED-CNT
           MOVE WS-BROKEN TO ED-CNT
           DISPLAY '  BROKEN PROMISES TO PAY         : ' ED-CNT
           DISPLAY '  NOTICES IN ' FUNCTION TRIM(WS-OUT-NAME).
