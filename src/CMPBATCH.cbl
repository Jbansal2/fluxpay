       IDENTIFICATION DIVISION.
       PROGRAM-ID. CMPBATCH.
      *================================================================*
      * PROGRAM  : CMPBATCH                                            *
      * PURPOSE  : DAILY COMPLAINT SLA RUN AND MONTHLY RETURN.         *
      *            1. ESCALATION - EVERY PENDING TICKET PAST ITS DUE   *
      *               DATE GOES ONE LEVEL UP (BRANCH -> REGIONAL ->    *
      *               PRINCIPAL NODAL OFFICER) WITH 7 MORE DAYS. AT    *
      *               LEVEL 3 IT IS ONLY REPORTED.                     *
      *            2. RETURN FOR THE MONTH TO DATE - RECEIVED,         *
      *               DISPOSED, PENDING BY CATEGORY, AVERAGE DAYS TO   *
      *               DISPOSE, % WITHIN SLA, REFUNDS AND COMPENSATION, *
      *               AND COMPLAINTS PENDING OVER 30 DAYS (THE         *
      *               CUSTOMER MAY THEN APPROACH THE RBI OMBUDSMAN).   *
      * INPUT    : RUN DATE YYYYMMDD (BLANK = TODAY)                   *
      * OUTPUT   : data/CMP_<DATE>.TXT, data/OMBUD_<YYYYMM>.TXT        *
      * FILES    : CMPMAST (INDEXED, I-O)                              *
      * CALLS    : DATEUTIL                                            *
      * RC       : 0 NOTHING OVERDUE, 4 ESCALATIONS OR TICKETS OVER 30 *
      *            DAYS, 12 FILE ERROR                                 *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMP-FILE ASSIGN TO 'data/CMPMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CMP-NO
               FILE STATUS IS WS-CMP-FS.
           SELECT ESC-FILE ASSIGN TO WS-ESC-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ESC-FS.
           SELECT RET-FILE ASSIGN TO WS-RET-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RET-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CMP-FILE.
           COPY CMPREC.
       FD  ESC-FILE.
       01  ESC-LINE                    PIC X(100).
       FD  RET-FILE.
       01  RET-LINE                    PIC X(100).

       WORKING-STORAGE SECTION.
           COPY DATEPARM.
       01  WS-CMP-FS                   PIC X(02).
       01  WS-ESC-FS                   PIC X(02).
       01  WS-RET-FS                   PIC X(02).
       01  WS-ESC-NAME                 PIC X(40).
       01  WS-RET-NAME                 PIC X(40).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-RUN-DATE-R REDEFINES WS-RUN-DATE.
           05  WS-RUN-YYYYMM           PIC 9(06).
           05  WS-RUN-DD               PIC 9(02).
       01  WS-MONTH-START              PIC 9(08).
       01  WS-EOF-SW                   PIC X(01).
       01  WS-AGE                      PIC S9(07).
       01  WS-TAT                      PIC S9(07).
       01  WS-C                        PIC 9(02).
       01  WS-ESCALATED                PIC 9(05) VALUE ZERO.
       01  WS-AT-TOP                   PIC 9(05) VALUE ZERO.
       01  WS-OMBUDSMAN                PIC 9(05) VALUE ZERO.
       01  WS-OMBUDSMAN-DAYS           PIC 9(03) VALUE 30.
       01  WS-EXTRA-DAYS               PIC 9(03) VALUE 7.
       01  WS-LEVEL-NAME               PIC X(24).
       01  WS-HOLD                     PIC X(100).

      * CATEGORY TOTALS FOR THE RETURN (SAME ORDER AS CMPMNT)
       01  WS-CAT-CODES                PIC X(32) VALUE
           'ATMFCARDFRADCHRGACCTLOANSERVOTHR'.
       01  WS-CAT-CODES-R REDEFINES WS-CAT-CODES.
           05  WS-CAT-CODE OCCURS 8 TIMES PIC X(04).
       01  WS-CAT-TOTALS.
           05  WS-CT OCCURS 9 TIMES.
               10  CT-RECEIVED         PIC 9(05).
               10  CT-DISPOSED         PIC 9(05).
               10  CT-PENDING          PIC 9(05).
               10  CT-IN-SLA           PIC 9(05).
               10  CT-TAT-DAYS         PIC 9(07).
       01  WS-REFUNDS                  PIC 9(11)V99 VALUE ZERO.
       01  WS-COMPENSATION             PIC 9(11)V99 VALUE ZERO.
       01  WS-AVG-TAT                  PIC ZZ9.9.
       01  WS-SLA-PCT                  PIC ZZ9.9.
       01  WS-DISP-N1                  PIC ZZ,ZZ9.
       01  WS-DISP-N2                  PIC ZZ,ZZ9.
       01  WS-DISP-N3                  PIC ZZ,ZZ9.
       01  WS-DISP-AMT                 PIC ZZ,ZZZ,ZZ9.99.
       01  WS-DISP-AGE                 PIC Z,ZZ9.

      * PENDING > 30 DAYS, KEPT FOR THE LIST AT THE END OF THE RETURN
       01  WS-OLD-CNT                  PIC 9(04) VALUE ZERO.
       01  WS-OLD-TABLE.
           05  WS-OLD OCCURS 500 TIMES.
               10  OLD-NO              PIC 9(08).
               10  OLD-CAT             PIC X(04).
               10  OLD-ACCT            PIC 9(10).
               10  OLD-AGE             PIC 9(05).
               10  OLD-LEVEL           PIC 9(01).
       01  WS-O                        PIC 9(04).

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'RUN DATE (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           IF WS-IN-DATE = SPACES OR WS-IN-DATE IS NOT NUMERIC
               ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           ELSE
               MOVE WS-IN-DATE TO WS-RUN-DATE
           END-IF
           COMPUTE WS-MONTH-START = WS-RUN-YYYYMM * 100 + 1
           INITIALIZE WS-CAT-TOTALS
           OPEN I-O CMP-FILE
           IF WS-CMP-FS = '35'
               DISPLAY 'CMPBATCH: NO TICKETS ON FILE.'
               MOVE ZERO TO RETURN-CODE
               GOBACK
           END-IF
           IF WS-CMP-FS NOT = '00'
               DISPLAY 'CMPBATCH: OPEN FAILED FS=' WS-CMP-FS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           STRING 'data/CMP_' WS-RUN-DATE '.TXT'
                  DELIMITED BY SIZE INTO WS-ESC-NAME
           STRING 'data/OMBUD_' WS-RUN-YYYYMM '.TXT'
                  DELIMITED BY SIZE INTO WS-RET-NAME
           OPEN OUTPUT ESC-FILE
           MOVE SPACES TO ESC-LINE
           STRING 'COMPLAINT ESCALATIONS ' WS-RUN-DATE
                  DELIMITED BY SIZE INTO ESC-LINE
           WRITE ESC-LINE
           MOVE 'TICKET   CAT  ACCOUNT     WAS DUE   NOW AT' TO ESC-LINE
           WRITE ESC-LINE

           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ CMP-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N' AND CMP-OPEN-DATE <= WS-RUN-DATE
                   PERFORM 1000-ESCALATE
                   PERFORM 2000-ACCUMULATE
               END-IF
           END-PERFORM

           MOVE SPACES TO ESC-LINE
           WRITE ESC-LINE
           MOVE SPACES TO ESC-LINE
           STRING 'ESCALATED ' WS-ESCALATED '  OVERDUE AT NODAL '
                  'OFFICER ' WS-AT-TOP DELIMITED BY SIZE INTO ESC-LINE
           WRITE ESC-LINE
           DISPLAY FUNCTION TRIM(ESC-LINE TRAILING)
           CLOSE ESC-FILE CMP-FILE

           PERFORM 3000-WRITE-RETURN
           IF WS-ESCALATED + WS-AT-TOP + WS-OMBUDSMAN > ZERO
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE ZERO TO RETURN-CODE
           END-IF
           GOBACK.

      *----------------------------------------------------------------*
      * ESCALATION                                                     *
      *----------------------------------------------------------------*
       1000-ESCALATE.
           IF NOT CMP-PENDING OR CMP-DUE-DATE NOT < WS-RUN-DATE
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO ESC-LINE
           IF CMP-LEVEL >= 3
               ADD 1 TO WS-AT-TOP
               STRING CMP-NO ' ' CMP-CATEGORY ' ' CMP-ACCT-NO '  '
                      CMP-DUE-DATE '  STILL OVERDUE AT PRINCIPAL '
                      'NODAL OFFICER' DELIMITED BY SIZE INTO ESC-LINE
               WRITE ESC-LINE
               EXIT PARAGRAPH
           END-IF
           STRING CMP-NO ' ' CMP-CATEGORY ' ' CMP-ACCT-NO '  '
                  CMP-DUE-DATE '  ' DELIMITED BY SIZE INTO ESC-LINE
           ADD 1 TO CMP-LEVEL
           SET CMP-ESCALATED TO TRUE
           MOVE WS-RUN-DATE TO CMP-ESCALATED-DATE
           SET DU-ADD-DAYS TO TRUE
           MOVE WS-RUN-DATE TO DU-DATE-1
           MOVE WS-EXTRA-DAYS TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO CMP-DUE-DATE
           REWRITE CMP-RECORD
           ADD 1 TO WS-ESCALATED
           EVALUATE CMP-LEVEL
               WHEN 2 MOVE 'LEVEL 2 REGIONAL OFFICE' TO WS-LEVEL-NAME
               WHEN OTHER
                   MOVE 'LEVEL 3 NODAL OFFICER' TO WS-LEVEL-NAME
           END-EVALUATE
           MOVE ESC-LINE TO WS-HOLD
           MOVE SPACES TO ESC-LINE
           STRING WS-HOLD(1:46) WS-LEVEL-NAME ' DUE ' CMP-DUE-DATE
                  DELIMITED BY SIZE INTO ESC-LINE
           WRITE ESC-LINE
           DISPLAY FUNCTION TRIM(ESC-LINE TRAILING).

      *----------------------------------------------------------------*
      * MONTH-TO-DATE RETURN FIGURES (COMPLAINTS ONLY - SERVICE        *
      * REQUESTS ARE NOT REPORTABLE COMPLAINTS)                        *
      *----------------------------------------------------------------*
       2000-ACCUMULATE.
           IF NOT CMP-COMPLAINT
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-C FROM 1 BY 1
                   UNTIL WS-C > 8 OR WS-CAT-CODE(WS-C) = CMP-CATEGORY
           END-PERFORM
           IF CMP-OPEN-DATE >= WS-MONTH-START
               ADD 1 TO CT-RECEIVED(WS-C) CT-RECEIVED(9)
           END-IF
           IF CMP-DISPOSED AND CMP-RESOLVED-DATE >= WS-MONTH-START
              AND CMP-RESOLVED-DATE <= WS-RUN-DATE
               ADD 1 TO CT-DISPOSED(WS-C) CT-DISPOSED(9)
               SET DU-DAYS-BETWEEN TO TRUE
               MOVE CMP-OPEN-DATE     TO DU-DATE-1
               MOVE CMP-RESOLVED-DATE TO DU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DU-RESULT-DAYS TO WS-TAT
               ADD WS-TAT TO CT-TAT-DAYS(WS-C) CT-TAT-DAYS(9)
               IF CMP-RESOLVED-DATE <= CMP-DUE-DATE
                   ADD 1 TO CT-IN-SLA(WS-C) CT-IN-SLA(9)
               END-IF
               ADD CMP-REFUND-AMT TO WS-REFUNDS
               ADD CMP-COMP-AMT   TO WS-COMPENSATION
           END-IF
           IF CMP-PENDING
               ADD 1 TO CT-PENDING(WS-C) CT-PENDING(9)
               SET DU-DAYS-BETWEEN TO TRUE
               MOVE CMP-OPEN-DATE TO DU-DATE-1
               MOVE WS-RUN-DATE   TO DU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DU-RESULT-DAYS TO WS-AGE
               IF WS-AGE > WS-OMBUDSMAN-DAYS
                   ADD 1 TO WS-OMBUDSMAN
                   IF WS-OLD-CNT < 500
                       ADD 1 TO WS-OLD-CNT
                       MOVE CMP-NO       TO OLD-NO(WS-OLD-CNT)
                       MOVE CMP-CATEGORY TO OLD-CAT(WS-OLD-CNT)
                       MOVE CMP-ACCT-NO  TO OLD-ACCT(WS-OLD-CNT)
                       MOVE WS-AGE       TO OLD-AGE(WS-OLD-CNT)
                       MOVE CMP-LEVEL    TO OLD-LEVEL(WS-OLD-CNT)
                   END-IF
               END-IF
           END-IF.

       3000-WRITE-RETURN.
           OPEN OUTPUT RET-FILE
           MOVE SPACES TO RET-LINE
           STRING 'CUSTOMER COMPLAINTS RETURN  ' WS-MONTH-START
                  ' TO ' WS-RUN-DATE DELIMITED BY SIZE INTO RET-LINE
           WRITE RET-LINE
           MOVE SPACES TO RET-LINE
           WRITE RET-LINE
           MOVE 'CATEGORY  RECEIVED  DISPOSED   PENDING  AVG DAYS  '
             TO RET-LINE
           MOVE 'WITHIN SLA %' TO RET-LINE(51:)
           WRITE RET-LINE
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > 9
               PERFORM 3100-RETURN-LINE
           END-PERFORM
           MOVE SPACES TO RET-LINE
           WRITE RET-LINE
           MOVE WS-REFUNDS TO WS-DISP-AMT
           MOVE SPACES TO RET-LINE
           STRING 'REFUNDS ON DISPOSED COMPLAINTS : ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO RET-LINE
           WRITE RET-LINE
           MOVE WS-COMPENSATION TO WS-DISP-AMT
           MOVE SPACES TO RET-LINE
           STRING 'COMPENSATION PAID              : ' WS-DISP-AMT
                  DELIMITED BY SIZE INTO RET-LINE
           WRITE RET-LINE
           MOVE SPACES TO RET-LINE
           WRITE RET-LINE
           MOVE WS-OMBUDSMAN TO WS-DISP-N1
           MOVE SPACES TO RET-LINE
           STRING 'PENDING OVER 30 DAYS (CUSTOMER MAY APPROACH THE '
                  'RBI OMBUDSMAN): ' FUNCTION TRIM(WS-DISP-N1)
                  DELIMITED BY SIZE INTO RET-LINE
           WRITE RET-LINE
           DISPLAY FUNCTION TRIM(RET-LINE TRAILING)
           PERFORM VARYING WS-O FROM 1 BY 1 UNTIL WS-O > WS-OLD-CNT
               MOVE OLD-AGE(WS-O) TO WS-DISP-AGE
               MOVE SPACES TO RET-LINE
               STRING '  ' OLD-NO(WS-O) ' ' OLD-CAT(WS-O) ' '
                      OLD-ACCT(WS-O) '  ' WS-DISP-AGE ' DAYS  LEVEL '
                      OLD-LEVEL(WS-O) DELIMITED BY SIZE INTO RET-LINE
               WRITE RET-LINE
           END-PERFORM
           CLOSE RET-FILE.

       3100-RETURN-LINE.
           MOVE CT-RECEIVED(WS-C) TO WS-DISP-N1
           MOVE CT-DISPOSED(WS-C) TO WS-DISP-N2
           MOVE CT-PENDING(WS-C)  TO WS-DISP-N3
           IF CT-DISPOSED(WS-C) > ZERO
               COMPUTE WS-AVG-TAT ROUNDED =
                       CT-TAT-DAYS(WS-C) / CT-DISPOSED(WS-C)
               COMPUTE WS-SLA-PCT ROUNDED =
                       CT-IN-SLA(WS-C) * 100 / CT-DISPOSED(WS-C)
           ELSE
               MOVE ZERO TO WS-AVG-TAT WS-SLA-PCT
           END-IF
           IF WS-C = 9
               MOVE 'TOTAL' TO WS-HOLD
           ELSE
               MOVE WS-CAT-CODE(WS-C) TO WS-HOLD
           END-IF
           MOVE SPACES TO RET-LINE
           STRING WS-HOLD(1:8) '  ' WS-DISP-N1 '    ' WS-DISP-N2
                  '    ' WS-DISP-N3 '     ' WS-AVG-TAT '       '
                  WS-SLA-PCT DELIMITED BY SIZE INTO RET-LINE
           WRITE RET-LINE
           IF WS-C = 9
               DISPLAY FUNCTION TRIM(RET-LINE TRAILING)
           END-IF.
