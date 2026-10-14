       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROVBAT.
      *================================================================*
      * PROGRAM  : PROVBAT                                             *
      * PURPOSE  : RBI IRAC NORMS - ASSET CLASSIFICATION, PROVISIONING *
      *            AND INTEREST ACCRUAL FOR THE LOAN BOOK.             *
      *            CLASSIFICATION BY DAYS PAST DUE (DPD) OF THE OLDEST *
      *            UNPAID EMI; A LOAN BECOMES NPA AFTER 90 DAYS AND    *
      *            AGES THROUGH THE NPA CATEGORIES:                    *
      *              S  STANDARD       DPD UP TO 90          0.40%     *
      *              B  SUB-STANDARD   NPA UP TO 12 MONTHS  15%        *
      *              1  DOUBTFUL-1     NPA 12-24 MONTHS     25%        *
      *              2  DOUBTFUL-2     NPA 24-48 MONTHS     40%        *
      *              3  DOUBTFUL-3     NPA OVER 48 MONTHS  100%        *
      *            UNSECURED (PERSONAL) LOANS IN ANY DOUBTFUL CATEGORY *
      *            ARE PROVIDED AT 100%, AS THERE IS NO SECURITY TO    *
      *            FALL BACK ON. WRITTEN-OFF LOANS ARE SKIPPED.        *
      *            THE TOTAL PROVISION REQUIRED IS COMPARED WITH WHAT  *
      *            GL 1299 ALREADY HOLDS AND ONLY THE DIFFERENCE IS    *
      *            BOOKED (DR 5101 / CR 1299, OR THE REVERSE).         *
      *            ACCRUAL: INTEREST EARNED SINCE THE LAST EMI DATE ON *
      *            STANDARD LOANS ONLY - INCOME ON AN NPA IS NOT       *
      *            RECOGNISED UNTIL IT IS ACTUALLY RECEIVED. HELD IN   *
      *            GL 1240, ADJUSTED TO THE NEW FIGURE EACH RUN        *
      *            (DR 1240 / CR 4001, OR THE REVERSE).                *
      * INPUT    : RUN DATE YYYYMMDD (BLANK = TODAY)                   *
      * OUTPUT   : data/PROV_<DATE>.TXT                                *
      * FILES    : LOANMAST (I-O), GLMAST (INPUT)                      *
      * CALLS    : DATEUTIL, GLPOST                                    *
      * RETURN   : 0 = OK, 12 = FILE / GL ERROR                        *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS SEQUENTIAL
               RECORD KEY IS LOAN-NO FILE STATUS IS WS-LOAN-FS.
           SELECT GL-FILE ASSIGN TO 'data/GLMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS RANDOM
               RECORD KEY IS GL-CODE FILE STATUS IS WS-GL-FS.
           SELECT RPT-FILE ASSIGN TO WS-RPT-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  GL-FILE.
           COPY GLREC.
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-LOAN-FS                  PIC X(02).
       01  WS-GL-FS                    PIC X(02).
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-NAME                 PIC X(40).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-EOF-SW                   PIC X(01) VALUE 'N'.
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-RETURN-CODE              PIC 9(02) VALUE ZERO.
       01  WS-LINE                     PIC X(100).
       01  WS-DPD                      PIC S9(05).
       01  WS-NPA-DAYS                 PIC S9(05).
       01  WS-C                        PIC 9(01).
       01  WS-PCT                      PIC 9(03)V99.
       01  WS-PROVISION                PIC S9(13)V99.
       01  WS-DAYS-ACCRUED             PIC S9(05).
       01  WS-ACCRUAL                  PIC S9(13)V99.
       01  WS-LAST-DUE                 PIC 9(08).
      * CLASS TABLE: CODE, NAME, PERCENTAGE
       01  WS-CLASS-DEF.
           05  FILLER PIC X(17) VALUE 'SSTANDARD   00040'.
           05  FILLER PIC X(17) VALUE 'BSUBSTANDRD 01500'.
           05  FILLER PIC X(17) VALUE '1DOUBTFUL-1 02500'.
           05  FILLER PIC X(17) VALUE '2DOUBTFUL-2 04000'.
           05  FILLER PIC X(17) VALUE '3DOUBTFUL-3 10000'.
       01  WS-CLASS-R REDEFINES WS-CLASS-DEF.
           05  WS-CLASS OCCURS 5 TIMES.
               10  CL-CODE             PIC X(01).
               10  CL-NAME             PIC X(11).
               10  CL-PCT              PIC 9(03)V99.
       01  WS-CLASS-TOTALS.
           05  WS-CLASS-TOT OCCURS 5 TIMES.
               10  CT-COUNT            PIC 9(06).
               10  CT-OUTSTANDING      PIC S9(13)V99.
               10  CT-PROVISION        PIC S9(13)V99.
       01  WS-TOTALS.
           05  WS-REQUIRED             PIC S9(13)V99 VALUE ZERO.
           05  WS-HELD                 PIC S9(13)V99 VALUE ZERO.
           05  WS-ACCRUED-TOTAL        PIC S9(13)V99 VALUE ZERO.
           05  WS-ACCRUED-HELD         PIC S9(13)V99 VALUE ZERO.
           05  WS-DELTA                PIC S9(13)V99.
           05  WS-RECLASSIFIED         PIC 9(06) VALUE ZERO.
       01  WS-OLD-CLASS                PIC X(01).
       01  ED-AMT                      PIC ---,---,---,--9.99.
       01  ED-AMT2                     PIC ---,---,---,--9.99.
       01  ED-CNT                      PIC ZZZ,ZZ9.
       01  ED-PCT                      PIC ZZ9.99.
           COPY DATEPARM.
           COPY GLPARM.
           COPY GLCODES.

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
           INITIALIZE WS-CLASS-TOTALS
           OPEN I-O LOAN-FILE
           IF WS-LOAN-FS NOT = '00'
               DISPLAY 'PROVBAT: NO LOAN MASTER - NOTHING TO DO'
               GOBACK
           END-IF
           STRING 'data/PROV_' WS-RUN-DATE '.TXT' DELIMITED BY SIZE
               INTO WS-RPT-NAME
           OPEN OUTPUT RPT-FILE
           STRING 'ASSET CLASSIFICATION AND PROVISIONING AS ON '
                  WS-RUN-DATE DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           PERFORM UNTIL END-OF-FILE
               READ LOAN-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF NOT LOAN-CLOSED AND NOT LOAN-WRITTEN-OFF
                           PERFORM 1000-CLASSIFY
                       END-IF
               END-READ
           END-PERFORM
           CLOSE LOAN-FILE
           PERFORM 2000-READ-GL-HELD
           PERFORM 3000-BOOK-PROVISION
           PERFORM 3100-BOOK-ACCRUAL
           PERFORM 9000-REPORT
           CLOSE RPT-FILE
           MOVE WS-RETURN-CODE TO RETURN-CODE
           GOBACK.

      *================================================================*
      * ONE LOAN                                                       *
      *================================================================*
       1000-CLASSIFY.
           PERFORM 1100-DAYS-PAST-DUE
           COMPUTE WS-NPA-DAYS = WS-DPD - 90
           EVALUATE TRUE
               WHEN WS-DPD <= 90 AND NOT LOAN-NPA
                   MOVE 1 TO WS-C
               WHEN WS-NPA-DAYS <= 365
                   MOVE 2 TO WS-C
               WHEN WS-NPA-DAYS <= 730
                   MOVE 3 TO WS-C
               WHEN WS-NPA-DAYS <= 1460
                   MOVE 4 TO WS-C
               WHEN OTHER
                   MOVE 5 TO WS-C
           END-EVALUATE
           MOVE CL-PCT(WS-C) TO WS-PCT
           IF WS-C >= 3 AND LOAN-PERSONAL
               MOVE 100 TO WS-PCT
           END-IF
           COMPUTE WS-PROVISION ROUNDED =
                   LOAN-OUTSTANDING * WS-PCT / 100
           ADD 1                TO CT-COUNT(WS-C)
           ADD LOAN-OUTSTANDING TO CT-OUTSTANDING(WS-C)
           ADD WS-PROVISION     TO CT-PROVISION(WS-C)
           ADD WS-PROVISION     TO WS-REQUIRED
           IF WS-C = 1
               PERFORM 1200-ACCRUE
           END-IF
      *    REMEMBER THE CLASS ON THE LOAN; REPORT ANY MOVEMENT
           MOVE LOAN-ASSET-CLASS TO WS-OLD-CLASS
           IF WS-OLD-CLASS NOT = CL-CODE(WS-C)
               MOVE CL-CODE(WS-C) TO LOAN-ASSET-CLASS
               REWRITE LOAN-RECORD
               IF WS-OLD-CLASS NOT = SPACE
                   ADD 1 TO WS-RECLASSIFIED
                   STRING '  LOAN ' LOAN-NO ' MOVED ' WS-OLD-CLASS
                          ' -> ' CL-CODE(WS-C) '  DPD ' WS-DPD
                          DELIMITED BY SIZE INTO WS-LINE
                   PERFORM 8000-PUT
               END-IF
           END-IF.

      *    DPD = DAYS SINCE THE OLDEST UNPAID EMI (NEXT DUE DATE LESS
      *    THE NUMBER OF OVERDUE EMIS, IN MONTHS)
       1100-DAYS-PAST-DUE.
           MOVE ZERO TO WS-DPD
           IF LOAN-OVERDUE-EMIS = ZERO
               EXIT PARAGRAPH
           END-IF
           SET DU-ADD-MONTHS TO TRUE
           MOVE LOAN-NEXT-DUE-DATE TO DU-DATE-1
           COMPUTE DU-NUMBER = 0 - LOAN-OVERDUE-EMIS
           CALL 'DATEUTIL' USING DATE-PARMS
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE DU-RESULT-DATE TO DU-DATE-1
           MOVE WS-RUN-DATE    TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           IF DU-OK AND DU-RESULT-DAYS > ZERO
               MOVE DU-RESULT-DAYS TO WS-DPD
           END-IF.

      *    INTEREST FROM THE LAST EMI DATE (ONE MONTH BEFORE THE NEXT
      *    ONE) TO THE RUN DATE, SIMPLE INTEREST ON THE OUTSTANDING
       1200-ACCRUE.
           SET DU-ADD-MONTHS TO TRUE
           MOVE LOAN-NEXT-DUE-DATE TO DU-DATE-1
           MOVE -1 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO WS-LAST-DUE
           IF WS-LAST-DUE < LOAN-DISB-DATE
               MOVE LOAN-DISB-DATE TO WS-LAST-DUE
           END-IF
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE WS-LAST-DUE TO DU-DATE-1
           MOVE WS-RUN-DATE TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DU-OK OR DU-RESULT-DAYS <= ZERO
               EXIT PARAGRAPH
           END-IF
           MOVE DU-RESULT-DAYS TO WS-DAYS-ACCRUED
           IF WS-DAYS-ACCRUED > 31
               MOVE 31 TO WS-DAYS-ACCRUED
           END-IF
           COMPUTE WS-ACCRUAL ROUNDED = LOAN-OUTSTANDING * LOAN-RATE
                   * WS-DAYS-ACCRUED / 36500
           ADD WS-ACCRUAL TO WS-ACCRUED-TOTAL.

      *================================================================*
      * WHAT THE LEDGER ALREADY HOLDS (1299 IS A CREDIT = NEGATIVE)    *
      *================================================================*
       2000-READ-GL-HELD.
           OPEN INPUT GL-FILE
           IF WS-GL-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           MOVE GLC-LOAN-PROVISION TO GL-CODE
           READ GL-FILE
               NOT INVALID KEY
                   COMPUTE WS-HELD = 0 - GL-BALANCE
           END-READ
           MOVE GLC-INT-ACCRUED TO GL-CODE
           READ GL-FILE
               NOT INVALID KEY
                   MOVE GL-BALANCE TO WS-ACCRUED-HELD
           END-READ
           CLOSE GL-FILE.

       3000-BOOK-PROVISION.
           COMPUTE WS-DELTA = WS-REQUIRED - WS-HELD
           IF WS-DELTA = ZERO
               EXIT PARAGRAPH
           END-IF
           IF WS-DELTA > ZERO
               MOVE GLC-PROVISION-EXPENSE TO GP-DR-GL
               MOVE GLC-LOAN-PROVISION    TO GP-CR-GL
               MOVE WS-DELTA              TO GP-AMOUNT
               MOVE 'ADDITIONAL LOAN LOSS PROVISION' TO GP-NARRATION
           ELSE
               MOVE GLC-LOAN-PROVISION    TO GP-DR-GL
               MOVE GLC-PROVISION-EXPENSE TO GP-CR-GL
               COMPUTE GP-AMOUNT = 0 - WS-DELTA
               MOVE 'EXCESS PROVISION WRITTEN BACK' TO GP-NARRATION
           END-IF
           MOVE SPACES TO GP-REF
           STRING 'PROV' WS-RUN-DATE DELIMITED BY SIZE INTO GP-REF
           CALL 'GLPOST' USING GL-PARMS
           IF NOT GP-OK
               DISPLAY 'PROVBAT: PROVISION GL FAILED, RC=' GP-RC
               MOVE 12 TO WS-RETURN-CODE
           END-IF.

       3100-BOOK-ACCRUAL.
           COMPUTE WS-DELTA = WS-ACCRUED-TOTAL - WS-ACCRUED-HELD
           IF WS-DELTA = ZERO
               EXIT PARAGRAPH
           END-IF
           IF WS-DELTA > ZERO
               MOVE GLC-INT-ACCRUED     TO GP-DR-GL
               MOVE GLC-LOAN-INT-INCOME TO GP-CR-GL
               MOVE WS-DELTA            TO GP-AMOUNT
           ELSE
               MOVE GLC-LOAN-INT-INCOME TO GP-DR-GL
               MOVE GLC-INT-ACCRUED     TO GP-CR-GL
               COMPUTE GP-AMOUNT = 0 - WS-DELTA
           END-IF
           MOVE 'INTEREST ACCRUAL ADJUSTMENT' TO GP-NARRATION
           MOVE SPACES TO GP-REF
           STRING 'ACCR' WS-RUN-DATE DELIMITED BY SIZE INTO GP-REF
           CALL 'GLPOST' USING GL-PARMS
           IF NOT GP-OK
               DISPLAY 'PROVBAT: ACCRUAL GL FAILED, RC=' GP-RC
               MOVE 12 TO WS-RETURN-CODE
           END-IF.

      *================================================================*
      * REPORT                                                         *
      *================================================================*
       9000-REPORT.
           PERFORM 8000-PUT
           MOVE 'CLASS         LOANS        OUTSTANDING   PROV %'
               TO WS-LINE
           STRING '         PROVISION' DELIMITED BY SIZE
               INTO WS-LINE(50:18)
           PERFORM 8000-PUT
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > 5
               MOVE CT-COUNT(WS-C)       TO ED-CNT
               MOVE CT-OUTSTANDING(WS-C) TO ED-AMT
               MOVE CL-PCT(WS-C)         TO ED-PCT
               MOVE CT-PROVISION(WS-C)   TO ED-AMT2
               STRING CL-NAME(WS-C) ' ' ED-CNT ' ' ED-AMT ' ' ED-PCT
                      ' ' ED-AMT2 DELIMITED BY SIZE INTO WS-LINE
               PERFORM 8000-PUT
           END-PERFORM
           PERFORM 8000-PUT
           MOVE WS-REQUIRED TO ED-AMT
           STRING 'PROVISION REQUIRED       ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-HELD TO ED-AMT
           STRING 'PROVISION HELD BEFORE    ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           COMPUTE WS-DELTA = WS-REQUIRED - WS-HELD
           MOVE WS-DELTA TO ED-AMT
           STRING 'CHARGED (+) / RELEASED(-)' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-ACCRUED-TOTAL TO ED-AMT
           STRING 'INTEREST ACCRUED (STD)   ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-RECLASSIFIED TO ED-CNT
           STRING 'LOANS RECLASSIFIED       ' ED-CNT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT.

       8000-PUT.
           DISPLAY FUNCTION TRIM(WS-LINE TRAILING)
           WRITE RPT-LINE FROM WS-LINE
           MOVE SPACES TO WS-LINE.
