       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMNT.
      *================================================================*
      * PROGRAM  : SIMNT                                               *
      * PURPOSE  : STANDING INSTRUCTIONS - SET UP A MONTHLY            *
      *            TRANSFER, LOAN EMI DEBIT OR RD INSTALMENT DEBIT,    *
      *            INQUIRE, SUSPEND / RESUME, CANCEL, LIST.            *
      *            EXECUTION IS DONE BY SIBATCH.                       *
      *            RULES: DAY OF MONTH 1-28 (SO EVERY MONTH HAS IT);   *
      *            A LOAN / RD SI MUST DEBIT THE LOAN'S / RD'S OWN     *
      *            LINKED ACCOUNT; ONE ACTIVE SI PER LOAN OR RD.       *
      * FILES    : SIMAST (I-O), LOANMAST, DEPMAST (INPUT)             *
      * CALLS    : ACCTPOST (INQUIRY), DATEUTIL, AUDITLOG              *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SI-FILE ASSIGN TO 'data/SIMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SI-ID
               FILE STATUS IS WS-SI-FS.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LOAN-NO
               FILE STATUS IS WS-LOAN-FS.
           SELECT DEP-FILE ASSIGN TO 'data/DEPMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS DEP-NO
               FILE STATUS IS WS-DEP-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  SI-FILE.
           COPY SIREC.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  DEP-FILE.
           COPY DEPREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY DATEPARM.
           COPY SESSION.
           COPY AUDPARM.

       01  WS-SI-FS                    PIC X(02).
           88  SI-FS-OK                          VALUE '00'.
           88  SI-FS-NOFILE                      VALUE '35'.
       01  WS-LOAN-FS                  PIC X(02).
           88  LOAN-FS-OK                        VALUE '00'.
       01  WS-DEP-FS                   PIC X(02).
           88  DEP-FS-OK                         VALUE '00'.

       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
           88  CHOICE-BACK                       VALUE '9'.
       01  WS-CONFIRM                  PIC X(01).
           88  CONFIRM-YES                       VALUES 'Y' 'y'.
       01  WS-VALID-SW                 PIC X(01).
           88  INPUT-VALID                       VALUE 'Y'.
           88  INPUT-INVALID                     VALUE 'N'.

       01  WS-IN.
           05  WS-IN-SI-ID             PIC 9(08).
           05  WS-IN-FROM              PIC 9(10).
           05  WS-IN-TYPE              PIC X(01).
           05  WS-IN-TARGET            PIC 9(10).
           05  WS-IN-AMOUNT            PIC 9(11)V99.
           05  WS-IN-DAY               PIC 9(02).
           05  WS-IN-END               PIC X(08).

       01  WS-NEXT-SI-ID               PIC 9(08).
       01  WS-FIRST-RUN                PIC 9(08).
       01  WS-FIRST-RUN-R REDEFINES WS-FIRST-RUN.
           05  WS-FR-YYYYMM            PIC 9(06).
           05  WS-FR-DD                PIC 9(02).
       01  WS-LIST-CNT                 PIC 9(05).
       01  WS-SAVE-SI                  PIC X(150).
       01  WS-DUP-SW                   PIC X(01).
           88  DUPLICATE-SI                      VALUE 'Y'.
       01  WS-DISP-AMT                 PIC ZZ,ZZZ,ZZZ,ZZ9.99.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN I-O SI-FILE
           IF SI-FS-NOFILE
               OPEN OUTPUT SI-FILE
               CLOSE SI-FILE
               OPEN I-O SI-FILE
           END-IF
           IF NOT SI-FS-OK
               DISPLAY 'SI MASTER OPEN FAILED, FS=' WS-SI-FS
               GOBACK
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL CHOICE-BACK
               PERFORM 1000-SHOW-MENU
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-CREATE
                   WHEN '2' PERFORM 3000-INQUIRY
                   WHEN '3' PERFORM 4000-SUSPEND-RESUME
                   WHEN '4' PERFORM 4500-CANCEL
                   WHEN '5' PERFORM 5000-LIST-FOR-ACCOUNT
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE SI-FILE
           GOBACK.

       1000-SHOW-MENU.
           DISPLAY ' '
           DISPLAY '--------- STANDING INSTRUCTIONS ---------'
           DISPLAY ' 1. NEW STANDING INSTRUCTION'
           DISPLAY ' 2. INQUIRY'
           DISPLAY ' 3. SUSPEND / RESUME'
           DISPLAY ' 4. CANCEL'
           DISPLAY ' 5. LIST FOR AN ACCOUNT'
           DISPLAY ' 9. BACK TO MAIN MENU'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT.

      *================================================================*
      * CREATE                                                         *
      *================================================================*
       2000-CREATE.
           SET INPUT-VALID TO TRUE
           INITIALIZE WS-IN
           DISPLAY 'DEBIT ACCOUNT NO             : ' WITH NO ADVANCING
           ACCEPT WS-IN-FROM
           DISPLAY 'TYPE (T=TRANSFER L=LOAN EMI R=RD): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-TYPE
           MOVE FUNCTION UPPER-CASE(WS-IN-TYPE) TO WS-IN-TYPE
           DISPLAY 'TARGET ACCOUNT / LOAN / RD NO : ' WITH NO ADVANCING
           ACCEPT WS-IN-TARGET
           IF WS-IN-TYPE = 'T'
               DISPLAY 'MONTHLY AMOUNT               : '
                       WITH NO ADVANCING
               ACCEPT WS-IN-AMOUNT
           END-IF
           DISPLAY 'DAY OF MONTH (1-28)          : ' WITH NO ADVANCING
           ACCEPT WS-IN-DAY
           DISPLAY 'END DATE YYYYMMDD (BLANK=NONE): ' WITH NO ADVANCING
           ACCEPT WS-IN-END

           PERFORM 2100-VALIDATE
           IF INPUT-INVALID
               DISPLAY 'STANDING INSTRUCTION NOT CREATED.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 2200-FIRST-RUN-DATE
           PERFORM 2300-NEXT-SI-ID
           PERFORM 2400-WRITE-SI.

       2100-VALIDATE.
           MOVE WS-IN-TYPE TO SI-TYPE
           IF NOT SI-TYPE-VALID
               DISPLAY '- TYPE MUST BE T, L OR R.'
               SET INPUT-INVALID TO TRUE
               EXIT PARAGRAPH
           END-IF
           IF WS-IN-DAY < 1 OR WS-IN-DAY > 28
               DISPLAY '- DAY OF MONTH MUST BE 1 TO 28.'
               SET INPUT-INVALID TO TRUE
           END-IF
           IF WS-IN-END NOT = SPACES
               IF WS-IN-END IS NUMERIC
                   MOVE WS-IN-END TO DU-DATE-1
                   SET DU-VALIDATE TO TRUE
                   CALL 'DATEUTIL' USING DATE-PARMS
               END-IF
               IF WS-IN-END NOT NUMERIC OR NOT DU-OK
                   DISPLAY '- END DATE IS NOT A VALID DATE.'
                   SET INPUT-INVALID TO TRUE
               END-IF
           END-IF
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-FROM TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY '- DEBIT ACCOUNT NOT FOUND.'
               SET INPUT-INVALID TO TRUE
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN SI-TRANSFER
                   PERFORM 2110-VALIDATE-TRANSFER
               WHEN SI-LOAN-EMI
                   PERFORM 2120-VALIDATE-LOAN
               WHEN SI-RD-INSTALMENT
                   PERFORM 2130-VALIDATE-RD
           END-EVALUATE
           IF INPUT-VALID AND NOT SI-TRANSFER
               PERFORM 2140-CHECK-NO-DUPLICATE
           END-IF.

       2110-VALIDATE-TRANSFER.
           IF WS-IN-AMOUNT NOT > ZERO
               DISPLAY '- AMOUNT MUST BE GREATER THAN ZERO.'
               SET INPUT-INVALID TO TRUE
           END-IF
           IF WS-IN-TARGET = WS-IN-FROM
               DISPLAY '- CANNOT TRANSFER TO THE SAME ACCOUNT.'
               SET INPUT-INVALID TO TRUE
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-TARGET TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY '- TARGET ACCOUNT NOT FOUND.'
               SET INPUT-INVALID TO TRUE
           END-IF.

       2120-VALIDATE-LOAN.
           OPEN INPUT LOAN-FILE
           MOVE WS-IN-TARGET TO LOAN-NO
           READ LOAN-FILE
               INVALID KEY
                   DISPLAY '- LOAN NOT FOUND.'
                   SET INPUT-INVALID TO TRUE
           END-READ
           IF INPUT-VALID
               EVALUATE TRUE
                   WHEN LOAN-CLOSED
                       DISPLAY '- LOAN IS CLOSED.'
                       SET INPUT-INVALID TO TRUE
                   WHEN LOAN-ACCT-NO NOT = WS-IN-FROM
                       DISPLAY '- LOAN EMI MUST BE DEBITED FROM ITS '
                               'LINKED ACCOUNT ' LOAN-ACCT-NO
                       SET INPUT-INVALID TO TRUE
                   WHEN OTHER
                       MOVE LOAN-EMI TO WS-IN-AMOUNT
               END-EVALUATE
           END-IF
           CLOSE LOAN-FILE.

       2130-VALIDATE-RD.
           OPEN INPUT DEP-FILE
           MOVE WS-IN-TARGET TO DEP-NO
           READ DEP-FILE
               INVALID KEY
                   DISPLAY '- DEPOSIT NOT FOUND.'
                   SET INPUT-INVALID TO TRUE
           END-READ
           IF INPUT-VALID
               EVALUATE TRUE
                   WHEN NOT DEP-RD OR NOT DEP-ACTIVE
                       DISPLAY '- NOT AN ACTIVE RECURRING DEPOSIT.'
                       SET INPUT-INVALID TO TRUE
                   WHEN DEP-ACCT-NO NOT = WS-IN-FROM
                       DISPLAY '- RD MUST BE FUNDED FROM ITS LINKED '
                               'ACCOUNT ' DEP-ACCT-NO
                       SET INPUT-INVALID TO TRUE
                   WHEN OTHER
                       MOVE DEP-AMOUNT TO WS-IN-AMOUNT
               END-EVALUATE
           END-IF
           CLOSE DEP-FILE.

      *    ONLY ONE LIVE SI MAY PAY A GIVEN LOAN OR RD
       2140-CHECK-NO-DUPLICATE.
           MOVE 'N' TO WS-DUP-SW
           MOVE ZERO TO SI-ID
           START SI-FILE KEY IS NOT LESS THAN SI-ID
               INVALID KEY EXIT PARAGRAPH
           END-START
           PERFORM FOREVER
               READ SI-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF SI-TARGET = WS-IN-TARGET AND SI-TYPE = WS-IN-TYPE
                  AND (SI-ACTIVE OR SI-SUSPENDED)
                   MOVE 'Y' TO WS-DUP-SW
                   EXIT PERFORM
               END-IF
           END-PERFORM
           IF DUPLICATE-SI
               DISPLAY '- SI ' SI-ID ' ALREADY PAYS THIS TARGET.'
               SET INPUT-INVALID TO TRUE
           END-IF.

      *    FIRST RUN = THE CHOSEN DAY IN THIS MONTH IF STILL AHEAD,
      *    OTHERWISE IN NEXT MONTH
       2200-FIRST-RUN-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE WS-CURR-YYYYMM TO WS-FR-YYYYMM
           MOVE WS-IN-DAY      TO WS-FR-DD
           IF WS-FIRST-RUN <= WS-CURR-DATE
               SET DU-ADD-MONTHS TO TRUE
               MOVE WS-FIRST-RUN TO DU-DATE-1
               MOVE 1 TO DU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DU-RESULT-DATE TO WS-FIRST-RUN
           END-IF.

       2300-NEXT-SI-ID.
           MOVE 30000000 TO WS-NEXT-SI-ID
           MOVE 99999999 TO SI-ID
           START SI-FILE KEY IS LESS THAN OR EQUAL SI-ID
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ SI-FILE PREVIOUS RECORD
                       AT END CONTINUE
                       NOT AT END MOVE SI-ID TO WS-NEXT-SI-ID
                   END-READ
           END-START
           ADD 1 TO WS-NEXT-SI-ID.

       2400-WRITE-SI.
           INITIALIZE SI-RECORD
           MOVE WS-NEXT-SI-ID  TO SI-ID
           MOVE WS-IN-FROM     TO SI-FROM-ACCT
           MOVE WS-IN-TYPE     TO SI-TYPE
           MOVE WS-IN-TARGET   TO SI-TARGET
           MOVE WS-IN-AMOUNT   TO SI-AMOUNT
           MOVE WS-IN-DAY      TO SI-DAY-OF-MONTH
           MOVE WS-CURR-DATE   TO SI-START-DATE
           IF WS-IN-END IS NUMERIC
               MOVE WS-IN-END  TO SI-END-DATE
           ELSE
               MOVE 99991231   TO SI-END-DATE
           END-IF
           SET SI-ACTIVE       TO TRUE
           MOVE WS-FIRST-RUN   TO SI-NEXT-RUN-DATE SI-CYCLE-DUE-DATE
           MOVE 'NOT YET RUN'  TO SI-LAST-RESULT
           MOVE SES-USER-ID    TO SI-CREATED-BY
           WRITE SI-RECORD
               INVALID KEY
                   DISPLAY 'SI WRITE FAILED, FS=' WS-SI-FS
                   EXIT PARAGRAPH
           END-WRITE
           MOVE SI-AMOUNT TO WS-DISP-AMT
           DISPLAY 'SI ' SI-ID ' CREATED: ' WS-DISP-AMT
                   ' ON DAY ' SI-DAY-OF-MONTH ', FIRST RUN '
                   SI-NEXT-RUN-DATE
           MOVE 'SI-CREATE' TO AU-ACTION
           PERFORM 9000-AUDIT.

      *================================================================*
      * INQUIRY / SUSPEND / CANCEL / LIST                              *
      *================================================================*
       3000-INQUIRY.
           PERFORM 8000-READ-SI
           IF NOT SI-FS-OK
               EXIT PARAGRAPH
           END-IF
           DISPLAY '----------------------------------------'
           DISPLAY 'SI          : ' SI-ID '   STATUS ' SI-STATUS
           EVALUATE TRUE
               WHEN SI-TRANSFER
                   DISPLAY 'TYPE        : TRANSFER TO ' SI-TARGET
               WHEN SI-LOAN-EMI
                   DISPLAY 'TYPE        : LOAN EMI FOR ' SI-TARGET
               WHEN SI-RD-INSTALMENT
                   DISPLAY 'TYPE        : RD INSTALMENT FOR ' SI-TARGET
           END-EVALUATE
           DISPLAY 'FROM ACCOUNT: ' SI-FROM-ACCT
           MOVE SI-AMOUNT TO WS-DISP-AMT
           DISPLAY 'AMOUNT      : ' WS-DISP-AMT
           DISPLAY 'DAY / END   : ' SI-DAY-OF-MONTH ' / ' SI-END-DATE
           DISPLAY 'NEXT RUN    : ' SI-NEXT-RUN-DATE
           DISPLAY 'LAST RUN    : ' SI-LAST-RUN-DATE '  '
                   FUNCTION TRIM(SI-LAST-RESULT)
           DISPLAY 'EXECUTED    : ' SI-EXECUTED-CNT '   MISSED '
                   SI-MISSED-CNT
           DISPLAY '----------------------------------------'.

       4000-SUSPEND-RESUME.
           PERFORM 8000-READ-SI
           IF NOT SI-FS-OK
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRUE
               WHEN SI-ACTIVE
                   SET SI-SUSPENDED TO TRUE
                   MOVE 'SI-SUSPEND' TO AU-ACTION
               WHEN SI-SUSPENDED
                   SET SI-ACTIVE TO TRUE
                   MOVE ZERO TO SI-ATTEMPTS
                   MOVE 'SI-RESUME' TO AU-ACTION
               WHEN OTHER
                   DISPLAY 'SI IS CANCELLED OR COMPLETED.'
                   EXIT PARAGRAPH
           END-EVALUATE
           REWRITE SI-RECORD
           DISPLAY 'SI ' SI-ID ' IS NOW STATUS ' SI-STATUS
           PERFORM 9000-AUDIT.

       4500-CANCEL.
           PERFORM 8000-READ-SI
           IF NOT SI-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF SI-CANCELLED OR SI-COMPLETED
               DISPLAY 'SI IS ALREADY CLOSED.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'CANCEL SI ' SI-ID ' (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF NOT CONFIRM-YES
               EXIT PARAGRAPH
           END-IF
           SET SI-CANCELLED TO TRUE
           REWRITE SI-RECORD
           DISPLAY 'SI CANCELLED.'
           MOVE 'SI-CANCEL' TO AU-ACTION
           PERFORM 9000-AUDIT.

       5000-LIST-FOR-ACCOUNT.
           DISPLAY 'ACCOUNT NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-FROM
           MOVE ZERO TO WS-LIST-CNT SI-ID
           START SI-FILE KEY IS NOT LESS THAN SI-ID
               INVALID KEY
                   DISPLAY 'NO STANDING INSTRUCTIONS.'
                   EXIT PARAGRAPH
           END-START
           DISPLAY 'SI ID    T TARGET             AMOUNT DAY S NEXT RUN'
           PERFORM FOREVER
               READ SI-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF SI-FROM-ACCT = WS-IN-FROM
                   ADD 1 TO WS-LIST-CNT
                   MOVE SI-AMOUNT TO WS-DISP-AMT
                   DISPLAY SI-ID ' ' SI-TYPE ' ' SI-TARGET
                           WS-DISP-AMT ' ' SI-DAY-OF-MONTH ' '
                           SI-STATUS ' ' SI-NEXT-RUN-DATE
               END-IF
           END-PERFORM
           IF WS-LIST-CNT = ZERO
               DISPLAY 'NO STANDING INSTRUCTIONS FOR THIS ACCOUNT.'
           END-IF.

       8000-READ-SI.
           DISPLAY 'SI ID: ' WITH NO ADVANCING
           ACCEPT WS-IN-SI-ID
           MOVE WS-IN-SI-ID TO SI-ID
           READ SI-FILE
               INVALID KEY
                   DISPLAY 'SI ' WS-IN-SI-ID ' NOT FOUND.'
           END-READ.

       9000-AUDIT.
           MOVE 'SIMNT'  TO AU-PROGRAM
           MOVE SI-ID    TO AU-KEY
           MOVE SPACES   TO AU-DETAIL
           STRING SI-TYPE ' FROM ' SI-FROM-ACCT ' TO ' SI-TARGET
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
