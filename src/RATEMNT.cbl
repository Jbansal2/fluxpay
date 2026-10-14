       IDENTIFICATION DIVISION.
       PROGRAM-ID. RATEMNT.
      *================================================================*
      * PROGRAM  : RATEMNT                                             *
      * PURPOSE  : INTEREST RATE MASTER MAINTENANCE (MANAGER).         *
      *              1 - LIST RATES                                    *
      *              2 - CHANGE A RATE                                 *
      *              3 - RESET REPO-LINKED LOANS NOW (LNRESET)       *
      *            THE FIRST USE SEEDS THE MASTER FROM THE BUILT-IN    *
      *            TABLE (RATEDFLT). EVERY CHANGE IS KEPT IN           *
      *            data/RATEHIST.DAT AND THE AUDIT TRAIL. A SINGLE     *
      *            CHANGE IS LIMITED TO 2.00 PERCENTAGE POINTS - A     *
      *            BIGGER MOVE IS ALMOST ALWAYS A TYPING MISTAKE.      *
      * FILES    : RATEMAST (I-O), RATEHIST (EXTEND)                   *
      * CALLS    : LNRESET, AUDITLOG                                 *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATE-FILE ASSIGN TO 'data/RATEMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS RATE-CODE FILE STATUS IS WS-FS.
           SELECT HIST-FILE ASSIGN TO 'data/RATEHIST.DAT'
               ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS WS-HFS.

       DATA DIVISION.
       FILE SECTION.
       FD  RATE-FILE.
           COPY RATEREC.
       FD  HIST-FILE.
       01  HIST-LINE                   PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-HFS                      PIC X(02).
       01  WS-CHOICE                   PIC X(01).
       01  WS-EXIT-SW                  PIC X(01) VALUE 'N'.
           88  EXIT-REQUESTED                    VALUE 'Y'.
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-IN-CODE                  PIC X(08).
       01  WS-IN-VALUE                 PIC X(10).
       01  WS-NEW-VALUE                PIC 9(02)V9(04).
       01  WS-DIFF                     PIC S9(02)V9(04).
       01  WS-MAX-MOVE                 PIC 9(02)V9(04) VALUE 2.
       01  WS-CONFIRM                  PIC X(01).
       01  WS-TODAY                    PIC 9(08).
       01  ED-RATE                     PIC Z9.9999.
       01  ED-RATE2                    PIC Z9.9999.
           COPY RATEDFLT.
           COPY SESSION.
           COPY AUDPARM.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'RATE MAINTENANCE NEEDS A MANAGER.'
               GOBACK
           END-IF
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           PERFORM 1000-OPEN-OR-SEED
           IF WS-FS NOT = '00'
               GOBACK
           END-IF
           MOVE 'N' TO WS-EXIT-SW
           PERFORM UNTIL EXIT-REQUESTED
               DISPLAY ' '
               DISPLAY '------------- INTEREST RATES -------------'
               DISPLAY ' 1. LIST RATES'
               DISPLAY ' 2. CHANGE A RATE'
               DISPLAY ' 3. RESET REPO-LINKED LOANS NOW'
               DISPLAY ' 9. BACK TO MAIN MENU'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               PERFORM 8990-GUARD-END-OF-INPUT
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-LIST
                   WHEN '2' PERFORM 3000-CHANGE
                   WHEN '3'
                       CLOSE RATE-FILE
                       CALL 'LNRESET'
                       OPEN I-O RATE-FILE
                   WHEN '9' SET EXIT-REQUESTED TO TRUE
                   WHEN SPACE CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE RATE-FILE
           GOBACK.

       1000-OPEN-OR-SEED.
           OPEN I-O RATE-FILE
           IF WS-FS = '35'
               OPEN OUTPUT RATE-FILE
               PERFORM VARYING RDF-IDX FROM 1 BY 1 UNTIL RDF-IDX > 8
                   INITIALIZE RATE-RECORD
                   MOVE RDF-CODE(RDF-IDX)  TO RATE-CODE
                   MOVE RDF-DESC(RDF-IDX)  TO RATE-DESC
                   MOVE RDF-VALUE(RDF-IDX) TO RATE-VALUE
                                              RATE-PREV-VALUE
                   MOVE WS-TODAY           TO RATE-EFF-DATE
                   MOVE 'SYSTEM'           TO RATE-CHANGED-BY
                   WRITE RATE-RECORD
               END-PERFORM
               CLOSE RATE-FILE
               DISPLAY 'RATE MASTER CREATED WITH STARTING RATES.'
               OPEN I-O RATE-FILE
           END-IF
           IF WS-FS NOT = '00'
               DISPLAY 'RATE MASTER OPEN FAILED, FS=' WS-FS
           END-IF.

       2000-LIST.
           DISPLAY 'CODE     DESCRIPTION                       RATE'
                   '  PREVIOUS  FROM'
           MOVE LOW-VALUES TO RATE-CODE
           MOVE 'N' TO WS-EOF-SW
           START RATE-FILE KEY IS NOT LESS THAN RATE-CODE
               INVALID KEY SET END-OF-FILE TO TRUE
           END-START
           PERFORM UNTIL END-OF-FILE
               READ RATE-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       MOVE RATE-VALUE      TO ED-RATE
                       MOVE RATE-PREV-VALUE TO ED-RATE2
                       DISPLAY RATE-CODE ' ' RATE-DESC ' ' ED-RATE
                               '  ' ED-RATE2 '  ' RATE-EFF-DATE
               END-READ
           END-PERFORM.

       3000-CHANGE.
           DISPLAY 'RATE CODE : ' WITH NO ADVANCING
           ACCEPT WS-IN-CODE
           MOVE FUNCTION UPPER-CASE(WS-IN-CODE) TO RATE-CODE
           READ RATE-FILE
               INVALID KEY
                   DISPLAY 'NO SUCH RATE CODE.'
                   EXIT PARAGRAPH
           END-READ
           MOVE RATE-VALUE TO ED-RATE
           DISPLAY 'CURRENT   : ' ED-RATE '%'
           DISPLAY 'NEW RATE  : ' WITH NO ADVANCING
           ACCEPT WS-IN-VALUE
           IF FUNCTION TEST-NUMVAL(WS-IN-VALUE) NOT = ZERO
               DISPLAY 'NOT A NUMBER.'
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-NEW-VALUE = FUNCTION NUMVAL(WS-IN-VALUE)
           IF WS-NEW-VALUE = ZERO OR WS-NEW-VALUE > 30
               DISPLAY 'RATE MUST BE ABOVE 0 AND AT MOST 30.'
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-DIFF = WS-NEW-VALUE - RATE-VALUE
           IF FUNCTION ABS(WS-DIFF) > WS-MAX-MOVE
               DISPLAY 'CHANGE OVER 2.00 POINTS REFUSED - CHECK INPUT.'
               EXIT PARAGRAPH
           END-IF
           IF WS-DIFF = ZERO
               DISPLAY 'RATE UNCHANGED.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-NEW-VALUE TO ED-RATE2
           DISPLAY 'CHANGE ' FUNCTION TRIM(RATE-CODE) ' FROM ' ED-RATE
                   ' TO ' ED-RATE2 ' (Y/N): ' WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
               DISPLAY 'NOT CHANGED.'
               EXIT PARAGRAPH
           END-IF
           MOVE RATE-VALUE   TO RATE-PREV-VALUE
           MOVE WS-NEW-VALUE TO RATE-VALUE
           MOVE WS-TODAY     TO RATE-EFF-DATE
           MOVE SES-USER-ID  TO RATE-CHANGED-BY
           REWRITE RATE-RECORD
               INVALID KEY
                   DISPLAY 'REWRITE FAILED, FS=' WS-FS
                   EXIT PARAGRAPH
           END-REWRITE
           DISPLAY 'RATE ' FUNCTION TRIM(RATE-CODE) ' CHANGED TO '
                   ED-RATE2
           PERFORM 3100-HISTORY
           IF RATE-CODE = 'REPO' OR RATE-CODE = 'HLSPREAD'
               DISPLAY 'RUN OPTION 3 (OR EOD) TO RESET HOME LOANS.'
           END-IF.

       3100-HISTORY.
           OPEN EXTEND HIST-FILE
           IF WS-HFS = '35'
               OPEN OUTPUT HIST-FILE
           END-IF
           MOVE SPACES TO HIST-LINE
           STRING WS-TODAY ' ' RATE-CODE ' ' ED-RATE ' -> ' ED-RATE2
                  ' BY ' SES-USER-ID DELIMITED BY SIZE INTO HIST-LINE
           WRITE HIST-LINE
           CLOSE HIST-FILE
           MOVE 'RATEMNT'     TO AU-PROGRAM
           MOVE 'RATE CHANGE' TO AU-ACTION
           MOVE RATE-CODE     TO AU-KEY
           MOVE SPACES        TO AU-DETAIL
           STRING ED-RATE ' -> ' ED-RATE2 DELIMITED BY SIZE
               INTO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       8990-GUARD-END-OF-INPUT.
           IF WS-CHOICE = SPACE
               ADD 1 TO WS-BLANK-INPUTS
               IF WS-BLANK-INPUTS > 20
                   SET EXIT-REQUESTED TO TRUE
               END-IF
           ELSE
               MOVE ZERO TO WS-BLANK-INPUTS
           END-IF.
