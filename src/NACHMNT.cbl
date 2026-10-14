       IDENTIFICATION DIVISION.
       PROGRAM-ID. NACHMNT.
      *================================================================*
      * PROGRAM  : NACHMNT                                             *
      * PURPOSE  : NACH DEBIT MANDATES                                 *
      *              1 - REGISTER A MANDATE                            *
      *              2 - MANDATES OF AN ACCOUNT                        *
      *              3 - MANDATE INQUIRY (UMRN)                        *
      *              4 - CANCEL (CUSTOMER REQUEST)                     *
      *              5 - REACTIVATE A SUSPENDED MANDATE (MANAGER)      *
      *            RULES: ACCOUNT MUST BE ACTIVE; MAXIMUM 10,00,000;   *
      *            END DATE (IF GIVEN) AFTER START DATE; THE SAME      *
      *            CORPORATE + CUSTOMER REFERENCE CANNOT BE ACTIVE     *
      *            TWICE ON ONE ACCOUNT. DEBITS ARE PROCESSED BY       *
      *            NACHBAT.                                            *
      * FILES    : MANDMAST (I-O)                                      *
      * CALLS    : ACCTPOST (INQUIRY), DATEUTIL, AUDITLOG              *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAND-FILE ASSIGN TO 'data/MANDMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS NM-UMRN FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  MAND-FILE.
           COPY MANDREC.

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-CHOICE                   PIC X(01).
       01  WS-EXIT-SW                  PIC X(01) VALUE 'N'.
           88  EXIT-REQUESTED                    VALUE 'Y'.
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-OK-SW                    PIC X(01).
           88  INPUT-OK                          VALUE 'Y'.
       01  WS-TODAY                    PIC 9(08).
       01  WS-IN.
           05  WS-IN-ACCT-NO           PIC 9(10).
           05  WS-IN-CORPORATE         PIC X(30).
           05  WS-IN-CORP-REF          PIC X(20).
           05  WS-IN-MAX               PIC 9(09)V99.
           05  WS-IN-FREQ              PIC X(01).
           05  WS-IN-START             PIC 9(08).
           05  WS-IN-END               PIC 9(08).
           05  WS-IN-UMRN              PIC X(20).
       01  WS-CONFIRM                  PIC X(01).
       01  WS-SERIAL                   PIC 9(08).
       01  WS-COUNT                    PIC 9(04).
       01  WS-MAX-MANDATE              PIC 9(09)V99 VALUE 1000000.
       01  ED-AMT                      PIC Z,ZZZ,ZZ9.99.
       01  WS-STATUS-NAME              PIC X(10).
           COPY POSTPARM.
           COPY DATEPARM.
           COPY SESSION.
           COPY AUDPARM.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           OPEN I-O MAND-FILE
           IF WS-FS = '35'
               OPEN OUTPUT MAND-FILE
               CLOSE MAND-FILE
               OPEN I-O MAND-FILE
           END-IF
           IF WS-FS NOT = '00'
               DISPLAY 'MANDATE FILE OPEN FAILED, FS=' WS-FS
               GOBACK
           END-IF
           MOVE 'N' TO WS-EXIT-SW
           PERFORM UNTIL EXIT-REQUESTED
               DISPLAY ' '
               DISPLAY '------------- NACH MANDATES -------------'
               DISPLAY ' 1. REGISTER A MANDATE'
               DISPLAY ' 2. MANDATES OF AN ACCOUNT'
               DISPLAY ' 3. MANDATE INQUIRY'
               DISPLAY ' 4. CANCEL A MANDATE'
               IF SES-CAN-APPROVE
                   DISPLAY ' 5. REACTIVATE A SUSPENDED MANDATE'
               END-IF
               DISPLAY ' 9. BACK TO MAIN MENU'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               PERFORM 8990-GUARD-END-OF-INPUT
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 1000-REGISTER
                   WHEN '2' PERFORM 2000-LIST-ACCOUNT
                   WHEN '3' PERFORM 3000-INQUIRY
                   WHEN '4' PERFORM 4000-CANCEL
                   WHEN '5' PERFORM 5000-REACTIVATE
                   WHEN '9' SET EXIT-REQUESTED TO TRUE
                   WHEN SPACE CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE MAND-FILE
           GOBACK.

      *================================================================*
      * 1 - REGISTER                                                   *
      *================================================================*
       1000-REGISTER.
           MOVE 'Y' TO WS-OK-SW
           DISPLAY 'ACCOUNT NO            : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'CORPORATE (BENEFICIARY): ' WITH NO ADVANCING
           ACCEPT WS-IN-CORPORATE
           DISPLAY 'CUSTOMER REF AT CORP. : ' WITH NO ADVANCING
           ACCEPT WS-IN-CORP-REF
           DISPLAY 'MAXIMUM AMOUNT        : ' WITH NO ADVANCING
           ACCEPT WS-IN-MAX
           DISPLAY 'FREQUENCY (M/Q/Y/A)   : ' WITH NO ADVANCING
           ACCEPT WS-IN-FREQ
           MOVE FUNCTION UPPER-CASE(WS-IN-FREQ) TO WS-IN-FREQ
           DISPLAY 'START DATE (YYYYMMDD) : ' WITH NO ADVANCING
           ACCEPT WS-IN-START
           DISPLAY 'END DATE (0 = OPEN)   : ' WITH NO ADVANCING
           ACCEPT WS-IN-END

           PERFORM 1100-VALIDATE
           IF NOT INPUT-OK
               EXIT PARAGRAPH
           END-IF
           PERFORM 1200-NEXT-SERIAL
           INITIALIZE MAND-RECORD
           STRING 'CBNK' WS-TODAY WS-SERIAL DELIMITED BY SIZE
               INTO NM-UMRN
           MOVE WS-IN-ACCT-NO   TO NM-ACCT-NO
           MOVE WS-IN-CORPORATE TO NM-CORPORATE
           MOVE WS-IN-CORP-REF  TO NM-CORP-REF
           MOVE WS-IN-MAX       TO NM-MAX-AMOUNT
           MOVE WS-IN-FREQ      TO NM-FREQUENCY
           MOVE WS-IN-START     TO NM-START-DATE
           MOVE WS-IN-END       TO NM-END-DATE
           SET NM-ACTIVE        TO TRUE
           MOVE WS-TODAY        TO NM-REG-DATE
           MOVE SES-USER-ID     TO NM-REG-BY
           WRITE MAND-RECORD
               INVALID KEY
                   DISPLAY 'MANDATE WRITE FAILED, FS=' WS-FS
                   EXIT PARAGRAPH
           END-WRITE
           DISPLAY 'MANDATE REGISTERED. UMRN ' NM-UMRN
           MOVE 'REGISTER' TO AU-ACTION
           PERFORM 9000-AUDIT.

       1100-VALIDATE.
           IF WS-IN-CORPORATE = SPACES OR WS-IN-CORP-REF = SPACES
               DISPLAY 'CORPORATE AND CUSTOMER REFERENCE ARE REQUIRED.'
               MOVE 'N' TO WS-OK-SW
           END-IF
           IF WS-IN-MAX NOT > ZERO OR WS-IN-MAX > WS-MAX-MANDATE
               DISPLAY 'MAXIMUM AMOUNT MUST BE 1 TO 10,00,000.'
               MOVE 'N' TO WS-OK-SW
           END-IF
           MOVE WS-IN-FREQ TO NM-FREQUENCY
           IF NOT NM-FREQ-VALID
               DISPLAY 'FREQUENCY MUST BE M, Q, Y OR A.'
               MOVE 'N' TO WS-OK-SW
           END-IF
           SET DU-VALIDATE TO TRUE
           MOVE WS-IN-START TO DU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DU-OK
               DISPLAY 'INVALID START DATE.'
               MOVE 'N' TO WS-OK-SW
           END-IF
           IF WS-IN-END NOT = ZERO
               MOVE WS-IN-END TO DU-DATE-1
               CALL 'DATEUTIL' USING DATE-PARMS
               IF NOT DU-OK OR WS-IN-END NOT > WS-IN-START
                   DISPLAY 'END DATE MUST BE A VALID DATE AFTER START.'
                   MOVE 'N' TO WS-OK-SW
               END-IF
           END-IF
           IF NOT INPUT-OK
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'ACCOUNT NOT FOUND OR NOT ACTIVE.'
               MOVE 'N' TO WS-OK-SW
               EXIT PARAGRAPH
           END-IF
      *    NO DUPLICATE ACTIVE MANDATE FOR THE SAME CORPORATE REFERENCE
           PERFORM 8000-START-BROWSE
           PERFORM UNTIL END-OF-FILE
               READ MAND-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF NM-ACCT-NO = WS-IN-ACCT-NO AND NM-ACTIVE
                          AND NM-CORPORATE = WS-IN-CORPORATE
                          AND NM-CORP-REF = WS-IN-CORP-REF
                           DISPLAY 'ACTIVE MANDATE ALREADY EXISTS: '
                                   NM-UMRN
                           MOVE 'N' TO WS-OK-SW
                           SET END-OF-FILE TO TRUE
                       END-IF
               END-READ
           END-PERFORM.

       1200-NEXT-SERIAL.
           MOVE ZERO TO WS-SERIAL
           MOVE HIGH-VALUES TO NM-UMRN
           START MAND-FILE KEY IS LESS THAN NM-UMRN
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ MAND-FILE PREVIOUS RECORD
                       NOT AT END
                           IF NM-UMRN(13:8) IS NUMERIC
                               MOVE NM-UMRN(13:8) TO WS-SERIAL
                           END-IF
                   END-READ
           END-START
           ADD 1 TO WS-SERIAL.

      *================================================================*
      * 2 / 3 - ENQUIRIES                                              *
      *================================================================*
       2000-LIST-ACCOUNT.
           DISPLAY 'ACCOUNT NO : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           MOVE ZERO TO WS-COUNT
           PERFORM 8000-START-BROWSE
           PERFORM UNTIL END-OF-FILE
               READ MAND-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF NM-ACCT-NO = WS-IN-ACCT-NO
                           ADD 1 TO WS-COUNT
                           PERFORM 8100-STATUS-NAME
                           MOVE NM-MAX-AMOUNT TO ED-AMT
                           DISPLAY NM-UMRN ' ' NM-CORPORATE(1:20) ' '
                                   ED-AMT ' ' NM-FREQUENCY ' '
                                   WS-STATUS-NAME
                       END-IF
               END-READ
           END-PERFORM
           DISPLAY WS-COUNT ' MANDATE(S)'.

       3000-INQUIRY.
           PERFORM 8200-READ-UMRN
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8100-STATUS-NAME
           DISPLAY 'UMRN          : ' NM-UMRN
           DISPLAY 'ACCOUNT       : ' NM-ACCT-NO
           DISPLAY 'CORPORATE     : ' NM-CORPORATE
           DISPLAY 'CUSTOMER REF  : ' NM-CORP-REF
           MOVE NM-MAX-AMOUNT TO ED-AMT
           DISPLAY 'MAXIMUM       : ' ED-AMT '  FREQUENCY '
                   NM-FREQUENCY
           DISPLAY 'VALID         : ' NM-START-DATE ' TO '
                   NM-END-DATE
           DISPLAY 'STATUS        : ' WS-STATUS-NAME
           MOVE NM-LAST-AMOUNT TO ED-AMT
           DISPLAY 'LAST DEBIT    : ' NM-LAST-DEBIT-DATE ' ' ED-AMT
           DISPLAY 'DEBITS ' NM-DEBITS '  BOUNCES ' NM-BOUNCES
                   ' (IN A ROW ' NM-CONSEC-BOUNCES ')'.

      *================================================================*
      * 4 / 5 - STATUS CHANGES                                         *
      *================================================================*
       4000-CANCEL.
           PERFORM 8200-READ-UMRN
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NM-CANCELLED
               DISPLAY 'MANDATE IS ALREADY CANCELLED.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'CANCEL MANDATE FOR ' NM-CORPORATE ' (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = 'Y'
               DISPLAY 'NOT CANCELLED.'
               EXIT PARAGRAPH
           END-IF
           SET NM-CANCELLED TO TRUE
           REWRITE MAND-RECORD
           DISPLAY 'MANDATE ' NM-UMRN ' CANCELLED.'
           MOVE 'CANCEL' TO AU-ACTION
           PERFORM 9000-AUDIT.

       5000-REACTIVATE.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'NEEDS A MANAGER.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8200-READ-UMRN
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT NM-SUSPENDED
               DISPLAY 'ONLY A SUSPENDED MANDATE CAN BE REACTIVATED.'
               EXIT PARAGRAPH
           END-IF
           SET NM-ACTIVE TO TRUE
           MOVE ZERO TO NM-CONSEC-BOUNCES
           REWRITE MAND-RECORD
           DISPLAY 'MANDATE ' NM-UMRN ' REACTIVATED.'
           MOVE 'REACTIVATE' TO AU-ACTION
           PERFORM 9000-AUDIT.

      *================================================================*
      * COMMON                                                         *
      *================================================================*
       8000-START-BROWSE.
           MOVE 'N' TO WS-EOF-SW
           MOVE LOW-VALUES TO NM-UMRN
           START MAND-FILE KEY IS NOT LESS THAN NM-UMRN
               INVALID KEY SET END-OF-FILE TO TRUE
           END-START.

       8100-STATUS-NAME.
           EVALUATE TRUE
               WHEN NM-ACTIVE    MOVE 'ACTIVE'    TO WS-STATUS-NAME
               WHEN NM-CANCELLED MOVE 'CANCELLED' TO WS-STATUS-NAME
               WHEN NM-SUSPENDED MOVE 'SUSPENDED' TO WS-STATUS-NAME
               WHEN OTHER        MOVE 'UNKNOWN'   TO WS-STATUS-NAME
           END-EVALUATE.

       8200-READ-UMRN.
           DISPLAY 'UMRN : ' WITH NO ADVANCING
           ACCEPT WS-IN-UMRN
           MOVE FUNCTION UPPER-CASE(WS-IN-UMRN) TO NM-UMRN
           READ MAND-FILE
               INVALID KEY
                   DISPLAY 'NO SUCH MANDATE.'
                   MOVE '23' TO WS-FS
           END-READ.

       9000-AUDIT.
           MOVE 'NACHMNT' TO AU-PROGRAM
           MOVE NM-UMRN(5:16) TO AU-KEY
           MOVE SPACES TO AU-DETAIL
           STRING NM-ACCT-NO ' ' NM-CORPORATE DELIMITED BY SIZE
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
