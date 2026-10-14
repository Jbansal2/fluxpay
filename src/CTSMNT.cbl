       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTSMNT.
      *================================================================*
      * PROGRAM  : CTSMNT                                              *
      * PURPOSE  : OUTWARD CHEQUES (CTS) AT THE COUNTER                *
      *              1 - DEPOSIT (LODGE) A CHEQUE ON ANOTHER BANK      *
      *              2 - CHEQUES DEPOSITED INTO AN ACCOUNT             *
      *              3 - CHEQUE STATUS                                 *
      *            NOTHING IS CREDITED AT THE COUNTER - THE CUSTOMER   *
      *            IS PAID ONLY WHEN THE CHEQUE CLEARS (CTSRET).       *
      *            CHECKS: IFSC FORMAT; NOT AN ON-US CHEQUE (CBNK);    *
      *            NOT POST-DATED; NOT STALE (OVER 3 MONTHS); SAME     *
      *            DRAWEE BANK + CHEQUE NO NOT ALREADY IN CLEARING.    *
      * FILES    : OUTCHQ (I-O)                                        *
      * CALLS    : ACCTPOST (INQUIRY), DATEUTIL, AUDITLOG              *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OCHQ-FILE ASSIGN TO 'data/OUTCHQ.DAT'
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS OC-ID FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  OCHQ-FILE.
           COPY OCHQREC.

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
       01  WS-STALE-DATE               PIC 9(08).
       01  WS-NEXT-ID                  PIC 9(08).
       01  WS-COUNT                    PIC 9(04).
       01  WS-IN.
           05  WS-IN-ACCT-NO           PIC 9(10).
           05  WS-IN-IFSC              PIC X(11).
           05  WS-IN-CHEQUE-NO         PIC 9(06).
           05  WS-IN-CHEQUE-DATE       PIC 9(08).
           05  WS-IN-AMOUNT            PIC 9(11)V99.
           05  WS-IN-DRAWER            PIC X(30).
           05  WS-IN-ID                PIC 9(08).
       01  WS-I                        PIC 9(02).
       01  WS-STATUS-NAME              PIC X(11).
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           COPY POSTPARM.
           COPY DATEPARM.
           COPY SESSION.
           COPY AUDPARM.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           OPEN I-O OCHQ-FILE
           IF WS-FS = '35'
               OPEN OUTPUT OCHQ-FILE
               CLOSE OCHQ-FILE
               OPEN I-O OCHQ-FILE
           END-IF
           IF WS-FS NOT = '00'
               DISPLAY 'OUTWARD CHEQUE FILE OPEN FAILED, FS=' WS-FS
               GOBACK
           END-IF
           MOVE 'N' TO WS-EXIT-SW
           PERFORM UNTIL EXIT-REQUESTED
               DISPLAY ' '
               DISPLAY '----------- OUTWARD CHEQUES (CTS) -----------'
               DISPLAY ' 1. DEPOSIT A CHEQUE ON ANOTHER BANK'
               DISPLAY ' 2. CHEQUES DEPOSITED INTO AN ACCOUNT'
               DISPLAY ' 3. CHEQUE STATUS'
               DISPLAY ' 9. BACK'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               PERFORM 8990-GUARD-END-OF-INPUT
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 1000-LODGE
                   WHEN '2' PERFORM 2000-LIST-ACCOUNT
                   WHEN '3' PERFORM 3000-STATUS
                   WHEN '9' SET EXIT-REQUESTED TO TRUE
                   WHEN SPACE CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE OCHQ-FILE
           GOBACK.

      *================================================================*
      * 1 - LODGE                                                      *
      *================================================================*
       1000-LODGE.
           MOVE 'Y' TO WS-OK-SW
           DISPLAY 'CREDIT ACCOUNT NO    : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'DRAWEE BANK IFSC     : ' WITH NO ADVANCING
           ACCEPT WS-IN-IFSC
           MOVE FUNCTION UPPER-CASE(WS-IN-IFSC) TO WS-IN-IFSC
           DISPLAY 'CHEQUE NO            : ' WITH NO ADVANCING
           ACCEPT WS-IN-CHEQUE-NO
           DISPLAY 'CHEQUE DATE YYYYMMDD : ' WITH NO ADVANCING
           ACCEPT WS-IN-CHEQUE-DATE
           DISPLAY 'AMOUNT               : ' WITH NO ADVANCING
           ACCEPT WS-IN-AMOUNT
           DISPLAY 'DRAWER NAME          : ' WITH NO ADVANCING
           ACCEPT WS-IN-DRAWER
           PERFORM 1100-VALIDATE
           IF NOT INPUT-OK
               EXIT PARAGRAPH
           END-IF
           PERFORM 1200-NEXT-ID
           INITIALIZE OCHQ-RECORD
           MOVE WS-NEXT-ID        TO OC-ID
           MOVE WS-IN-ACCT-NO     TO OC-ACCT-NO
           MOVE WS-IN-IFSC        TO OC-DRAWEE-IFSC
           MOVE WS-IN-CHEQUE-NO   TO OC-CHEQUE-NO
           MOVE WS-IN-CHEQUE-DATE TO OC-CHEQUE-DATE
           MOVE WS-IN-AMOUNT      TO OC-AMOUNT
           MOVE WS-IN-DRAWER      TO OC-DRAWER-NAME
           STRING 'IMG' WS-TODAY WS-NEXT-ID DELIMITED BY SIZE
               INTO OC-IMAGE-REF
           SET OC-LODGED          TO TRUE
           MOVE WS-TODAY          TO OC-LODGE-DATE
           MOVE SES-USER-ID       TO OC-LODGED-BY
           WRITE OCHQ-RECORD
               INVALID KEY
                   DISPLAY 'WRITE FAILED, FS=' WS-FS
                   EXIT PARAGRAPH
           END-WRITE
           MOVE OC-AMOUNT TO ED-AMT
           DISPLAY 'CHEQUE LODGED FOR CLEARING. REF ' OC-ID
           DISPLAY 'IMAGE ' OC-IMAGE-REF ' - CREDIT ON CLEARANCE.'
           MOVE 'CTSMNT'  TO AU-PROGRAM
           MOVE 'LODGE'   TO AU-ACTION
           MOVE OC-ID     TO AU-KEY
           MOVE SPACES    TO AU-DETAIL
           STRING OC-ACCT-NO ' ' OC-DRAWEE-IFSC ' ' ED-AMT
               DELIMITED BY SIZE INTO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS.

       1100-VALIDATE.
      *    IFSC: 4 LETTERS, A ZERO, 6 LETTERS/DIGITS
           IF WS-IN-IFSC(1:4) IS NOT ALPHABETIC
              OR WS-IN-IFSC(1:4) = SPACES
              OR WS-IN-IFSC(5:1) NOT = '0'
               DISPLAY 'INVALID IFSC.'
               MOVE 'N' TO WS-OK-SW
           END-IF
           PERFORM VARYING WS-I FROM 6 BY 1 UNTIL WS-I > 11
               IF WS-IN-IFSC(WS-I:1) = SPACE
                   DISPLAY 'INVALID IFSC.'
                   MOVE 'N' TO WS-OK-SW
                   MOVE 11 TO WS-I
               END-IF
           END-PERFORM
           IF WS-IN-IFSC(1:4) = 'CBNK'
               DISPLAY 'ON-US CHEQUE - PRESENT THROUGH INWARD CLEARING.'
               MOVE 'N' TO WS-OK-SW
           END-IF
           IF WS-IN-AMOUNT NOT > ZERO
               DISPLAY 'AMOUNT MUST BE GREATER THAN ZERO.'
               MOVE 'N' TO WS-OK-SW
           END-IF
           IF WS-IN-DRAWER = SPACES
               DISPLAY 'DRAWER NAME IS REQUIRED.'
               MOVE 'N' TO WS-OK-SW
           END-IF
           SET DU-VALIDATE TO TRUE
           MOVE WS-IN-CHEQUE-DATE TO DU-DATE-1
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DU-OK
               DISPLAY 'INVALID CHEQUE DATE.'
               MOVE 'N' TO WS-OK-SW
           ELSE
               SET DU-ADD-MONTHS TO TRUE
               MOVE WS-TODAY TO DU-DATE-1
               MOVE -3 TO DU-NUMBER
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DU-RESULT-DATE TO WS-STALE-DATE
               EVALUATE TRUE
                   WHEN WS-IN-CHEQUE-DATE > WS-TODAY
                       DISPLAY 'CHEQUE IS POST-DATED.'
                       MOVE 'N' TO WS-OK-SW
                   WHEN WS-IN-CHEQUE-DATE < WS-STALE-DATE
                       DISPLAY 'CHEQUE IS STALE (OVER 3 MONTHS OLD).'
                       MOVE 'N' TO WS-OK-SW
               END-EVALUATE
           END-IF
           IF NOT INPUT-OK
               EXIT PARAGRAPH
           END-IF
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'CREDIT ACCOUNT NOT FOUND OR NOT ACTIVE.'
               MOVE 'N' TO WS-OK-SW
               EXIT PARAGRAPH
           END-IF
      *    SAME INSTRUMENT ALREADY IN THE PIPELINE?
           PERFORM 8000-START-BROWSE
           PERFORM UNTIL END-OF-FILE
               READ OCHQ-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF OC-DRAWEE-IFSC(1:4) = WS-IN-IFSC(1:4)
                          AND OC-CHEQUE-NO = WS-IN-CHEQUE-NO
                          AND (OC-LODGED OR OC-SENT OR OC-CLEARED)
                           DISPLAY 'CHEQUE ALREADY DEPOSITED, REF '
                                   OC-ID
                           MOVE 'N' TO WS-OK-SW
                           SET END-OF-FILE TO TRUE
                       END-IF
               END-READ
           END-PERFORM.

       1200-NEXT-ID.
           MOVE ZERO TO WS-NEXT-ID
           MOVE 99999999 TO OC-ID
           START OCHQ-FILE KEY IS LESS THAN OR EQUAL OC-ID
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ OCHQ-FILE PREVIOUS RECORD
                       NOT AT END MOVE OC-ID TO WS-NEXT-ID
                   END-READ
           END-START
           ADD 1 TO WS-NEXT-ID.

      *================================================================*
      * 2 / 3 - ENQUIRIES                                              *
      *================================================================*
       2000-LIST-ACCOUNT.
           DISPLAY 'ACCOUNT NO : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           MOVE ZERO TO WS-COUNT
           PERFORM 8000-START-BROWSE
           PERFORM UNTIL END-OF-FILE
               READ OCHQ-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF OC-ACCT-NO = WS-IN-ACCT-NO
                           ADD 1 TO WS-COUNT
                           PERFORM 8100-STATUS-NAME
                           MOVE OC-AMOUNT TO ED-AMT
                           DISPLAY OC-ID ' ' OC-DRAWEE-IFSC ' '
                                   OC-CHEQUE-NO ' ' ED-AMT ' '
                                   WS-STATUS-NAME
                       END-IF
               END-READ
           END-PERFORM
           DISPLAY WS-COUNT ' CHEQUE(S)'.

       3000-STATUS.
           DISPLAY 'REFERENCE NO : ' WITH NO ADVANCING
           ACCEPT WS-IN-ID
           MOVE WS-IN-ID TO OC-ID
           READ OCHQ-FILE
               INVALID KEY
                   DISPLAY 'NO SUCH REFERENCE.'
                   EXIT PARAGRAPH
           END-READ
           PERFORM 8100-STATUS-NAME
           MOVE OC-AMOUNT TO ED-AMT
           DISPLAY 'ACCOUNT      : ' OC-ACCT-NO
           DISPLAY 'DRAWEE       : ' OC-DRAWEE-IFSC ' CHEQUE '
                   OC-CHEQUE-NO ' DATED ' OC-CHEQUE-DATE
           DISPLAY 'DRAWER       : ' OC-DRAWER-NAME
           DISPLAY 'AMOUNT       : ' ED-AMT
           DISPLAY 'STATUS       : ' WS-STATUS-NAME
           DISPLAY 'LODGED       : ' OC-LODGE-DATE ' BY '
                   OC-LODGED-BY
           IF OC-BATCH-DATE NOT = ZERO
               DISPLAY 'PRESENTED    : ' OC-BATCH-DATE
           END-IF
           IF OC-RETURNED
               DISPLAY 'RETURNED     : ' OC-RETURN-REASON ' '
                       OC-RETURN-TEXT
           END-IF.

      *================================================================*
      * COMMON                                                         *
      *================================================================*
       8000-START-BROWSE.
           MOVE 'N' TO WS-EOF-SW
           MOVE ZERO TO OC-ID
           START OCHQ-FILE KEY IS NOT LESS THAN OC-ID
               INVALID KEY SET END-OF-FILE TO TRUE
           END-START.

       8100-STATUS-NAME.
           EVALUATE TRUE
               WHEN OC-LODGED   MOVE 'LODGED'   TO WS-STATUS-NAME
               WHEN OC-SENT     MOVE 'IN CLEARING' TO WS-STATUS-NAME
               WHEN OC-CLEARED  MOVE 'CREDITED' TO WS-STATUS-NAME
               WHEN OC-RETURNED MOVE 'RETURNED' TO WS-STATUS-NAME
               WHEN OTHER       MOVE 'UNKNOWN'  TO WS-STATUS-NAME
           END-EVALUATE.

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
