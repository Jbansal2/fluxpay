       IDENTIFICATION DIVISION.
       PROGRAM-ID. HLDMNT.
      *================================================================*
      * PROGRAM  : HLDMNT                                              *
      * PURPOSE  : ACCOUNT HOLDERS AND NOMINEE.                        *
      *            - LINK THE PRIMARY HOLDER (KYC-VERIFIED CUSTOMER)   *
      *            - ADD / REMOVE JOINT HOLDERS (MAX 3 HOLDERS)        *
      *            - OPERATING MODE: S SINGLE, E EITHER OR SURVIVOR,   *
      *              F FORMER OR SURVIVOR, J JOINTLY (ALL SIGN).       *
      *              S NEEDS EXACTLY ONE HOLDER; THE OTHERS NEED 2+.   *
      *              REMOVING A JOINT HOLDER DOWN TO ONE RESETS THE    *
      *              MODE TO S.                                        *
      *            - NOMINEE (ONE PER ACCOUNT). A MINOR NOMINEE        *
      *              (UNDER 18 TODAY) MUST HAVE A GUARDIAN.            *
      * FILES    : ACCTREL (INDEXED, I-O), CUSTMAST (INPUT)            *
      * CALLS    : ACCTPOST (INQUIRY), DATEUTIL, AUDITLOG              *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HLD-FILE ASSIGN TO 'data/ACCTREL.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS HLD-KEY
               FILE STATUS IS WS-HLD-FS.
           SELECT CUST-FILE ASSIGN TO 'data/CUSTMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-CUST-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  HLD-FILE.
           COPY HLDREC.
       FD  CUST-FILE.
           COPY CUSTREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY POSTPARM.
           COPY DATEPARM.
           COPY AUDPARM.

       01  WS-HLD-FS                   PIC X(02).
           88  HLD-FS-OK                         VALUE '00'.
           88  HLD-FS-NOFILE                     VALUE '35'.
       01  WS-CUST-FS                  PIC X(02).
           88  CUST-FS-OK                        VALUE '00'.

       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
           88  CHOICE-BACK                       VALUE '9'.
       01  WS-CONFIRM                  PIC X(01).
           88  CONFIRM-YES                       VALUES 'Y' 'y'.

       01  WS-IN-ACCT-NO               PIC 9(10).
       01  WS-IN-CUST-ID               PIC 9(08).
       01  WS-IN-MODE                  PIC X(01).
       01  WS-IN-NOM-NAME              PIC X(40).
       01  WS-IN-NOM-REL               PIC X(15).
       01  WS-IN-NOM-DOB               PIC X(08).
       01  WS-IN-GUARDIAN              PIC X(40).

      * THE ACCOUNT'S CURRENT RELATIONSHIPS, LOADED BY 8100
       01  WS-REL.
           05  WS-HAS-HEADER           PIC X(01).
               88  REL-EXISTS                    VALUE 'Y'.
           05  WS-CUR-MODE             PIC X(01).
           05  WS-HOLDER-CNT           PIC 9(01).
           05  WS-HOLDER OCCURS 3 TIMES.
               10  WS-H-SEQ            PIC 9(02).
               10  WS-H-CUST           PIC 9(08).
               10  WS-H-ROLE           PIC X(01).
           05  WS-HAS-NOMINEE          PIC X(01).
               88  NOMINEE-EXISTS                VALUE 'Y'.
       01  WS-I                        PIC 9(01).
       01  WS-FREE-SEQ                 PIC 9(02).
       01  WS-ADULT-CUTOFF             PIC 9(08).
       01  WS-CUST-OPEN-SW             PIC X(01) VALUE 'N'.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN I-O HLD-FILE
           IF HLD-FS-NOFILE
               OPEN OUTPUT HLD-FILE
               CLOSE HLD-FILE
               OPEN I-O HLD-FILE
           END-IF
           IF NOT HLD-FS-OK
               DISPLAY 'RELATIONSHIP FILE OPEN FAILED, FS=' WS-HLD-FS
               GOBACK
           END-IF
           OPEN INPUT CUST-FILE
           IF CUST-FS-OK
               MOVE 'Y' TO WS-CUST-OPEN-SW
           ELSE
               MOVE 'N' TO WS-CUST-OPEN-SW
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL CHOICE-BACK
               PERFORM 1000-SHOW-MENU
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-VIEW
                   WHEN '2' PERFORM 3000-ADD-HOLDER
                   WHEN '3' PERFORM 3500-REMOVE-HOLDER
                   WHEN '4' PERFORM 4000-SET-MODE
                   WHEN '5' PERFORM 5000-SET-NOMINEE
                   WHEN '6' PERFORM 5500-DELETE-NOMINEE
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE HLD-FILE
           IF WS-CUST-OPEN-SW = 'Y'
               CLOSE CUST-FILE
           END-IF
           GOBACK.

       1000-SHOW-MENU.
           DISPLAY ' '
           DISPLAY '--------- HOLDERS AND NOMINEE ---------'
           DISPLAY ' 1. VIEW HOLDERS AND NOMINEE'
           DISPLAY ' 2. ADD HOLDER (FIRST ONE BECOMES PRIMARY)'
           DISPLAY ' 3. REMOVE JOINT HOLDER'
           DISPLAY ' 4. SET OPERATING MODE'
           DISPLAY ' 5. ADD / CHANGE NOMINEE'
           DISPLAY ' 6. CANCEL NOMINATION'
           DISPLAY ' 9. BACK TO MAIN MENU'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           PERFORM 8990-GUARD-END-OF-INPUT.

      *================================================================*
      * VIEW                                                           *
      *================================================================*
       2000-VIEW.
           PERFORM 8000-ACCEPT-ACCOUNT
           IF PP-RETURN-CODE NOT = ZERO
               EXIT PARAGRAPH
           END-IF
           PERFORM 8100-LOAD-RELATIONS
           DISPLAY '----------------------------------------'
           DISPLAY 'ACCOUNT : ' WS-IN-ACCT-NO '  ' PP-HOLDER-NAME
           IF NOT REL-EXISTS
               DISPLAY 'NO HOLDERS LINKED YET.'
               EXIT PARAGRAPH
           END-IF
           EVALUATE WS-CUR-MODE
               WHEN 'S' DISPLAY 'MODE    : SINGLE'
               WHEN 'E' DISPLAY 'MODE    : EITHER OR SURVIVOR'
               WHEN 'F' DISPLAY 'MODE    : FORMER OR SURVIVOR'
               WHEN 'J' DISPLAY 'MODE    : JOINTLY (ALL TO SIGN)'
           END-EVALUATE
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-HOLDER-CNT
               MOVE WS-H-CUST(WS-I) TO CUST-ID
               PERFORM 8200-READ-CUSTOMER
               IF WS-H-ROLE(WS-I) = 'P'
                   DISPLAY 'PRIMARY : ' WS-H-CUST(WS-I) '  '
                           CUST-NAME
               ELSE
                   DISPLAY 'JOINT   : ' WS-H-CUST(WS-I) '  '
                           CUST-NAME
               END-IF
           END-PERFORM
           MOVE WS-IN-ACCT-NO TO HLD-ACCT-NO
           MOVE 90 TO HLD-SEQ
           READ HLD-FILE
               INVALID KEY
                   DISPLAY 'NOMINEE : NONE REGISTERED'
               NOT INVALID KEY
                   DISPLAY 'NOMINEE : ' FUNCTION TRIM(HLD-NOM-NAME)
                           ' (' FUNCTION TRIM(HLD-NOM-RELATION)
                           ', DOB ' HLD-NOM-DOB ')'
                   IF HLD-NOM-GUARDIAN NOT = SPACES
                       DISPLAY 'GUARDIAN: '
                               FUNCTION TRIM(HLD-NOM-GUARDIAN)
                   END-IF
           END-READ
           DISPLAY '----------------------------------------'.

      *================================================================*
      * HOLDERS                                                        *
      *================================================================*
       3000-ADD-HOLDER.
           PERFORM 8000-ACCEPT-ACCOUNT
           IF PP-RETURN-CODE NOT = ZERO
               EXIT PARAGRAPH
           END-IF
           PERFORM 8100-LOAD-RELATIONS
           IF WS-HOLDER-CNT >= 3
               DISPLAY 'AN ACCOUNT CAN HAVE AT MOST 3 HOLDERS.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'CUSTOMER ID: ' WITH NO ADVANCING
           ACCEPT WS-IN-CUST-ID
           MOVE WS-IN-CUST-ID TO CUST-ID
           PERFORM 8200-READ-CUSTOMER
           IF NOT CUST-FS-OK
               DISPLAY 'CUSTOMER NOT FOUND.'
               EXIT PARAGRAPH
           END-IF
           IF NOT KYC-VERIFIED
               DISPLAY 'CUSTOMER KYC IS NOT VERIFIED.'
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-HOLDER-CNT
               IF WS-H-CUST(WS-I) = WS-IN-CUST-ID
                   DISPLAY 'CUSTOMER IS ALREADY A HOLDER.'
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           IF NOT REL-EXISTS
               INITIALIZE HLD-RECORD
               MOVE WS-IN-ACCT-NO TO HLD-ACCT-NO
               MOVE 00 TO HLD-SEQ
               SET MODE-SINGLE TO TRUE
               WRITE HLD-RECORD
           END-IF
           PERFORM 3100-FIND-FREE-SEQ
           INITIALIZE HLD-RECORD
           MOVE WS-IN-ACCT-NO TO HLD-ACCT-NO
           MOVE WS-FREE-SEQ   TO HLD-SEQ
           MOVE WS-IN-CUST-ID TO HLD-CUST-ID
           MOVE WS-CURR-DATE  TO HLD-ADDED-DATE
           IF WS-HOLDER-CNT = ZERO
               SET HLD-PRIMARY TO TRUE
           ELSE
               SET HLD-JOINT TO TRUE
           END-IF
           WRITE HLD-RECORD
               INVALID KEY
                   DISPLAY 'HOLDER WRITE FAILED, FS=' WS-HLD-FS
                   EXIT PARAGRAPH
           END-WRITE
           IF HLD-PRIMARY
               DISPLAY 'PRIMARY HOLDER LINKED. MODE: SINGLE.'
           ELSE
               DISPLAY 'JOINT HOLDER ADDED. SET THE OPERATING MODE '
                       '(OPTION 4).'
           END-IF
           MOVE 'HOLDER-ADD' TO AU-ACTION
           MOVE SPACES TO AU-DETAIL
           STRING 'CUSTOMER ' WS-IN-CUST-ID ' ROLE ' HLD-ROLE
                  DELIMITED BY SIZE INTO AU-DETAIL
           END-STRING
           PERFORM 9000-AUDIT.

      *    PRIMARY IS ALWAYS 01; JOINT HOLDERS TAKE 02 OR 03
       3100-FIND-FREE-SEQ.
           IF WS-HOLDER-CNT = ZERO
               MOVE 01 TO WS-FREE-SEQ
               EXIT PARAGRAPH
           END-IF
           MOVE 02 TO WS-FREE-SEQ
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-HOLDER-CNT
               IF WS-H-SEQ(WS-I) = WS-FREE-SEQ
                   MOVE 03 TO WS-FREE-SEQ
               END-IF
           END-PERFORM.

       3500-REMOVE-HOLDER.
           PERFORM 8000-ACCEPT-ACCOUNT
           IF PP-RETURN-CODE NOT = ZERO
               EXIT PARAGRAPH
           END-IF
           PERFORM 8100-LOAD-RELATIONS
           DISPLAY 'CUSTOMER ID TO REMOVE: ' WITH NO ADVANCING
           ACCEPT WS-IN-CUST-ID
           MOVE ZERO TO WS-FREE-SEQ
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-HOLDER-CNT
               IF WS-H-CUST(WS-I) = WS-IN-CUST-ID
                   MOVE WS-H-SEQ(WS-I) TO WS-FREE-SEQ
               END-IF
           END-PERFORM
           EVALUATE TRUE
               WHEN WS-FREE-SEQ = ZERO
                   DISPLAY 'CUSTOMER IS NOT A HOLDER OF THIS ACCOUNT.'
                   EXIT PARAGRAPH
               WHEN WS-FREE-SEQ = 01
                   DISPLAY 'THE PRIMARY HOLDER CANNOT BE REMOVED.'
                   EXIT PARAGRAPH
           END-EVALUATE
           DISPLAY 'ALL HOLDERS HAVE SIGNED THE REQUEST (Y/N): '
                   WITH NO ADVANCING
           ACCEPT WS-CONFIRM
           IF NOT CONFIRM-YES
               DISPLAY 'REMOVAL NEEDS THE CONSENT OF ALL HOLDERS.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-ACCT-NO TO HLD-ACCT-NO
           MOVE WS-FREE-SEQ   TO HLD-SEQ
           DELETE HLD-FILE
               INVALID KEY
                   DISPLAY 'REMOVE FAILED, FS=' WS-HLD-FS
                   EXIT PARAGRAPH
           END-DELETE
           IF WS-HOLDER-CNT - 1 = 1
               MOVE WS-IN-ACCT-NO TO HLD-ACCT-NO
               MOVE 00 TO HLD-SEQ
               READ HLD-FILE
               SET MODE-SINGLE TO TRUE
               REWRITE HLD-RECORD
               DISPLAY 'JOINT HOLDER REMOVED. MODE RESET TO SINGLE.'
           ELSE
               DISPLAY 'JOINT HOLDER REMOVED.'
           END-IF
           MOVE 'HOLDER-DEL' TO AU-ACTION
           MOVE SPACES TO AU-DETAIL
           STRING 'CUSTOMER ' WS-IN-CUST-ID
                  DELIMITED BY SIZE INTO AU-DETAIL
           END-STRING
           PERFORM 9000-AUDIT.

       4000-SET-MODE.
           PERFORM 8000-ACCEPT-ACCOUNT
           IF PP-RETURN-CODE NOT = ZERO
               EXIT PARAGRAPH
           END-IF
           PERFORM 8100-LOAD-RELATIONS
           IF NOT REL-EXISTS
               DISPLAY 'LINK THE PRIMARY HOLDER FIRST.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'MODE (S/E/F/J): ' WITH NO ADVANCING
           ACCEPT WS-IN-MODE
           MOVE FUNCTION UPPER-CASE(WS-IN-MODE) TO WS-IN-MODE
           MOVE WS-IN-ACCT-NO TO HLD-ACCT-NO
           MOVE 00 TO HLD-SEQ
           READ HLD-FILE
           MOVE WS-IN-MODE TO HLD-MODE
           EVALUATE TRUE
               WHEN NOT MODE-VALID
                   DISPLAY 'MODE MUST BE S, E, F OR J.'
                   EXIT PARAGRAPH
               WHEN MODE-SINGLE AND WS-HOLDER-CNT > 1
                   DISPLAY 'SINGLE MODE NEEDS EXACTLY ONE HOLDER.'
                   EXIT PARAGRAPH
               WHEN NOT MODE-SINGLE AND WS-HOLDER-CNT < 2
                   DISPLAY 'JOINT MODES NEED AT LEAST TWO HOLDERS.'
                   EXIT PARAGRAPH
           END-EVALUATE
           REWRITE HLD-RECORD
           DISPLAY 'OPERATING MODE SET TO ' HLD-MODE '.'
           MOVE 'MODE-SET' TO AU-ACTION
           MOVE SPACES TO AU-DETAIL
           STRING 'MODE ' HLD-MODE DELIMITED BY SIZE INTO AU-DETAIL
           END-STRING
           PERFORM 9000-AUDIT.

      *================================================================*
      * NOMINEE                                                        *
      *================================================================*
       5000-SET-NOMINEE.
           PERFORM 8000-ACCEPT-ACCOUNT
           IF PP-RETURN-CODE NOT = ZERO
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'NOMINEE NAME          : ' WITH NO ADVANCING
           ACCEPT WS-IN-NOM-NAME
           DISPLAY 'RELATIONSHIP          : ' WITH NO ADVANCING
           ACCEPT WS-IN-NOM-REL
           DISPLAY 'DATE OF BIRTH YYYYMMDD: ' WITH NO ADVANCING
           ACCEPT WS-IN-NOM-DOB
           IF WS-IN-NOM-NAME = SPACES OR WS-IN-NOM-REL = SPACES
               DISPLAY 'NAME AND RELATIONSHIP ARE REQUIRED.'
               EXIT PARAGRAPH
           END-IF
           IF WS-IN-NOM-DOB IS NUMERIC
               MOVE WS-IN-NOM-DOB TO DU-DATE-1
               SET DU-VALIDATE TO TRUE
               CALL 'DATEUTIL' USING DATE-PARMS
           END-IF
           IF WS-IN-NOM-DOB NOT NUMERIC OR NOT DU-OK
               DISPLAY 'DATE OF BIRTH IS NOT VALID.'
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           IF WS-IN-NOM-DOB > WS-CURR-DATE
               DISPLAY 'DATE OF BIRTH IS IN THE FUTURE.'
               EXIT PARAGRAPH
           END-IF
      *    MINOR = BORN AFTER (TODAY - 18 YEARS)
           SET DU-ADD-MONTHS TO TRUE
           MOVE WS-CURR-DATE TO DU-DATE-1
           MOVE -216 TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO WS-ADULT-CUTOFF
           MOVE SPACES TO WS-IN-GUARDIAN
           IF WS-IN-NOM-DOB > WS-ADULT-CUTOFF
               DISPLAY 'NOMINEE IS A MINOR - GUARDIAN NAME: '
                       WITH NO ADVANCING
               ACCEPT WS-IN-GUARDIAN
               IF WS-IN-GUARDIAN = SPACES
                   DISPLAY 'A GUARDIAN IS REQUIRED FOR A MINOR NOMINEE.'
                   EXIT PARAGRAPH
               END-IF
           END-IF

           MOVE WS-IN-ACCT-NO TO HLD-ACCT-NO
           MOVE 90 TO HLD-SEQ
           READ HLD-FILE
               INVALID KEY MOVE 'N' TO WS-HAS-NOMINEE
               NOT INVALID KEY MOVE 'Y' TO WS-HAS-NOMINEE
           END-READ
           INITIALIZE HLD-DATA
           MOVE FUNCTION UPPER-CASE(WS-IN-NOM-NAME) TO HLD-NOM-NAME
           MOVE FUNCTION UPPER-CASE(WS-IN-NOM-REL)  TO HLD-NOM-RELATION
           MOVE WS-IN-NOM-DOB                       TO HLD-NOM-DOB
           MOVE FUNCTION UPPER-CASE(WS-IN-GUARDIAN) TO HLD-NOM-GUARDIAN
           MOVE WS-CURR-DATE TO HLD-ADDED-DATE
           IF NOMINEE-EXISTS
               REWRITE HLD-RECORD
               DISPLAY 'NOMINEE UPDATED.'
               MOVE 'NOMINEE-CHG' TO AU-ACTION
           ELSE
               WRITE HLD-RECORD
               DISPLAY 'NOMINEE REGISTERED.'
               MOVE 'NOMINEE-ADD' TO AU-ACTION
           END-IF
           MOVE HLD-NOM-NAME TO AU-DETAIL
           PERFORM 9000-AUDIT.

       5500-DELETE-NOMINEE.
           PERFORM 8000-ACCEPT-ACCOUNT
           IF PP-RETURN-CODE NOT = ZERO
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-ACCT-NO TO HLD-ACCT-NO
           MOVE 90 TO HLD-SEQ
           DELETE HLD-FILE
               INVALID KEY
                   DISPLAY 'NO NOMINEE REGISTERED.'
                   EXIT PARAGRAPH
           END-DELETE
           DISPLAY 'NOMINATION CANCELLED.'
           MOVE 'NOMINEE-DEL' TO AU-ACTION
           MOVE SPACES TO AU-DETAIL
           PERFORM 9000-AUDIT.

      *================================================================*
      * COMMON                                                         *
      *================================================================*
       8000-ACCEPT-ACCOUNT.
           DISPLAY 'ACCOUNT NO: ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF NOT PP-OK
               DISPLAY 'ACCOUNT NOT FOUND.'
           END-IF.

      *    READS SEQ 00-03 FOR THE ACCOUNT INTO WS-REL
       8100-LOAD-RELATIONS.
           INITIALIZE WS-REL
           MOVE 'N' TO WS-HAS-HEADER WS-HAS-NOMINEE
           MOVE WS-IN-ACCT-NO TO HLD-ACCT-NO
           MOVE 00 TO HLD-SEQ
           START HLD-FILE KEY IS NOT LESS THAN HLD-KEY
               INVALID KEY EXIT PARAGRAPH
           END-START
           PERFORM FOREVER
               READ HLD-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF HLD-ACCT-NO NOT = WS-IN-ACCT-NO
                   EXIT PERFORM
               END-IF
               EVALUATE TRUE
                   WHEN HLD-IS-HEADER
                       MOVE 'Y' TO WS-HAS-HEADER
                       MOVE HLD-MODE TO WS-CUR-MODE
                   WHEN HLD-IS-HOLDER
                       ADD 1 TO WS-HOLDER-CNT
                       MOVE HLD-SEQ     TO WS-H-SEQ(WS-HOLDER-CNT)
                       MOVE HLD-CUST-ID TO WS-H-CUST(WS-HOLDER-CNT)
                       MOVE HLD-ROLE    TO WS-H-ROLE(WS-HOLDER-CNT)
                   WHEN HLD-IS-NOMINEE
                       MOVE 'Y' TO WS-HAS-NOMINEE
               END-EVALUATE
           END-PERFORM.

       8200-READ-CUSTOMER.
           IF WS-CUST-OPEN-SW NOT = 'Y'
               MOVE '35' TO WS-CUST-FS
               MOVE '(NO CUSTOMER FILE)' TO CUST-NAME
               EXIT PARAGRAPH
           END-IF
           READ CUST-FILE
               INVALID KEY
                   MOVE '(CUSTOMER RECORD MISSING)' TO CUST-NAME
           END-READ.

       9000-AUDIT.
           MOVE 'HLDMNT'      TO AU-PROGRAM
           MOVE WS-IN-ACCT-NO TO AU-KEY
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
