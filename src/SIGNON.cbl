       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIGNON.
      *================================================================*
      * PROGRAM  : SIGNON                                              *
      * PURPOSE  : USER SIGN-ON FOR THE ONLINE SYSTEM.                 *
      *            - FIRST RUN (NO USERS): CREATES USER 'ADMIN' AND    *
      *              ASKS FOR ITS PASSWORD.                            *
      *            - 3 ATTEMPTS PER SESSION; 5 CONSECUTIVE FAILURES ON *
      *              ONE USER ID LOCK THAT USER UNTIL AN ADMIN RESETS. *
      *            - PASSWORDS EXPIRE AFTER 90 DAYS; A RESET OR EXPIRED*
      *              PASSWORD MUST BE CHANGED BEFORE CONTINUING.       *
      *            - THE SAME MESSAGE IS SHOWN FOR AN UNKNOWN USER AND *
      *              A WRONG PASSWORD, SO USER IDS CANNOT BE PROBED.   *
      * CALL     : CALL 'SIGNON' USING LS-SIGNON-RC                    *
      *            RC 00 = SIGNED ON (SESSION FILLED), 90 = FAILED     *
      * FILES    : USERMAST (INDEXED, I-O)                             *
      * CALLS    : SECUTIL, AUDITLOG, DATEUTIL                         *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO 'data/USERMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS USR-ID
               FILE STATUS IS WS-USER-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
           COPY USERREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY SESSION.
           COPY SECPARM.
           COPY AUDPARM.
           COPY DATEPARM.

       01  WS-USER-FS                  PIC X(02).
           88  USER-FS-OK                        VALUE '00'.
           88  USER-FS-NOFILE                    VALUE '35'.

       01  WS-IN-USER-ID               PIC X(08).
       01  WS-IN-PASSWORD              PIC X(20).
       01  WS-IN-NEW-PWD               PIC X(20).
       01  WS-IN-NEW-PWD-2             PIC X(20).
       01  WS-ATTEMPT                  PIC 9(01).
       01  WS-MAX-ATTEMPTS             PIC 9(01) VALUE 3.
       01  WS-LOCK-AFTER               PIC 9(01) VALUE 5.
       01  WS-PWD-MAX-AGE-DAYS         PIC 9(03) VALUE 90.
       01  WS-PWD-OK-SW                PIC X(01).
           88  NEW-PWD-OK                        VALUE 'Y'.
       01  WS-PWD-TRIES                PIC 9(02) VALUE ZERO.
       01  WS-FILE-EMPTY-SW            PIC X(01).
           88  USER-FILE-EMPTY                   VALUE 'Y'.

       LINKAGE SECTION.
       01  LS-SIGNON-RC                PIC 9(02).

       PROCEDURE DIVISION USING LS-SIGNON-RC.
       0000-MAIN.
           MOVE 90 TO LS-SIGNON-RC
           MOVE SPACES TO SESSION-INFO
           PERFORM 1000-OPEN-USER-FILE
           IF NOT USER-FS-OK
               GOBACK
           END-IF
           IF USER-FILE-EMPTY
               PERFORM 2000-BOOTSTRAP-ADMIN
           END-IF
           MOVE ZERO TO WS-ATTEMPT
           PERFORM 3000-TRY-SIGNON
               UNTIL SES-IS-SIGNED-ON
                  OR WS-ATTEMPT >= WS-MAX-ATTEMPTS
           CLOSE USER-FILE
           IF SES-IS-SIGNED-ON
               MOVE ZERO TO LS-SIGNON-RC
           ELSE
               DISPLAY 'TOO MANY FAILED ATTEMPTS. GOODBYE.'
           END-IF
           GOBACK.

       1000-OPEN-USER-FILE.
           MOVE 'N' TO WS-FILE-EMPTY-SW
           OPEN I-O USER-FILE
           IF USER-FS-NOFILE
               OPEN OUTPUT USER-FILE
               CLOSE USER-FILE
               OPEN I-O USER-FILE
           END-IF
           IF NOT USER-FS-OK
               DISPLAY 'USER MASTER OPEN FAILED, FS=' WS-USER-FS
               EXIT PARAGRAPH
           END-IF
           MOVE LOW-VALUES TO USR-ID
           START USER-FILE KEY IS NOT LESS THAN USR-ID
               INVALID KEY MOVE 'Y' TO WS-FILE-EMPTY-SW
           END-START
      *    START SETS THE FILE STATUS; THE OPEN ITSELF WORKED
           MOVE '00' TO WS-USER-FS.

      *----------------------------------------------------------------*
      * FIRST RUN - CREATE ADMIN                                       *
      *----------------------------------------------------------------*
       2000-BOOTSTRAP-ADMIN.
           DISPLAY ' '
           DISPLAY '*** FIRST RUN: NO USERS DEFINED ***'
           DISPLAY 'CREATING ADMINISTRATOR USER ID: ADMIN'
           MOVE 'N' TO WS-PWD-OK-SW
           MOVE ZERO TO WS-PWD-TRIES
           PERFORM 8000-ACCEPT-NEW-PASSWORD UNTIL NEW-PWD-OK
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           INITIALIZE USER-RECORD
           MOVE 'ADMIN'                 TO USR-ID
           MOVE 'SYSTEM ADMINISTRATOR'  TO USR-NAME
           SET USR-ADMIN                TO TRUE
           MOVE 'HO0001'                TO USR-BRANCH
           SET USR-ACTIVE               TO TRUE
           MOVE 'N'                     TO USR-MUST-CHANGE
           MOVE WS-CURR-DATE            TO USR-PWD-CHANGED-DATE
                                           USR-CREATED-DATE
           MOVE 'SYSTEM'                TO USR-CREATED-BY
           PERFORM 8100-HASH-NEW-PASSWORD
           WRITE USER-RECORD
               INVALID KEY
                   DISPLAY 'ADMIN CREATE FAILED, FS=' WS-USER-FS
           END-WRITE
           MOVE 'SIGNON'   TO AU-PROGRAM
           MOVE 'BOOTSTRAP' TO AU-ACTION
           MOVE 'ADMIN'    TO AU-KEY
           MOVE 'FIRST-RUN ADMINISTRATOR CREATED' TO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS
           DISPLAY 'ADMIN CREATED. PLEASE SIGN ON.'.

      *----------------------------------------------------------------*
      * ONE SIGN-ON ATTEMPT                                            *
      *----------------------------------------------------------------*
       3000-TRY-SIGNON.
           ADD 1 TO WS-ATTEMPT
           DISPLAY ' '
           DISPLAY 'USER ID  : ' WITH NO ADVANCING
           ACCEPT WS-IN-USER-ID
           DISPLAY 'PASSWORD : ' WITH NO ADVANCING
           ACCEPT WS-IN-PASSWORD
           MOVE FUNCTION UPPER-CASE(WS-IN-USER-ID) TO WS-IN-USER-ID
           MOVE WS-IN-USER-ID TO USR-ID AU-KEY
           MOVE 'SIGNON' TO AU-PROGRAM

           READ USER-FILE
               INVALID KEY
                   DISPLAY 'INVALID USER ID OR PASSWORD.'
                   MOVE 'LOGIN-FAIL' TO AU-ACTION
                   MOVE 'UNKNOWN USER ID' TO AU-DETAIL
                   CALL 'AUDITLOG' USING AUD-PARMS
                   EXIT PARAGRAPH
           END-READ

           EVALUATE TRUE
               WHEN USR-DISABLED
                   DISPLAY 'USER ID IS DISABLED. CONTACT ADMIN.'
                   MOVE 'LOGIN-FAIL' TO AU-ACTION
                   MOVE 'DISABLED USER' TO AU-DETAIL
                   CALL 'AUDITLOG' USING AUD-PARMS
                   EXIT PARAGRAPH
               WHEN USR-LOCKED
                   DISPLAY 'USER ID IS LOCKED. CONTACT ADMIN.'
                   MOVE 'LOGIN-FAIL' TO AU-ACTION
                   MOVE 'LOCKED USER' TO AU-DETAIL
                   CALL 'AUDITLOG' USING AUD-PARMS
                   EXIT PARAGRAPH
           END-EVALUATE

           MOVE 'PH'           TO SC-FUNCTION
           MOVE WS-IN-PASSWORD TO SC-PASSWORD
           MOVE USR-ID         TO SC-SALT
           CALL 'SECUTIL' USING SEC-PARMS
           IF SC-HASH NOT = USR-PWD-HASH
               PERFORM 3100-RECORD-FAILURE
               EXIT PARAGRAPH
           END-IF
           PERFORM 3200-RECORD-SUCCESS.

       3100-RECORD-FAILURE.
           DISPLAY 'INVALID USER ID OR PASSWORD.'
           ADD 1 TO USR-FAILED-CNT
           MOVE 'LOGIN-FAIL' TO AU-ACTION
           MOVE 'WRONG PASSWORD' TO AU-DETAIL
           IF USR-FAILED-CNT >= WS-LOCK-AFTER
               SET USR-LOCKED TO TRUE
               MOVE 'LOCKED' TO AU-ACTION
               MOVE 'LOCKED AFTER REPEATED FAILURES' TO AU-DETAIL
           END-IF
           REWRITE USER-RECORD
           CALL 'AUDITLOG' USING AUD-PARMS.

       3200-RECORD-SUCCESS.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE ZERO TO USR-FAILED-CNT
           MOVE WS-CURR-DATE TO USR-LAST-LOGIN-DATE
           MOVE WS-CURR-TIME TO USR-LAST-LOGIN-TIME

           MOVE USR-ID     TO SES-USER-ID
           MOVE USR-NAME   TO SES-USER-NAME
           MOVE USR-ROLE   TO SES-ROLE
           MOVE USR-BRANCH TO SES-BRANCH
           MOVE 'Y'        TO SES-SIGNED-ON

           PERFORM 3300-CHECK-PASSWORD-AGE
           IF USR-PWD-MUST-CHANGE
               DISPLAY 'YOU MUST SET A NEW PASSWORD NOW.'
               MOVE 'N' TO WS-PWD-OK-SW
               MOVE ZERO TO WS-PWD-TRIES
               PERFORM 8000-ACCEPT-NEW-PASSWORD UNTIL NEW-PWD-OK
               PERFORM 8100-HASH-NEW-PASSWORD
               MOVE 'N' TO USR-MUST-CHANGE
               MOVE WS-CURR-DATE TO USR-PWD-CHANGED-DATE
               MOVE 'PWD-CHANGE' TO AU-ACTION
               MOVE 'FORCED CHANGE AT SIGN-ON' TO AU-DETAIL
               CALL 'AUDITLOG' USING AUD-PARMS
           END-IF
           REWRITE USER-RECORD
               INVALID KEY
                   DISPLAY 'WARNING: USER UPDATE FAILED FS='
                           WS-USER-FS
           END-REWRITE
           MOVE 'LOGIN' TO AU-ACTION
           MOVE 'SIGNED ON' TO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS
           DISPLAY 'WELCOME, ' FUNCTION TRIM(USR-NAME)
                   '  (ROLE ' USR-ROLE ', BRANCH ' USR-BRANCH ')'.

       3300-CHECK-PASSWORD-AGE.
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE USR-PWD-CHANGED-DATE TO DU-DATE-1
           MOVE WS-CURR-DATE         TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           IF DU-OK AND DU-RESULT-DAYS > WS-PWD-MAX-AGE-DAYS
               DISPLAY 'YOUR PASSWORD HAS EXPIRED.'
               MOVE 'Y' TO USR-MUST-CHANGE
           END-IF.

      *----------------------------------------------------------------*
      * NEW PASSWORD - ENTER TWICE, MUST MEET POLICY                   *
      *----------------------------------------------------------------*
       8000-ACCEPT-NEW-PASSWORD.
           MOVE 'N' TO WS-PWD-OK-SW
      *    WITHOUT A LIMIT A CLOSED INPUT STREAM LOOPS HERE FOREVER
           ADD 1 TO WS-PWD-TRIES
           IF WS-PWD-TRIES > 5
               DISPLAY 'TOO MANY ATTEMPTS - SIGN-ON ABANDONED.'
               CLOSE USER-FILE
               MOVE 90 TO LS-SIGNON-RC
               GOBACK
           END-IF
           DISPLAY 'NEW PASSWORD     : ' WITH NO ADVANCING
           ACCEPT WS-IN-NEW-PWD
           DISPLAY 'CONFIRM PASSWORD : ' WITH NO ADVANCING
           ACCEPT WS-IN-NEW-PWD-2
           IF WS-IN-NEW-PWD NOT = WS-IN-NEW-PWD-2
               DISPLAY 'PASSWORDS DO NOT MATCH.'
               EXIT PARAGRAPH
           END-IF
           MOVE 'PV'          TO SC-FUNCTION
           MOVE WS-IN-NEW-PWD TO SC-PASSWORD
           CALL 'SECUTIL' USING SEC-PARMS
           EVALUATE SC-RC
               WHEN 00 MOVE 'Y' TO WS-PWD-OK-SW
               WHEN 10 DISPLAY 'PASSWORD MUST BE AT LEAST 8 CHARACTERS.'
               WHEN 20 DISPLAY 'PASSWORD MUST CONTAIN A LETTER.'
               WHEN 30 DISPLAY 'PASSWORD MUST CONTAIN A DIGIT.'
               WHEN OTHER DISPLAY 'PASSWORD MUST NOT CONTAIN SPACES.'
           END-EVALUATE.

       8100-HASH-NEW-PASSWORD.
           MOVE 'PH'          TO SC-FUNCTION
           MOVE WS-IN-NEW-PWD TO SC-PASSWORD
           MOVE USR-ID        TO SC-SALT
           CALL 'SECUTIL' USING SEC-PARMS
           MOVE SC-HASH       TO USR-PWD-HASH.
