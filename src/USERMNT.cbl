       IDENTIFICATION DIVISION.
       PROGRAM-ID. USERMNT.
      *================================================================*
      * PROGRAM  : USERMNT                                             *
      * PURPOSE  : USER ADMINISTRATION AND PASSWORD CHANGE.            *
      *            MODE 'A' (ADMIN ONLY): ADD USER, RESET PASSWORD,    *
      *              DISABLE / ENABLE, LIST USERS, VIEW AUDIT TRAIL.   *
      *            MODE 'P' (ANY USER): CHANGE OWN PASSWORD.           *
      *            SAFETY RULES: AN ADMIN CANNOT DISABLE THEIR OWN ID, *
      *            AND THE LAST ACTIVE ADMIN CANNOT BE DISABLED.       *
      * CALL     : CALL 'USERMNT' USING LS-MODE                        *
      * FILES    : USERMAST (INDEXED, I-O), AUDITLOG (INPUT)           *
      * CALLS    : SECUTIL, AUDITLOG                                   *
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
           SELECT AUD-FILE ASSIGN TO 'data/AUDITLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-AUD-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
           COPY USERREC.
       FD  AUD-FILE.
           COPY AUDREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY SESSION.
           COPY SECPARM.
           COPY AUDPARM.

       01  WS-USER-FS                  PIC X(02).
           88  USER-FS-OK                        VALUE '00'.
       01  WS-AUD-FS                   PIC X(02).

       01  WS-CHOICE                   PIC X(01).
           88  CHOICE-BACK                       VALUE '9'.
       01  WS-CONFIRM                  PIC X(01).
           88  CONFIRM-YES                       VALUES 'Y' 'y'.

       01  WS-IN-USER-ID               PIC X(08).
       01  WS-IN-NAME                  PIC X(30).
       01  WS-IN-ROLE                  PIC X(01).
       01  WS-IN-BRANCH                PIC X(06).
       01  WS-IN-OLD-PWD               PIC X(20).
       01  WS-IN-NEW-PWD               PIC X(20).
       01  WS-IN-NEW-PWD-2             PIC X(20).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-PWD-OK-SW                PIC X(01).
           88  NEW-PWD-OK                        VALUE 'Y'.
       01  WS-ACTIVE-ADMINS            PIC 9(03).
       01  WS-LIST-CNT                 PIC 9(05).
       01  WS-TARGET-ID                PIC X(08).

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       LINKAGE SECTION.
       01  LS-MODE                     PIC X(01).

       PROCEDURE DIVISION USING LS-MODE.
       0000-MAIN.
           IF NOT SES-IS-SIGNED-ON
               DISPLAY 'NOT SIGNED ON.'
               GOBACK
           END-IF
           OPEN I-O USER-FILE
           IF NOT USER-FS-OK
               DISPLAY 'USER MASTER OPEN FAILED, FS=' WS-USER-FS
               GOBACK
           END-IF
           EVALUATE LS-MODE
               WHEN 'P'
                   PERFORM 7000-CHANGE-OWN-PASSWORD
               WHEN 'A'
                   IF SES-ADMIN
                       PERFORM 1000-ADMIN-MENU
                   ELSE
                       DISPLAY 'USER ADMINISTRATION IS FOR ADMINS ONLY.'
                   END-IF
           END-EVALUATE
           CLOSE USER-FILE
           GOBACK.

       1000-ADMIN-MENU.
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL CHOICE-BACK
               DISPLAY ' '
               DISPLAY '----------- USER ADMINISTRATION -----------'
               DISPLAY ' 1. ADD USER'
               DISPLAY ' 2. RESET PASSWORD / UNLOCK'
               DISPLAY ' 3. DISABLE / ENABLE USER'
               DISPLAY ' 4. LIST USERS'
               DISPLAY ' 5. VIEW AUDIT TRAIL'
               DISPLAY ' 9. BACK TO MAIN MENU'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               PERFORM 8990-GUARD-END-OF-INPUT
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 2000-ADD-USER
                   WHEN '2' PERFORM 3000-RESET-PASSWORD
                   WHEN '3' PERFORM 4000-TOGGLE-USER
                   WHEN '4' PERFORM 5000-LIST-USERS
                   WHEN '5' PERFORM 6000-VIEW-AUDIT
                   WHEN '9' CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM.

      *----------------------------------------------------------------*
      * ADD USER - ADMIN SETS A TEMPORARY PASSWORD; THE USER MUST      *
      * CHANGE IT AT FIRST SIGN-ON.                                    *
      *----------------------------------------------------------------*
       2000-ADD-USER.
           DISPLAY 'NEW USER ID (MAX 8) : ' WITH NO ADVANCING
           ACCEPT WS-IN-USER-ID
           MOVE FUNCTION UPPER-CASE(WS-IN-USER-ID) TO WS-IN-USER-ID
           IF WS-IN-USER-ID = SPACES OR WS-IN-USER-ID(1:1) = SPACE
               DISPLAY 'USER ID IS REQUIRED.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-USER-ID TO USR-ID
           READ USER-FILE
               NOT INVALID KEY
                   DISPLAY 'USER ID ALREADY EXISTS.'
                   EXIT PARAGRAPH
           END-READ
           DISPLAY 'FULL NAME           : ' WITH NO ADVANCING
           ACCEPT WS-IN-NAME
           DISPLAY 'ROLE (T/M/A)        : ' WITH NO ADVANCING
           ACCEPT WS-IN-ROLE
           DISPLAY 'BRANCH CODE         : ' WITH NO ADVANCING
           ACCEPT WS-IN-BRANCH
           MOVE FUNCTION UPPER-CASE(WS-IN-ROLE) TO WS-IN-ROLE
           INITIALIZE USER-RECORD
           MOVE WS-IN-USER-ID TO USR-ID
           MOVE WS-IN-ROLE    TO USR-ROLE
           IF NOT USR-ROLE-VALID
               DISPLAY 'ROLE MUST BE T, M OR A.'
               EXIT PARAGRAPH
           END-IF
           IF WS-IN-NAME = SPACES
               DISPLAY 'NAME IS REQUIRED.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'TEMPORARY PASSWORD:'
           PERFORM 8000-ACCEPT-NEW-PASSWORD
           IF NOT NEW-PWD-OK
               EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE FUNCTION UPPER-CASE(WS-IN-NAME) TO USR-NAME
           MOVE FUNCTION UPPER-CASE(WS-IN-BRANCH) TO USR-BRANCH
           SET USR-ACTIVE     TO TRUE
           MOVE 'Y'           TO USR-MUST-CHANGE
           MOVE WS-CURR-DATE  TO USR-PWD-CHANGED-DATE USR-CREATED-DATE
           MOVE SES-USER-ID   TO USR-CREATED-BY
           PERFORM 8100-HASH-NEW-PASSWORD
           WRITE USER-RECORD
               INVALID KEY
                   DISPLAY 'USER WRITE FAILED, FS=' WS-USER-FS
                   EXIT PARAGRAPH
           END-WRITE
           MOVE 'USER-ADD' TO AU-ACTION
           STRING 'ROLE ' USR-ROLE ' BRANCH ' USR-BRANCH
                  DELIMITED BY SIZE INTO AU-DETAIL
           END-STRING
           PERFORM 9000-AUDIT
           DISPLAY 'USER ' USR-ID ' CREATED. PASSWORD MUST BE CHANGED'
                   ' AT FIRST SIGN-ON.'.

       3000-RESET-PASSWORD.
           PERFORM 8200-READ-TARGET-USER
           IF NOT USER-FS-OK
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'TEMPORARY PASSWORD:'
           PERFORM 8000-ACCEPT-NEW-PASSWORD
           IF NOT NEW-PWD-OK
               EXIT PARAGRAPH
           END-IF
           PERFORM 8100-HASH-NEW-PASSWORD
           MOVE 'Y'  TO USR-MUST-CHANGE
           MOVE ZERO TO USR-FAILED-CNT
           IF USR-LOCKED
               SET USR-ACTIVE TO TRUE
           END-IF
           REWRITE USER-RECORD
           MOVE 'PWD-RESET' TO AU-ACTION
           MOVE 'PASSWORD RESET BY ADMIN, UNLOCKED' TO AU-DETAIL
           PERFORM 9000-AUDIT
           DISPLAY 'PASSWORD RESET. USER MUST CHANGE IT AT SIGN-ON.'.

       4000-TOGGLE-USER.
           PERFORM 8200-READ-TARGET-USER
           IF NOT USER-FS-OK
               EXIT PARAGRAPH
           END-IF
           IF USR-DISABLED
               SET USR-ACTIVE TO TRUE
               MOVE ZERO TO USR-FAILED-CNT
               MOVE 'USER-ENABLE' TO AU-ACTION
           ELSE
               IF USR-ID = SES-USER-ID
                   DISPLAY 'YOU CANNOT DISABLE YOUR OWN USER ID.'
                   EXIT PARAGRAPH
               END-IF
               IF USR-ADMIN
                   MOVE USR-ID TO WS-TARGET-ID
                   PERFORM 4100-COUNT-ACTIVE-ADMINS
                   MOVE WS-TARGET-ID TO USR-ID
                   READ USER-FILE
                   IF WS-ACTIVE-ADMINS <= 1
                       DISPLAY 'CANNOT DISABLE THE LAST ACTIVE ADMIN.'
                       EXIT PARAGRAPH
                   END-IF
               END-IF
               DISPLAY 'DISABLE USER ' USR-ID ' (Y/N): '
                       WITH NO ADVANCING
               ACCEPT WS-CONFIRM
               IF NOT CONFIRM-YES
                   EXIT PARAGRAPH
               END-IF
               SET USR-DISABLED TO TRUE
               MOVE 'USER-DISABLE' TO AU-ACTION
           END-IF
           REWRITE USER-RECORD
           MOVE SPACES TO AU-DETAIL
           PERFORM 9000-AUDIT
           DISPLAY 'USER ' USR-ID ' IS NOW STATUS ' USR-STATUS.

       4100-COUNT-ACTIVE-ADMINS.
           MOVE ZERO TO WS-ACTIVE-ADMINS
           MOVE LOW-VALUES TO USR-ID
           START USER-FILE KEY IS NOT LESS THAN USR-ID
               INVALID KEY EXIT PARAGRAPH
           END-START
           PERFORM FOREVER
               READ USER-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               IF USR-ADMIN AND NOT USR-DISABLED
                   ADD 1 TO WS-ACTIVE-ADMINS
               END-IF
           END-PERFORM.

       5000-LIST-USERS.
           MOVE ZERO TO WS-LIST-CNT
           MOVE LOW-VALUES TO USR-ID
           START USER-FILE KEY IS NOT LESS THAN USR-ID
               INVALID KEY
                   DISPLAY 'NO USERS.'
                   EXIT PARAGRAPH
           END-START
           DISPLAY 'USER ID   NAME                           R ST'
                   ' BRANCH  LAST LOGIN'
           PERFORM FOREVER
               READ USER-FILE NEXT RECORD
                   AT END EXIT PERFORM
               END-READ
               ADD 1 TO WS-LIST-CNT
               DISPLAY USR-ID '  ' USR-NAME ' ' USR-ROLE ' '
                       USR-STATUS '  ' USR-BRANCH '  '
                       USR-LAST-LOGIN-DATE
           END-PERFORM
           DISPLAY WS-LIST-CNT ' USER(S).'.

      *----------------------------------------------------------------*
      * AUDIT TRAIL FOR ONE DATE, OPTIONALLY ONE USER                  *
      *----------------------------------------------------------------*
       6000-VIEW-AUDIT.
           DISPLAY 'DATE YYYYMMDD (BLANK = TODAY): ' WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           IF WS-IN-DATE = SPACES
               MOVE FUNCTION CURRENT-DATE(1:8) TO WS-IN-DATE
           END-IF
           DISPLAY 'USER ID (BLANK = ALL)        : ' WITH NO ADVANCING
           ACCEPT WS-IN-USER-ID
           MOVE FUNCTION UPPER-CASE(WS-IN-USER-ID) TO WS-IN-USER-ID
           OPEN INPUT AUD-FILE
           IF WS-AUD-FS NOT = '00'
               DISPLAY 'NO AUDIT TRAIL YET.'
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO WS-LIST-CNT
           DISPLAY 'TIME    USER     PROGRAM  ACTION       KEY'
                   '              DETAIL'
           PERFORM FOREVER
               READ AUD-FILE
                   AT END EXIT PERFORM
               END-READ
               IF AUD-DATE = WS-IN-DATE
                  AND (WS-IN-USER-ID = SPACES
                       OR AUD-USER-ID = WS-IN-USER-ID)
                   ADD 1 TO WS-LIST-CNT
                   DISPLAY AUD-TIME '  ' AUD-USER-ID ' ' AUD-PROGRAM
                           ' ' AUD-ACTION ' ' AUD-KEY ' '
                           FUNCTION TRIM(AUD-DETAIL)
               END-IF
           END-PERFORM
           CLOSE AUD-FILE
           DISPLAY WS-LIST-CNT ' ENTRY(IES).'.

      *----------------------------------------------------------------*
      * CHANGE OWN PASSWORD                                            *
      *----------------------------------------------------------------*
       7000-CHANGE-OWN-PASSWORD.
           MOVE SES-USER-ID TO USR-ID
           READ USER-FILE
               INVALID KEY
                   DISPLAY 'YOUR USER RECORD WAS NOT FOUND.'
                   EXIT PARAGRAPH
           END-READ
           DISPLAY 'CURRENT PASSWORD : ' WITH NO ADVANCING
           ACCEPT WS-IN-OLD-PWD
           MOVE 'PH'          TO SC-FUNCTION
           MOVE WS-IN-OLD-PWD TO SC-PASSWORD
           MOVE USR-ID        TO SC-SALT
           CALL 'SECUTIL' USING SEC-PARMS
           IF SC-HASH NOT = USR-PWD-HASH
               DISPLAY 'CURRENT PASSWORD IS WRONG.'
               MOVE 'PWD-CHG-FAIL' TO AU-ACTION
               MOVE 'WRONG CURRENT PASSWORD' TO AU-DETAIL
               PERFORM 9000-AUDIT
               EXIT PARAGRAPH
           END-IF
           PERFORM 8000-ACCEPT-NEW-PASSWORD
           IF NOT NEW-PWD-OK
               EXIT PARAGRAPH
           END-IF
           IF WS-IN-NEW-PWD = WS-IN-OLD-PWD
               DISPLAY 'NEW PASSWORD MUST BE DIFFERENT.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8100-HASH-NEW-PASSWORD
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE WS-CURR-DATE TO USR-PWD-CHANGED-DATE
           MOVE 'N' TO USR-MUST-CHANGE
           REWRITE USER-RECORD
           MOVE 'PWD-CHANGE' TO AU-ACTION
           MOVE 'CHANGED BY USER' TO AU-DETAIL
           PERFORM 9000-AUDIT
           DISPLAY 'PASSWORD CHANGED.'.

      *----------------------------------------------------------------*
      * COMMON                                                         *
      *----------------------------------------------------------------*
       8000-ACCEPT-NEW-PASSWORD.
           MOVE 'N' TO WS-PWD-OK-SW
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

       8200-READ-TARGET-USER.
           DISPLAY 'USER ID: ' WITH NO ADVANCING
           ACCEPT WS-IN-USER-ID
           MOVE FUNCTION UPPER-CASE(WS-IN-USER-ID) TO WS-IN-USER-ID
           MOVE WS-IN-USER-ID TO USR-ID
           READ USER-FILE
               INVALID KEY
                   DISPLAY 'USER ' WS-IN-USER-ID ' NOT FOUND.'
           END-READ.

       9000-AUDIT.
           MOVE 'USERMNT' TO AU-PROGRAM
           MOVE USR-ID    TO AU-KEY
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
