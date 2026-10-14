       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRMNT.
      *================================================================*
      * PROGRAM  : BRMNT                                               *
      * PURPOSE  : BRANCH MASTER MAINTENANCE (ADMIN).                  *
      *            1 ADD BRANCH  - IFSC FORMAT AAAA0XXXXXX, ONE HEAD   *
      *                            OFFICE ONLY                         *
      *            2 LIST BRANCHES WITH ACCOUNT COUNTS                 *
      *            3 CLOSE BRANCH - ONLY WHEN NO OPEN ACCOUNT IS LEFT  *
      *                            IN IT (MOVE THEM WITH ACCTMNT 3)    *
      *            4 REGISTER EVERY BRANCH CODE FOUND ON ACCOUNTS      *
      *              THAT IS NOT YET IN THE MASTER                     *
      * FILES    : BRMAST (I-O), ACCTMAST (INPUT)                      *
      * CALLS    : AUDITLOG                                            *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BR-FILE ASSIGN TO 'data/BRMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BR-CODE
               FILE STATUS IS WS-BR-FS.
           SELECT ACCT-FILE ASSIGN TO 'data/ACCTMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-ACCT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  BR-FILE.
           COPY BRMREC.
       FD  ACCT-FILE.
           COPY ACCTREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY SESSION.
           COPY AUDPARM.
       01  WS-BR-FS                    PIC X(02).
       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.
       01  WS-EOF-SW                   PIC X(01).
       01  WS-CONFIRM                  PIC X(01).
       01  WS-IN-CODE                  PIC X(06).
       01  WS-IN-IFSC                  PIC X(11).
       01  WS-I                        PIC 9(02).
       01  WS-OK-SW                    PIC X(01).
       01  WS-HO-COUNT                 PIC 9(03).
       01  WS-ADDED                    PIC 9(03).

      * BRANCH CODES SEEN ON ACCOUNTS, WITH OPEN-ACCOUNT COUNTS
       01  WS-SEEN-CNT                 PIC 9(03).
       01  WS-SEEN-TABLE.
           05  WS-SEEN OCCURS 200 TIMES.
               10  SEEN-CODE           PIC X(06).
               10  SEEN-OPEN           PIC 9(07).
       01  WS-S                        PIC 9(03).
       01  WS-J                        PIC 9(03).
       01  WS-SAVE-BR                  PIC X(100).
       01  WS-DISP-CNT                 PIC ZZZ,ZZ9.

       PROCEDURE DIVISION.
       0000-MAIN.
           IF NOT SES-ADMIN
               DISPLAY 'BRANCH MASTER IS FOR ADMINISTRATORS ONLY.'
               GOBACK
           END-IF
           OPEN I-O BR-FILE
           IF WS-BR-FS = '35'
               OPEN OUTPUT BR-FILE
               CLOSE BR-FILE
               OPEN I-O BR-FILE
           END-IF
           IF WS-BR-FS NOT = '00'
               DISPLAY 'BRANCH MASTER OPEN FAILED, FS=' WS-BR-FS
               GOBACK
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE = '9'
               DISPLAY ' '
               DISPLAY '------------ BRANCH MASTER ------------'
               DISPLAY ' 1. ADD BRANCH'
               DISPLAY ' 2. LIST BRANCHES'
               DISPLAY ' 3. CLOSE BRANCH'
               DISPLAY ' 4. REGISTER BRANCH CODES FOUND ON ACCOUNTS'
               DISPLAY ' 9. BACK'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 1000-ADD
                   WHEN '2' PERFORM 2000-LIST
                   WHEN '3' PERFORM 3000-CLOSE
                   WHEN '4' PERFORM 4000-REGISTER-SEEN
                   WHEN '9' CONTINUE
                   WHEN SPACE
                       ADD 1 TO WS-BLANK-INPUTS
                       IF WS-BLANK-INPUTS > 20
                           MOVE '9' TO WS-CHOICE
                       END-IF
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
               IF WS-CHOICE NOT = SPACE
                   MOVE ZERO TO WS-BLANK-INPUTS
               END-IF
           END-PERFORM
           CLOSE BR-FILE
           GOBACK.

       1000-ADD.
           DISPLAY 'BRANCH CODE (6)   : ' WITH NO ADVANCING
           ACCEPT WS-IN-CODE
           MOVE FUNCTION UPPER-CASE(WS-IN-CODE) TO WS-IN-CODE
           MOVE WS-IN-CODE TO BR-CODE
           READ BR-FILE
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   DISPLAY 'BRANCH ' WS-IN-CODE ' ALREADY EXISTS.'
                   EXIT PARAGRAPH
           END-READ
           INITIALIZE BRM-RECORD
           MOVE WS-IN-CODE TO BR-CODE
           DISPLAY 'NAME              : ' WITH NO ADVANCING
           ACCEPT BR-NAME
           DISPLAY 'IFSC              : ' WITH NO ADVANCING
           ACCEPT WS-IN-IFSC
           DISPLAY 'CITY              : ' WITH NO ADVANCING
           ACCEPT BR-CITY
           DISPLAY 'REGION            : ' WITH NO ADVANCING
           ACCEPT BR-REGION
           DISPLAY 'H=HEAD OFFICE B=BRANCH: ' WITH NO ADVANCING
           ACCEPT BR-TYPE
           MOVE FUNCTION UPPER-CASE(BR-NAME)   TO BR-NAME
           MOVE FUNCTION UPPER-CASE(WS-IN-IFSC) TO BR-IFSC
           MOVE FUNCTION UPPER-CASE(BR-CITY)   TO BR-CITY
           MOVE FUNCTION UPPER-CASE(BR-REGION) TO BR-REGION
           MOVE FUNCTION UPPER-CASE(BR-TYPE)   TO BR-TYPE
           PERFORM 1100-CHECK-IFSC
           EVALUATE TRUE
               WHEN WS-IN-CODE = SPACES OR BR-NAME = SPACES
                   DISPLAY 'CODE AND NAME ARE REQUIRED.'
               WHEN WS-OK-SW = 'N'
                   DISPLAY 'IFSC MUST BE 4 LETTERS, 0, THEN 6 LETTERS/'
                           'DIGITS.'
               WHEN NOT BR-HEAD-OFFICE AND NOT BR-BRANCH
                   DISPLAY 'TYPE MUST BE H OR B.'
               WHEN OTHER
                   IF BR-HEAD-OFFICE
                       PERFORM 1200-COUNT-HO
                   END-IF
                   IF BR-HEAD-OFFICE AND WS-HO-COUNT > ZERO
                       DISPLAY 'THERE IS ALREADY A HEAD OFFICE.'
                   ELSE
                       MOVE WS-IN-CODE   TO BR-CODE
                       SET BR-OPEN       TO TRUE
                       MOVE WS-CURR-DATE TO BR-OPEN-DATE
                       WRITE BRM-RECORD
                       MOVE 'BRANCH ADD' TO AU-ACTION
                       MOVE BR-CODE TO AU-KEY
                       MOVE BR-NAME TO AU-DETAIL
                       PERFORM 8900-AUDIT
                       DISPLAY 'BRANCH ' BR-CODE ' ADDED.'
                   END-IF
           END-EVALUATE.

       1100-CHECK-IFSC.
           MOVE 'Y' TO WS-OK-SW
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               IF BR-IFSC(WS-I:1) IS NOT ALPHABETIC
                  OR BR-IFSC(WS-I:1) = SPACE
                   MOVE 'N' TO WS-OK-SW
               END-IF
           END-PERFORM
           IF BR-IFSC(5:1) NOT = '0'
               MOVE 'N' TO WS-OK-SW
           END-IF
           PERFORM VARYING WS-I FROM 6 BY 1 UNTIL WS-I > 11
               IF BR-IFSC(WS-I:1) = SPACE
                   MOVE 'N' TO WS-OK-SW
               END-IF
           END-PERFORM.

       1200-COUNT-HO.
           MOVE BRM-RECORD TO WS-SAVE-BR
           MOVE ZERO TO WS-HO-COUNT
           MOVE LOW-VALUES TO BR-CODE
           START BR-FILE KEY >= BR-CODE
               INVALID KEY MOVE 'Y' TO WS-EOF-SW
               NOT INVALID KEY MOVE 'N' TO WS-EOF-SW
           END-START
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ BR-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N' AND BR-HEAD-OFFICE AND BR-OPEN
                   ADD 1 TO WS-HO-COUNT
               END-IF
           END-PERFORM
           MOVE WS-SAVE-BR TO BRM-RECORD.

       2000-LIST.
           PERFORM 8000-COUNT-ACCOUNTS
           MOVE LOW-VALUES TO BR-CODE
           START BR-FILE KEY >= BR-CODE
               INVALID KEY
                   DISPLAY 'NO BRANCHES REGISTERED.'
                   EXIT PARAGRAPH
           END-START
           DISPLAY 'CODE   NAME                           IFSC        '
                   'T S  ACCOUNTS'
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ BR-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N'
                   MOVE BR-CODE TO WS-IN-CODE
                   PERFORM 8100-FIND-SEEN
                   IF WS-S > ZERO
                       MOVE SEEN-OPEN(WS-S) TO WS-DISP-CNT
                   ELSE
                       MOVE ZERO TO WS-DISP-CNT
                   END-IF
                   DISPLAY BR-CODE ' ' BR-NAME ' ' BR-IFSC ' '
                           BR-TYPE ' ' BR-STATUS ' ' WS-DISP-CNT
               END-IF
           END-PERFORM.

       3000-CLOSE.
           DISPLAY 'BRANCH CODE TO CLOSE: ' WITH NO ADVANCING
           ACCEPT WS-IN-CODE
           MOVE FUNCTION UPPER-CASE(WS-IN-CODE) TO BR-CODE
           READ BR-FILE
               INVALID KEY
                   DISPLAY 'BRANCH ' WS-IN-CODE ' NOT FOUND.'
                   EXIT PARAGRAPH
           END-READ
           IF BR-CLOSED
               DISPLAY 'BRANCH IS ALREADY CLOSED.'
               EXIT PARAGRAPH
           END-IF
           IF BR-HEAD-OFFICE
               DISPLAY 'THE HEAD OFFICE CANNOT BE CLOSED.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8000-COUNT-ACCOUNTS
           MOVE BR-CODE TO WS-IN-CODE
           PERFORM 8100-FIND-SEEN
           IF WS-S > ZERO AND SEEN-OPEN(WS-S) > ZERO
               MOVE SEEN-OPEN(WS-S) TO WS-DISP-CNT
               DISPLAY 'BRANCH STILL HAS ' FUNCTION TRIM(WS-DISP-CNT)
                       ' OPEN ACCOUNT(S) - MOVE THEM FIRST.'
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-CODE TO BR-CODE
           READ BR-FILE
           SET BR-CLOSED TO TRUE
           MOVE WS-CURR-DATE TO BR-CLOSE-DATE
           REWRITE BRM-RECORD
           MOVE 'BRANCH CLOSE' TO AU-ACTION
           MOVE BR-CODE TO AU-KEY
           MOVE SPACES  TO AU-DETAIL
           PERFORM 8900-AUDIT
           DISPLAY 'BRANCH ' BR-CODE ' CLOSED.'.

       4000-REGISTER-SEEN.
           PERFORM 8000-COUNT-ACCOUNTS
           MOVE ZERO TO WS-ADDED
           PERFORM VARYING WS-S FROM 1 BY 1 UNTIL WS-S > WS-SEEN-CNT
               MOVE SEEN-CODE(WS-S) TO BR-CODE
               READ BR-FILE
                   INVALID KEY
                       INITIALIZE BRM-RECORD
                       MOVE SEEN-CODE(WS-S) TO BR-CODE
                       STRING 'BRANCH ' SEEN-CODE(WS-S)
                              DELIMITED BY SIZE INTO BR-NAME
                       IF SEEN-CODE(WS-S)(1:2) = 'HO'
                           SET BR-HEAD-OFFICE TO TRUE
                       ELSE
                           SET BR-BRANCH TO TRUE
                       END-IF
                       SET BR-OPEN TO TRUE
                       MOVE WS-CURR-DATE TO BR-OPEN-DATE
                       WRITE BRM-RECORD
                       ADD 1 TO WS-ADDED
               END-READ
           END-PERFORM
           DISPLAY WS-ADDED ' BRANCH CODE(S) REGISTERED FROM ACCOUNTS.'.

      *    ONE PASS OVER ACCOUNTS: DISTINCT BRANCH CODES, OPEN COUNTS
       8000-COUNT-ACCOUNTS.
           MOVE ZERO TO WS-SEEN-CNT
           INITIALIZE WS-SEEN-TABLE
           OPEN INPUT ACCT-FILE
           IF NOT ACCT-FS-OK
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ ACCT-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N'
                   MOVE ACCT-BRANCH-CODE TO WS-IN-CODE
                   PERFORM 8100-FIND-SEEN
                   IF WS-S = ZERO AND WS-SEEN-CNT < 200
                       ADD 1 TO WS-SEEN-CNT
                       MOVE WS-SEEN-CNT TO WS-S
                       MOVE WS-IN-CODE TO SEEN-CODE(WS-S)
                   END-IF
                   IF WS-S > ZERO AND NOT ACCT-CLOSED
                       ADD 1 TO SEEN-OPEN(WS-S)
                   END-IF
               END-IF
           END-PERFORM
           CLOSE ACCT-FILE
           MOVE 'N' TO WS-EOF-SW.

      *    WS-IN-CODE -> WS-S (SLOT IN THE SEEN TABLE), ZERO = NONE
       8100-FIND-SEEN.
           MOVE ZERO TO WS-S
           PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-SEEN-CNT OR WS-S > ZERO
               IF SEEN-CODE(WS-J) = WS-IN-CODE
                   MOVE WS-J TO WS-S
               END-IF
           END-PERFORM.

       8900-AUDIT.
           MOVE 'BRMNT' TO AU-PROGRAM
           CALL 'AUDITLOG' USING AUD-PARMS.
