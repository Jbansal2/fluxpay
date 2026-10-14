       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBXFR.
      *================================================================*
      * PROGRAM  : CBXFR        TRANSACTION CBFT                       *
      * PURPOSE  : ONLINE FUND TRANSFER BETWEEN TWO ACCOUNTS, IN TWO   *
      *            SCREENS (PSEUDO-CONVERSATIONAL):                    *
      *              1. ENTER - VALIDATE, SHOW BOTH NAMES, ASK Y/N     *
      *              2. ENTER WITH Y - POST                            *
      *            THE DETAILS FROM STEP 1 ARE KEPT IN THE COMMAREA,   *
      *            AND STEP 2 REFUSES IF THE SCREEN WAS CHANGED IN     *
      *            BETWEEN. POSTING IS ONE UNIT OF WORK: BOTH ACCOUNTS *
      *            ARE READ FOR UPDATE (LOWER NUMBER FIRST, SO TWO     *
      *            OPPOSITE TRANSFERS CANNOT DEADLOCK), BOTH REWRITTEN,*
      *            TWO TRANLOG RECORDS WRITTEN, THEN SYNCPOINT. ANY    *
      *            FAILURE ROLLS EVERYTHING BACK.                      *
      *            RULES AS IN ACCTPOST: BOTH ACCOUNTS ACTIVE; DEBIT   *
      *            WITHIN AVAILABLE (SAVINGS ABOVE MINIMUM, CURRENT    *
      *            WITHIN OD LIMIT). ONLINE LIMIT 2,00,000 - ABOVE IT  *
      *            THE BRANCH MAKER-CHECKER PROCESS APPLIES.           *
      *            GL: TRANSFER OUT AND IN BOTH USE THE TRANSFER       *
      *            SUSPENSE GL, SO THE PAIR NETS TO ZERO AND NO GL     *
      *            ENTRY IS NEEDED ONLINE.                             *
      *            NOT DONE ONLINE: AML SCREENING (BATCH AMLRPT SEES   *
      *            THE TRANLOG RECORDS).                               *
      * FILES    : ACCTMAST (KSDS) - READ UPDATE / REWRITE             *
      *            TRANLOG  (ESDS) - WRITE                             *
      * MAPSET   : CBFTMS (MAP CBFTMAP)                                *
      * STATUS   : UNTESTED - NO CICS IN THIS ENVIRONMENT.             *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESP                     PIC S9(08) COMP.
       01  WS-MSG                      PIC X(79).
       01  WS-DATE                     PIC X(10).
       01  WS-YYYYMMDD                 PIC 9(08).
       01  WS-HHMMSS                   PIC 9(06).
       01  WS-ABSTIME                  PIC S9(15) COMP-3.
       01  WS-OK-SW                    PIC X(01).
           88  INPUT-OK                          VALUE 'Y'.
       01  WS-ONLINE-LIMIT             PIC 9(11)V99 VALUE 200000.
       01  WS-IN-FROM                  PIC 9(10).
       01  WS-IN-TO                    PIC 9(10).
       01  WS-IN-AMOUNT                PIC 9(11)V99.
       01  WS-AVAILABLE                PIC S9(13)V99.
       01  WS-RBA                      PIC S9(08) COMP.
       01  WS-TRAN-SEQ                 PIC 9(04) VALUE ZERO.
       01  ED-AMT                      PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  ED-AMT13                    PIC ZZZZZZZZZ9.99.
      * TWO ACCOUNT BUFFERS: FIRST = LOWER ACCOUNT NUMBER
       01  WS-FIRST-KEY                PIC 9(10).
       01  WS-SECOND-KEY               PIC 9(10).
       01  WS-FIRST-ACCT               PIC X(156).
       01  WS-SECOND-ACCT              PIC X(156).
       01  WS-DEBIT-NAME               PIC X(40).
           COPY ACCTREC.
           COPY TRANREC.
           COPY CBFTMS.
           COPY CBCOMM.
           COPY DFHAID.

       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(100).

       PROCEDURE DIVISION.
       0000-MAIN.
           IF EIBCALEN = ZERO
               EXEC CICS XCTL PROGRAM('CBSIGN') END-EXEC
           END-IF
           MOVE DFHCOMMAREA(1:LENGTH OF CB-COMMAREA) TO CB-COMMAREA
           MOVE SPACES TO WS-MSG
           EVALUATE TRUE
               WHEN CA-PROGRAM NOT = 'CBXFR'
                   MOVE SPACE TO CA-STATE
                   MOVE LOW-VALUES TO CBFTMAPO
                   PERFORM 8000-SEND
               WHEN EIBAID = DFHPF3 OR EIBAID = DFHCLEAR
                   MOVE SPACES TO CA-PROGRAM
                   EXEC CICS XCTL PROGRAM('CBMENU')
                        COMMAREA(CB-COMMAREA)
                        LENGTH(LENGTH OF CB-COMMAREA)
                   END-EXEC
               WHEN EIBAID = DFHPF5
                   MOVE SPACE TO CA-STATE
                   MOVE LOW-VALUES TO CBFTMAPO
                   PERFORM 8000-SEND
               WHEN EIBAID = DFHENTER
                   PERFORM 1000-PROCESS
               WHEN OTHER
                   MOVE 'INVALID KEY' TO WS-MSG
                   MOVE LOW-VALUES TO CBFTMAPO
                   PERFORM 8000-SEND
           END-EVALUATE
           MOVE 'CBXFR' TO CA-PROGRAM
           EXEC CICS RETURN TRANSID('CBFT')
                COMMAREA(CB-COMMAREA) LENGTH(LENGTH OF CB-COMMAREA)
           END-EXEC
           GOBACK.

       1000-PROCESS.
           EXEC CICS RECEIVE MAP('CBFTMAP') MAPSET('CBFTMS')
                INTO(CBFTMAPI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ENTER THE TRANSFER DETAILS' TO WS-MSG
               MOVE LOW-VALUES TO CBFTMAPO
               PERFORM 8000-SEND
               EXIT PARAGRAPH
           END-IF
           PERFORM 2000-EDIT-INPUT
           IF NOT INPUT-OK
               MOVE SPACE TO CA-STATE
               PERFORM 8100-RESEND-INPUT
               EXIT PARAGRAPH
           END-IF
           IF CA-AWAIT-CONFIRM
               PERFORM 4000-CONFIRM-STEP
           ELSE
               PERFORM 3000-VALIDATE-STEP
           END-IF.

      *================================================================*
      * FIELD EDITS (BOTH STEPS)                                       *
      *================================================================*
       2000-EDIT-INPUT.
           MOVE 'Y' TO WS-OK-SW
           IF FROMACL = ZERO OR FROMACI IS NOT NUMERIC
              OR TOACL = ZERO OR TOACI IS NOT NUMERIC
               MOVE 'BOTH ACCOUNT NUMBERS MUST BE 10 DIGITS' TO WS-MSG
               MOVE 'N' TO WS-OK-SW
               EXIT PARAGRAPH
           END-IF
           MOVE FROMACI TO WS-IN-FROM
           MOVE TOACI   TO WS-IN-TO
           IF WS-IN-FROM = WS-IN-TO
               MOVE 'CANNOT TRANSFER TO THE SAME ACCOUNT' TO WS-MSG
               MOVE 'N' TO WS-OK-SW
               EXIT PARAGRAPH
           END-IF
           IF AMOUNTL = ZERO
              OR FUNCTION TEST-NUMVAL(AMOUNTI(1:AMOUNTL)) NOT = ZERO
               MOVE 'AMOUNT IS NOT A VALID NUMBER' TO WS-MSG
               MOVE 'N' TO WS-OK-SW
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-IN-AMOUNT =
                   FUNCTION NUMVAL(AMOUNTI(1:AMOUNTL))
           EVALUATE TRUE
               WHEN WS-IN-AMOUNT NOT > ZERO
                   MOVE 'AMOUNT MUST BE GREATER THAN ZERO' TO WS-MSG
                   MOVE 'N' TO WS-OK-SW
               WHEN WS-IN-AMOUNT > WS-ONLINE-LIMIT
                   MOVE 'ABOVE 2,00,000 - USE THE BRANCH MAKER-CHECKER'
                        TO WS-MSG
                   MOVE 'N' TO WS-OK-SW
           END-EVALUATE.

      *================================================================*
      * STEP 1 - CHECK BOTH ACCOUNTS, SHOW NAMES, ASK FOR CONFIRMATION *
      *================================================================*
       3000-VALIDATE-STEP.
           MOVE WS-IN-FROM TO WS-FIRST-KEY
           EXEC CICS READ FILE('ACCTMAST') INTO(ACCT-RECORD)
                RIDFLD(WS-FIRST-KEY) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'FROM ACCOUNT NOT FOUND' TO WS-MSG
               PERFORM 8100-RESEND-INPUT
               EXIT PARAGRAPH
           END-IF
           PERFORM 6000-AVAILABLE
           IF NOT ACCT-ACTIVE
               MOVE 'FROM ACCOUNT IS NOT ACTIVE' TO WS-MSG
               PERFORM 8100-RESEND-INPUT
               EXIT PARAGRAPH
           END-IF
           IF WS-IN-AMOUNT > WS-AVAILABLE
               MOVE WS-AVAILABLE TO ED-AMT
               STRING 'INSUFFICIENT FUNDS. AVAILABLE ' ED-AMT
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 8100-RESEND-INPUT
               EXIT PARAGRAPH
           END-IF
           MOVE ACCT-HOLDER-NAME TO WS-DEBIT-NAME
           MOVE WS-IN-TO TO WS-SECOND-KEY
           EXEC CICS READ FILE('ACCTMAST') INTO(ACCT-RECORD)
                RIDFLD(WS-SECOND-KEY) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'TO ACCOUNT NOT FOUND' TO WS-MSG
               PERFORM 8100-RESEND-INPUT
               EXIT PARAGRAPH
           END-IF
           IF NOT ACCT-ACTIVE
               MOVE 'TO ACCOUNT IS NOT ACTIVE' TO WS-MSG
               PERFORM 8100-RESEND-INPUT
               EXIT PARAGRAPH
           END-IF
           MOVE WS-IN-FROM   TO CA-FROM-ACCT
           MOVE WS-IN-TO     TO CA-TO-ACCT
           MOVE WS-IN-AMOUNT TO CA-AMOUNT
           MOVE NARRI        TO CA-NARRATION
           IF NARRL = ZERO
               MOVE 'ONLINE TRANSFER' TO CA-NARRATION
           END-IF
           SET CA-AWAIT-CONFIRM TO TRUE
           MOVE LOW-VALUES TO CBFTMAPO
           MOVE WS-IN-FROM   TO FROMACO
           MOVE WS-IN-TO     TO TOACO
           MOVE WS-IN-AMOUNT TO ED-AMT13
           MOVE ED-AMT13     TO AMOUNTO
           MOVE CA-NARRATION TO NARRO
           MOVE WS-DEBIT-NAME    TO FROMNMO
           MOVE ACCT-HOLDER-NAME TO TONMO
           MOVE 'CHECK THE NAMES, TYPE Y IN CONFIRM AND PRESS ENTER'
               TO WS-MSG
           PERFORM 8000-SEND.

      *================================================================*
      * STEP 2 - POST (ONLY IF THE SCREEN STILL MATCHES STEP 1)        *
      *================================================================*
       4000-CONFIRM-STEP.
           IF WS-IN-FROM NOT = CA-FROM-ACCT OR WS-IN-TO NOT = CA-TO-ACCT
              OR WS-IN-AMOUNT NOT = CA-AMOUNT
               MOVE SPACE TO CA-STATE
               MOVE 'DETAILS CHANGED - CHECKED AGAIN, PLEASE CONFIRM'
                   TO WS-MSG
               PERFORM 3000-VALIDATE-STEP
               EXIT PARAGRAPH
           END-IF
           IF CONFIRML = ZERO OR
              (CONFIRMI NOT = 'Y' AND CONFIRMI NOT = 'y')
               MOVE SPACE TO CA-STATE
               MOVE 'TRANSFER NOT CONFIRMED - NOTHING POSTED' TO WS-MSG
               MOVE LOW-VALUES TO CBFTMAPO
               PERFORM 8000-SEND
               EXIT PARAGRAPH
           END-IF
           PERFORM 5000-POST
           MOVE SPACE TO CA-STATE
           MOVE LOW-VALUES TO CBFTMAPO
           PERFORM 8000-SEND.

       5000-POST.
      *    LOCK IN ACCOUNT-NUMBER ORDER
           IF CA-FROM-ACCT < CA-TO-ACCT
               MOVE CA-FROM-ACCT TO WS-FIRST-KEY
               MOVE CA-TO-ACCT   TO WS-SECOND-KEY
           ELSE
               MOVE CA-TO-ACCT   TO WS-FIRST-KEY
               MOVE CA-FROM-ACCT TO WS-SECOND-KEY
           END-IF
           EXEC CICS READ FILE('ACCTMAST') INTO(WS-FIRST-ACCT)
                RIDFLD(WS-FIRST-KEY) UPDATE RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               EXEC CICS READ FILE('ACCTMAST') INTO(WS-SECOND-ACCT)
                    RIDFLD(WS-SECOND-KEY) UPDATE RESP(WS-RESP)
               END-EXEC
           END-IF
           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9000-ROLLBACK
               MOVE 'ACCOUNT NOT AVAILABLE - TRY AGAIN' TO WS-MSG
               EXIT PARAGRAPH
           END-IF
      *    DEBIT SIDE - RE-CHECKED UNDER THE LOCK
           IF WS-FIRST-KEY = CA-FROM-ACCT
               MOVE WS-FIRST-ACCT TO ACCT-RECORD
           ELSE
               MOVE WS-SECOND-ACCT TO ACCT-RECORD
           END-IF
           PERFORM 6000-AVAILABLE
           IF NOT ACCT-ACTIVE OR CA-AMOUNT > WS-AVAILABLE
               PERFORM 9000-ROLLBACK
               MOVE 'NOT POSTED - BALANCE OR STATUS CHANGED' TO WS-MSG
               EXIT PARAGRAPH
           END-IF
           SUBTRACT CA-AMOUNT FROM ACCT-BALANCE
           PERFORM 7000-STAMP-ACCOUNT
           MOVE 'TO' TO TRAN-TYPE
           MOVE CA-TO-ACCT TO TRAN-REF-ACCT
           PERFORM 7100-BUILD-TRAN
           IF WS-FIRST-KEY = CA-FROM-ACCT
               MOVE ACCT-RECORD TO WS-FIRST-ACCT
           ELSE
               MOVE ACCT-RECORD TO WS-SECOND-ACCT
           END-IF
           PERFORM 7200-WRITE-TRAN
           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9000-ROLLBACK
               MOVE 'NOT POSTED - TRANSACTION LOG ERROR' TO WS-MSG
               EXIT PARAGRAPH
           END-IF
      *    CREDIT SIDE
           IF WS-FIRST-KEY = CA-TO-ACCT
               MOVE WS-FIRST-ACCT TO ACCT-RECORD
           ELSE
               MOVE WS-SECOND-ACCT TO ACCT-RECORD
           END-IF
           IF NOT ACCT-ACTIVE
               PERFORM 9000-ROLLBACK
               MOVE 'NOT POSTED - TO ACCOUNT NO LONGER ACTIVE' TO WS-MSG
               EXIT PARAGRAPH
           END-IF
           ADD CA-AMOUNT TO ACCT-BALANCE
           PERFORM 7000-STAMP-ACCOUNT
           MOVE 'TI' TO TRAN-TYPE
           MOVE CA-FROM-ACCT TO TRAN-REF-ACCT
           PERFORM 7100-BUILD-TRAN
           IF WS-FIRST-KEY = CA-TO-ACCT
               MOVE ACCT-RECORD TO WS-FIRST-ACCT
           ELSE
               MOVE ACCT-RECORD TO WS-SECOND-ACCT
           END-IF
           PERFORM 7200-WRITE-TRAN
           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9000-ROLLBACK
               MOVE 'NOT POSTED - TRANSACTION LOG ERROR' TO WS-MSG
               EXIT PARAGRAPH
           END-IF
      *    BOTH MASTERS, THEN COMMIT
           EXEC CICS REWRITE FILE('ACCTMAST') FROM(WS-FIRST-ACCT)
                RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               EXEC CICS REWRITE FILE('ACCTMAST') FROM(WS-SECOND-ACCT)
                    RESP(WS-RESP)
               END-EXEC
           END-IF
           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 9000-ROLLBACK
               MOVE 'NOT POSTED - ACCOUNT UPDATE FAILED' TO WS-MSG
               EXIT PARAGRAPH
           END-IF
           EXEC CICS SYNCPOINT END-EXEC
           MOVE CA-AMOUNT TO ED-AMT
           STRING 'TRANSFERRED ' ED-AMT ' TO ' CA-TO-ACCT
               DELIMITED BY SIZE INTO WS-MSG.

      *    AVAILABLE = BALANCE - MINIMUM (SAVINGS), + OD LIMIT (CURRENT)
       6000-AVAILABLE.
           IF ACCT-CURRENT
               COMPUTE WS-AVAILABLE = ACCT-BALANCE + ACCT-OD-LIMIT
           ELSE
               COMPUTE WS-AVAILABLE = ACCT-BALANCE - ACCT-MIN-BALANCE
           END-IF
           IF WS-AVAILABLE < ZERO
               MOVE ZERO TO WS-AVAILABLE
           END-IF.

       7000-STAMP-ACCOUNT.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                YYYYMMDD(WS-YYYYMMDD) TIME(WS-HHMMSS)
           END-EXEC
           MOVE WS-YYYYMMDD TO ACCT-LAST-TXN-DATE.

       7100-BUILD-TRAN.
           ADD 1 TO WS-TRAN-SEQ
      *    DATE + TIME + SEQUENCE; THE BATCH SIDE USES DATE * 10000 +
      *    ITS OWN SEQUENCE, SO ONLINE IDS START WITH A 9 IN THE SEQ
           COMPUTE TRAN-ID = WS-YYYYMMDD * 10000 + 9000 + WS-TRAN-SEQ
           MOVE ACCT-NO        TO TRAN-ACCT-NO
           MOVE WS-YYYYMMDD    TO TRAN-DATE
           MOVE WS-HHMMSS      TO TRAN-TIME
           MOVE CA-AMOUNT      TO TRAN-AMOUNT
           MOVE ACCT-BALANCE   TO TRAN-BAL-AFTER
           MOVE CA-NARRATION   TO TRAN-NARRATION.

       7200-WRITE-TRAN.
           EXEC CICS WRITE FILE('TRANLOG') FROM(TRAN-RECORD)
                RIDFLD(WS-RBA) RBA RESP(WS-RESP)
           END-EXEC.

       8000-SEND.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME) DDMMYYYY(WS-DATE)
                DATESEP('/')
           END-EXEC
           MOVE WS-DATE TO TRNDATEO
           MOVE WS-MSG  TO MSGO
           EXEC CICS SEND MAP('CBFTMAP') MAPSET('CBFTMS')
                FROM(CBFTMAPO) ERASE FREEKB
           END-EXEC.

      *    SEND BACK WHAT THE USER TYPED, WITH THE ERROR MESSAGE
       8100-RESEND-INPUT.
           MOVE SPACE TO CA-STATE
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME) DDMMYYYY(WS-DATE)
                DATESEP('/')
           END-EXEC
           MOVE WS-DATE TO TRNDATEO
           MOVE WS-MSG  TO MSGO
           EXEC CICS SEND MAP('CBFTMAP') MAPSET('CBFTMS')
                FROM(CBFTMAPO) DATAONLY FREEKB
           END-EXEC.

       9000-ROLLBACK.
           EXEC CICS SYNCPOINT ROLLBACK END-EXEC.
