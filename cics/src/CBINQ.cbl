       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBINQ.
      *================================================================*
      * PROGRAM  : CBINQ        TRANSACTION CBAI                       *
      * PURPOSE  : ONLINE ACCOUNT INQUIRY - BALANCE, AVAILABLE AMOUNT  *
      *            (SAME RULE AS ACCTPOST: SAVINGS = BALANCE - MINIMUM,*
      *            CURRENT = BALANCE + OD LIMIT) AND THE LAST FIVE     *
      *            TRANSACTIONS, NEWEST FIRST.                         *
      * FILES    : ACCTMAST (KSDS)  - READ                             *
      *            TRANACCT (PATH OVER AN ALTERNATE INDEX ON THE       *
      *            ACCOUNT NUMBER OF THE TRANLOG ESDS) - BROWSE        *
      * MAPSET   : CBAIMS (MAP CBAIMAP)                                *
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
       01  WS-ABSTIME                  PIC S9(15) COMP-3.
       01  WS-ACCT-KEY                 PIC 9(10).
       01  WS-BROWSE-KEY               PIC 9(10).
       01  WS-AVAILABLE                PIC S9(13)V99.
       01  WS-N                        PIC 9(01).
       01  WS-TXN-LINE                 PIC X(78).
       01  ED-AMT                      PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-TXN-AMT                  PIC Z,ZZZ,ZZZ,ZZ9.99.
           COPY ACCTREC.
           COPY TRANREC.
           COPY CBAIMS.
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
               WHEN CA-PROGRAM NOT = 'CBINQ'
                   MOVE LOW-VALUES TO CBAIMAPO
                   PERFORM 8000-SEND
               WHEN EIBAID = DFHPF3 OR EIBAID = DFHCLEAR
                   MOVE SPACES TO CA-PROGRAM
                   EXEC CICS XCTL PROGRAM('CBMENU')
                        COMMAREA(CB-COMMAREA)
                        LENGTH(LENGTH OF CB-COMMAREA)
                   END-EXEC
               WHEN EIBAID = DFHPF5
                   MOVE LOW-VALUES TO CBAIMAPO
                   PERFORM 8000-SEND
               WHEN EIBAID = DFHENTER
                   PERFORM 1000-INQUIRE
               WHEN OTHER
                   MOVE 'INVALID KEY' TO WS-MSG
                   MOVE LOW-VALUES TO CBAIMAPO
                   PERFORM 8000-SEND
           END-EVALUATE
           MOVE 'CBINQ' TO CA-PROGRAM
           EXEC CICS RETURN TRANSID('CBAI')
                COMMAREA(CB-COMMAREA) LENGTH(LENGTH OF CB-COMMAREA)
           END-EXEC
           GOBACK.

       1000-INQUIRE.
           EXEC CICS RECEIVE MAP('CBAIMAP') MAPSET('CBAIMS')
                INTO(CBAIMAPI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL) OR ACCTNOL = ZERO
              OR ACCTNOI IS NOT NUMERIC
               MOVE 'ENTER A 10-DIGIT ACCOUNT NUMBER' TO WS-MSG
               MOVE LOW-VALUES TO CBAIMAPO
               PERFORM 8000-SEND
               EXIT PARAGRAPH
           END-IF
           MOVE ACCTNOI TO WS-ACCT-KEY
           EXEC CICS READ FILE('ACCTMAST') INTO(ACCT-RECORD)
                RIDFLD(WS-ACCT-KEY) RESP(WS-RESP)
           END-EXEC
           MOVE LOW-VALUES TO CBAIMAPO
           MOVE WS-ACCT-KEY TO ACCTNOO
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM 2000-SHOW-ACCOUNT
                   PERFORM 3000-RECENT-TXNS
               WHEN DFHRESP(NOTFND)
                   MOVE 'ACCOUNT NOT FOUND' TO WS-MSG
               WHEN OTHER
                   STRING 'ACCOUNT FILE ERROR, RESP ' WS-RESP
                       DELIMITED BY SIZE INTO WS-MSG
           END-EVALUATE
           PERFORM 8000-SEND.

       2000-SHOW-ACCOUNT.
           MOVE ACCT-HOLDER-NAME TO NAMEO
           IF ACCT-SAVINGS
               MOVE 'SAVINGS' TO ACTYPEO
               COMPUTE WS-AVAILABLE = ACCT-BALANCE - ACCT-MIN-BALANCE
           ELSE
               MOVE 'CURRENT' TO ACTYPEO
               COMPUTE WS-AVAILABLE = ACCT-BALANCE + ACCT-OD-LIMIT
           END-IF
           IF WS-AVAILABLE < ZERO
               MOVE ZERO TO WS-AVAILABLE
           END-IF
           EVALUATE TRUE
               WHEN ACCT-ACTIVE  MOVE 'ACTIVE'  TO STATUSO
               WHEN ACCT-FROZEN  MOVE 'FROZEN'  TO STATUSO
               WHEN ACCT-DORMANT MOVE 'DORMANT' TO STATUSO
               WHEN ACCT-CLOSED  MOVE 'CLOSED'  TO STATUSO
               WHEN OTHER        MOVE ACCT-STATUS TO STATUSO
           END-EVALUATE
           MOVE ACCT-BRANCH-CODE TO BRANCHO
           MOVE ACCT-BALANCE     TO ED-AMT
           MOVE ED-AMT           TO BALO
           MOVE WS-AVAILABLE     TO ED-AMT
           MOVE ED-AMT           TO AVAILO
           MOVE ACCT-LAST-TXN-DATE TO LASTTXNO.

      *    NEWEST FIRST: POSITION JUST PAST THIS ACCOUNT ON THE
      *    ALTERNATE INDEX AND READ BACKWARDS
       3000-RECENT-TXNS.
           COMPUTE WS-BROWSE-KEY = WS-ACCT-KEY + 1
           EXEC CICS STARTBR FILE('TRANACCT') RIDFLD(WS-BROWSE-KEY)
                GTEQ RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NOTFND)
      *        NOTHING BEYOND THIS ACCOUNT - START FROM THE END
               MOVE 9999999999 TO WS-BROWSE-KEY
               EXEC CICS STARTBR FILE('TRANACCT')
                    RIDFLD(WS-BROWSE-KEY) RESP(WS-RESP)
               END-EXEC
           END-IF
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'NO TRANSACTION HISTORY AVAILABLE' TO WS-MSG
               EXIT PARAGRAPH
           END-IF
           MOVE ZERO TO WS-N
           PERFORM UNTIL WS-N = 5
               EXEC CICS READPREV FILE('TRANACCT') INTO(TRAN-RECORD)
                    RIDFLD(WS-BROWSE-KEY) RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                  AND WS-RESP NOT = DFHRESP(DUPKEY)
                   EXIT PERFORM
               END-IF
               IF TRAN-ACCT-NO = WS-ACCT-KEY
                   ADD 1 TO WS-N
                   PERFORM 3100-FORMAT-TXN
               ELSE
                   IF TRAN-ACCT-NO < WS-ACCT-KEY
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM
           EXEC CICS ENDBR FILE('TRANACCT') END-EXEC
           IF WS-N = ZERO
               MOVE 'NO TRANSACTIONS ON THIS ACCOUNT' TO WS-MSG
           END-IF.

       3100-FORMAT-TXN.
           MOVE TRAN-AMOUNT TO ED-TXN-AMT
           MOVE SPACES TO WS-TXN-LINE
           STRING TRAN-DATE ' ' TRAN-TYPE ' ' ED-TXN-AMT ' '
                  TRAN-NARRATION DELIMITED BY SIZE INTO WS-TXN-LINE
           EVALUATE WS-N
               WHEN 1 MOVE WS-TXN-LINE TO TXN1O
               WHEN 2 MOVE WS-TXN-LINE TO TXN2O
               WHEN 3 MOVE WS-TXN-LINE TO TXN3O
               WHEN 4 MOVE WS-TXN-LINE TO TXN4O
               WHEN 5 MOVE WS-TXN-LINE TO TXN5O
           END-EVALUATE.

       8000-SEND.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME) DDMMYYYY(WS-DATE)
                DATESEP('/')
           END-EXEC
           MOVE WS-DATE TO TRNDATEO
           MOVE WS-MSG  TO MSGO
           EXEC CICS SEND MAP('CBAIMAP') MAPSET('CBAIMS')
                FROM(CBAIMAPO) ERASE FREEKB
           END-EXEC.
