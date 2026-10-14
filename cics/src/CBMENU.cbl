       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBMENU.
      *================================================================*
      * PROGRAM  : CBMENU       TRANSACTION CBMN                       *
      * PURPOSE  : ONLINE MAIN MENU. 1 = ACCOUNT INQUIRY (CBINQ),      *
      *            2 = FUND TRANSFER (CBXFR). PF3/CLEAR SIGNS OFF.     *
      * MAPSET   : CBMNMS (MAP CBMNMAP)                                *
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
           COPY CBMNMS.
           COPY CBCOMM.
           COPY DFHAID.

       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(100).

       PROCEDURE DIVISION.
       0000-MAIN.
           IF EIBCALEN = ZERO
      *        NOBODY SIGNED ON - START AT THE SIGN-ON SCREEN
               EXEC CICS XCTL PROGRAM('CBSIGN') END-EXEC
           END-IF
           MOVE DFHCOMMAREA(1:LENGTH OF CB-COMMAREA) TO CB-COMMAREA
           MOVE SPACES TO WS-MSG
           EVALUATE TRUE
               WHEN CA-PROGRAM NOT = 'CBMENU'
                   PERFORM 8000-SEND-FRESH
               WHEN EIBAID = DFHPF3 OR EIBAID = DFHCLEAR
                   EXEC CICS SIGNOFF END-EXEC
                   EXEC CICS SEND TEXT FROM('SIGNED OFF. THANK YOU.')
                        LENGTH(22) ERASE FREEKB
                   END-EXEC
                   EXEC CICS RETURN END-EXEC
               WHEN EIBAID = DFHENTER
                   PERFORM 1000-OPTION
               WHEN OTHER
                   MOVE 'INVALID KEY' TO WS-MSG
                   PERFORM 8000-SEND-FRESH
           END-EVALUATE
           MOVE 'CBMENU' TO CA-PROGRAM
           EXEC CICS RETURN TRANSID('CBMN')
                COMMAREA(CB-COMMAREA) LENGTH(LENGTH OF CB-COMMAREA)
           END-EXEC
           GOBACK.

       1000-OPTION.
           EXEC CICS RECEIVE MAP('CBMNMAP') MAPSET('CBMNMS')
                INTO(CBMNMAPI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE LOW-VALUES TO OPTIONI
           END-IF
           MOVE SPACE TO CA-STATE
           EVALUATE OPTIONI
               WHEN '1'
                   EXEC CICS XCTL PROGRAM('CBINQ')
                        COMMAREA(CB-COMMAREA)
                        LENGTH(LENGTH OF CB-COMMAREA)
                   END-EXEC
               WHEN '2'
                   EXEC CICS XCTL PROGRAM('CBXFR')
                        COMMAREA(CB-COMMAREA)
                        LENGTH(LENGTH OF CB-COMMAREA)
                   END-EXEC
               WHEN OTHER
                   MOVE 'CHOOSE 1 OR 2' TO WS-MSG
                   PERFORM 8000-SEND-FRESH
           END-EVALUATE.

       8000-SEND-FRESH.
           MOVE LOW-VALUES TO CBMNMAPO
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME) DDMMYYYY(WS-DATE)
                DATESEP('/')
           END-EXEC
           MOVE WS-DATE TO TRNDATEO
           STRING 'SIGNED ON AS ' CA-USER DELIMITED BY SIZE
               INTO USERNMO
           MOVE WS-MSG TO MSGO
           EXEC CICS SEND MAP('CBMNMAP') MAPSET('CBMNMS')
                FROM(CBMNMAPO) ERASE FREEKB
           END-EXEC.
