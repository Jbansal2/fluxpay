       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBSIGN.
      *================================================================*
      * PROGRAM  : CBSIGN       TRANSACTION CBSN                       *
      * PURPOSE  : CICS SIGN-ON SCREEN. PASSWORDS ARE CHECKED BY THE   *
      *            EXTERNAL SECURITY MANAGER (EXEC CICS SIGNON), NOT   *
      *            BY THIS PROGRAM - THE BATCH USER FILE IS NOT USED   *
      *            ONLINE. ON SUCCESS CONTROL PASSES TO CBMENU.        *
      * MAPSET   : CBSNMS (MAP CBSNMAP)                                *
      * STATUS   : UNTESTED - NO CICS IN THIS ENVIRONMENT. THE COBOL   *
      *            OUTSIDE THE EXEC CICS BLOCKS IS SYNTAX-CHECKED BY   *
      *            cics/check.sh.                                      *
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
           COPY CBSNMS.
           COPY CBCOMM.
           COPY DFHAID.
           COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(100).

       PROCEDURE DIVISION.
       0000-MAIN.
           EVALUATE TRUE
               WHEN EIBCALEN = ZERO
                   MOVE SPACES TO WS-MSG
                   PERFORM 8000-SEND-FRESH
               WHEN EIBAID = DFHPF3 OR EIBAID = DFHCLEAR
                   EXEC CICS SEND TEXT FROM('COREBANK - GOODBYE')
                        LENGTH(18) ERASE FREEKB
                   END-EXEC
                   EXEC CICS RETURN END-EXEC
               WHEN EIBAID = DFHENTER
                   PERFORM 1000-SIGN-ON
               WHEN OTHER
                   MOVE 'INVALID KEY - PRESS ENTER TO SIGN ON' TO WS-MSG
                   PERFORM 8000-SEND-FRESH
           END-EVALUATE
           MOVE 'CBSIGN' TO CA-PROGRAM
           EXEC CICS RETURN TRANSID('CBSN')
                COMMAREA(CB-COMMAREA) LENGTH(LENGTH OF CB-COMMAREA)
           END-EXEC
           GOBACK.

       1000-SIGN-ON.
           EXEC CICS RECEIVE MAP('CBSNMAP') MAPSET('CBSNMS')
                INTO(CBSNMAPI) RESP(WS-RESP)
           END-EXEC
           IF WS-RESP NOT = DFHRESP(NORMAL)
              OR USERIDL = ZERO OR PASSWDL = ZERO
               MOVE 'ENTER USER ID AND PASSWORD' TO WS-MSG
               PERFORM 8000-SEND-FRESH
               EXIT PARAGRAPH
           END-IF
           EXEC CICS SIGNON USERID(USERIDI) PASSWORD(PASSWDI)
                RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   INITIALIZE CB-COMMAREA
                   MOVE USERIDI TO CA-USER
                   EXEC CICS XCTL PROGRAM('CBMENU')
                        COMMAREA(CB-COMMAREA)
                        LENGTH(LENGTH OF CB-COMMAREA)
                   END-EXEC
               WHEN DFHRESP(NOTAUTH)
      *            SAME MESSAGE FOR BAD USER AND BAD PASSWORD
                   MOVE 'INVALID USER ID OR PASSWORD' TO WS-MSG
               WHEN DFHRESP(USERIDERR)
                   MOVE 'INVALID USER ID OR PASSWORD' TO WS-MSG
               WHEN OTHER
                   MOVE 'SIGN-ON FAILED - CALL THE HELP DESK' TO WS-MSG
           END-EVALUATE
           PERFORM 8000-SEND-FRESH.

       8000-SEND-FRESH.
           MOVE LOW-VALUES TO CBSNMAPO
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME) DDMMYYYY(WS-DATE)
                DATESEP('/')
           END-EXEC
           MOVE WS-DATE TO TRNDATEO
           MOVE WS-MSG  TO MSGO
           EXEC CICS SEND MAP('CBSNMAP') MAPSET('CBSNMS')
                FROM(CBSNMAPO) ERASE FREEKB
           END-EXEC.
