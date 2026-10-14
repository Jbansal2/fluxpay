       IDENTIFICATION DIVISION.
       PROGRAM-ID. STMTREQ.
      *================================================================*
      * PROGRAM  : STMTREQ                                             *
      * PURPOSE  : TAKES A CUSTOMER'S REQUEST FOR A STATEMENT FOR ANY  *
      *            PERIOD (UP TO ONE YEAR, NOT IN THE FUTURE) AND      *
      *            QUEUES IT FOR THE NEXT STMTBAT RUN. E-MAIL DELIVERY *
      *            NEEDS AN E-MAIL ADDRESS ON THE PRIMARY HOLDER'S     *
      *            CUSTOMER RECORD; OTHERWISE IT IS PRINTED.           *
      * FILES    : STMTREQ (EXTEND)                                    *
      * CALLS    : ACCTPOST (INQUIRY), CUSTLKUP, DATEUTIL              *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REQ-FILE ASSIGN TO 'data/STMTREQ.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  REQ-FILE.
           COPY SREQREC.

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-TODAY                    PIC 9(08).
       01  WS-IN-ACCT-NO               PIC 9(10).
       01  WS-IN-FROM                  PIC 9(08).
       01  WS-IN-TO                    PIC 9(08).
       01  WS-IN-DELIVERY              PIC X(01).
           COPY POSTPARM.
           COPY CLKPARM.
           COPY DATEPARM.
           COPY SESSION.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           DISPLAY 'ACCOUNT NO               : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT-NO
           DISPLAY 'FROM DATE (YYYYMMDD)     : ' WITH NO ADVANCING
           ACCEPT WS-IN-FROM
           DISPLAY 'TO DATE   (YYYYMMDD)     : ' WITH NO ADVANCING
           ACCEPT WS-IN-TO
           DISPLAY 'DELIVERY E=E-MAIL P=POST : ' WITH NO ADVANCING
           ACCEPT WS-IN-DELIVERY
           MOVE FUNCTION UPPER-CASE(WS-IN-DELIVERY) TO WS-IN-DELIVERY
           INITIALIZE POST-PARMS
           SET PP-INQUIRE TO TRUE
           MOVE WS-IN-ACCT-NO TO PP-ACCT-NO
           CALL 'ACCTPOST' USING POST-PARMS
           IF PP-NOT-FOUND
               DISPLAY 'ACCOUNT NOT FOUND.'
               GOBACK
           END-IF
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE WS-IN-FROM TO DU-DATE-1
           MOVE WS-IN-TO   TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           IF NOT DU-OK OR DU-RESULT-DAYS < ZERO
              OR DU-RESULT-DAYS > 366 OR WS-IN-TO > WS-TODAY
               DISPLAY 'PERIOD MUST BE VALID, AT MOST ONE YEAR AND '
                       'NOT IN THE FUTURE.'
               GOBACK
           END-IF
           IF WS-IN-DELIVERY NOT = 'E' AND WS-IN-DELIVERY NOT = 'P'
               DISPLAY 'DELIVERY MUST BE E OR P.'
               GOBACK
           END-IF
           IF WS-IN-DELIVERY = 'E'
               MOVE WS-IN-ACCT-NO TO CLK-ACCT-NO
               MOVE 'N' TO CLK-CLOSE
               CALL 'CUSTLKUP' USING CLK-PARMS
               MOVE 'Y' TO CLK-CLOSE
               CALL 'CUSTLKUP' USING CLK-PARMS
               IF CLK-EMAIL = SPACES
                   DISPLAY 'NO E-MAIL ON RECORD - WILL BE POSTED.'
                   MOVE 'P' TO WS-IN-DELIVERY
               END-IF
           END-IF
           INITIALIZE SREQ-RECORD
           MOVE WS-IN-ACCT-NO  TO SR-ACCT-NO
           MOVE WS-IN-FROM     TO SR-FROM-DATE
           MOVE WS-IN-TO       TO SR-TO-DATE
           MOVE WS-IN-DELIVERY TO SR-DELIVERY
           MOVE WS-TODAY       TO SR-REQ-DATE
           MOVE SES-USER-ID    TO SR-REQ-BY
           OPEN EXTEND REQ-FILE
           IF WS-FS = '35'
               OPEN OUTPUT REQ-FILE
           END-IF
           WRITE SREQ-RECORD
           CLOSE REQ-FILE
           DISPLAY 'STATEMENT REQUEST QUEUED (' WS-IN-DELIVERY ').'
           GOBACK.
