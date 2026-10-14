      *================================================================*
      * COPYBOOK : SREQREC                                             *
      * PURPOSE  : STATEMENT REQUEST (LINE SEQUENTIAL QUEUE, STMTREQ). *
      *            PICKED UP BY THE NEXT STMTBAT RUN.                  *
      *================================================================*
       01  SREQ-RECORD.
           05  SR-ACCT-NO              PIC 9(10).
           05  SR-FROM-DATE            PIC 9(08).
           05  SR-TO-DATE              PIC 9(08).
      *    E = E-MAIL, P = PRINT AND POST
           05  SR-DELIVERY             PIC X(01).
           05  SR-REQ-DATE             PIC 9(08).
           05  SR-REQ-BY               PIC X(08).
