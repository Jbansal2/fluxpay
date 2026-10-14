      *================================================================*
      * COPYBOOK : COLLREC                                             *
      * PURPOSE  : COLLECTIONS FOLLOW-UP LOG (LINE SEQUENTIAL, APPEND) *
      *            ACTION: C CALL, V VISIT, L LETTER, P PROMISE TO     *
      *            PAY, S SETTLED, R REMINDER SENT BY COLLBAT          *
      *================================================================*
       01  COLL-RECORD.
           05  CL-LOAN-NO              PIC 9(10).
           05  CL-DATE                 PIC 9(08).
           05  CL-TIME                 PIC 9(06).
           05  CL-ACTION               PIC X(01).
           05  CL-PTP-DATE             PIC 9(08).
           05  CL-PTP-AMOUNT           PIC 9(11)V99.
           05  CL-REMARKS              PIC X(40).
           05  CL-USER                 PIC X(08).
