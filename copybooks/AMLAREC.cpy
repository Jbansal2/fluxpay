      *================================================================*
      * COPYBOOK : AMLAREC                                             *
      * PURPOSE  : AML ALERT (LINE SEQUENTIAL, APPEND ONLY)            *
      *            CTR = CASH TRANSACTION REPORT (THRESHOLD BREACH)    *
      *            STR = SUSPICIOUS TRANSACTION, NEEDS REVIEW          *
      *================================================================*
       01  ALERT-RECORD.
           05  AL-DATE                 PIC 9(08).
           05  AL-TIME                 PIC 9(06).
           05  AL-KIND                 PIC X(03).
               88  AL-IS-CTR                     VALUE 'CTR'.
               88  AL-IS-STR                     VALUE 'STR'.
           05  AL-RULE                 PIC X(12).
           05  AL-ACCT-NO              PIC 9(10).
           05  AL-AMOUNT               PIC 9(13)V99.
           05  AL-NAME                 PIC X(40).
