      *================================================================*
      * COPYBOOK : AUDREC                                              *
      * PURPOSE  : AUDIT TRAIL RECORD (LINE SEQUENTIAL, APPEND ONLY)   *
      *================================================================*
       01  AUD-RECORD.
           05  AUD-DATE                PIC 9(08).
           05  AUD-TIME                PIC 9(06).
           05  AUD-USER-ID             PIC X(08).
           05  AUD-ROLE                PIC X(01).
           05  AUD-PROGRAM             PIC X(08).
           05  AUD-ACTION              PIC X(12).
           05  AUD-KEY                 PIC X(16).
           05  AUD-DETAIL              PIC X(40).
