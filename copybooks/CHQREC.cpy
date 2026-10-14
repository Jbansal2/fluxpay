      *================================================================*
      * COPYBOOK : CHQREC                                              *
      * PURPOSE  : CHEQUE LEAF REGISTER (INDEXED). ONE RECORD PER      *
      *            LEAF ISSUED, KEY = ACCOUNT NO + CHEQUE NO.          *
      *================================================================*
       01  CHQ-RECORD.
           05  CHQ-KEY.
               10  CHQ-ACCT-NO         PIC 9(10).
               10  CHQ-NO              PIC 9(06).
           05  CHQ-BOOK-NO             PIC 9(04).
           05  CHQ-STATUS              PIC X(01).
               88  CHQ-UNUSED                    VALUE 'U'.
               88  CHQ-PAID                      VALUE 'P'.
               88  CHQ-RETURNED                  VALUE 'R'.
               88  CHQ-STOPPED                   VALUE 'S'.
           05  CHQ-ISSUE-DATE          PIC 9(08).
           05  CHQ-PRESENTED-DATE      PIC 9(08).
           05  CHQ-AMOUNT              PIC 9(09)V99.
           05  CHQ-PAYEE               PIC X(30).
           05  CHQ-REASON-CODE         PIC X(02).
           05  CHQ-STOP-REASON         PIC X(20).
           05  FILLER                  PIC X(20).
