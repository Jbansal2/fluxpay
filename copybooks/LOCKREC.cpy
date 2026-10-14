      *================================================================*
      * COPYBOOK : LOCKREC                                             *
      * PURPOSE  : SAFE DEPOSIT LOCKER MASTER (INDEXED, KEY = LOCK-NO) *
      *================================================================*
       01  LOCK-RECORD.
           05  LOCK-NO                 PIC 9(05).
           05  LOCK-BRANCH             PIC X(06).
           05  LOCK-SIZE               PIC X(01).
               88  LOCK-SMALL                    VALUE 'S'.
               88  LOCK-MEDIUM                   VALUE 'M'.
               88  LOCK-LARGE                    VALUE 'L'.
               88  LOCK-SIZE-VALID               VALUES 'S' 'M' 'L'.
           05  LOCK-STATUS             PIC X(01).
               88  LOCK-VACANT                   VALUE 'V'.
               88  LOCK-ALLOTTED                 VALUE 'A'.
               88  LOCK-OUT-OF-SERVICE           VALUE 'X'.
           05  LOCK-CUST-ID            PIC 9(08).
           05  LOCK-RENT-ACCT          PIC 9(10).
           05  LOCK-ALLOT-DATE         PIC 9(08).
           05  LOCK-PAID-UNTIL         PIC 9(08).
           05  LOCK-KEY-NO             PIC X(08).
           05  LOCK-VISITS             PIC 9(05).
           05  LOCK-LAST-VISIT         PIC 9(08).
           05  FILLER                  PIC X(20).
