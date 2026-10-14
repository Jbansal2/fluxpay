      *================================================================*
      * COPYBOOK : AMLDREC                                             *
      * PURPOSE  : AML DAILY TALLY PER ACCOUNT AND DAY (INDEXED)       *
      *================================================================*
       01  AMLDAY-RECORD.
           05  AD-KEY.
               10  AD-ACCT-NO          PIC 9(10).
               10  AD-DATE             PIC 9(08).
           05  AD-CASH-IN              PIC 9(13)V99.
           05  AD-CASH-OUT             PIC 9(13)V99.
           05  AD-ALL-CREDITS          PIC 9(13)V99.
           05  AD-ALL-DEBITS           PIC 9(13)V99.
           05  AD-NEAR-LIMIT-CNT       PIC 9(03).
           05  AD-FLAGS.
               10  AD-CTR-IN-RAISED    PIC X(01).
               10  AD-CTR-OUT-RAISED   PIC X(01).
               10  AD-STRUCT-RAISED    PIC X(01).
               10  AD-PASSTHRU-RAISED  PIC X(01).
           05  FILLER                  PIC X(10).
