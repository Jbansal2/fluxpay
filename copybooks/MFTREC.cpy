      *================================================================*
      * COPYBOOK : MFTREC                                              *
      * PURPOSE  : THIRD-PARTY PRODUCT TRANSACTION LOG (LINE           *
      *            SEQUENTIAL, APPEND ONLY). TYPE P PURCHASE, S SIP,   *
      *            R REDEMPTION, I PREMIUM, X SIP / PREMIUM MISSED.    *
      * LENGTH   : 100 BYTES                                           *
      *================================================================*
       01  MFT-RECORD.
           05  MT-DATE                 PIC 9(08).
           05  MT-ACCT-NO              PIC 9(10).
           05  MT-CODE                 PIC X(06).
           05  MT-TYPE                 PIC X(01).
           05  MT-AMOUNT               PIC 9(11)V99.
           05  MT-UNITS                PIC 9(09)V9(03).
           05  MT-NAV                  PIC 9(05)V9(04).
           05  MT-LOAD                 PIC 9(09)V99.
           05  MT-REF                  PIC X(12).
           05  MT-USER                 PIC X(08).
           05  FILLER                  PIC X(10).
