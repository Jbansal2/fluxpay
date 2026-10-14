      *================================================================*
      * COPYBOOK : MFHREC                                              *
      * PURPOSE  : CUSTOMER HOLDING OF A THIRD-PARTY PRODUCT (INDEXED, *
      *            KEY = ACCOUNT + PRODUCT CODE). FOR A FUND: UNITS    *
      *            AND COST. FOR INSURANCE: THE POLICY AND ITS PREMIUM *
      *            SCHEDULE.                                           *
      * LENGTH   : 140 BYTES                                           *
      *================================================================*
       01  MFH-RECORD.
           05  MH-KEY.
               10  MH-ACCT-NO          PIC 9(10).
               10  MH-CODE             PIC X(06).
           05  MH-STATUS               PIC X(01).
               88  MH-ACTIVE                     VALUE 'A'.
               88  MH-LAPSED                     VALUE 'L'.
               88  MH-CLOSED                     VALUE 'C'.
           05  MH-UNITS                PIC 9(09)V9(03).
           05  MH-COST                 PIC 9(11)V99.
           05  MH-FIRST-DATE           PIC 9(08).
           05  MH-LAST-TXN-DATE        PIC 9(08).
           05  MH-POLICY-NO            PIC X(12).
           05  MH-SUM-ASSURED          PIC 9(11)V99.
           05  MH-PREMIUM              PIC 9(09)V99.
           05  MH-FREQUENCY            PIC X(01).
               88  MH-FREQ-VALID                 VALUES 'Y' 'H' 'Q'
                                                 'M'.
           05  MH-NEXT-DUE             PIC 9(08).
           05  MH-PREMIUMS-PAID        PIC 9(03).
           05  MH-REDEEMED-VALUE       PIC 9(11)V99.
           05  FILLER                  PIC X(21).
