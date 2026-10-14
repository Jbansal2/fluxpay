      *================================================================*
      * COPYBOOK : TFREC                                               *
      * PURPOSE  : TRADE FINANCE INSTRUMENT (INDEXED, KEY = TF-NO).    *
      *            G = BANK GUARANTEE, L = LETTER OF CREDIT. BOTH ARE  *
      *            OFF-BALANCE-SHEET UNTIL THE BANK ACTUALLY PAYS; THE *
      *            MARGIN THE CUSTOMER DEPOSITS IS HELD IN GL 2905.    *
      *================================================================*
       01  TF-RECORD.
           05  TF-NO                   PIC 9(08).
           05  TF-TYPE                 PIC X(01).
               88  TF-GUARANTEE                  VALUE 'G'.
               88  TF-LC                         VALUE 'L'.
           05  TF-ACCT-NO              PIC 9(10).
           05  TF-BENEFICIARY          PIC X(40).
           05  TF-AMOUNT               PIC 9(11)V99.
           05  TF-UTILISED             PIC 9(11)V99.
           05  TF-ISSUE-DATE           PIC 9(08).
           05  TF-EXPIRY-DATE          PIC 9(08).
      *    BG: CLAIMS ACCEPTED UNTIL THIS DATE (EXPIRY + CLAIM PERIOD)
           05  TF-CLAIM-END            PIC 9(08).
           05  TF-MARGIN-PCT           PIC 9(03).
           05  TF-MARGIN-HELD          PIC 9(11)V99.
           05  TF-COMMISSION           PIC 9(09)V99.
           05  TF-STATUS               PIC X(01).
               88  TF-ACTIVE                     VALUE 'A'.
               88  TF-INVOKED                    VALUE 'I'.
               88  TF-FULLY-DRAWN                VALUE 'U'.
               88  TF-EXPIRED                    VALUE 'E'.
               88  TF-CANCELLED                  VALUE 'C'.
               88  TF-LIVE                       VALUE 'A'.
           05  TF-ISSUED-BY            PIC X(08).
           05  TF-CLOSE-DATE           PIC 9(08).
           05  FILLER                  PIC X(10).
