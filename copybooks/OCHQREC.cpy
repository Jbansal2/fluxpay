      *================================================================*
      * COPYBOOK : OCHQREC                                             *
      * PURPOSE  : OUTWARD CHEQUE REGISTER (INDEXED, KEY = OC-ID).     *
      *            A CHEQUE DRAWN ON ANOTHER BANK, DEPOSITED BY OUR    *
      *            CUSTOMER AND SENT FOR COLLECTION THROUGH THE        *
      *            CHEQUE TRUNCATION SYSTEM (CTS).                     *
      *================================================================*
       01  OCHQ-RECORD.
           05  OC-ID                   PIC 9(08).
           05  OC-ACCT-NO              PIC 9(10).
           05  OC-DRAWEE-IFSC          PIC X(11).
           05  OC-CHEQUE-NO            PIC 9(06).
           05  OC-CHEQUE-DATE          PIC 9(08).
           05  OC-AMOUNT               PIC 9(11)V99.
           05  OC-DRAWER-NAME          PIC X(30).
      *    IMAGE CAPTURED AT THE COUNTER (CTS SENDS IMAGES, NOT PAPER)
           05  OC-IMAGE-REF            PIC X(20).
           05  OC-STATUS               PIC X(01).
               88  OC-LODGED                     VALUE 'L'.
               88  OC-SENT                       VALUE 'S'.
               88  OC-CLEARED                    VALUE 'C'.
               88  OC-RETURNED                   VALUE 'R'.
           05  OC-LODGE-DATE           PIC 9(08).
           05  OC-LODGED-BY            PIC X(08).
           05  OC-BATCH-DATE           PIC 9(08).
           05  OC-RESULT-DATE          PIC 9(08).
           05  OC-RETURN-REASON        PIC 9(02).
           05  OC-RETURN-TEXT          PIC X(30).
           05  FILLER                  PIC X(10).
