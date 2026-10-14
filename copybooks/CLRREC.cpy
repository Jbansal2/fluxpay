      *================================================================*
      * COPYBOOK : CLRREC                                              *
      * PURPOSE  : INWARD CLEARING RECORD - ONE CHEQUE PRESENTED ON    *
      *            US BY ANOTHER BANK (FIXED 72-BYTE TEXT LINE).       *
      *            THE SAME LAYOUT, WITH THE REASON FILLED IN, IS      *
      *            USED FOR THE OUTWARD RETURN FILE.                   *
      *================================================================*
       01  CLR-RECORD.
           05  CLR-SEQ                 PIC 9(06).
           05  CLR-ACCT-NO             PIC 9(10).
           05  CLR-CHQ-NO              PIC 9(06).
           05  CLR-AMOUNT              PIC 9(09)V99.
           05  CLR-CHQ-DATE            PIC 9(08).
           05  CLR-PAYEE               PIC X(25).
           05  CLR-REASON-CODE         PIC X(02).
           05  CLR-RESULT              PIC X(01).
               88  CLR-PAID                      VALUE 'P'.
               88  CLR-RETURN                    VALUE 'R'.
               88  CLR-UNPROCESSED               VALUE SPACE.
           05  FILLER                  PIC X(03).
