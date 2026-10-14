      *================================================================*
      * COPYBOOK : FDRTREC                                             *
      * PURPOSE  : VERSIONED FD/RD RATE CARD (INDEXED, KEY = EFFECTIVE *
      *            DATE + SLAB UPPER BOUND). A NEW CARD IS A NEW SET   *
      *            OF ROWS WITH A LATER EFFECTIVE DATE; OLD ROWS ARE   *
      *            NEVER CHANGED, SO A DEPOSIT KEEPS THE RATE OF THE   *
      *            CARD IN FORCE ON THE DAY IT WAS BOOKED.             *
      * LENGTH   : 49 BYTES                                            *
      *================================================================*
       01  FDRT-RECORD.
           05  FR-KEY.
               10  FR-EFF-DATE         PIC 9(08).
               10  FR-MAX-MONTHS       PIC 9(03).
           05  FR-GEN-RATE             PIC 9(02)V9(04).
           05  FR-SNR-RATE             PIC 9(02)V9(04).
           05  FR-SET-BY               PIC X(08).
           05  FR-SET-DATE             PIC 9(08).
           05  FILLER                  PIC X(10).
