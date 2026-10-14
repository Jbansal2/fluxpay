      *================================================================*
      * COPYBOOK : CARDREC                                             *
      * PURPOSE  : DEBIT CARD MASTER (INDEXED, KEY = CARD-NO)          *
      * NOTE     : PIN IS NEVER STORED - ONLY A ONE-WAY HASH. A REAL   *
      *            BANK WOULD USE AN HSM WITH PIN BLOCKS (ISO 9564).   *
      *================================================================*
       01  CARD-RECORD.
           05  CARD-NO                 PIC 9(16).
           05  CARD-ACCT-NO            PIC 9(10).
           05  CARD-HOLDER-NAME        PIC X(26).
           05  CARD-STATUS             PIC X(01).
               88  CARD-ACTIVE                   VALUE 'A'.
               88  CARD-PIN-LOCKED               VALUE 'L'.
               88  CARD-HOTLISTED                VALUE 'H'.
               88  CARD-EXPIRED                  VALUE 'E'.
           05  CARD-PIN-HASH           PIC 9(09).
           05  CARD-WRONG-PIN-CNT      PIC 9(01).
           05  CARD-EXPIRY-YYYYMM      PIC 9(06).
           05  CARD-DAILY-LIMIT        PIC 9(07)V99.
           05  CARD-USED-TODAY         PIC 9(07)V99.
           05  CARD-LAST-USE-DATE      PIC 9(08).
           05  CARD-ISSUE-DATE         PIC 9(08).
           05  CARD-HOTLIST-REASON     PIC X(20).
           05  FILLER                  PIC X(30).
