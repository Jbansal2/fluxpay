      *================================================================*
      * COPYBOOK : RATEREC                                             *
      * PURPOSE  : INTEREST RATE MASTER (INDEXED, KEY = RATE-CODE).    *
      *            ONE ROW PER BENCHMARK OR CARD RATE, E.G. REPO,      *
      *            SAVINGS, FD-1Y. THE PREVIOUS VALUE IS KEPT SO A     *
      *            WRONG CHANGE CAN BE SEEN (AND UNDONE) AT ONCE.      *
      *================================================================*
       01  RATE-RECORD.
           05  RATE-CODE               PIC X(08).
           05  RATE-DESC               PIC X(30).
           05  RATE-VALUE              PIC 9(02)V9(04).
           05  RATE-PREV-VALUE         PIC 9(02)V9(04).
           05  RATE-EFF-DATE           PIC 9(08).
           05  RATE-CHANGED-BY         PIC X(08).
           05  FILLER                  PIC X(14).
