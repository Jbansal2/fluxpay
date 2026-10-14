      *================================================================*
      * COPYBOOK : SWPREC                                              *
      * PURPOSE  : AUTO-SWEEP MANDATE (INDEXED, KEY = ACCOUNT NO).     *
      *            SWEEP-OUT: BALANCE ABOVE THRESHOLD GOES INTO FDS OF *
      *            SW-UNIT EACH. SWEEP-IN: BALANCE BELOW FLOOR IS      *
      *            REFILLED BY BREAKING THE NEWEST SWEEP FDS FIRST.    *
      * LENGTH   : 120 BYTES                                           *
      *================================================================*
       01  SWP-RECORD.
           05  SW-ACCT-NO              PIC 9(10).
           05  SW-STATUS               PIC X(01).
               88  SW-ACTIVE                     VALUE 'A'.
               88  SW-CANCELLED                  VALUE 'C'.
           05  SW-THRESHOLD            PIC 9(11)V99.
           05  SW-FLOOR                PIC 9(11)V99.
           05  SW-UNIT                 PIC 9(11)V99.
           05  SW-TENURE               PIC 9(03).
           05  SW-CREATED-DATE         PIC 9(08).
           05  SW-CREATED-BY           PIC X(08).
           05  SW-LAST-RUN-DATE        PIC 9(08).
           05  SW-OUT-COUNT            PIC 9(05).
           05  SW-OUT-AMOUNT           PIC 9(13)V99.
           05  SW-IN-COUNT             PIC 9(05).
           05  SW-IN-AMOUNT            PIC 9(13)V99.
           05  FILLER                  PIC X(03).
