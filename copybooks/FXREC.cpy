      *================================================================*
      * COPYBOOK : FXREC                                               *
      * PURPOSE  : FOREX CARD RATES (INDEXED, KEY = FX-CCY) AND THE    *
      *            REMITTANCE LOG (LINE SEQUENTIAL, APPEND ONLY).      *
      *            BUY = BANK BUYS FOREIGN CURRENCY (INWARD), SELL =   *
      *            BANK SELLS (OUTWARD); MID IS THEIR AVERAGE.         *
      *================================================================*
       01  FX-RECORD.
           05  FX-CCY                  PIC X(03).
           05  FX-NAME                 PIC X(20).
           05  FX-BUY                  PIC 9(03)V9(04).
           05  FX-SELL                 PIC 9(03)V9(04).
      *    UNITS OF THE CURRENCY IN ONE US DOLLAR (FOR LRS LIMITS)
           05  FX-PER-USD              PIC 9(03)V9(04).
           05  FX-SET-DATE             PIC 9(08).
           05  FX-SET-BY               PIC X(08).
           05  FILLER                  PIC X(10).
