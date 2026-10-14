      *================================================================*
      * COPYBOOK : FXDFLT                                              *
      * PURPOSE  : STARTING CARD RATES, USED TO SEED FXRATE ON FIRST   *
      *            USE. ILLUSTRATIVE ONLY - TREASURY SETS THE REAL     *
      *            RATES EVERY MORNING (FXMNT OPTION 2).               *
      *            CCY NAME(20) BUY 999V9999 SELL 999V9999 PER-USD     *
      *================================================================*
       01  FXD-TABLE.
           05  FILLER PIC X(44) VALUE
               'USDUS DOLLAR           083100008430000010000'.
           05  FILLER PIC X(44) VALUE
               'EUREURO                089800009130000009200'.
           05  FILLER PIC X(44) VALUE
               'GBPPOUND STERLING      105200010690000007900'.
           05  FILLER PIC X(44) VALUE
               'AEDUAE DIRHAM          022500002310000036725'.
           05  FILLER PIC X(44) VALUE
               'SGDSINGAPORE DOLLAR    061500006280000013400'.
       01  FXD-TABLE-R REDEFINES FXD-TABLE.
           05  FXD-ENTRY OCCURS 5 TIMES INDEXED BY FXD-IDX.
               10  FXD-CCY             PIC X(03).
               10  FXD-NAME            PIC X(20).
               10  FXD-BUY             PIC 9(03)V9(04).
               10  FXD-SELL            PIC 9(03)V9(04).
               10  FXD-PER-USD         PIC 9(03)V9(04).
