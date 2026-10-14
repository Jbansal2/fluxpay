      *================================================================*
      * COPYBOOK : CASHREC                                             *
      * PURPOSE  : BRANCH CASH POSITION (INDEXED, KEY = CP-KEY).       *
      *            HOLDER 'VAULT' (DATE ZERO) IS THE BRANCH STRONG     *
      *            ROOM; ANY OTHER HOLDER IS A TELLER DRAWER FOR ONE   *
      *            BUSINESS DATE.                                      *
      *            NOTES ARE KEPT AS PIECES PER DENOMINATION (500,     *
      *            200, 100, 50, 20, 10) PLUS COINS AS AN AMOUNT.      *
      *================================================================*
       01  CASH-RECORD.
           05  CP-KEY.
               10  CP-BRANCH           PIC X(06).
               10  CP-HOLDER           PIC X(08).
               10  CP-DATE             PIC 9(08).
           05  CP-STATUS               PIC X(01).
               88  CP-OPEN                       VALUE 'O'.
               88  CP-TALLIED                    VALUE 'T'.
               88  CP-DIFFERENCE                 VALUE 'D'.
      *    VAULT: BOOK STOCK. DRAWER: PIECES DECLARED AT CLOSING.
           05  CP-PIECES               PIC 9(07) OCCURS 6 TIMES.
           05  CP-COINS                PIC 9(09)V99.
      *    DRAWER MOVEMENTS FOR THE DAY
           05  CP-OPENING              PIC 9(11)V99.
           05  CP-RECEIPTS             PIC 9(11)V99.
           05  CP-PAYMENTS             PIC 9(11)V99.
           05  CP-RECEIPT-CNT          PIC 9(05).
           05  CP-PAYMENT-CNT          PIC 9(05).
           05  CP-PHYSICAL             PIC 9(11)V99.
           05  CP-DIFF                 PIC S9(11)V99.
           05  CP-OPENED-BY            PIC X(08).
           05  CP-CLOSE-TIME           PIC 9(06).
           05  FILLER                  PIC X(10).
