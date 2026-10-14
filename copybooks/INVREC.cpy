      *================================================================*
      * COPYBOOK : INVREC                                              *
      * PURPOSE  : BRANCH STOCK OF SECURITY STATIONERY (INDEXED, KEY = *
      *            BRANCH + ITEM). ITEMS: CB10 / CB25 CHEQUE BOOKS,    *
      *            DCRD DEBIT CARD BLANKS, PINM PIN MAILERS, CCRD      *
      *            CREDIT CARD BLANKS.                                 *
      * LENGTH   : 100 BYTES                                           *
      *================================================================*
       01  INV-RECORD.
           05  INV-KEY.
               10  INV-BRANCH          PIC X(06).
               10  INV-ITEM            PIC X(04).
           05  INV-DESC                PIC X(30).
           05  INV-ON-HAND             PIC 9(07).
           05  INV-REORDER-LEVEL       PIC 9(07).
           05  INV-REORDER-QTY         PIC 9(07).
      *    ORDERED FROM THE PRINTER / CARD BUREAU, NOT YET RECEIVED
           05  INV-ON-ORDER            PIC 9(07).
           05  INV-LAST-RECEIPT        PIC 9(08).
           05  INV-LAST-ISSUE          PIC 9(08).
           05  INV-ISSUED-TOTAL        PIC 9(07).
           05  FILLER                  PIC X(09).
