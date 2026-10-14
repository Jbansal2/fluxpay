      *================================================================*
      * COPYBOOK : LNPROD                                              *
      * PURPOSE  : TERM LOAN PRODUCTS - RATE, MAXIMUM AMOUNT, MAXIMUM  *
      *            TENURE. HOME LOANS ARE REPRICED OFF REPO (RATE      *
      *            MASTER); THE RATE HERE IS THEIR STARTING POINT.     *
      *            USED BY LOANMNT (DIRECT SANCTION) AND LOANAPP.      *
      *================================================================*
       01  WS-PRODUCT-TABLE.
      *        TYPE RATE   MAX AMOUNT (11V99) TENURE
           05  FILLER PIC X(23) VALUE 'H0850000005000000000360'.
           05  FILLER PIC X(23) VALUE 'P1350000000150000000060'.
           05  FILLER PIC X(23) VALUE 'V0950000000200000000084'.
       01  WS-PRODUCT-R REDEFINES WS-PRODUCT-TABLE.
           05  WS-PRODUCT OCCURS 3 TIMES INDEXED BY PRD-IDX.
               10  PRD-TYPE            PIC X(01).
               10  PRD-RATE            PIC 9(02)V9(04).
               10  PRD-MAX-AMOUNT      PIC 9(11)V99.
               10  PRD-MAX-TENURE      PIC 9(03).
