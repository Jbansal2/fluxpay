      *================================================================*
      * COPYBOOK : DENOMS                                              *
      * PURPOSE  : NOTE DENOMINATIONS IN THE ORDER USED BY CASHREC     *
      *================================================================*
       01  DENOM-TABLE.
           05  FILLER                  PIC 9(03) VALUE 500.
           05  FILLER                  PIC 9(03) VALUE 200.
           05  FILLER                  PIC 9(03) VALUE 100.
           05  FILLER                  PIC 9(03) VALUE 050.
           05  FILLER                  PIC 9(03) VALUE 020.
           05  FILLER                  PIC 9(03) VALUE 010.
       01  DENOM-TABLE-R REDEFINES DENOM-TABLE.
           05  DENOM-VALUE             PIC 9(03) OCCURS 6 TIMES.
