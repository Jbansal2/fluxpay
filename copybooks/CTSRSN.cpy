      *================================================================*
      * COPYBOOK : CTSRSN                                              *
      * PURPOSE  : CHEQUE RETURN REASONS (NPCI CTS CODES, SUBSET)      *
      *================================================================*
       01  CTS-REASON-TABLE.
           05  FILLER PIC X(32) VALUE '01FUNDS INSUFFICIENT          '.
           05  FILLER PIC X(32) VALUE '02EXCEEDS ARRANGEMENT         '.
           05  FILLER PIC X(32) VALUE '04CHEQUE STALE                '.
           05  FILLER PIC X(32) VALUE '05CHEQUE POST-DATED           '.
           05  FILLER PIC X(32) VALUE '12PAYMENT STOPPED BY DRAWER   '.
           05  FILLER PIC X(32) VALUE '21ACCOUNT CLOSED              '.
           05  FILLER PIC X(32) VALUE '22SIGNATURE DIFFERS           '.
           05  FILLER PIC X(32) VALUE '31AMOUNT IN WORDS/FIGURES DIFF'.
       01  CTS-REASON-R REDEFINES CTS-REASON-TABLE.
           05  CTS-REASON OCCURS 8 TIMES INDEXED BY CRS-IDX.
               10  CRS-CODE            PIC 9(02).
               10  CRS-TEXT            PIC X(30).
