      *================================================================*
      * COPYBOOK : RATEDFLT                                            *
      * PURPOSE  : BUILT-IN STARTING RATES (USED UNTIL THE RATE MASTER *
      *            HAS A ROW). CODE(8) VALUE 99V9999 DESCRIPTION(30)   *
      *================================================================*
       01  RDF-TABLE.
           05  FILLER PIC X(44) VALUE
               'REPO    065000RBI POLICY REPO RATE          '.
           05  FILLER PIC X(44) VALUE
               'HLSPREAD020000HOME LOAN SPREAD OVER REPO    '.
           05  FILLER PIC X(44) VALUE
               'SAVINGS 030000SAVINGS BANK RATE             '.
           05  FILLER PIC X(44) VALUE
               'FD1Y    068000FIXED DEPOSIT 1 YEAR          '.
           05  FILLER PIC X(44) VALUE
               'PERSONAL135000PERSONAL LOAN (FIXED)         '.
           05  FILLER PIC X(44) VALUE
               'VEHICLE 095000VEHICLE LOAN (FIXED)          '.
           05  FILLER PIC X(44) VALUE
               'CRR     030000CASH RESERVE RATIO (% OF NDTL)'.
           05  FILLER PIC X(44) VALUE
               'SLR     180000STAT. LIQUIDITY RATIO (% NDTL)'.
       01  RDF-TABLE-R REDEFINES RDF-TABLE.
           05  RDF-ENTRY OCCURS 8 TIMES INDEXED BY RDF-IDX.
               10  RDF-CODE            PIC X(08).
               10  RDF-VALUE           PIC 9(02)V9(04).
               10  RDF-DESC            PIC X(30).
