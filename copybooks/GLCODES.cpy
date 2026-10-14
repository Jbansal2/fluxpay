      *================================================================*
      * COPYBOOK : GLCODES                                             *
      * PURPOSE  : NAMED GL CODES SO PROGRAMS NEVER HARD-CODE NUMBERS. *
      *            THE FULL CHART OF ACCOUNTS (NAMES, TYPES) IS SEEDED *
      *            BY GLPOST THE FIRST TIME THE LEDGER IS OPENED.      *
      *================================================================*
       01  GL-CODES.
           05  GLC-CASH                PIC 9(04) VALUE 1001.
           05  GLC-ATM-CASH            PIC 9(04) VALUE 1002.
           05  GLC-LOANS               PIC 9(04) VALUE 1201.
           05  GLC-LOAN-REPAY-SUSP     PIC 9(04) VALUE 1290.
           05  GLC-CLEARING            PIC 9(04) VALUE 1301.
           05  GLC-PAYMENT-SETTLE      PIC 9(04) VALUE 1302.
           05  GLC-CARD-RECEIVABLE     PIC 9(04) VALUE 1210.
           05  GLC-GOLD-LOANS          PIC 9(04) VALUE 1220.
           05  GLC-DEPOSIT-LOANS       PIC 9(04) VALUE 1230.
           05  GLC-MERCHANT-SETTLE     PIC 9(04) VALUE 2903.
           05  GLC-CASH-SHORTAGE       PIC 9(04) VALUE 1295.
           05  GLC-CURRENCY-CHEST      PIC 9(04) VALUE 1303.
           05  GLC-NACH-SETTLE         PIC 9(04) VALUE 1304.
           05  GLC-CASH-EXCESS         PIC 9(04) VALUE 2904.
           05  GLC-NOSTRO              PIC 9(04) VALUE 1305.
           05  GLC-INT-ACCRUED         PIC 9(04) VALUE 1240.
           05  GLC-LOAN-PROVISION      PIC 9(04) VALUE 1299.
           05  GLC-MARGIN-MONEY        PIC 9(04) VALUE 2905.
           05  GLC-TCS-PAYABLE         PIC 9(04) VALUE 2503.
           05  GLC-BAD-DEBT-RECOVERED  PIC 9(04) VALUE 4103.
           05  GLC-EXCHANGE-INCOME     PIC 9(04) VALUE 4104.
           05  GLC-TRADE-COMMISSION    PIC 9(04) VALUE 4105.
           05  GLC-PROVISION-EXPENSE   PIC 9(04) VALUE 5101.
           05  GLC-CARD-INT-INCOME     PIC 9(04) VALUE 4003.
           05  GLC-SAVINGS-DEP         PIC 9(04) VALUE 2101.
           05  GLC-CURRENT-DEP         PIC 9(04) VALUE 2102.
           05  GLC-TERM-DEP            PIC 9(04) VALUE 2201.
           05  GLC-GST-PAYABLE         PIC 9(04) VALUE 2501.
           05  GLC-TDS-PAYABLE         PIC 9(04) VALUE 2502.
           05  GLC-TRANSFER-SUSP       PIC 9(04) VALUE 2900.
           05  GLC-OPENING-CONTROL     PIC 9(04) VALUE 3001.
           05  GLC-LOAN-INT-INCOME     PIC 9(04) VALUE 4001.
           05  GLC-OD-INT-INCOME       PIC 9(04) VALUE 4002.
           05  GLC-FEE-INCOME          PIC 9(04) VALUE 4101.
           05  GLC-LOCKER-INCOME       PIC 9(04) VALUE 4102.
           05  GLC-SAV-INT-EXPENSE     PIC 9(04) VALUE 5001.
           05  GLC-TD-INT-EXPENSE      PIC 9(04) VALUE 5002.
           05  GLC-INTER-BRANCH        PIC 9(04) VALUE 1310.
           05  GLC-MF-COLLECTIONS      PIC 9(04) VALUE 2906.
           05  GLC-GENERAL-RESERVE     PIC 9(04) VALUE 3002.
           05  GLC-PROFIT-AND-LOSS     PIC 9(04) VALUE 3003.
           05  GLC-STATUTORY-RESERVE   PIC 9(04) VALUE 3004.
           05  GLC-MF-COMMISSION       PIC 9(04) VALUE 4106.
           05  GLC-COMPENSATION-EXP    PIC 9(04) VALUE 5102.
