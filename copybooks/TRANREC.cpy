      *================================================================*
      * COPYBOOK : TRANREC                                             *
      * PURPOSE  : TRANSACTION LOG RECORD (SEQUENTIAL, APPEND ONLY)    *
      *================================================================*
       01  TRAN-RECORD.
           05  TRAN-ID                 PIC 9(12).
           05  TRAN-ACCT-NO            PIC 9(10).
           05  TRAN-DATE               PIC 9(08).
           05  TRAN-TIME               PIC 9(06).
           05  TRAN-TYPE               PIC X(02).
               88  TRAN-DEPOSIT                  VALUE 'DP'.
               88  TRAN-WITHDRAW                 VALUE 'WD'.
               88  TRAN-XFER-OUT                 VALUE 'TO'.
               88  TRAN-XFER-IN                  VALUE 'TI'.
               88  TRAN-INTEREST                 VALUE 'IN'.
               88  TRAN-LOAN-DISB                VALUE 'LD'.
               88  TRAN-LOAN-REPAY               VALUE 'LR'.
               88  TRAN-ATM-WITHDRAW             VALUE 'AW'.
               88  TRAN-DEP-OPEN                 VALUE 'FO'.
               88  TRAN-DEP-MATURITY             VALUE 'FM'.
               88  TRAN-RD-INSTALMENT            VALUE 'RI'.
               88  TRAN-CHARGES                  VALUE 'CH'.
               88  TRAN-CHEQUE-PAID              VALUE 'CQ'.
               88  TRAN-PAYMENT-OUT              VALUE 'PO'.
               88  TRAN-PAYMENT-IN               VALUE 'PI'.
               88  TRAN-OD-INTEREST              VALUE 'OI'.
               88  TRAN-CARD-PAYMENT             VALUE 'CP'.
               88  TRAN-NACH-DEBIT               VALUE 'ND'.
               88  TRAN-OUTWARD-CHQ              VALUE 'OC'.
               88  TRAN-TRADE-DEBIT              VALUE 'TF'.
               88  TRAN-TRADE-REFUND             VALUE 'TR'.
               88  TRAN-FX-INWARD                VALUE 'XI'.
               88  TRAN-FX-OUTWARD               VALUE 'XO'.
               88  TRAN-CREDIT-TYPE              VALUES 'DP' 'TI'
                                                 'IN' 'LD' 'FM' 'PI'
                                                 'OC' 'TR' 'XI'.
           05  TRAN-AMOUNT             PIC 9(11)V99.
           05  TRAN-BAL-AFTER          PIC S9(13)V99.
           05  TRAN-REF-ACCT           PIC 9(10).
           05  TRAN-NARRATION          PIC X(30).
