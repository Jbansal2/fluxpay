      *================================================================*
      * COPYBOOK : LNPPARM                                             *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'LNEMIPAY' USING LNP-PARMS *
      *            LOAN-RECORD. THE CALLER OWNS THE LOAN FILE AND      *
      *            REWRITES THE RECORD AFTER A SUCCESSFUL PAYMENT.     *
      *   QE / PE - QUOTE / PAY THE NEXT EMI (PLUS ANY PENALTY)        *
      *   QF / PF - QUOTE / PAY FULL FORECLOSURE                       *
      *================================================================*
       01  LNP-PARMS.
           05  LNP-MODE                PIC X(02).
               88  LNP-QUOTE-EMI                 VALUE 'QE'.
               88  LNP-PAY-EMI                   VALUE 'PE'.
               88  LNP-QUOTE-FORECLOSE           VALUE 'QF'.
               88  LNP-PAY-FORECLOSE             VALUE 'PF'.
               88  LNP-IS-PAYMENT                VALUES 'PE' 'PF'.
           05  LNP-NARRATION           PIC X(30).
           05  LNP-PRINCIPAL           PIC 9(11)V99.
           05  LNP-INTEREST            PIC 9(09)V99.
           05  LNP-PENALTY             PIC 9(09)V99.
           05  LNP-CHARGE              PIC 9(09)V99.
           05  LNP-AMOUNT-DUE          PIC 9(11)V99.
           05  LNP-AVAILABLE           PIC S9(13)V99.
           05  LNP-RC                  PIC 9(02).
               88  LNP-OK                        VALUE 00.
               88  LNP-LOAN-CLOSED               VALUE 10.
               88  LNP-ACCT-PROBLEM              VALUE 20.
               88  LNP-INSUFFICIENT              VALUE 30.
               88  LNP-BAD-MODE                  VALUE 40.
