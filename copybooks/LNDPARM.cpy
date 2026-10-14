      *================================================================*
      * COPYBOOK : LNDPARM                                             *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'LNDISB' - BOOK AND        *
      *            DISBURSE ONE TERM LOAN THAT HAS ALREADY BEEN        *
      *            PRICED AND APPROVED BY THE CALLER.                  *
      *================================================================*
       01  LND-PARMS.
           05  LND-ACCT-NO             PIC 9(10).
           05  LND-TYPE                PIC X(01).
           05  LND-AMOUNT              PIC 9(11)V99.
           05  LND-TENURE              PIC 9(03).
           05  LND-RATE                PIC 9(02)V9(04).
           05  LND-EMI                 PIC 9(09)V99.
      *    F = FIXED, R = REPO-LINKED (SPREAD OVER REPO)
           05  LND-RATE-TYPE           PIC X(01).
           05  LND-SPREAD              PIC 9(02)V9(04).
           05  LND-NARRATION           PIC X(30).
      *    RETURNED
           05  LND-LOAN-NO             PIC 9(10).
           05  LND-FIRST-DUE           PIC 9(08).
           05  LND-RC                  PIC 9(02).
               88  LND-OK                        VALUE 00.
               88  LND-CREDIT-FAILED             VALUE 10.
               88  LND-WRITE-FAILED              VALUE 20.
               88  LND-FILE-ERROR                VALUE 90.
           05  LND-POST-RC             PIC 9(02).
