      *================================================================*
      * COPYBOOK : CCPPARM                                             *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'CCPOST' USING CCP-PARMS   *
      *            CC-RECORD. THE CALLER OWNS THE CARD FILE AND        *
      *            REWRITES THE RECORD WHEN CCP-OK.                    *
      *================================================================*
       01  CCP-PARMS.
           05  CCP-TYPE                PIC X(01).
           05  CCP-AMOUNT              PIC 9(09)V99.
           05  CCP-MERCHANT            PIC X(25).
      *    PAYMENTS ONLY: 'Y' WHEN THE MONEY ALREADY CAME THROUGH
      *    ACCTPOST (TYPE CP, WHICH POSTS DR CUSTOMER / CR 1210);
      *    OTHERWISE IT IS CASH AT THE COUNTER (DR 1001 / CR 1210)
           05  CCP-FROM-ACCOUNT        PIC X(01).
           05  CCP-RUN-DATE            PIC 9(08).
           05  CCP-AUTH-CODE           PIC X(06).
           05  CCP-AVAILABLE           PIC S9(11)V99.
           05  CCP-RC                  PIC 9(02).
               88  CCP-OK                        VALUE 00.
               88  CCP-CARD-BLOCKED              VALUE 10.
               88  CCP-CARD-EXPIRED              VALUE 11.
               88  CCP-OVER-LIMIT                VALUE 20.
               88  CCP-OVER-CASH-LIMIT           VALUE 21.
               88  CCP-BAD-REQUEST               VALUE 40.
