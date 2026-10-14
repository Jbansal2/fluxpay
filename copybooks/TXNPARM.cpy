      *================================================================*
      * COPYBOOK : TXNPARM                                             *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'TXNEXEC' (TELLER CASH     *
      *            DEPOSIT / WITHDRAWAL / INTERNAL TRANSFER). THE LAST *
      *            ACCTPOST RESULT IS LEFT IN THE CALLER'S POST-PARMS. *
      *================================================================*
       01  TXN-PARMS.
           05  TX-FUNCTION             PIC X(01).
               88  TX-DEPOSIT                    VALUE 'D'.
               88  TX-WITHDRAW                   VALUE 'W'.
               88  TX-TRANSFER                   VALUE 'T'.
           05  TX-FROM-ACCT            PIC 9(10).
           05  TX-TO-ACCT              PIC 9(10).
           05  TX-AMOUNT               PIC 9(11)V99.
           05  TX-NARRATION            PIC X(30).
      *    WHOSE DRAWER THE CASH GOES THROUGH (BLANK = SIGNED-ON USER)
           05  TX-CASH-USER            PIC X(08).
           05  TX-CASH-BRANCH          PIC X(06).
      *    CASHUTIL RESULT: 00 TRACKED, 04 NO DRAWER OPEN
           05  TX-CASH-RC              PIC 9(02).
           05  TX-RC                   PIC 9(02).
               88  TX-OK                         VALUE 00.
               88  TX-POST-FAILED                VALUE 10.
               88  TX-TARGET-MISSING             VALUE 20.
               88  TX-SAME-ACCOUNT               VALUE 30.
               88  TX-REVERSED                   VALUE 40.
               88  TX-REVERSAL-FAILED            VALUE 50.
