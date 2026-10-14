      *================================================================*
      * COPYBOOK : CSHPARM                                             *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'CASHUTIL' - RECORD A CASH *
      *            RECEIPT (R) OR PAYMENT (P) IN A TELLER'S DRAWER.    *
      *            USER/BRANCH BLANK = THE SIGNED-ON USER.             *
      *================================================================*
       01  CSH-PARMS.
           05  CSH-FUNCTION            PIC X(01).
               88  CSH-RECEIPT                   VALUE 'R'.
               88  CSH-PAYMENT                   VALUE 'P'.
           05  CSH-USER                PIC X(08).
           05  CSH-BRANCH              PIC X(06).
           05  CSH-AMOUNT              PIC 9(11)V99.
           05  CSH-RC                  PIC 9(02).
               88  CSH-OK                        VALUE 00.
               88  CSH-NO-DRAWER                 VALUE 04.
               88  CSH-FILE-ERROR                VALUE 90.
