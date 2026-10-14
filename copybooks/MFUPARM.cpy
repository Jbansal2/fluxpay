      *================================================================*
      * COPYBOOK : MFUPARM                                             *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'MFUTIL'                   *
      *   B - BUY MU-AMOUNT OF FUND MU-CODE FOR MU-ACCT-NO (MU-SOURCE  *
      *       P = LUMPSUM, S = SIP)       -> MU-UNITS, MU-NAV          *
      *   R - REDEEM MU-UNITS (ZERO = ALL) -> MU-VALUE, MU-LOAD, MU-NET*
      *   I - COLLECT THE NEXT PREMIUM OF POLICY MU-CODE  -> MU-NET,   *
      *       MU-COMMISSION (POSTED TO GL HERE)                        *
      * THE CALLER MUST NOT HAVE MFSCHEME / MFHOLD OPEN.               *
      *================================================================*
       01  MFU-PARMS.
           05  MU-FUNCTION             PIC X(01).
               88  MU-BUY                        VALUE 'B'.
               88  MU-REDEEM                     VALUE 'R'.
               88  MU-PREMIUM                    VALUE 'I'.
           05  MU-ACCT-NO              PIC 9(10).
           05  MU-CODE                 PIC X(06).
           05  MU-SOURCE               PIC X(01).
           05  MU-AMOUNT               PIC 9(11)V99.
           05  MU-UNITS                PIC 9(09)V9(03).
           05  MU-RUN-DATE             PIC 9(08).
           05  MU-REF                  PIC X(12).
      *    RETURNED
           05  MU-NAV                  PIC 9(05)V9(04).
           05  MU-VALUE                PIC 9(11)V99.
           05  MU-LOAD                 PIC 9(09)V99.
           05  MU-NET                  PIC 9(11)V99.
           05  MU-COMMISSION           PIC 9(09)V99.
           05  MU-RC                   PIC 9(02).
               88  MU-OK                         VALUE 00.
               88  MU-NO-PRODUCT                 VALUE 10.
               88  MU-BELOW-MINIMUM              VALUE 20.
               88  MU-NO-FUNDS                   VALUE 30.
               88  MU-NO-HOLDING                 VALUE 40.
               88  MU-POST-FAILED                VALUE 50.
               88  MU-FILE-ERROR                 VALUE 90.
           05  MU-POST-RC              PIC 9(02).
