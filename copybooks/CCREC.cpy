      *================================================================*
      * COPYBOOK : CCREC                                               *
      * PURPOSE  : CREDIT CARD ACCOUNT (INDEXED, KEY = CC-NO).         *
      *            CC-OUTSTANDING IS EVERYTHING OWED RIGHT NOW;        *
      *            CC-STMT-BALANCE IS WHAT THE LAST STATEMENT BILLED.  *
      *================================================================*
       01  CC-RECORD.
           05  CC-NO                   PIC 9(16).
           05  CC-CUST-ID              PIC 9(08).
           05  CC-NAME                 PIC X(26).
           05  CC-PAY-ACCT             PIC 9(10).
           05  CC-STATUS               PIC X(01).
               88  CC-ACTIVE                     VALUE 'A'.
               88  CC-BLOCKED                    VALUE 'B'.
               88  CC-CLOSED                     VALUE 'X'.
           05  CC-EXPIRY-YYYYMM        PIC 9(06).
           05  CC-LIMIT                PIC 9(09)V99.
           05  CC-CASH-LIMIT           PIC 9(09)V99.
           05  CC-OUTSTANDING          PIC S9(11)V99.
           05  CC-CASH-OUTSTANDING     PIC 9(11)V99.
      *    STATEMENT CYCLE
           05  CC-NEXT-STMT-DATE       PIC 9(08).
           05  CC-LAST-STMT-DATE       PIC 9(08).
           05  CC-STMT-BALANCE         PIC S9(11)V99.
           05  CC-MIN-DUE              PIC 9(09)V99.
           05  CC-DUE-DATE             PIC 9(08).
           05  CC-PAID-SINCE-STMT      PIC 9(11)V99.
           05  CC-CYCLE-FLAGS.
               10  CC-LATE-FEE-DONE    PIC X(01).
               10  CC-AUTOPAY-DONE     PIC X(01).
      *    AUTOPAY FROM CC-PAY-ACCT ON THE DUE DATE
           05  CC-AUTOPAY              PIC X(01).
               88  CC-AUTOPAY-OFF                VALUE 'N'.
               88  CC-AUTOPAY-MIN                VALUE 'M'.
               88  CC-AUTOPAY-FULL               VALUE 'F'.
           05  CC-LATE-COUNT           PIC 9(03).
           05  CC-ISSUE-DATE           PIC 9(08).
           05  FILLER                  PIC X(20).
