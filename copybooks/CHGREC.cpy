      *================================================================*
      * COPYBOOK : CHGREC                                              *
      * PURPOSE  : CHARGES LEDGER (INDEXED). ONE RECORD PER ACCOUNT,   *
      *            CHARGE CODE AND PERIOD - THE KEY ITSELF STOPS THE   *
      *            SAME CHARGE BEING LEVIED TWICE.                     *
      *================================================================*
       01  CHG-RECORD.
           05  CHG-KEY.
               10  CHG-ACCT-NO         PIC 9(10).
               10  CHG-CODE            PIC X(03).
                   88  CHG-MIN-BAL               VALUE 'MBP'.
                   88  CHG-SMS                   VALUE 'SMS'.
                   88  CHG-CARD-FEE              VALUE 'DCF'.
                   88  CHG-LOCKER-RENT           VALUE 'LKR'.
               10  CHG-PERIOD          PIC 9(06).
           05  CHG-REF                 PIC X(16).
           05  CHG-BASE-AMT            PIC 9(07)V99.
           05  CHG-GST-AMT             PIC 9(07)V99.
           05  CHG-TOTAL-AMT           PIC 9(07)V99.
           05  CHG-STATUS              PIC X(01).
               88  CHG-POSTED                    VALUE 'P'.
               88  CHG-NOT-RECOVERED             VALUE 'N'.
           05  CHG-RUN-DATE            PIC 9(08).
           05  FILLER                  PIC X(20).
