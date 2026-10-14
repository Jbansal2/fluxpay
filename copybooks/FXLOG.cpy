      *================================================================*
      * COPYBOOK : FXLOG                                               *
      * PURPOSE  : ONE LINE PER FOREIGN REMITTANCE (FXLOG.DAT)         *
      *================================================================*
       01  FXL-RECORD.
           05  FXL-DATE                PIC 9(08).
           05  FXL-FY                  PIC 9(04).
           05  FXL-ACCT-NO             PIC 9(10).
           05  FXL-DIRECTION           PIC X(01).
               88  FXL-INWARD                    VALUE 'I'.
               88  FXL-OUTWARD                   VALUE 'O'.
           05  FXL-CCY                 PIC X(03).
           05  FXL-FC-AMOUNT           PIC 9(11)V99.
           05  FXL-RATE                PIC 9(03)V9(04).
           05  FXL-INR-AMOUNT          PIC 9(13)V99.
           05  FXL-USD-AMOUNT          PIC 9(11)V99.
           05  FXL-TCS                 PIC 9(11)V99.
           05  FXL-PURPOSE             PIC X(05).
           05  FXL-PARTY               PIC X(30).
           05  FXL-USER                PIC X(08).
