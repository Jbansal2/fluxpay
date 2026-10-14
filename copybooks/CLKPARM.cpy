      *================================================================*
      * COPYBOOK : CLKPARM                                             *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'CUSTLKUP' - THE PRIMARY   *
      *            HOLDER OF AN ACCOUNT, FROM ACCTREL + CUSTMAST.      *
      *            CALL WITH CLK-CLOSE = 'Y' AT THE END OF A RUN TO    *
      *            RELEASE THE FILES.                                  *
      *================================================================*
       01  CLK-PARMS.
           05  CLK-ACCT-NO             PIC 9(10).
           05  CLK-CLOSE               PIC X(01).
           05  CLK-CUST-ID             PIC 9(08).
           05  CLK-NAME                PIC X(40).
           05  CLK-PAN                 PIC X(10).
           05  CLK-DOB                 PIC 9(08).
           05  CLK-GENDER              PIC X(01).
           05  CLK-PINCODE             PIC 9(06).
           05  CLK-EMAIL               PIC X(40).
           05  CLK-KYC-STATUS          PIC X(01).
               88  CLK-KYC-VERIFIED              VALUE 'V'.
           05  CLK-RC                  PIC 9(02).
               88  CLK-OK                        VALUE 00.
               88  CLK-NO-HOLDER                 VALUE 10.
               88  CLK-NO-CUSTOMER               VALUE 20.
