      *================================================================*
      * COPYBOOK : TDSREC                                              *
      * PURPOSE  : TDS DEDUCTION LOG (LINE SEQUENTIAL, APPEND ONLY) -  *
      *            SOURCE FOR THE TDS CERTIFICATE (FORM 16A STYLE)     *
      *================================================================*
       01  TDS-RECORD.
           05  TDS-DATE                PIC 9(08).
           05  TDS-FY-START            PIC 9(04).
           05  TDS-ACCT-NO             PIC 9(10).
           05  TDS-DEP-NO              PIC 9(10).
           05  TDS-INTEREST            PIC 9(11)V99.
           05  TDS-RATE                PIC 9(02)V99.
           05  TDS-AMOUNT              PIC 9(11)V99.
           05  TDS-NET-PAID            PIC 9(11)V99.
