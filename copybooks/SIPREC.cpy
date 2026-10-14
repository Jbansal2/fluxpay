      *================================================================*
      * COPYBOOK : SIPREC                                              *
      * PURPOSE  : SYSTEMATIC INVESTMENT PLAN (INDEXED, KEY = SIP NO). *
      *            MFBATCH DEBITS THE ACCOUNT ON SP-NEXT-DATE; THREE   *
      *            MISSED INSTALMENTS IN A ROW CANCEL THE SIP.         *
      * LENGTH   : 80 BYTES                                            *
      *================================================================*
       01  SIP-RECORD.
           05  SP-NO                   PIC 9(08).
           05  SP-ACCT-NO              PIC 9(10).
           05  SP-CODE                 PIC X(06).
           05  SP-AMOUNT               PIC 9(09)V99.
           05  SP-DAY                  PIC 9(02).
           05  SP-NEXT-DATE            PIC 9(08).
           05  SP-END-DATE             PIC 9(08).
           05  SP-STATUS               PIC X(01).
               88  SP-ACTIVE                     VALUE 'A'.
               88  SP-CANCELLED                  VALUE 'C'.
               88  SP-COMPLETED                  VALUE 'D'.
           05  SP-PAID-CNT             PIC 9(03).
           05  SP-MISSED-CNT           PIC 9(02).
           05  SP-REG-DATE             PIC 9(08).
           05  FILLER                  PIC X(13).
