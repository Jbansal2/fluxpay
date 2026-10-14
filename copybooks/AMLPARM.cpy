      *================================================================*
      * COPYBOOK : AMLPARM                                             *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'AMLCHK' - ONE POSTED      *
      *            TRANSACTION TO BE SCREENED                          *
      *================================================================*
       01  AML-PARMS.
           05  AM-ACCT-NO              PIC 9(10).
           05  AM-TRAN-TYPE            PIC X(02).
           05  AM-DR-CR                PIC X(01).
               88  AM-IS-DEBIT                   VALUE 'D'.
               88  AM-IS-CREDIT                  VALUE 'C'.
           05  AM-AMOUNT               PIC 9(11)V99.
           05  AM-DATE                 PIC 9(08).
           05  AM-TIME                 PIC 9(06).
           05  AM-NAME                 PIC X(40).
