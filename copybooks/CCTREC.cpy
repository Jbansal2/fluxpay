      *================================================================*
      * COPYBOOK : CCTREC                                              *
      * PURPOSE  : CREDIT CARD TRANSACTION (LINE SEQUENTIAL, APPEND)   *
      *            DEBITS (P C F I G) INCREASE WHAT IS OWED,           *
      *            CREDITS (Y R) REDUCE IT                             *
      *================================================================*
       01  CCT-RECORD.
           05  CCT-CARD-NO             PIC 9(16).
           05  CCT-DATE                PIC 9(08).
           05  CCT-TIME                PIC 9(06).
           05  CCT-TYPE                PIC X(01).
               88  CCT-PURCHASE                  VALUE 'P'.
               88  CCT-CASH-ADVANCE              VALUE 'C'.
               88  CCT-FEE                       VALUE 'F'.
               88  CCT-INTEREST                  VALUE 'I'.
               88  CCT-GST                       VALUE 'G'.
               88  CCT-PAYMENT                   VALUE 'Y'.
               88  CCT-REFUND                    VALUE 'R'.
               88  CCT-IS-CREDIT                 VALUES 'Y' 'R'.
           05  CCT-AMOUNT              PIC 9(09)V99.
           05  CCT-MERCHANT            PIC X(25).
           05  CCT-AUTH-CODE           PIC X(06).
           05  CCT-OUTSTANDING-AFTER   PIC S9(11)V99.
