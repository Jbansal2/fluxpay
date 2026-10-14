      *================================================================*
      * COPYBOOK : GLPARM                                              *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'GLPOST' - ONE BALANCED    *
      *            JOURNAL ENTRY: DEBIT ONE GL, CREDIT ANOTHER.        *
      *================================================================*
       01  GL-PARMS.
           05  GP-DR-GL                PIC 9(04).
           05  GP-CR-GL                PIC 9(04).
           05  GP-AMOUNT               PIC 9(13)V99.
           05  GP-REF                  PIC X(16).
           05  GP-NARRATION            PIC X(30).
           05  GP-RC                   PIC 9(02).
               88  GP-OK                         VALUE 00.
               88  GP-UNKNOWN-GL                 VALUE 10.
               88  GP-BAD-AMOUNT                 VALUE 20.
               88  GP-FILE-ERROR                 VALUE 90.
