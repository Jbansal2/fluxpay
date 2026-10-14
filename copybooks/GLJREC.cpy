      *================================================================*
      * COPYBOOK : GLJREC                                              *
      * PURPOSE  : GL JOURNAL LINE (LINE SEQUENTIAL, APPEND ONLY).     *
      *            EVERY ENTRY WRITES TWO LINES WITH THE SAME ENTRY ID *
      *            - ONE DEBIT, ONE CREDIT, SAME AMOUNT.               *
      *================================================================*
       01  GJ-RECORD.
           05  GJ-DATE                 PIC 9(08).
           05  GJ-TIME                 PIC 9(06).
           05  GJ-ENTRY-ID             PIC 9(12).
           05  GJ-GL-CODE              PIC 9(04).
           05  GJ-DR-CR                PIC X(01).
           05  GJ-AMOUNT               PIC 9(13)V99.
           05  GJ-REF                  PIC X(16).
           05  GJ-NARRATION            PIC X(30).
           05  GJ-USER-ID              PIC X(08).
