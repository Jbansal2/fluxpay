      *================================================================*
      * COPYBOOK : MFSREC                                              *
      * PURPOSE  : THIRD-PARTY PRODUCT MASTER (INDEXED, KEY = CODE) -  *
      *            MUTUAL FUND SCHEMES (TYPE M) AND INSURANCE PLANS    *
      *            (TYPE I) THE BANK DISTRIBUTES.                      *
      * LENGTH   : 110 BYTES                                           *
      *================================================================*
       01  MFS-RECORD.
           05  MS-CODE                 PIC X(06).
           05  MS-NAME                 PIC X(30).
           05  MS-TYPE                 PIC X(01).
               88  MS-MUTUAL-FUND                VALUE 'M'.
               88  MS-INSURANCE                  VALUE 'I'.
           05  MS-CATEGORY             PIC X(04).
           05  MS-RISK                 PIC X(01).
               88  MS-HIGH-RISK                  VALUE 'H'.
           05  MS-NAV                  PIC 9(05)V9(04).
           05  MS-NAV-DATE             PIC 9(08).
      *    MF: TRAIL COMMISSION % P.A. ON ASSETS
      *    INSURANCE: COMMISSION % OF EVERY PREMIUM COLLECTED
           05  MS-COMM-PCT             PIC 9(02)V9(04).
           05  MS-MIN-AMT              PIC 9(09)V99.
      *    MF ONLY: EXIT LOAD IF REDEEMED WITHIN MS-LOAD-DAYS
           05  MS-EXIT-LOAD-PCT        PIC 9(01)V99.
           05  MS-LOAD-DAYS            PIC 9(03).
           05  MS-STATUS               PIC X(01).
               88  MS-OPEN                       VALUE 'A'.
           05  MS-SET-BY               PIC X(08).
           05  FILLER                  PIC X(19).
