      *================================================================*
      * COPYBOOK : UTSEEDW                                             *
      * PURPOSE  : FIELDS FOR UT-SEED-ACCOUNT (COPY UTSEEDP). THE      *
      *            DRIVER DECLARES ACCT-FILE ON data/ACCTMAST.DAT WITH *
      *            COPY ACCTREC AND FILE STATUS UT-ACCT-FS.            *
      *================================================================*
       01  UT-SEED.
           05  UT-SEED-NO              PIC 9(10).
           05  UT-SEED-TYPE            PIC X(01).
           05  UT-SEED-STATUS          PIC X(01).
           05  UT-SEED-BALANCE         PIC S9(13)V99.
           05  UT-SEED-MIN             PIC S9(09)V99.
           05  UT-SEED-OD              PIC 9(09)V99.
           05  UT-SEED-BRANCH          PIC X(06).
       01  UT-ACCT-FS                  PIC X(02).
