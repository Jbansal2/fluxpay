      *================================================================*
      * COPYBOOK : LOANREC                                             *
      * PURPOSE  : LOAN MASTER RECORD (INDEXED, KEY = LOAN-NO)         *
      *================================================================*
       01  LOAN-RECORD.
           05  LOAN-NO                 PIC 9(10).
           05  LOAN-ACCT-NO            PIC 9(10).
           05  LOAN-TYPE               PIC X(01).
               88  LOAN-HOME                     VALUE 'H'.
               88  LOAN-PERSONAL                 VALUE 'P'.
               88  LOAN-VEHICLE                  VALUE 'V'.
               88  LOAN-GOLD                     VALUE 'G'.
               88  LOAN-AGAINST-FD               VALUE 'F'.
               88  LOAN-TYPE-VALID               VALUES 'H' 'P' 'V'.
               88  LOAN-SECURED                  VALUES 'G' 'F'.
           05  LOAN-STATUS             PIC X(01).
               88  LOAN-ACTIVE                   VALUE 'A'.
               88  LOAN-CLOSED                   VALUE 'C'.
               88  LOAN-NPA                      VALUE 'N'.
           05  LOAN-PRINCIPAL          PIC 9(11)V99 COMP-3.
           05  LOAN-RATE               PIC 9(02)V9(04).
           05  LOAN-TENURE             PIC 9(03).
           05  LOAN-EMI                PIC 9(09)V99 COMP-3.
           05  LOAN-OUTSTANDING        PIC 9(11)V99 COMP-3.
           05  LOAN-EMIS-PAID          PIC 9(03).
           05  LOAN-OVERDUE-EMIS       PIC 9(03).
           05  LOAN-PENALTY-DUE        PIC 9(09)V99 COMP-3.
           05  LOAN-TOTAL-INT-PAID     PIC 9(11)V99 COMP-3.
           05  LOAN-DISB-DATE          PIC 9(08).
           05  LOAN-NEXT-DUE-DATE      PIC 9(08).
           05  LOAN-LAST-PAY-DATE      PIC 9(08).
           05  LOAN-CLOSE-DATE         PIC 9(08).
      *    SECURED LOANS (GOLD / AGAINST FD)
           05  LOAN-COLLATERAL-VALUE   PIC 9(11)V99 COMP-3.
           05  LOAN-GOLD-GRAMS         PIC 9(05)V999.
           05  LOAN-GOLD-CARAT         PIC 9(02).
           05  LOAN-LIEN-DEP-NO        PIC 9(10).
           05  LOAN-MARGIN-CALL        PIC X(01).
               88  LOAN-IN-MARGIN-CALL           VALUE 'Y'.
      *    INTEREST TYPE: F = FIXED, R = REPO-LINKED (EBLR). FOR R THE
      *    RATE IS REPO + LOAN-SPREAD AND IS RESET BY LNRESET.
      *    RESET PREFERENCE: T = KEEP EMI, CHANGE TENURE; E = KEEP
      *    TENURE, CHANGE EMI.
           05  LOAN-RATE-TYPE          PIC X(01).
               88  LOAN-FIXED-RATE               VALUES 'F' SPACE.
               88  LOAN-REPO-LINKED              VALUE 'R'.
           05  LOAN-SPREAD             PIC 9(02)V9(04).
           05  LOAN-RESET-PREF         PIC X(01).
               88  LOAN-RESET-TENURE             VALUES 'T' SPACE.
               88  LOAN-RESET-EMI                VALUE 'E'.
      *    IRAC ASSET CLASS, SET BY PROVBAT: S STANDARD, B SUB-STANDARD,
      *    1/2/3 DOUBTFUL, L LOSS; W = WRITTEN OFF (COLLECTIONS)
           05  LOAN-ASSET-CLASS        PIC X(01).
               88  LOAN-WRITTEN-OFF              VALUE 'W'.
           05  FILLER                  PIC X(03).
