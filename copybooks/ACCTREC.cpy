      *================================================================*
      * COPYBOOK : ACCTREC                                             *
      * PURPOSE  : ACCOUNT MASTER RECORD LAYOUT (INDEXED, KEY=ACCT-NO) *
      * LENGTH   : 156 BYTES                                           *
      *================================================================*
       01  ACCT-RECORD.
           05  ACCT-NO                 PIC 9(10).
           05  ACCT-HOLDER-NAME        PIC X(40).
           05  ACCT-TYPE               PIC X(01).
               88  ACCT-SAVINGS                  VALUE 'S'.
               88  ACCT-CURRENT                  VALUE 'C'.
               88  ACCT-TYPE-VALID               VALUES 'S' 'C'.
           05  ACCT-STATUS             PIC X(01).
               88  ACCT-ACTIVE                   VALUE 'A'.
               88  ACCT-FROZEN                   VALUE 'F'.
               88  ACCT-CLOSED                   VALUE 'X'.
               88  ACCT-DORMANT                  VALUE 'D'.
           05  ACCT-BALANCE            PIC S9(13)V99 COMP-3.
           05  ACCT-MIN-BALANCE        PIC S9(09)V99 COMP-3.
           05  ACCT-INT-RATE           PIC 9(02)V9(04).
           05  ACCT-OPEN-DATE          PIC 9(08).
           05  ACCT-LAST-TXN-DATE      PIC 9(08).
           05  ACCT-LAST-INT-DATE      PIC 9(08).
           05  ACCT-PHONE              PIC X(10).
           05  ACCT-BRANCH-CODE        PIC X(06).
      *    DAILY-PRODUCT INTEREST: SUM OF END-OF-DAY BALANCES SINCE
      *    THE LAST INTEREST RUN (SAVINGS CREDIT / OVERDRAFT DEBIT)
           05  ACCT-INT-PRODUCT        PIC S9(15)V99 COMP-3.
           05  ACCT-PRODUCT-DAYS       PIC 9(03).
           05  ACCT-LAST-PRODUCT-DATE  PIC 9(08).
      *    OVERDRAFT (CURRENT ACCOUNTS ONLY)
           05  ACCT-OD-LIMIT           PIC 9(09)V99 COMP-3.
           05  ACCT-OD-RATE            PIC 9(02)V9(04).
           05  ACCT-OD-PRODUCT         PIC S9(15)V99 COMP-3.
      *    RESIDENCY: SPACE/R RESIDENT, E NRE (REPATRIABLE),
      *    O NRO (INDIAN INCOME OF A NON-RESIDENT) - SET IN FXMNT
           05  ACCT-RESIDENCY          PIC X(01).
               88  ACCT-RESIDENT                 VALUES 'R' SPACE.
               88  ACCT-NRE                      VALUE 'E'.
               88  ACCT-NRO                      VALUE 'O'.
           05  FILLER                  PIC X(02).
