      *================================================================*
      * COPYBOOK : DEPREC                                              *
      * PURPOSE  : TERM DEPOSIT MASTER - FD AND RD (KEY = DEP-NO)      *
      *            FOR FD, DEP-AMOUNT IS THE PRINCIPAL.                *
      *            FOR RD, DEP-AMOUNT IS THE MONTHLY INSTALMENT.       *
      *================================================================*
       01  DEP-RECORD.
           05  DEP-NO                  PIC 9(10).
           05  DEP-ACCT-NO             PIC 9(10).
           05  DEP-TYPE                PIC X(01).
               88  DEP-FD                        VALUE 'F'.
               88  DEP-RD                        VALUE 'R'.
           05  DEP-STATUS              PIC X(01).
               88  DEP-ACTIVE                    VALUE 'A'.
               88  DEP-MATURED                   VALUE 'M'.
               88  DEP-PREMATURE                 VALUE 'P'.
           05  DEP-SENIOR-FLAG         PIC X(01).
               88  DEP-SENIOR                    VALUE 'Y'.
           05  DEP-AMOUNT              PIC 9(11)V99 COMP-3.
           05  DEP-RATE                PIC 9(02)V9(04).
           05  DEP-TENURE              PIC 9(03).
           05  DEP-OPEN-DATE           PIC 9(08).
           05  DEP-MATURITY-DATE       PIC 9(08).
           05  DEP-MATURITY-AMT        PIC 9(11)V99 COMP-3.
           05  DEP-RD-PAID-CNT         PIC 9(03).
           05  DEP-RD-NEXT-DUE         PIC 9(08).
           05  DEP-RD-MISSED-CNT       PIC 9(03).
           05  DEP-TOTAL-DEPOSITED     PIC 9(11)V99 COMP-3.
           05  DEP-PAYOUT-AMT          PIC 9(11)V99 COMP-3.
           05  DEP-CLOSE-DATE          PIC 9(08).
      *    LIEN HOLDER WHEN THE DEPOSIT IS PLEDGED (ZERO = FREE):
      *    5XXXXXXXXX = LOAN NO (SECLOAN), 1XXXXXXXXX = CURRENT
      *    ACCOUNT WHOSE OVERDRAFT IT SECURES (SWEEPMNT)
           05  DEP-LIEN-LOAN-NO        PIC 9(10).
               88  DEP-LIEN-FOR-OD               VALUES 1000000000
                                                 THRU 1999999999.
      *    'Y' = OPENED BY THE AUTO-SWEEP (SWEEPBAT), BROKEN LIFO
           05  DEP-SWEEP-FLAG          PIC X(01).
               88  DEP-IS-SWEEP                  VALUE 'Y'.
           05  FILLER                  PIC X(19).
