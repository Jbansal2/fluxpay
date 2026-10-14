      *================================================================*
      * COPYBOOK : GLREC                                               *
      * PURPOSE  : GENERAL LEDGER MASTER (INDEXED, KEY = GL-CODE).     *
      *            BALANCES ARE HELD DEBIT-POSITIVE: ASSETS AND        *
      *            EXPENSES ARE NORMALLY POSITIVE, LIABILITIES,        *
      *            CAPITAL AND INCOME NORMALLY NEGATIVE. THE SUM OF    *
      *            ALL BALANCES IS ZERO WHEN THE BOOKS ARE IN BALANCE. *
      *================================================================*
       01  GL-RECORD.
           05  GL-CODE                 PIC 9(04).
           05  GL-NAME                 PIC X(30).
           05  GL-TYPE                 PIC X(01).
               88  GL-ASSET                      VALUE 'A'.
               88  GL-LIABILITY                  VALUE 'L'.
               88  GL-CAPITAL                    VALUE 'C'.
               88  GL-INCOME                     VALUE 'I'.
               88  GL-EXPENSE                    VALUE 'E'.
           05  GL-BALANCE              PIC S9(15)V99 COMP-3.
           05  GL-TOTAL-DEBITS         PIC S9(15)V99 COMP-3.
           05  GL-TOTAL-CREDITS        PIC S9(15)V99 COMP-3.
           05  GL-LAST-POST-DATE       PIC 9(08).
           05  FILLER                  PIC X(20).
