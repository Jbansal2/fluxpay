      *================================================================*
      * COPYBOOK : MANDREC                                             *
      * PURPOSE  : NACH DEBIT MANDATE REGISTER (INDEXED, KEY = UMRN).  *
      *            A MANDATE IS THE CUSTOMER'S STANDING PERMISSION FOR *
      *            A CORPORATE (INSURER, MUTUAL FUND, LENDER) TO DEBIT *
      *            HIS ACCOUNT UP TO A MAXIMUM AMOUNT.                 *
      *            UMRN = 'CBNK' + REGISTRATION DATE + 8-DIGIT SERIAL. *
      *================================================================*
       01  MAND-RECORD.
           05  NM-UMRN                 PIC X(20).
           05  NM-ACCT-NO              PIC 9(10).
           05  NM-CORPORATE            PIC X(30).
           05  NM-CORP-REF             PIC X(20).
           05  NM-MAX-AMOUNT           PIC 9(09)V99.
           05  NM-FREQUENCY            PIC X(01).
               88  NM-MONTHLY                    VALUE 'M'.
               88  NM-QUARTERLY                  VALUE 'Q'.
               88  NM-YEARLY                     VALUE 'Y'.
               88  NM-AS-PRESENTED               VALUE 'A'.
               88  NM-FREQ-VALID                 VALUES 'M' 'Q' 'Y'
                                                        'A'.
           05  NM-START-DATE           PIC 9(08).
      *    ZERO = UNTIL CANCELLED
           05  NM-END-DATE             PIC 9(08).
           05  NM-STATUS               PIC X(01).
               88  NM-ACTIVE                     VALUE 'A'.
               88  NM-CANCELLED                  VALUE 'C'.
               88  NM-SUSPENDED                  VALUE 'S'.
           05  NM-REG-DATE             PIC 9(08).
           05  NM-REG-BY               PIC X(08).
           05  NM-LAST-DEBIT-DATE      PIC 9(08).
           05  NM-LAST-AMOUNT          PIC 9(09)V99.
           05  NM-DEBITS               PIC 9(05).
           05  NM-BOUNCES              PIC 9(03).
           05  NM-CONSEC-BOUNCES       PIC 9(02).
           05  FILLER                  PIC X(10).
