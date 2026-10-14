      *================================================================*
      * COPYBOOK : POSTPARM                                            *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'ACCTPOST'. USED BY EVERY  *
      *            MODULE THAT NEEDS TO DEBIT, CREDIT OR LOOK UP A     *
      *            SAVINGS/CURRENT ACCOUNT.                            *
      *================================================================*
       01  POST-PARMS.
           05  PP-FUNCTION             PIC X(01).
               88  PP-DEBIT                      VALUE 'D'.
               88  PP-CREDIT                     VALUE 'C'.
               88  PP-INQUIRE                    VALUE 'I'.
           05  PP-ACCT-NO              PIC 9(10).
           05  PP-AMOUNT               PIC 9(11)V99.
           05  PP-TRAN-TYPE            PIC X(02).
           05  PP-REF-ACCT             PIC 9(10).
           05  PP-NARRATION            PIC X(30).
      *    'Y' LETS A DEBIT DIP BELOW MINIMUM BALANCE (BANK CHARGES)
           05  PP-IGNORE-MIN-BAL       PIC X(01).
               88  PP-ALLOW-BELOW-MIN            VALUE 'Y'.
      *    GL TO USE ON THE OTHER SIDE OF THE ENTRY; ZERO = DEFAULT
      *    FOR THE TRANSACTION TYPE (SEE ACCTPOST 5000-GL-CONTRA)
           05  PP-CONTRA-GL            PIC 9(04).
      *    'Y' = BANK-INITIATED (INTEREST, CHARGES): ALLOWED ON A
      *    DORMANT ACCOUNT AND DOES NOT COUNT AS CUSTOMER ACTIVITY
           05  PP-SYSTEM-TXN           PIC X(01).
               88  PP-IS-SYSTEM-TXN              VALUE 'Y'.
      *    RETURNED FIELDS
           05  PP-HOLDER-NAME          PIC X(40).
           05  PP-ACCT-TYPE            PIC X(01).
           05  PP-BALANCE              PIC S9(13)V99.
           05  PP-AVAILABLE            PIC S9(13)V99.
           05  PP-RETURN-CODE          PIC 9(02).
               88  PP-OK                         VALUE 00.
               88  PP-NOT-FOUND                  VALUE 10.
               88  PP-NOT-ACTIVE                 VALUE 20.
               88  PP-INSUFFICIENT               VALUE 30.
               88  PP-BAD-REQUEST                VALUE 40.
               88  PP-FILE-ERROR                 VALUE 90.
