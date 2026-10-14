      *================================================================*
      * COPYBOOK : MKRPARM                                             *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'MKRQUEUE'                 *
      *            ADD   - QUEUE A TRANSACTION, RETURNS MKR-ID         *
      *            COUNT - NUMBER OF PENDING ITEMS IN MKR-PENDING      *
      *            EXPIRE- PENDING ITEMS MADE BEFORE MKR-RUN-DATE ARE  *
      *                    MARKED EXPIRED; HOW MANY IN MKR-PENDING     *
      *================================================================*
       01  MKR-PARMS.
           05  MKR-FUNCTION            PIC X(01).
               88  MKR-ADD                       VALUE 'A'.
               88  MKR-COUNT                     VALUE 'C'.
               88  MKR-EXPIRE                    VALUE 'E'.
           05  MKR-RUN-DATE            PIC 9(08).
           05  MKR-TYPE                PIC X(01).
           05  MKR-FROM-ACCT           PIC 9(10).
           05  MKR-TO-ACCT             PIC 9(10).
           05  MKR-AMOUNT              PIC 9(11)V99.
           05  MKR-NARRATION           PIC X(30).
           05  MKR-ID                  PIC 9(08).
           05  MKR-PENDING             PIC 9(06).
           05  MKR-RC                  PIC 9(02).
               88  MKR-OK                        VALUE 00.
               88  MKR-FILE-ERROR                VALUE 90.
