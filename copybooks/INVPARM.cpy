      *================================================================*
      * COPYBOOK : INVPARM                                             *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'INVUTIL' - TAKE ONE UNIT  *
      *            (OR MORE) OF AN ITEM OUT OF A BRANCH'S STOCK WHEN   *
      *            IT IS HANDED TO A CUSTOMER.                         *
      *   IV-RC 00 ISSUED, 10 ITEM NOT STOCK-CONTROLLED AT THE BRANCH  *
      *         (CALLER MAY GO AHEAD), 30 OUT OF STOCK, 90 FILE ERROR  *
      *================================================================*
       01  INV-PARMS.
           05  IV-BRANCH               PIC X(06).
           05  IV-ITEM                 PIC X(04).
           05  IV-QTY                  PIC 9(05).
           05  IV-REF                  PIC X(16).
           05  IV-ON-HAND              PIC 9(07).
           05  IV-REORDER-HIT          PIC X(01).
               88  IV-AT-REORDER-LEVEL           VALUE 'Y'.
           05  IV-RC                   PIC 9(02).
               88  IV-OK                         VALUE 00.
               88  IV-NOT-TRACKED                VALUE 10.
               88  IV-OUT-OF-STOCK               VALUE 30.
               88  IV-FILE-ERROR                 VALUE 90.
