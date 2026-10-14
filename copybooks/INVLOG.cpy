      *================================================================*
      * COPYBOOK : INVLOG                                              *
      * PURPOSE  : STOCK MOVEMENT LOG (LINE SEQUENTIAL, APPEND ONLY).  *
      *            R RECEIPT, I ISSUE, T TRANSFER OUT, U TRANSFER IN,  *
      *            A ADJUSTMENT (DAMAGED / LOST), O ORDER PLACED       *
      * LENGTH   : 70 BYTES                                            *
      *================================================================*
       01  IVL-RECORD.
           05  IVL-DATE                PIC 9(08).
           05  IVL-TIME                PIC 9(06).
           05  IVL-BRANCH              PIC X(06).
           05  IVL-ITEM                PIC X(04).
           05  IVL-TYPE                PIC X(01).
           05  IVL-QTY                 PIC 9(07).
           05  IVL-BAL-AFTER           PIC 9(07).
           05  IVL-REF                 PIC X(16).
           05  IVL-USER                PIC X(08).
           05  FILLER                  PIC X(07).
