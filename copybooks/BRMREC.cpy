      *================================================================*
      * COPYBOOK : BRMREC                                              *
      * PURPOSE  : BRANCH MASTER (INDEXED, KEY = BRANCH CODE)          *
      * LENGTH   : 100 BYTES                                           *
      *================================================================*
       01  BRM-RECORD.
           05  BR-CODE                 PIC X(06).
           05  BR-NAME                 PIC X(30).
           05  BR-IFSC                 PIC X(11).
           05  BR-CITY                 PIC X(20).
           05  BR-REGION               PIC X(10).
           05  BR-TYPE                 PIC X(01).
               88  BR-HEAD-OFFICE                VALUE 'H'.
               88  BR-BRANCH                     VALUE 'B'.
           05  BR-STATUS               PIC X(01).
               88  BR-OPEN                       VALUE 'A'.
               88  BR-CLOSED                     VALUE 'C'.
           05  BR-OPEN-DATE            PIC 9(08).
           05  BR-CLOSE-DATE           PIC 9(08).
           05  FILLER                  PIC X(05).
