      *================================================================*
      * COPYBOOK : DATEPARM                                            *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'DATEUTIL'                 *
      *   AM - ADD DU-NUMBER MONTHS TO DU-DATE-1  -> DU-RESULT-DATE    *
      *   AD - ADD DU-NUMBER DAYS TO DU-DATE-1    -> DU-RESULT-DATE    *
      *   DB - DAYS FROM DU-DATE-1 TO DU-DATE-2   -> DU-RESULT-DAYS    *
      *   VD - VALIDATE DU-DATE-1                 -> DU-RC ONLY        *
      *================================================================*
       01  DATE-PARMS.
           05  DU-FUNCTION             PIC X(02).
               88  DU-ADD-MONTHS                 VALUE 'AM'.
               88  DU-ADD-DAYS                   VALUE 'AD'.
               88  DU-DAYS-BETWEEN               VALUE 'DB'.
               88  DU-VALIDATE                   VALUE 'VD'.
           05  DU-DATE-1               PIC 9(08).
           05  DU-DATE-2               PIC 9(08).
           05  DU-NUMBER               PIC S9(05).
           05  DU-RESULT-DATE          PIC 9(08).
           05  DU-RESULT-DAYS          PIC S9(07).
           05  DU-RC                   PIC 9(02).
               88  DU-OK                         VALUE 00.
               88  DU-INVALID-DATE               VALUE 10.
               88  DU-INVALID-FUNCTION           VALUE 20.
