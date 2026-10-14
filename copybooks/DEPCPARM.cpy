      *================================================================*
      * COPYBOOK : DEPCPARM                                            *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'DEPCALC'                  *
      *   RT - RATE FOR DC-TENURE MONTHS (+ SENIOR)  -> DC-RESULT-RATE *
      *        FROM THE RATE CARD IN FORCE ON DC-AS-OF-DATE (NOT       *
      *        NUMERIC / ZERO = TODAY); DC-RATE-EFF-DATE RETURNS THE   *
      *        CARD'S EFFECTIVE DATE (ZERO = BUILT-IN DEFAULT CARD)    *
      *   FV - FD VALUE OF DC-AMOUNT AFTER DC-MONTHS  -> DC-RESULT-AMT *
      *   RV - RD VALUE: DC-COUNT INSTALMENTS OF DC-AMOUNT, VALUED AT  *
      *        MONTH DC-MONTHS                        -> DC-RESULT-AMT *
      * INTEREST IS COMPOUNDED QUARTERLY AT DC-RATE % P.A.             *
      *================================================================*
       01  DEPC-PARMS.
           05  DC-FUNCTION             PIC X(02).
           05  DC-AMOUNT               PIC 9(11)V99.
           05  DC-RATE                 PIC 9(02)V9(04).
           05  DC-MONTHS               PIC 9(03).
           05  DC-COUNT                PIC 9(03).
           05  DC-TENURE               PIC 9(03).
           05  DC-SENIOR               PIC X(01).
           05  DC-RESULT-AMT           PIC 9(11)V99.
           05  DC-RESULT-RATE          PIC 9(02)V9(04).
           05  DC-RC                   PIC 9(02).
           05  DC-AS-OF-DATE           PIC 9(08).
           05  DC-RATE-EFF-DATE        PIC 9(08).
