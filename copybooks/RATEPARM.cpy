      *================================================================*
      * COPYBOOK : RATEPARM                                            *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'RATEUTIL'                 *
      *            RTP-CODE IN, RTP-VALUE + RTP-EFF-DATE OUT.          *
      *            RC 00 FROM THE MASTER, 04 BUILT-IN DEFAULT USED     *
      *            (FILE OR ROW MISSING), 08 UNKNOWN CODE.             *
      *================================================================*
       01  RATE-PARMS.
           05  RTP-CODE                PIC X(08).
           05  RTP-VALUE               PIC 9(02)V9(04).
           05  RTP-EFF-DATE            PIC 9(08).
           05  RTP-RC                  PIC 9(02).
               88  RTP-OK                        VALUE 00.
               88  RTP-DEFAULTED                 VALUE 04.
               88  RTP-UNKNOWN                   VALUE 08.
