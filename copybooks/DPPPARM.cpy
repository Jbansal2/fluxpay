      *================================================================*
      * COPYBOOK : DPPPARM                                             *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'DEPPAY' USING DPP-PARMS   *
      *            DEP-RECORD - PAY OUT A TERM DEPOSIT WITH TDS        *
      *================================================================*
       01  DPP-PARMS.
           05  DPP-GROSS               PIC 9(11)V99.
           05  DPP-NARRATION           PIC X(30).
           05  DPP-RUN-DATE            PIC 9(08).
           05  DPP-INTEREST            PIC 9(11)V99.
           05  DPP-TDS                 PIC 9(11)V99.
           05  DPP-NET                 PIC 9(11)V99.
           05  DPP-RC                  PIC 9(02).
               88  DPP-OK                        VALUE 00.
