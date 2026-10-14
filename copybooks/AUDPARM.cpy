      *================================================================*
      * COPYBOOK : AUDPARM                                             *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'AUDITLOG'. THE USER ID    *
      *            AND TIMESTAMP ARE FILLED IN BY AUDITLOG ITSELF.     *
      *================================================================*
       01  AUD-PARMS.
           05  AU-PROGRAM              PIC X(08).
           05  AU-ACTION               PIC X(12).
           05  AU-KEY                  PIC X(16).
           05  AU-DETAIL               PIC X(40).
