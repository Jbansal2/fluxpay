      *================================================================*
      * COPYBOOK : SECPARM                                             *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'SECUTIL'                  *
      *   PH - HASH SC-PASSWORD SALTED WITH SC-SALT -> SC-HASH         *
      *   PV - CHECK SC-PASSWORD AGAINST PASSWORD POLICY -> SC-RC      *
      *        (00 OK, 10 TOO SHORT, 20 NO LETTER, 30 NO DIGIT,        *
      *         40 CONTAINS SPACES)                                    *
      *================================================================*
       01  SEC-PARMS.
           05  SC-FUNCTION             PIC X(02).
           05  SC-PASSWORD             PIC X(20).
           05  SC-SALT                 PIC X(08).
           05  SC-HASH                 PIC 9(09).
           05  SC-RC                   PIC 9(02).
