      *================================================================*
      * COPYBOOK : CKPPARM                                             *
      * PURPOSE  : PARAMETER BLOCK FOR CALL 'CHKPOINT'                 *
      *   B - BEGIN: RESUMES AN UNFINISHED RUN FOR THE SAME PROGRAM    *
      *       AND RUN DATE (CKP-RESTARTED = 'Y', STEP/KEY/COUNTERS     *
      *       RETURNED) OR STARTS A FRESH ONE                          *
      *   S - SAVE A COMMIT POINT (LAST KEY, RECORDS, COUNTERS)        *
      *   E - END OF STEP CKP-STEP (CLEARS THE LAST KEY)               *
      *   C - RUN COMPLETE                                             *
      *================================================================*
       01  CKP-PARMS.
           05  CKP-FUNCTION            PIC X(01).
               88  CKP-BEGIN                     VALUE 'B'.
               88  CKP-SAVE                      VALUE 'S'.
               88  CKP-END-STEP                  VALUE 'E'.
               88  CKP-COMPLETE                  VALUE 'C'.
           05  CKP-PROGRAM             PIC X(08).
           05  CKP-RUN-DATE            PIC 9(08).
           05  CKP-STEP                PIC 9(02).
           05  CKP-LAST-KEY            PIC X(20).
           05  CKP-RECORDS             PIC 9(09).
           05  CKP-COUNTERS            PIC X(80).
           05  CKP-RESTARTED           PIC X(01).
               88  CKP-IS-RESTART                VALUE 'Y'.
           05  CKP-RESTARTS            PIC 9(03).
           05  CKP-RC                  PIC 9(02).
               88  CKP-OK                        VALUE 00.
               88  CKP-FILE-ERROR                VALUE 90.
