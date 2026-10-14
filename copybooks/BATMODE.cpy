      *================================================================*
      * COPYBOOK : BATMODE                                             *
      * PURPOSE  : RUN-UNIT WIDE "KEEP FILES OPEN" SWITCH SHARED BY    *
      *            ACCTPOST, GLPOST, AUDITLOG AND AMLCHK (EXTERNAL, SO *
      *            EVERY MODULE SEES THE SAME BYTE). SET ONLY THROUGH  *
      *            CALL 'BATCHCTL' USING 'S' (START) / 'E' (END).      *
      *              'Y' - LEAVE FILES OPEN BETWEEN CALLS              *
      *              'E' - CLOSE WHATEVER IS OPEN AND RETURN AT ONCE   *
      *              ANYTHING ELSE - OPEN/CLOSE ON EVERY CALL (ONLINE) *
      *================================================================*
       01  CB-BATCH-MODE               PIC X(01) EXTERNAL.
           88  CB-KEEP-OPEN                      VALUE 'Y'.
           88  CB-CLOSE-ALL                      VALUE 'E'.
