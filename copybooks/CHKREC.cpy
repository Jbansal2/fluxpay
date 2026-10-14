      *================================================================*
      * COPYBOOK : CHKREC                                              *
      * PURPOSE  : BATCH CHECKPOINT RECORD (INDEXED, KEY = CK-KEY).    *
      *            ONE ROW PER PROGRAM PER RUN DATE.                   *
      *================================================================*
       01  CHK-RECORD.
           05  CK-KEY.
               10  CK-PROGRAM          PIC X(08).
               10  CK-RUN-DATE         PIC 9(08).
           05  CK-STATUS               PIC X(01).
               88  CK-RUNNING                    VALUE 'R'.
               88  CK-COMPLETE                   VALUE 'C'.
      *    LAST STEP THAT FINISHED COMPLETELY (0 = NONE)
           05  CK-STEP                 PIC 9(02).
      *    LAST KEY COMMITTED INSIDE THE CURRENT STEP
           05  CK-LAST-KEY             PIC X(20).
           05  CK-RECORDS              PIC 9(09).
      *    PROGRAM-DEFINED RUNNING TOTALS, RESTORED ON RESTART
           05  CK-COUNTERS             PIC X(80).
           05  CK-STARTED              PIC X(14).
           05  CK-UPDATED              PIC X(14).
           05  CK-RESTARTS             PIC 9(03).
           05  FILLER                  PIC X(11).
