      *================================================================*
      * COPYBOOK : CBCOMM                                              *
      * PURPOSE  : COMMAREA PASSED BETWEEN THE COREBANK CICS PROGRAMS. *
      *            PSEUDO-CONVERSATIONAL: NOTHING IS HELD IN STORAGE   *
      *            WHILE THE USER IS THINKING - THE STATE RIDES HERE.  *
      *================================================================*
       01  CB-COMMAREA.
      *    PROGRAM THAT OWNS THE SCREEN NOW; ANOTHER VALUE MEANS WE
      *    HAVE JUST ARRIVED BY XCTL AND MUST SEND A FRESH MAP
           05  CA-PROGRAM              PIC X(08).
           05  CA-USER                 PIC X(08).
           05  CA-STATE                PIC X(01).
               88  CA-NEW-ENTRY                  VALUE SPACE.
               88  CA-AWAIT-CONFIRM              VALUE 'C'.
           05  CA-FROM-ACCT            PIC 9(10).
           05  CA-TO-ACCT              PIC 9(10).
           05  CA-AMOUNT               PIC 9(11)V99.
           05  CA-NARRATION            PIC X(30).
           05  FILLER                  PIC X(20).
