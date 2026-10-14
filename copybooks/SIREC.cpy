      *================================================================*
      * COPYBOOK : SIREC                                               *
      * PURPOSE  : STANDING INSTRUCTION MASTER (INDEXED, KEY = SI-ID)  *
      *================================================================*
       01  SI-RECORD.
           05  SI-ID                   PIC 9(08).
           05  SI-FROM-ACCT            PIC 9(10).
           05  SI-TYPE                 PIC X(01).
               88  SI-TRANSFER                   VALUE 'T'.
               88  SI-LOAN-EMI                   VALUE 'L'.
               88  SI-RD-INSTALMENT              VALUE 'R'.
               88  SI-TYPE-VALID                 VALUES 'T' 'L' 'R'.
      *    TARGET: ACCOUNT NO (T), LOAN NO (L) OR DEPOSIT NO (R)
           05  SI-TARGET               PIC 9(10).
      *    FIXED AMOUNT FOR TRANSFERS; EMI / INSTALMENT ARE LOOKED UP
           05  SI-AMOUNT               PIC 9(11)V99.
           05  SI-DAY-OF-MONTH         PIC 9(02).
           05  SI-START-DATE           PIC 9(08).
           05  SI-END-DATE             PIC 9(08).
           05  SI-STATUS               PIC X(01).
               88  SI-ACTIVE                     VALUE 'A'.
               88  SI-SUSPENDED                  VALUE 'S'.
               88  SI-CANCELLED                  VALUE 'C'.
               88  SI-COMPLETED                  VALUE 'E'.
           05  SI-NEXT-RUN-DATE        PIC 9(08).
           05  SI-CYCLE-DUE-DATE       PIC 9(08).
           05  SI-ATTEMPTS             PIC 9(01).
           05  SI-LAST-RUN-DATE        PIC 9(08).
           05  SI-LAST-RESULT          PIC X(30).
           05  SI-EXECUTED-CNT         PIC 9(04).
           05  SI-MISSED-CNT           PIC 9(04).
           05  SI-CREATED-BY           PIC X(08).
           05  FILLER                  PIC X(10).
