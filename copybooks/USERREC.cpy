      *================================================================*
      * COPYBOOK : USERREC                                             *
      * PURPOSE  : SYSTEM USER MASTER (INDEXED, KEY = USR-ID)          *
      *================================================================*
       01  USER-RECORD.
           05  USR-ID                  PIC X(08).
           05  USR-NAME                PIC X(30).
           05  USR-ROLE                PIC X(01).
               88  USR-TELLER                    VALUE 'T'.
               88  USR-MANAGER                   VALUE 'M'.
               88  USR-ADMIN                     VALUE 'A'.
               88  USR-ROLE-VALID                VALUES 'T' 'M' 'A'.
           05  USR-BRANCH              PIC X(06).
           05  USR-PWD-HASH            PIC 9(09).
           05  USR-STATUS              PIC X(01).
               88  USR-ACTIVE                    VALUE 'A'.
               88  USR-LOCKED                    VALUE 'L'.
               88  USR-DISABLED                  VALUE 'D'.
           05  USR-FAILED-CNT          PIC 9(01).
           05  USR-MUST-CHANGE         PIC X(01).
               88  USR-PWD-MUST-CHANGE           VALUE 'Y'.
           05  USR-PWD-CHANGED-DATE    PIC 9(08).
           05  USR-LAST-LOGIN-DATE     PIC 9(08).
           05  USR-LAST-LOGIN-TIME     PIC 9(06).
           05  USR-CREATED-BY          PIC X(08).
           05  USR-CREATED-DATE        PIC 9(08).
           05  FILLER                  PIC X(20).
