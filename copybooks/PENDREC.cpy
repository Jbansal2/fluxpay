      *================================================================*
      * COPYBOOK : PENDREC                                             *
      * PURPOSE  : MAKER-CHECKER QUEUE (INDEXED, KEY = PND-ID).        *
      *            A TELLER TRANSACTION ABOVE THE MAKER LIMIT IS NOT   *
      *            POSTED; IT WAITS HERE UNTIL A DIFFERENT USER WITH   *
      *            APPROVAL RIGHTS ACCEPTS OR REJECTS IT.              *
      *================================================================*
       01  PEND-RECORD.
           05  PND-ID                  PIC 9(08).
           05  PND-TYPE                PIC X(01).
               88  PND-DEPOSIT                   VALUE 'D'.
               88  PND-WITHDRAW                  VALUE 'W'.
               88  PND-TRANSFER                  VALUE 'T'.
           05  PND-FROM-ACCT           PIC 9(10).
           05  PND-TO-ACCT             PIC 9(10).
           05  PND-AMOUNT              PIC 9(11)V99.
           05  PND-NARRATION           PIC X(30).
           05  PND-STATUS              PIC X(01).
               88  PND-PENDING                   VALUE 'P'.
               88  PND-APPROVED                  VALUE 'A'.
               88  PND-REJECTED                  VALUE 'R'.
               88  PND-FAILED                    VALUE 'F'.
               88  PND-EXPIRED                   VALUE 'X'.
           05  PND-MAKER               PIC X(08).
           05  PND-MAKER-BRANCH        PIC X(06).
           05  PND-MAKE-DATE           PIC 9(08).
           05  PND-MAKE-TIME           PIC 9(06).
           05  PND-CHECKER             PIC X(08).
           05  PND-CHECK-DATE          PIC 9(08).
           05  PND-CHECK-TIME          PIC 9(06).
           05  PND-REMARK              PIC X(30).
           05  PND-RESULT-RC           PIC 9(02).
           05  FILLER                  PIC X(10).
