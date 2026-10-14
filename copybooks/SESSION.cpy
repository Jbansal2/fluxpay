      *================================================================*
      * COPYBOOK : SESSION                                             *
      * PURPOSE  : SIGNED-ON USER, SHARED BY EVERY PROGRAM IN THE RUN  *
      *            UNIT THROUGH AN EXTERNAL RECORD. SET BY SIGNON (OR  *
      *            BY RUNPGM FOR BATCH), READ BY AUDITLOG AND BY ANY   *
      *            PROGRAM THAT CHECKS ROLES.                          *
      *================================================================*
       01  SESSION-INFO EXTERNAL.
           05  SES-USER-ID             PIC X(08).
           05  SES-USER-NAME           PIC X(30).
           05  SES-ROLE                PIC X(01).
               88  SES-TELLER                    VALUE 'T'.
               88  SES-MANAGER                   VALUE 'M'.
               88  SES-ADMIN                     VALUE 'A'.
               88  SES-BATCH                     VALUE 'B'.
               88  SES-CAN-APPROVE               VALUES 'M' 'A' 'B'.
           05  SES-BRANCH              PIC X(06).
           05  SES-SIGNED-ON           PIC X(01).
               88  SES-IS-SIGNED-ON              VALUE 'Y'.
