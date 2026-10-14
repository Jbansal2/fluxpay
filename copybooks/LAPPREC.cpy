      *================================================================*
      * COPYBOOK : LAPPREC                                             *
      * PURPOSE  : LOAN APPLICATION (INDEXED, KEY = LA-APP-NO).        *
      *            LIFE: U UNDER REVIEW -> A APPROVED -> D DISBURSED,  *
      *            OR R REJECTED (BY RULE OR BY THE APPROVER),         *
      *            OR W WITHDRAWN BY THE CUSTOMER.                     *
      *================================================================*
       01  LAPP-RECORD.
           05  LA-APP-NO               PIC 9(08).
           05  LA-ACCT-NO              PIC 9(10).
           05  LA-CUST-ID              PIC 9(08).
           05  LA-TYPE                 PIC X(01).
           05  LA-AMOUNT               PIC 9(11)V99.
           05  LA-TENURE               PIC 9(03).
           05  LA-RATE                 PIC 9(02)V9(04).
           05  LA-EMI                  PIC 9(09)V99.
      *    APPLICANT'S DECLARED FIGURES
           05  LA-MONTHLY-INCOME       PIC 9(09)V99.
           05  LA-EXISTING-EMI         PIC 9(09)V99.
           05  LA-BUREAU-SCORE         PIC 9(03).
      *    ASSESSMENT
           05  LA-FOIR-PCT             PIC 9(03)V99.
           05  LA-SCORE                PIC 9(03).
           05  LA-GRADE                PIC X(01).
           05  LA-LEVEL                PIC X(01).
               88  LA-LEVEL-BRANCH               VALUE 'M'.
               88  LA-LEVEL-CREDIT               VALUE 'A'.
           05  LA-STATUS               PIC X(01).
               88  LA-UNDER-REVIEW               VALUE 'U'.
               88  LA-APPROVED                   VALUE 'A'.
               88  LA-REJECTED                   VALUE 'R'.
               88  LA-DISBURSED                  VALUE 'D'.
               88  LA-WITHDRAWN                  VALUE 'W'.
           05  LA-REASON               PIC X(40).
           05  LA-ENTERED-BY           PIC X(08).
           05  LA-ENTRY-DATE           PIC 9(08).
           05  LA-DECIDED-BY           PIC X(08).
           05  LA-DECISION-DATE        PIC 9(08).
           05  LA-SANCTION-VALID       PIC 9(08).
           05  LA-LOAN-NO              PIC 9(10).
           05  FILLER                  PIC X(10).
