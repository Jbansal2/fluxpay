      *================================================================*
      * COPYBOOK : CMPREC                                              *
      * PURPOSE  : CUSTOMER COMPLAINT / SERVICE REQUEST (INDEXED, KEY  *
      *            = TICKET NO). STATUS O OPEN, E ESCALATED, R         *
      *            RESOLVED (AWAITING CUSTOMER), C CLOSED, J REJECTED. *
      *            LEVEL 1 BRANCH, 2 REGIONAL, 3 PRINCIPAL NODAL       *
      *            OFFICER.                                            *
      * LENGTH   : 300 BYTES                                           *
      *================================================================*
       01  CMP-RECORD.
           05  CMP-NO                  PIC 9(08).
           05  CMP-TYPE                PIC X(01).
               88  CMP-COMPLAINT                 VALUE 'C'.
               88  CMP-SERVICE-REQ               VALUE 'S'.
           05  CMP-CATEGORY            PIC X(04).
               88  CMP-ATM-FAILED                VALUE 'ATMF'.
           05  CMP-CHANNEL             PIC X(01).
           05  CMP-CUST-ID             PIC 9(08).
           05  CMP-ACCT-NO             PIC 9(10).
           05  CMP-BRANCH              PIC X(06).
           05  CMP-DESC                PIC X(60).
      *    FOR A FAILED ATM / CARD DEBIT: DATE AND AMOUNT DISPUTED
           05  CMP-TXN-DATE            PIC 9(08).
           05  CMP-AMOUNT              PIC 9(09)V99.
           05  CMP-OPEN-DATE           PIC 9(08).
           05  CMP-OPEN-TIME           PIC 9(06).
           05  CMP-SLA-DAYS            PIC 9(03).
           05  CMP-DUE-DATE            PIC 9(08).
           05  CMP-LEVEL               PIC 9(01).
           05  CMP-STATUS              PIC X(01).
               88  CMP-OPEN                      VALUE 'O'.
               88  CMP-ESCALATED                 VALUE 'E'.
               88  CMP-RESOLVED                  VALUE 'R'.
               88  CMP-CLOSED                    VALUE 'C'.
               88  CMP-REJECTED                  VALUE 'J'.
               88  CMP-PENDING                   VALUES 'O' 'E'.
               88  CMP-DISPOSED                  VALUES 'R' 'C' 'J'.
           05  CMP-LOGGED-BY           PIC X(08).
           05  CMP-ASSIGNED-TO         PIC X(08).
           05  CMP-RESOLUTION          PIC X(60).
           05  CMP-RESOLVED-DATE       PIC 9(08).
           05  CMP-RESOLVED-BY         PIC X(08).
           05  CMP-REFUND-AMT          PIC 9(09)V99.
           05  CMP-COMP-AMT            PIC 9(09)V99.
           05  CMP-REOPEN-CNT          PIC 9(01).
           05  CMP-ESCALATED-DATE      PIC 9(08).
           05  FILLER                  PIC X(33).
