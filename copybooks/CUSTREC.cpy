      *================================================================*
      * COPYBOOK : CUSTREC                                             *
      * PURPOSE  : CUSTOMER MASTER / CIF (INDEXED, KEY = CUST-ID)      *
      * NOTE     : ONLY THE LAST 4 DIGITS OF AADHAAR ARE KEPT, AS PER  *
      *            UIDAI GUIDANCE ON STORING AADHAAR NUMBERS.          *
      *================================================================*
       01  CUST-RECORD.
           05  CUST-ID                 PIC 9(08).
           05  CUST-NAME               PIC X(40).
           05  CUST-DOB                PIC 9(08).
           05  CUST-GENDER             PIC X(01).
               88  CUST-GENDER-VALID             VALUES 'M' 'F' 'T'.
           05  CUST-PAN                PIC X(10).
           05  CUST-AADHAAR-LAST4      PIC X(04).
           05  CUST-MOBILE             PIC X(10).
           05  CUST-EMAIL              PIC X(40).
           05  CUST-ADDRESS.
               10  CUST-ADDR-LINE1     PIC X(40).
               10  CUST-ADDR-LINE2     PIC X(40).
               10  CUST-CITY           PIC X(20).
               10  CUST-STATE          PIC X(20).
               10  CUST-PINCODE        PIC 9(06).
           05  CUST-KYC-STATUS         PIC X(01).
               88  KYC-PENDING                   VALUE 'P'.
               88  KYC-VERIFIED                  VALUE 'V'.
               88  KYC-REJECTED                  VALUE 'R'.
           05  CUST-KYC-DATE           PIC 9(08).
           05  CUST-KYC-REMARKS        PIC X(30).
           05  CUST-RISK               PIC X(01).
               88  RISK-LOW                      VALUE 'L'.
               88  RISK-MEDIUM                   VALUE 'M'.
               88  RISK-HIGH                     VALUE 'H'.
               88  RISK-VALID                    VALUES 'L' 'M' 'H'.
           05  CUST-CREATED-DATE       PIC 9(08).
           05  CUST-ACCT-COUNT         PIC 9(01).
           05  CUST-ACCOUNTS           PIC 9(10) OCCURS 5 TIMES.
           05  FILLER                  PIC X(20).
