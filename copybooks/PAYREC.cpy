      *================================================================*
      * COPYBOOK : PAYREC                                              *
      * PURPOSE  : PAYMENT REGISTER (INDEXED, KEY = PAY-REF). ONE      *
      *            RECORD PER OUTWARD OR INWARD NEFT/RTGS/IMPS/UPI.    *
      *            PAY-REF = MODE LETTER + YYYYMMDD + 7-DIGIT SEQUENCE *
      *================================================================*
       01  PAY-RECORD.
           05  PAY-REF                 PIC X(16).
           05  PAY-MODE                PIC X(01).
               88  PAY-NEFT                      VALUE 'N'.
               88  PAY-RTGS                      VALUE 'R'.
               88  PAY-IMPS                      VALUE 'I'.
               88  PAY-UPI                       VALUE 'U'.
               88  PAY-MODE-VALID                VALUES 'N' 'R' 'I'
                                                        'U'.
               88  PAY-INSTANT                   VALUES 'I' 'U'.
           05  PAY-DIRECTION           PIC X(01).
               88  PAY-OUTWARD                   VALUE 'O'.
               88  PAY-INWARD                    VALUE 'I'.
           05  PAY-STATUS              PIC X(01).
               88  PAY-PENDING                   VALUE 'P'.
               88  PAY-SETTLED                   VALUE 'S'.
               88  PAY-RETURNED                  VALUE 'R'.
           05  PAY-OUR-ACCT            PIC 9(10).
           05  PAY-OTHER-NAME          PIC X(35).
           05  PAY-OTHER-ACCT          PIC X(18).
           05  PAY-OTHER-IFSC          PIC X(11).
           05  PAY-OTHER-VPA           PIC X(30).
           05  PAY-AMOUNT              PIC 9(11)V99.
           05  PAY-CHARGE              PIC 9(05)V99.
           05  PAY-GST                 PIC 9(05)V99.
           05  PAY-UTR                 PIC X(22).
           05  PAY-CREATED-DATE        PIC 9(08).
           05  PAY-CREATED-TIME        PIC 9(06).
           05  PAY-SETTLED-DATE        PIC 9(08).
           05  PAY-RETURN-REASON       PIC X(30).
           05  PAY-REMARKS             PIC X(30).
           05  FILLER                  PIC X(20).
