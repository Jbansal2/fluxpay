      *================================================================*
      * COPYBOOK : FRAUDREC                                            *
      * PURPOSE  : FRAUD ALERT (INDEXED, KEY = FA-KEY). THE KEY IS     *
      *            DATE + ACCOUNT + RULE, SO RE-RUNNING THE SCAN FOR   *
      *            A DAY NEVER RAISES THE SAME ALERT TWICE.            *
      *================================================================*
       01  FRAUD-RECORD.
           05  FA-KEY.
               10  FA-DATE             PIC 9(08).
               10  FA-ACCT-NO          PIC 9(10).
               10  FA-RULE             PIC X(02).
                   88  FA-VELOCITY               VALUE 'VL'.
                   88  FA-ODD-HOURS              VALUE 'OH'.
                   88  FA-NEW-PAYEE              VALUE 'NP'.
                   88  FA-DORMANT-WAKE           VALUE 'DW'.
                   88  FA-PASS-THROUGH           VALUE 'PT'.
           05  FA-AMOUNT               PIC 9(11)V99.
           05  FA-DETAIL               PIC X(50).
           05  FA-STATUS               PIC X(01).
               88  FA-OPEN                       VALUE 'O'.
               88  FA-GENUINE                    VALUE 'G'.
               88  FA-FRAUD                      VALUE 'F'.
           05  FA-REVIEWED-BY          PIC X(08).
           05  FA-REVIEW-DATE          PIC 9(08).
           05  FA-REMARKS              PIC X(30).
           05  FILLER                  PIC X(10).
