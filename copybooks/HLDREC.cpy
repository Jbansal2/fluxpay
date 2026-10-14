      *================================================================*
      * COPYBOOK : HLDREC                                              *
      * PURPOSE  : ACCOUNT RELATIONSHIPS (INDEXED, KEY = ACCOUNT +     *
      *            SEQUENCE). ONE ACCOUNT HAS:                         *
      *              SEQ 00    - HEADER WITH THE OPERATING MODE        *
      *              SEQ 01-03 - HOLDERS (01 IS ALWAYS THE PRIMARY)    *
      *              SEQ 90    - NOMINEE                               *
      *================================================================*
       01  HLD-RECORD.
           05  HLD-KEY.
               10  HLD-ACCT-NO         PIC 9(10).
               10  HLD-SEQ             PIC 9(02).
                   88  HLD-IS-HEADER             VALUE 00.
                   88  HLD-IS-HOLDER             VALUES 01 THRU 03.
                   88  HLD-IS-NOMINEE            VALUE 90.
           05  HLD-DATA.
      *        HEADER
               10  HLD-MODE            PIC X(01).
                   88  MODE-SINGLE               VALUE 'S'.
                   88  MODE-EITHER-SURVIVOR      VALUE 'E'.
                   88  MODE-FORMER-SURVIVOR      VALUE 'F'.
                   88  MODE-JOINTLY              VALUE 'J'.
                   88  MODE-VALID                VALUES 'S' 'E' 'F'
                                                        'J'.
      *        HOLDER
               10  HLD-CUST-ID         PIC 9(08).
               10  HLD-ROLE            PIC X(01).
                   88  HLD-PRIMARY               VALUE 'P'.
                   88  HLD-JOINT                 VALUE 'J'.
               10  HLD-ADDED-DATE      PIC 9(08).
      *        NOMINEE
               10  HLD-NOM-NAME        PIC X(40).
               10  HLD-NOM-RELATION    PIC X(15).
               10  HLD-NOM-DOB         PIC 9(08).
               10  HLD-NOM-GUARDIAN    PIC X(40).
               10  FILLER              PIC X(20).
