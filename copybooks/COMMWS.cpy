      *================================================================*
      * COPYBOOK : COMMWS                                              *
      * PURPOSE  : COMMON WORKING STORAGE - FILE STATUS, DATE/TIME     *
      *================================================================*
       01  WS-FILE-STATUSES.
           05  WS-ACCT-FS              PIC X(02).
               88  ACCT-FS-OK                    VALUE '00'.
               88  ACCT-FS-DUPKEY                VALUE '22'.
               88  ACCT-FS-NOTFOUND              VALUE '23'.
               88  ACCT-FS-NOFILE                VALUE '35'.
           05  WS-TRAN-FS              PIC X(02).
               88  TRAN-FS-OK                    VALUE '00'.
               88  TRAN-FS-EOF                   VALUE '10'.
               88  TRAN-FS-NOFILE                VALUE '35'.

       01  WS-CURRENT-DT.
           05  WS-CURR-DATE            PIC 9(08).
           05  WS-CURR-DATE-R REDEFINES WS-CURR-DATE.
               10  WS-CURR-YYYYMM      PIC 9(06).
               10  WS-CURR-DD          PIC 9(02).
           05  WS-CURR-TIME            PIC 9(06).
           05  FILLER                  PIC X(07).

       01  WS-MSG                      PIC X(70) VALUE SPACES.
