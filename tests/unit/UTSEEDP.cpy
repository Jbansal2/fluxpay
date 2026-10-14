      *================================================================*
      * COPYBOOK : UTSEEDP                                             *
      * PURPOSE  : WRITES ONE ACCOUNT STRAIGHT TO THE ACCOUNT MASTER   *
      *            (CREATING IT IF NEEDED) SO A UNIT TEST CAN START    *
      *            FROM A KNOWN STATE. THE FILE IS CLOSED AGAIN SO THE *
      *            MODULE UNDER TEST CAN OPEN IT.                      *
      *================================================================*
       UT-SEED-ACCOUNT.
           OPEN I-O ACCT-FILE
           IF UT-ACCT-FS = '35'
               OPEN OUTPUT ACCT-FILE
           END-IF
           INITIALIZE ACCT-RECORD
           MOVE UT-SEED-NO      TO ACCT-NO
           MOVE 'UNIT TEST HOLDER' TO ACCT-HOLDER-NAME
           MOVE UT-SEED-TYPE    TO ACCT-TYPE
           MOVE UT-SEED-STATUS  TO ACCT-STATUS
           MOVE UT-SEED-BALANCE TO ACCT-BALANCE
           MOVE UT-SEED-MIN     TO ACCT-MIN-BALANCE
           MOVE UT-SEED-OD      TO ACCT-OD-LIMIT
           MOVE UT-SEED-BRANCH  TO ACCT-BRANCH-CODE
           MOVE 20260101        TO ACCT-OPEN-DATE ACCT-LAST-TXN-DATE
           WRITE ACCT-RECORD
               INVALID KEY REWRITE ACCT-RECORD
           END-WRITE
           CLOSE ACCT-FILE.

      *    READS UT-SEED-NO BACK INTO ACCT-RECORD (FILE CLOSED AFTER)
       UT-READ-ACCOUNT.
           OPEN INPUT ACCT-FILE
           MOVE UT-SEED-NO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY INITIALIZE ACCT-RECORD
           END-READ
           CLOSE ACCT-FILE.
