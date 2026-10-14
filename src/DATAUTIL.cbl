       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATAUTIL.
      *================================================================*
      * PROGRAM  : DATAUTIL                                            *
      * PURPOSE  : DATA UTILITIES (ADMIN / BATCH)                      *
      *              1 - EXPORT ACCOUNT MASTER TO CSV                  *
      *                  (data/ACCTEXP.CSV)                            *
      *              2 - IMPORT / MIGRATE ACCOUNTS FROM CSV            *
      *                  (data/ACCTIMP.CSV -> ACCTMAST, REJECTS TO     *
      *                  data/ACCTREJ.CSV WITH A REASON)               *
      *                  CSV LAYOUT: NAME,TYPE,BALANCE,PHONE,BRANCH    *
      *              3 - CONTROL TOTALS (RECORD COUNT, BALANCE TOTAL,  *
      *                  ACCOUNT-NUMBER HASH TOTAL) FOR BACKUP CHECKS  *
      *              4 - RECONCILE MASTER BALANCES WITH THE LAST       *
      *                  BALANCE IN THE TRANSACTION LOG (SORT BY       *
      *                  ACCOUNT, DUPLICATES KEPT IN ORDER)            *
      * FILES    : ACCTMAST (I-O), TRANLOG, CSV FILES, SORTWK          *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE ASSIGN TO 'data/ACCTMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-ACCT-FS.
           SELECT TRAN-FILE ASSIGN TO 'data/TRANLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRAN-FS.
           SELECT EXP-FILE ASSIGN TO 'data/ACCTEXP.CSV'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CSV-FS.
           SELECT IMP-FILE ASSIGN TO 'data/ACCTIMP.CSV'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CSV-FS.
           SELECT REJ-FILE ASSIGN TO 'data/ACCTREJ.CSV'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-REJ-FS.
           SELECT SORT-FILE ASSIGN TO 'data/SORTWK2.TMP'.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
           COPY ACCTREC.
       FD  TRAN-FILE.
           COPY TRANREC.
       FD  EXP-FILE.
       01  EXP-LINE                    PIC X(150).
       FD  IMP-FILE.
       01  IMP-LINE                    PIC X(150).
       FD  REJ-FILE.
       01  REJ-LINE                    PIC X(200).
       SD  SORT-FILE.
       01  SORT-RECORD.
           05  SR-ACCT-NO              PIC 9(10).
           05  SR-BAL-AFTER            PIC S9(13)V99.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY GLPARM.
           COPY GLCODES.

       01  WS-CSV-FS                   PIC X(02).
       01  WS-REJ-FS                   PIC X(02).
       01  WS-CHOICE                   PIC X(01).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-RETURN-CODE              PIC 9(02) VALUE ZERO.

      * EXPORT
       01  WS-E-BAL                    PIC -(13)9.99.
       01  WS-E-MINBAL                 PIC -(9)9.99.
       01  WS-E-RATE                   PIC 9(02).9(04).
       01  WS-CSV-OUT                  PIC X(150).

      * IMPORT
       01  WS-CSV-FIELDS.
           05  CSV-NAME                PIC X(40).
           05  CSV-TYPE                PIC X(05).
           05  CSV-BALANCE             PIC X(16).
           05  CSV-PHONE               PIC X(12).
           05  CSV-BRANCH              PIC X(08).
       01  WS-FIELD-COUNT              PIC 9(02).
       01  WS-REJECT-REASON            PIC X(40).
       01  WS-BAL-NUM                  PIC S9(13)V99.
       01  WS-NEXT-ACCT-NO             PIC 9(10).
       01  WS-LINE-NO                  PIC 9(07).
       01  WS-TRAN-SEQ                 PIC 9(04).

      * TOTALS
       01  WS-CTL.
           05  WS-CTL-COUNT            PIC 9(09).
           05  WS-CTL-ACTIVE           PIC 9(09).
           05  WS-CTL-BALANCE          PIC S9(15)V99.
           05  WS-CTL-HASH             PIC 9(18).
           05  WS-IMP-READ             PIC 9(07).
           05  WS-IMP-LOADED           PIC 9(07).
           05  WS-IMP-REJECTED         PIC 9(07).
           05  WS-IMP-AMOUNT           PIC S9(15)V99.
           05  WS-REC-CHECKED          PIC 9(07).
           05  WS-REC-MISMATCH         PIC 9(07).
           05  WS-REC-NO-HISTORY       PIC 9(07).

      * RECONCILIATION
       01  WS-SORT-EOF-SW              PIC X(01).
           88  END-OF-SORT                       VALUE 'Y'.
       01  WS-PREV-ACCT                PIC 9(10).
       01  WS-LAST-BAL                 PIC S9(13)V99.
       01  WS-SEEN-SW                  PIC X(01).
           88  ACCOUNT-SEEN                      VALUE 'Y'.
       01  WS-MASTER-EOF-SW            PIC X(01).
           88  END-OF-MASTER                     VALUE 'Y'.

       01  WS-D-CNT                    PIC ZZZ,ZZZ,ZZ9.
       01  WS-D-AMT                    PIC -ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-D-AMT2                   PIC -ZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-D-HASH                   PIC Z(17)9.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE ZERO TO WS-RETURN-CODE
           INITIALIZE WS-CTL
           DISPLAY ' '
           DISPLAY '------------- DATA UTILITIES -------------'
           DISPLAY ' 1. EXPORT ACCOUNTS TO CSV'
           DISPLAY ' 2. IMPORT ACCOUNTS FROM CSV'
           DISPLAY ' 3. CONTROL TOTALS'
           DISPLAY ' 4. RECONCILE MASTER WITH TRANSACTION LOG'
           DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
           ACCEPT WS-CHOICE
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           EVALUATE WS-CHOICE
               WHEN '1' PERFORM 1000-EXPORT
               WHEN '2' PERFORM 2000-IMPORT
               WHEN '3' PERFORM 3000-CONTROL-TOTALS
               WHEN '4' PERFORM 4000-RECONCILE
               WHEN OTHER
                   DISPLAY 'INVALID CHOICE.'
                   MOVE 8 TO WS-RETURN-CODE
           END-EVALUATE
           MOVE WS-RETURN-CODE TO RETURN-CODE
           GOBACK.

      *================================================================*
      * 1 - EXPORT                                                     *
      *================================================================*
       1000-EXPORT.
           OPEN INPUT ACCT-FILE
           IF NOT ACCT-FS-OK
               DISPLAY 'ACCOUNT MASTER NOT AVAILABLE, FS=' WS-ACCT-FS
               MOVE 12 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           OPEN OUTPUT EXP-FILE
           MOVE SPACES TO EXP-LINE
           STRING 'ACCT_NO,NAME,TYPE,STATUS,BALANCE,MIN_BALANCE,'
                  'RATE,OPEN_DATE,LAST_TXN_DATE,PHONE,BRANCH'
                  DELIMITED BY SIZE INTO EXP-LINE
           END-STRING
           WRITE EXP-LINE
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ ACCT-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END PERFORM 1100-WRITE-CSV-LINE
               END-READ
           END-PERFORM
           CLOSE ACCT-FILE EXP-FILE
           MOVE WS-CTL-COUNT TO WS-D-CNT
           MOVE WS-CTL-BALANCE TO WS-D-AMT
           DISPLAY 'EXPORTED ' FUNCTION TRIM(WS-D-CNT)
                   ' ACCOUNTS, BALANCE TOTAL ' FUNCTION TRIM(WS-D-AMT)
           DISPLAY 'FILE: data/ACCTEXP.CSV'.

      *    NAMES ARE QUOTED BECAUSE THEY MAY CONTAIN COMMAS
       1100-WRITE-CSV-LINE.
           ADD 1 TO WS-CTL-COUNT
           ADD ACCT-BALANCE TO WS-CTL-BALANCE
           MOVE ACCT-BALANCE  TO WS-E-BAL
           MOVE ACCT-INT-RATE TO WS-E-RATE
           MOVE ACCT-MIN-BALANCE TO WS-E-MINBAL
           MOVE SPACES TO WS-CSV-OUT
           STRING ACCT-NO ',"'
                  FUNCTION TRIM(ACCT-HOLDER-NAME) '",'
                  ACCT-TYPE ',' ACCT-STATUS ','
                  FUNCTION TRIM(WS-E-BAL) ','
                  FUNCTION TRIM(WS-E-MINBAL) ','
                  WS-E-RATE ','
                  ACCT-OPEN-DATE ',' ACCT-LAST-TXN-DATE ','
                  FUNCTION TRIM(ACCT-PHONE) ','
                  FUNCTION TRIM(ACCT-BRANCH-CODE)
                  DELIMITED BY SIZE INTO WS-CSV-OUT
           END-STRING
           MOVE WS-CSV-OUT TO EXP-LINE
           WRITE EXP-LINE.

      *================================================================*
      * 2 - IMPORT                                                     *
      *================================================================*
       2000-IMPORT.
           OPEN INPUT IMP-FILE
           IF WS-CSV-FS NOT = '00'
               DISPLAY 'NO IMPORT FILE (data/ACCTIMP.CSV).'
               MOVE 12 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           OPEN I-O ACCT-FILE
           IF ACCT-FS-NOFILE
               OPEN OUTPUT ACCT-FILE
               CLOSE ACCT-FILE
               OPEN I-O ACCT-FILE
           END-IF
           IF NOT ACCT-FS-OK
               DISPLAY 'ACCOUNT MASTER NOT AVAILABLE, FS=' WS-ACCT-FS
               CLOSE IMP-FILE
               MOVE 12 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           OPEN OUTPUT REJ-FILE
           OPEN EXTEND TRAN-FILE
           IF TRAN-FS-NOFILE
               OPEN OUTPUT TRAN-FILE
           END-IF
           PERFORM 2050-GET-NEXT-ACCT-NO
           MOVE ZERO TO WS-LINE-NO WS-TRAN-SEQ
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ IMP-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-LINE-NO
      *                FIRST LINE IS A HEADER IF IT STARTS 'NAME'
                       IF WS-LINE-NO = 1
                          AND FUNCTION UPPER-CASE(IMP-LINE(1:4))
                              = 'NAME'
                           CONTINUE
                       ELSE
                           PERFORM 2100-IMPORT-ONE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE IMP-FILE ACCT-FILE REJ-FILE TRAN-FILE

           MOVE WS-IMP-READ TO WS-D-CNT
           DISPLAY 'RECORDS READ     : ' WS-D-CNT
           MOVE WS-IMP-LOADED TO WS-D-CNT
           DISPLAY 'ACCOUNTS LOADED  : ' WS-D-CNT
           MOVE WS-IMP-REJECTED TO WS-D-CNT
           DISPLAY 'REJECTED         : ' WS-D-CNT
           MOVE WS-IMP-AMOUNT TO WS-D-AMT
           DISPLAY 'BALANCES LOADED  : ' WS-D-AMT
           IF WS-IMP-REJECTED > ZERO
               DISPLAY 'SEE data/ACCTREJ.CSV FOR REJECTED LINES.'
               MOVE 4 TO WS-RETURN-CODE
           END-IF.

       2050-GET-NEXT-ACCT-NO.
           MOVE 1000000000 TO WS-NEXT-ACCT-NO
           MOVE 9999999999 TO ACCT-NO
           START ACCT-FILE KEY IS LESS THAN OR EQUAL ACCT-NO
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ ACCT-FILE PREVIOUS RECORD
                       AT END CONTINUE
                       NOT AT END MOVE ACCT-NO TO WS-NEXT-ACCT-NO
                   END-READ
           END-START.

       2100-IMPORT-ONE.
           ADD 1 TO WS-IMP-READ
           MOVE SPACES TO WS-CSV-FIELDS WS-REJECT-REASON
           MOVE ZERO TO WS-FIELD-COUNT
           UNSTRING IMP-LINE DELIMITED BY ','
               INTO CSV-NAME CSV-TYPE CSV-BALANCE CSV-PHONE CSV-BRANCH
               TALLYING IN WS-FIELD-COUNT
           END-UNSTRING
           PERFORM 2200-VALIDATE-CSV
           IF WS-REJECT-REASON NOT = SPACES
               ADD 1 TO WS-IMP-REJECTED
               MOVE SPACES TO REJ-LINE
               STRING FUNCTION TRIM(IMP-LINE TRAILING) ',REJECTED: '
                      WS-REJECT-REASON
                      DELIMITED BY SIZE INTO REJ-LINE
               END-STRING
               WRITE REJ-LINE
               EXIT PARAGRAPH
           END-IF
           PERFORM 2300-LOAD-ACCOUNT.

       2200-VALIDATE-CSV.
           MOVE FUNCTION UPPER-CASE(CSV-TYPE) TO CSV-TYPE
           EVALUATE TRUE
               WHEN WS-FIELD-COUNT < 5
                   MOVE 'EXPECTED 5 FIELDS' TO WS-REJECT-REASON
               WHEN CSV-NAME = SPACES
                   MOVE 'NAME MISSING' TO WS-REJECT-REASON
               WHEN CSV-TYPE NOT = 'S' AND CSV-TYPE NOT = 'C'
                   MOVE 'TYPE MUST BE S OR C' TO WS-REJECT-REASON
               WHEN FUNCTION TEST-NUMVAL(CSV-BALANCE) NOT = ZERO
                   MOVE 'BALANCE NOT NUMERIC' TO WS-REJECT-REASON
               WHEN CSV-PHONE(1:10) NOT NUMERIC
                 OR CSV-PHONE(11:2) NOT = SPACES
                   MOVE 'PHONE MUST BE 10 DIGITS' TO WS-REJECT-REASON
               WHEN CSV-BRANCH = SPACES
                   MOVE 'BRANCH MISSING' TO WS-REJECT-REASON
           END-EVALUATE
           IF WS-REJECT-REASON NOT = SPACES
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-BAL-NUM = FUNCTION NUMVAL(CSV-BALANCE)
           IF WS-BAL-NUM < ZERO
               MOVE 'NEGATIVE BALANCE' TO WS-REJECT-REASON
           END-IF
           IF CSV-TYPE = 'S' AND WS-BAL-NUM < 1000
               MOVE 'SAVINGS BELOW MINIMUM 1000' TO WS-REJECT-REASON
           END-IF.

       2300-LOAD-ACCOUNT.
           ADD 1 TO WS-NEXT-ACCT-NO
           INITIALIZE ACCT-RECORD
           MOVE WS-NEXT-ACCT-NO TO ACCT-NO
           MOVE FUNCTION UPPER-CASE(CSV-NAME) TO ACCT-HOLDER-NAME
           MOVE CSV-TYPE(1:1)   TO ACCT-TYPE
           SET ACCT-ACTIVE      TO TRUE
           MOVE WS-BAL-NUM      TO ACCT-BALANCE
           MOVE CSV-PHONE(1:10) TO ACCT-PHONE
           MOVE FUNCTION UPPER-CASE(CSV-BRANCH) TO ACCT-BRANCH-CODE
           MOVE WS-CURR-DATE    TO ACCT-OPEN-DATE ACCT-LAST-TXN-DATE
                                   ACCT-LAST-INT-DATE
           IF ACCT-SAVINGS
               MOVE 1000.00 TO ACCT-MIN-BALANCE
               MOVE 3.5     TO ACCT-INT-RATE
           END-IF
           WRITE ACCT-RECORD
               INVALID KEY
                   ADD 1 TO WS-IMP-REJECTED
                   MOVE SPACES TO REJ-LINE
                   STRING FUNCTION TRIM(IMP-LINE TRAILING)
                          ',REJECTED: WRITE FAILED FS ' WS-ACCT-FS
                          DELIMITED BY SIZE INTO REJ-LINE
                   END-STRING
                   WRITE REJ-LINE
                   EXIT PARAGRAPH
           END-WRITE
           ADD 1 TO WS-IMP-LOADED
           ADD WS-BAL-NUM TO WS-IMP-AMOUNT
      *    OPENING BALANCE GOES IN THE LOG SO RECONCILIATION HOLDS
           IF WS-BAL-NUM > ZERO
               ADD 1 TO WS-TRAN-SEQ
               COMPUTE TRAN-ID = WS-CURR-DATE * 10000 + WS-TRAN-SEQ
               MOVE ACCT-NO      TO TRAN-ACCT-NO
               MOVE WS-CURR-DATE TO TRAN-DATE
               MOVE WS-CURR-TIME TO TRAN-TIME
               MOVE 'DP'         TO TRAN-TYPE
               MOVE WS-BAL-NUM   TO TRAN-AMOUNT TRAN-BAL-AFTER
               MOVE ZERO         TO TRAN-REF-ACCT
               MOVE 'MIGRATED OPENING BALANCE' TO TRAN-NARRATION
               WRITE TRAN-RECORD
      *        MIGRATED MONEY COMES FROM THE OLD SYSTEM, NOT THE TILL
               MOVE GLC-OPENING-CONTROL TO GP-DR-GL
               IF ACCT-SAVINGS
                   MOVE GLC-SAVINGS-DEP TO GP-CR-GL
               ELSE
                   MOVE GLC-CURRENT-DEP TO GP-CR-GL
               END-IF
               MOVE WS-BAL-NUM TO GP-AMOUNT
               MOVE ACCT-NO    TO GP-REF
               MOVE 'MIGRATED OPENING BALANCE' TO GP-NARRATION
               CALL 'GLPOST' USING GL-PARMS
           END-IF.

      *================================================================*
      * 3 - CONTROL TOTALS                                             *
      *================================================================*
       3000-CONTROL-TOTALS.
           OPEN INPUT ACCT-FILE
           IF NOT ACCT-FS-OK
               DISPLAY 'ACCOUNT MASTER NOT AVAILABLE, FS=' WS-ACCT-FS
               MOVE 12 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ ACCT-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1            TO WS-CTL-COUNT
                       ADD ACCT-BALANCE TO WS-CTL-BALANCE
                       ADD ACCT-NO      TO WS-CTL-HASH
                       IF ACCT-ACTIVE
                           ADD 1 TO WS-CTL-ACTIVE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACCT-FILE
           DISPLAY 'ACCOUNT MASTER CONTROL TOTALS - ' WS-CURR-DATE
           MOVE WS-CTL-COUNT TO WS-D-CNT
           DISPLAY 'RECORDS          : ' WS-D-CNT
           MOVE WS-CTL-ACTIVE TO WS-D-CNT
           DISPLAY 'ACTIVE           : ' WS-D-CNT
           MOVE WS-CTL-BALANCE TO WS-D-AMT
           DISPLAY 'BALANCE TOTAL    : ' WS-D-AMT
           MOVE WS-CTL-HASH TO WS-D-HASH
           DISPLAY 'ACCT NO HASH     : ' WS-D-HASH
           DISPLAY 'RECORD THESE BEFORE A BACKUP AND COMPARE AFTER A'
                   ' RESTORE.'.

      *================================================================*
      * 4 - RECONCILIATION                                             *
      *================================================================*
       4000-RECONCILE.
           DISPLAY 'RECONCILING MASTER BALANCES WITH TRANSACTION LOG'
           SORT SORT-FILE
               ON ASCENDING KEY SR-ACCT-NO
               WITH DUPLICATES IN ORDER
               INPUT PROCEDURE  IS 4100-RELEASE-TRANS
               OUTPUT PROCEDURE IS 4200-COMPARE
           MOVE WS-REC-CHECKED TO WS-D-CNT
           DISPLAY 'ACCOUNTS CHECKED : ' WS-D-CNT
           MOVE WS-REC-MISMATCH TO WS-D-CNT
           DISPLAY 'MISMATCHES       : ' WS-D-CNT
           MOVE WS-REC-NO-HISTORY TO WS-D-CNT
           DISPLAY 'NO LOG HISTORY   : ' WS-D-CNT
           IF WS-REC-MISMATCH > ZERO
               MOVE 8 TO WS-RETURN-CODE
           ELSE
               IF WS-REC-NO-HISTORY > ZERO
                   MOVE 4 TO WS-RETURN-CODE
               ELSE
                   DISPLAY 'MASTER AND LOG AGREE.'
               END-IF
           END-IF.

       4100-RELEASE-TRANS.
           OPEN INPUT TRAN-FILE
           IF NOT TRAN-FS-OK
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ TRAN-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       MOVE TRAN-ACCT-NO   TO SR-ACCT-NO
                       MOVE TRAN-BAL-AFTER TO SR-BAL-AFTER
                       RELEASE SORT-RECORD
               END-READ
           END-PERFORM
           CLOSE TRAN-FILE.

      *    MATCH/MERGE: SORTED LOG GROUPS AGAINST THE MASTER READ IN
      *    KEY ORDER. THE LAST LOG RECORD OF AN ACCOUNT (LOG ORDER IS
      *    KEPT BY THE SORT) HOLDS ITS CLOSING BALANCE.
       4200-COMPARE.
           MOVE 'N' TO WS-SORT-EOF-SW WS-SEEN-SW WS-MASTER-EOF-SW
           MOVE ZERO TO WS-PREV-ACCT
           OPEN INPUT ACCT-FILE
           IF ACCT-FS-OK
               PERFORM 4500-READ-MASTER
           ELSE
               SET END-OF-MASTER TO TRUE
           END-IF
           PERFORM UNTIL END-OF-SORT
               RETURN SORT-FILE
                   AT END SET END-OF-SORT TO TRUE
                   NOT AT END
                       IF ACCOUNT-SEEN AND SR-ACCT-NO NOT = WS-PREV-ACCT
                           PERFORM 4300-CHECK-ONE
                       END-IF
                       MOVE SR-ACCT-NO   TO WS-PREV-ACCT
                       MOVE SR-BAL-AFTER TO WS-LAST-BAL
                       MOVE 'Y' TO WS-SEEN-SW
               END-RETURN
           END-PERFORM
           IF ACCOUNT-SEEN
               PERFORM 4300-CHECK-ONE
           END-IF
      *    WHATEVER IS LEFT IN THE MASTER NEVER APPEARED IN THE LOG
           PERFORM UNTIL END-OF-MASTER
               PERFORM 4400-NO-HISTORY
               PERFORM 4500-READ-MASTER
           END-PERFORM
           CLOSE ACCT-FILE.

       4300-CHECK-ONE.
           ADD 1 TO WS-REC-CHECKED
           PERFORM UNTIL END-OF-MASTER OR ACCT-NO >= WS-PREV-ACCT
               PERFORM 4400-NO-HISTORY
               PERFORM 4500-READ-MASTER
           END-PERFORM
           IF END-OF-MASTER OR ACCT-NO > WS-PREV-ACCT
               ADD 1 TO WS-REC-MISMATCH
               DISPLAY '  ' WS-PREV-ACCT ' IN LOG BUT NOT IN MASTER'
               EXIT PARAGRAPH
           END-IF
           IF ACCT-BALANCE NOT = WS-LAST-BAL
               ADD 1 TO WS-REC-MISMATCH
               MOVE ACCT-BALANCE TO WS-D-AMT
               MOVE WS-LAST-BAL  TO WS-D-AMT2
               DISPLAY '  ' ACCT-NO ' MASTER ' FUNCTION TRIM(WS-D-AMT)
                       ' LOG ' FUNCTION TRIM(WS-D-AMT2)
           END-IF
           PERFORM 4500-READ-MASTER.

       4400-NO-HISTORY.
           IF ACCT-BALANCE NOT = ZERO
               ADD 1 TO WS-REC-NO-HISTORY
               MOVE ACCT-BALANCE TO WS-D-AMT
               DISPLAY '  ' ACCT-NO ' HAS BALANCE '
                       FUNCTION TRIM(WS-D-AMT) ' BUT NO LOG HISTORY'
           END-IF.

       4500-READ-MASTER.
           READ ACCT-FILE NEXT RECORD
               AT END SET END-OF-MASTER TO TRUE
           END-READ
      *    ANY OTHER READ ERROR ENDS THE MERGE RATHER THAN LOOPING
           IF NOT ACCT-FS-OK
               SET END-OF-MASTER TO TRUE
           END-IF.
