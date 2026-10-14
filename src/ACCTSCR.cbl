       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTSCR.
      *================================================================*
      * PROGRAM  : ACCTSCR                                             *
      * PURPOSE  : CUSTOMER 360 SCREEN - A FULL-SCREEN, FORM-BASED     *
      *            VIEW OF ONE ACCOUNT BUILT WITH THE SCREEN SECTION   *
      *            (THE GNUCOBOL EQUIVALENT OF A CICS BMS MAP):        *
      *              - ACCOUNT DETAILS AND AVAILABLE BALANCE           *
      *              - LOANS, TERM DEPOSITS AND CARDS ON THE ACCOUNT   *
      *              - THE LAST FIVE TRANSACTIONS                      *
      *            KEYS: ENTER = LOOK UP THE ACCOUNT TYPED            *
      *                  F3    = RETURN TO THE MAIN MENU               *
      *            READ-ONLY: NOTHING IS UPDATED FROM THIS SCREEN.     *
      * FILES    : ACCTMAST, LOANMAST, DEPMAST, CARDMAST, TRANLOG      *
      *            (ALL INPUT)                                         *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS WS-CRT-STATUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE ASSIGN TO 'data/ACCTMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-ACCT-FS.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS LOAN-NO
               FILE STATUS IS WS-LOAN-FS.
           SELECT DEP-FILE ASSIGN TO 'data/DEPMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS DEP-NO
               FILE STATUS IS WS-DEP-FS.
           SELECT CARD-FILE ASSIGN TO 'data/CARDMAST.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CARD-NO
               FILE STATUS IS WS-CARD-FS.
           SELECT TRAN-FILE ASSIGN TO 'data/TRANLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRAN-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
           COPY ACCTREC.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  DEP-FILE.
           COPY DEPREC.
       FD  CARD-FILE.
           COPY CARDREC.
       FD  TRAN-FILE.
           COPY TRANREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.

       01  WS-CRT-STATUS               PIC 9(04) VALUE ZERO.
           88  KEY-ENTER                         VALUE 0000.
           88  KEY-F3                            VALUE 1003.
       01  WS-LOAN-FS                  PIC X(02).
       01  WS-DEP-FS                   PIC X(02).
       01  WS-CARD-FS                  PIC X(02).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-DONE-SW                  PIC X(01) VALUE 'N'.
           88  SCREEN-DONE                       VALUE 'Y'.

      * SCREEN FIELDS
       01  SC-ACCT-NO                  PIC 9(10) VALUE ZERO.
       01  SC-MESSAGE                  PIC X(60) VALUE SPACES.
       01  SC-NAME                     PIC X(40).
       01  SC-TYPE                     PIC X(08).
       01  SC-STATUS                   PIC X(08).
       01  SC-BRANCH                   PIC X(06).
       01  SC-OPENED                   PIC 9999/99/99.
       01  SC-LAST-TXN                 PIC 9999/99/99.
       01  SC-BALANCE                  PIC -,---,---,---,--9.99.
       01  SC-AVAILABLE                PIC -,---,---,---,--9.99.
       01  SC-OD-LIMIT                 PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  SC-LOAN-CNT                 PIC Z9.
       01  SC-LOAN-OUT                 PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  SC-DEP-CNT                  PIC Z9.
       01  SC-DEP-AMT                  PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  SC-CARD-CNT                 PIC Z9.
       01  SC-TXN-LINES.
           05  SC-TXN-LINE             PIC X(70) OCCURS 5 TIMES.

      * WORK
       01  WS-LOAN-CNT                 PIC 9(02).
       01  WS-LOAN-OUT                 PIC 9(13)V99.
       01  WS-DEP-CNT                  PIC 9(02).
       01  WS-DEP-AMT                  PIC 9(13)V99.
       01  WS-CARD-CNT                 PIC 9(02).
       01  WS-AVAILABLE                PIC S9(13)V99.
       01  WS-RING.
           05  WS-RING-LINE            PIC X(70) OCCURS 5 TIMES.
       01  WS-RING-NEXT                PIC 9(01).
       01  WS-RING-CNT                 PIC 9(05).
       01  WS-I                        PIC 9(01).
       01  WS-J                        PIC 9(01).
       01  WS-E-AMT                    PIC Z,ZZZ,ZZZ,ZZ9.99.
       01  WS-E-DATE                   PIC 9999/99/99.
       01  WS-DR-CR                    PIC X(02).

       SCREEN SECTION.
       01  SCR-ACCOUNT-360.
           05  BLANK SCREEN.
           05  LINE 1 COL 2  VALUE 'CORE BANKING SYSTEM'
                             FOREGROUND-COLOR 6 HIGHLIGHT.
           05  LINE 1 COL 50 VALUE 'CUSTOMER 360 VIEW'
                             FOREGROUND-COLOR 6.
           05  LINE 2 COL 2  PIC X(78) FROM ALL '-'.
           05  LINE 3 COL 2  VALUE 'ACCOUNT NO:'.
           05  LINE 3 COL 14 PIC 9(10) USING SC-ACCT-NO
                             REVERSE-VIDEO.
           05  LINE 3 COL 30 VALUE 'ENTER=LOOK UP  F3=EXIT'
                             FOREGROUND-COLOR 3.
           05  LINE 5 COL 2  VALUE 'HOLDER  :'.
           05  LINE 5 COL 12 PIC X(40) FROM SC-NAME HIGHLIGHT.
           05  LINE 6 COL 2  VALUE 'TYPE    :'.
           05  LINE 6 COL 12 PIC X(08) FROM SC-TYPE.
           05  LINE 6 COL 30 VALUE 'STATUS :'.
           05  LINE 6 COL 39 PIC X(08) FROM SC-STATUS.
           05  LINE 6 COL 55 VALUE 'BRANCH:'.
           05  LINE 6 COL 63 PIC X(06) FROM SC-BRANCH.
           05  LINE 7 COL 2  VALUE 'OPENED  :'.
           05  LINE 7 COL 12 PIC X(10) FROM SC-OPENED.
           05  LINE 7 COL 30 VALUE 'LAST CUSTOMER ACTIVITY:'.
           05  LINE 7 COL 54 PIC X(10) FROM SC-LAST-TXN.
           05  LINE 9 COL 2  VALUE 'BALANCE   :'.
           05  LINE 9 COL 14 PIC X(21) FROM SC-BALANCE
                             FOREGROUND-COLOR 2 HIGHLIGHT.
           05  LINE 9 COL 40 VALUE 'AVAILABLE :'.
           05  LINE 9 COL 52 PIC X(21) FROM SC-AVAILABLE.
           05  LINE 10 COL 2 VALUE 'OD LIMIT  :'.
           05  LINE 10 COL 14 PIC X(17) FROM SC-OD-LIMIT.
           05  LINE 12 COL 2 VALUE 'LOANS     :'.
           05  LINE 12 COL 14 PIC X(02) FROM SC-LOAN-CNT.
           05  LINE 12 COL 18 VALUE 'OUTSTANDING'.
           05  LINE 12 COL 30 PIC X(17) FROM SC-LOAN-OUT.
           05  LINE 13 COL 2 VALUE 'DEPOSITS  :'.
           05  LINE 13 COL 14 PIC X(02) FROM SC-DEP-CNT.
           05  LINE 13 COL 18 VALUE 'HELD'.
           05  LINE 13 COL 30 PIC X(17) FROM SC-DEP-AMT.
           05  LINE 14 COL 2 VALUE 'CARDS     :'.
           05  LINE 14 COL 14 PIC X(02) FROM SC-CARD-CNT.
           05  LINE 16 COL 2 VALUE 'LAST 5 TRANSACTIONS'
                             FOREGROUND-COLOR 6.
           05  LINE 17 COL 2 PIC X(70) FROM SC-TXN-LINE(1).
           05  LINE 18 COL 2 PIC X(70) FROM SC-TXN-LINE(2).
           05  LINE 19 COL 2 PIC X(70) FROM SC-TXN-LINE(3).
           05  LINE 20 COL 2 PIC X(70) FROM SC-TXN-LINE(4).
           05  LINE 21 COL 2 PIC X(70) FROM SC-TXN-LINE(5).
           05  LINE 23 COL 2 PIC X(78) FROM ALL '-'.
           05  LINE 24 COL 2 PIC X(60) FROM SC-MESSAGE
                             FOREGROUND-COLOR 4 HIGHLIGHT.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 'N' TO WS-DONE-SW
           PERFORM 1000-CLEAR-PANEL
           MOVE 'TYPE AN ACCOUNT NUMBER AND PRESS ENTER' TO SC-MESSAGE
           PERFORM UNTIL SCREEN-DONE
               DISPLAY SCR-ACCOUNT-360
               ACCEPT SCR-ACCOUNT-360
               EVALUATE TRUE
                   WHEN KEY-F3
                       SET SCREEN-DONE TO TRUE
                   WHEN KEY-ENTER
                       PERFORM 2000-LOAD-ACCOUNT
                   WHEN OTHER
                       MOVE 'USE ENTER OR F3' TO SC-MESSAGE
               END-EVALUATE
           END-PERFORM
           GOBACK.

       1000-CLEAR-PANEL.
           MOVE SPACES TO SC-NAME SC-TYPE SC-STATUS SC-BRANCH
                          SC-TXN-LINES
           MOVE ZERO TO SC-OPENED SC-LAST-TXN SC-BALANCE SC-AVAILABLE
                        SC-OD-LIMIT SC-LOAN-CNT SC-LOAN-OUT SC-DEP-CNT
                        SC-DEP-AMT SC-CARD-CNT.

      *----------------------------------------------------------------*
      * GATHER EVERYTHING FOR THE ACCOUNT                              *
      *----------------------------------------------------------------*
       2000-LOAD-ACCOUNT.
           PERFORM 1000-CLEAR-PANEL
           OPEN INPUT ACCT-FILE
           IF NOT ACCT-FS-OK
               MOVE 'ACCOUNT MASTER NOT AVAILABLE' TO SC-MESSAGE
               EXIT PARAGRAPH
           END-IF
           MOVE SC-ACCT-NO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   MOVE 'ACCOUNT NOT FOUND' TO SC-MESSAGE
                   CLOSE ACCT-FILE
                   EXIT PARAGRAPH
           END-READ
           CLOSE ACCT-FILE
           PERFORM 2100-ACCOUNT-PART
           PERFORM 2200-LOANS-PART
           PERFORM 2300-DEPOSITS-PART
           PERFORM 2400-CARDS-PART
           PERFORM 2500-TRANSACTIONS-PART
           MOVE 'ACCOUNT LOADED - ENTER ANOTHER NUMBER OR F3'
                                                 TO SC-MESSAGE.

       2100-ACCOUNT-PART.
           MOVE ACCT-HOLDER-NAME TO SC-NAME
           MOVE ACCT-BRANCH-CODE TO SC-BRANCH
           IF ACCT-SAVINGS
               MOVE 'SAVINGS' TO SC-TYPE
               COMPUTE WS-AVAILABLE = ACCT-BALANCE - ACCT-MIN-BALANCE
           ELSE
               MOVE 'CURRENT' TO SC-TYPE
               COMPUTE WS-AVAILABLE = ACCT-BALANCE + ACCT-OD-LIMIT
           END-IF
           IF WS-AVAILABLE < ZERO
               MOVE ZERO TO WS-AVAILABLE
           END-IF
           EVALUATE TRUE
               WHEN ACCT-ACTIVE  MOVE 'ACTIVE'  TO SC-STATUS
               WHEN ACCT-FROZEN  MOVE 'FROZEN'  TO SC-STATUS
               WHEN ACCT-CLOSED  MOVE 'CLOSED'  TO SC-STATUS
               WHEN ACCT-DORMANT MOVE 'DORMANT' TO SC-STATUS
           END-EVALUATE
           MOVE ACCT-OPEN-DATE     TO SC-OPENED
           MOVE ACCT-LAST-TXN-DATE TO SC-LAST-TXN
           MOVE ACCT-BALANCE       TO SC-BALANCE
           MOVE WS-AVAILABLE       TO SC-AVAILABLE
           MOVE ACCT-OD-LIMIT      TO SC-OD-LIMIT.

       2200-LOANS-PART.
           MOVE ZERO TO WS-LOAN-CNT WS-LOAN-OUT
           OPEN INPUT LOAN-FILE
           IF WS-LOAN-FS = '00'
               MOVE 'N' TO WS-EOF-SW
               PERFORM UNTIL END-OF-FILE
                   READ LOAN-FILE NEXT RECORD
                       AT END SET END-OF-FILE TO TRUE
                       NOT AT END
                           IF LOAN-ACCT-NO = SC-ACCT-NO
                              AND NOT LOAN-CLOSED
                               ADD 1 TO WS-LOAN-CNT
                               ADD LOAN-OUTSTANDING TO WS-LOAN-OUT
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE LOAN-FILE
           END-IF
           MOVE WS-LOAN-CNT TO SC-LOAN-CNT
           MOVE WS-LOAN-OUT TO SC-LOAN-OUT.

       2300-DEPOSITS-PART.
           MOVE ZERO TO WS-DEP-CNT WS-DEP-AMT
           OPEN INPUT DEP-FILE
           IF WS-DEP-FS = '00'
               MOVE 'N' TO WS-EOF-SW
               PERFORM UNTIL END-OF-FILE
                   READ DEP-FILE NEXT RECORD
                       AT END SET END-OF-FILE TO TRUE
                       NOT AT END
                           IF DEP-ACCT-NO = SC-ACCT-NO AND DEP-ACTIVE
                               ADD 1 TO WS-DEP-CNT
                               ADD DEP-TOTAL-DEPOSITED TO WS-DEP-AMT
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE DEP-FILE
           END-IF
           MOVE WS-DEP-CNT TO SC-DEP-CNT
           MOVE WS-DEP-AMT TO SC-DEP-AMT.

       2400-CARDS-PART.
           MOVE ZERO TO WS-CARD-CNT
           OPEN INPUT CARD-FILE
           IF WS-CARD-FS = '00'
               MOVE 'N' TO WS-EOF-SW
               PERFORM UNTIL END-OF-FILE
                   READ CARD-FILE NEXT RECORD
                       AT END SET END-OF-FILE TO TRUE
                       NOT AT END
                           IF CARD-ACCT-NO = SC-ACCT-NO
                              AND (CARD-ACTIVE OR CARD-PIN-LOCKED)
                               ADD 1 TO WS-CARD-CNT
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CARD-FILE
           END-IF
           MOVE WS-CARD-CNT TO SC-CARD-CNT.

      *    ONE PASS OVER THE LOG KEEPING A RING OF THE LAST FIVE LINES
       2500-TRANSACTIONS-PART.
           MOVE SPACES TO WS-RING
           MOVE 1 TO WS-RING-NEXT
           MOVE ZERO TO WS-RING-CNT
           OPEN INPUT TRAN-FILE
           IF NOT TRAN-FS-OK
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ TRAN-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF TRAN-ACCT-NO = SC-ACCT-NO
                           PERFORM 2510-ADD-TO-RING
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TRAN-FILE
      *    NEWEST FIRST ON THE SCREEN
           MOVE WS-RING-NEXT TO WS-J
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               IF WS-J = 1
                   MOVE 5 TO WS-J
               ELSE
                   SUBTRACT 1 FROM WS-J
               END-IF
               MOVE WS-RING-LINE(WS-J) TO SC-TXN-LINE(WS-I)
           END-PERFORM.

       2510-ADD-TO-RING.
           IF TRAN-CREDIT-TYPE
               MOVE 'CR' TO WS-DR-CR
           ELSE
               MOVE 'DR' TO WS-DR-CR
           END-IF
           MOVE TRAN-DATE   TO WS-E-DATE
           MOVE TRAN-AMOUNT TO WS-E-AMT
           MOVE SPACES TO WS-RING-LINE(WS-RING-NEXT)
           STRING WS-E-DATE '  ' TRAN-TYPE '  ' WS-E-AMT ' ' WS-DR-CR
                  '  ' TRAN-NARRATION
                  DELIMITED BY SIZE INTO WS-RING-LINE(WS-RING-NEXT)
           END-STRING
           ADD 1 TO WS-RING-CNT
           IF WS-RING-NEXT = 5
               MOVE 1 TO WS-RING-NEXT
           ELSE
               ADD 1 TO WS-RING-NEXT
           END-IF.
