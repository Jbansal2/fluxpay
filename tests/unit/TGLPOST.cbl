       IDENTIFICATION DIVISION.
       PROGRAM-ID. TGLPOST.
      *================================================================*
      * UNIT TESTS FOR GLPOST - BALANCED ENTRIES, ALL-OR-NOTHING ON A  *
      * BAD GL, CHART SEEDING AND THE TWO-LINE JOURNAL                 *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GL-FILE ASSIGN TO 'data/GLMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
               RECORD KEY IS GL-CODE FILE STATUS IS WS-GL-FS.
           SELECT JRNL-FILE ASSIGN TO 'data/GLJRNL.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JRNL-FS.
       DATA DIVISION.
       FILE SECTION.
       FD  GL-FILE.
           COPY GLREC.
       FD  JRNL-FILE.
           COPY GLJREC.
       WORKING-STORAGE SECTION.
           COPY GLPARM.
           COPY UTWS.
       01  WS-GL-FS                    PIC X(02).
       01  WS-JRNL-FS                  PIC X(02).
       01  WS-EOF                      PIC X(01).
       01  WS-COUNT                    PIC 9(05).
       01  WS-SUM                      PIC S9(15)V99.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 'GLPOST' TO UT-SUITE

           MOVE 'FIRST ENTRY SEEDS THE LEDGER, RC 00' TO UT-NAME
           MOVE 1001 TO GP-DR-GL
           MOVE 2101 TO GP-CR-GL
           MOVE 1500.25 TO GP-AMOUNT
           MOVE 'UT1' TO GP-REF
           MOVE 'UNIT TEST' TO GP-NARRATION
           CALL 'GLPOST' USING GL-PARMS
           MOVE ZERO TO UT-EXPECTED-NUM
           MOVE GP-RC TO UT-ACTUAL-NUM
           PERFORM UT-ASSERT-NUM

           MOVE 'CHART HAS ALL 44 ACCOUNTS' TO UT-NAME
           PERFORM 8000-SCAN-LEDGER
           MOVE 44 TO UT-EXPECTED-NUM
           MOVE WS-COUNT TO UT-ACTUAL-NUM
           PERFORM UT-ASSERT-NUM

           MOVE 'LEDGER SUMS TO ZERO' TO UT-NAME
           MOVE ZERO TO UT-EXPECTED-NUM
           MOVE WS-SUM TO UT-ACTUAL-NUM
           PERFORM UT-ASSERT-NUM

           MOVE 'DEBIT SIDE UP (CASH +1,500.25)' TO UT-NAME
           MOVE 1001 TO GL-CODE
           PERFORM 8100-BALANCE
           MOVE 1500.25 TO UT-EXPECTED-NUM
           PERFORM UT-ASSERT-NUM

           MOVE 'CREDIT SIDE DOWN (SAVINGS -1,500.25)' TO UT-NAME
           MOVE 2101 TO GL-CODE
           PERFORM 8100-BALANCE
           MOVE -1500.25 TO UT-EXPECTED-NUM
           PERFORM UT-ASSERT-NUM

           MOVE 'SAME GL BOTH SIDES -> RC 20' TO UT-NAME
           MOVE 1001 TO GP-CR-GL
           CALL 'GLPOST' USING GL-PARMS
           MOVE 20 TO UT-EXPECTED-NUM
           MOVE GP-RC TO UT-ACTUAL-NUM
           PERFORM UT-ASSERT-NUM

           MOVE 'ZERO AMOUNT -> RC 20' TO UT-NAME
           MOVE 2101 TO GP-CR-GL
           MOVE ZERO TO GP-AMOUNT
           CALL 'GLPOST' USING GL-PARMS
           MOVE 20 TO UT-EXPECTED-NUM
           MOVE GP-RC TO UT-ACTUAL-NUM
           PERFORM UT-ASSERT-NUM

           MOVE 'UNKNOWN CREDIT GL -> RC 10' TO UT-NAME
           MOVE 9999 TO GP-CR-GL
           MOVE 100 TO GP-AMOUNT
           CALL 'GLPOST' USING GL-PARMS
           MOVE 10 TO UT-EXPECTED-NUM
           MOVE GP-RC TO UT-ACTUAL-NUM
           PERFORM UT-ASSERT-NUM

           MOVE 'BAD ENTRY CHANGES NOTHING (CASH STILL 1,500.25)'
             TO UT-NAME
           MOVE 1001 TO GL-CODE
           PERFORM 8100-BALANCE
           MOVE 1500.25 TO UT-EXPECTED-NUM
           PERFORM UT-ASSERT-NUM

           MOVE 'ENTRY TO RESERVES AND P&L GLS WORKS' TO UT-NAME
           MOVE 3003 TO GP-DR-GL
           MOVE 3004 TO GP-CR-GL
           MOVE 250 TO GP-AMOUNT
           CALL 'GLPOST' USING GL-PARMS
           MOVE ZERO TO UT-EXPECTED-NUM
           MOVE GP-RC TO UT-ACTUAL-NUM
           PERFORM UT-ASSERT-NUM

           MOVE 'JOURNAL HAS 2 LINES PER GOOD ENTRY' TO UT-NAME
           MOVE ZERO TO WS-COUNT WS-SUM
           OPEN INPUT JRNL-FILE
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ JRNL-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-COUNT
                       IF GJ-DR-CR = 'D'
                           ADD GJ-AMOUNT TO WS-SUM
                       ELSE
                           SUBTRACT GJ-AMOUNT FROM WS-SUM
                       END-IF
               END-READ
           END-PERFORM
           CLOSE JRNL-FILE
           MOVE 4 TO UT-EXPECTED-NUM
           MOVE WS-COUNT TO UT-ACTUAL-NUM
           PERFORM UT-ASSERT-NUM

           MOVE 'JOURNAL DEBITS EQUAL CREDITS' TO UT-NAME
           MOVE ZERO TO UT-EXPECTED-NUM
           MOVE WS-SUM TO UT-ACTUAL-NUM
           PERFORM UT-ASSERT-NUM

           PERFORM UT-FINISH
           STOP RUN.

       8000-SCAN-LEDGER.
           MOVE ZERO TO WS-COUNT WS-SUM
           OPEN INPUT GL-FILE
           MOVE ZERO TO GL-CODE
           START GL-FILE KEY >= GL-CODE
               INVALID KEY MOVE 'Y' TO WS-EOF
               NOT INVALID KEY MOVE 'N' TO WS-EOF
           END-START
           PERFORM UNTIL WS-EOF = 'Y'
               READ GL-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-COUNT
                       ADD GL-BALANCE TO WS-SUM
               END-READ
           END-PERFORM
           CLOSE GL-FILE.

       8100-BALANCE.
           OPEN INPUT GL-FILE
           READ GL-FILE KEY IS GL-CODE
               INVALID KEY MOVE ZERO TO GL-BALANCE
           END-READ
           MOVE GL-BALANCE TO UT-ACTUAL-NUM
           CLOSE GL-FILE.

           COPY UTPROC.
