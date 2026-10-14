       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNDISB.
      *================================================================*
      * PROGRAM  : LNDISB                                              *
      * PURPOSE  : BOOKS AND DISBURSES A TERM LOAN: ALLOCATES THE NEXT *
      *            LOAN NUMBER, CREDITS THE LINKED ACCOUNT (ACCTPOST,  *
      *            TYPE LD, WHICH ALSO POSTS DR 1201 LOANS) AND WRITES *
      *            THE LOAN MASTER WITH THE FIRST EMI DUE IN ONE       *
      *            MONTH. SHARED BY LOANMNT (DIRECT SANCTION) AND      *
      *            LOANAPP (AFTER CREDIT APPROVAL) SO BOTH BOOK LOANS  *
      *            THE SAME WAY.                                       *
      * FILES    : LOANMAST (I-O)                                      *
      * CALLS    : ACCTPOST, DATEUTIL                                  *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION (MOVED OUT OF LOANMNT)              *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS LOAN-NO FILE STATUS IS WS-LOAN-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
           COPY LOANREC.

       WORKING-STORAGE SECTION.
       01  WS-LOAN-FS                  PIC X(02).
       01  WS-TODAY                    PIC 9(08).
       01  WS-NEXT-LOAN-NO             PIC 9(10).
           COPY POSTPARM.
           COPY DATEPARM.

       LINKAGE SECTION.
           COPY LNDPARM.

       PROCEDURE DIVISION USING LND-PARMS.
       0000-MAIN.
           MOVE ZERO TO LND-RC LND-LOAN-NO LND-FIRST-DUE LND-POST-RC
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           OPEN I-O LOAN-FILE
           IF WS-LOAN-FS = '35'
               OPEN OUTPUT LOAN-FILE
               CLOSE LOAN-FILE
               OPEN I-O LOAN-FILE
           END-IF
           IF WS-LOAN-FS NOT = '00'
               SET LND-FILE-ERROR TO TRUE
               GOBACK
           END-IF
           PERFORM 1000-NEXT-LOAN-NO
           PERFORM 2000-CREDIT-ACCOUNT
           IF LND-OK
               PERFORM 3000-WRITE-LOAN
           END-IF
           CLOSE LOAN-FILE
           GOBACK.

       1000-NEXT-LOAN-NO.
           MOVE 5000000000 TO WS-NEXT-LOAN-NO
           MOVE 9999999999 TO LOAN-NO
           START LOAN-FILE KEY IS LESS THAN OR EQUAL LOAN-NO
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ LOAN-FILE PREVIOUS RECORD
                       AT END CONTINUE
                       NOT AT END MOVE LOAN-NO TO WS-NEXT-LOAN-NO
                   END-READ
           END-START
           ADD 1 TO WS-NEXT-LOAN-NO.

       2000-CREDIT-ACCOUNT.
           INITIALIZE POST-PARMS
           SET PP-CREDIT TO TRUE
           MOVE LND-ACCT-NO     TO PP-ACCT-NO
           MOVE LND-AMOUNT      TO PP-AMOUNT
           MOVE 'LD'            TO PP-TRAN-TYPE
           MOVE WS-NEXT-LOAN-NO TO PP-REF-ACCT
           MOVE LND-NARRATION   TO PP-NARRATION
           IF LND-NARRATION = SPACES
               MOVE 'LOAN DISBURSAL' TO PP-NARRATION
           END-IF
           CALL 'ACCTPOST' USING POST-PARMS
           MOVE PP-RETURN-CODE TO LND-POST-RC
           IF NOT PP-OK
               SET LND-CREDIT-FAILED TO TRUE
           END-IF.

       3000-WRITE-LOAN.
           INITIALIZE LOAN-RECORD
           MOVE WS-NEXT-LOAN-NO TO LOAN-NO
           MOVE LND-ACCT-NO     TO LOAN-ACCT-NO
           MOVE LND-TYPE        TO LOAN-TYPE
           SET LOAN-ACTIVE      TO TRUE
           MOVE LND-AMOUNT      TO LOAN-PRINCIPAL LOAN-OUTSTANDING
           MOVE LND-RATE        TO LOAN-RATE
           MOVE LND-TENURE      TO LOAN-TENURE
           MOVE LND-EMI         TO LOAN-EMI
           MOVE LND-RATE-TYPE   TO LOAN-RATE-TYPE
           MOVE LND-SPREAD      TO LOAN-SPREAD
           MOVE 'T'             TO LOAN-RESET-PREF
           MOVE 'S'             TO LOAN-ASSET-CLASS
           MOVE WS-TODAY        TO LOAN-DISB-DATE
           SET DU-ADD-MONTHS TO TRUE
           MOVE WS-TODAY TO DU-DATE-1
           MOVE 1        TO DU-NUMBER
           CALL 'DATEUTIL' USING DATE-PARMS
           MOVE DU-RESULT-DATE TO LOAN-NEXT-DUE-DATE LND-FIRST-DUE
           WRITE LOAN-RECORD
               INVALID KEY
                   SET LND-WRITE-FAILED TO TRUE
           END-WRITE
           MOVE WS-NEXT-LOAN-NO TO LND-LOAN-NO.
