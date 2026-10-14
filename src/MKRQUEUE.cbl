       IDENTIFICATION DIVISION.
       PROGRAM-ID. MKRQUEUE.
      *================================================================*
      * PROGRAM  : MKRQUEUE                                            *
      * PURPOSE  : WRITES TO (OR COUNTS) THE MAKER-CHECKER QUEUE.      *
      *            THE MAKER IS THE SIGNED-ON USER FROM THE SESSION;   *
      *            IDS ARE ALLOCATED AS HIGHEST EXISTING + 1.          *
      * FILES    : PENDTXN (I-O)                                       *
      * CALLS    : AUDITLOG                                            *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PEND-FILE ASSIGN TO 'data/PENDTXN.DAT'
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS PND-ID FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  PEND-FILE.
           COPY PENDREC.

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-NEXT-ID                  PIC 9(08).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-NOW.
           05  WS-NOW-DATE             PIC 9(08).
           05  WS-NOW-TIME             PIC 9(06).
           05  FILLER                  PIC X(07).
       01  ED-AMT                      PIC ZZ,ZZZ,ZZ9.99.
           COPY SESSION.
           COPY AUDPARM.

       LINKAGE SECTION.
           COPY MKRPARM.

       PROCEDURE DIVISION USING MKR-PARMS.
       0000-MAIN.
           MOVE ZERO TO MKR-RC
           OPEN I-O PEND-FILE
           IF WS-FS = '35'
               OPEN OUTPUT PEND-FILE
               CLOSE PEND-FILE
               OPEN I-O PEND-FILE
           END-IF
           IF WS-FS NOT = '00'
               SET MKR-FILE-ERROR TO TRUE
               GOBACK
           END-IF
           EVALUATE TRUE
               WHEN MKR-ADD   PERFORM 1000-ADD
               WHEN MKR-COUNT PERFORM 2000-COUNT
               WHEN MKR-EXPIRE PERFORM 3000-EXPIRE
           END-EVALUATE
           CLOSE PEND-FILE
           GOBACK.

       1000-ADD.
           MOVE ZERO TO WS-NEXT-ID
           MOVE 99999999 TO PND-ID
           START PEND-FILE KEY IS LESS THAN OR EQUAL PND-ID
               INVALID KEY CONTINUE
               NOT INVALID KEY
                   READ PEND-FILE PREVIOUS RECORD
                       NOT AT END MOVE PND-ID TO WS-NEXT-ID
                   END-READ
           END-START
           ADD 1 TO WS-NEXT-ID
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           INITIALIZE PEND-RECORD
           MOVE WS-NEXT-ID     TO PND-ID
           MOVE MKR-TYPE       TO PND-TYPE
           MOVE MKR-FROM-ACCT  TO PND-FROM-ACCT
           MOVE MKR-TO-ACCT    TO PND-TO-ACCT
           MOVE MKR-AMOUNT     TO PND-AMOUNT
           MOVE MKR-NARRATION  TO PND-NARRATION
           SET PND-PENDING     TO TRUE
           MOVE SES-USER-ID    TO PND-MAKER
           MOVE SES-BRANCH     TO PND-MAKER-BRANCH
           MOVE WS-NOW-DATE    TO PND-MAKE-DATE
           MOVE WS-NOW-TIME    TO PND-MAKE-TIME
           WRITE PEND-RECORD
               INVALID KEY
                   SET MKR-FILE-ERROR TO TRUE
                   EXIT PARAGRAPH
           END-WRITE
           MOVE WS-NEXT-ID TO MKR-ID
           MOVE 'MKRQUEUE'   TO AU-PROGRAM
           MOVE 'QUEUED'     TO AU-ACTION
           MOVE PND-ID       TO AU-KEY
           MOVE MKR-AMOUNT   TO ED-AMT
           MOVE SPACES       TO AU-DETAIL
           STRING MKR-TYPE ' ' MKR-FROM-ACCT ' ' ED-AMT
               DELIMITED BY SIZE INTO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS.

       2000-COUNT.
           MOVE ZERO TO MKR-PENDING
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ PEND-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF PND-PENDING
                           ADD 1 TO MKR-PENDING
                       END-IF
               END-READ
           END-PERFORM.

       3000-EXPIRE.
           MOVE ZERO TO MKR-PENDING
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ PEND-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF PND-PENDING AND PND-MAKE-DATE < MKR-RUN-DATE
                           SET PND-EXPIRED TO TRUE
                           MOVE 'EXPIRED AT EOD' TO PND-REMARK
                           MOVE 'EOD'          TO PND-CHECKER
                           MOVE MKR-RUN-DATE   TO PND-CHECK-DATE
                           REWRITE PEND-RECORD
                           ADD 1 TO MKR-PENDING
                       END-IF
               END-READ
           END-PERFORM.
