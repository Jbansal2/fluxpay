       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHKPOINT.
      *================================================================*
      * PROGRAM  : CHKPOINT                                            *
      * PURPOSE  : CHECKPOINT / RESTART SERVICE FOR LONG BATCH RUNS.   *
      *            A PROGRAM CALLS BEGIN AT START-UP, SAVE EVERY N     *
      *            RECORDS (AFTER ITS UPDATES ARE WRITTEN), END-STEP   *
      *            WHEN A STEP FINISHES AND COMPLETE AT THE END. IF    *
      *            THE JOB DIES, THE NEXT RUN FOR THE SAME RUN DATE    *
      *            GETS THE LAST COMMIT POINT BACK FROM BEGIN AND      *
      *            CARRIES ON FROM THERE INSTEAD OF STARTING OVER.     *
      *            A COMPLETED RUN IS NOT RESUMED - RUNNING AGAIN      *
      *            STARTS FRESH (EVERY STEP IS SAFE TO REPEAT).        *
      * TESTING  : ENVIRONMENT CB_ABEND_AT=<PROGRAM>:<N> MAKES THE N-TH*
      *            SAVE OF THAT PROGRAM END THE RUN WITH RC 16 RIGHT   *
      *            AFTER THE CHECKPOINT IS WRITTEN - A SIMULATED       *
      *            CRASH FOR RESTART TESTS.                            *
      * FILES    : CHKPOINT (I-O)                                      *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHK-FILE ASSIGN TO 'data/CHKPOINT.DAT'
               ORGANIZATION IS INDEXED ACCESS IS RANDOM
               RECORD KEY IS CK-KEY FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CHK-FILE.
           COPY CHKREC.

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-NOW                      PIC X(21).
       01  WS-ABEND-ENV                PIC X(30).
       01  WS-ABEND-PGM                PIC X(08).
       01  WS-ABEND-AT                 PIC X(09).
       01  WS-ABEND-N                  PIC 9(09) VALUE ZERO.
       01  WS-SAVES                    PIC 9(09) VALUE ZERO.
       01  WS-FIRST-SW                 PIC X(01) VALUE 'Y'.

       LINKAGE SECTION.
           COPY CKPPARM.

       PROCEDURE DIVISION USING CKP-PARMS.
       0000-MAIN.
           IF WS-FIRST-SW = 'Y'
               PERFORM 0100-READ-ABEND-SETTING
               MOVE 'N' TO WS-FIRST-SW
           END-IF
           MOVE ZERO TO CKP-RC
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           OPEN I-O CHK-FILE
           IF WS-FS = '35'
               OPEN OUTPUT CHK-FILE
               CLOSE CHK-FILE
               OPEN I-O CHK-FILE
           END-IF
           IF WS-FS NOT = '00'
               DISPLAY 'CHKPOINT: OPEN FAILED, FS=' WS-FS
               SET CKP-FILE-ERROR TO TRUE
               GOBACK
           END-IF
           MOVE CKP-PROGRAM  TO CK-PROGRAM
           MOVE CKP-RUN-DATE TO CK-RUN-DATE
           EVALUATE TRUE
               WHEN CKP-BEGIN    PERFORM 1000-BEGIN
               WHEN CKP-SAVE     PERFORM 2000-SAVE
               WHEN CKP-END-STEP PERFORM 3000-END-STEP
               WHEN CKP-COMPLETE PERFORM 4000-COMPLETE
           END-EVALUATE
           CLOSE CHK-FILE
           IF CKP-SAVE AND WS-ABEND-N > ZERO
              AND CKP-PROGRAM = WS-ABEND-PGM
               ADD 1 TO WS-SAVES
               IF WS-SAVES = WS-ABEND-N
                   DISPLAY 'CHKPOINT: SIMULATED ABEND AFTER COMMIT '
                           WS-SAVES ' (CB_ABEND_AT)'
                   MOVE 16 TO RETURN-CODE
                   STOP RUN
               END-IF
           END-IF
           GOBACK.

       0100-READ-ABEND-SETTING.
           MOVE SPACES TO WS-ABEND-ENV
           ACCEPT WS-ABEND-ENV FROM ENVIRONMENT 'CB_ABEND_AT'
           IF WS-ABEND-ENV NOT = SPACES
               UNSTRING WS-ABEND-ENV DELIMITED BY ':'
                   INTO WS-ABEND-PGM WS-ABEND-AT
               IF FUNCTION TRIM(WS-ABEND-AT) IS NUMERIC
                   MOVE FUNCTION NUMVAL(WS-ABEND-AT) TO WS-ABEND-N
               END-IF
           END-IF.

       1000-BEGIN.
           MOVE 'N' TO CKP-RESTARTED
           READ CHK-FILE
               INVALID KEY MOVE '23' TO WS-FS
           END-READ
           IF WS-FS = '00' AND CK-RUNNING
      *        UNFINISHED RUN - HAND BACK THE LAST COMMIT POINT
               ADD 1 TO CK-RESTARTS
               MOVE WS-NOW(1:14) TO CK-UPDATED
               REWRITE CHK-RECORD
               MOVE 'Y'         TO CKP-RESTARTED
               MOVE CK-STEP     TO CKP-STEP
               MOVE CK-LAST-KEY TO CKP-LAST-KEY
               MOVE CK-RECORDS  TO CKP-RECORDS
               MOVE CK-COUNTERS TO CKP-COUNTERS
               MOVE CK-RESTARTS TO CKP-RESTARTS
               EXIT PARAGRAPH
           END-IF
           INITIALIZE CHK-RECORD
           MOVE CKP-PROGRAM  TO CK-PROGRAM
           MOVE CKP-RUN-DATE TO CK-RUN-DATE
           SET CK-RUNNING    TO TRUE
           MOVE WS-NOW(1:14) TO CK-STARTED CK-UPDATED
           IF WS-FS = '00'
               REWRITE CHK-RECORD
           ELSE
               WRITE CHK-RECORD
           END-IF
           MOVE ZERO   TO CKP-STEP CKP-RECORDS CKP-RESTARTS
           MOVE SPACES TO CKP-LAST-KEY CKP-COUNTERS.

       2000-SAVE.
           PERFORM 8000-READ-FOR-UPDATE
           IF CKP-FILE-ERROR
               EXIT PARAGRAPH
           END-IF
           MOVE CKP-LAST-KEY TO CK-LAST-KEY
           MOVE CKP-RECORDS  TO CK-RECORDS
           MOVE CKP-COUNTERS TO CK-COUNTERS
           PERFORM 8100-REWRITE.

       3000-END-STEP.
           PERFORM 8000-READ-FOR-UPDATE
           IF CKP-FILE-ERROR
               EXIT PARAGRAPH
           END-IF
           MOVE CKP-STEP     TO CK-STEP
           MOVE SPACES       TO CK-LAST-KEY
           MOVE CKP-COUNTERS TO CK-COUNTERS
           PERFORM 8100-REWRITE.

       4000-COMPLETE.
           PERFORM 8000-READ-FOR-UPDATE
           IF CKP-FILE-ERROR
               EXIT PARAGRAPH
           END-IF
           SET CK-COMPLETE TO TRUE
           PERFORM 8100-REWRITE.

       8000-READ-FOR-UPDATE.
           READ CHK-FILE
               INVALID KEY
                   DISPLAY 'CHKPOINT: NO BEGIN FOR ' CKP-PROGRAM
                   SET CKP-FILE-ERROR TO TRUE
           END-READ.

       8100-REWRITE.
           MOVE WS-NOW(1:14) TO CK-UPDATED
           REWRITE CHK-RECORD
               INVALID KEY
                   DISPLAY 'CHKPOINT: REWRITE FAILED, FS=' WS-FS
                   SET CKP-FILE-ERROR TO TRUE
           END-REWRITE.
