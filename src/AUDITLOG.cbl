       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUDITLOG.
      *================================================================*
      * PROGRAM  : AUDITLOG                                            *
      * PURPOSE  : APPENDS ONE LINE TO THE AUDIT TRAIL. THE CALLER     *
      *            SAYS WHAT HAPPENED; WHO AND WHEN COME FROM THE      *
      *            SESSION AND THE SYSTEM CLOCK SO THEY CANNOT BE      *
      *            SUPPLIED BY THE CALLER.                             *
      *            A FAILURE TO WRITE IS REPORTED BUT NEVER STOPS THE  *
      *            CALLING TRANSACTION.                                *
      * CALL     : CALL 'AUDITLOG' USING AUD-PARMS  (COPY AUDPARM)     *
      * FILES    : AUDITLOG (LINE SEQUENTIAL, EXTEND)                  *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AUD-FILE ASSIGN TO 'data/AUDITLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-AUD-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  AUD-FILE.
           COPY AUDREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY SESSION.
       01  WS-AUD-FS                   PIC X(02).

       LINKAGE SECTION.
           COPY AUDPARM.

       PROCEDURE DIVISION USING AUD-PARMS.
       0000-MAIN.
           OPEN EXTEND AUD-FILE
           IF WS-AUD-FS = '35'
               OPEN OUTPUT AUD-FILE
           END-IF
           IF WS-AUD-FS NOT = '00'
               DISPLAY 'AUDITLOG: WARNING - AUDIT FILE UNAVAILABLE FS='
                       WS-AUD-FS
               GOBACK
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           MOVE WS-CURR-DATE TO AUD-DATE
           MOVE WS-CURR-TIME TO AUD-TIME
           IF SES-USER-ID = SPACES OR LOW-VALUES
               MOVE '*NOUSER*' TO AUD-USER-ID
               MOVE SPACE      TO AUD-ROLE
           ELSE
               MOVE SES-USER-ID TO AUD-USER-ID
               MOVE SES-ROLE    TO AUD-ROLE
           END-IF
           MOVE AU-PROGRAM TO AUD-PROGRAM
           MOVE AU-ACTION  TO AUD-ACTION
           MOVE AU-KEY     TO AUD-KEY
           MOVE AU-DETAIL  TO AUD-DETAIL
           WRITE AUD-RECORD
           CLOSE AUD-FILE
           GOBACK.
