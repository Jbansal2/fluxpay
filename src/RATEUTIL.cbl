       IDENTIFICATION DIVISION.
       PROGRAM-ID. RATEUTIL.
      *================================================================*
      * PROGRAM  : RATEUTIL                                            *
      * PURPOSE  : LOOK UP ONE RATE IN THE RATE MASTER. IF THE MASTER  *
      *            OR THE ROW IS MISSING THE BUILT-IN DEFAULT BELOW IS *
      *            RETURNED WITH RC 04, SO A NEW INSTALLATION WORKS    *
      *            BEFORE ANYONE HAS SET A RATE. RATEMNT USES THE SAME *
      *            TABLE TO SEED THE MASTER.                           *
      * FILES    : RATEMAST (INPUT)                                    *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RATE-FILE ASSIGN TO 'data/RATEMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS RANDOM
               RECORD KEY IS RATE-CODE FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  RATE-FILE.
           COPY RATEREC.

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
           COPY RATEDFLT.

       LINKAGE SECTION.
           COPY RATEPARM.

       PROCEDURE DIVISION USING RATE-PARMS.
       0000-MAIN.
           MOVE ZERO TO RTP-VALUE RTP-EFF-DATE
           OPEN INPUT RATE-FILE
           IF WS-FS = '00'
               MOVE RTP-CODE TO RATE-CODE
               READ RATE-FILE
                   INVALID KEY MOVE '23' TO WS-FS
               END-READ
               IF WS-FS = '00'
                   MOVE RATE-VALUE    TO RTP-VALUE
                   MOVE RATE-EFF-DATE TO RTP-EFF-DATE
                   SET RTP-OK TO TRUE
               END-IF
               CLOSE RATE-FILE
           END-IF
           IF WS-FS NOT = '00'
               SET RDF-IDX TO 1
               SEARCH RDF-ENTRY
                   AT END
                       SET RTP-UNKNOWN TO TRUE
                   WHEN RDF-CODE(RDF-IDX) = RTP-CODE
                       MOVE RDF-VALUE(RDF-IDX) TO RTP-VALUE
                       SET RTP-DEFAULTED TO TRUE
               END-SEARCH
           END-IF
           GOBACK.
