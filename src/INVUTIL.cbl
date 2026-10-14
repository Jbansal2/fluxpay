       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVUTIL.
      *================================================================*
      * PROGRAM  : INVUTIL                                             *
      * PURPOSE  : ISSUES STOCK AT A BRANCH (CHEQUE BOOK, CARD BLANK)  *
      *            AND LOGS THE MOVEMENT. CALLED BY CHQMNT AND CARDMNT *
      *            AT THE MOMENT THE ITEM IS HANDED OVER. AN ITEM THE  *
      *            BRANCH DOES NOT STOCK-CONTROL RETURNS RC 10 SO      *
      *            BRANCHES THAT HAVE NOT LOADED STOCK KEEP WORKING.   *
      * CALL     : CALL 'INVUTIL' USING INV-PARMS  (COPY INVPARM)      *
      * FILES    : INVSTOCK (INDEXED, I-O), INVLOG (EXTEND)            *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INV-FILE ASSIGN TO 'data/INVSTOCK.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS INV-KEY
               FILE STATUS IS WS-INV-FS.
           SELECT LOG-FILE ASSIGN TO 'data/INVLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LOG-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  INV-FILE.
           COPY INVREC.
       FD  LOG-FILE.
           COPY INVLOG.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY SESSION.
       01  WS-INV-FS                   PIC X(02).
       01  WS-LOG-FS                   PIC X(02).

       LINKAGE SECTION.
           COPY INVPARM.

       PROCEDURE DIVISION USING INV-PARMS.
       0000-MAIN.
           SET IV-OK TO TRUE
           MOVE 'N' TO IV-REORDER-HIT
           MOVE ZERO TO IV-ON-HAND
           IF IV-QTY NOT NUMERIC OR IV-QTY = ZERO
               MOVE 1 TO IV-QTY
           END-IF
           OPEN I-O INV-FILE
           IF WS-INV-FS = '35'
               SET IV-NOT-TRACKED TO TRUE
               GOBACK
           END-IF
           IF WS-INV-FS NOT = '00'
               SET IV-FILE-ERROR TO TRUE
               GOBACK
           END-IF
           MOVE IV-BRANCH TO INV-BRANCH
           MOVE IV-ITEM   TO INV-ITEM
           READ INV-FILE
               INVALID KEY
                   SET IV-NOT-TRACKED TO TRUE
           END-READ
           IF IV-OK
               IF INV-ON-HAND < IV-QTY
                   SET IV-OUT-OF-STOCK TO TRUE
                   MOVE INV-ON-HAND TO IV-ON-HAND
               ELSE
                   PERFORM 1000-ISSUE
               END-IF
           END-IF
           CLOSE INV-FILE
           GOBACK.

       1000-ISSUE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           SUBTRACT IV-QTY FROM INV-ON-HAND
           ADD IV-QTY TO INV-ISSUED-TOTAL
           MOVE WS-CURR-DATE TO INV-LAST-ISSUE
           REWRITE INV-RECORD
               INVALID KEY
                   SET IV-FILE-ERROR TO TRUE
                   EXIT PARAGRAPH
           END-REWRITE
           MOVE INV-ON-HAND TO IV-ON-HAND
           IF INV-ON-HAND <= INV-REORDER-LEVEL
               MOVE 'Y' TO IV-REORDER-HIT
           END-IF
           OPEN EXTEND LOG-FILE
           IF WS-LOG-FS = '35'
               OPEN OUTPUT LOG-FILE
           END-IF
           INITIALIZE IVL-RECORD
           MOVE WS-CURR-DATE TO IVL-DATE
           MOVE WS-CURR-TIME TO IVL-TIME
           MOVE INV-BRANCH   TO IVL-BRANCH
           MOVE INV-ITEM     TO IVL-ITEM
           MOVE 'I'          TO IVL-TYPE
           MOVE IV-QTY       TO IVL-QTY
           MOVE INV-ON-HAND  TO IVL-BAL-AFTER
           MOVE IV-REF       TO IVL-REF
           MOVE SES-USER-ID  TO IVL-USER
           WRITE IVL-RECORD
           CLOSE LOG-FILE.
