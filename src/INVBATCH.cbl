       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVBATCH.
      *================================================================*
      * PROGRAM  : INVBATCH                                            *
      * PURPOSE  : END-OF-DAY STOCK CHECK FOR EVERY BRANCH AND ITEM.   *
      *            AN ITEM AT OR BELOW ITS REORDER LEVEL WITH NOTHING  *
      *            ON ORDER GETS AN AUTOMATIC ORDER FOR ITS REORDER    *
      *            QUANTITY; OUT-OF-STOCK ITEMS ARE LISTED SEPARATELY. *
      * INPUT    : RUN DATE YYYYMMDD (BLANK = TODAY)                   *
      * OUTPUT   : data/REORDER_<DATE>.TXT                             *
      * FILES    : INVSTOCK (I-O), INVLOG (EXTEND)                     *
      * RC       : 0 ALL ABOVE REORDER LEVEL, 4 ORDERS RAISED OR ITEMS *
      *            OUT OF STOCK, 12 FILE ERROR                         *
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
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS INV-KEY
               FILE STATUS IS WS-INV-FS.
           SELECT LOG-FILE ASSIGN TO 'data/INVLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LOG-FS.
           SELECT RPT-FILE ASSIGN TO WS-RPT-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  INV-FILE.
           COPY INVREC.
       FD  LOG-FILE.
           COPY INVLOG.
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(90).

       WORKING-STORAGE SECTION.
       01  WS-INV-FS                   PIC X(02).
       01  WS-LOG-FS                   PIC X(02).
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-NAME                 PIC X(40).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-TIME                     PIC 9(08).
       01  WS-EOF-SW                   PIC X(01).
       01  WS-ITEMS                    PIC 9(05) VALUE ZERO.
       01  WS-ORDERS                   PIC 9(05) VALUE ZERO.
       01  WS-PENDING                  PIC 9(05) VALUE ZERO.
       01  WS-OUT                      PIC 9(05) VALUE ZERO.
       01  WS-NOTE                     PIC X(24).
       01  WS-NOTE2                    PIC X(24).
       01  WS-DISP-QTY                 PIC Z,ZZZ,ZZ9.
       01  WS-DISP-QTY2                PIC Z,ZZZ,ZZ9.
       01  WS-DISP-QTY3                PIC Z,ZZZ,ZZ9.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'RUN DATE (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           IF WS-IN-DATE = SPACES OR WS-IN-DATE IS NOT NUMERIC
               ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           ELSE
               MOVE WS-IN-DATE TO WS-RUN-DATE
           END-IF
           ACCEPT WS-TIME FROM TIME
           OPEN I-O INV-FILE
           IF WS-INV-FS = '35'
               DISPLAY 'INVBATCH: NO STOCK UNDER CONTROL.'
               MOVE ZERO TO RETURN-CODE
               GOBACK
           END-IF
           IF WS-INV-FS NOT = '00'
               DISPLAY 'INVBATCH: STOCK FILE OPEN FAILED ' WS-INV-FS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           STRING 'data/REORDER_' WS-RUN-DATE '.TXT'
                  DELIMITED BY SIZE INTO WS-RPT-NAME
           OPEN OUTPUT RPT-FILE
           MOVE SPACES TO RPT-LINE
           STRING 'BRANCH STOCK REORDER REPORT ' WS-RUN-DATE
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           MOVE 'BRANCH ITEM    ON HAND  REORDER AT   ORDERED  ACTION'
             TO RPT-LINE
           WRITE RPT-LINE
           OPEN EXTEND LOG-FILE
           IF WS-LOG-FS = '35'
               OPEN OUTPUT LOG-FILE
           END-IF

           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ INV-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N'
                   ADD 1 TO WS-ITEMS
                   IF INV-ON-HAND <= INV-REORDER-LEVEL
                       PERFORM 1000-BELOW-LEVEL
                   END-IF
               END-IF
           END-PERFORM

           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE SPACES TO RPT-LINE
           STRING 'ITEMS ' WS-ITEMS '  ORDERS RAISED ' WS-ORDERS
                  '  ALREADY ON ORDER ' WS-PENDING
                  '  OUT OF STOCK ' WS-OUT
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           DISPLAY FUNCTION TRIM(RPT-LINE TRAILING)
           CLOSE INV-FILE LOG-FILE RPT-FILE
           IF WS-ORDERS + WS-PENDING + WS-OUT > ZERO
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE ZERO TO RETURN-CODE
           END-IF
           GOBACK.

       1000-BELOW-LEVEL.
           IF INV-ON-HAND = ZERO
               ADD 1 TO WS-OUT
           END-IF
           IF INV-ON-ORDER > ZERO
               ADD 1 TO WS-PENDING
               MOVE 'ON ORDER - CHASE' TO WS-NOTE
           ELSE
               ADD 1 TO WS-ORDERS
               MOVE INV-REORDER-QTY TO INV-ON-ORDER
               REWRITE INV-RECORD
               PERFORM 1100-LOG-ORDER
               MOVE 'ORDER RAISED' TO WS-NOTE
           END-IF
           IF INV-ON-HAND = ZERO
               MOVE WS-NOTE TO WS-NOTE2
               MOVE SPACES  TO WS-NOTE
               STRING FUNCTION TRIM(WS-NOTE2) ' (OUT)'
                      DELIMITED BY SIZE INTO WS-NOTE
           END-IF
           MOVE INV-ON-HAND       TO WS-DISP-QTY
           MOVE INV-REORDER-LEVEL TO WS-DISP-QTY2
           MOVE INV-ON-ORDER      TO WS-DISP-QTY3
           MOVE SPACES TO RPT-LINE
           STRING INV-BRANCH ' ' INV-ITEM ' ' WS-DISP-QTY '   '
                  WS-DISP-QTY2 ' ' WS-DISP-QTY3 '  ' WS-NOTE
                  DELIMITED BY SIZE INTO RPT-LINE
           WRITE RPT-LINE
           DISPLAY FUNCTION TRIM(RPT-LINE TRAILING).

       1100-LOG-ORDER.
           INITIALIZE IVL-RECORD
           MOVE WS-RUN-DATE     TO IVL-DATE
           MOVE WS-TIME(1:6)    TO IVL-TIME
           MOVE INV-BRANCH      TO IVL-BRANCH
           MOVE INV-ITEM        TO IVL-ITEM
           MOVE 'O'             TO IVL-TYPE
           MOVE INV-REORDER-QTY TO IVL-QTY
           MOVE INV-ON-HAND     TO IVL-BAL-AFTER
           MOVE 'AUTO REORDER'  TO IVL-REF
           MOVE 'BATCH'         TO IVL-USER
           WRITE IVL-RECORD.
