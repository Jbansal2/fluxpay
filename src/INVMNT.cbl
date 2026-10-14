       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVMNT.
      *================================================================*
      * PROGRAM  : INVMNT                                              *
      * PURPOSE  : BRANCH STOCK REGISTER FOR CHEQUE BOOKS AND CARD     *
      *            BLANKS (SECURITY STATIONERY).                       *
      *            1 STOCK POSITION OF A BRANCH                        *
      *            2 START STOCK CONTROL FOR AN ITEM (MANAGER)         *
      *            3 RECEIVE STOCK (CLEARS WHAT WAS ON ORDER)          *
      *            4 TRANSFER TO ANOTHER BRANCH                        *
      *            5 WRITE OFF DAMAGED / LOST (MANAGER)                *
      *            6 PLACE ORDER                                       *
      *            7 MOVEMENT HISTORY OF AN ITEM                       *
      *            ISSUES TO CUSTOMERS HAPPEN IN CHQMNT / CARDMNT      *
      *            THROUGH INVUTIL; THE EOD BATCH INVBATCH RAISES      *
      *            REORDERS.                                           *
      * FILES    : INVSTOCK (I-O), INVLOG (EXTEND / INPUT)             *
      * CALLS    : AUDITLOG                                            *
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
               ACCESS MODE IS DYNAMIC
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
           COPY AUDPARM.
       01  WS-INV-FS                   PIC X(02).
       01  WS-LOG-FS                   PIC X(02).
       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.
       01  WS-EOF-SW                   PIC X(01).
       01  WS-CONFIRM                  PIC X(01).
       01  WS-IN-BRANCH                PIC X(06).
       01  WS-IN-TO-BRANCH             PIC X(06).
       01  WS-IN-ITEM                  PIC X(04).
       01  WS-IN-QTY                   PIC X(07).
       01  WS-IN-REF                   PIC X(16).
       01  WS-QTY                      PIC 9(07).
       01  WS-LEVEL                    PIC 9(07).
       01  WS-REORDER                  PIC 9(07).
       01  WS-CNT                      PIC 9(05).
       01  WS-FLAG                     PIC X(10).
       01  WS-DISP-QTY                 PIC Z,ZZZ,ZZ9.
       01  WS-DISP-QTY2                PIC Z,ZZZ,ZZ9.
       01  WS-DISP-QTY3                PIC Z,ZZZ,ZZ9.
       01  WS-SAVE-RECORD              PIC X(100).
       01  WS-LOG-TYPE                 PIC X(01).
       01  WS-LOG-BAL                  PIC 9(07).
       01  WS-LOG-BRANCH               PIC X(06).

      * ITEM CATALOGUE: CODE + DESCRIPTION
       01  WS-CATALOGUE.
           05  FILLER PIC X(34) VALUE
               'CB10CHEQUE BOOK - 10 LEAVES       '.
           05  FILLER PIC X(34) VALUE
               'CB25CHEQUE BOOK - 25 LEAVES       '.
           05  FILLER PIC X(34) VALUE
               'DCRDDEBIT CARD BLANK              '.
           05  FILLER PIC X(34) VALUE
               'PINMPIN MAILER                    '.
           05  FILLER PIC X(34) VALUE
               'CCRDCREDIT CARD BLANK             '.
       01  WS-CATALOGUE-R REDEFINES WS-CATALOGUE.
           05  WS-CAT OCCURS 5 TIMES INDEXED BY CAT-IDX.
               10  CAT-CODE            PIC X(04).
               10  CAT-DESC            PIC X(30).

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN I-O INV-FILE
           IF WS-INV-FS = '35'
               OPEN OUTPUT INV-FILE
               CLOSE INV-FILE
               OPEN I-O INV-FILE
           END-IF
           IF WS-INV-FS NOT = '00'
               DISPLAY 'STOCK FILE OPEN FAILED, FS=' WS-INV-FS
               GOBACK
           END-IF
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE = '9'
               DISPLAY ' '
               DISPLAY '---------- BRANCH STOCK REGISTER ----------'
               DISPLAY ' 1. STOCK POSITION'
               DISPLAY ' 2. START STOCK CONTROL FOR AN ITEM (MANAGER)'
               DISPLAY ' 3. RECEIVE STOCK'
               DISPLAY ' 4. TRANSFER TO ANOTHER BRANCH'
               DISPLAY ' 5. WRITE OFF DAMAGED / LOST (MANAGER)'
               DISPLAY ' 6. PLACE ORDER'
               DISPLAY ' 7. MOVEMENT HISTORY'
               DISPLAY ' 9. BACK'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 1000-POSITION
                   WHEN '2' PERFORM 2000-START-ITEM
                   WHEN '3' PERFORM 3000-RECEIVE
                   WHEN '4' PERFORM 4000-TRANSFER
                   WHEN '5' PERFORM 5000-WRITE-OFF
                   WHEN '6' PERFORM 6000-ORDER
                   WHEN '7' PERFORM 7000-HISTORY
                   WHEN '9' CONTINUE
                   WHEN SPACE
                       ADD 1 TO WS-BLANK-INPUTS
                       IF WS-BLANK-INPUTS > 20
                           MOVE '9' TO WS-CHOICE
                       END-IF
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
               IF WS-CHOICE NOT = SPACE
                   MOVE ZERO TO WS-BLANK-INPUTS
               END-IF
           END-PERFORM
           CLOSE INV-FILE
           GOBACK.

      *----------------------------------------------------------------*
      * 1. POSITION                                                    *
      *----------------------------------------------------------------*
       1000-POSITION.
           PERFORM 8000-ASK-BRANCH
           MOVE WS-IN-BRANCH TO INV-BRANCH
           MOVE LOW-VALUES   TO INV-ITEM
           START INV-FILE KEY >= INV-KEY
               INVALID KEY MOVE 'Y' TO WS-EOF-SW
               NOT INVALID KEY MOVE 'N' TO WS-EOF-SW
           END-START
           MOVE ZERO TO WS-CNT
           DISPLAY 'STOCK AT ' WS-IN-BRANCH
           DISPLAY 'ITEM DESCRIPTION                    ON HAND  '
                   'REORDER AT  ON ORDER'
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ INV-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N'
                   IF INV-BRANCH NOT = WS-IN-BRANCH
                       MOVE 'Y' TO WS-EOF-SW
                   ELSE
                       ADD 1 TO WS-CNT
                       PERFORM 1100-POSITION-LINE
                   END-IF
               END-IF
           END-PERFORM
           IF WS-CNT = ZERO
               DISPLAY 'NO ITEMS UNDER STOCK CONTROL AT '
                       WS-IN-BRANCH '.'
           END-IF.

       1100-POSITION-LINE.
           EVALUATE TRUE
               WHEN INV-ON-HAND = ZERO
                   MOVE 'OUT' TO WS-FLAG
               WHEN INV-ON-HAND <= INV-REORDER-LEVEL
                   MOVE 'REORDER' TO WS-FLAG
               WHEN OTHER
                   MOVE SPACES TO WS-FLAG
           END-EVALUATE
           MOVE INV-ON-HAND       TO WS-DISP-QTY
           MOVE INV-REORDER-LEVEL TO WS-DISP-QTY2
           MOVE INV-ON-ORDER      TO WS-DISP-QTY3
           DISPLAY INV-ITEM ' ' INV-DESC WS-DISP-QTY '   '
                   WS-DISP-QTY2 ' ' WS-DISP-QTY3 ' ' WS-FLAG.

      *----------------------------------------------------------------*
      * 2. START STOCK CONTROL                                         *
      *----------------------------------------------------------------*
       2000-START-ITEM.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'ONLY A MANAGER CAN START STOCK CONTROL.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8000-ASK-BRANCH
           PERFORM 8100-ASK-ITEM
           IF WS-IN-ITEM = SPACES
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'OPENING QUANTITY     : ' WITH NO ADVANCING
           PERFORM 8200-ASK-QTY
           MOVE WS-QTY TO WS-LOG-BAL
           DISPLAY 'REORDER LEVEL        : ' WITH NO ADVANCING
           PERFORM 8200-ASK-QTY
           MOVE WS-QTY TO WS-LEVEL
           DISPLAY 'REORDER QUANTITY     : ' WITH NO ADVANCING
           PERFORM 8200-ASK-QTY
           MOVE WS-QTY TO WS-REORDER
           IF WS-REORDER = ZERO
               DISPLAY 'REORDER QUANTITY MUST BE ABOVE ZERO.'
               EXIT PARAGRAPH
           END-IF
           INITIALIZE INV-RECORD
           MOVE WS-IN-BRANCH TO INV-BRANCH
           MOVE WS-IN-ITEM   TO INV-ITEM
           MOVE CAT-DESC(CAT-IDX) TO INV-DESC
           MOVE WS-LOG-BAL   TO INV-ON-HAND
           MOVE WS-LEVEL     TO INV-REORDER-LEVEL
           MOVE WS-REORDER   TO INV-REORDER-QTY
           MOVE WS-CURR-DATE TO INV-LAST-RECEIPT
           WRITE INV-RECORD
               INVALID KEY
                   DISPLAY WS-IN-ITEM ' IS ALREADY STOCK-CONTROLLED AT '
                           WS-IN-BRANCH '.'
                   EXIT PARAGRAPH
           END-WRITE
           MOVE 'R' TO WS-LOG-TYPE
           MOVE 'OPENING STOCK' TO WS-IN-REF
           MOVE INV-ON-HAND TO WS-QTY
           PERFORM 8500-LOG
           MOVE 'STOCK START' TO AU-ACTION
           PERFORM 8900-AUDIT
           DISPLAY WS-IN-ITEM ' NOW STOCK-CONTROLLED AT ' WS-IN-BRANCH
                   '.'.

      *----------------------------------------------------------------*
      * 3. RECEIVE                                                     *
      *----------------------------------------------------------------*
       3000-RECEIVE.
           PERFORM 8000-ASK-BRANCH
           PERFORM 8100-ASK-ITEM
           IF WS-IN-ITEM = SPACES
               EXIT PARAGRAPH
           END-IF
           PERFORM 8300-READ-ITEM
           IF WS-INV-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'QUANTITY RECEIVED    : ' WITH NO ADVANCING
           PERFORM 8200-ASK-QTY
           IF WS-QTY = ZERO
               DISPLAY 'QUANTITY MUST BE ABOVE ZERO.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'CHALLAN / DELIVERY REF: ' WITH NO ADVANCING
           ACCEPT WS-IN-REF
           ADD WS-QTY TO INV-ON-HAND
           IF WS-QTY >= INV-ON-ORDER
               MOVE ZERO TO INV-ON-ORDER
           ELSE
               SUBTRACT WS-QTY FROM INV-ON-ORDER
           END-IF
           MOVE WS-CURR-DATE TO INV-LAST-RECEIPT
           REWRITE INV-RECORD
           MOVE 'R' TO WS-LOG-TYPE
           PERFORM 8500-LOG
           MOVE INV-ON-HAND TO WS-DISP-QTY
           DISPLAY 'RECEIVED. ' WS-IN-ITEM ' ON HAND AT ' WS-IN-BRANCH
                   ': ' FUNCTION TRIM(WS-DISP-QTY).

      *----------------------------------------------------------------*
      * 4. TRANSFER - BOTH BRANCHES MUST STOCK-CONTROL THE ITEM        *
      *----------------------------------------------------------------*
       4000-TRANSFER.
           PERFORM 8000-ASK-BRANCH
           PERFORM 8100-ASK-ITEM
           IF WS-IN-ITEM = SPACES
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'TO BRANCH            : ' WITH NO ADVANCING
           ACCEPT WS-IN-TO-BRANCH
           MOVE FUNCTION UPPER-CASE(WS-IN-TO-BRANCH) TO WS-IN-TO-BRANCH
           DISPLAY 'QUANTITY             : ' WITH NO ADVANCING
           PERFORM 8200-ASK-QTY
           IF WS-IN-TO-BRANCH = WS-IN-BRANCH OR WS-QTY = ZERO
               DISPLAY 'NEED ANOTHER BRANCH AND A QUANTITY ABOVE ZERO.'
               EXIT PARAGRAPH
           END-IF
      *    RECEIVING SIDE FIRST, SO A MISSING RECORD CHANGES NOTHING
           MOVE WS-IN-TO-BRANCH TO INV-BRANCH
           MOVE WS-IN-ITEM      TO INV-ITEM
           READ INV-FILE
               INVALID KEY
                   DISPLAY WS-IN-ITEM ' IS NOT STOCK-CONTROLLED AT '
                           WS-IN-TO-BRANCH '.'
                   EXIT PARAGRAPH
           END-READ
           PERFORM 8300-READ-ITEM
           IF WS-INV-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF INV-ON-HAND < WS-QTY
               MOVE INV-ON-HAND TO WS-DISP-QTY
               DISPLAY 'ONLY ' FUNCTION TRIM(WS-DISP-QTY)
                       ' ON HAND AT ' WS-IN-BRANCH '.'
               EXIT PARAGRAPH
           END-IF
           SUBTRACT WS-QTY FROM INV-ON-HAND
           REWRITE INV-RECORD
           MOVE 'T' TO WS-LOG-TYPE
           MOVE SPACES TO WS-IN-REF
           STRING 'TO ' WS-IN-TO-BRANCH DELIMITED BY SIZE
                  INTO WS-IN-REF
           PERFORM 8500-LOG

           MOVE WS-IN-TO-BRANCH TO INV-BRANCH
           MOVE WS-IN-ITEM      TO INV-ITEM
           READ INV-FILE
           ADD WS-QTY TO INV-ON-HAND
           MOVE WS-CURR-DATE TO INV-LAST-RECEIPT
           REWRITE INV-RECORD
           MOVE 'U' TO WS-LOG-TYPE
           MOVE SPACES TO WS-IN-REF
           STRING 'FROM ' WS-IN-BRANCH DELIMITED BY SIZE
                  INTO WS-IN-REF
           PERFORM 8500-LOG
           MOVE WS-QTY TO WS-DISP-QTY
           DISPLAY FUNCTION TRIM(WS-DISP-QTY) ' ' WS-IN-ITEM
                   ' TRANSFERRED ' WS-IN-BRANCH ' -> '
                   WS-IN-TO-BRANCH '.'.

      *----------------------------------------------------------------*
      * 5. WRITE OFF                                                   *
      *----------------------------------------------------------------*
       5000-WRITE-OFF.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'ONLY A MANAGER CAN WRITE OFF STOCK.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 8000-ASK-BRANCH
           PERFORM 8100-ASK-ITEM
           IF WS-IN-ITEM = SPACES
               EXIT PARAGRAPH
           END-IF
           PERFORM 8300-READ-ITEM
           IF WS-INV-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'QUANTITY             : ' WITH NO ADVANCING
           PERFORM 8200-ASK-QTY
           DISPLAY 'REASON (DAMAGED/LOST): ' WITH NO ADVANCING
           ACCEPT WS-IN-REF
           IF WS-QTY = ZERO OR WS-QTY > INV-ON-HAND
              OR WS-IN-REF = SPACES
               DISPLAY 'QUANTITY MUST BE 1 - ON HAND, WITH A REASON.'
               EXIT PARAGRAPH
           END-IF
           SUBTRACT WS-QTY FROM INV-ON-HAND
           REWRITE INV-RECORD
           MOVE 'A' TO WS-LOG-TYPE
           PERFORM 8500-LOG
           MOVE 'STOCK W/OFF' TO AU-ACTION
           PERFORM 8900-AUDIT
           MOVE WS-QTY TO WS-DISP-QTY
           DISPLAY FUNCTION TRIM(WS-DISP-QTY) ' ' WS-IN-ITEM
                   ' WRITTEN OFF - ' FUNCTION TRIM(WS-IN-REF) '.'.

      *----------------------------------------------------------------*
      * 6. ORDER - ONE OPEN ORDER AT A TIME PER ITEM                   *
      *----------------------------------------------------------------*
       6000-ORDER.
           PERFORM 8000-ASK-BRANCH
           PERFORM 8100-ASK-ITEM
           IF WS-IN-ITEM = SPACES
               EXIT PARAGRAPH
           END-IF
           PERFORM 8300-READ-ITEM
           IF WS-INV-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF INV-ON-ORDER > ZERO
               MOVE INV-ON-ORDER TO WS-DISP-QTY
               DISPLAY 'AN ORDER FOR ' FUNCTION TRIM(WS-DISP-QTY)
                       ' IS ALREADY OPEN.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'QUANTITY (BLANK = REORDER QTY): ' WITH NO ADVANCING
           PERFORM 8200-ASK-QTY
           IF WS-QTY = ZERO
               MOVE INV-REORDER-QTY TO WS-QTY
           END-IF
           MOVE WS-QTY TO INV-ON-ORDER
           REWRITE INV-RECORD
           MOVE 'O' TO WS-LOG-TYPE
           MOVE 'MANUAL ORDER' TO WS-IN-REF
           PERFORM 8500-LOG
           MOVE WS-QTY TO WS-DISP-QTY
           DISPLAY 'ORDER PLACED FOR ' FUNCTION TRIM(WS-DISP-QTY) ' '
                   WS-IN-ITEM '.'.

      *----------------------------------------------------------------*
      * 7. HISTORY - FROM THE MOVEMENT LOG                             *
      *----------------------------------------------------------------*
       7000-HISTORY.
           PERFORM 8000-ASK-BRANCH
           PERFORM 8100-ASK-ITEM
           IF WS-IN-ITEM = SPACES
               EXIT PARAGRAPH
           END-IF
           OPEN INPUT LOG-FILE
           IF WS-LOG-FS NOT = '00'
               DISPLAY 'NO MOVEMENTS RECORDED.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'DATE     T     QTY  BALANCE  REFERENCE        USER'
           MOVE ZERO TO WS-CNT
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL WS-EOF-SW = 'Y'
               READ LOG-FILE
                   AT END MOVE 'Y' TO WS-EOF-SW
               END-READ
               IF WS-EOF-SW = 'N' AND IVL-BRANCH = WS-IN-BRANCH
                  AND IVL-ITEM = WS-IN-ITEM
                   ADD 1 TO WS-CNT
                   MOVE IVL-QTY       TO WS-DISP-QTY
                   MOVE IVL-BAL-AFTER TO WS-DISP-QTY2
                   DISPLAY IVL-DATE ' ' IVL-TYPE WS-DISP-QTY
                           WS-DISP-QTY2 '  ' IVL-REF ' ' IVL-USER
               END-IF
           END-PERFORM
           CLOSE LOG-FILE
           DISPLAY WS-CNT ' MOVEMENT(S).'.

      *----------------------------------------------------------------*
      * COMMON                                                         *
      *----------------------------------------------------------------*
       8000-ASK-BRANCH.
           DISPLAY 'BRANCH (BLANK = ' SES-BRANCH ')  : '
                   WITH NO ADVANCING
           ACCEPT WS-IN-BRANCH
           MOVE FUNCTION UPPER-CASE(WS-IN-BRANCH) TO WS-IN-BRANCH
           IF WS-IN-BRANCH = SPACES
               MOVE SES-BRANCH TO WS-IN-BRANCH
           END-IF.

       8100-ASK-ITEM.
           DISPLAY 'ITEM (CB10 CB25 DCRD PINM CCRD): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-ITEM
           MOVE FUNCTION UPPER-CASE(WS-IN-ITEM) TO WS-IN-ITEM
           SET CAT-IDX TO 1
           SEARCH WS-CAT
               AT END
                   DISPLAY 'UNKNOWN ITEM ' WS-IN-ITEM '.'
                   MOVE SPACES TO WS-IN-ITEM
               WHEN CAT-CODE(CAT-IDX) = WS-IN-ITEM
                   CONTINUE
           END-SEARCH.

       8200-ASK-QTY.
           ACCEPT WS-IN-QTY
           IF WS-IN-QTY NOT = SPACES
              AND FUNCTION TEST-NUMVAL(WS-IN-QTY) = ZERO
               COMPUTE WS-QTY = FUNCTION NUMVAL(WS-IN-QTY)
           ELSE
               MOVE ZERO TO WS-QTY
           END-IF.

       8300-READ-ITEM.
           MOVE WS-IN-BRANCH TO INV-BRANCH
           MOVE WS-IN-ITEM   TO INV-ITEM
           READ INV-FILE
               INVALID KEY
                   DISPLAY WS-IN-ITEM ' IS NOT STOCK-CONTROLLED AT '
                           WS-IN-BRANCH '.'
           END-READ.

      *    LOGS WS-LOG-TYPE / WS-QTY / WS-IN-REF FOR THE RECORD IN HAND
       8500-LOG.
           OPEN EXTEND LOG-FILE
           IF WS-LOG-FS = '35'
               OPEN OUTPUT LOG-FILE
           END-IF
           INITIALIZE IVL-RECORD
           MOVE WS-CURR-DATE TO IVL-DATE
           MOVE WS-CURR-TIME TO IVL-TIME
           MOVE INV-BRANCH   TO IVL-BRANCH
           MOVE INV-ITEM     TO IVL-ITEM
           MOVE WS-LOG-TYPE  TO IVL-TYPE
           MOVE WS-QTY       TO IVL-QTY
           MOVE INV-ON-HAND  TO IVL-BAL-AFTER
           MOVE WS-IN-REF    TO IVL-REF
           MOVE SES-USER-ID  TO IVL-USER
           WRITE IVL-RECORD
           CLOSE LOG-FILE.

       8900-AUDIT.
           MOVE 'INVMNT' TO AU-PROGRAM
           MOVE INV-KEY  TO AU-KEY
           MOVE SPACES   TO AU-DETAIL
           MOVE WS-QTY   TO WS-DISP-QTY
           STRING 'QTY ' FUNCTION TRIM(WS-DISP-QTY) ' '
                  WS-IN-REF DELIMITED BY SIZE INTO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS.
