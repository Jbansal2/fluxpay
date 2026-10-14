       IDENTIFICATION DIVISION.
       PROGRAM-ID. CASHMNT.
      *================================================================*
      * PROGRAM  : CASHMNT                                             *
      * PURPOSE  : BRANCH CASH MANAGEMENT                              *
      *              1 - VAULT POSITION AND OPEN DRAWERS               *
      *              2 - RECEIVE CASH FROM CURRENCY CHEST  (MANAGER)   *
      *              3 - REMIT CASH TO CURRENCY CHEST      (MANAGER)   *
      *              4 - ISSUE CASH TO A TELLER            (MANAGER)   *
      *              5 - MY DRAWER                                     *
      *              6 - CLOSE MY DRAWER (CASH TALLY)                  *
      *            THE VAULT IS KEPT BY DENOMINATION. A TELLER'S       *
      *            EXPECTED CASH = ISSUED + RECEIPTS - PAYMENTS; AT    *
      *            CLOSING HE COUNTS HIS NOTES, THEY GO BACK INTO THE  *
      *            VAULT AND ANY DIFFERENCE IS BOOKED:                 *
      *              SHORTAGE  DR 1295 CASH SHORTAGE  CR 1001 CASH     *
      *              EXCESS    DR 1001 CASH  CR 2904 CASH EXCESS       *
      *            CHEST MOVEMENTS: DR/CR 1001 AGAINST 1303.           *
      * FILES    : CASHPOS (I-O)                                       *
      * CALLS    : GLPOST, AUDITLOG                                    *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CASH-FILE ASSIGN TO 'data/CASHPOS.DAT'
               ORGANIZATION IS INDEXED ACCESS IS DYNAMIC
               RECORD KEY IS CP-KEY FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CASH-FILE.
           COPY CASHREC.

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-CHOICE                   PIC X(01).
       01  WS-EXIT-SW                  PIC X(01) VALUE 'N'.
           88  EXIT-REQUESTED                    VALUE 'Y'.
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-OK-SW                    PIC X(01).
           88  INPUT-OK                          VALUE 'Y'.
       01  WS-TODAY                    PIC 9(08).
       01  WS-NOW                      PIC X(21).
       01  WS-BRANCH                   PIC X(06).
       01  WS-IN-BRANCH                PIC X(06).
       01  WS-IN-TELLER                PIC X(08).
       01  WS-CONFIRM                  PIC X(01).
       01  WS-I                        PIC 9(01).
      * DENOMINATIONS ENTERED FOR ONE MOVEMENT
       01  WS-IN-PIECES                PIC 9(07) OCCURS 6 TIMES.
       01  WS-IN-COINS                 PIC 9(09)V99.
       01  WS-IN-TOTAL                 PIC 9(11)V99.
       01  WS-TOTAL                    PIC 9(11)V99.
       01  WS-EXPECTED                 PIC S9(11)V99.
       01  WS-ABS-DIFF                 PIC 9(11)V99.
       01  WS-DRAWERS                  PIC 9(04).
       01  WS-DRAWER-CASH              PIC 9(11)V99.
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-SAMT                     PIC -Z,ZZZ,ZZZ,ZZ9.99.
       01  ED-PCS                      PIC ZZZ,ZZ9.
       01  ED-DEN                      PIC ZZ9.
           COPY DENOMS.
           COPY GLCODES.
           COPY GLPARM.
           COPY SESSION.
           COPY AUDPARM.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           OPEN I-O CASH-FILE
           IF WS-FS = '35'
               OPEN OUTPUT CASH-FILE
               CLOSE CASH-FILE
               OPEN I-O CASH-FILE
           END-IF
           IF WS-FS NOT = '00'
               DISPLAY 'CASH POSITION FILE OPEN FAILED, FS=' WS-FS
               GOBACK
           END-IF
           MOVE 'N' TO WS-EXIT-SW
           PERFORM UNTIL EXIT-REQUESTED
               DISPLAY ' '
               DISPLAY '------------- BRANCH CASH -------------'
               DISPLAY ' 1. VAULT POSITION AND OPEN DRAWERS'
               IF SES-CAN-APPROVE
                   DISPLAY ' 2. RECEIVE FROM CURRENCY CHEST'
                   DISPLAY ' 3. REMIT TO CURRENCY CHEST'
                   DISPLAY ' 4. ISSUE CASH TO A TELLER'
               END-IF
               DISPLAY ' 5. MY DRAWER'
               DISPLAY ' 6. CLOSE MY DRAWER (CASH TALLY)'
               DISPLAY ' 9. BACK TO MAIN MENU'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               PERFORM 8990-GUARD-END-OF-INPUT
               EVALUATE WS-CHOICE
                   WHEN '1' PERFORM 1000-POSITION
                   WHEN '2' PERFORM 2000-CHEST-RECEIPT
                   WHEN '3' PERFORM 3000-CHEST-REMIT
                   WHEN '4' PERFORM 4000-ISSUE-TO-TELLER
                   WHEN '5' PERFORM 5000-MY-DRAWER
                   WHEN '6' PERFORM 6000-CLOSE-DRAWER
                   WHEN '9' SET EXIT-REQUESTED TO TRUE
                   WHEN SPACE CONTINUE
                   WHEN OTHER DISPLAY 'INVALID CHOICE.'
               END-EVALUATE
           END-PERFORM
           CLOSE CASH-FILE
           GOBACK.

      *================================================================*
      * 1 - POSITION                                                   *
      *================================================================*
       1000-POSITION.
           PERFORM 7000-ASK-BRANCH
           PERFORM 7100-READ-VAULT
           DISPLAY 'VAULT ' WS-BRANCH
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               MOVE DENOM-VALUE(WS-I) TO ED-DEN
               MOVE CP-PIECES(WS-I)   TO ED-PCS
               COMPUTE ED-AMT = CP-PIECES(WS-I) * DENOM-VALUE(WS-I)
               DISPLAY '  RS ' ED-DEN ' X ' ED-PCS ' = ' ED-AMT
           END-PERFORM
           MOVE CP-COINS TO ED-AMT
           DISPLAY '  COINS             ' ED-AMT
           PERFORM 7300-VAULT-TOTAL
           MOVE WS-TOTAL TO ED-AMT
           DISPLAY '  VAULT TOTAL       ' ED-AMT
           MOVE ZERO TO WS-DRAWERS WS-DRAWER-CASH
           MOVE WS-BRANCH TO CP-BRANCH
           MOVE LOW-VALUES TO CP-HOLDER
           MOVE ZERO TO CP-DATE
           MOVE 'N' TO WS-EOF-SW
           START CASH-FILE KEY IS NOT LESS THAN CP-KEY
               INVALID KEY SET END-OF-FILE TO TRUE
           END-START
           PERFORM UNTIL END-OF-FILE
               READ CASH-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF CP-BRANCH NOT = WS-BRANCH
                           SET END-OF-FILE TO TRUE
                       ELSE
                           IF CP-DATE = WS-TODAY AND CP-OPEN
                               PERFORM 1100-SHOW-DRAWER
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           MOVE WS-DRAWER-CASH TO ED-AMT
           DISPLAY '  OPEN DRAWERS: ' WS-DRAWERS '  HOLDING ' ED-AMT.

       1100-SHOW-DRAWER.
           ADD 1 TO WS-DRAWERS
           COMPUTE WS-EXPECTED = CP-OPENING + CP-RECEIPTS - CP-PAYMENTS
           ADD WS-EXPECTED TO WS-DRAWER-CASH
           MOVE WS-EXPECTED TO ED-SAMT
           DISPLAY '  DRAWER ' CP-HOLDER ' EXPECTED ' ED-SAMT.

      *================================================================*
      * 2/3 - CURRENCY CHEST                                           *
      *================================================================*
       2000-CHEST-RECEIPT.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'NEEDS A MANAGER.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 7000-ASK-BRANCH
           PERFORM 7200-ACCEPT-DENOMS
           IF NOT INPUT-OK
               EXIT PARAGRAPH
           END-IF
           PERFORM 7100-READ-VAULT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               ADD WS-IN-PIECES(WS-I) TO CP-PIECES(WS-I)
           END-PERFORM
           ADD WS-IN-COINS TO CP-COINS
           PERFORM 7400-SAVE-VAULT
           MOVE GLC-CASH           TO GP-DR-GL
           MOVE GLC-CURRENCY-CHEST TO GP-CR-GL
           MOVE 'CASH FROM CURRENCY CHEST' TO GP-NARRATION
           PERFORM 7600-GL
           MOVE WS-IN-TOTAL TO ED-AMT
           DISPLAY 'RECEIVED FROM CHEST: ' ED-AMT
           MOVE 'CHEST IN' TO AU-ACTION
           PERFORM 7700-AUDIT.

       3000-CHEST-REMIT.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'NEEDS A MANAGER.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 7000-ASK-BRANCH
           PERFORM 7200-ACCEPT-DENOMS
           IF NOT INPUT-OK
               EXIT PARAGRAPH
           END-IF
           PERFORM 7100-READ-VAULT
           PERFORM 7500-CHECK-VAULT-STOCK
           IF NOT INPUT-OK
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               SUBTRACT WS-IN-PIECES(WS-I) FROM CP-PIECES(WS-I)
           END-PERFORM
           SUBTRACT WS-IN-COINS FROM CP-COINS
           PERFORM 7400-SAVE-VAULT
           MOVE GLC-CURRENCY-CHEST TO GP-DR-GL
           MOVE GLC-CASH           TO GP-CR-GL
           MOVE 'CASH REMITTED TO CHEST' TO GP-NARRATION
           PERFORM 7600-GL
           MOVE WS-IN-TOTAL TO ED-AMT
           DISPLAY 'REMITTED TO CHEST: ' ED-AMT
           MOVE 'CHEST OUT' TO AU-ACTION
           PERFORM 7700-AUDIT.

      *================================================================*
      * 4 - ISSUE CASH TO A TELLER (OPENS OR TOPS UP HIS DRAWER)       *
      *================================================================*
       4000-ISSUE-TO-TELLER.
           IF NOT SES-CAN-APPROVE
               DISPLAY 'NEEDS A MANAGER.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 7000-ASK-BRANCH
           DISPLAY 'TELLER USER ID : ' WITH NO ADVANCING
           ACCEPT WS-IN-TELLER
           MOVE FUNCTION UPPER-CASE(WS-IN-TELLER) TO WS-IN-TELLER
           IF WS-IN-TELLER = SPACES OR WS-IN-TELLER = 'VAULT'
               DISPLAY 'INVALID TELLER.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 7200-ACCEPT-DENOMS
           IF NOT INPUT-OK
               EXIT PARAGRAPH
           END-IF
           PERFORM 7100-READ-VAULT
           PERFORM 7500-CHECK-VAULT-STOCK
           IF NOT INPUT-OK
               EXIT PARAGRAPH
           END-IF
      *    CHECK THE DRAWER BEFORE TOUCHING THE VAULT
           MOVE WS-BRANCH    TO CP-BRANCH
           MOVE WS-IN-TELLER TO CP-HOLDER
           MOVE WS-TODAY     TO CP-DATE
           READ CASH-FILE
               INVALID KEY MOVE '23' TO WS-FS
           END-READ
           IF WS-FS = '00' AND NOT CP-OPEN
               DISPLAY 'DRAWER ALREADY CLOSED FOR TODAY.'
               EXIT PARAGRAPH
           END-IF
           PERFORM 7100-READ-VAULT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               SUBTRACT WS-IN-PIECES(WS-I) FROM CP-PIECES(WS-I)
           END-PERFORM
           SUBTRACT WS-IN-COINS FROM CP-COINS
           PERFORM 7400-SAVE-VAULT
           MOVE WS-BRANCH    TO CP-BRANCH
           MOVE WS-IN-TELLER TO CP-HOLDER
           MOVE WS-TODAY     TO CP-DATE
           READ CASH-FILE
               INVALID KEY
                   INITIALIZE CASH-RECORD
                   MOVE WS-BRANCH    TO CP-BRANCH
                   MOVE WS-IN-TELLER TO CP-HOLDER
                   MOVE WS-TODAY     TO CP-DATE
                   SET CP-OPEN       TO TRUE
                   MOVE SES-USER-ID  TO CP-OPENED-BY
                   MOVE WS-IN-TOTAL  TO CP-OPENING
                   WRITE CASH-RECORD
                   DISPLAY 'DRAWER OPENED FOR ' WS-IN-TELLER
               NOT INVALID KEY
                   ADD WS-IN-TOTAL TO CP-OPENING
                   REWRITE CASH-RECORD
                   DISPLAY 'DRAWER TOPPED UP FOR ' WS-IN-TELLER
           END-READ
           MOVE WS-IN-TOTAL TO ED-AMT
           DISPLAY 'CASH ISSUED: ' ED-AMT
           MOVE 'ISSUE CASH' TO AU-ACTION
           PERFORM 7700-AUDIT.

      *================================================================*
      * 5 - MY DRAWER                                                  *
      *================================================================*
       5000-MY-DRAWER.
           PERFORM 7800-READ-MY-DRAWER
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-EXPECTED = CP-OPENING + CP-RECEIPTS - CP-PAYMENTS
           MOVE CP-OPENING TO ED-AMT
           DISPLAY 'CASH ISSUED     : ' ED-AMT
           MOVE CP-RECEIPTS TO ED-AMT
           DISPLAY 'RECEIPTS (' CP-RECEIPT-CNT ')  : ' ED-AMT
           MOVE CP-PAYMENTS TO ED-AMT
           DISPLAY 'PAYMENTS (' CP-PAYMENT-CNT ')  : ' ED-AMT
           MOVE WS-EXPECTED TO ED-SAMT
           DISPLAY 'EXPECTED CASH   : ' ED-SAMT
           IF NOT CP-OPEN
               MOVE CP-DIFF TO ED-SAMT
               DISPLAY 'DRAWER CLOSED, DIFFERENCE ' ED-SAMT
           END-IF.

      *================================================================*
      * 6 - CLOSE MY DRAWER: COUNT, COMPARE, RETURN TO VAULT, BOOK DIFF*
      *================================================================*
       6000-CLOSE-DRAWER.
           PERFORM 7800-READ-MY-DRAWER
           IF WS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           IF NOT CP-OPEN
               DISPLAY 'DRAWER IS ALREADY CLOSED.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'COUNT YOUR CASH:'
           PERFORM 7200-ACCEPT-DENOMS
           IF NOT INPUT-OK AND WS-IN-TOTAL NOT = ZERO
               EXIT PARAGRAPH
           END-IF
           PERFORM 7800-READ-MY-DRAWER
           COMPUTE WS-EXPECTED = CP-OPENING + CP-RECEIPTS - CP-PAYMENTS
           MOVE WS-IN-TOTAL TO CP-PHYSICAL
           COMPUTE CP-DIFF = WS-IN-TOTAL - WS-EXPECTED
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               MOVE WS-IN-PIECES(WS-I) TO CP-PIECES(WS-I)
           END-PERFORM
           MOVE WS-IN-COINS TO CP-COINS
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           MOVE WS-NOW(9:6) TO CP-CLOSE-TIME
           IF CP-DIFF = ZERO
               SET CP-TALLIED TO TRUE
           ELSE
               SET CP-DIFFERENCE TO TRUE
           END-IF
           REWRITE CASH-RECORD
           MOVE CP-DIFF TO WS-EXPECTED
      *    NOTES GO BACK TO THE VAULT
           PERFORM 7100-READ-VAULT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               ADD WS-IN-PIECES(WS-I) TO CP-PIECES(WS-I)
           END-PERFORM
           ADD WS-IN-COINS TO CP-COINS
           PERFORM 7400-SAVE-VAULT
           MOVE WS-IN-TOTAL TO ED-AMT
           DISPLAY 'COUNTED         : ' ED-AMT
           EVALUATE TRUE
               WHEN WS-EXPECTED = ZERO
                   DISPLAY 'CASH TALLIED. DRAWER CLOSED.'
               WHEN WS-EXPECTED < ZERO
                   COMPUTE WS-ABS-DIFF = 0 - WS-EXPECTED
                   MOVE WS-ABS-DIFF TO ED-AMT
                   DISPLAY 'CASH SHORT BY ' ED-AMT
                           ' - BOOKED TO SHORTAGE SUSPENSE.'
                   MOVE WS-ABS-DIFF       TO WS-IN-TOTAL
                   MOVE GLC-CASH-SHORTAGE TO GP-DR-GL
                   MOVE GLC-CASH          TO GP-CR-GL
                   MOVE 'TELLER CASH SHORTAGE' TO GP-NARRATION
                   PERFORM 7600-GL
               WHEN OTHER
                   MOVE WS-EXPECTED TO WS-ABS-DIFF
                   MOVE WS-ABS-DIFF TO ED-AMT
                   DISPLAY 'CASH EXCESS OF ' ED-AMT
                           ' - BOOKED TO CASH EXCESS.'
                   MOVE WS-ABS-DIFF     TO WS-IN-TOTAL
                   MOVE GLC-CASH        TO GP-DR-GL
                   MOVE GLC-CASH-EXCESS TO GP-CR-GL
                   MOVE 'TELLER CASH EXCESS' TO GP-NARRATION
                   PERFORM 7600-GL
           END-EVALUATE
           MOVE 'CLOSE DRAWER' TO AU-ACTION
           PERFORM 7700-AUDIT.

      *================================================================*
      * COMMON                                                         *
      *================================================================*
       7000-ASK-BRANCH.
           MOVE SES-BRANCH TO WS-BRANCH
           IF SES-CAN-APPROVE
               DISPLAY 'BRANCH (BLANK = ' SES-BRANCH '): '
                       WITH NO ADVANCING
               MOVE SPACES TO WS-IN-BRANCH
               ACCEPT WS-IN-BRANCH
               IF WS-IN-BRANCH NOT = SPACES
                   MOVE FUNCTION UPPER-CASE(WS-IN-BRANCH) TO WS-BRANCH
               END-IF
           END-IF.

      *    VAULT ROW IN THE RECORD BUFFER (CREATED EMPTY IF MISSING)
       7100-READ-VAULT.
           MOVE WS-BRANCH TO CP-BRANCH
           MOVE 'VAULT'   TO CP-HOLDER
           MOVE ZERO      TO CP-DATE
           READ CASH-FILE
               INVALID KEY
                   INITIALIZE CASH-RECORD
                   MOVE WS-BRANCH TO CP-BRANCH
                   MOVE 'VAULT'   TO CP-HOLDER
                   MOVE ZERO      TO CP-DATE
                   SET CP-OPEN    TO TRUE
                   WRITE CASH-RECORD
           END-READ.

       7200-ACCEPT-DENOMS.
           MOVE 'Y' TO WS-OK-SW
           MOVE ZERO TO WS-IN-TOTAL
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               MOVE DENOM-VALUE(WS-I) TO ED-DEN
               DISPLAY '  RS ' ED-DEN ' NOTES : ' WITH NO ADVANCING
               ACCEPT WS-IN-PIECES(WS-I)
               COMPUTE WS-IN-TOTAL = WS-IN-TOTAL
                       + WS-IN-PIECES(WS-I) * DENOM-VALUE(WS-I)
           END-PERFORM
           DISPLAY '  COINS (AMOUNT): ' WITH NO ADVANCING
           ACCEPT WS-IN-COINS
           ADD WS-IN-COINS TO WS-IN-TOTAL
           MOVE WS-IN-TOTAL TO ED-AMT
           DISPLAY '  TOTAL          : ' ED-AMT
           IF WS-IN-TOTAL = ZERO
               DISPLAY 'NO CASH ENTERED.'
               MOVE 'N' TO WS-OK-SW
           END-IF.

       7300-VAULT-TOTAL.
           MOVE CP-COINS TO WS-TOTAL
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               COMPUTE WS-TOTAL = WS-TOTAL
                       + CP-PIECES(WS-I) * DENOM-VALUE(WS-I)
           END-PERFORM.

       7400-SAVE-VAULT.
           REWRITE CASH-RECORD
               INVALID KEY
                   DISPLAY 'VAULT UPDATE FAILED, FS=' WS-FS
           END-REWRITE.

      *    THE VAULT MUST HOLD EVERY NOTE ASKED FOR, DENOMINATION-WISE
       7500-CHECK-VAULT-STOCK.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               IF WS-IN-PIECES(WS-I) > CP-PIECES(WS-I)
                   MOVE DENOM-VALUE(WS-I) TO ED-DEN
                   MOVE CP-PIECES(WS-I)   TO ED-PCS
                   DISPLAY 'VAULT HAS ONLY ' ED-PCS ' NOTES OF RS '
                           ED-DEN '.'
                   MOVE 'N' TO WS-OK-SW
               END-IF
           END-PERFORM
           IF WS-IN-COINS > CP-COINS
               DISPLAY 'NOT ENOUGH COINS IN THE VAULT.'
               MOVE 'N' TO WS-OK-SW
           END-IF.

       7600-GL.
           MOVE WS-IN-TOTAL TO GP-AMOUNT
           MOVE SPACES TO GP-REF
           STRING 'CASH-' WS-BRANCH DELIMITED BY SIZE INTO GP-REF
           CALL 'GLPOST' USING GL-PARMS
           IF NOT GP-OK
               DISPLAY 'GL POSTING FAILED, RC=' GP-RC
           END-IF.

       7700-AUDIT.
           MOVE 'CASHMNT' TO AU-PROGRAM
           MOVE SPACES TO AU-KEY AU-DETAIL
           STRING WS-BRANCH ' ' CP-HOLDER DELIMITED BY SIZE
               INTO AU-KEY
           MOVE WS-IN-TOTAL TO ED-AMT
           STRING 'AMOUNT ' ED-AMT DELIMITED BY SIZE INTO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS.

       7800-READ-MY-DRAWER.
           MOVE SES-BRANCH  TO CP-BRANCH WS-BRANCH
           MOVE SES-USER-ID TO CP-HOLDER
           MOVE WS-TODAY    TO CP-DATE
           READ CASH-FILE
               INVALID KEY
                   DISPLAY 'NO CASH HAS BEEN ISSUED TO YOU TODAY.'
                   MOVE '23' TO WS-FS
           END-READ.

      * STOPS A MENU SPINNING FOREVER WHEN INPUT IS PIPED AND RUNS OUT
       8990-GUARD-END-OF-INPUT.
           IF WS-CHOICE = SPACE
               ADD 1 TO WS-BLANK-INPUTS
               IF WS-BLANK-INPUTS > 20
                   SET EXIT-REQUESTED TO TRUE
               END-IF
           ELSE
               MOVE ZERO TO WS-BLANK-INPUTS
           END-IF.
