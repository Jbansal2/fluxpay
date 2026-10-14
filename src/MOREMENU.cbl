       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOREMENU.
      *================================================================*
      * PROGRAM  : MOREMENU                                            *
      * PURPOSE  : SECOND MENU PAGE (MAIN MENU OPTION 0) - THE MAIN    *
      *            MENU HAS USED UP ITS SINGLE-KEY OPTIONS.            *
      *              1 - FRAUD ALERTS          (MANAGER)  FRAUDMNT     *
      *              2 - TRADE FINANCE (BG / LC)          TFMNT        *
      *              3 - FOREX AND NRI REMITTANCES        FXMNT        *
      *              4 - STATEMENT REQUESTS               STMTREQ      *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHOICE                   PIC X(01) VALUE SPACE.
       01  WS-BLANK-INPUTS             PIC 9(03) VALUE ZERO.
           COPY SESSION.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE SPACE TO WS-CHOICE
           PERFORM UNTIL WS-CHOICE = '9'
               DISPLAY ' '
               DISPLAY '------------- MORE OPTIONS -------------'
               IF SES-CAN-APPROVE
                   DISPLAY ' 1. FRAUD ALERTS'
               END-IF
               DISPLAY ' 2. TRADE FINANCE (BANK GUARANTEE / LC)'
               DISPLAY ' 3. FOREX AND NRI REMITTANCES'
               DISPLAY ' 4. STATEMENT REQUESTS'
               DISPLAY ' 5. MUTUAL FUNDS AND INSURANCE'
               DISPLAY ' 6. BRANCH STOCK (CHEQUE BOOKS / CARDS)'
               DISPLAY ' 7. COMPLAINTS AND SERVICE REQUESTS'
               DISPLAY ' 8. SWEEP AND OVERDRAFT AGAINST FD'
               DISPLAY ' A. DEPOSIT RATE CARD'
               IF SES-ADMIN
                   DISPLAY ' B. BRANCH MASTER'
               END-IF
               DISPLAY ' 9. BACK TO MAIN MENU'
               DISPLAY 'ENTER CHOICE: ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
               EVALUATE WS-CHOICE
                   WHEN '1'
                       IF SES-CAN-APPROVE
                           CALL 'FRAUDMNT'
                       ELSE
                           DISPLAY 'YOU ARE NOT AUTHORISED FOR THIS '
                                   'OPTION.'
                       END-IF
                   WHEN '2' CALL 'TFMNT'
                   WHEN '3' CALL 'FXMNT'
                   WHEN '4' CALL 'STMTREQ'
                   WHEN '5' CALL 'MFMNT'
                   WHEN '6' CALL 'INVMNT'
                   WHEN '7' CALL 'CMPMNT'
                   WHEN '8' CALL 'SWEEPMNT'
                   WHEN 'A' CALL 'FDRTMNT'
                   WHEN 'B'
                       IF SES-ADMIN
                           CALL 'BRMNT'
                       ELSE
                           DISPLAY 'YOU ARE NOT AUTHORISED FOR THIS '
                                   'OPTION.'
                       END-IF
                   WHEN '9' CONTINUE
                   WHEN SPACE
      *                INPUT EXHAUSTED: LEAVE AFTER 20 BLANKS
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
           GOBACK.
