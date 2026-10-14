       IDENTIFICATION DIVISION.
       PROGRAM-ID. CASHUTIL.
      *================================================================*
      * PROGRAM  : CASHUTIL                                            *
      * PURPOSE  : KEEPS A TELLER'S DRAWER IN STEP WITH THE CASH HE    *
      *            TAKES IN AND PAYS OUT. CALLED BY TXNEXEC AFTER A    *
      *            CASH DEPOSIT OR WITHDRAWAL HAS POSTED. IF THE USER  *
      *            HAS NO DRAWER OPEN TODAY RC 04 IS RETURNED AND THE  *
      *            CALLER WARNS - THE POSTING ITSELF IS NOT UNDONE.    *
      * FILES    : CASHPOS (I-O)                                       *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CASH-FILE ASSIGN TO 'data/CASHPOS.DAT'
               ORGANIZATION IS INDEXED ACCESS IS RANDOM
               RECORD KEY IS CP-KEY FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CASH-FILE.
           COPY CASHREC.

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-TODAY                    PIC 9(08).
           COPY SESSION.

       LINKAGE SECTION.
           COPY CSHPARM.

       PROCEDURE DIVISION USING CSH-PARMS.
       0000-MAIN.
           SET CSH-OK TO TRUE
           ACCEPT WS-TODAY FROM DATE YYYYMMDD
           OPEN I-O CASH-FILE
           IF WS-FS = '35'
               SET CSH-NO-DRAWER TO TRUE
               GOBACK
           END-IF
           IF WS-FS NOT = '00'
               SET CSH-FILE-ERROR TO TRUE
               GOBACK
           END-IF
           MOVE CSH-BRANCH TO CP-BRANCH
           MOVE CSH-USER   TO CP-HOLDER
           IF CSH-BRANCH = SPACES
               MOVE SES-BRANCH TO CP-BRANCH
           END-IF
           IF CSH-USER = SPACES
               MOVE SES-USER-ID TO CP-HOLDER
           END-IF
           MOVE WS-TODAY TO CP-DATE
           READ CASH-FILE
               INVALID KEY MOVE '23' TO WS-FS
           END-READ
           IF WS-FS NOT = '00' OR NOT CP-OPEN
               SET CSH-NO-DRAWER TO TRUE
               CLOSE CASH-FILE
               GOBACK
           END-IF
           EVALUATE TRUE
               WHEN CSH-RECEIPT
                   ADD CSH-AMOUNT TO CP-RECEIPTS
                   ADD 1          TO CP-RECEIPT-CNT
               WHEN CSH-PAYMENT
                   ADD CSH-AMOUNT TO CP-PAYMENTS
                   ADD 1          TO CP-PAYMENT-CNT
           END-EVALUATE
           REWRITE CASH-RECORD
               INVALID KEY SET CSH-FILE-ERROR TO TRUE
           END-REWRITE
           CLOSE CASH-FILE
           GOBACK.
