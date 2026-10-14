       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPOST.
      *================================================================*
      * PROGRAM  : CCPOST                                              *
      * PURPOSE  : APPLIES ONE CREDIT CARD TRANSACTION TO A CARD       *
      *            RECORD IN THE CALLER'S BUFFER, LOGS IT TO CCTXN AND *
      *            POSTS THE GL ENTRY. SHARED BY CCMNT (ONLINE) AND    *
      *            CCBATCH (BILLING).                                  *
      *              P PURCHASE      - AUTHORISED AGAINST THE LIMIT    *
      *              C CASH ADVANCE  - ALSO AGAINST THE CASH LIMIT     *
      *              F FEE, I INTEREST, G GST - NEVER DECLINED         *
      *              Y PAYMENT, R MERCHANT REFUND - REDUCE THE BALANCE *
      *            PAYMENTS CLEAR CASH ADVANCES FIRST (THEY CARRY      *
      *            INTEREST FROM DAY ONE).                             *
      * GL       : P DR 1210 CR 2903   C DR 1210 CR 1002               *
      *            F DR 1210 CR 4101   I DR 1210 CR 4003               *
      *            G DR 1210 CR 2501   R DR 2903 CR 1210               *
      *            Y (CASH) DR 1001 CR 1210; (FROM ACCOUNT) ALREADY    *
      *              POSTED BY ACCTPOST AS DR CUSTOMER CR 1210         *
      * CALL     : CALL 'CCPOST' USING CCP-PARMS CC-RECORD             *
      * FILES    : CCTXN (LINE SEQUENTIAL, EXTEND)                     *
      * CALLS    : GLPOST                                              *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CCT-FILE ASSIGN TO 'data/CCTXN.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CCT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CCT-FILE.
           COPY CCTREC.

       WORKING-STORAGE SECTION.
           COPY COMMWS.
           COPY GLPARM.
           COPY GLCODES.

       01  WS-CCT-FS                   PIC X(02).
       01  WS-AUTH-SEQ                 PIC 9(06) VALUE ZERO.
       01  WS-AVAILABLE                PIC S9(11)V99.
       01  WS-CASH-AVAILABLE           PIC S9(11)V99.
       01  WS-CASH-PART                PIC 9(11)V99.

       LINKAGE SECTION.
           COPY CCPPARM.
           COPY CCREC.

       PROCEDURE DIVISION USING CCP-PARMS CC-RECORD.
       0000-MAIN.
           MOVE ZERO TO CCP-RC
           MOVE SPACES TO CCP-AUTH-CODE
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           IF CCP-RUN-DATE = ZERO
               MOVE WS-CURR-DATE TO CCP-RUN-DATE
           END-IF
           COMPUTE WS-AVAILABLE = CC-LIMIT - CC-OUTSTANDING
           MOVE WS-AVAILABLE TO CCP-AVAILABLE
           IF CCP-AMOUNT NOT > ZERO
               SET CCP-BAD-REQUEST TO TRUE
               GOBACK
           END-IF
           EVALUATE CCP-TYPE
               WHEN 'P'
               WHEN 'C'
                   PERFORM 1000-AUTHORISE
               WHEN 'F'
               WHEN 'I'
               WHEN 'G'
               WHEN 'Y'
               WHEN 'R'
                   CONTINUE
               WHEN OTHER
                   SET CCP-BAD-REQUEST TO TRUE
           END-EVALUATE
           IF NOT CCP-OK
               GOBACK
           END-IF
           PERFORM 2000-APPLY
           PERFORM 3000-LOG
           PERFORM 4000-POST-GL
           COMPUTE CCP-AVAILABLE = CC-LIMIT - CC-OUTSTANDING
           GOBACK.

       1000-AUTHORISE.
           EVALUATE TRUE
               WHEN NOT CC-ACTIVE
                   SET CCP-CARD-BLOCKED TO TRUE
               WHEN CC-EXPIRY-YYYYMM < CCP-RUN-DATE(1:6)
                   SET CCP-CARD-EXPIRED TO TRUE
               WHEN CCP-AMOUNT > WS-AVAILABLE
                   SET CCP-OVER-LIMIT TO TRUE
               WHEN CCP-TYPE = 'C'
                   COMPUTE WS-CASH-AVAILABLE =
                           CC-CASH-LIMIT - CC-CASH-OUTSTANDING
                   IF CCP-AMOUNT > WS-CASH-AVAILABLE
                       SET CCP-OVER-CASH-LIMIT TO TRUE
                   END-IF
           END-EVALUATE
           IF CCP-OK
               ADD 1 TO WS-AUTH-SEQ
               MOVE SPACES TO CCP-AUTH-CODE
               STRING 'A' WS-CURR-TIME(2:3) WS-AUTH-SEQ(5:2)
                      DELIMITED BY SIZE INTO CCP-AUTH-CODE
               END-STRING
           END-IF.

       2000-APPLY.
           IF CCP-TYPE = 'Y' OR CCP-TYPE = 'R'
               SUBTRACT CCP-AMOUNT FROM CC-OUTSTANDING
               IF CCP-TYPE = 'Y'
                   ADD CCP-AMOUNT TO CC-PAID-SINCE-STMT
                   IF CCP-AMOUNT > CC-CASH-OUTSTANDING
                       MOVE CC-CASH-OUTSTANDING TO WS-CASH-PART
                   ELSE
                       MOVE CCP-AMOUNT TO WS-CASH-PART
                   END-IF
                   SUBTRACT WS-CASH-PART FROM CC-CASH-OUTSTANDING
               END-IF
           ELSE
               ADD CCP-AMOUNT TO CC-OUTSTANDING
               IF CCP-TYPE = 'C'
                   ADD CCP-AMOUNT TO CC-CASH-OUTSTANDING
               END-IF
           END-IF.

       3000-LOG.
           OPEN EXTEND CCT-FILE
           IF WS-CCT-FS = '35'
               OPEN OUTPUT CCT-FILE
           END-IF
           IF WS-CCT-FS NOT = '00'
               DISPLAY 'CCPOST: WARNING - CARD LOG UNAVAILABLE'
               EXIT PARAGRAPH
           END-IF
           MOVE CC-NO          TO CCT-CARD-NO
           MOVE CCP-RUN-DATE   TO CCT-DATE
           MOVE WS-CURR-TIME   TO CCT-TIME
           MOVE CCP-TYPE       TO CCT-TYPE
           MOVE CCP-AMOUNT     TO CCT-AMOUNT
           MOVE CCP-MERCHANT   TO CCT-MERCHANT
           MOVE CCP-AUTH-CODE  TO CCT-AUTH-CODE
           MOVE CC-OUTSTANDING TO CCT-OUTSTANDING-AFTER
           WRITE CCT-RECORD
           CLOSE CCT-FILE.

       4000-POST-GL.
           MOVE GLC-CARD-RECEIVABLE TO GP-DR-GL
           EVALUATE CCP-TYPE
               WHEN 'P' MOVE GLC-MERCHANT-SETTLE TO GP-CR-GL
               WHEN 'C' MOVE GLC-ATM-CASH        TO GP-CR-GL
               WHEN 'F' MOVE GLC-FEE-INCOME      TO GP-CR-GL
               WHEN 'I' MOVE GLC-CARD-INT-INCOME TO GP-CR-GL
               WHEN 'G' MOVE GLC-GST-PAYABLE     TO GP-CR-GL
               WHEN 'R'
                   MOVE GLC-MERCHANT-SETTLE TO GP-DR-GL
                   MOVE GLC-CARD-RECEIVABLE TO GP-CR-GL
               WHEN 'Y'
                   IF CCP-FROM-ACCOUNT = 'Y'
                       EXIT PARAGRAPH
                   END-IF
                   MOVE GLC-CASH            TO GP-DR-GL
                   MOVE GLC-CARD-RECEIVABLE TO GP-CR-GL
           END-EVALUATE
           MOVE CCP-AMOUNT TO GP-AMOUNT
           MOVE CC-NO(1:16) TO GP-REF
           MOVE CCP-MERCHANT TO GP-NARRATION
           CALL 'GLPOST' USING GL-PARMS
           IF NOT GP-OK
               DISPLAY 'CCPOST: WARNING - GL ENTRY FAILED RC=' GP-RC
           END-IF.
