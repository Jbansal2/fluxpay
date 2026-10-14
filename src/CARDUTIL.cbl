       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARDUTIL.
      *================================================================*
      * PROGRAM  : CARDUTIL                                            *
      * PURPOSE  : CARD HELPER ROUTINES                                *
      *   'LU' - LUHN CHECK DIGIT FOR THE FIRST 15 DIGITS OF CU-CARD-NO*
      *          AND STORE IT IN DIGIT 16                              *
      *   'LV' - VALIDATE LUHN ON ALL 16 DIGITS (CU-RC 00 = VALID)     *
      *   'PH' - ONE-WAY HASH OF CU-PIN SALTED WITH THE CARD NUMBER    *
      * CALL     : CALL 'CARDUTIL' USING CU-PARMS                      *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-DIGITS                   PIC 9(16).
       01  WS-DIGITS-R REDEFINES WS-DIGITS.
           05  WS-DIGIT                PIC 9(01) OCCURS 16 TIMES.
       01  WS-IX                       PIC 9(02).
       01  WS-POS-FROM-RIGHT           PIC 9(02).
       01  WS-D                        PIC 9(02).
       01  WS-SUM                      PIC 9(04).
       01  WS-CHECK                    PIC 9(01).
       01  WS-HASH-WORK                PIC 9(18).
       01  WS-SALT                     PIC 9(09).

       LINKAGE SECTION.
           COPY CARDPARM.

       PROCEDURE DIVISION USING CU-PARMS.
       0000-MAIN.
           MOVE ZERO TO CU-RC
           EVALUATE CU-FUNCTION
               WHEN 'LU'
                   PERFORM 1000-ADD-CHECK-DIGIT
               WHEN 'LV'
                   PERFORM 2000-VALIDATE-LUHN
               WHEN 'PH'
                   PERFORM 3000-HASH-PIN
               WHEN OTHER
                   MOVE 99 TO CU-RC
           END-EVALUATE
           GOBACK.

      *    LUHN: FROM THE RIGHT, DOUBLE EVERY SECOND DIGIT. WHEN
      *    COMPUTING A CHECK DIGIT, DIGIT 15 IS THE FIRST DOUBLED.
       1000-ADD-CHECK-DIGIT.
           MOVE CU-CARD-NO TO WS-DIGITS
           MOVE ZERO TO WS-DIGIT(16) WS-SUM
           PERFORM VARYING WS-IX FROM 15 BY -1 UNTIL WS-IX < 1
               COMPUTE WS-POS-FROM-RIGHT = 16 - WS-IX
               PERFORM 1100-ADD-DIGIT
           END-PERFORM
           COMPUTE WS-CHECK =
                   FUNCTION MOD(10 - FUNCTION MOD(WS-SUM, 10), 10)
           MOVE WS-CHECK TO WS-DIGIT(16)
           MOVE WS-DIGITS TO CU-CARD-NO.

       1100-ADD-DIGIT.
           MOVE WS-DIGIT(WS-IX) TO WS-D
           IF FUNCTION MOD(WS-POS-FROM-RIGHT, 2) = 1
               COMPUTE WS-D = WS-D * 2
               IF WS-D > 9
                   SUBTRACT 9 FROM WS-D
               END-IF
           END-IF
           ADD WS-D TO WS-SUM.

       2000-VALIDATE-LUHN.
           MOVE CU-CARD-NO TO WS-DIGITS
           MOVE ZERO TO WS-SUM
           PERFORM VARYING WS-IX FROM 16 BY -1 UNTIL WS-IX < 1
               COMPUTE WS-POS-FROM-RIGHT = 16 - WS-IX
               PERFORM 1100-ADD-DIGIT
           END-PERFORM
           IF FUNCTION MOD(WS-SUM, 10) NOT = ZERO
               MOVE 10 TO CU-RC
           END-IF.

      *    ILLUSTRATIVE ONLY - NOT CRYPTOGRAPHIC. SEE CARDREC NOTE.
       3000-HASH-PIN.
           COMPUTE WS-SALT = FUNCTION MOD(CU-CARD-NO, 999999937)
           COMPUTE WS-HASH-WORK =
                   (CU-PIN + 7919) * 104729 + WS-SALT * 31
           COMPUTE CU-HASH = FUNCTION MOD(WS-HASH-WORK, 999999893).
