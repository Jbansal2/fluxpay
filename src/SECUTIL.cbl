       IDENTIFICATION DIVISION.
       PROGRAM-ID. SECUTIL.
      *================================================================*
      * PROGRAM  : SECUTIL                                             *
      * PURPOSE  : PASSWORD HASHING AND PASSWORD POLICY CHECK.         *
      *            POLICY: 8-20 CHARACTERS, AT LEAST ONE LETTER AND    *
      *            ONE DIGIT, NO EMBEDDED SPACES.                      *
      * NOTE     : THE HASH IS A SALTED POLYNOMIAL HASH - FINE FOR A   *
      *            LEARNING SYSTEM, NOT FOR PRODUCTION. A REAL SYSTEM  *
      *            WOULD DELEGATE TO RACF / AN LDAP DIRECTORY.         *
      * CALL     : CALL 'SECUTIL' USING SEC-PARMS  (COPY SECPARM)      *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-I                        PIC 9(02).
       01  WS-LEN                      PIC 9(02).
       01  WS-CHAR                     PIC X(01).
       01  WS-HASH                     PIC 9(18).
       01  WS-PRIME                    PIC 9(09) VALUE 999999937.
       01  WS-SALTED                   PIC X(28).
       01  WS-HAS-LETTER               PIC X(01).
       01  WS-HAS-DIGIT                PIC X(01).
       01  WS-HAS-SPACE                PIC X(01).
       01  WS-ROUND                    PIC 9(01).

       LINKAGE SECTION.
           COPY SECPARM.

       PROCEDURE DIVISION USING SEC-PARMS.
       0000-MAIN.
           MOVE ZERO TO SC-RC
           EVALUATE SC-FUNCTION
               WHEN 'PH' PERFORM 1000-HASH
               WHEN 'PV' PERFORM 2000-POLICY
               WHEN OTHER MOVE 99 TO SC-RC
           END-EVALUATE
           GOBACK.

      *    TWO ROUNDS OVER SALT + PASSWORD, BASE 131, MOD A LARGE PRIME
       1000-HASH.
           STRING SC-SALT SC-PASSWORD DELIMITED BY SIZE
                  INTO WS-SALTED
           END-STRING
           MOVE 7 TO WS-HASH
           PERFORM VARYING WS-ROUND FROM 1 BY 1 UNTIL WS-ROUND > 2
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 28
                   MOVE WS-SALTED(WS-I:1) TO WS-CHAR
                   COMPUTE WS-HASH = FUNCTION MOD(
                       WS-HASH * 131 + FUNCTION ORD(WS-CHAR) + WS-I,
                       WS-PRIME)
               END-PERFORM
           END-PERFORM
           MOVE WS-HASH TO SC-HASH.

       2000-POLICY.
           MOVE 'N' TO WS-HAS-LETTER WS-HAS-DIGIT WS-HAS-SPACE
           COMPUTE WS-LEN =
                   FUNCTION LENGTH(FUNCTION TRIM(SC-PASSWORD TRAILING))
           IF SC-PASSWORD = SPACES
               MOVE ZERO TO WS-LEN
           END-IF
           IF WS-LEN < 8
               MOVE 10 TO SC-RC
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
               MOVE SC-PASSWORD(WS-I:1) TO WS-CHAR
               EVALUATE TRUE
                   WHEN WS-CHAR IS ALPHABETIC AND WS-CHAR NOT = SPACE
                       MOVE 'Y' TO WS-HAS-LETTER
                   WHEN WS-CHAR IS NUMERIC
                       MOVE 'Y' TO WS-HAS-DIGIT
                   WHEN WS-CHAR = SPACE
                       MOVE 'Y' TO WS-HAS-SPACE
               END-EVALUATE
           END-PERFORM
           EVALUATE TRUE
               WHEN WS-HAS-SPACE = 'Y'  MOVE 40 TO SC-RC
               WHEN WS-HAS-LETTER = 'N' MOVE 20 TO SC-RC
               WHEN WS-HAS-DIGIT = 'N'  MOVE 30 TO SC-RC
           END-EVALUATE.
