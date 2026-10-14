       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUDEXRPT.
      *================================================================*
      * PROGRAM  : AUDEXRPT                                            *
      * PURPOSE  : EXCEPTION REPORT FOR INTERNAL AUDIT / RBI           *
      *            INSPECTION, BUILT FROM THE AUDIT TRAIL FOR A DATE   *
      *            RANGE. SECTIONS:                                    *
      *              A. FAILED SIGN-ONS - USERS WITH 3+ IN A DAY, AND  *
      *                 LOCKED USER IDS                                *
      *              B. ACCESS REFUSED - MENU OPTIONS OUTSIDE A ROLE,  *
      *                 SELF-APPROVAL ATTEMPTS (MAKER = CHECKER)       *
      *              C. AFTER-HOURS ACTIVITY - ANY HUMAN USER ACTION   *
      *                 BEFORE 08:00 OR AFTER 20:00 (BATCH EXCLUDED)   *
      *              D. SENSITIVE CHANGES - RATES, RESIDENCY, FREEZES, *
      *                 WRITE-OFFS, SETTLEMENTS, FRAUD DECISIONS,      *
      *                 USER ADMINISTRATION                            *
      *              E. ACTIVITY BY USER - COUNT OF AUDITED ACTIONS    *
      * INPUT    : FROM DATE, TO DATE (BLANK = TODAY FOR BOTH)         *
      * OUTPUT   : data/AUDEX_<FROM>_<TO>.TXT                          *
      * FILES    : AUDITLOG (INPUT)                                    *
      * RETURN   : 0 = NO EXCEPTIONS, 4 = EXCEPTIONS LISTED            *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AUD-FILE ASSIGN TO 'data/AUDITLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-AUD-FS.
           SELECT RPT-FILE ASSIGN TO WS-RPT-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  AUD-FILE.
           COPY AUDREC.
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(110).

       WORKING-STORAGE SECTION.
       01  WS-AUD-FS                   PIC X(02).
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-NAME                 PIC X(50).
       01  WS-IN-FROM                  PIC X(08).
       01  WS-IN-TO                    PIC X(08).
       01  WS-FROM                     PIC 9(08).
       01  WS-TO                       PIC 9(08).
       01  WS-EOF-SW                   PIC X(01) VALUE 'N'.
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-LINE                     PIC X(110).
       01  WS-PASS                     PIC 9(01).
      * FAILED SIGN-ONS PER USER AND DAY
       01  WS-FAIL-COUNT               PIC 9(04) VALUE ZERO.
       01  WS-FAIL-TABLE.
           05  WS-FAIL OCCURS 500 TIMES.
               10  FL-DATE             PIC 9(08).
               10  FL-USER             PIC X(08).
               10  FL-COUNT            PIC 9(04).
      * ACTIONS PER USER
       01  WS-USER-COUNT               PIC 9(04) VALUE ZERO.
       01  WS-USER-TABLE.
           05  WS-USR OCCURS 200 TIMES.
               10  UT-USER             PIC X(08).
               10  UT-ACTIONS          PIC 9(07).
               10  UT-AFTER-HOURS      PIC 9(05).
       01  WS-I                        PIC 9(04).
       01  WS-FOUND-SW                 PIC X(01).
           88  FOUND                             VALUE 'Y'.
       01  WS-COUNTS.
           05  WS-READ                 PIC 9(07) VALUE ZERO.
           05  WS-IN-RANGE             PIC 9(07) VALUE ZERO.
           05  WS-SEC-CNT              PIC 9(05) OCCURS 4 TIMES.
       01  WS-SENSITIVE-SW             PIC X(01).
           88  SENSITIVE                         VALUE 'Y'.
       01  ED-CNT                      PIC ZZ,ZZ9.
       01  ED-CNT7                     PIC Z,ZZZ,ZZ9.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'FROM DATE (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-FROM
           DISPLAY 'TO DATE   (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-TO
           IF WS-IN-FROM IS NUMERIC
               MOVE WS-IN-FROM TO WS-FROM
           ELSE
               ACCEPT WS-FROM FROM DATE YYYYMMDD
           END-IF
           IF WS-IN-TO IS NUMERIC
               MOVE WS-IN-TO TO WS-TO
           ELSE
               ACCEPT WS-TO FROM DATE YYYYMMDD
           END-IF
           MOVE ZERO TO WS-SEC-CNT(1) WS-SEC-CNT(2) WS-SEC-CNT(3)
                        WS-SEC-CNT(4)
           STRING 'data/AUDEX_' WS-FROM '_' WS-TO '.TXT'
               DELIMITED BY SIZE INTO WS-RPT-NAME
           OPEN OUTPUT RPT-FILE
           STRING 'AUDIT EXCEPTION REPORT ' WS-FROM ' TO ' WS-TO
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
      *    PASS 1 COLLECTS COUNTS, PASSES 2-4 PRINT ONE SECTION EACH
           PERFORM 1000-PASS-COUNTS
           PERFORM 2000-SECTION-A
           MOVE 2 TO WS-PASS
           PERFORM 8100-HEADING
           PERFORM 3000-SCAN
           MOVE 3 TO WS-PASS
           PERFORM 8100-HEADING
           PERFORM 3000-SCAN
           MOVE 4 TO WS-PASS
           PERFORM 8100-HEADING
           PERFORM 3000-SCAN
           PERFORM 4000-SECTION-E
           CLOSE RPT-FILE
           IF WS-SEC-CNT(1) + WS-SEC-CNT(2) + WS-SEC-CNT(3) > ZERO
               MOVE 4 TO RETURN-CODE
           END-IF
           GOBACK.

      *================================================================*
      * PASS 1 - FAILED SIGN-ONS PER USER/DAY, ACTIONS PER USER        *
      *================================================================*
       1000-PASS-COUNTS.
           OPEN INPUT AUD-FILE
           IF WS-AUD-FS NOT = '00'
               MOVE 'NO AUDIT TRAIL.' TO WS-LINE
               PERFORM 8000-PUT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ AUD-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-READ
                       IF AUD-DATE >= WS-FROM AND AUD-DATE <= WS-TO
                           ADD 1 TO WS-IN-RANGE
                           PERFORM 1100-COUNT-USER
                           IF AUD-ACTION = 'LOGIN-FAIL'
                               PERFORM 1200-COUNT-FAIL
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE AUD-FILE.

       1100-COUNT-USER.
           MOVE 'N' TO WS-FOUND-SW
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-USER-COUNT OR FOUND
               IF UT-USER(WS-I) = AUD-USER-ID
                   SET FOUND TO TRUE
               END-IF
           END-PERFORM
           IF FOUND
               SUBTRACT 1 FROM WS-I
           ELSE
               IF WS-USER-COUNT >= 200
                   EXIT PARAGRAPH
               END-IF
               ADD 1 TO WS-USER-COUNT
               MOVE WS-USER-COUNT TO WS-I
               MOVE AUD-USER-ID TO UT-USER(WS-I)
               MOVE ZERO TO UT-ACTIONS(WS-I) UT-AFTER-HOURS(WS-I)
           END-IF
           ADD 1 TO UT-ACTIONS(WS-I)
           IF AUD-USER-ID NOT = 'BATCH'
              AND (AUD-TIME < 080000 OR AUD-TIME >= 200000)
               ADD 1 TO UT-AFTER-HOURS(WS-I)
           END-IF.

      *    THE USER ID TRIED IS IN THE KEY (NOBODY IS SIGNED ON YET)
       1200-COUNT-FAIL.
           MOVE 'N' TO WS-FOUND-SW
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-FAIL-COUNT OR FOUND
               IF FL-DATE(WS-I) = AUD-DATE
                  AND FL-USER(WS-I) = AUD-KEY(1:8)
                   SET FOUND TO TRUE
                   ADD 1 TO FL-COUNT(WS-I)
               END-IF
           END-PERFORM
           IF NOT FOUND AND WS-FAIL-COUNT < 500
               ADD 1 TO WS-FAIL-COUNT
               MOVE AUD-DATE     TO FL-DATE(WS-FAIL-COUNT)
               MOVE AUD-KEY(1:8) TO FL-USER(WS-FAIL-COUNT)
               MOVE 1            TO FL-COUNT(WS-FAIL-COUNT)
           END-IF.

      *================================================================*
      * A - FAILED SIGN-ONS                                            *
      *================================================================*
       2000-SECTION-A.
           MOVE 1 TO WS-PASS
           PERFORM 8100-HEADING
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-FAIL-COUNT
               IF FL-COUNT(WS-I) >= 3
                   ADD 1 TO WS-SEC-CNT(1)
                   MOVE FL-COUNT(WS-I) TO ED-CNT
                   STRING '  ' FL-DATE(WS-I) '  USER ' FL-USER(WS-I)
                          '  FAILED SIGN-ONS ' ED-CNT
                          DELIMITED BY SIZE INTO WS-LINE
                   PERFORM 8000-PUT
               END-IF
           END-PERFORM.

      *================================================================*
      * PASSES 2-4 OVER THE TRAIL                                      *
      *================================================================*
       3000-SCAN.
           OPEN INPUT AUD-FILE
           IF WS-AUD-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ AUD-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF AUD-DATE >= WS-FROM AND AUD-DATE <= WS-TO
                           EVALUATE WS-PASS
                               WHEN 2 PERFORM 3100-REFUSED
                               WHEN 3 PERFORM 3200-AFTER-HOURS
                               WHEN 4 PERFORM 3300-SENSITIVE
                           END-EVALUATE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE AUD-FILE.

       3100-REFUSED.
           IF AUD-ACTION = 'DENIED' OR AUD-ACTION = 'SELF-CHECK'
              OR AUD-ACTION = 'LOCKED'
               ADD 1 TO WS-SEC-CNT(2)
               PERFORM 3900-PRINT-ENTRY
           END-IF.

       3200-AFTER-HOURS.
           IF AUD-USER-ID NOT = 'BATCH' AND AUD-USER-ID NOT = SPACES
              AND (AUD-TIME < 080000 OR AUD-TIME >= 200000)
               ADD 1 TO WS-SEC-CNT(3)
               PERFORM 3900-PRINT-ENTRY
           END-IF.

       3300-SENSITIVE.
           MOVE 'N' TO WS-SENSITIVE-SW
           EVALUATE AUD-PROGRAM
               WHEN 'RATEMNT'  WHEN 'FRAUDMNT' WHEN 'USERMNT'
                   SET SENSITIVE TO TRUE
               WHEN 'COLLMNT'
                   IF AUD-ACTION = 'WRITE-OFF' OR AUD-ACTION = 'OTS'
                       SET SENSITIVE TO TRUE
                   END-IF
               WHEN 'FXMNT'
                   IF AUD-ACTION = 'RESIDENCY'
                      OR AUD-ACTION = 'SET RATE'
                       SET SENSITIVE TO TRUE
                   END-IF
               WHEN 'ACCTMNT'
                   SET SENSITIVE TO TRUE
           END-EVALUATE
           IF SENSITIVE
               ADD 1 TO WS-SEC-CNT(4)
               PERFORM 3900-PRINT-ENTRY
           END-IF.

       3900-PRINT-ENTRY.
           STRING '  ' AUD-DATE ' ' AUD-TIME(1:2) ':' AUD-TIME(3:2)
                  ' ' AUD-USER-ID ' ' AUD-PROGRAM ' ' AUD-ACTION ' '
                  AUD-KEY ' ' AUD-DETAIL
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT.

      *================================================================*
      * E - ACTIVITY BY USER                                           *
      *================================================================*
       4000-SECTION-E.
           MOVE 5 TO WS-PASS
           PERFORM 8100-HEADING
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-USER-COUNT
               MOVE UT-ACTIONS(WS-I) TO ED-CNT7
               MOVE UT-AFTER-HOURS(WS-I) TO ED-CNT
               STRING '  ' UT-USER(WS-I) '  ACTIONS ' ED-CNT7
                      '  AFTER HOURS ' ED-CNT
                   DELIMITED BY SIZE INTO WS-LINE
               PERFORM 8000-PUT
           END-PERFORM
           PERFORM 8000-PUT
           MOVE WS-IN-RANGE TO ED-CNT7
           STRING 'AUDIT RECORDS IN RANGE : ' ED-CNT7
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE WS-SEC-CNT(WS-I) TO ED-CNT
               EVALUATE WS-I
                   WHEN 1 STRING 'A FAILED SIGN-ON USERS : ' ED-CNT
                              DELIMITED BY SIZE INTO WS-LINE
                   WHEN 2 STRING 'B ACCESS REFUSED       : ' ED-CNT
                              DELIMITED BY SIZE INTO WS-LINE
                   WHEN 3 STRING 'C AFTER-HOURS ACTIONS  : ' ED-CNT
                              DELIMITED BY SIZE INTO WS-LINE
                   WHEN 4 STRING 'D SENSITIVE CHANGES    : ' ED-CNT
                              DELIMITED BY SIZE INTO WS-LINE
               END-EVALUATE
               PERFORM 8000-PUT
           END-PERFORM.

       8000-PUT.
           DISPLAY FUNCTION TRIM(WS-LINE TRAILING)
           WRITE RPT-LINE FROM WS-LINE
           MOVE SPACES TO WS-LINE.

       8100-HEADING.
           PERFORM 8000-PUT
           EVALUATE WS-PASS
               WHEN 1 MOVE 'A. FAILED SIGN-ONS (3 OR MORE IN A DAY)'
                          TO WS-LINE
               WHEN 2 MOVE 'B. ACCESS REFUSED / LOCKED / SELF-APPROVAL'
                          TO WS-LINE
               WHEN 3
                   MOVE 'C. ACTIVITY BEFORE 08:00 OR AFTER 20:00'
                     TO WS-LINE
               WHEN 4 MOVE 'D. SENSITIVE CHANGES' TO WS-LINE
               WHEN 5 MOVE 'E. ACTIVITY BY USER' TO WS-LINE
           END-EVALUATE
           PERFORM 8000-PUT.
