       IDENTIFICATION DIVISION.
       PROGRAM-ID. TDS26AS.
      *================================================================*
      * PROGRAM  : TDS26AS                                             *
      * PURPOSE  : RECONCILES THE TDS THE BANK DEDUCTED (TDSLOG) WITH  *
      *            WHAT THE INCOME TAX DEPARTMENT SHOWS AGAINST THE    *
      *            BANK'S TAN (THE FORM 26AS / TRACES EXTRACT,         *
      *            data/F26AS.DAT). DEDUCTIONS ARE SUMMED PER PAN OF   *
      *            THE PRIMARY HOLDER. EXCEPTIONS:                     *
      *              MISSING IN 26AS  - DEDUCTED BUT NOT CREDITED TO   *
      *                                 THE CUSTOMER: CORRECT THE TDS  *
      *                                 RETURN OR THE CUSTOMER LOSES   *
      *                                 THE CREDIT                     *
      *              NOT IN BANK      - TAX DEPARTMENT SHOWS TDS THE   *
      *                                 BANK HAS NO RECORD OF          *
      *              AMOUNT DIFFERS                                    *
      *              NO PAN           - DEDUCTION ON AN ACCOUNT WITH   *
      *                                 NO LINKED PAN (CANNOT FILE)    *
      *            F26AS LAYOUT: PAN X(10) FY-START 9(4)               *
      *                          AMOUNT 9(11)V99                       *
      * INPUT    : FINANCIAL YEAR START (E.G. 2026)                    *
      * OUTPUT   : data/TDS26AS_<FY>.TXT                               *
      * FILES    : TDSLOG, F26AS (INPUT)                               *
      * CALLS    : CUSTLKUP                                            *
      * RETURN   : 0 = MATCHED, 4 = EXCEPTIONS, 12 = NO 26AS FILE      *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TDS-FILE ASSIGN TO 'data/TDSLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TDS-FS.
           SELECT AS-FILE ASSIGN TO 'data/F26AS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-AS-FS.
           SELECT RPT-FILE ASSIGN TO WS-RPT-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  TDS-FILE.
           COPY TDSREC.
       FD  AS-FILE.
       01  AS-RECORD.
           05  AS-PAN                  PIC X(10).
           05  AS-FY-START             PIC 9(04).
           05  AS-AMOUNT               PIC 9(11)V99.
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-TDS-FS                   PIC X(02).
       01  WS-AS-FS                    PIC X(02).
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-NAME                 PIC X(40).
       01  WS-IN-FY                    PIC X(04).
       01  WS-FY                       PIC 9(04).
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
      * PER-PAN TOTALS: BANK SIDE AND 26AS SIDE
       01  WS-PAN-COUNT                PIC 9(04) VALUE ZERO.
       01  WS-PAN-TABLE.
           05  WS-PAN-ENTRY OCCURS 2000 TIMES INDEXED BY PX.
               10  PT-PAN              PIC X(10).
               10  PT-BANK             PIC 9(11)V99.
               10  PT-26AS             PIC 9(11)V99.
               10  PT-IN-26AS          PIC X(01).
       01  WS-WANTED-PAN               PIC X(10).
       01  WS-FOUND-SW                 PIC X(01).
           88  PAN-FOUND                         VALUE 'Y'.
       01  WS-TOTALS.
           05  WS-BANK-TOTAL           PIC 9(13)V99 VALUE ZERO.
           05  WS-26AS-TOTAL           PIC 9(13)V99 VALUE ZERO.
           05  WS-NO-PAN-AMT           PIC 9(13)V99 VALUE ZERO.
           05  WS-NO-PAN-CNT           PIC 9(05) VALUE ZERO.
           05  WS-MATCHED              PIC 9(05) VALUE ZERO.
           05  WS-EXCEPTIONS           PIC 9(05) VALUE ZERO.
       01  WS-LINE                     PIC X(100).
       01  WS-RESULT                   PIC X(16).
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-AMT2                     PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-CNT                      PIC ZZ,ZZ9.
           COPY CLKPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'FINANCIAL YEAR START (YYYY): ' WITH NO ADVANCING
           ACCEPT WS-IN-FY
           IF WS-IN-FY IS NUMERIC
               MOVE WS-IN-FY TO WS-FY
           ELSE
               MOVE FUNCTION CURRENT-DATE(1:4) TO WS-FY
               IF FUNCTION CURRENT-DATE(5:2) < '04'
                   SUBTRACT 1 FROM WS-FY
               END-IF
           END-IF
           OPEN INPUT AS-FILE
           IF WS-AS-FS NOT = '00'
               DISPLAY 'TDS26AS: NO 26AS EXTRACT data/F26AS.DAT'
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           STRING 'data/TDS26AS_' WS-FY '.TXT' DELIMITED BY SIZE
               INTO WS-RPT-NAME
           OPEN OUTPUT RPT-FILE
           STRING 'TDS RECONCILIATION WITH FORM 26AS - FY ' WS-FY
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           PERFORM 1000-LOAD-BANK-SIDE
           PERFORM 2000-LOAD-26AS-SIDE
           CLOSE AS-FILE
           PERFORM 3000-COMPARE
           PERFORM 9000-TOTALS
           CLOSE RPT-FILE
           IF WS-EXCEPTIONS > ZERO OR WS-NO-PAN-CNT > ZERO
               MOVE 4 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-LOAD-BANK-SIDE.
           OPEN INPUT TDS-FILE
           IF WS-TDS-FS NOT = '00'
               MOVE 'NO TDS DEDUCTED BY THE BANK (NO TDSLOG).'
                   TO WS-LINE
               PERFORM 8000-PUT
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ TDS-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF TDS-FY-START = WS-FY
                           PERFORM 1100-ADD-DEDUCTION
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TDS-FILE
           MOVE 'Y' TO CLK-CLOSE
           CALL 'CUSTLKUP' USING CLK-PARMS.

       1100-ADD-DEDUCTION.
           ADD TDS-AMOUNT TO WS-BANK-TOTAL
           MOVE TDS-ACCT-NO TO CLK-ACCT-NO
           MOVE 'N' TO CLK-CLOSE
           CALL 'CUSTLKUP' USING CLK-PARMS
           IF NOT CLK-OK OR CLK-PAN = SPACES
               ADD 1 TO WS-NO-PAN-CNT
               ADD TDS-AMOUNT TO WS-NO-PAN-AMT
               MOVE TDS-AMOUNT TO ED-AMT
               STRING 'NO PAN           ACCOUNT ' TDS-ACCT-NO
                      ' TDS ' ED-AMT DELIMITED BY SIZE INTO WS-LINE
               PERFORM 8000-PUT
               EXIT PARAGRAPH
           END-IF
           MOVE CLK-PAN TO WS-WANTED-PAN
           PERFORM 7000-FIND-OR-ADD
           IF PAN-FOUND
               ADD TDS-AMOUNT TO PT-BANK(PX)
           END-IF.

       2000-LOAD-26AS-SIDE.
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ AS-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF AS-FY-START = WS-FY
                           ADD AS-AMOUNT TO WS-26AS-TOTAL
                           MOVE AS-PAN TO WS-WANTED-PAN
                           PERFORM 7000-FIND-OR-ADD
                           IF PAN-FOUND
                               ADD AS-AMOUNT TO PT-26AS(PX)
                               MOVE 'Y' TO PT-IN-26AS(PX)
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

       3000-COMPARE.
           PERFORM 8000-PUT
           MOVE 'PAN                   BANK TDS        26AS TDS  RESULT'
               TO WS-LINE
           PERFORM 8000-PUT
           PERFORM VARYING PX FROM 1 BY 1 UNTIL PX > WS-PAN-COUNT
               EVALUATE TRUE
                   WHEN PT-BANK(PX) = PT-26AS(PX)
                       MOVE 'MATCHED' TO WS-RESULT
                       ADD 1 TO WS-MATCHED
                   WHEN PT-IN-26AS(PX) NOT = 'Y'
                       MOVE 'MISSING IN 26AS' TO WS-RESULT
                       ADD 1 TO WS-EXCEPTIONS
                   WHEN PT-BANK(PX) = ZERO
                       MOVE 'NOT IN BANK' TO WS-RESULT
                       ADD 1 TO WS-EXCEPTIONS
                   WHEN OTHER
                       MOVE 'AMOUNT DIFFERS' TO WS-RESULT
                       ADD 1 TO WS-EXCEPTIONS
               END-EVALUATE
               MOVE PT-BANK(PX) TO ED-AMT
               MOVE PT-26AS(PX) TO ED-AMT2
               STRING PT-PAN(PX) ' ' ED-AMT ' ' ED-AMT2 '  '
                      WS-RESULT DELIMITED BY SIZE INTO WS-LINE
               PERFORM 8000-PUT
           END-PERFORM.

      *    PX POINTS AT THE PAN'S ROW (ADDED IF NEW); TABLE FULL = NOT
      *    FOUND, REPORTED ONCE
       7000-FIND-OR-ADD.
           MOVE 'N' TO WS-FOUND-SW
           PERFORM VARYING PX FROM 1 BY 1
                   UNTIL PX > WS-PAN-COUNT OR PAN-FOUND
               IF PT-PAN(PX) = WS-WANTED-PAN
                   SET PAN-FOUND TO TRUE
               END-IF
           END-PERFORM
           IF PAN-FOUND
               SET PX DOWN BY 1
               EXIT PARAGRAPH
           END-IF
           IF WS-PAN-COUNT >= 2000
               DISPLAY 'TDS26AS: PAN TABLE FULL - ' WS-WANTED-PAN
                       ' SKIPPED'
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-PAN-COUNT
           SET PX TO WS-PAN-COUNT
           MOVE WS-WANTED-PAN TO PT-PAN(PX)
           MOVE ZERO TO PT-BANK(PX) PT-26AS(PX)
           MOVE 'N' TO PT-IN-26AS(PX)
           SET PAN-FOUND TO TRUE.

       8000-PUT.
           DISPLAY FUNCTION TRIM(WS-LINE TRAILING)
           WRITE RPT-LINE FROM WS-LINE
           MOVE SPACES TO WS-LINE.

       9000-TOTALS.
           PERFORM 8000-PUT
           MOVE WS-BANK-TOTAL TO ED-AMT
           STRING 'TOTAL DEDUCTED BY BANK : ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-26AS-TOTAL TO ED-AMT
           STRING 'TOTAL SHOWN IN 26AS    : ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-MATCHED TO ED-CNT
           STRING 'PANS MATCHED           : ' ED-CNT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-EXCEPTIONS TO ED-CNT
           STRING 'PAN EXCEPTIONS         : ' ED-CNT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-NO-PAN-CNT TO ED-CNT
           MOVE WS-NO-PAN-AMT TO ED-AMT
           STRING 'DEDUCTIONS WITHOUT PAN : ' ED-CNT ' ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT.
