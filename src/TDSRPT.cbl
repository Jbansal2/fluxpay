       IDENTIFICATION DIVISION.
       PROGRAM-ID. TDSRPT.
      *================================================================*
      * PROGRAM  : TDSRPT                                              *
      * PURPOSE  : TDS CERTIFICATES (FORM 16A STYLE) FOR ONE FINANCIAL *
      *            YEAR - ONE BLOCK PER ACCOUNT LISTING EVERY DEPOSIT  *
      *            PAYOUT WITH INTEREST, TDS AND NET, PLUS A BANK-WIDE *
      *            TOTAL THAT SHOULD EQUAL THE TDS PAYABLE GL (2502).  *
      * INPUT    : FY START YEAR (BLANK = CURRENT FY),                 *
      *            ACCOUNT NO (BLANK = ALL ACCOUNTS)                   *
      * OUTPUT   : SCREEN AND data/TDS_<FY>.TXT                        *
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
           SELECT SORT-FILE ASSIGN TO 'data/SORTWK3.TMP'.
           SELECT RPT-FILE ASSIGN TO WS-RPT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  TDS-FILE.
           COPY TDSREC.
       SD  SORT-FILE.
       01  SORT-RECORD.
           05  SR-ACCT-NO              PIC 9(10).
           05  SR-DATE                 PIC 9(08).
           05  SR-DEP-NO               PIC 9(10).
           05  SR-INTEREST             PIC 9(11)V99.
           05  SR-TDS                  PIC 9(11)V99.
           05  SR-NET                  PIC 9(11)V99.
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(90).

       WORKING-STORAGE SECTION.
           COPY COMMWS.
       01  WS-TDS-FS                   PIC X(02).
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-FILENAME             PIC X(40).
       01  WS-RPT-WORK                 PIC X(90).
       01  WS-IN-FY                    PIC X(04).
       01  WS-IN-ACCT                  PIC X(10).
       01  WS-FY                       PIC 9(04).
       01  WS-FY-END                   PIC 9(04).
       01  WS-ACCT-FILTER              PIC 9(10) VALUE ZERO.
       01  WS-EOF-SW                   PIC X(01).
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-SORT-EOF-SW              PIC X(01).
           88  END-OF-SORT                       VALUE 'Y'.
       01  WS-PREV-ACCT                PIC 9(10).
       01  WS-ACC-INT                  PIC 9(13)V99.
       01  WS-ACC-TDS                  PIC 9(13)V99.
       01  WS-GRAND-INT                PIC 9(15)V99.
       01  WS-GRAND-TDS                PIC 9(15)V99.
       01  WS-CERT-CNT                 PIC 9(05).
       01  WS-E-INT                    PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-E-TDS                    PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-E-NET                    PIC ZZ,ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE ZERO TO WS-GRAND-INT WS-GRAND-TDS WS-CERT-CNT
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
           DISPLAY 'FY START YEAR (BLANK = CURRENT): ' WITH NO ADVANCING
           ACCEPT WS-IN-FY
           DISPLAY 'ACCOUNT NO (BLANK = ALL)       : ' WITH NO ADVANCING
           ACCEPT WS-IN-ACCT
           IF WS-IN-FY IS NUMERIC
               MOVE WS-IN-FY TO WS-FY
           ELSE
               IF WS-CURR-DATE(5:2) >= '04'
                   MOVE WS-CURR-DATE(1:4) TO WS-FY
               ELSE
                   COMPUTE WS-FY = FUNCTION NUMVAL(WS-CURR-DATE(1:4))
                                 - 1
               END-IF
           END-IF
           IF WS-IN-ACCT IS NUMERIC
               MOVE WS-IN-ACCT TO WS-ACCT-FILTER
           END-IF
           COMPUTE WS-FY-END = WS-FY + 1
           STRING 'data/TDS_' WS-FY '.TXT' DELIMITED BY SIZE
                  INTO WS-RPT-FILENAME
           END-STRING
           OPEN OUTPUT RPT-FILE
           SORT SORT-FILE
               ON ASCENDING KEY SR-ACCT-NO SR-DATE
               INPUT PROCEDURE  IS 1000-SELECT-ENTRIES
               OUTPUT PROCEDURE IS 2000-PRINT-CERTIFICATES
           MOVE ALL '=' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           MOVE WS-GRAND-INT TO WS-E-INT
           MOVE WS-GRAND-TDS TO WS-E-TDS
           STRING 'FY ' WS-FY '-' WS-FY-END(3:2) ' TOTAL: '
                  'CERTIFICATES ' WS-CERT-CNT '  INTEREST '
                  FUNCTION TRIM(WS-E-INT) '  TDS '
                  FUNCTION TRIM(WS-E-TDS)
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT
           CLOSE RPT-FILE
           DISPLAY 'REPORT SAVED TO ' WS-RPT-FILENAME
           MOVE ZERO TO RETURN-CODE
           GOBACK.

       1000-SELECT-ENTRIES.
           OPEN INPUT TDS-FILE
           IF WS-TDS-FS NOT = '00'
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-EOF-SW
           PERFORM UNTIL END-OF-FILE
               READ TDS-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF TDS-FY-START = WS-FY
                          AND (WS-ACCT-FILTER = ZERO
                               OR TDS-ACCT-NO = WS-ACCT-FILTER)
                           MOVE TDS-ACCT-NO  TO SR-ACCT-NO
                           MOVE TDS-DATE     TO SR-DATE
                           MOVE TDS-DEP-NO   TO SR-DEP-NO
                           MOVE TDS-INTEREST TO SR-INTEREST
                           MOVE TDS-AMOUNT   TO SR-TDS
                           MOVE TDS-NET-PAID TO SR-NET
                           RELEASE SORT-RECORD
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TDS-FILE.

       2000-PRINT-CERTIFICATES.
           MOVE 'N' TO WS-SORT-EOF-SW
           MOVE ZERO TO WS-PREV-ACCT
           PERFORM UNTIL END-OF-SORT
               RETURN SORT-FILE
                   AT END SET END-OF-SORT TO TRUE
                   NOT AT END
                       IF SR-ACCT-NO NOT = WS-PREV-ACCT
                           IF WS-PREV-ACCT NOT = ZERO
                               PERFORM 2200-CERT-FOOTER
                           END-IF
                           PERFORM 2100-CERT-HEADER
                       END-IF
                       PERFORM 2300-CERT-LINE
               END-RETURN
           END-PERFORM
           IF WS-PREV-ACCT NOT = ZERO
               PERFORM 2200-CERT-FOOTER
           END-IF.

       2100-CERT-HEADER.
           MOVE SR-ACCT-NO TO WS-PREV-ACCT
           MOVE ZERO TO WS-ACC-INT WS-ACC-TDS
           ADD 1 TO WS-CERT-CNT
           MOVE ALL '=' TO WS-RPT-WORK
           PERFORM 8000-EMIT
           STRING 'CERTIFICATE OF TAX DEDUCTED AT SOURCE - FY '
                  WS-FY '-' WS-FY-END(3:2)
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT
           STRING 'DEDUCTEE ACCOUNT: ' SR-ACCT-NO
                  '   DEDUCTOR: CORE BANK (TAN: XXXX00000X)'
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT
           MOVE 'DATE      DEPOSIT NO            INTEREST'
                                                 TO WS-RPT-WORK
           MOVE 'TDS            NET PAID' TO WS-RPT-WORK(52:)
           PERFORM 8000-EMIT.

       2200-CERT-FOOTER.
           MOVE WS-ACC-INT TO WS-E-INT
           MOVE WS-ACC-TDS TO WS-E-TDS
           STRING 'TOTAL                ' WS-E-INT ' ' WS-E-TDS
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT
           ADD WS-ACC-INT TO WS-GRAND-INT
           ADD WS-ACC-TDS TO WS-GRAND-TDS.

       2300-CERT-LINE.
           MOVE SR-INTEREST TO WS-E-INT
           MOVE SR-TDS      TO WS-E-TDS
           MOVE SR-NET      TO WS-E-NET
           STRING SR-DATE '  ' SR-DEP-NO ' ' WS-E-INT ' ' WS-E-TDS ' '
                  WS-E-NET
                  DELIMITED BY SIZE INTO WS-RPT-WORK
           END-STRING
           PERFORM 8000-EMIT
           ADD SR-INTEREST TO WS-ACC-INT
           ADD SR-TDS      TO WS-ACC-TDS.

       8000-EMIT.
           MOVE WS-RPT-WORK TO RPT-LINE
           WRITE RPT-LINE
           DISPLAY FUNCTION TRIM(WS-RPT-WORK TRAILING)
           MOVE SPACES TO WS-RPT-WORK.
