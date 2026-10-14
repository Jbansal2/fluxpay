       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIBILOUT.
      *================================================================*
      * PROGRAM  : CIBILOUT                                            *
      * PURPOSE  : MONTHLY CREDIT BUREAU SUBMISSION. ONE FIXED-WIDTH   *
      *            RECORD PER LOAN THAT WAS LIVE DURING THE MONTH      *
      *            (ACTIVE, NPA, OR CLOSED IN THE MONTH), WITH THE     *
      *            BORROWER'S KYC DATA, BALANCES, DAYS PAST DUE AND    *
      *            ASSET CLASSIFICATION:                               *
      *              DPD 0 STD, 1-30 SMA0, 31-60 SMA1, 61-90 SMA2,     *
      *              OVER 90 OR NPA STATUS -> NPA                      *
      *            A RECORD WITHOUT PAN OR LINKED CUSTOMER IS STILL    *
      *            WRITTEN (THE BUREAU NEEDS THE ACCOUNT) BUT IS       *
      *            LISTED AS INCOMPLETE SO BRANCHES CAN FIX THE KYC.   *
      * LAYOUT   : 'HD' MEMBER X(8) REPORT-DATE 9(8)                   *
      *            'BR' LOAN 9(10) CUST 9(8) NAME X(40) PAN X(10)      *
      *                 DOB 9(8) GENDER X(1) PIN 9(6) TYPE X(2)        *
      *                 OPENED 9(8) LAST-PAID 9(8) CLOSED 9(8)         *
      *                 SANCTIONED 9(11)V99 BALANCE 9(11)V99           *
      *                 OVERDUE 9(11)V99 DPD 9(3) CLASS X(4)           *
      *            'TR' RECORDS 9(7) TOTAL-BALANCE 9(13)V99            *
      *            ACCOUNT TYPE: 02 HOUSING 05 PERSONAL 01 AUTO        *
      *                          07 GOLD 08 AGAINST DEPOSIT            *
      * INPUT    : REPORT DATE YYYYMMDD (BLANK = TODAY) - MONTH END    *
      * OUTPUT   : data/CIBIL_<YYYYMM>.TXT                             *
      * FILES    : LOANMAST (INPUT)                                    *
      * CALLS    : CUSTLKUP, DATEUTIL                                  *
      * RETURN   : 0 = ALL COMPLETE, 4 = INCOMPLETE RECORDS            *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE ASSIGN TO 'data/LOANMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS SEQUENTIAL
               RECORD KEY IS LOAN-NO FILE STATUS IS WS-LOAN-FS.
           SELECT OUT-FILE ASSIGN TO WS-OUT-NAME
               ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS WS-OUT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
           COPY LOANREC.
       FD  OUT-FILE.
       01  OUT-LINE                    PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-LOAN-FS                  PIC X(02).
       01  WS-OUT-FS                   PIC X(02).
       01  WS-OUT-NAME                 PIC X(40).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-REPORT-DATE              PIC 9(08).
       01  WS-REPORT-R REDEFINES WS-REPORT-DATE.
           05  WS-REPORT-YYYYMM        PIC 9(06).
           05  FILLER                  PIC 9(02).
       01  WS-EOF-SW                   PIC X(01) VALUE 'N'.
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-DPD                      PIC S9(05).
       01  WS-COUNTS.
           05  WS-READ                 PIC 9(07) VALUE ZERO.
           05  WS-WRITTEN              PIC 9(07) VALUE ZERO.
           05  WS-INCOMPLETE           PIC 9(07) VALUE ZERO.
           05  WS-CLASS-CNT            PIC 9(07) OCCURS 5 TIMES.
           05  WS-TOTAL-BAL            PIC 9(13)V99 VALUE ZERO.
       01  WS-C                        PIC 9(01).
       01  OUT-HEADER.
           05  FILLER                  PIC X(02) VALUE 'HD'.
           05  FILLER                  PIC X(08) VALUE 'CBNK0001'.
           05  OH-DATE                 PIC 9(08).
       01  OUT-DETAIL.
           05  FILLER                  PIC X(02) VALUE 'BR'.
           05  OD-LOAN-NO              PIC 9(10).
           05  OD-CUST-ID              PIC 9(08).
           05  OD-NAME                 PIC X(40).
           05  OD-PAN                  PIC X(10).
           05  OD-DOB                  PIC 9(08).
           05  OD-GENDER               PIC X(01).
           05  OD-PIN                  PIC 9(06).
           05  OD-TYPE                 PIC X(02).
           05  OD-OPENED               PIC 9(08).
           05  OD-LAST-PAID            PIC 9(08).
           05  OD-CLOSED               PIC 9(08).
           05  OD-SANCTIONED           PIC 9(11)V99.
           05  OD-BALANCE              PIC 9(11)V99.
           05  OD-OVERDUE              PIC 9(11)V99.
           05  OD-DPD                  PIC 9(03).
           05  OD-CLASS                PIC X(04).
       01  OUT-TRAILER.
           05  FILLER                  PIC X(02) VALUE 'TR'.
           05  OT-COUNT                PIC 9(07).
           05  OT-TOTAL                PIC 9(13)V99.
       01  WS-CLASS-NAMES.
           05  FILLER                  PIC X(04) VALUE 'STD '.
           05  FILLER                  PIC X(04) VALUE 'SMA0'.
           05  FILLER                  PIC X(04) VALUE 'SMA1'.
           05  FILLER                  PIC X(04) VALUE 'SMA2'.
           05  FILLER                  PIC X(04) VALUE 'NPA '.
       01  WS-CLASS-R REDEFINES WS-CLASS-NAMES.
           05  WS-CLASS-NAME           PIC X(04) OCCURS 5 TIMES.
       01  ED-CNT                      PIC ZZZ,ZZ9.
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           COPY CLKPARM.
           COPY DATEPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'REPORT DATE (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           IF WS-IN-DATE = SPACES OR WS-IN-DATE NOT NUMERIC
               ACCEPT WS-REPORT-DATE FROM DATE YYYYMMDD
           ELSE
               MOVE WS-IN-DATE TO WS-REPORT-DATE
           END-IF
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > 5
               MOVE ZERO TO WS-CLASS-CNT(WS-C)
           END-PERFORM
           OPEN INPUT LOAN-FILE
           IF WS-LOAN-FS NOT = '00'
               DISPLAY 'CIBILOUT: NO LOAN MASTER - NOTHING TO REPORT'
               GOBACK
           END-IF
           STRING 'data/CIBIL_' WS-REPORT-YYYYMM '.TXT'
               DELIMITED BY SIZE INTO WS-OUT-NAME
           OPEN OUTPUT OUT-FILE
           MOVE WS-REPORT-DATE TO OH-DATE
           WRITE OUT-LINE FROM OUT-HEADER
           PERFORM UNTIL END-OF-FILE
               READ LOAN-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-READ
                       IF NOT LOAN-CLOSED
                          OR LOAN-CLOSE-DATE(1:6) = WS-REPORT-YYYYMM
                           PERFORM 1000-BUILD-RECORD
                       END-IF
               END-READ
           END-PERFORM
           MOVE WS-WRITTEN   TO OT-COUNT
           MOVE WS-TOTAL-BAL TO OT-TOTAL
           WRITE OUT-LINE FROM OUT-TRAILER
           CLOSE LOAN-FILE OUT-FILE
           MOVE 'Y' TO CLK-CLOSE
           CALL 'CUSTLKUP' USING CLK-PARMS
           PERFORM 9000-SUMMARY
           IF WS-INCOMPLETE > ZERO
               MOVE 4 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-BUILD-RECORD.
           INITIALIZE OUT-DETAIL
           MOVE 'BR' TO OUT-DETAIL(1:2)
           MOVE LOAN-NO TO OD-LOAN-NO
           MOVE LOAN-ACCT-NO TO CLK-ACCT-NO
           MOVE 'N' TO CLK-CLOSE
           CALL 'CUSTLKUP' USING CLK-PARMS
           MOVE CLK-CUST-ID  TO OD-CUST-ID
           MOVE CLK-NAME     TO OD-NAME
           MOVE CLK-PAN      TO OD-PAN
           MOVE CLK-DOB      TO OD-DOB
           MOVE CLK-GENDER   TO OD-GENDER
           MOVE CLK-PINCODE  TO OD-PIN
           EVALUATE TRUE
               WHEN LOAN-HOME       MOVE '02' TO OD-TYPE
               WHEN LOAN-PERSONAL   MOVE '05' TO OD-TYPE
               WHEN LOAN-VEHICLE    MOVE '01' TO OD-TYPE
               WHEN LOAN-GOLD       MOVE '07' TO OD-TYPE
               WHEN LOAN-AGAINST-FD MOVE '08' TO OD-TYPE
               WHEN OTHER           MOVE '00' TO OD-TYPE
           END-EVALUATE
           MOVE LOAN-DISB-DATE     TO OD-OPENED
           MOVE LOAN-LAST-PAY-DATE TO OD-LAST-PAID
           MOVE LOAN-CLOSE-DATE    TO OD-CLOSED
           MOVE LOAN-PRINCIPAL     TO OD-SANCTIONED
           MOVE LOAN-OUTSTANDING   TO OD-BALANCE
           COMPUTE OD-OVERDUE = LOAN-OVERDUE-EMIS * LOAN-EMI
                                + LOAN-PENALTY-DUE
           PERFORM 1100-DAYS-PAST-DUE
           MOVE WS-DPD TO OD-DPD
           EVALUATE TRUE
               WHEN LOAN-NPA OR WS-DPD > 90  MOVE 5 TO WS-C
               WHEN WS-DPD > 60              MOVE 4 TO WS-C
               WHEN WS-DPD > 30              MOVE 3 TO WS-C
               WHEN WS-DPD > 0               MOVE 2 TO WS-C
               WHEN OTHER                    MOVE 1 TO WS-C
           END-EVALUATE
           MOVE WS-CLASS-NAME(WS-C) TO OD-CLASS
           ADD 1 TO WS-CLASS-CNT(WS-C)
           WRITE OUT-LINE FROM OUT-DETAIL
           ADD 1 TO WS-WRITTEN
           ADD LOAN-OUTSTANDING TO WS-TOTAL-BAL
           IF NOT CLK-OK OR CLK-PAN = SPACES
               ADD 1 TO WS-INCOMPLETE
               DISPLAY '  INCOMPLETE: LOAN ' LOAN-NO ' ACCOUNT '
                       LOAN-ACCT-NO ' - NO LINKED CUSTOMER / PAN'
           END-IF.

      *    DPD = DAYS SINCE THE OLDEST UNPAID EMI FELL DUE. THE NEXT
      *    DUE DATE MOVES ON EVEN WHEN AN EMI IS MISSED, SO THE OLDEST
      *    ONE IS (OVERDUE EMIS) MONTHS BEFORE IT.
       1100-DAYS-PAST-DUE.
           MOVE ZERO TO WS-DPD
           IF LOAN-OVERDUE-EMIS = ZERO OR LOAN-CLOSED
               EXIT PARAGRAPH
           END-IF
           SET DU-ADD-MONTHS TO TRUE
           MOVE LOAN-NEXT-DUE-DATE TO DU-DATE-1
           COMPUTE DU-NUMBER = 0 - LOAN-OVERDUE-EMIS
           CALL 'DATEUTIL' USING DATE-PARMS
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE DU-RESULT-DATE TO DU-DATE-1
           MOVE WS-REPORT-DATE TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           IF DU-OK AND DU-RESULT-DAYS > ZERO
               MOVE DU-RESULT-DAYS TO WS-DPD
           END-IF
           IF WS-DPD > 999
               MOVE 999 TO WS-DPD
           END-IF.

       9000-SUMMARY.
           DISPLAY ' '
           DISPLAY 'CREDIT BUREAU FILE ' FUNCTION TRIM(WS-OUT-NAME)
           MOVE WS-WRITTEN TO ED-CNT
           DISPLAY 'RECORDS WRITTEN : ' ED-CNT
           MOVE WS-TOTAL-BAL TO ED-AMT
           DISPLAY 'TOTAL BALANCE   : ' ED-AMT
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > 5
               MOVE WS-CLASS-CNT(WS-C) TO ED-CNT
               DISPLAY '  ' WS-CLASS-NAME(WS-C) '          : ' ED-CNT
           END-PERFORM
           MOVE WS-INCOMPLETE TO ED-CNT
           DISPLAY 'INCOMPLETE KYC  : ' ED-CNT.
