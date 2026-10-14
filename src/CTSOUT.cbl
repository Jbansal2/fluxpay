       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTSOUT.
      *================================================================*
      * PROGRAM  : CTSOUT                                              *
      * PURPOSE  : BUILDS THE OUTWARD CTS PRESENTATION FILE: EVERY     *
      *            CHEQUE LODGED AT THE COUNTERS AND NOT YET SENT GOES *
      *            INTO ONE BATCH FOR THE CLEARING HOUSE AND IS MARKED *
      *            'IN CLEARING'. HEADER / DETAIL / TRAILER LAYOUT:    *
      *              H  BANK X(4)  DATE 9(8)                           *
      *              D  REF 9(8) IFSC X(11) CHEQUE 9(6) DATE 9(8)      *
      *                 AMOUNT 9(11)V99 IMAGE X(20) ACCOUNT 9(10)      *
      *              T  COUNT 9(6) TOTAL 9(13)V99                      *
      * INPUT    : RUN DATE (BLANK = TODAY)                            *
      * OUTPUT   : data/CTSOUT_<DATE>.TXT                              *
      * FILES    : OUTCHQ (I-O)                                        *
      * RETURN   : 0 = OK (ALSO WHEN NOTHING TO SEND), 12 = FILE ERROR *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OCHQ-FILE ASSIGN TO 'data/OUTCHQ.DAT'
               ORGANIZATION IS INDEXED ACCESS IS SEQUENTIAL
               RECORD KEY IS OC-ID FILE STATUS IS WS-FS.
           SELECT OUT-FILE ASSIGN TO WS-OUT-NAME
               ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS WS-OUT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  OCHQ-FILE.
           COPY OCHQREC.
       FD  OUT-FILE.
       01  OUT-LINE                    PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-OUT-FS                   PIC X(02).
       01  WS-OUT-NAME                 PIC X(40).
       01  WS-IN-RUN-DATE              PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-EOF-SW                   PIC X(01) VALUE 'N'.
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-COUNT                    PIC 9(06) VALUE ZERO.
       01  WS-TOTAL                    PIC 9(13)V99 VALUE ZERO.
       01  OUT-HEADER.
           05  FILLER                  PIC X(01) VALUE 'H'.
           05  FILLER                  PIC X(04) VALUE 'CBNK'.
           05  OH-DATE                 PIC 9(08).
       01  OUT-DETAIL.
           05  FILLER                  PIC X(01) VALUE 'D'.
           05  OD-REF                  PIC 9(08).
           05  OD-IFSC                 PIC X(11).
           05  OD-CHEQUE-NO            PIC 9(06).
           05  OD-CHEQUE-DATE          PIC 9(08).
           05  OD-AMOUNT               PIC 9(11)V99.
           05  OD-IMAGE                PIC X(20).
           05  OD-ACCT-NO              PIC 9(10).
       01  OUT-TRAILER.
           05  FILLER                  PIC X(01) VALUE 'T'.
           05  OT-COUNT                PIC 9(06).
           05  OT-TOTAL                PIC 9(13)V99.
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'RUN DATE (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-RUN-DATE
           IF WS-IN-RUN-DATE = SPACES OR WS-IN-RUN-DATE NOT NUMERIC
               ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           ELSE
               MOVE WS-IN-RUN-DATE TO WS-RUN-DATE
           END-IF
           OPEN I-O OCHQ-FILE
           IF WS-FS = '35'
               DISPLAY 'CTSOUT: NO OUTWARD CHEQUES ON FILE.'
               GOBACK
           END-IF
           IF WS-FS NOT = '00'
               DISPLAY 'CTSOUT: OPEN FAILED, FS=' WS-FS
               MOVE 12 TO RETURN-CODE
               GOBACK
           END-IF
           STRING 'data/CTSOUT_' WS-RUN-DATE '.TXT'
               DELIMITED BY SIZE INTO WS-OUT-NAME
           OPEN OUTPUT OUT-FILE
           MOVE WS-RUN-DATE TO OH-DATE
           WRITE OUT-LINE FROM OUT-HEADER
           PERFORM UNTIL END-OF-FILE
               READ OCHQ-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       IF OC-LODGED
                           PERFORM 1000-PRESENT
                       END-IF
               END-READ
           END-PERFORM
           MOVE WS-COUNT TO OT-COUNT
           MOVE WS-TOTAL TO OT-TOTAL
           WRITE OUT-LINE FROM OUT-TRAILER
           CLOSE OCHQ-FILE OUT-FILE
           MOVE WS-TOTAL TO ED-AMT
           DISPLAY 'CTSOUT: ' WS-COUNT ' CHEQUE(S) PRESENTED, TOTAL '
                   ED-AMT
           DISPLAY 'CTSOUT: FILE ' FUNCTION TRIM(WS-OUT-NAME)
           GOBACK.

       1000-PRESENT.
           MOVE OC-ID          TO OD-REF
           MOVE OC-DRAWEE-IFSC TO OD-IFSC
           MOVE OC-CHEQUE-NO   TO OD-CHEQUE-NO
           MOVE OC-CHEQUE-DATE TO OD-CHEQUE-DATE
           MOVE OC-AMOUNT      TO OD-AMOUNT
           MOVE OC-IMAGE-REF   TO OD-IMAGE
           MOVE OC-ACCT-NO     TO OD-ACCT-NO
           WRITE OUT-LINE FROM OUT-DETAIL
           SET OC-SENT TO TRUE
           MOVE WS-RUN-DATE TO OC-BATCH-DATE
           REWRITE OCHQ-RECORD
           ADD 1 TO WS-COUNT
           ADD OC-AMOUNT TO WS-TOTAL.
