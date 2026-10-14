       IDENTIFICATION DIVISION.
       PROGRAM-ID. CASHRPT.
      *================================================================*
      * PROGRAM  : CASHRPT                                             *
      * PURPOSE  : END-OF-DAY CASH POSITION FOR EVERY BRANCH:          *
      *            VAULT BY DENOMINATION, EACH TELLER DRAWER OF THE    *
      *            DAY WITH ITS TALLY RESULT, AND THE BANK-WIDE TOTAL  *
      *            (VAULTS + OPEN DRAWERS) AGAINST GL 1001.            *
      *            A DRAWER STILL OPEN OR NOT TALLIED IS AN EXCEPTION. *
      * INPUT    : BUSINESS DATE YYYYMMDD (BLANK = TODAY)              *
      * OUTPUT   : data/CASH_<DATE>.TXT                                *
      * FILES    : CASHPOS, GLMAST (INPUT)                             *
      * RETURN   : 0 = ALL TALLIED, 4 = OPEN DRAWERS OR DIFFERENCES    *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CASH-FILE ASSIGN TO 'data/CASHPOS.DAT'
               ORGANIZATION IS INDEXED ACCESS IS SEQUENTIAL
               RECORD KEY IS CP-KEY FILE STATUS IS WS-FS.
           SELECT GL-FILE ASSIGN TO 'data/GLMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS RANDOM
               RECORD KEY IS GL-CODE FILE STATUS IS WS-GL-FS.
           SELECT RPT-FILE ASSIGN TO WS-RPT-NAME
               ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS WS-RPT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  CASH-FILE.
           COPY CASHREC.
       FD  GL-FILE.
           COPY GLREC.
       FD  RPT-FILE.
       01  RPT-LINE                    PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-FS                       PIC X(02).
       01  WS-GL-FS                    PIC X(02).
       01  WS-RPT-FS                   PIC X(02).
       01  WS-RPT-NAME                 PIC X(40).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-DATE                     PIC 9(08).
       01  WS-EOF-SW                   PIC X(01) VALUE 'N'.
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-I                        PIC 9(01).
       01  WS-AMT                      PIC S9(13)V99.
       01  WS-EXPECTED                 PIC S9(13)V99.
       01  WS-VAULTS                   PIC S9(13)V99 VALUE ZERO.
       01  WS-OPEN-CASH                PIC S9(13)V99 VALUE ZERO.
       01  WS-GL-CASH                  PIC S9(15)V99 VALUE ZERO.
       01  WS-DIFF                     PIC S9(15)V99.
       01  WS-EXCEPTIONS               PIC 9(05) VALUE ZERO.
       01  WS-SHORT-TOTAL              PIC S9(13)V99 VALUE ZERO.
       01  WS-EXCESS-TOTAL             PIC S9(13)V99 VALUE ZERO.
       01  WS-RESULT                   PIC X(12).
       01  WS-LINE                     PIC X(100).
       01  ED-AMT                      PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-AMT2                     PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-AMT3                     PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-PCS                      PIC ZZZ,ZZ9.
       01  ED-CNT                      PIC ZZ,ZZ9.
           COPY DENOMS.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'BUSINESS DATE (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           IF WS-IN-DATE = SPACES OR WS-IN-DATE NOT NUMERIC
               ACCEPT WS-DATE FROM DATE YYYYMMDD
           ELSE
               MOVE WS-IN-DATE TO WS-DATE
           END-IF
           STRING 'data/CASH_' WS-DATE '.TXT' DELIMITED BY SIZE
               INTO WS-RPT-NAME
           OPEN OUTPUT RPT-FILE
           STRING 'BRANCH CASH POSITION FOR ' WS-DATE
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           OPEN INPUT CASH-FILE
           IF WS-FS NOT = '00'
               MOVE 'NO CASH POSITIONS RECORDED.' TO WS-LINE
               PERFORM 8000-PUT
               CLOSE RPT-FILE
               GOBACK
           END-IF
           PERFORM UNTIL END-OF-FILE
               READ CASH-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       EVALUATE TRUE
                           WHEN CP-HOLDER = 'VAULT'
                               PERFORM 1000-VAULT
                           WHEN CP-DATE = WS-DATE
                               PERFORM 2000-DRAWER
                       END-EVALUATE
               END-READ
           END-PERFORM
           CLOSE CASH-FILE
           PERFORM 3000-AGAINST-GL
           CLOSE RPT-FILE
           IF WS-EXCEPTIONS > ZERO
               MOVE 4 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-VAULT.
           PERFORM 8000-PUT
           MOVE CP-COINS TO WS-AMT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               COMPUTE WS-AMT = WS-AMT
                       + CP-PIECES(WS-I) * DENOM-VALUE(WS-I)
           END-PERFORM
           ADD WS-AMT TO WS-VAULTS
           MOVE WS-AMT TO ED-AMT
           STRING 'BRANCH ' CP-BRANCH '  VAULT ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               IF CP-PIECES(WS-I) > ZERO
                   MOVE CP-PIECES(WS-I) TO ED-PCS
                   STRING '    RS ' DENOM-VALUE(WS-I) ' X ' ED-PCS
                       DELIMITED BY SIZE INTO WS-LINE
                   PERFORM 8000-PUT
               END-IF
           END-PERFORM.

       2000-DRAWER.
           COMPUTE WS-EXPECTED = CP-OPENING + CP-RECEIPTS - CP-PAYMENTS
           EVALUATE TRUE
               WHEN CP-OPEN
                   MOVE 'NOT CLOSED' TO WS-RESULT
                   ADD 1 TO WS-EXCEPTIONS
                   ADD WS-EXPECTED TO WS-OPEN-CASH
               WHEN CP-TALLIED
                   MOVE 'TALLIED' TO WS-RESULT
               WHEN OTHER
                   IF CP-DIFF < ZERO
                       MOVE 'SHORT' TO WS-RESULT
                       ADD CP-DIFF TO WS-SHORT-TOTAL
                   ELSE
                       MOVE 'EXCESS' TO WS-RESULT
                       ADD CP-DIFF TO WS-EXCESS-TOTAL
                   END-IF
                   ADD 1 TO WS-EXCEPTIONS
           END-EVALUATE
           MOVE WS-EXPECTED TO ED-AMT
           MOVE CP-PHYSICAL TO ED-AMT2
           MOVE CP-DIFF     TO ED-AMT3
           STRING '  DRAWER ' CP-HOLDER ' EXPECTED' ED-AMT
                  ' COUNTED' ED-AMT2 ' DIFF' ED-AMT3 ' ' WS-RESULT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT.

       3000-AGAINST-GL.
           OPEN INPUT GL-FILE
           IF WS-GL-FS = '00'
               MOVE 1001 TO GL-CODE
               READ GL-FILE
                   NOT INVALID KEY MOVE GL-BALANCE TO WS-GL-CASH
               END-READ
               CLOSE GL-FILE
           END-IF
           PERFORM 8000-PUT
           MOVE WS-VAULTS TO ED-AMT
           STRING 'CASH IN VAULTS        ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-OPEN-CASH TO ED-AMT
           STRING 'CASH IN OPEN DRAWERS  ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-GL-CASH TO ED-AMT
           STRING 'GL 1001 CASH IN HAND  ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           COMPUTE WS-DIFF = WS-GL-CASH - WS-VAULTS - WS-OPEN-CASH
           MOVE WS-DIFF TO ED-AMT
           STRING 'GL NOT COVERED BY CASH' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-SHORT-TOTAL TO ED-AMT
           STRING 'SHORTAGES TODAY       ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-EXCESS-TOTAL TO ED-AMT
           STRING 'EXCESS TODAY          ' ED-AMT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT
           MOVE WS-EXCEPTIONS TO ED-CNT
           STRING 'DRAWER EXCEPTIONS     ' ED-CNT
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM 8000-PUT.

       8000-PUT.
           DISPLAY FUNCTION TRIM(WS-LINE TRAILING)
           WRITE RPT-LINE FROM WS-LINE
           MOVE SPACES TO WS-LINE.
