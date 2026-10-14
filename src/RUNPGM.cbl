       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUNPGM.
      *================================================================*
      * PROGRAM  : RUNPGM                                              *
      * PURPOSE  : BATCH LAUNCHER - THE "EXEC PGM=" OF THIS SYSTEM.    *
      *            RUNS ONE BATCH PROGRAM UNDER THE BATCH USER ID      *
      *            WITHOUT AN INTERACTIVE SIGN-ON, WRITES JOB START /  *
      *            END TO THE AUDIT TRAIL AND PASSES THE STEP RETURN   *
      *            CODE BACK TO THE SHELL SO JOB SCRIPTS CAN TEST IT.  *
      * USAGE    : runpgm <PROGRAM> [< input-file]                     *
      *            ONLY PROGRAMS IN THE ALLOWED LIST BELOW CAN RUN -   *
      *            ONLINE SCREENS ARE NEVER STARTED FROM BATCH.        *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY SESSION.
           COPY AUDPARM.

       01  WS-COMMAND-LINE             PIC X(80).
       01  WS-PROGRAM                  PIC X(08).
           88  PGM-ALLOWED                       VALUES
                                                 'INTCALC' 'EODBATCH'
                                                 'CHGBATCH' 'CLRBATCH'
                                                 'MISRPT' 'DATAUTIL'
                                                 'TESTGEN' 'GLRPT'
                                                 'AMLRPT' 'TDSRPT'
                                                 'PAYBATCH' 'SIBATCH'
                                                 'CCBATCH' 'GOLDREVL'
                                                 'APIGW' 'SQLLOAD'
                                                 'SQLRPT' 'LNRESET'
                                                 'CASHRPT' 'NACHBAT'
                                                 'CTSOUT' 'CTSRET'
                                                 'CIBILOUT' 'TDS26AS'
                                                 'REGRPT' 'PROVBAT'
                                                 'COLLBAT' 'FRAUDBAT'
                                                 'TFBATCH' 'AUDEXRPT'
                                                 'STMTBAT' 'SQLINQ'
                                                 'SWEEPBAT' 'MFBATCH'
                                                 'INVBATCH' 'CMPBATCH'
                                                 'BRGLBAT' 'YREND'.
       01  WS-STEP-RC                  PIC S9(04) COMP VALUE ZERO.
       01  WS-DISP-RC                  PIC 9(04).

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-COMMAND-LINE FROM COMMAND-LINE
           MOVE FUNCTION UPPER-CASE(WS-COMMAND-LINE) TO WS-PROGRAM
           IF NOT PGM-ALLOWED
               DISPLAY 'RUNPGM: PROGRAM "' FUNCTION TRIM(WS-PROGRAM)
                       '" IS NOT A BATCH PROGRAM.'
               DISPLAY 'ALLOWED: INTCALC EODBATCH CHGBATCH CLRBATCH '
                       'MISRPT DATAUTIL TESTGEN GLRPT AMLRPT TDSRPT '
                       'PAYBATCH SIBATCH CCBATCH GOLDREVL APIGW '
                       'SQLLOAD SQLRPT LNRESET CASHRPT NACHBAT '
                       'CTSOUT CTSRET CIBILOUT TDS26AS REGRPT '
                       'PROVBAT COLLBAT FRAUDBAT TFBATCH AUDEXRPT '
                       'STMTBAT SQLINQ SWEEPBAT MFBATCH INVBATCH '
                       'CMPBATCH BRGLBAT YREND'
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF

           MOVE 'BATCH'         TO SES-USER-ID
           MOVE 'BATCH SCHEDULER' TO SES-USER-NAME
           SET SES-BATCH        TO TRUE
           MOVE 'HO0001'        TO SES-BRANCH
           MOVE 'Y'             TO SES-SIGNED-ON

           MOVE 'RUNPGM'    TO AU-PROGRAM
           MOVE 'JOB-START' TO AU-ACTION
           MOVE WS-PROGRAM  TO AU-KEY
           MOVE SPACES      TO AU-DETAIL
           CALL 'AUDITLOG' USING AUD-PARMS

           DISPLAY 'RUNPGM: STEP ' FUNCTION TRIM(WS-PROGRAM)
                   ' STARTED'
           MOVE ZERO TO RETURN-CODE
           IF WS-PROGRAM = 'INTCALC'
               CALL 'INTCALC' USING WS-STEP-RC
           ELSE
               CALL WS-PROGRAM
                   ON EXCEPTION
                       DISPLAY 'RUNPGM: MODULE NOT FOUND'
                       MOVE 16 TO RETURN-CODE
               END-CALL
               MOVE RETURN-CODE TO WS-STEP-RC
           END-IF

           MOVE WS-STEP-RC TO WS-DISP-RC
           MOVE 'JOB-END' TO AU-ACTION
           STRING 'RC=' WS-DISP-RC DELIMITED BY SIZE INTO AU-DETAIL
           END-STRING
           CALL 'AUDITLOG' USING AUD-PARMS
           DISPLAY 'RUNPGM: STEP ' FUNCTION TRIM(WS-PROGRAM)
                   ' ENDED RC=' WS-DISP-RC
           MOVE WS-STEP-RC TO RETURN-CODE
           STOP RUN.
