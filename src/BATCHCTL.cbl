       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCHCTL.
      *================================================================*
      * PROGRAM  : BATCHCTL                                            *
      * PURPOSE  : SWITCHES THE POSTING CHAIN INTO / OUT OF BATCH      *
      *            "KEEP FILES OPEN" MODE. A BATCH THAT POSTS          *
      *            THOUSANDS OF ENTRIES CALLS 'S' BEFORE ITS LOOP AND  *
      *            'E' AFTER IT; IN BETWEEN ACCTPOST, GLPOST, AUDITLOG *
      *            AND AMLCHK OPEN THEIR FILES ONCE INSTEAD OF ON      *
      *            EVERY CALL.                                         *
      * CALL     : CALL 'BATCHCTL' USING BC-FUNCTION (PIC X)           *
      *              'S' START, 'E' END (FLUSH AND CLOSE EVERYTHING)   *
      * RULE     : WHILE IN BATCH MODE THE CALLER MUST NOT OPEN        *
      *            ACCTMAST, GLMAST, TRANLOG, GLJRNL OR AUDITLOG       *
      *            ITSELF.                                             *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY BATMODE.
           COPY POSTPARM.
           COPY GLPARM.
           COPY AUDPARM.
           COPY AMLPARM.

       LINKAGE SECTION.
       01  BC-FUNCTION                 PIC X(01).

       PROCEDURE DIVISION USING BC-FUNCTION.
       0000-MAIN.
           EVALUATE BC-FUNCTION
               WHEN 'S'
                   SET CB-KEEP-OPEN TO TRUE
               WHEN 'E'
                   IF CB-KEEP-OPEN
                       SET CB-CLOSE-ALL TO TRUE
                       CALL 'ACCTPOST' USING POST-PARMS
                       CALL 'GLPOST'   USING GL-PARMS
                       CALL 'AUDITLOG' USING AUD-PARMS
                       CALL 'AMLCHK'   USING AML-PARMS
                   END-IF
                   MOVE 'N' TO CB-BATCH-MODE
           END-EVALUATE
           GOBACK.
