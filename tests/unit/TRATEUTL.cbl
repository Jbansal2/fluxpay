       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRATEUTL.
      *================================================================*
      * UNIT TESTS FOR RATEUTIL (RUN IN AN EMPTY DATA DIRECTORY, SO    *
      * EVERY LOOKUP FALLS BACK TO THE BUILT-IN TABLE)                 *
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY RATEPARM.
           COPY UTWS.

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE 'RATEUTIL' TO UT-SUITE

           MOVE 'REPO DEFAULT IS 6.50' TO UT-NAME
           MOVE 'REPO' TO RTP-CODE
           CALL 'RATEUTIL' USING RATE-PARMS
           MOVE 6.5 TO UT-EXPECTED-NUM
           MOVE RTP-VALUE TO UT-ACTUAL-NUM
           PERFORM UT-ASSERT-NUM

           MOVE 'NO MASTER -> RC 04 (DEFAULT USED)' TO UT-NAME
           MOVE 4 TO UT-EXPECTED-NUM
           MOVE RTP-RC TO UT-ACTUAL-NUM
           PERFORM UT-ASSERT-NUM

           MOVE 'SLR DEFAULT IS 18.00' TO UT-NAME
           MOVE 'SLR' TO RTP-CODE
           CALL 'RATEUTIL' USING RATE-PARMS
           MOVE 18 TO UT-EXPECTED-NUM
           MOVE RTP-VALUE TO UT-ACTUAL-NUM
           PERFORM UT-ASSERT-NUM

           MOVE 'UNKNOWN CODE -> RC 08, VALUE ZERO' TO UT-NAME
           MOVE 'NOSUCH' TO RTP-CODE
           CALL 'RATEUTIL' USING RATE-PARMS
           COMPUTE UT-ACTUAL-NUM = RTP-RC * 100 + RTP-VALUE
           MOVE 800 TO UT-EXPECTED-NUM
           PERFORM UT-ASSERT-NUM

           PERFORM UT-FINISH
           STOP RUN.

           COPY UTPROC.
