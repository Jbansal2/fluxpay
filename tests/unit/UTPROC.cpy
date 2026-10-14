      *================================================================*
      * COPYBOOK : UTPROC                                              *
      * PURPOSE  : ASSERTION PARAGRAPHS FOR THE COBOL UNIT TESTS.      *
      *            COPY AT THE END OF THE PROCEDURE DIVISION. OUTPUT   *
      *            LINES START WITH 'UT PASS' / 'UT FAIL' SO THE       *
      *            RUNNER CAN COUNT THEM; UT-FINISH SETS RC 1 ON ANY   *
      *            FAILURE.                                            *
      *================================================================*
       UT-ASSERT-NUM.
           IF UT-ACTUAL-NUM = UT-EXPECTED-NUM
               ADD 1 TO UT-PASSED
               DISPLAY 'UT PASS ' FUNCTION TRIM(UT-SUITE) ': '
                       FUNCTION TRIM(UT-NAME)
           ELSE
               ADD 1 TO UT-FAILED
               MOVE UT-EXPECTED-NUM TO UT-ED-NUM
               MOVE UT-ACTUAL-NUM   TO UT-ED-NUM2
               DISPLAY 'UT FAIL ' FUNCTION TRIM(UT-SUITE) ': '
                       FUNCTION TRIM(UT-NAME)
               DISPLAY '        EXPECTED ' UT-ED-NUM
               DISPLAY '        ACTUAL   ' UT-ED-NUM2
           END-IF.

       UT-ASSERT-STR.
           IF UT-ACTUAL-STR = UT-EXPECTED-STR
               ADD 1 TO UT-PASSED
               DISPLAY 'UT PASS ' FUNCTION TRIM(UT-SUITE) ': '
                       FUNCTION TRIM(UT-NAME)
           ELSE
               ADD 1 TO UT-FAILED
               DISPLAY 'UT FAIL ' FUNCTION TRIM(UT-SUITE) ': '
                       FUNCTION TRIM(UT-NAME)
               DISPLAY '        EXPECTED "'
                       FUNCTION TRIM(UT-EXPECTED-STR) '"'
               DISPLAY '        ACTUAL   "'
                       FUNCTION TRIM(UT-ACTUAL-STR) '"'
           END-IF.

       UT-FINISH.
           DISPLAY 'UT DONE ' FUNCTION TRIM(UT-SUITE) ': '
                   UT-PASSED ' PASSED, ' UT-FAILED ' FAILED'
           IF UT-FAILED > ZERO
               MOVE 1 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.
