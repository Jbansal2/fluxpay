      *================================================================*
      * COPYBOOK : UTWS                                                *
      * PURPOSE  : WORKING STORAGE FOR THE COBOL UNIT TEST HARNESS.    *
      *            A TEST SETS UT-NAME, MOVES VALUES TO UT-EXPECTED-*  *
      *            AND UT-ACTUAL-* AND PERFORMS AN UT-ASSERT PARAGRAPH *
      *            FROM UTPROC.                                        *
      *================================================================*
       01  UT-STATE.
           05  UT-SUITE                PIC X(20).
           05  UT-NAME                 PIC X(50).
           05  UT-PASSED               PIC 9(04) VALUE ZERO.
           05  UT-FAILED               PIC 9(04) VALUE ZERO.
           05  UT-EXPECTED-NUM         PIC S9(13)V9(4).
           05  UT-ACTUAL-NUM           PIC S9(13)V9(4).
           05  UT-EXPECTED-STR         PIC X(40).
           05  UT-ACTUAL-STR           PIC X(40).
           05  UT-ED-NUM               PIC -Z(12)9.9999.
           05  UT-ED-NUM2              PIC -Z(12)9.9999.
