       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTLKUP.
      *================================================================*
      * PROGRAM  : CUSTLKUP                                            *
      * PURPOSE  : FINDS THE PRIMARY HOLDER (ACCTREL SEQ 01) OF AN     *
      *            ACCOUNT AND RETURNS HIS KYC DATA FROM CUSTMAST.     *
      *            THE FILES STAY OPEN BETWEEN CALLS (BATCH PROGRAMS   *
      *            CALL THIS ONCE PER RECORD); CLK-CLOSE = 'Y' CLOSES. *
      * FILES    : ACCTREL, CUSTMAST (INPUT)                           *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HLD-FILE ASSIGN TO 'data/ACCTREL.DAT'
               ORGANIZATION IS INDEXED ACCESS IS RANDOM
               RECORD KEY IS HLD-KEY FILE STATUS IS WS-HLD-FS.
           SELECT CUST-FILE ASSIGN TO 'data/CUSTMAST.DAT'
               ORGANIZATION IS INDEXED ACCESS IS RANDOM
               RECORD KEY IS CUST-ID FILE STATUS IS WS-CUST-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  HLD-FILE.
           COPY HLDREC.
       FD  CUST-FILE.
           COPY CUSTREC.

       WORKING-STORAGE SECTION.
       01  WS-HLD-FS                   PIC X(02).
       01  WS-CUST-FS                  PIC X(02).
       01  WS-OPEN-SW                  PIC X(01) VALUE 'N'.
           88  FILES-OPEN                        VALUE 'Y'.
       01  WS-HLD-AVAIL                PIC X(01) VALUE 'N'.
       01  WS-CUST-AVAIL               PIC X(01) VALUE 'N'.

       LINKAGE SECTION.
           COPY CLKPARM.

       PROCEDURE DIVISION USING CLK-PARMS.
       0000-MAIN.
           IF CLK-CLOSE = 'Y'
               IF FILES-OPEN
                   IF WS-HLD-AVAIL = 'Y'
                       CLOSE HLD-FILE
                   END-IF
                   IF WS-CUST-AVAIL = 'Y'
                       CLOSE CUST-FILE
                   END-IF
                   MOVE 'N' TO WS-OPEN-SW WS-HLD-AVAIL WS-CUST-AVAIL
               END-IF
               GOBACK
           END-IF
           IF NOT FILES-OPEN
               OPEN INPUT HLD-FILE
               IF WS-HLD-FS = '00'
                   MOVE 'Y' TO WS-HLD-AVAIL
               END-IF
               OPEN INPUT CUST-FILE
               IF WS-CUST-FS = '00'
                   MOVE 'Y' TO WS-CUST-AVAIL
               END-IF
               MOVE 'Y' TO WS-OPEN-SW
           END-IF
           MOVE ZERO   TO CLK-CUST-ID CLK-DOB CLK-PINCODE
           MOVE SPACES TO CLK-NAME CLK-PAN CLK-GENDER CLK-KYC-STATUS
                       CLK-EMAIL
           SET CLK-NO-HOLDER TO TRUE
           IF WS-HLD-AVAIL NOT = 'Y'
               GOBACK
           END-IF
           MOVE CLK-ACCT-NO TO HLD-ACCT-NO
           MOVE 01          TO HLD-SEQ
           READ HLD-FILE
               INVALID KEY GOBACK
           END-READ
           MOVE HLD-CUST-ID TO CLK-CUST-ID CUST-ID
           SET CLK-NO-CUSTOMER TO TRUE
           IF WS-CUST-AVAIL NOT = 'Y'
               GOBACK
           END-IF
           READ CUST-FILE
               INVALID KEY GOBACK
           END-READ
           MOVE CUST-NAME    TO CLK-NAME
           MOVE CUST-PAN     TO CLK-PAN
           MOVE CUST-DOB     TO CLK-DOB
           MOVE CUST-GENDER  TO CLK-GENDER
           MOVE CUST-PINCODE TO CLK-PINCODE
           MOVE CUST-KYC-STATUS TO CLK-KYC-STATUS
           MOVE CUST-EMAIL   TO CLK-EMAIL
           SET CLK-OK TO TRUE
           GOBACK.
