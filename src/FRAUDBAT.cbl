       IDENTIFICATION DIVISION.
       PROGRAM-ID. FRAUDBAT.
      *================================================================*
      * PROGRAM  : FRAUDBAT                                            *
      * PURPOSE  : FRAUD MONITORING - SCANS ONE DAY OF THE TRANSACTION *
      *            LOG AGAINST BEHAVIOUR RULES AND RAISES ALERTS FOR   *
      *            REVIEW (FRAUDMNT). RULES:                           *
      *              VL VELOCITY     MORE THAN 5 CUSTOMER DEBITS ON    *
      *                              ONE ACCOUNT IN THE DAY            *
      *              OH ODD HOURS    A DEBIT OF 50,000 OR MORE BETWEEN *
      *                              23:00 AND 05:00                   *
      *              NP NEW PAYEE    FIRST-EVER TRANSFER TO AN ACCOUNT *
      *                              OF 1,00,000 OR MORE               *
      *              DW DORMANT WAKE FIRST DEBIT AFTER 12+ MONTHS OF   *
      *                              NO ACTIVITY                       *
      *              PT PASS-THROUGH A CREDIT OF 1,00,000+ AND DEBITS  *
      *                              OF AT LEAST 90% OF IT THE SAME DAY*
      *                              (TYPICAL OF A MULE ACCOUNT)       *
      *            "CUSTOMER DEBIT" = WD TO AW PO CQ ND CP; BANK       *
      *            CHARGES AND INTEREST ARE NOT BEHAVIOUR.             *
      *            HISTORY BEFORE THE RUN DATE IS READ FROM THE SAME   *
      *            LOG (LAST ACTIVITY DATE, PAYEES ALREADY USED).      *
      * INPUT    : RUN DATE YYYYMMDD (BLANK = TODAY)                   *
      * FILES    : TRANLOG (INPUT), FRAUDALT (I-O)                     *
      * CALLS    : DATEUTIL                                            *
      * RETURN   : 0 = NO NEW ALERTS, 4 = NEW ALERTS RAISED            *
      *----------------------------------------------------------------*
      * MODIFICATION HISTORY                                           *
      * DATE       DESCRIPTION                                         *
      * 2026-10-14 INITIAL VERSION                                     *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRAN-FILE ASSIGN TO 'data/TRANLOG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRAN-FS.
           SELECT ALERT-FILE ASSIGN TO 'data/FRAUDALT.DAT'
               ORGANIZATION IS INDEXED ACCESS IS RANDOM
               RECORD KEY IS FA-KEY FILE STATUS IS WS-ALERT-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRAN-FILE.
           COPY TRANREC.
       FD  ALERT-FILE.
           COPY FRAUDREC.

       WORKING-STORAGE SECTION.
       01  WS-TRAN-FS                  PIC X(02).
       01  WS-ALERT-FS                 PIC X(02).
       01  WS-IN-DATE                  PIC X(08).
       01  WS-RUN-DATE                 PIC 9(08).
       01  WS-EOF-SW                   PIC X(01) VALUE 'N'.
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-DEBIT-SW                 PIC X(01).
           88  CUSTOMER-DEBIT                    VALUE 'Y'.
      * THRESHOLDS
       01  WS-VELOCITY-MAX             PIC 9(03) VALUE 5.
       01  WS-ODD-HOURS-AMT            PIC 9(11)V99 VALUE 50000.
       01  WS-NEW-PAYEE-AMT            PIC 9(11)V99 VALUE 100000.
       01  WS-PASS-CREDIT-AMT          PIC 9(11)V99 VALUE 100000.
       01  WS-PASS-PCT                 PIC 9V99 VALUE 0.90.
       01  WS-DORMANT-DAYS             PIC 9(05) VALUE 365.
      * PER-ACCOUNT STATE - OPEN-ADDRESSING HASH TABLE (8191 SLOTS,
      * A PRIME), SO A LOOKUP IS ONE OR TWO PROBES INSTEAD OF A SCAN.
      * A LINEAR TABLE TOOK 10.8 S FOR 50,000 ENTRIES ON 5,000
      * ACCOUNTS; THE HASH TAKES WELL UNDER A SECOND (DOCS/PERFORMANCE)
       01  WS-ACCT-SLOTS               PIC 9(05) VALUE 8191.
       01  WS-ACCT-COUNT               PIC 9(05) VALUE ZERO.
       01  WS-ACCT-TABLE.
           05  WS-ACCT OCCURS 8191 TIMES.
               10  AT-ACCT-NO          PIC 9(10).
               10  AT-LAST-DATE        PIC 9(08).
               10  AT-DEBITS-TODAY     PIC 9(05).
               10  AT-DEBIT-AMT        PIC 9(13)V99.
               10  AT-BIG-CREDIT       PIC 9(13)V99.
               10  AT-WAKE-CHECKED     PIC X(01).
       01  WS-A                        PIC 9(05).
      * PAYEES ALREADY USED: ACCOUNT + PAYEE, HASHED THE SAME WAY
       01  WS-PAIR-SLOTS               PIC 9(05) VALUE 16381.
       01  WS-PAIR-COUNT               PIC 9(05) VALUE ZERO.
       01  WS-PAIR-TABLE.
           05  WS-PAIR OCCURS 16381 TIMES.
               10  PR-ACCT-NO          PIC 9(10).
               10  PR-PAYEE            PIC 9(10).
       01  WS-P                        PIC 9(05).
       01  WS-PROBES                   PIC 9(05).
       01  WS-FOUND-SW                 PIC X(01).
           88  FOUND                             VALUE 'Y'.
       01  WS-COUNTS.
           05  WS-READ                 PIC 9(07) VALUE ZERO.
           05  WS-TODAY-TXNS           PIC 9(07) VALUE ZERO.
           05  WS-NEW-ALERTS           PIC 9(05) VALUE ZERO.
           05  WS-DUPLICATES           PIC 9(05) VALUE ZERO.
       01  WS-NEW-ALERT                PIC X(02).
       01  WS-NEW-AMOUNT               PIC 9(11)V99.
       01  WS-NEW-DETAIL               PIC X(50).
       01  WS-NEW-ACCT                 PIC 9(10).
       01  ED-AMT                      PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  ED-CNT                      PIC ZZ,ZZ9.
           COPY DATEPARM.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'RUN DATE (YYYYMMDD, BLANK = TODAY): '
                   WITH NO ADVANCING
           ACCEPT WS-IN-DATE
           IF WS-IN-DATE = SPACES OR WS-IN-DATE NOT NUMERIC
               ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           ELSE
               MOVE WS-IN-DATE TO WS-RUN-DATE
           END-IF
           INITIALIZE WS-ACCT-TABLE WS-PAIR-TABLE
           OPEN INPUT TRAN-FILE
           IF WS-TRAN-FS NOT = '00'
               DISPLAY 'FRAUDBAT: NO TRANSACTION LOG - NOTHING TO SCAN'
               GOBACK
           END-IF
           OPEN I-O ALERT-FILE
           IF WS-ALERT-FS = '35'
               OPEN OUTPUT ALERT-FILE
               CLOSE ALERT-FILE
               OPEN I-O ALERT-FILE
           END-IF
           PERFORM UNTIL END-OF-FILE
               READ TRAN-FILE
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-READ
                       PERFORM 1000-ONE-TRANSACTION
               END-READ
           END-PERFORM
           CLOSE TRAN-FILE
           PERFORM 3000-END-OF-DAY-RULES
           CLOSE ALERT-FILE
           PERFORM 9000-SUMMARY
           IF WS-NEW-ALERTS > ZERO
               MOVE 4 TO RETURN-CODE
           END-IF
           GOBACK.

       1000-ONE-TRANSACTION.
           IF TRAN-DATE > WS-RUN-DATE
               EXIT PARAGRAPH
           END-IF
           EVALUATE TRAN-TYPE
               WHEN 'WD' WHEN 'TO' WHEN 'AW' WHEN 'PO'
               WHEN 'CQ' WHEN 'ND' WHEN 'CP'
                   MOVE 'Y' TO WS-DEBIT-SW
               WHEN OTHER
                   MOVE 'N' TO WS-DEBIT-SW
           END-EVALUATE
           PERFORM 7000-FIND-ACCOUNT
           IF TRAN-DATE < WS-RUN-DATE
      *        HISTORY: REMEMBER ACTIVITY AND PAYEES
               IF TRAN-INTEREST OR TRAN-CHARGES OR TRAN-OD-INTEREST
                   CONTINUE
               ELSE
                   MOVE TRAN-DATE TO AT-LAST-DATE(WS-A)
               END-IF
               IF TRAN-XFER-OUT AND TRAN-REF-ACCT NOT = ZERO
                   PERFORM 7100-FIND-PAIR
                   IF NOT FOUND
                       PERFORM 7200-ADD-PAIR
                   END-IF
               END-IF
               EXIT PARAGRAPH
           END-IF
      *    THE RUN DATE ITSELF
           ADD 1 TO WS-TODAY-TXNS
           IF CUSTOMER-DEBIT
               ADD 1           TO AT-DEBITS-TODAY(WS-A)
               ADD TRAN-AMOUNT TO AT-DEBIT-AMT(WS-A)
               PERFORM 2000-ODD-HOURS
               PERFORM 2100-DORMANT-WAKE
               IF TRAN-XFER-OUT
                   PERFORM 2200-NEW-PAYEE
               END-IF
           ELSE
               IF TRAN-CREDIT-TYPE AND TRAN-AMOUNT >= WS-PASS-CREDIT-AMT
                   ADD TRAN-AMOUNT TO AT-BIG-CREDIT(WS-A)
               END-IF
           END-IF
           IF NOT (TRAN-INTEREST OR TRAN-CHARGES OR TRAN-OD-INTEREST)
               MOVE TRAN-DATE TO AT-LAST-DATE(WS-A)
           END-IF.

      *================================================================*
      * RULES CHECKED TRANSACTION BY TRANSACTION                       *
      *================================================================*
       2000-ODD-HOURS.
           IF TRAN-AMOUNT >= WS-ODD-HOURS-AMT
              AND (TRAN-TIME >= 230000 OR TRAN-TIME < 050000)
               MOVE 'OH' TO WS-NEW-ALERT
               MOVE TRAN-AMOUNT TO WS-NEW-AMOUNT
               MOVE SPACES TO WS-NEW-DETAIL
               STRING TRAN-TYPE ' AT ' TRAN-TIME(1:2) ':'
                      TRAN-TIME(3:2) ' - ' TRAN-NARRATION
                   DELIMITED BY SIZE INTO WS-NEW-DETAIL
               PERFORM 8000-RAISE
           END-IF.

      *    ONLY THE FIRST DEBIT OF THE DAY IS COMPARED WITH THE LAST
      *    ACTIVITY BEFORE TODAY
       2100-DORMANT-WAKE.
           IF AT-WAKE-CHECKED(WS-A) = 'Y'
               EXIT PARAGRAPH
           END-IF
           MOVE 'Y' TO AT-WAKE-CHECKED(WS-A)
           IF AT-LAST-DATE(WS-A) = ZERO
               EXIT PARAGRAPH
           END-IF
           SET DU-DAYS-BETWEEN TO TRUE
           MOVE AT-LAST-DATE(WS-A) TO DU-DATE-1
           MOVE WS-RUN-DATE        TO DU-DATE-2
           CALL 'DATEUTIL' USING DATE-PARMS
           IF DU-OK AND DU-RESULT-DAYS > WS-DORMANT-DAYS
               MOVE 'DW' TO WS-NEW-ALERT
               MOVE TRAN-AMOUNT TO WS-NEW-AMOUNT
               MOVE SPACES TO WS-NEW-DETAIL
               STRING 'FIRST DEBIT SINCE ' AT-LAST-DATE(WS-A)
                   DELIMITED BY SIZE INTO WS-NEW-DETAIL
               PERFORM 8000-RAISE
           END-IF.

       2200-NEW-PAYEE.
           IF TRAN-REF-ACCT = ZERO
               EXIT PARAGRAPH
           END-IF
           PERFORM 7100-FIND-PAIR
           IF NOT FOUND
               IF TRAN-AMOUNT >= WS-NEW-PAYEE-AMT
                   MOVE 'NP' TO WS-NEW-ALERT
                   MOVE TRAN-AMOUNT TO WS-NEW-AMOUNT
                   MOVE SPACES TO WS-NEW-DETAIL
                   STRING 'FIRST TRANSFER TO ' TRAN-REF-ACCT
                       DELIMITED BY SIZE INTO WS-NEW-DETAIL
                   PERFORM 8000-RAISE
               END-IF
      *        A PAYEE IS "KNOWN" ONCE USED, EVEN LATER THE SAME DAY
               PERFORM 7200-ADD-PAIR
           END-IF.

      *================================================================*
      * RULES ON THE DAY'S TOTALS                                      *
      *================================================================*
       3000-END-OF-DAY-RULES.
           PERFORM VARYING WS-A FROM 1 BY 1 UNTIL WS-A > WS-ACCT-SLOTS
             IF AT-ACCT-NO(WS-A) NOT = ZERO
               MOVE AT-ACCT-NO(WS-A) TO WS-NEW-ACCT
               IF AT-DEBITS-TODAY(WS-A) > WS-VELOCITY-MAX
                   MOVE 'VL' TO WS-NEW-ALERT
                   MOVE AT-DEBIT-AMT(WS-A) TO WS-NEW-AMOUNT
                   MOVE SPACES TO WS-NEW-DETAIL
                   STRING AT-DEBITS-TODAY(WS-A) ' DEBITS IN ONE DAY'
                       DELIMITED BY SIZE INTO WS-NEW-DETAIL
                   PERFORM 8100-RAISE-FOR
               END-IF
               IF AT-BIG-CREDIT(WS-A) > ZERO
                  AND AT-DEBIT-AMT(WS-A) >=
                      AT-BIG-CREDIT(WS-A) * WS-PASS-PCT
                   MOVE 'PT' TO WS-NEW-ALERT
                   MOVE AT-BIG-CREDIT(WS-A) TO WS-NEW-AMOUNT
                   MOVE AT-DEBIT-AMT(WS-A)  TO ED-AMT
                   MOVE SPACES TO WS-NEW-DETAIL
                   STRING 'CREDIT MOVED OUT SAME DAY, DEBITS '
                          ED-AMT DELIMITED BY SIZE INTO WS-NEW-DETAIL
                   PERFORM 8100-RAISE-FOR
               END-IF
             END-IF
           END-PERFORM.

      *================================================================*
      * HELPERS                                                        *
      *================================================================*
      *    WS-A -> THE ACCOUNT'S SLOT (CREATED EMPTY IF NEW).
      *    START AT ACCOUNT MOD SLOTS, STEP ON TO THE NEXT FREE SLOT.
       7000-FIND-ACCOUNT.
           COMPUTE WS-A = FUNCTION MOD(TRAN-ACCT-NO, WS-ACCT-SLOTS) + 1
           MOVE ZERO TO WS-PROBES
           PERFORM UNTIL AT-ACCT-NO(WS-A) = TRAN-ACCT-NO
                      OR AT-ACCT-NO(WS-A) = ZERO
                      OR WS-PROBES >= WS-ACCT-SLOTS
               ADD 1 TO WS-A WS-PROBES
               IF WS-A > WS-ACCT-SLOTS
                   MOVE 1 TO WS-A
               END-IF
           END-PERFORM
           IF AT-ACCT-NO(WS-A) = ZERO
               ADD 1 TO WS-ACCT-COUNT
               INITIALIZE WS-ACCT(WS-A)
               MOVE TRAN-ACCT-NO TO AT-ACCT-NO(WS-A)
               MOVE 'N' TO AT-WAKE-CHECKED(WS-A)
           END-IF.

      *    WS-P -> SLOT OF (ACCOUNT, PAYEE); FOUND IF ALREADY THERE
       7100-FIND-PAIR.
           MOVE 'N' TO WS-FOUND-SW
           COMPUTE WS-P = FUNCTION MOD(
                   FUNCTION MOD(TRAN-ACCT-NO, WS-PAIR-SLOTS) * 31
                   + TRAN-REF-ACCT, WS-PAIR-SLOTS) + 1
           MOVE ZERO TO WS-PROBES
           PERFORM UNTIL PR-ACCT-NO(WS-P) = ZERO OR FOUND
                      OR WS-PROBES >= WS-PAIR-SLOTS
               IF PR-ACCT-NO(WS-P) = TRAN-ACCT-NO
                  AND PR-PAYEE(WS-P) = TRAN-REF-ACCT
                   SET FOUND TO TRUE
               ELSE
                   ADD 1 TO WS-P WS-PROBES
                   IF WS-P > WS-PAIR-SLOTS
                       MOVE 1 TO WS-P
                   END-IF
               END-IF
           END-PERFORM.

      *    CALLED AFTER 7100 HAS LEFT WS-P ON A FREE SLOT
       7200-ADD-PAIR.
           IF PR-ACCT-NO(WS-P) = ZERO
               MOVE TRAN-ACCT-NO  TO PR-ACCT-NO(WS-P)
               MOVE TRAN-REF-ACCT TO PR-PAYEE(WS-P)
               ADD 1 TO WS-PAIR-COUNT
           END-IF.

       8000-RAISE.
           MOVE TRAN-ACCT-NO TO WS-NEW-ACCT
           PERFORM 8100-RAISE-FOR.

      *    ONE ALERT PER DAY, ACCOUNT AND RULE (THE FILE KEY)
       8100-RAISE-FOR.
           INITIALIZE FRAUD-RECORD
           MOVE WS-RUN-DATE   TO FA-DATE
           MOVE WS-NEW-ACCT   TO FA-ACCT-NO
           MOVE WS-NEW-ALERT  TO FA-RULE
           MOVE WS-NEW-AMOUNT TO FA-AMOUNT
           MOVE WS-NEW-DETAIL TO FA-DETAIL
           SET FA-OPEN        TO TRUE
           WRITE FRAUD-RECORD
               INVALID KEY
                   ADD 1 TO WS-DUPLICATES
               NOT INVALID KEY
                   ADD 1 TO WS-NEW-ALERTS
                   MOVE WS-NEW-AMOUNT TO ED-AMT
                   DISPLAY '  ALERT ' WS-NEW-ALERT ' ' WS-NEW-ACCT ' '
                           ED-AMT ' ' WS-NEW-DETAIL
           END-WRITE.

       9000-SUMMARY.
           DISPLAY ' '
           DISPLAY 'FRAUD SCAN FOR ' WS-RUN-DATE
           MOVE WS-READ TO ED-CNT
           DISPLAY 'LOG RECORDS READ    : ' ED-CNT
           MOVE WS-TODAY-TXNS TO ED-CNT
           DISPLAY 'TRANSACTIONS TODAY  : ' ED-CNT
           MOVE WS-NEW-ALERTS TO ED-CNT
           DISPLAY 'NEW ALERTS          : ' ED-CNT
           MOVE WS-DUPLICATES TO ED-CNT
           DISPLAY 'ALREADY RAISED      : ' ED-CNT.
