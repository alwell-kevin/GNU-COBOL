       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING-CONSOLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE
               ASSIGN TO DYNAMIC WS-ACCOUNT-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACCOUNT-STATUS.

           SELECT LEDGER-FILE
               ASSIGN TO DYNAMIC WS-LEDGER-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LEDGER-STATUS.

           SELECT ALERT-FILE
               ASSIGN TO DYNAMIC WS-ALERT-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ALERT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-LINE                   PIC X(256).

       FD  LEDGER-FILE.
       01  LEDGER-LINE                    PIC X(256).

       FD  ALERT-FILE.
       01  ALERT-LINE                     PIC X(256).

       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-TABLE.
           05 WS-ACCOUNT-ENTRY OCCURS 200 TIMES.
               10 WS-TBL-EMP-ID           PIC 9(4) VALUE 0.
               10 WS-TBL-CHECKING         PIC S9(7)V99 VALUE 0.
               10 WS-TBL-SAVINGS          PIC S9(7)V99 VALUE 0.
               10 WS-TBL-LOAN             PIC S9(7)V99 VALUE 0.

       01  WS-AUDIT-TABLE.
           05 WS-AUDIT-ENTRY OCCURS 300 TIMES.
               10 WS-AUDIT-ID             PIC 9(9) VALUE 0.
               10 WS-AUDIT-DEBIT          PIC 9(9)V99 VALUE 0.
               10 WS-AUDIT-CREDIT         PIC 9(9)V99 VALUE 0.

       01  WS-RECENT-BUFFER.
           05 WS-RECENT-LINE OCCURS 8 TIMES PIC X(256).
       01  WS-ALERT-BUFFER.
           05 WS-ALERT-BUF-LINE OCCURS 300 TIMES PIC X(256).

       77  WS-ACCOUNT-PATH                PIC X(256)
                                           VALUE "data/accounts.dat".
       77  WS-LEDGER-PATH                 PIC X(256)
                                           VALUE "data/ledger.dat".
       77  WS-ALERT-PATH                  PIC X(256)
                                           VALUE "data/alerts.dat".

       77  WS-ACCOUNT-STATUS              PIC XX VALUE SPACES.
       77  WS-LEDGER-STATUS               PIC XX VALUE SPACES.
       77  WS-ALERT-STATUS                PIC XX VALUE SPACES.

       77  WS-BANKING-EXIT                PIC X VALUE "N".
       77  WS-MENU-OPTION                 PIC X VALUE SPACE.
       77  WS-EOF                         PIC X VALUE "N".
       77  WS-OP-OK                       PIC X VALUE "Y".

       77  WS-ACCOUNT-COUNT               PIC 9(4) VALUE 0.
       77  WS-ROW-IDX                     PIC 9(4) VALUE 0.
       77  WS-USER-IDX                    PIC 9(4) VALUE 0.
       77  WS-PARSE-OK                    PIC X VALUE "Y".
       77  WS-FIELD-COUNT                 PIC 9 VALUE 0.

       77  WS-ID-TEXT                     PIC X(20).
       77  WS-CHECKING-TEXT               PIC X(20).
       77  WS-SAVINGS-TEXT                PIC X(20).
       77  WS-LOAN-TEXT                   PIC X(20).

       77  WS-AMOUNT-IN                   PIC X(20) VALUE SPACES.
       77  WS-TRANSFER-AMOUNT             PIC 9(7)V99 VALUE 0.
       77  WS-POST-AMOUNT                 PIC 9(7)V99 VALUE 0.
       77  WS-INTEREST-AMOUNT             PIC 9(7)V99 VALUE 0.
       77  WS-FEE-AMOUNT                  PIC 9(7)V99 VALUE 0.
       77  WS-NET-POSITION                PIC S9(7)V99 VALUE 0.

       77  WS-OLD-CHECKING                PIC S9(7)V99 VALUE 0.
       77  WS-OLD-SAVINGS                 PIC S9(7)V99 VALUE 0.

       77  WS-HIGH-VALUE-THRESHOLD        PIC 9(7)V99 VALUE 3000.00.
       77  WS-OVERDRAFT-FEE               PIC 9(3)V99 VALUE 35.00.
       77  WS-DAILY-INTEREST-RATE         PIC 9V9999 VALUE 0.0010.

       77  WS-TX-ID                       PIC 9(9) VALUE 0.
       77  WS-MAX-TX-ID                   PIC 9(9) VALUE 0.

       77  WS-CURRENT-DATE                PIC X(21) VALUE SPACES.
       77  WS-TIMESTAMP                   PIC X(19) VALUE SPACES.

       77  WS-LINE-TX-ID-TEXT             PIC X(20).
       77  WS-LINE-TS-TEXT                PIC X(30).
       77  WS-LINE-EMP-ID-TEXT            PIC X(20).
       77  WS-LINE-ENTRY-TYPE             PIC X(20).
       77  WS-LINE-ACCOUNT                PIC X(30).
       77  WS-LINE-AMOUNT-TEXT            PIC X(20).
       77  WS-LINE-MEMO                   PIC X(80).

       77  WS-PARSED-TX-ID                PIC 9(9) VALUE 0.
       77  WS-PARSED-EMP-ID               PIC 9(4) VALUE 0.
       77  WS-PARSED-AMOUNT               PIC 9(7)V99 VALUE 0.

       77  WS-RECENT-COUNT                PIC 99 VALUE 0.
       77  WS-RECENT-IDX                  PIC 99 VALUE 0.
       77  WS-ALERT-COUNT                 PIC 9(4) VALUE 0.
       77  WS-ALERT-IDX                   PIC 9(4) VALUE 0.

       77  WS-AUDIT-COUNT                 PIC 9(4) VALUE 0.
       77  WS-AUDIT-IDX                   PIC 9(4) VALUE 0.
       77  WS-AUDIT-FOUND-IDX             PIC 9(4) VALUE 0.
       77  WS-TX-COUNT                    PIC 9(4) VALUE 0.
       77  WS-IMBALANCED-COUNT            PIC 9(4) VALUE 0.

       77  WS-DEBIT-ACCOUNT               PIC X(20) VALUE SPACES.
       77  WS-CREDIT-ACCOUNT              PIC X(20) VALUE SPACES.
       77  WS-POST-MEMO                   PIC X(40) VALUE SPACES.

       77  WS-ENTRY-TYPE                  PIC X(10) VALUE SPACES.
       77  WS-ENTRY-ACCOUNT               PIC X(20) VALUE SPACES.
       77  WS-ENTRY-MEMO                  PIC X(40) VALUE SPACES.

       77  WS-DUMMY                       PIC X VALUE SPACE.
       77  WS-NOISE                       PIC X VALUE SPACE.
       77  WS-ALERT-WRITTEN               PIC X VALUE "N".

       77  WS-ID-OUT-TEXT                 PIC X(12).
       77  WS-TX-ID-TEXT                  PIC X(12).
       77  WS-EMP-ID-TEXT                 PIC X(12).
       77  WS-AMOUNT-EDIT                 PIC -9999999.99.
       77  WS-AMOUNT-TEXT                 PIC X(20).
       77  WS-CHECKING-OUT-TEXT           PIC X(20).
       77  WS-SAVINGS-OUT-TEXT            PIC X(20).
       77  WS-LOAN-OUT-TEXT               PIC X(20).

       77  WS-AMOUNT-DISPLAY              PIC -ZZ,ZZZ,ZZ9.99.

       77  ANSI-RESET                     PIC X(4) VALUE X"1B5B306D".
       77  ANSI-BASE                      PIC X(8) VALUE X"1B5B34303B39376D".
       77  ANSI-KEYWORD                   PIC X(7) VALUE X"1B5B39363B316D".
       77  ANSI-IDENT                     PIC X(8) VALUE X"1B5B33373B316D".
       77  ANSI-COMMENT                   PIC X(5) VALUE X"1B5B33326D".
       77  ANSI-LITERAL                   PIC X(5) VALUE X"1B5B39336D".
       77  ANSI-NUMBER                    PIC X(5) VALUE X"1B5B39356D".
       77  ANSI-OPERATOR                  PIC X(5) VALUE X"1B5B39346D".
       77  ANSI-INPUT                     PIC X(5) VALUE X"1B5B39326D".
       77  ANSI-HOME                      PIC X(3) VALUE X"1B5B48".
       77  ANSI-CLEAR                     PIC X(4) VALUE X"1B5B324A".

       LINKAGE SECTION.
       01  LK-EMPLOYEE-RECORD.
           COPY "employee_record.cpy".

       PROCEDURE DIVISION USING LK-EMPLOYEE-RECORD.
       MAIN-PROCEDURE.
           PERFORM LOAD-ACCOUNT-TABLE
           IF WS-OP-OK NOT = "Y"
               PERFORM CLEAR-SCREEN
               DISPLAY ANSI-LITERAL WITH NO ADVANCING
               DISPLAY "ERROR: Cannot load data/accounts.dat"
               DISPLAY ANSI-BASE WITH NO ADVANCING
               PERFORM WAIT-FOR-ENTER
               GOBACK
           END-IF

           PERFORM LOCATE-CURRENT-ACCOUNT
           IF WS-USER-IDX = 0
               PERFORM CLEAR-SCREEN
               DISPLAY ANSI-LITERAL WITH NO ADVANCING
               DISPLAY "ERROR: No bank account row found for employee "
                       EMP-ID OF LK-EMPLOYEE-RECORD
               DISPLAY ANSI-BASE WITH NO ADVANCING
               PERFORM WAIT-FOR-ENTER
               GOBACK
           END-IF

           MOVE "N" TO WS-BANKING-EXIT
           PERFORM UNTIL WS-BANKING-EXIT = "Y"
               PERFORM DISPLAY-BANKING-MENU
               ACCEPT WS-MENU-OPTION
               PERFORM HANDLE-BANKING-OPTION
           END-PERFORM

           GOBACK.

       CLEAR-SCREEN.
           DISPLAY FUNCTION CONCATENATE(ANSI-CLEAR, ANSI-HOME)
               WITH NO ADVANCING.

       DISPLAY-BANKING-MENU.
           PERFORM CLEAR-SCREEN

           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "Helical Community Bank Console"
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY "User: " FUNCTION TRIM(EMP-FULL-NAME OF LK-EMPLOYEE-RECORD)
                   " (" EMP-ID OF LK-EMPLOYEE-RECORD ")"

           DISPLAY ANSI-COMMENT WITH NO ADVANCING
           DISPLAY "----------------------------------------------"

           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY " 01." WITH NO ADVANCING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY " Account summary"

           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY " 02." WITH NO ADVANCING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY " Transfer checking -> savings"

           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY " 03." WITH NO ADVANCING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY " Transfer savings -> checking"

           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY " 04." WITH NO ADVANCING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY " Recent ledger entries"

           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY " 05." WITH NO ADVANCING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY " Run end-of-day batch"

           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY " 06." WITH NO ADVANCING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY " Ledger balance audit"

           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY " 07." WITH NO ADVANCING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY " Return to payroll menu"

           DISPLAY " "
           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "Choose option: " WITH NO ADVANCING
           DISPLAY ANSI-INPUT WITH NO ADVANCING
           DISPLAY "__ " WITH NO ADVANCING
           DISPLAY ANSI-BASE WITH NO ADVANCING.

       HANDLE-BANKING-OPTION.
           EVALUATE WS-MENU-OPTION
               WHEN "1"
                   PERFORM SHOW-ACCOUNT-SUMMARY
               WHEN "2"
                   PERFORM TRANSFER-CHECKING-TO-SAVINGS
               WHEN "3"
                   PERFORM TRANSFER-SAVINGS-TO-CHECKING
               WHEN "4"
                   PERFORM SHOW-RECENT-LEDGER
               WHEN "5"
                   PERFORM RUN-END-OF-DAY-BATCH
               WHEN "6"
                   PERFORM RUN-LEDGER-AUDIT
               WHEN "7"
                   MOVE "Y" TO WS-BANKING-EXIT
               WHEN OTHER
                   DISPLAY ANSI-LITERAL WITH NO ADVANCING
                   DISPLAY "Invalid option. Choose 1-7."
                   DISPLAY ANSI-BASE WITH NO ADVANCING
                   PERFORM WAIT-FOR-ENTER
           END-EVALUATE.

       SHOW-ACCOUNT-SUMMARY.
           PERFORM CLEAR-SCREEN

           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "Account Summary"
           DISPLAY ANSI-COMMENT WITH NO ADVANCING
           DISPLAY "----------------------------------------------"

           MOVE WS-TBL-CHECKING(WS-USER-IDX) TO WS-AMOUNT-DISPLAY
           DISPLAY "Checking Balance : $" WS-AMOUNT-DISPLAY

           MOVE WS-TBL-SAVINGS(WS-USER-IDX) TO WS-AMOUNT-DISPLAY
           DISPLAY "Savings Balance  : $" WS-AMOUNT-DISPLAY

           MOVE WS-TBL-LOAN(WS-USER-IDX) TO WS-AMOUNT-DISPLAY
           DISPLAY "Loan Balance     : $" WS-AMOUNT-DISPLAY

           COMPUTE WS-NET-POSITION =
               WS-TBL-CHECKING(WS-USER-IDX) +
               WS-TBL-SAVINGS(WS-USER-IDX) -
               WS-TBL-LOAN(WS-USER-IDX)

           MOVE WS-NET-POSITION TO WS-AMOUNT-DISPLAY
           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY "Net Position     : $" WS-AMOUNT-DISPLAY
           DISPLAY ANSI-BASE WITH NO ADVANCING

           PERFORM WAIT-FOR-ENTER.

       TRANSFER-CHECKING-TO-SAVINGS.
           PERFORM CLEAR-SCREEN
           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "Transfer: Checking -> Savings"

           MOVE WS-TBL-CHECKING(WS-USER-IDX) TO WS-AMOUNT-DISPLAY
           DISPLAY "Available checking balance: $" WS-AMOUNT-DISPLAY

           PERFORM PROMPT-FOR-TRANSFER-AMOUNT
           IF WS-OP-OK NOT = "Y"
               PERFORM WAIT-FOR-ENTER
               EXIT PARAGRAPH
           END-IF

           IF WS-TRANSFER-AMOUNT > WS-TBL-CHECKING(WS-USER-IDX)
               DISPLAY ANSI-LITERAL WITH NO ADVANCING
               DISPLAY "Insufficient funds in checking."
               DISPLAY ANSI-BASE WITH NO ADVANCING
               PERFORM WAIT-FOR-ENTER
               EXIT PARAGRAPH
           END-IF

           MOVE WS-TBL-CHECKING(WS-USER-IDX) TO WS-OLD-CHECKING
           MOVE WS-TBL-SAVINGS(WS-USER-IDX) TO WS-OLD-SAVINGS

           COMPUTE WS-TBL-CHECKING(WS-USER-IDX) =
               WS-TBL-CHECKING(WS-USER-IDX) - WS-TRANSFER-AMOUNT
           COMPUTE WS-TBL-SAVINGS(WS-USER-IDX) =
               WS-TBL-SAVINGS(WS-USER-IDX) + WS-TRANSFER-AMOUNT

           MOVE "CHECKING" TO WS-DEBIT-ACCOUNT
           MOVE "SAVINGS" TO WS-CREDIT-ACCOUNT
           MOVE "TRANSFER_TO_SAVINGS" TO WS-POST-MEMO
           MOVE WS-TRANSFER-AMOUNT TO WS-POST-AMOUNT

           PERFORM COMMIT-WITH-LEDGER

           IF WS-OP-OK = "Y"
               MOVE "N" TO WS-ALERT-WRITTEN
               IF FUNCTION NUMVAL(FUNCTION TRIM(WS-AMOUNT-IN)) >= 3000
                   PERFORM APPEND-HIGH-VALUE-ALERT
                   IF WS-ALERT-WRITTEN = "Y"
                       DISPLAY ANSI-COMMENT WITH NO ADVANCING
                       DISPLAY "Risk alert recorded for compliance review."
                   END-IF
               END-IF
               DISPLAY ANSI-COMMENT WITH NO ADVANCING
               DISPLAY "Transfer posted and journaled."
               DISPLAY ANSI-BASE WITH NO ADVANCING
           END-IF

           PERFORM WAIT-FOR-ENTER.

       TRANSFER-SAVINGS-TO-CHECKING.
           PERFORM CLEAR-SCREEN
           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "Transfer: Savings -> Checking"

           MOVE WS-TBL-SAVINGS(WS-USER-IDX) TO WS-AMOUNT-DISPLAY
           DISPLAY "Available savings balance: $" WS-AMOUNT-DISPLAY

           PERFORM PROMPT-FOR-TRANSFER-AMOUNT
           IF WS-OP-OK NOT = "Y"
               PERFORM WAIT-FOR-ENTER
               EXIT PARAGRAPH
           END-IF

           IF WS-TRANSFER-AMOUNT > WS-TBL-SAVINGS(WS-USER-IDX)
               DISPLAY ANSI-LITERAL WITH NO ADVANCING
               DISPLAY "Insufficient funds in savings."
               DISPLAY ANSI-BASE WITH NO ADVANCING
               PERFORM WAIT-FOR-ENTER
               EXIT PARAGRAPH
           END-IF

           MOVE WS-TBL-CHECKING(WS-USER-IDX) TO WS-OLD-CHECKING
           MOVE WS-TBL-SAVINGS(WS-USER-IDX) TO WS-OLD-SAVINGS

           COMPUTE WS-TBL-SAVINGS(WS-USER-IDX) =
               WS-TBL-SAVINGS(WS-USER-IDX) - WS-TRANSFER-AMOUNT
           COMPUTE WS-TBL-CHECKING(WS-USER-IDX) =
               WS-TBL-CHECKING(WS-USER-IDX) + WS-TRANSFER-AMOUNT

           MOVE "SAVINGS" TO WS-DEBIT-ACCOUNT
           MOVE "CHECKING" TO WS-CREDIT-ACCOUNT
           MOVE "TRANSFER_TO_CHECKING" TO WS-POST-MEMO
           MOVE WS-TRANSFER-AMOUNT TO WS-POST-AMOUNT

           PERFORM COMMIT-WITH-LEDGER

           IF WS-OP-OK = "Y"
               MOVE "N" TO WS-ALERT-WRITTEN
               IF FUNCTION NUMVAL(FUNCTION TRIM(WS-AMOUNT-IN)) >= 3000
                   PERFORM APPEND-HIGH-VALUE-ALERT
                   IF WS-ALERT-WRITTEN = "Y"
                       DISPLAY ANSI-COMMENT WITH NO ADVANCING
                       DISPLAY "Risk alert recorded for compliance review."
                   END-IF
               END-IF
               DISPLAY ANSI-COMMENT WITH NO ADVANCING
               DISPLAY "Transfer posted and journaled."
               DISPLAY ANSI-BASE WITH NO ADVANCING
           END-IF

           PERFORM WAIT-FOR-ENTER.

       PROMPT-FOR-TRANSFER-AMOUNT.
           MOVE "Y" TO WS-OP-OK
           MOVE SPACES TO WS-AMOUNT-IN

           DISPLAY " "
           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "Transfer amount: " WITH NO ADVANCING
           DISPLAY ANSI-INPUT WITH NO ADVANCING
           DISPLAY "__________ " WITH NO ADVANCING
           DISPLAY ANSI-BASE WITH NO ADVANCING
           ACCEPT WS-AMOUNT-IN

           IF FUNCTION TEST-NUMVAL(FUNCTION TRIM(WS-AMOUNT-IN)) NOT = 0
               MOVE "N" TO WS-OP-OK
               DISPLAY ANSI-LITERAL WITH NO ADVANCING
               DISPLAY "Invalid amount."
               DISPLAY ANSI-BASE WITH NO ADVANCING
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-TRANSFER-AMOUNT ROUNDED =
               FUNCTION NUMVAL(FUNCTION TRIM(WS-AMOUNT-IN))

           IF WS-TRANSFER-AMOUNT <= 0
               MOVE "N" TO WS-OP-OK
               DISPLAY ANSI-LITERAL WITH NO ADVANCING
               DISPLAY "Amount must be greater than zero."
               DISPLAY ANSI-BASE WITH NO ADVANCING
           END-IF.

       COMMIT-WITH-LEDGER.
           MOVE "Y" TO WS-OP-OK

           PERFORM SAVE-ACCOUNT-TABLE
           IF WS-OP-OK NOT = "Y"
               DISPLAY ANSI-LITERAL WITH NO ADVANCING
               DISPLAY "Unable to save account balances."
               DISPLAY ANSI-BASE WITH NO ADVANCING
               EXIT PARAGRAPH
           END-IF

           PERFORM POST-DOUBLE-ENTRY
           IF WS-OP-OK NOT = "Y"
               MOVE WS-OLD-CHECKING TO WS-TBL-CHECKING(WS-USER-IDX)
               MOVE WS-OLD-SAVINGS TO WS-TBL-SAVINGS(WS-USER-IDX)
               PERFORM SAVE-ACCOUNT-TABLE
               DISPLAY ANSI-LITERAL WITH NO ADVANCING
               DISPLAY "Ledger post failed. Balances rolled back."
               DISPLAY ANSI-BASE WITH NO ADVANCING
               EXIT PARAGRAPH
           END-IF.

       SHOW-RECENT-LEDGER.
           PERFORM CLEAR-SCREEN
           MOVE 0 TO WS-RECENT-COUNT

           PERFORM VARYING WS-RECENT-IDX FROM 1 BY 1
                   UNTIL WS-RECENT-IDX > 8
               MOVE SPACES TO WS-RECENT-LINE(WS-RECENT-IDX)
           END-PERFORM

           OPEN INPUT LEDGER-FILE
           IF WS-LEDGER-STATUS = "35"
               DISPLAY ANSI-COMMENT WITH NO ADVANCING
               DISPLAY "No ledger entries yet."
               DISPLAY ANSI-BASE WITH NO ADVANCING
               PERFORM WAIT-FOR-ENTER
               EXIT PARAGRAPH
           END-IF

           IF WS-LEDGER-STATUS NOT = "00"
               DISPLAY ANSI-LITERAL WITH NO ADVANCING
               DISPLAY "Cannot read ledger file."
               DISPLAY ANSI-BASE WITH NO ADVANCING
               PERFORM WAIT-FOR-ENTER
               EXIT PARAGRAPH
           END-IF

           MOVE "N" TO WS-EOF
           PERFORM UNTIL WS-EOF = "Y"
               READ LEDGER-FILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       PERFORM PARSE-LEDGER-LINE
                       IF WS-PARSE-OK = "Y"
                           IF WS-PARSED-EMP-ID = EMP-ID OF LK-EMPLOYEE-RECORD
                               PERFORM PUSH-RECENT-LINE
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE LEDGER-FILE

           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "Recent Ledger Entries (latest up to 8)"
           DISPLAY ANSI-COMMENT WITH NO ADVANCING
           DISPLAY "----------------------------------------------"

           IF WS-RECENT-COUNT = 0
               DISPLAY "No entries found for this user."
           ELSE
               PERFORM VARYING WS-RECENT-IDX FROM 1 BY 1
                       UNTIL WS-RECENT-IDX > WS-RECENT-COUNT
                   DISPLAY WS-RECENT-LINE(WS-RECENT-IDX)
               END-PERFORM
           END-IF

           DISPLAY ANSI-BASE WITH NO ADVANCING
           PERFORM WAIT-FOR-ENTER.

       PUSH-RECENT-LINE.
           IF WS-RECENT-COUNT < 8
               ADD 1 TO WS-RECENT-COUNT
               MOVE FUNCTION TRIM(LEDGER-LINE)
                 TO WS-RECENT-LINE(WS-RECENT-COUNT)
           ELSE
               PERFORM VARYING WS-RECENT-IDX FROM 1 BY 1
                       UNTIL WS-RECENT-IDX > 7
                   MOVE WS-RECENT-LINE(WS-RECENT-IDX + 1)
                     TO WS-RECENT-LINE(WS-RECENT-IDX)
               END-PERFORM
               MOVE FUNCTION TRIM(LEDGER-LINE) TO WS-RECENT-LINE(8)
           END-IF.

       RUN-END-OF-DAY-BATCH.
           PERFORM CLEAR-SCREEN

           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "End-of-Day Batch"
           DISPLAY ANSI-COMMENT WITH NO ADVANCING
           DISPLAY "- Apply savings interest"
           DISPLAY "- Apply overdraft fee when checking is negative"

           MOVE 0 TO WS-INTEREST-AMOUNT WS-FEE-AMOUNT

           IF WS-TBL-SAVINGS(WS-USER-IDX) > 0
               COMPUTE WS-INTEREST-AMOUNT ROUNDED =
                   WS-TBL-SAVINGS(WS-USER-IDX) * WS-DAILY-INTEREST-RATE

               IF WS-INTEREST-AMOUNT > 0
                   MOVE WS-TBL-CHECKING(WS-USER-IDX) TO WS-OLD-CHECKING
                   MOVE WS-TBL-SAVINGS(WS-USER-IDX) TO WS-OLD-SAVINGS

                   COMPUTE WS-TBL-SAVINGS(WS-USER-IDX) =
                       WS-TBL-SAVINGS(WS-USER-IDX) + WS-INTEREST-AMOUNT

                   MOVE "BANK_INT_EXPENSE" TO WS-DEBIT-ACCOUNT
                   MOVE "SAVINGS" TO WS-CREDIT-ACCOUNT
                   MOVE "EOD_INTEREST" TO WS-POST-MEMO
                   MOVE WS-INTEREST-AMOUNT TO WS-POST-AMOUNT

                   PERFORM COMMIT-WITH-LEDGER
               END-IF
           END-IF

           IF WS-OP-OK = "Y"
              AND WS-TBL-CHECKING(WS-USER-IDX) < 0
               MOVE WS-OVERDRAFT-FEE TO WS-FEE-AMOUNT

               MOVE WS-TBL-CHECKING(WS-USER-IDX) TO WS-OLD-CHECKING
               MOVE WS-TBL-SAVINGS(WS-USER-IDX) TO WS-OLD-SAVINGS

               COMPUTE WS-TBL-CHECKING(WS-USER-IDX) =
                   WS-TBL-CHECKING(WS-USER-IDX) - WS-FEE-AMOUNT

               MOVE "CHECKING" TO WS-DEBIT-ACCOUNT
               MOVE "BANK_FEE_INCOME" TO WS-CREDIT-ACCOUNT
               MOVE "EOD_OVERDRAFT_FEE" TO WS-POST-MEMO
               MOVE WS-FEE-AMOUNT TO WS-POST-AMOUNT

               PERFORM COMMIT-WITH-LEDGER
           END-IF

           IF WS-OP-OK = "Y"
               DISPLAY ANSI-COMMENT WITH NO ADVANCING
               DISPLAY "EOD batch completed."
           ELSE
               DISPLAY ANSI-LITERAL WITH NO ADVANCING
               DISPLAY "EOD batch incomplete due to posting error."
           END-IF

           MOVE WS-INTEREST-AMOUNT TO WS-AMOUNT-DISPLAY
           DISPLAY "Interest Posted : $" WS-AMOUNT-DISPLAY

           MOVE WS-FEE-AMOUNT TO WS-AMOUNT-DISPLAY
           DISPLAY "Overdraft Fee   : $" WS-AMOUNT-DISPLAY

           MOVE WS-TBL-CHECKING(WS-USER-IDX) TO WS-AMOUNT-DISPLAY
           DISPLAY "Checking        : $" WS-AMOUNT-DISPLAY

           MOVE WS-TBL-SAVINGS(WS-USER-IDX) TO WS-AMOUNT-DISPLAY
           DISPLAY "Savings         : $" WS-AMOUNT-DISPLAY

           DISPLAY ANSI-BASE WITH NO ADVANCING
           PERFORM WAIT-FOR-ENTER.

       RUN-LEDGER-AUDIT.
           PERFORM CLEAR-SCREEN
           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "Ledger Balance Audit"

           MOVE 0 TO WS-AUDIT-COUNT WS-TX-COUNT WS-IMBALANCED-COUNT
           MOVE "N" TO WS-EOF

           PERFORM VARYING WS-AUDIT-IDX FROM 1 BY 1
                   UNTIL WS-AUDIT-IDX > 300
               MOVE 0 TO WS-AUDIT-ID(WS-AUDIT-IDX)
               MOVE 0 TO WS-AUDIT-DEBIT(WS-AUDIT-IDX)
               MOVE 0 TO WS-AUDIT-CREDIT(WS-AUDIT-IDX)
           END-PERFORM

           OPEN INPUT LEDGER-FILE
           IF WS-LEDGER-STATUS = "35"
               DISPLAY ANSI-COMMENT WITH NO ADVANCING
               DISPLAY "No ledger entries to audit."
               DISPLAY ANSI-BASE WITH NO ADVANCING
               PERFORM WAIT-FOR-ENTER
               EXIT PARAGRAPH
           END-IF

           IF WS-LEDGER-STATUS NOT = "00"
               DISPLAY ANSI-LITERAL WITH NO ADVANCING
               DISPLAY "Cannot read ledger file."
               DISPLAY ANSI-BASE WITH NO ADVANCING
               PERFORM WAIT-FOR-ENTER
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-EOF = "Y"
               READ LEDGER-FILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       PERFORM PARSE-LEDGER-LINE
                       IF WS-PARSE-OK = "Y"
                           IF WS-PARSED-EMP-ID = EMP-ID OF LK-EMPLOYEE-RECORD
                               PERFORM FIND-OR-CREATE-AUDIT-ENTRY
                               IF WS-AUDIT-FOUND-IDX > 0
                                   IF FUNCTION UPPER-CASE(
                                      FUNCTION TRIM(WS-LINE-ENTRY-TYPE)) =
                                      "DEBIT"
                                       ADD WS-PARSED-AMOUNT TO
                                           WS-AUDIT-DEBIT(
                                             WS-AUDIT-FOUND-IDX)
                                   ELSE
                                       IF FUNCTION UPPER-CASE(
                                          FUNCTION TRIM(WS-LINE-ENTRY-TYPE)) =
                                          "CREDIT"
                                           ADD WS-PARSED-AMOUNT TO
                                               WS-AUDIT-CREDIT(
                                                 WS-AUDIT-FOUND-IDX)
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE LEDGER-FILE

           MOVE WS-AUDIT-COUNT TO WS-TX-COUNT

           PERFORM VARYING WS-AUDIT-IDX FROM 1 BY 1
                   UNTIL WS-AUDIT-IDX > WS-AUDIT-COUNT
               IF WS-AUDIT-DEBIT(WS-AUDIT-IDX) NOT =
                  WS-AUDIT-CREDIT(WS-AUDIT-IDX)
                   ADD 1 TO WS-IMBALANCED-COUNT
               END-IF
           END-PERFORM

           DISPLAY ANSI-COMMENT WITH NO ADVANCING
           DISPLAY "----------------------------------------------"
           DISPLAY "Transactions audited : " WS-TX-COUNT
           DISPLAY "Imbalanced txns      : " WS-IMBALANCED-COUNT

           IF WS-IMBALANCED-COUNT = 0
               DISPLAY ANSI-COMMENT WITH NO ADVANCING
               DISPLAY "Result: balanced double-entry ledger."
           ELSE
               DISPLAY ANSI-LITERAL WITH NO ADVANCING
               DISPLAY "Result: one or more transactions are unbalanced."
           END-IF

           DISPLAY ANSI-BASE WITH NO ADVANCING
           PERFORM WAIT-FOR-ENTER.

       FIND-OR-CREATE-AUDIT-ENTRY.
           MOVE 0 TO WS-AUDIT-FOUND-IDX

           PERFORM VARYING WS-AUDIT-IDX FROM 1 BY 1
                   UNTIL WS-AUDIT-IDX > WS-AUDIT-COUNT
                   OR WS-AUDIT-FOUND-IDX > 0
               IF WS-AUDIT-ID(WS-AUDIT-IDX) = WS-PARSED-TX-ID
                   MOVE WS-AUDIT-IDX TO WS-AUDIT-FOUND-IDX
               END-IF
           END-PERFORM

           IF WS-AUDIT-FOUND-IDX = 0
               IF WS-AUDIT-COUNT < 300
                   ADD 1 TO WS-AUDIT-COUNT
                   MOVE WS-PARSED-TX-ID TO WS-AUDIT-ID(WS-AUDIT-COUNT)
                   MOVE 0 TO WS-AUDIT-DEBIT(WS-AUDIT-COUNT)
                   MOVE 0 TO WS-AUDIT-CREDIT(WS-AUDIT-COUNT)
                   MOVE WS-AUDIT-COUNT TO WS-AUDIT-FOUND-IDX
               END-IF
           END-IF.

       COMMIT-ACCOUNT-TABLE.
           PERFORM SAVE-ACCOUNT-TABLE.

       LOAD-ACCOUNT-TABLE.
           MOVE "Y" TO WS-OP-OK
           MOVE 0 TO WS-ACCOUNT-COUNT
           MOVE "N" TO WS-EOF

           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCOUNT-STATUS NOT = "00"
               MOVE "N" TO WS-OP-OK
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-EOF = "Y"
               READ ACCOUNT-FILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       IF WS-ACCOUNT-COUNT >= 200
                           MOVE "Y" TO WS-EOF
                       ELSE
                           ADD 1 TO WS-ACCOUNT-COUNT
                           PERFORM PARSE-ACCOUNT-LINE
                           IF WS-PARSE-OK NOT = "Y"
                               MOVE "N" TO WS-OP-OK
                               MOVE "Y" TO WS-EOF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE ACCOUNT-FILE.

       PARSE-ACCOUNT-LINE.
           MOVE "Y" TO WS-PARSE-OK
           MOVE 0 TO WS-FIELD-COUNT
           MOVE SPACES TO WS-ID-TEXT WS-CHECKING-TEXT
                          WS-SAVINGS-TEXT WS-LOAN-TEXT

           UNSTRING ACCOUNT-LINE DELIMITED BY "|"
               INTO WS-ID-TEXT
                    WS-CHECKING-TEXT
                    WS-SAVINGS-TEXT
                    WS-LOAN-TEXT
               TALLYING IN WS-FIELD-COUNT
           END-UNSTRING

           IF WS-FIELD-COUNT NOT = 4
               MOVE "N" TO WS-PARSE-OK
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION TEST-NUMVAL(FUNCTION TRIM(WS-ID-TEXT)) NOT = 0
               MOVE "N" TO WS-PARSE-OK
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION TEST-NUMVAL(FUNCTION TRIM(WS-CHECKING-TEXT)) NOT = 0
               MOVE "N" TO WS-PARSE-OK
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION TEST-NUMVAL(FUNCTION TRIM(WS-SAVINGS-TEXT)) NOT = 0
               MOVE "N" TO WS-PARSE-OK
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION TEST-NUMVAL(FUNCTION TRIM(WS-LOAN-TEXT)) NOT = 0
               MOVE "N" TO WS-PARSE-OK
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-ID-TEXT))
             TO WS-TBL-EMP-ID(WS-ACCOUNT-COUNT)
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-CHECKING-TEXT))
             TO WS-TBL-CHECKING(WS-ACCOUNT-COUNT)
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-SAVINGS-TEXT))
             TO WS-TBL-SAVINGS(WS-ACCOUNT-COUNT)
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-LOAN-TEXT))
             TO WS-TBL-LOAN(WS-ACCOUNT-COUNT).

       LOCATE-CURRENT-ACCOUNT.
           MOVE 0 TO WS-USER-IDX
           PERFORM VARYING WS-ROW-IDX FROM 1 BY 1
                   UNTIL WS-ROW-IDX > WS-ACCOUNT-COUNT
                   OR WS-USER-IDX > 0
               IF WS-TBL-EMP-ID(WS-ROW-IDX) = EMP-ID OF LK-EMPLOYEE-RECORD
                   MOVE WS-ROW-IDX TO WS-USER-IDX
               END-IF
           END-PERFORM.

       SAVE-ACCOUNT-TABLE.
           MOVE "Y" TO WS-OP-OK

           OPEN OUTPUT ACCOUNT-FILE
           IF WS-ACCOUNT-STATUS NOT = "00"
               MOVE "N" TO WS-OP-OK
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-ROW-IDX FROM 1 BY 1
                   UNTIL WS-ROW-IDX > WS-ACCOUNT-COUNT
               MOVE WS-TBL-EMP-ID(WS-ROW-IDX) TO WS-ID-OUT-TEXT

               MOVE WS-TBL-CHECKING(WS-ROW-IDX) TO WS-AMOUNT-EDIT
               MOVE WS-AMOUNT-EDIT TO WS-CHECKING-OUT-TEXT

               MOVE WS-TBL-SAVINGS(WS-ROW-IDX) TO WS-AMOUNT-EDIT
               MOVE WS-AMOUNT-EDIT TO WS-SAVINGS-OUT-TEXT

               MOVE WS-TBL-LOAN(WS-ROW-IDX) TO WS-AMOUNT-EDIT
               MOVE WS-AMOUNT-EDIT TO WS-LOAN-OUT-TEXT

               STRING FUNCTION TRIM(WS-ID-OUT-TEXT)
                      "|"
                      FUNCTION TRIM(WS-CHECKING-OUT-TEXT)
                      "|"
                      FUNCTION TRIM(WS-SAVINGS-OUT-TEXT)
                      "|"
                      FUNCTION TRIM(WS-LOAN-OUT-TEXT)
                 INTO ACCOUNT-LINE
               END-STRING

               WRITE ACCOUNT-LINE
           END-PERFORM

           CLOSE ACCOUNT-FILE.

       POST-DOUBLE-ENTRY.
           MOVE "Y" TO WS-OP-OK

           PERFORM GET-NEXT-TX-ID
           IF WS-OP-OK NOT = "Y"
               EXIT PARAGRAPH
           END-IF

           PERFORM BUILD-TIMESTAMP
           PERFORM ENSURE-LEDGER-FILE
           IF WS-OP-OK NOT = "Y"
               EXIT PARAGRAPH
           END-IF

           OPEN EXTEND LEDGER-FILE
           IF WS-LEDGER-STATUS NOT = "00"
               MOVE "N" TO WS-OP-OK
               EXIT PARAGRAPH
           END-IF

           MOVE "DEBIT" TO WS-ENTRY-TYPE
           MOVE WS-DEBIT-ACCOUNT TO WS-ENTRY-ACCOUNT
           MOVE WS-POST-MEMO TO WS-ENTRY-MEMO
           PERFORM WRITE-LEDGER-LINE

           MOVE "CREDIT" TO WS-ENTRY-TYPE
           MOVE WS-CREDIT-ACCOUNT TO WS-ENTRY-ACCOUNT
           MOVE WS-POST-MEMO TO WS-ENTRY-MEMO
           PERFORM WRITE-LEDGER-LINE

           CLOSE LEDGER-FILE.

       WRITE-LEDGER-LINE.
           MOVE WS-TX-ID TO WS-TX-ID-TEXT
           MOVE EMP-ID OF LK-EMPLOYEE-RECORD TO WS-EMP-ID-TEXT
           MOVE WS-POST-AMOUNT TO WS-AMOUNT-EDIT
           MOVE WS-AMOUNT-EDIT TO WS-AMOUNT-TEXT

           STRING FUNCTION TRIM(WS-TX-ID-TEXT)
                  "|"
                  WS-TIMESTAMP
                  "|"
                  FUNCTION TRIM(WS-EMP-ID-TEXT)
                  "|"
                  FUNCTION TRIM(WS-ENTRY-TYPE)
                  "|"
                  FUNCTION TRIM(WS-ENTRY-ACCOUNT)
                  "|"
                  FUNCTION TRIM(WS-AMOUNT-TEXT)
                  "|"
                  FUNCTION TRIM(WS-ENTRY-MEMO)
             INTO LEDGER-LINE
           END-STRING

           WRITE LEDGER-LINE.

       GET-NEXT-TX-ID.
           MOVE "Y" TO WS-OP-OK
           MOVE 0 TO WS-MAX-TX-ID
           MOVE "N" TO WS-EOF

           OPEN INPUT LEDGER-FILE
           IF WS-LEDGER-STATUS = "35"
               MOVE 1 TO WS-TX-ID
               EXIT PARAGRAPH
           END-IF

           IF WS-LEDGER-STATUS NOT = "00"
               MOVE "N" TO WS-OP-OK
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-EOF = "Y"
               READ LEDGER-FILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       PERFORM PARSE-LEDGER-LINE
                       IF WS-PARSE-OK = "Y"
                           IF WS-PARSED-TX-ID > WS-MAX-TX-ID
                               MOVE WS-PARSED-TX-ID TO WS-MAX-TX-ID
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE LEDGER-FILE
           COMPUTE WS-TX-ID = WS-MAX-TX-ID + 1.

       ENSURE-LEDGER-FILE.
           MOVE "Y" TO WS-OP-OK

           OPEN I-O LEDGER-FILE
           IF WS-LEDGER-STATUS = "00"
               CLOSE LEDGER-FILE
               EXIT PARAGRAPH
           END-IF

           IF WS-LEDGER-STATUS = "35"
               OPEN OUTPUT LEDGER-FILE
               IF WS-LEDGER-STATUS = "00"
                   CLOSE LEDGER-FILE
               ELSE
                   MOVE "N" TO WS-OP-OK
               END-IF
               EXIT PARAGRAPH
           END-IF

           MOVE "N" TO WS-OP-OK.

       APPEND-HIGH-VALUE-ALERT.
           MOVE "N" TO WS-ALERT-WRITTEN
           PERFORM BUILD-TIMESTAMP

           MOVE EMP-ID OF LK-EMPLOYEE-RECORD TO WS-EMP-ID-TEXT
           MOVE WS-POST-AMOUNT TO WS-AMOUNT-EDIT
           MOVE WS-AMOUNT-EDIT TO WS-AMOUNT-TEXT

           STRING WS-TIMESTAMP
                  "|"
                  FUNCTION TRIM(WS-EMP-ID-TEXT)
                  "|HIGH_VALUE_TRANSFER|"
                  FUNCTION TRIM(WS-AMOUNT-TEXT)
                  "|"
                  FUNCTION TRIM(WS-POST-MEMO)
             INTO ALERT-LINE
           END-STRING

           MOVE 0 TO WS-ALERT-COUNT
           MOVE "N" TO WS-EOF

           OPEN INPUT ALERT-FILE
           IF WS-ALERT-STATUS = "00"
               PERFORM UNTIL WS-EOF = "Y"
                   READ ALERT-FILE
                       AT END
                           MOVE "Y" TO WS-EOF
                       NOT AT END
                           IF WS-ALERT-COUNT < 300
                               ADD 1 TO WS-ALERT-COUNT
                               MOVE ALERT-LINE
                                 TO WS-ALERT-BUF-LINE(WS-ALERT-COUNT)
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE ALERT-FILE
           ELSE
               IF WS-ALERT-STATUS NOT = "35"
                   EXIT PARAGRAPH
               END-IF
           END-IF

           OPEN OUTPUT ALERT-FILE
           IF WS-ALERT-STATUS NOT = "00"
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-ALERT-IDX FROM 1 BY 1
                   UNTIL WS-ALERT-IDX > WS-ALERT-COUNT
               MOVE WS-ALERT-BUF-LINE(WS-ALERT-IDX) TO ALERT-LINE
               WRITE ALERT-LINE
           END-PERFORM

           WRITE ALERT-LINE
           IF WS-ALERT-STATUS = "00"
               MOVE "Y" TO WS-ALERT-WRITTEN
           END-IF
           CLOSE ALERT-FILE.

       BUILD-TIMESTAMP.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           STRING WS-CURRENT-DATE(1:4)
                  "-"
                  WS-CURRENT-DATE(5:2)
                  "-"
                  WS-CURRENT-DATE(7:2)
                  " "
                  WS-CURRENT-DATE(9:2)
                  ":"
                  WS-CURRENT-DATE(11:2)
                  ":"
                  WS-CURRENT-DATE(13:2)
             INTO WS-TIMESTAMP
           END-STRING.

       PARSE-LEDGER-LINE.
           MOVE "Y" TO WS-PARSE-OK
           MOVE 0 TO WS-FIELD-COUNT
           MOVE SPACES TO WS-LINE-TX-ID-TEXT
                          WS-LINE-TS-TEXT
                          WS-LINE-EMP-ID-TEXT
                          WS-LINE-ENTRY-TYPE
                          WS-LINE-ACCOUNT
                          WS-LINE-AMOUNT-TEXT
                          WS-LINE-MEMO

           UNSTRING LEDGER-LINE DELIMITED BY "|"
               INTO WS-LINE-TX-ID-TEXT
                    WS-LINE-TS-TEXT
                    WS-LINE-EMP-ID-TEXT
                    WS-LINE-ENTRY-TYPE
                    WS-LINE-ACCOUNT
                    WS-LINE-AMOUNT-TEXT
                    WS-LINE-MEMO
               TALLYING IN WS-FIELD-COUNT
           END-UNSTRING

           IF WS-FIELD-COUNT NOT = 7
               MOVE "N" TO WS-PARSE-OK
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION TEST-NUMVAL(
              FUNCTION TRIM(WS-LINE-TX-ID-TEXT)) NOT = 0
               MOVE "N" TO WS-PARSE-OK
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION TEST-NUMVAL(
              FUNCTION TRIM(WS-LINE-EMP-ID-TEXT)) NOT = 0
               MOVE "N" TO WS-PARSE-OK
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION TEST-NUMVAL(
              FUNCTION TRIM(WS-LINE-AMOUNT-TEXT)) NOT = 0
               MOVE "N" TO WS-PARSE-OK
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-LINE-TX-ID-TEXT))
             TO WS-PARSED-TX-ID
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-LINE-EMP-ID-TEXT))
             TO WS-PARSED-EMP-ID
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-LINE-AMOUNT-TEXT))
             TO WS-PARSED-AMOUNT.

       WAIT-FOR-ENTER.
           DISPLAY " "
           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "Press ENTER to continue..." WITH NO ADVANCING
           DISPLAY ANSI-BASE WITH NO ADVANCING
           ACCEPT WS-DUMMY.
