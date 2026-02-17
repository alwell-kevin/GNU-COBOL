       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUTHENTICATE-EMPLOYEE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE
               ASSIGN TO DYNAMIC WS-DATA-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-LINE                 PIC X(256).

       WORKING-STORAGE SECTION.
       01  WS-PARSED-EMPLOYEE.
           COPY "employee_record.cpy".

       77  WS-DATA-PATH                  PIC X(256).
       77  WS-FILE-STATUS                PIC XX VALUE SPACES.
       77  WS-ATTEMPT                    PIC 9 VALUE 0.
       77  WS-MAX-ATTEMPTS               PIC 9 VALUE 3.
       77  WS-EOF                        PIC X VALUE "N".
       77  WS-MATCHED                    PIC X VALUE "N".
       77  WS-ROW-NUMBER                 PIC 9(5) VALUE 0.
       77  WS-FIELD-COUNT                PIC 9 VALUE 0.

       77  WS-USERNAME-IN                PIC X(20).
       77  WS-PASSWORD-IN                PIC X(20).
       77  WS-INPUT-USERNAME-NORM        PIC X(20).
       77  WS-ROW-USERNAME-NORM          PIC X(20).

       77  WS-ID-TEXT                    PIC X(20).
       77  WS-USER-TEXT                  PIC X(20).
       77  WS-PASS-TEXT                  PIC X(20).
       77  WS-NAME-TEXT                  PIC X(40).
       77  WS-RATE-TEXT                  PIC X(20).
       77  WS-HOURS-TEXT                 PIC X(20).
       77  WS-YTD-TEXT                   PIC X(20).

       77  ANSI-RESET                    PIC X(4) VALUE X"1B5B306D".
       77  ANSI-BASE                     PIC X(8) VALUE X"1B5B34303B39376D".
       77  ANSI-KEYWORD                  PIC X(7) VALUE X"1B5B39363B316D".
       77  ANSI-IDENT                    PIC X(8) VALUE X"1B5B33373B316D".
       77  ANSI-COMMENT                  PIC X(5) VALUE X"1B5B33326D".
       77  ANSI-LITERAL                  PIC X(5) VALUE X"1B5B39336D".
       77  ANSI-NUMBER                   PIC X(5) VALUE X"1B5B39356D".
       77  ANSI-OPERATOR                 PIC X(5) VALUE X"1B5B39346D".
       77  ANSI-INPUT                    PIC X(5) VALUE X"1B5B39326D".
       77  ANSI-HOME                     PIC X(3) VALUE X"1B5B48".
       77  ANSI-CLEAR                    PIC X(4) VALUE X"1B5B324A".

       77  WS-CURRENT-DATE               PIC X(21) VALUE SPACES.
       77  WS-DATE-DISPLAY               PIC X(10) VALUE SPACES.
       77  WS-TIME-DISPLAY               PIC X(8) VALUE SPACES.
       77  WS-TERM-COLS-TEXT             PIC X(10) VALUE SPACES.
       77  WS-TERM-ROWS-TEXT             PIC X(10) VALUE SPACES.
       77  WS-SCREEN-COLS                PIC 9(4) VALUE 132.
       77  WS-SCREEN-ROWS                PIC 9(4) VALUE 42.
       77  WS-BLOCK-WIDTH                PIC 9(4) VALUE 60.
       77  WS-BLOCK-HEIGHT               PIC 9(4) VALUE 20.
       77  WS-LEFT-PAD                   PIC 9(4) VALUE 0.
       77  WS-TOP-PAD                    PIC 9(4) VALUE 0.
       77  WS-PAD-I                      PIC 9(4) VALUE 0.
       77  WS-PAD-SPACES                 PIC X(200) VALUE ALL " ".

       LINKAGE SECTION.
       01  LK-DATA-PATH                  PIC X(256).
       01  LK-AUTH-RESULT                PIC X.
       01  LK-EMPLOYEE-RECORD.
           COPY "employee_record.cpy".

       PROCEDURE DIVISION USING LK-DATA-PATH LK-AUTH-RESULT
                                LK-EMPLOYEE-RECORD.
       MAIN-PROCEDURE.
           MOVE FUNCTION TRIM(LK-DATA-PATH) TO WS-DATA-PATH
           MOVE "N" TO LK-AUTH-RESULT
           MOVE SPACES TO LK-EMPLOYEE-RECORD

           PERFORM VARYING WS-ATTEMPT FROM 1 BY 1
               UNTIL WS-ATTEMPT > WS-MAX-ATTEMPTS
                  OR LK-AUTH-RESULT NOT = "N"
               PERFORM DRAW-SIGNON-SCREEN

               PERFORM DISPLAY-LEFT-PADDING
               DISPLAY ANSI-COMMENT WITH NO ADVANCING
               DISPLAY "Attempt " WS-ATTEMPT " of " WS-MAX-ATTEMPTS

               PERFORM DISPLAY-LEFT-PADDING
               DISPLAY " "
               PERFORM DISPLAY-LEFT-PADDING
               DISPLAY ANSI-KEYWORD WITH NO ADVANCING
               DISPLAY "User ID    : " WITH NO ADVANCING
               DISPLAY ANSI-INPUT WITH NO ADVANCING
               DISPLAY "__ " WITH NO ADVANCING
               DISPLAY ANSI-BASE WITH NO ADVANCING
               ACCEPT WS-USERNAME-IN

               PERFORM DISPLAY-LEFT-PADDING
               DISPLAY ANSI-KEYWORD WITH NO ADVANCING
               DISPLAY "Password   : " WITH NO ADVANCING
               DISPLAY ANSI-INPUT WITH NO ADVANCING
               DISPLAY "__ " WITH NO ADVANCING
               DISPLAY ANSI-BASE WITH NO ADVANCING
               ACCEPT WS-PASSWORD-IN

               PERFORM FIND-EMPLOYEE-ROW

               EVALUATE LK-AUTH-RESULT
                   WHEN "Y"
                       CONTINUE
                   WHEN "E"
                       CONTINUE
                   WHEN OTHER
                       PERFORM DISPLAY-LEFT-PADDING
                       DISPLAY ANSI-LITERAL WITH NO ADVANCING
                       DISPLAY "Invalid username or password."
               END-EVALUATE
           END-PERFORM

           GOBACK.

       DRAW-SIGNON-SCREEN.
           DISPLAY FUNCTION CONCATENATE(ANSI-CLEAR, ANSI-HOME)
               WITH NO ADVANCING
           MOVE 60 TO WS-BLOCK-WIDTH
           MOVE 20 TO WS-BLOCK-HEIGHT
           PERFORM PREPARE-CENTER-BLOCK
           PERFORM DISPLAY-TOP-PADDING

           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "Helical Pay Systems Login"
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY " "

           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-OPERATOR WITH NO ADVANCING
           DISPLAY "+-----------------------------------------+"
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "|          EMPLOYEE ACCESS TERMINAL       |"
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "|-----------------------------------------|"
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "|  VERIFY USER ID AND PASSWORD            |"
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "|  FOR PAYROLL VIEWER                     |"
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "+-----------------------------------------+"

           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY " "
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "Type your User ID and Password, then press ENTER:"
           DISPLAY ANSI-BASE WITH NO ADVANCING
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY " "
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-COMMENT WITH NO ADVANCING
           DISPLAY "ENTER=Sign-on   F3=Exit"
           DISPLAY ANSI-BASE WITH NO ADVANCING.

       LOAD-TERM-SIZE.
           MOVE SPACES TO WS-TERM-COLS-TEXT WS-TERM-ROWS-TEXT
           ACCEPT WS-TERM-COLS-TEXT FROM ENVIRONMENT "PAYVIEW_TERM_COLS"
           ACCEPT WS-TERM-ROWS-TEXT FROM ENVIRONMENT "PAYVIEW_TERM_ROWS"

           IF FUNCTION TEST-NUMVAL(FUNCTION TRIM(WS-TERM-COLS-TEXT)) = 0
               IF FUNCTION NUMVAL(FUNCTION TRIM(WS-TERM-COLS-TEXT)) > 0
                   MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-TERM-COLS-TEXT))
                     TO WS-SCREEN-COLS
               END-IF
           END-IF

           IF FUNCTION TEST-NUMVAL(FUNCTION TRIM(WS-TERM-ROWS-TEXT)) = 0
               IF FUNCTION NUMVAL(FUNCTION TRIM(WS-TERM-ROWS-TEXT)) > 0
                   MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-TERM-ROWS-TEXT))
                     TO WS-SCREEN-ROWS
               END-IF
           END-IF.

       PREPARE-CENTER-BLOCK.
           PERFORM LOAD-TERM-SIZE
           MOVE 0 TO WS-LEFT-PAD WS-TOP-PAD

           IF WS-SCREEN-COLS > WS-BLOCK-WIDTH
               COMPUTE WS-LEFT-PAD =
                   FUNCTION INTEGER((WS-SCREEN-COLS - WS-BLOCK-WIDTH) / 2)
           END-IF

           IF WS-SCREEN-ROWS > WS-BLOCK-HEIGHT
               COMPUTE WS-TOP-PAD =
                   FUNCTION INTEGER((WS-SCREEN-ROWS - WS-BLOCK-HEIGHT) / 2)
           END-IF

           IF WS-LEFT-PAD > 200
               MOVE 200 TO WS-LEFT-PAD
           END-IF.

       DISPLAY-TOP-PADDING.
           PERFORM VARYING WS-PAD-I FROM 1 BY 1 UNTIL WS-PAD-I > WS-TOP-PAD
               DISPLAY " "
           END-PERFORM.

       DISPLAY-LEFT-PADDING.
           IF WS-LEFT-PAD > 0
               DISPLAY WS-PAD-SPACES(1:WS-LEFT-PAD) WITH NO ADVANCING
           END-IF.

       FIND-EMPLOYEE-ROW.
           MOVE "N" TO WS-EOF
           MOVE "N" TO WS-MATCHED
           MOVE 0 TO WS-ROW-NUMBER
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(WS-USERNAME-IN))
             TO WS-INPUT-USERNAME-NORM

           OPEN INPUT EMPLOYEE-FILE
           IF WS-FILE-STATUS NOT = "00"
               PERFORM DISPLAY-LEFT-PADDING
               DISPLAY ANSI-LITERAL WITH NO ADVANCING
               DISPLAY "ERROR: Cannot open employee data file."
               MOVE "E" TO LK-AUTH-RESULT
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-EOF = "Y"
                    OR WS-MATCHED = "Y"
                    OR LK-AUTH-RESULT = "E"
               READ EMPLOYEE-FILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-ROW-NUMBER
                       PERFORM PARSE-ROW
                       IF LK-AUTH-RESULT NOT = "E"
                           PERFORM CHECK-CREDENTIALS
                       END-IF
               END-READ
           END-PERFORM

           CLOSE EMPLOYEE-FILE

           IF WS-MATCHED = "Y"
               MOVE "Y" TO LK-AUTH-RESULT
           ELSE
               IF LK-AUTH-RESULT NOT = "E"
                   MOVE "N" TO LK-AUTH-RESULT
               END-IF
           END-IF.

       PARSE-ROW.
           MOVE SPACES TO WS-PARSED-EMPLOYEE
           MOVE SPACES TO WS-ID-TEXT WS-USER-TEXT WS-PASS-TEXT
                          WS-NAME-TEXT WS-RATE-TEXT WS-HOURS-TEXT
                          WS-YTD-TEXT
           MOVE 0 TO WS-FIELD-COUNT

           UNSTRING EMPLOYEE-LINE DELIMITED BY "|"
               INTO WS-ID-TEXT
                    WS-USER-TEXT
                    WS-PASS-TEXT
                    WS-NAME-TEXT
                    WS-RATE-TEXT
                    WS-HOURS-TEXT
                    WS-YTD-TEXT
               TALLYING IN WS-FIELD-COUNT
           END-UNSTRING

           IF WS-FIELD-COUNT NOT = 7
               PERFORM REPORT-MALFORMED-ROW
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION TEST-NUMVAL(FUNCTION TRIM(WS-ID-TEXT)) NOT = 0
               PERFORM REPORT-MALFORMED-ROW
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION TEST-NUMVAL(FUNCTION TRIM(WS-RATE-TEXT)) NOT = 0
               PERFORM REPORT-MALFORMED-ROW
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION TEST-NUMVAL(FUNCTION TRIM(WS-HOURS-TEXT)) NOT = 0
               PERFORM REPORT-MALFORMED-ROW
               EXIT PARAGRAPH
           END-IF

           IF FUNCTION TEST-NUMVAL(FUNCTION TRIM(WS-YTD-TEXT)) NOT = 0
               PERFORM REPORT-MALFORMED-ROW
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-ID-TEXT))
             TO EMP-ID OF WS-PARSED-EMPLOYEE
           MOVE FUNCTION TRIM(WS-USER-TEXT)
             TO EMP-USERNAME OF WS-PARSED-EMPLOYEE
           MOVE FUNCTION TRIM(WS-PASS-TEXT)
             TO EMP-PASSWORD OF WS-PARSED-EMPLOYEE
           MOVE FUNCTION TRIM(WS-NAME-TEXT)
             TO EMP-FULL-NAME OF WS-PARSED-EMPLOYEE
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-RATE-TEXT))
             TO EMP-HOURLY-RATE OF WS-PARSED-EMPLOYEE
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-HOURS-TEXT))
             TO EMP-HOURS-PERIOD OF WS-PARSED-EMPLOYEE
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-YTD-TEXT))
             TO EMP-YTD-WAGES OF WS-PARSED-EMPLOYEE.

       CHECK-CREDENTIALS.
           MOVE FUNCTION UPPER-CASE(
                FUNCTION TRIM(EMP-USERNAME OF WS-PARSED-EMPLOYEE))
             TO WS-ROW-USERNAME-NORM

           IF WS-INPUT-USERNAME-NORM = WS-ROW-USERNAME-NORM
              AND FUNCTION TRIM(WS-PASSWORD-IN) =
                  FUNCTION TRIM(EMP-PASSWORD OF WS-PARSED-EMPLOYEE)
               MOVE WS-PARSED-EMPLOYEE TO LK-EMPLOYEE-RECORD
               MOVE "Y" TO WS-MATCHED
           END-IF.

       REPORT-MALFORMED-ROW.
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-LITERAL WITH NO ADVANCING
           DISPLAY "ERROR: Malformed employee row at line " WS-ROW-NUMBER
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-COMMENT WITH NO ADVANCING
           DISPLAY "Record: " FUNCTION TRIM(EMPLOYEE-LINE)
           MOVE "E" TO LK-AUTH-RESULT.
