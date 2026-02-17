       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYVIEW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STARTUP-FILE
               ASSIGN TO DYNAMIC WS-DATA-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STARTUP-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  STARTUP-FILE.
       01  STARTUP-RECORD                 PIC X(256).

       WORKING-STORAGE SECTION.
       01  WS-EMPLOYEE-RECORD.
           COPY "employee_record.cpy".
       01  WS-PAYROLL-CALC.
           COPY "payroll_calc.cpy".

       77  WS-DATA-PATH                   PIC X(256)
                                           VALUE "data/employees.dat".
       77  WS-STARTUP-FILE-STATUS         PIC XX VALUE SPACES.
       77  WS-AUTH-RESULT                 PIC X VALUE SPACE.
       77  WS-EXIT-APP                    PIC X VALUE "N".
       77  WS-EXIT-MENU                   PIC X VALUE "N".
       77  WS-HAVE-PENDING-OPTION         PIC X VALUE "N".
       77  WS-MENU-OPTION                 PIC X VALUE SPACE.
       77  WS-BRACKET-RATE                PIC 9V99 VALUE 0.

       77  WS-AMOUNT-DISPLAY              PIC ZZ,ZZZ,ZZ9.99.
       77  WS-HOURS-DISPLAY               PIC ZZ9.99.
       77  WS-RATE-DISPLAY                PIC Z9.99.
       77  WS-BRACKET-PERCENT             PIC 9(3)V99 VALUE 0.
       77  WS-EFFECTIVE-PERCENT-DISPLAY   PIC Z9.99.

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
       77  ANSI-ALT-ON                    PIC X(8) VALUE X"1B5B3F3130343968".
       77  ANSI-ALT-OFF                   PIC X(8) VALUE X"1B5B3F313034396C".
       77  ANSI-HIDE-CURSOR               PIC X(6) VALUE X"1B5B3F32356C".
       77  ANSI-SHOW-CURSOR               PIC X(6) VALUE X"1B5B3F323568".

       77  WS-CURRENT-DATE                PIC X(21) VALUE SPACES.
       77  WS-DATE-DISPLAY                PIC X(10) VALUE SPACES.
       77  WS-TIME-DISPLAY                PIC X(8) VALUE SPACES.
       77  WS-SCREEN-TITLE                PIC X(30) VALUE SPACES.
       77  WS-TERM-COLS-TEXT              PIC X(10) VALUE SPACES.
       77  WS-TERM-ROWS-TEXT              PIC X(10) VALUE SPACES.
       77  WS-SCREEN-COLS                 PIC 9(4) VALUE 132.
       77  WS-SCREEN-ROWS                 PIC 9(4) VALUE 42.
       77  WS-BLOCK-WIDTH                 PIC 9(4) VALUE 60.
       77  WS-BLOCK-HEIGHT                PIC 9(4) VALUE 16.
       77  WS-LEFT-PAD                    PIC 9(4) VALUE 0.
       77  WS-TOP-PAD                     PIC 9(4) VALUE 0.
       77  WS-PAD-I                       PIC 9(4) VALUE 0.
       77  WS-PAD-SPACES                  PIC X(200) VALUE ALL " ".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM ENTER-FULLSCREEN-THEME
           PERFORM CHECK-DATA-FILE
           IF RETURN-CODE NOT = 0
               PERFORM EXIT-FULLSCREEN-THEME
               STOP RUN
           END-IF

           PERFORM UNTIL WS-EXIT-APP = "Y"
               MOVE SPACES TO WS-EMPLOYEE-RECORD
               MOVE "N" TO WS-EXIT-MENU
               MOVE "N" TO WS-HAVE-PENDING-OPTION

               CALL "AUTHENTICATE-EMPLOYEE"
                   USING WS-DATA-PATH
                         WS-AUTH-RESULT
                         WS-EMPLOYEE-RECORD
               END-CALL

               EVALUATE WS-AUTH-RESULT
                   WHEN "Y"
                       PERFORM MENU-LOOP
                   WHEN "N"
                       DISPLAY ANSI-COMMENT WITH NO ADVANCING
                       DISPLAY "Maximum login attempts reached."
                       MOVE "Y" TO WS-EXIT-APP
                   WHEN "E"
                       DISPLAY ANSI-LITERAL WITH NO ADVANCING
                       DISPLAY "Authentication aborted due to data error."
                       MOVE 1 TO RETURN-CODE
                       MOVE "Y" TO WS-EXIT-APP
                   WHEN OTHER
                       DISPLAY ANSI-LITERAL WITH NO ADVANCING
                       DISPLAY "Unexpected authentication status."
                       MOVE 1 TO RETURN-CODE
                       MOVE "Y" TO WS-EXIT-APP
               END-EVALUATE
           END-PERFORM

           PERFORM EXIT-FULLSCREEN-THEME
           DISPLAY "Session ended."
           STOP RUN.

       ENTER-FULLSCREEN-THEME.
           DISPLAY FUNCTION CONCATENATE(
               ANSI-ALT-ON
               ANSI-HIDE-CURSOR
               ANSI-CLEAR
               ANSI-HOME
               ANSI-BASE)
               WITH NO ADVANCING.

       EXIT-FULLSCREEN-THEME.
           DISPLAY FUNCTION CONCATENATE(
               ANSI-RESET
               ANSI-SHOW-CURSOR
               ANSI-ALT-OFF)
               WITH NO ADVANCING.

       CLEAR-SCREEN.
           DISPLAY FUNCTION CONCATENATE(ANSI-CLEAR, ANSI-HOME)
               WITH NO ADVANCING.

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

       RENDER-HEADER.
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "Helical Pay Systems Login"

           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY " "
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY FUNCTION TRIM(WS-SCREEN-TITLE)
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY " ".

       CHECK-DATA-FILE.
           OPEN INPUT STARTUP-FILE
           IF WS-STARTUP-FILE-STATUS NOT = "00"
               DISPLAY ANSI-LITERAL WITH NO ADVANCING
               DISPLAY "ERROR: Cannot read employee data file: "
                       FUNCTION TRIM(WS-DATA-PATH)
               MOVE 1 TO RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           CLOSE STARTUP-FILE.

       MENU-LOOP.
           PERFORM UNTIL WS-EXIT-MENU = "Y"
               IF WS-HAVE-PENDING-OPTION NOT = "Y"
                   PERFORM DISPLAY-MENU-PROMPT
                   ACCEPT WS-MENU-OPTION
               ELSE
                   MOVE "N" TO WS-HAVE-PENDING-OPTION
               END-IF

               PERFORM HANDLE-MENU-OPTION
           END-PERFORM.

       DISPLAY-MENU-PROMPT.
           PERFORM CLEAR-SCREEN
           MOVE 60 TO WS-BLOCK-WIDTH
           MOVE 20 TO WS-BLOCK-HEIGHT
           PERFORM PREPARE-CENTER-BLOCK
           PERFORM DISPLAY-TOP-PADDING
           MOVE "Main Menu" TO WS-SCREEN-TITLE
           PERFORM RENDER-HEADER

           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY " 01." WITH NO ADVANCING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY " View pay details"

           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY " 02." WITH NO ADVANCING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY " View tax breakdown"

           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY " 03." WITH NO ADVANCING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY " View ASCII chart"

           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY " 04." WITH NO ADVANCING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY " Logout"

           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY " 05." WITH NO ADVANCING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY " Exit"

           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY " 06." WITH NO ADVANCING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY " Banking console"

           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY " "
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "Please select an option : " WITH NO ADVANCING
           DISPLAY ANSI-INPUT WITH NO ADVANCING
           DISPLAY "__ " WITH NO ADVANCING
           DISPLAY ANSI-BASE WITH NO ADVANCING.

       HANDLE-MENU-OPTION.
           EVALUATE WS-MENU-OPTION
               WHEN "1"
                   PERFORM SHOW-PAY-DETAILS
               WHEN "2"
                   PERFORM SHOW-TAX-BREAKDOWN
               WHEN "3"
                   PERFORM SHOW-ASCII-CHART
               WHEN "4"
                   PERFORM DISPLAY-LEFT-PADDING
                   DISPLAY ANSI-COMMENT WITH NO ADVANCING
                   DISPLAY "Logging out..."
                   MOVE "Y" TO WS-EXIT-MENU
               WHEN "5"
                   MOVE "Y" TO WS-EXIT-MENU
                   MOVE "Y" TO WS-EXIT-APP
               WHEN "6"
                   CALL "BANKING-CONSOLE"
                       USING WS-EMPLOYEE-RECORD
                   END-CALL
               WHEN OTHER
                   PERFORM DISPLAY-LEFT-PADDING
                   DISPLAY ANSI-LITERAL WITH NO ADVANCING
                   DISPLAY "Invalid option. Choose 1-6."
           END-EVALUATE.

       SHOW-PAY-DETAILS.
           PERFORM CALCULATE-PAYROLL-FIELDS
           PERFORM CLEAR-SCREEN
           MOVE 60 TO WS-BLOCK-WIDTH
           MOVE 18 TO WS-BLOCK-HEIGHT
           PERFORM PREPARE-CENTER-BLOCK
           PERFORM DISPLAY-TOP-PADDING
           MOVE "---- Pay Details ----" TO WS-SCREEN-TITLE
           PERFORM RENDER-HEADER

           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-IDENT WITH NO ADVANCING
           DISPLAY "Employee Name      : "
               FUNCTION TRIM(EMP-FULL-NAME)

           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY "Employee ID        : " EMP-ID

           MOVE EMP-HOURLY-RATE TO WS-AMOUNT-DISPLAY
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "Hourly Rate        : $" WS-AMOUNT-DISPLAY

           MOVE EMP-HOURS-PERIOD TO WS-HOURS-DISPLAY
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "Hours This Period  : " WS-HOURS-DISPLAY

           MOVE WS-GROSS-PAY TO WS-AMOUNT-DISPLAY
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "Gross Pay          : $" WS-AMOUNT-DISPLAY

           MOVE WS-TAX-AMOUNT TO WS-AMOUNT-DISPLAY
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "Taxes              : $" WS-AMOUNT-DISPLAY

           MOVE WS-NET-PAY TO WS-AMOUNT-DISPLAY
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "Net Pay            : $" WS-AMOUNT-DISPLAY

           MOVE EMP-YTD-WAGES TO WS-AMOUNT-DISPLAY
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "YTD Wages          : $" WS-AMOUNT-DISPLAY

           PERFORM PROMPT-NEXT-OPTION.

       SHOW-TAX-BREAKDOWN.
           PERFORM CALCULATE-PAYROLL-FIELDS
           PERFORM CLEAR-SCREEN
           MOVE 60 TO WS-BLOCK-WIDTH
           MOVE 16 TO WS-BLOCK-HEIGHT
           PERFORM PREPARE-CENTER-BLOCK
           PERFORM DISPLAY-TOP-PADDING
           MOVE "---- Tax Breakdown ----" TO WS-SCREEN-TITLE
           PERFORM RENDER-HEADER

           COMPUTE WS-BRACKET-PERCENT ROUNDED = WS-BRACKET-RATE * 100
           MOVE WS-BRACKET-PERCENT TO WS-RATE-DISPLAY

           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-NUMBER WITH NO ADVANCING
           DISPLAY "Bracket Rate       : " WS-RATE-DISPLAY "%"

           MOVE WS-EFFECTIVE-TAX-RATE TO WS-EFFECTIVE-PERCENT-DISPLAY
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "Effective Tax Rate : " WS-EFFECTIVE-PERCENT-DISPLAY "%"

           MOVE WS-GROSS-PAY TO WS-AMOUNT-DISPLAY
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "Gross Pay          : $" WS-AMOUNT-DISPLAY

           MOVE WS-TAX-AMOUNT TO WS-AMOUNT-DISPLAY
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "Tax Amount         : $" WS-AMOUNT-DISPLAY

           MOVE WS-NET-PAY TO WS-AMOUNT-DISPLAY
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY "Net Pay            : $" WS-AMOUNT-DISPLAY

           PERFORM PROMPT-NEXT-OPTION.

       SHOW-ASCII-CHART.
           PERFORM CALCULATE-PAYROLL-FIELDS
           PERFORM CLEAR-SCREEN
           MOVE 66 TO WS-BLOCK-WIDTH
           MOVE 16 TO WS-BLOCK-HEIGHT
           PERFORM PREPARE-CENTER-BLOCK
           PERFORM DISPLAY-TOP-PADDING
           MOVE "---- Payroll Chart ----" TO WS-SCREEN-TITLE
           PERFORM RENDER-HEADER

           CALL "RENDER-PAYROLL-CHART"
               USING WS-PAYROLL-CALC
                     WS-LEFT-PAD
           END-CALL

           PERFORM PROMPT-NEXT-OPTION.

       PROMPT-NEXT-OPTION.
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY " "
           PERFORM DISPLAY-LEFT-PADDING
           DISPLAY ANSI-KEYWORD WITH NO ADVANCING
           DISPLAY "Please select an option : " WITH NO ADVANCING
           DISPLAY ANSI-INPUT WITH NO ADVANCING
           DISPLAY "__ " WITH NO ADVANCING
           DISPLAY ANSI-BASE WITH NO ADVANCING
           ACCEPT WS-MENU-OPTION

           IF WS-MENU-OPTION = "M" OR WS-MENU-OPTION = "m"
               MOVE "N" TO WS-HAVE-PENDING-OPTION
           ELSE
               MOVE "Y" TO WS-HAVE-PENDING-OPTION
           END-IF.

       CALCULATE-PAYROLL-FIELDS.
           CALL "CALCULATE-PAYROLL"
               USING WS-EMPLOYEE-RECORD
                     WS-PAYROLL-CALC
                     WS-BRACKET-RATE
           END-CALL.
