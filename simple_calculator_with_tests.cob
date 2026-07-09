      *>============================================================
      *> PROGRAM: SIMPLE-CALCULATOR
      *> PURPOSE: An interactive calculator program with automated
      *>          testing capabilities. Supports basic arithmetic
      *>          operations: addition, subtraction, multiplication,
      *>          and division with error handling.
      *> 
      *> COMPATIBILITY: GnuCOBOL / OpenCOBOL and modern COBOL
      *> COMPILE: cobc -x simple_calculator_with_tests.cob
      *> RUN: ./simple_calculator_with_tests
      *> 
      *> FEATURES:
      *>   - Interactive mode for user-driven calculations
      *>   - Automated test mode for validation
      *>   - Division by zero detection and handling
      *>   - Support for decimal numbers
      *>   - Exit option (press 'Q')
      *>============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-CALCULATOR.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *>============================================================
      *> DATA VARIABLES DOCUMENTATION
      *>============================================================
       
      *> Input Variables - Store user input as strings
       01  WS-FIRST-STR       PIC X(30) VALUE SPACES.
           *> First operand entered by user (string format)
       01  WS-SECOND-STR      PIC X(30) VALUE SPACES.
           *> Second operand entered by user (string format)
       
      *> Numeric Variables - Store converted numbers for calculation
       01  WS-FIRST-NUM       PIC S9(9)V9(9) VALUE 0.
           *> First operand as numeric value (up to 9 digits, 9 decimals)
       01  WS-SECOND-NUM      PIC S9(9)V9(9) VALUE 0.
           *> Second operand as numeric value (up to 9 digits, 9 decimals)
       01  WS-RESULT          PIC S9(18)V9(9) VALUE 0.
           *> Result of calculation (up to 18 digits, 9 decimals)
       
      *> Operator and Control Variables
       01  WS-OPERATOR        PIC X        VALUE SPACE.
           *> Arithmetic operator: +, -, *, or /
       01  WS-ANSWER          PIC X        VALUE "Y".
           *> User response to continue: Y/N
       01  WS-ZERO-FLAG       PIC X        VALUE "N".
           *> Error flag for division by zero detection: Y/N
       01  WS-MODE            PIC X        VALUE SPACE.
           *> Program mode selection: I (Interactive) or T (Test)
       01  WS-TRIMMED         PIC X(30)    VALUE SPACES.
           *> Variable for trimmed string operations

       PROCEDURE DIVISION.
      *>============================================================
      *> MAIN-LOOP: Entry point of the program
      *> 
      *> FUNCTION: 
      *>   - Displays mode selection menu (Interactive or Test)
      *>   - Routes execution based on user's choice
      *>   - If Test mode: runs automated test cases, then exits
      *>   - If Interactive mode: enters main calculator loop
      *>
      *> LOGIC:
      *>   1. Ask user to select mode (I or T)
      *>   2. If 'T', perform test mode then exit
      *>   3. Otherwise, enter interactive calculation loop until
      *>      user chooses to exit (N)
      *>============================================================
       MAIN-LOOP.
           DISPLAY "Choose mode: (I)nteractive or (T)est: " WITH NO ADVANCING
           ACCEPT WS-MODE
           IF WS-MODE = "T" OR WS-MODE = "t"
               PERFORM TEST-MODE
               PERFORM END-PROGRAM
           END-IF

      *> Interactive mode loop - continues until user enters 'N'
           PERFORM UNTIL WS-ANSWER = "N" OR WS-ANSWER = "n"
               DISPLAY ""
               DISPLAY "Simple COBOL Calculator"
               DISPLAY "------------------------"
               
      *>    Prompt and accept first number (supports decimals)
               DISPLAY "Enter first number (eg 12.34): " WITH NO ADVANCING
               ACCEPT WS-FIRST-STR
               
      *>    Allow user to quit by pressing 'Q'
               IF FUNCTION TRIM(WS-FIRST-STR) = "Q" OR FUNCTION TRIM(WS-FIRST-STR) = "q"
                   PERFORM END-PROGRAM
               END-IF
               
      *>    Convert string input to numeric value
               MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-FIRST-STR)) TO WS-FIRST-NUM

      *>    Prompt and accept operator
               DISPLAY "Enter operator (+  -  *  /): " WITH NO ADVANCING
               ACCEPT WS-OPERATOR

      *>    Prompt and accept second number
               DISPLAY "Enter second number (eg 5.0): " WITH NO ADVANCING
               ACCEPT WS-SECOND-STR
               
      *>    Allow user to quit by pressing 'Q'
               IF FUNCTION TRIM(WS-SECOND-STR) = "Q" OR FUNCTION TRIM(WS-SECOND-STR) = "q"
                   PERFORM END-PROGRAM
               END-IF
               
      *>    Convert string input to numeric value
               MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-SECOND-STR)) TO WS-SECOND-NUM

      *>    Reset division-by-zero flag before calculation
               MOVE "N" TO WS-ZERO-FLAG

      *>    Evaluate operator and perform appropriate calculation
               EVALUATE WS-OPERATOR
                   WHEN "+" 
                       COMPUTE WS-RESULT = WS-FIRST-NUM + WS-SECOND-NUM
                   WHEN "-" 
                       COMPUTE WS-RESULT = WS-FIRST-NUM - WS-SECOND-NUM
                   WHEN "*" OR "x" OR "X"
                       COMPUTE WS-RESULT = WS-FIRST-NUM * WS-SECOND-NUM
                   WHEN "/" 
      *>            Check for division by zero before dividing
                       IF WS-SECOND-NUM = 0
                           MOVE "Y" TO WS-ZERO-FLAG
                       ELSE
                           COMPUTE WS-RESULT = WS-FIRST-NUM / WS-SECOND-NUM
                       END-IF
                   WHEN OTHER
      *>            Handle invalid operator input
                       DISPLAY "Error: Unsupported operator '" WS-OPERATOR "'"
                       CONTINUE
               END-EVALUATE

      *>    Display result or error message
               IF WS-ZERO-FLAG = "Y"
                   DISPLAY "Error: Division by zero"
               ELSE
                   *> Display result. Numeric fields will be shown in default numeric format.
                   DISPLAY "Result: " WS-RESULT
               END-IF

               DISPLAY "Simple Calculator Program"
               DISPLAY "Perform another calculation? (Y/N): " WITH NO ADVANCING
               ACCEPT WS-ANSWER
               IF WS-ANSWER = SPACE
                   MOVE "N" TO WS-ANSWER
               END-IF
           END-PERFORM.

       TEST-MODE.
      *>============================================================
      *> TEST-MODE: Automated test suite
      *>
      *> FUNCTION:
      *>   - Executes pre-defined test cases for all operations
      *>   - Validates correct behavior including error handling
      *>   - Displays results with pass/fail status
      *>
      *> TEST CASES:
      *>   Test 1: Addition (25.5 + 10.3 = 35.8)
      *>   Test 2: Subtraction (35.8 - 12.5 = 23.3)
      *>   Test 3: Multiplication (6.5 * 4 = 26)
      *>   Test 4: Division (20 / 4 = 5)
      *>   Test 5: Division by zero (10 / 0 = error)
      *>============================================================
           DISPLAY "Running test cases..."
           
      *>    Test Case 1: Addition
           MOVE 25.5 TO WS-FIRST-NUM
           MOVE 10.3 TO WS-SECOND-NUM
           MOVE "+" TO WS-OPERATOR
           PERFORM CALCULATE
           DISPLAY "Test 1 (Addition): Result = " WS-RESULT

      *>    Test Case 2: Subtraction
           MOVE 35.8 TO WS-FIRST-NUM
           MOVE 12.5 TO WS-SECOND-NUM
           MOVE "-" TO WS-OPERATOR
           PERFORM CALCULATE
           DISPLAY "Test 2 (Subtraction): Result = " WS-RESULT

      *>    Test Case 3: Multiplication
           MOVE 6.5 TO WS-FIRST-NUM
           MOVE 4 TO WS-SECOND-NUM
           MOVE "*" TO WS-OPERATOR
           PERFORM CALCULATE
           DISPLAY "Test 3 (Multiplication): Result = " WS-RESULT

      *>    Test Case 4: Division
           MOVE 20 TO WS-FIRST-NUM
           MOVE 4 TO WS-SECOND-NUM
           MOVE "/" TO WS-OPERATOR
           PERFORM CALCULATE
           DISPLAY "Test 4 (Division): Result = " WS-RESULT

      *>    Test Case 5: Division by zero (error handling test)
           MOVE 10 TO WS-FIRST-NUM
           MOVE 0 TO WS-SECOND-NUM
           MOVE "/" TO WS-OPERATOR
           PERFORM CALCULATE
           DISPLAY "Test 5 (Division by zero): " 
           IF WS-ZERO-FLAG = "Y"
               DISPLAY "Passed (Division by zero handled correctly)"
           ELSE
               DISPLAY "Failed (Division by zero not detected)"
           END-IF.

       CALCULATE.
      *>============================================================
      *> CALCULATE: Performs arithmetic operation
      *>
      *> INPUT:
      *>   - WS-FIRST-NUM: First operand (numeric)
      *>   - WS-SECOND-NUM: Second operand (numeric)
      *>   - WS-OPERATOR: Operation to perform (+, -, *, /)
      *>
      *> OUTPUT:
      *>   - WS-RESULT: Result of the calculation
      *>   - WS-ZERO-FLAG: Set to "Y" if division by zero detected
      *>
      *> LOGIC:
      *>   1. Reset zero-flag flag
      *>   2. Evaluate operator
      *>   3. Perform appropriate COMPUTE operation
      *>   4. For division, check for zero divisor and set flag if needed
      *>============================================================
           MOVE "N" TO WS-ZERO-FLAG
           EVALUATE WS-OPERATOR
               WHEN "+" 
                   COMPUTE WS-RESULT = WS-FIRST-NUM + WS-SECOND-NUM
               WHEN "-" 
                   COMPUTE WS-RESULT = WS-FIRST-NUM - WS-SECOND-NUM
               WHEN "*" OR "x" OR "X"
                   COMPUTE WS-RESULT = WS-FIRST-NUM * WS-SECOND-NUM
               WHEN "/" 
      *>            Prevent division by zero
                   IF WS-SECOND-NUM = 0
                       MOVE "Y" TO WS-ZERO-FLAG
                   ELSE
                       COMPUTE WS-RESULT = WS-FIRST-NUM / WS-SECOND-NUM
                   END-IF
               WHEN OTHER
      *>            Unknown operator - no action needed
                   CONTINUE
           END-EVALUATE.

       END-PROGRAM.
      *>============================================================
      *> END-PROGRAM: Cleanup and program termination
      *>
      *> FUNCTION:
      *>   - Displays farewell message
      *>   - Terminates the program gracefully
      *>============================================================
           Display "End of program". 
           STOP RUN.