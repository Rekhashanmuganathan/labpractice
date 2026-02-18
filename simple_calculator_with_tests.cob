* Simple interactive calculator in COBOL with testing options
       * Works with GnuCOBOL / OpenCOBOL and modern COBOL compilers
       * Compile (GnuCOBOL): cobc -x simple_calculator_with_tests.cob
       * Run: ./simple_calculator_with_tests

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-CALCULATOR.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FIRST-STR       PIC X(30) VALUE SPACES.
       01  WS-SECOND-STR      PIC X(30) VALUE SPACES.
       01  WS-FIRST-NUM       PIC S9(9)V9(9) VALUE 0.
       01  WS-SECOND-NUM      PIC S9(9)V9(9) VALUE 0.
       01  WS-RESULT          PIC S9(18)V9(9) VALUE 0.
       01  WS-OPERATOR        PIC X        VALUE SPACE.
       01  WS-ANSWER          PIC X        VALUE "Y".
       01  WS-ZERO-FLAG       PIC X        VALUE "N".
       01  WS-MODE            PIC X        VALUE SPACE.
       01  WS-TRIMMED         PIC X(30)    VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-LOOP.
           DISPLAY "Choose mode: (I)nteractive or (T)est: " WITH NO ADVANCING
           ACCEPT WS-MODE
           IF WS-MODE = "T" OR WS-MODE = "t"
               PERFORM TEST-MODE
               PERFORM END-PROGRAM
           END-IF

           PERFORM UNTIL WS-ANSWER = "N" OR WS-ANSWER = "n"
               DISPLAY ""
               DISPLAY "Simple COBOL Calculator"
               DISPLAY "------------------------"
               DISPLAY "Enter first number (eg 12.34): " WITH NO ADVANCING
               ACCEPT WS-FIRST-STR
               IF FUNCTION TRIM(WS-FIRST-STR) = "Q" OR FUNCTION TRIM(WS-FIRST-STR) = "q"
                   PERFORM END-PROGRAM
               END-IF
               MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-FIRST-STR)) TO WS-FIRST-NUM

               DISPLAY "Enter operator (+  -  *  /): " WITH NO ADVANCING
               ACCEPT WS-OPERATOR

               DISPLAY "Enter second number (eg 5.0): " WITH NO ADVANCING
               ACCEPT WS-SECOND-STR
               IF FUNCTION TRIM(WS-SECOND-STR) = "Q" OR FUNCTION TRIM(WS-SECOND-STR) = "q"
                   PERFORM END-PROGRAM
               END-IF
               MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-SECOND-STR)) TO WS-SECOND-NUM

               *> Reset flags
               MOVE "N" TO WS-ZERO-FLAG

               EVALUATE WS-OPERATOR
                   WHEN "+" 
                       COMPUTE WS-RESULT = WS-FIRST-NUM + WS-SECOND-NUM
                   WHEN "-" 
                       COMPUTE WS-RESULT = WS-FIRST-NUM - WS-SECOND-NUM
                   WHEN "*" OR "x" OR "X"
                       COMPUTE WS-RESULT = WS-FIRST-NUM * WS-SECOND-NUM
                   WHEN "/" 
                       IF WS-SECOND-NUM = 0
                           MOVE "Y" TO WS-ZERO-FLAG
                       ELSE
                           COMPUTE WS-RESULT = WS-FIRST-NUM / WS-SECOND-NUM
                       END-IF
                   WHEN OTHER
                       DISPLAY "Error: Unsupported operator '" WS-OPERATOR "'"
                       CONTINUE
               END-EVALUATE

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
           DISPLAY "Running test cases..."
           *> Test Case 1: Addition
           MOVE 25.5 TO WS-FIRST-NUM
           MOVE 10.3 TO WS-SECOND-NUM
           MOVE "+" TO WS-OPERATOR
           PERFORM CALCULATE
           DISPLAY "Test 1 (Addition): Result = " WS-RESULT

           *> Test Case 2: Subtraction
           MOVE 35.8 TO WS-FIRST-NUM
           MOVE 12.5 TO WS-SECOND-NUM
           MOVE "-" TO WS-OPERATOR
           PERFORM CALCULATE
           DISPLAY "Test 2 (Subtraction): Result = " WS-RESULT

           *> Test Case 3: Multiplication
           MOVE 6.5 TO WS-FIRST-NUM
           MOVE 4 TO WS-SECOND-NUM
           MOVE "*" TO WS-OPERATOR
           PERFORM CALCULATE
           DISPLAY "Test 3 (Multiplication): Result = " WS-RESULT

           *> Test Case 4: Division
           MOVE 20 TO WS-FIRST-NUM
           MOVE 4 TO WS-SECOND-NUM
           MOVE "/" TO WS-OPERATOR
           PERFORM CALCULATE
           DISPLAY "Test 4 (Division): Result = " WS-RESULT

           *> Test Case 5: Division by zero
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
           MOVE "N" TO WS-ZERO-FLAG
           EVALUATE WS-OPERATOR
               WHEN "+" 
                   COMPUTE WS-RESULT = WS-FIRST-NUM + WS-SECOND-NUM
               WHEN "-" 
                   COMPUTE WS-RESULT = WS-FIRST-NUM - WS-SECOND-NUM
               WHEN "*" OR "x" OR "X"
                   COMPUTE WS-RESULT = WS-FIRST-NUM * WS-SECOND-NUM
               WHEN "/" 
                   IF WS-SECOND-NUM = 0
                       MOVE "Y" TO WS-ZERO-FLAG
                   ELSE
                       COMPUTE WS-RESULT = WS-FIRST-NUM / WS-SECOND-NUM
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       END-PROGRAM.
           STOP RUN.