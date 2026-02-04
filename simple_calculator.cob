       * Simple interactive calculator in COBOL (console)
       * Works with GnuCOBOL / OpenCOBOL and modern COBOL compilers
       * Compile (GnuCOBOL): cobc -x simple_calculator.cob
       * Run: ./simple_calculator

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
       01  WS-TRIMMED         PIC X(30)    VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-LOOP.
           PERFORM UNTIL WS-ANSWER = "N" OR WS-ANSWER = "n"
               DISPLAY ""
               DISPLAY "Simple COBOL Calculator"
               DISPLAY "------------------------"
               DISPLAY "Enter first number (eg 12.34): " WITH NO ADVANCING
               ACCEPT WS-FIRST-STR
               IF FUNCTION TRIM(WS-FIRST-STR) = "Q" OR FUNCTION TRIM(WS-FIRST-STR) = "q"
                   GO TO END-PROGRAM
               END-IF
               MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-FIRST-STR)) TO WS-FIRST-NUM

               DISPLAY "Enter operator (+  -  *  /): " WITH NO ADVANCING
               ACCEPT WS-OPERATOR

               DISPLAY "Enter second number (eg 5.0): " WITH NO ADVANCING
               ACCEPT WS-SECOND-STR
               IF FUNCTION TRIM(WS-SECOND-STR) = "Q" OR FUNCTION TRIM(WS-SECOND-STR) = "q"
                   GO TO END-PROGRAM
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

       END-PROGRAM.
           STOP RUN.