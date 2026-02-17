IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDIT-CARD-APPROVAL.
       * This program evaluates credit card applications based on age, income, and credit score.

       ENVIRONMENT DIVISION.
       * Environment division is empty as no external files are used for input.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       * Declare variables to store applicant details and thresholds for approval.
       01  APPLICANT-DETAILS.
           05  APPLICANT-NAME       PIC X(30).  * Name of the applicant.
           05  APPLICANT-AGE        PIC 99.     * Age of the applicant.
           05  APPLICANT-INCOME     PIC 9(7)V99. * Monthly income of the applicant.
           05  APPLICANT-CREDIT-SCORE PIC 999.  * Credit score of the applicant.

       01  APPROVAL-STATUS         PIC X(8) VALUE SPACES. * Stores the approval status (APPROVED/REJECTED).
       01  WS-INCOME-THRESHOLD     PIC 9(7)V99 VALUE 30000.00. * Minimum income required for approval.
       01  WS-CREDIT-SCORE-THRESHOLD PIC 999 VALUE 700. * Minimum credit score required for approval.
       01  WS-AGE-MINIMUM          PIC 99 VALUE 21. * Minimum age required for approval.
       01  WS-AGE-MAXIMUM          PIC 99 VALUE 65. * Maximum age allowed for approval.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           * Prompt the user for applicant details and store them in variables.
           DISPLAY "Enter Applicant Name: " WITH NO ADVANCING
           ACCEPT APPLICANT-NAME

           DISPLAY "Enter Applicant Age: " WITH NO ADVANCING
           ACCEPT APPLICANT-AGE

           DISPLAY "Enter Applicant Monthly Income: " WITH NO ADVANCING
           ACCEPT APPLICANT-INCOME

           DISPLAY "Enter Applicant Credit Score: " WITH NO ADVANCING
           ACCEPT APPLICANT-CREDIT-SCORE

           * Perform the approval check based on the entered details.
           PERFORM CHECK-APPROVAL

           * Display the final approval status to the user.
           DISPLAY "Approval Status: " APPROVAL-STATUS

           * End the program.
           STOP RUN.

       CHECK-APPROVAL.
           * Check if the applicant's age is within the allowed range.
           IF APPLICANT-AGE < WS-AGE-MINIMUM OR APPLICANT-AGE > WS-AGE-MAXIMUM
               MOVE "REJECTED" TO APPROVAL-STATUS
           ELSE
               * Check if the applicant's income meets the minimum threshold.
               IF APPLICANT-INCOME < WS-INCOME-THRESHOLD
                   MOVE "REJECTED" TO APPROVAL-STATUS
               ELSE
                   * Check if the applicant's credit score meets the minimum threshold.
                   IF APPLICANT-CREDIT-SCORE < WS-CREDIT-SCORE-THRESHOLD
                       MOVE "REJECTED" TO APPROVAL-STATUS
                   ELSE
                       * Approve the application if all conditions are met.
                       MOVE "APPROVED" TO APPROVAL-STATUS
                   END-IF
               END-IF
           END-IF.