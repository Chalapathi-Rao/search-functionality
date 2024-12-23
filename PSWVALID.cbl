       IDENTIFICATION DIVISION.
       PROGRAM-ID. PasswordValidator.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PASSWORD           PIC X(20).
       01 PASSWORD-LENGTH    PIC 9(2) VALUE ZERO.
       01 UPPER-CASE-FLAG    PIC X VALUE 'N'.
       01 DIGIT-FLAG         PIC X VALUE 'N'.
       01 WS-VALID           PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       DISPLAY "Enter your password: "
       ACCEPT PASSWORD.

       * Get the length of the password
       UNSTRING PASSWORD DELIMITED BY SPACE INTO PASSWORD.
       COMPUTE PASSWORD-LENGTH = FUNCTION LENGTH(PASSWORD).

       * Check if the password length is at least 8 characters
       IF PASSWORD-LENGTH < 8
           DISPLAY "Password must be at least 8 characters long."
           STOP RUN
       END-IF.

       * Check for at least one uppercase letter
       PERFORM CHECK-UPPERCASE.

       * Check for at least one digit
       PERFORM CHECK-DIGIT.

       * If both conditions are met, it's a valid password
       IF UPPER-CASE-FLAG = 'Y' AND DIGIT-FLAG = 'Y'
           DISPLAY "Password is valid."
       ELSE
           DISPLAY "Password must contain at least one uppercase letter and one digit."
       END-IF.

       STOP RUN.

       * Subroutine to check for an uppercase letter
       CHECK-UPPERCASE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PASSWORD-LENGTH
               IF FUNCTION NUMVAL(PASSWORD(I:1)) = 0 AND
                  PASSWORD(I:1) = FUNCTION UPPERCASE(PASSWORD(I:1))
                   MOVE 'Y' TO UPPER-CASE-FLAG
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       * Subroutine to check for a digit
       CHECK-DIGIT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PASSWORD-LENGTH
               IF FUNCTION NUMVAL(PASSWORD(I:1)) > 0
                   MOVE 'Y' TO DIGIT-FLAG
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       *******
///////
