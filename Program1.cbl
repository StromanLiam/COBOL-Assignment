       IDENTIFICATION DIVISION.
       program-id. Program1 as "AssignmentOne.Program1".
       AUTHOR. LIAM STROMAN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F01-GRADES-FILE ASSIGN TO 'CodingAsst.dat'
                                   ORGANIZATION IS LINE sequential.
           SELECT F02-REPORT-FILE ASSIGN TO 'Report.dat'
                                   ORGANIZATION IS LINE sequential.

       DATA DIVISION.
       FILE SECTION.
      *input file definition
       FD F01-GRADES-FILE
           RECORD CONTAINS 53 CHARACTERS
      * actual number is very important
           DATA RECORD IS F01-GRADES-RECORD.

       01 F01-GRADES-RECORD.
         05 F01-ID PIC 9(5).
         05 F01-COURSE1 PIC X(7).
         05 F01-GRADE1 PIC X(1).
         05 F01-COURSE2 PIC X(7).
         05 F01-GRADE2 PIC X(1).
         05 F01-COURSE3 PIC X(7).
         05 F01-GRADE3 PIC X(1).
         05 F01-COURSE4 PIC X(7).
         05 F01-GRADE4 PIC X(1).
         05 F01-COURSE5 PIC X(7).
         05 F01-GRADE5 PIC X(1).
         05 F01-COURSE6 PIC X(7).
         05 F01-GRADE6 PIC X(1).

      *output file definition
       FD F02-REPORT-FILE
           RECORD CONTAINS 60 CHARACTERS
           DATA RECORD IS F02-REPORT-RECORD.
       01 F02-REPORT-RECORD.
         05 F02-OUTPUT-LINE PIC X(60).

       WORKING-STORAGE SECTION.
       01 W01-TITLE-LINE.
         05 PIC X(9) VALUE SPACES.
         05 PIC X(37) VALUE 'UNIVERSITY OF NOWHERE BY STUDENT NAME'.

       01 W01-SUBTITLE-LINE.
         05 PIC X(10) VALUE SPACES.
         05 PIC X(29) VALUE 'STUDENT CURRICULUM EVALUATION'.

       01 W01-HEADING-LINE-ONE.
         05 PIC X(10) VALUE 'STUDENT ID'.
         05 PIC X(20) VALUE SPACES.
         05 PIC X(21) VALUE 'PERCENTAGE OF COURSES'.

       01 W01-HEADING-LINE-TWO.
         05 PIC X(2) VALUE SPACES.
         05 PIC X(6) VALUE 'NUMBER'.
         05 PIC X(4) VALUE SPACES.
         05 PIC X(9) VALUE 'COMPLETED'.
         05 PIC X(3) VALUE SPACES.
         05 PIC X(9) VALUE 'REMAINING'.
         05 PIC X(3) VALUE SPACES.
         05 PIC X(11) VALUE 'TRANSFERRED'.
         05 PIC X(1) VALUE SPACES.
         05 PIC X(11) VALUE 'PROFICIENCY'.

       01 W02-COURSE PIC X(7).
       01 W02-GRADE PIC X(1).
       01 W02-NUMBER-COURSES PIC 999.
       01 W02-NUMBER-CREDITS PIC 999.
       01 W02-REMAINING PIC 999.
       01 W02-TRANSFERRED PIC 999.
       01 W02-PROFICIENCY PIC 999.

       01 W01-DATA-REMAINS-SWITCH PIC X(2) VALUE spaces.

       PROCEDURE DIVISION.
      *main
           PERFORM 100-OPEN-FILES
           PERFORM 200-PRINT-HEADINGS
           PERFORM 300-PROCESS-RECORDS
             UNTIL W01-DATA-REMAINS-SWITCH = 'NO'
           PERFORM 400-CLOSE-FILES
           STOP RUN.
      

      *100 open files paragraph

       100-OPEN-FILES.
           OPEN INPUT F01-GRADES-FILE
           OPEN OUTPUT F02-REPORT-FILE.

      *200 print headings paragraph

       200-PRINT-HEADINGS.
           MOVE W01-TITLE-LINE TO F02-OUTPUT-LINE
           WRITE F02-REPORT-RECORD

           MOVE W01-SUBTITLE-LINE TO F02-OUTPUT-LINE
           WRITE F02-REPORT-RECORD

           MOVE SPACES TO F02-OUTPUT-LINE
           WRITE F02-REPORT-RECORD

           MOVE W01-HEADING-LINE-ONE TO F02-OUTPUT-LINE
           WRITE F02-REPORT-RECORD

           MOVE W01-HEADING-LINE-TWO TO F02-OUTPUT-LINE
           WRITE F02-REPORT-RECORD

           MOVE SPACES TO F02-OUTPUT-LINE
           WRITE F02-REPORT-RECORD
           .

      *300 process records paragraph
      *read in a line, call check-all-grades, call calculate-percentages, WRITE to output then call clear-totals
      *in main, this function will be called in some sort of loop.

       300-PROCESS-RECORDS.
           READ F01-GRADES-FILE
               AT END MOVE 'NO' TO W01-DATA-REMAINS-SWITCH
           END-READ

           PERFORM 310-CHECK-ALL-GRADES
           PERFORM 320-CALCULATE-PERCENTAGES

      *MOVE TO OUTPUT

           STRING "  " DELIMITED BY SIZE
               F01-ID DELIMITED BY SIZE
               "         " DELIMITED BY SIZE
               W02-NUMBER-CREDITS DELIMITED BY SIZE
               "         " DELIMITED BY SIZE
                W02-REMAINING DELIMITED BY SIZE
                "          " DELIMITED BY SIZE
                W02-TRANSFERRED DELIMITED BY SIZE
                "         " DELIMITED BY SIZE
                W02-PROFICIENCY DELIMITED BY SIZE
                "    " DELIMITED BY SIZE
            INTO F02-REPORT-RECORD
           END-STRING

      *WRITE TO FILE

           IF W01-DATA-REMAINS-SWITCH IS NOT EQUAL TO 'NO'
               WRITE F02-REPORT-RECORD
           END-IF

           PERFORM 330-CLEAR-TOTALS
           .

      *310 check all grades paragraph

       310-CHECK-ALL-GRADES.
           MOVE F01-COURSE1 TO W02-COURSE
           MOVE F01-GRADE1 TO W02-GRADE
           PERFORM 312-CHECK-ONE-GRADE

           MOVE F01-COURSE2 TO W02-COURSE
           MOVE F01-GRADE2 TO W02-GRADE
           PERFORM 312-CHECK-ONE-GRADE

           MOVE F01-COURSE3 TO W02-COURSE
           MOVE F01-GRADE3 TO W02-GRADE
           PERFORM 312-CHECK-ONE-GRADE

           MOVE F01-COURSE4 TO W02-COURSE
           MOVE F01-GRADE4 TO W02-GRADE
           PERFORM 312-CHECK-ONE-GRADE

           MOVE F01-COURSE5 TO W02-COURSE
           MOVE F01-GRADE5 TO W02-GRADE
           PERFORM 312-CHECK-ONE-GRADE

           MOVE F01-COURSE6 TO W02-COURSE
           MOVE F01-GRADE6 TO W02-GRADE
           PERFORM 312-CHECK-ONE-GRADE.

      *312 check one grade paragraph
       312-CHECK-ONE-GRADE.
           IF W02-COURSE IS NOT EQUAL TO "       "
               ADD 1 TO W02-NUMBER-COURSES
               IF W02-GRADE = "A" OR
                 W02-GRADE = "B" OR
                 W02-GRADE = "C" OR
                 W02-GRADE = "D" OR
                 W02-GRADE = "P" OR
                 W02-GRADE = "K"
                   ADD 1 TO W02-NUMBER-CREDITS

                       IF W02-GRADE = "K"
                           ADD 1 TO W02-TRANSFERRED
                       END-IF

                       IF W02-GRADE = "P"
                           ADD 1 TO W02-PROFICIENCY
                       END-IF

               END-IF
           END-IF.

      *320 calculate percentages paragraph

       320-CALCULATE-PERCENTAGES.
      * number credits becomes number credits remaining
           SUBTRACT W02-NUMBER-CREDITS FROM W02-NUMBER-COURSES GIVING W02-NUMBER-CREDITS
      * remaining stores the remaining credits as a percentage
           MULTIPLY W02-NUMBER-CREDITS BY 100 GIVING W02-NUMBER-CREDITS
           DIVIDE W02-NUMBER-CREDITS BY W02-NUMBER-COURSES GIVING W02-REMAINING ROUNDED
           
      *This makes number credits a percent

           SUBTRACT W02-REMAINING FROM 100 GIVING W02-NUMBER-CREDITS

      * this handles transferred courses
           MULTIPLY W02-TRANSFERRED BY 100 GIVING W02-TRANSFERRED
           DIVIDE W02-TRANSFERRED BY W02-NUMBER-COURSES GIVING W02-TRANSFERRED

      * this handles proficiency credits
           MULTIPLY W02-PROFICIENCY BY 100 GIVING W02-PROFICIENCY
           DIVIDE W02-PROFICIENCY BY W02-NUMBER-COURSES GIVING W02-PROFICIENCY
           .

      *330 clear totals paragraph
       330-CLEAR-TOTALS.
           MOVE 000 TO W02-NUMBER-CREDITS
           MOVE 000 TO W02-NUMBER-COURSES
           MOVE 000 TO W02-TRANSFERRED
           MOVE 000 TO W02-PROFICIENCY.

      *400 close files paragraph
       400-CLOSE-FILES.
           CLOSE F01-GRADES-FILE
             F02-REPORT-FILE.

      

       END PROGRAM Program1.
