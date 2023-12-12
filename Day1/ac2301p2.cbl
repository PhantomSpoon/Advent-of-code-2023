      ****************************************************************
      *            IDENTIFICAITON DIVISION                         ***
      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Ac2301p2.
      ****************************************************************
      *            ENVIRONMENT DIIVISION                           ***
      ****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WS-INPUT1  ASSIGN TO INPUT01.
      ****************************************************************
      **           DATA DIVISION                                   ***
      ****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD WS-INPUT1.
       01 WS-INPUT1-POST    PIC X(80).
       WORKING-STORAGE SECTION.
      * WS-VARIABLES
       01 WS-VARIABLES.
          05 WS-COUNTER-CHAR   PIC 999.
          05 WS-FIRST-DIGIT    PIC 9.
          05 WS-SECOND-DIGIT   PIC 9.
          05 WS-DIGITS-COMB    PIC XX.
          05 WS-ADD-SUM REDEFINES WS-DIGITS-COMB PIC 99.
          05 WS-TOTAL-SUM      PIC 99999.
          05 WS-REV-INPUT      PIC X(80).
      * SWITCHES.
       01 SWITCHES.
          05 SWITCH-EOF          PIC X.
             88 EOF-Y            VALUE 'Y'.
             88 NOT-EOF          VALUE 'N'.
          05 FIRST-DIGIT         PIC X.
             88 FIRST-FOUND      VALUE 'Y'.
             88 FIRST-NONE       VALUE 'N'.
          05 SECOND-DIGIT        PIC X.
             88 SECOND-FOUND     VALUE 'Y'.
             88 SECOND-NONE      VALUE 'Y'.
      * INPUT AREA
       01 I-INPUT-AREA         PIC X(80).
      ****************************************************************
      **           PROCEDURE DIVISION                              ***
      ****************************************************************
       PROCEDURE DIVISION.
       A-MAIN SECTION.
           PERFORM B-INIT
           PERFORM C-PROCESS
           PERFORM Z-CLOSE
           .
           GOBACK.

       B-INIT SECTION.
           INITIALIZE WS-VARIABLES
           OPEN INPUT  WS-INPUT1
           .

       C-PROCESS SECTION.
           PERFORM R-READ
           PERFORM UNTIL EOF-Y
              PERFORM C-REPLACE
              PERFORM C-REPLACE
              PERFORM C-FIRST
              PERFORM C-SECOND
              PERFORM R-READ
              MOVE WS-FIRST-DIGIT  TO WS-DIGITS-COMB(1:1)
              MOVE WS-SECOND-DIGIT TO WS-DIGITS-COMB(2:1)
              ADD WS-ADD-SUM TO WS-TOTAL-SUM
           END-PERFORM
           .

       C-FIRST SECTION.
           MOVE 1 TO WS-COUNTER-CHAR
           PERFORM UNTIL FIRST-FOUND
              IF I-INPUT-AREA(WS-COUNTER-CHAR:1) IS NUMERIC
                 MOVE I-INPUT-AREA(WS-COUNTER-CHAR:1) TO WS-FIRST-DIGIT
                 SET FIRST-FOUND TO TRUE
              ELSE
                 ADD 1 TO WS-COUNTER-CHAR
              END-IF
           END-PERFORM
           .

       C-SECOND SECTION.
           MOVE 1 TO WS-COUNTER-CHAR
           MOVE FUNCTION REVERSE(I-INPUT-AREA) TO WS-REV-INPUT
           PERFORM UNTIL SECOND-FOUND
              IF WS-REV-INPUT(WS-COUNTER-CHAR:1) IS NUMERIC
                 MOVE WS-REV-INPUT(WS-COUNTER-CHAR:1) TO WS-SECOND-DIGIT
                 SET SECOND-FOUND TO TRUE
              ELSE
                 ADD 1 TO WS-COUNTER-CHAR
              END-IF
           END-PERFORM
           .

       C-REPLACE SECTION.
           INSPECT I-INPUT-AREA REPLACING
                   ALL 'one'   BY '1ne',
                   ALL 'two'   BY '2wo',
                   ALL 'three' BY '3hree',
                   ALL 'four'  BY '4our',
                   ALL 'five'  BY '5ive',
                   ALL 'six'   BY '6ix',
                   ALL 'seven' BY '7even',
                   ALL 'eight' BY '8ight',
                   ALL 'nine'  BY '9ine'
           .
       R-READ SECTION.
           INITIALIZE I-INPUT-AREA
                      FIRST-DIGIT
                      SECOND-DIGIT
           READ WS-INPUT1 INTO I-INPUT-AREA
           AT END
             SET EOF-Y TO TRUE
           NOT AT END
             CONTINUE
           END-READ
           .

       Z-CLOSE  SECTION.
           CLOSE WS-INPUT1
           DISPLAY 'TOTAL SUMMA: ' WS-TOTAL-SUM
           .
