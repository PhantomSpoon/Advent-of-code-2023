      ****************************************************************
      *            IDENTIFICAITON DIVISION                         ***
      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AC2302P2.
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
       01 WS-INPUT1-POST    PIC X(160).
       WORKING-STORAGE SECTION.
      * WS-VARIABLES
       01 WS-VARIABLES.
          05 WS-POS            PIC 999.
          05 WS-CUR-RED        PIC 9(2).
          05 WS-CUR-GREEN      PIC 9(2).
          05 WS-CUR-BLUE       PIC 9(2).
          05 WS-AMOUNT         PIC 9(2).
          05 WS-TOTAL          PIC 9(5).
          05 WS-TEMP           PIC 9(5).
      * C-CONSTANTS
       01 C-CONSTANTS.
          05 C-MAX-RED         PIC 9(2) VALUE 12.
          05 C-MAX-GREEN       PIC 9(2) VALUE 13.
          05 C-MAX-BLUE        PIC 9(2) VALUE 14.
      * SWITCHES.
       01 SWITCHES.
          05 SWITCH-EOF          PIC X.
             88 EOF-Y            VALUE 'Y'.
             88 NOT-EOF          VALUE 'N'.
      * INPUT AREA
       01 I-INPUT-AREA         PIC X(160).
       01 I-INPUT.
          05 I-GAME.
             10 FILLER         PIC X(5).
             10 I-ID           PIC X(3).
          05 I-OUTCOMES        PIC X(152).
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
              UNSTRING I-INPUT-AREA DELIMITED BY ':'
              INTO
              I-GAME
              I-OUTCOMES
              END-UNSTRING
              PERFORM CA-NUMB-CHECKER
              COMPUTE WS-TEMP = WS-CUR-RED * WS-CUR-GREEN * WS-CUR-BLUE
              ADD WS-TEMP TO WS-TOTAL
              PERFORM R-READ
           END-PERFORM
           DISPLAY 'SUM: ' WS-TOTAL
           .
      *
       CA-NUMB-CHECKER SECTION.
           INITIALIZE WS-POS WS-CUR-RED WS-CUR-GREEN WS-CUR-BLUE
           PERFORM UNTIL WS-POS > 159
              IF I-OUTCOMES(WS-POS:1) IS NUMERIC
                 IF I-OUTCOMES(WS-POS:2) IS NUMERIC
                    MOVE I-OUTCOMES(WS-POS:2) TO WS-AMOUNT
                    ADD 3 TO WS-POS
                 ELSE
                    MOVE I-OUTCOMES(WS-POS:1) TO WS-AMOUNT
                    ADD 2 TO WS-POS
                 END-IF
                 PERFORM CB-COLOR-CHECKER
              ELSE
              ADD 1 TO WS-POS
              END-IF
           END-PERFORM
           .

       CB-COLOR-CHECKER SECTION.
           EVALUATE I-OUTCOMES(WS-POS:1)
           WHEN 'r'
              IF WS-AMOUNT > WS-CUR-RED
                 MOVE WS-AMOUNT TO WS-CUR-RED
              END-IF
           WHEN 'g'
              IF WS-AMOUNT > WS-CUR-GREEN
                 MOVE WS-AMOUNT TO WS-CUR-GREEN
              END-IF
           WHEN 'b'
              IF WS-AMOUNT > WS-CUR-BLUE
                 MOVE WS-AMOUNT TO WS-CUR-BLUE
              END-IF
           END-EVALUATE
           .
       R-READ SECTION.
           INITIALIZE I-INPUT-AREA
           READ WS-INPUT1 INTO I-INPUT-AREA
           AT END
             SET EOF-Y TO TRUE
           NOT AT END
             CONTINUE
           END-READ
           .

       Z-CLOSE  SECTION.
           CLOSE WS-INPUT1
           .
