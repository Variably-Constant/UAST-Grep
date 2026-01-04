      *================================================================*
      * COBOL Test File for UAST-Grep
      * Tests: divisions, sections, paragraphs, data, procedures
      *================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. UAST-Grep-TEST.
       AUTHOR. TEST-AUTHOR.
       DATE-WRITTEN. 2024-01-15.
       DATE-COMPILED.
       SECURITY. NONE.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'INPUT.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
      *
           SELECT OUTPUT-FILE ASSIGN TO 'OUTPUT.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
      *
           SELECT INDEXED-FILE ASSIGN TO 'INDEXED.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IDX-KEY
               ALTERNATE RECORD KEY IS IDX-ALT-KEY
                   WITH DUPLICATES
               FILE STATUS IS WS-FILE-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  INPUT-RECORD                    PIC X(80).
      *
       FD  OUTPUT-FILE
           RECORDING MODE IS F.
       01  OUTPUT-RECORD                   PIC X(132).
      *
       FD  INDEXED-FILE.
       01  INDEXED-RECORD.
           05  IDX-KEY                     PIC 9(8).
           05  IDX-ALT-KEY                 PIC X(20).
           05  IDX-DATA                    PIC X(52).
      *
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Constants
      *----------------------------------------------------------------*
       01  WS-CONSTANTS.
           05  WS-MAX-ITEMS                PIC 9(4) VALUE 100.
           05  WS-DEFAULT-NAME             PIC X(20) VALUE 'UAST-Grep'.
           05  WS-VERSION                  PIC X(10) VALUE '1.0.0'.
      *
      *----------------------------------------------------------------*
      * Status and Flags
      *----------------------------------------------------------------*
       01  WS-STATUS-FLAGS.
           05  WS-FILE-STATUS              PIC XX VALUE SPACES.
               88  WS-FILE-OK              VALUE '00'.
               88  WS-FILE-EOF             VALUE '10'.
               88  WS-FILE-NOT-FOUND       VALUE '35'.
           05  WS-PROCESSING-FLAG          PIC 9 VALUE 0.
               88  WS-CONTINUE             VALUE 0.
               88  WS-STOP                 VALUE 1.
           05  WS-ERROR-FLAG               PIC 9 VALUE 0.
               88  WS-NO-ERROR             VALUE 0.
               88  WS-ERROR-OCCURRED       VALUE 1.
      *
      *----------------------------------------------------------------*
      * Counters and Accumulators
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-RECORD-COUNT             PIC 9(8) VALUE 0.
           05  WS-ERROR-COUNT              PIC 9(4) VALUE 0.
           05  WS-TOTAL-AMOUNT             PIC 9(12)V99 VALUE 0.
           05  WS-LOOP-INDEX               PIC 9(4) VALUE 0.
      *
      *----------------------------------------------------------------*
      * Working Variables
      *----------------------------------------------------------------*
       01  WS-WORK-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-YEAR                 PIC 9(4).
               10  WS-MONTH                PIC 9(2).
               10  WS-DAY                  PIC 9(2).
           05  WS-CURRENT-TIME.
               10  WS-HOUR                 PIC 9(2).
               10  WS-MINUTE               PIC 9(2).
               10  WS-SECOND               PIC 9(2).
           05  WS-TEMP-VALUE               PIC S9(9)V99 COMP-3.
           05  WS-RESULT                   PIC S9(15)V99.
           05  WS-MESSAGE                  PIC X(80).
      *
      *----------------------------------------------------------------*
      * Tables/Arrays
      *----------------------------------------------------------------*
       01  WS-ITEM-TABLE.
           05  WS-ITEM-ENTRY OCCURS 100 TIMES
               INDEXED BY WS-ITEM-IDX.
               10  WS-ITEM-ID              PIC 9(8).
               10  WS-ITEM-NAME            PIC X(30).
               10  WS-ITEM-VALUE           PIC 9(7)V99.
               10  WS-ITEM-STATUS          PIC X.
                   88  WS-ITEM-ACTIVE      VALUE 'A'.
                   88  WS-ITEM-INACTIVE    VALUE 'I'.
                   88  WS-ITEM-DELETED     VALUE 'D'.
      *
       01  WS-LOOKUP-TABLE.
           05  WS-LOOKUP-ENTRY OCCURS 10 TIMES
               ASCENDING KEY IS WS-LOOKUP-CODE
               INDEXED BY WS-LOOKUP-IDX.
               10  WS-LOOKUP-CODE          PIC X(3).
               10  WS-LOOKUP-DESC          PIC X(30).
      *
      *----------------------------------------------------------------*
      * Person Structure
      *----------------------------------------------------------------*
       01  WS-PERSON.
           05  WS-PERSON-NAME              PIC X(50).
           05  WS-PERSON-AGE               PIC 9(3).
           05  WS-PERSON-EMAIL             PIC X(50).
           05  WS-PERSON-ACTIVE            PIC 9.
               88  WS-PERSON-IS-ACTIVE     VALUE 1.
               88  WS-PERSON-IS-INACTIVE   VALUE 0.
      *
       PROCEDURE DIVISION.
      *================================================================*
      * Main Program Flow
      *================================================================*
       0000-MAIN-PROCEDURE.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-FILES
           PERFORM 9000-TERMINATE
           STOP RUN.
      *
      *----------------------------------------------------------------*
      * Initialization
      *----------------------------------------------------------------*
       1000-INITIALIZE.
           INITIALIZE WS-COUNTERS
           INITIALIZE WS-WORK-FIELDS
           PERFORM 1100-GET-CURRENT-DATE
           PERFORM 1200-OPEN-FILES
           IF WS-FILE-OK
               DISPLAY 'Processing started: ' WS-CURRENT-DATE
           ELSE
               PERFORM 8000-FILE-ERROR
           END-IF.
      *
       1100-GET-CURRENT-DATE.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME.
      *
       1200-OPEN-FILES.
           OPEN INPUT INPUT-FILE
           IF NOT WS-FILE-OK
               MOVE 'Error opening input file' TO WS-MESSAGE
               PERFORM 8100-DISPLAY-ERROR
           END-IF.
      *
           OPEN OUTPUT OUTPUT-FILE
           IF NOT WS-FILE-OK
               MOVE 'Error opening output file' TO WS-MESSAGE
               PERFORM 8100-DISPLAY-ERROR
           END-IF.
      *
      *----------------------------------------------------------------*
      * Main Processing
      *----------------------------------------------------------------*
       2000-PROCESS-FILES.
           SET WS-CONTINUE TO TRUE
           PERFORM UNTIL WS-STOP OR WS-FILE-EOF
               PERFORM 2100-READ-RECORD
               IF WS-FILE-OK
                   PERFORM 2200-PROCESS-RECORD
                   PERFORM 2300-WRITE-RECORD
               END-IF
           END-PERFORM.
      *
       2100-READ-RECORD.
           READ INPUT-FILE INTO INPUT-RECORD
               AT END SET WS-STOP TO TRUE
               NOT AT END
                   ADD 1 TO WS-RECORD-COUNT
           END-READ.
      *
       2200-PROCESS-RECORD.
           EVALUATE TRUE
               WHEN INPUT-RECORD(1:1) = 'H'
                   PERFORM 2210-PROCESS-HEADER
               WHEN INPUT-RECORD(1:1) = 'D'
                   PERFORM 2220-PROCESS-DETAIL
               WHEN INPUT-RECORD(1:1) = 'T'
                   PERFORM 2230-PROCESS-TRAILER
               WHEN OTHER
                   PERFORM 2240-PROCESS-OTHER
           END-EVALUATE.
      *
       2210-PROCESS-HEADER.
           MOVE INPUT-RECORD TO OUTPUT-RECORD
           DISPLAY 'Processing header record'.
      *
       2220-PROCESS-DETAIL.
           COMPUTE WS-TEMP-VALUE = FUNCTION NUMVAL(INPUT-RECORD(10:10))
           MULTIPLY WS-TEMP-VALUE BY 2 GIVING WS-RESULT
           ADD WS-RESULT TO WS-TOTAL-AMOUNT
           MOVE WS-RESULT TO OUTPUT-RECORD(50:15).
      *
       2230-PROCESS-TRAILER.
           DISPLAY 'Processing trailer record'.
      *
       2240-PROCESS-OTHER.
           IF INPUT-RECORD = SPACES
               CONTINUE
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'Unknown record type' TO WS-MESSAGE
               PERFORM 8100-DISPLAY-ERROR
           END-IF.
      *
       2300-WRITE-RECORD.
           WRITE OUTPUT-RECORD
           IF NOT WS-FILE-OK
               PERFORM 8000-FILE-ERROR
           END-IF.
      *
      *----------------------------------------------------------------*
      * Table Operations
      *----------------------------------------------------------------*
       3000-TABLE-OPERATIONS.
      *    Initialize table
           PERFORM VARYING WS-LOOP-INDEX FROM 1 BY 1
               UNTIL WS-LOOP-INDEX > WS-MAX-ITEMS
               INITIALIZE WS-ITEM-ENTRY(WS-LOOP-INDEX)
           END-PERFORM.
      *
      *    Search table
           SEARCH ALL WS-LOOKUP-ENTRY
               AT END
                   DISPLAY 'Item not found'
               WHEN WS-LOOKUP-CODE(WS-LOOKUP-IDX) = 'ABC'
                   DISPLAY WS-LOOKUP-DESC(WS-LOOKUP-IDX)
           END-SEARCH.
      *
      *----------------------------------------------------------------*
      * Calculations
      *----------------------------------------------------------------*
       4000-CALCULATIONS.
           COMPUTE WS-RESULT = WS-TEMP-VALUE + 100
           COMPUTE WS-RESULT ROUNDED =
               (WS-TEMP-VALUE * 1.15) / 100

           ADD WS-TEMP-VALUE TO WS-TOTAL-AMOUNT
           SUBTRACT 10 FROM WS-TEMP-VALUE
           MULTIPLY WS-TEMP-VALUE BY 2
           DIVIDE WS-TEMP-VALUE BY 3 GIVING WS-RESULT
               REMAINDER WS-LOOP-INDEX

           COMPUTE WS-RESULT = FUNCTION ABS(WS-TEMP-VALUE)
           COMPUTE WS-RESULT = FUNCTION MAX(10 20 30)
           COMPUTE WS-RESULT = FUNCTION MIN(10 20 30)
           COMPUTE WS-RESULT = FUNCTION MOD(17 5)
           COMPUTE WS-RESULT = FUNCTION INTEGER(3.7)
           MOVE FUNCTION UPPER-CASE(WS-MESSAGE) TO WS-MESSAGE.
      *
      *----------------------------------------------------------------*
      * String Operations
      *----------------------------------------------------------------*
       5000-STRING-OPERATIONS.
           STRING WS-DEFAULT-NAME DELIMITED BY SPACE
                  ' - '          DELIMITED BY SIZE
                  WS-VERSION     DELIMITED BY SPACE
                  INTO WS-MESSAGE
           END-STRING.

           UNSTRING INPUT-RECORD DELIMITED BY ','
               INTO WS-ITEM-NAME WS-ITEM-VALUE
           END-UNSTRING.

           INSPECT WS-MESSAGE TALLYING WS-LOOP-INDEX
               FOR ALL 'A'.

           INSPECT WS-MESSAGE REPLACING ALL 'OLD' BY 'NEW'.

           MOVE FUNCTION LENGTH(WS-MESSAGE) TO WS-LOOP-INDEX.
      *
      *----------------------------------------------------------------*
      * Error Handling
      *----------------------------------------------------------------*
       8000-FILE-ERROR.
           SET WS-ERROR-OCCURRED TO TRUE
           STRING 'File error: ' DELIMITED BY SIZE
                  WS-FILE-STATUS DELIMITED BY SIZE
                  INTO WS-MESSAGE
           END-STRING
           PERFORM 8100-DISPLAY-ERROR.
      *
       8100-DISPLAY-ERROR.
           ADD 1 TO WS-ERROR-COUNT
           DISPLAY 'ERROR: ' WS-MESSAGE.
      *
      *----------------------------------------------------------------*
      * Termination
      *----------------------------------------------------------------*
       9000-TERMINATE.
           CLOSE INPUT-FILE OUTPUT-FILE
           DISPLAY 'Records processed: ' WS-RECORD-COUNT
           DISPLAY 'Errors encountered: ' WS-ERROR-COUNT
           DISPLAY 'Total amount: ' WS-TOTAL-AMOUNT
           DISPLAY 'Processing complete'.
      *
       9999-END-OF-PROGRAM.
           EXIT.
