 IDENTIFICATION DIVISION. 
       PROGRAM-ID. ASS5. 
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT CUST-FILE ASSIGN TO CUST 
           ORGANIZATION IS INDEXED 
           ACCESS MODE IS RANDOM 
           RECORD KEY IS CUST-ID 
           FILE STATUS IS WS-STATUS. 
 
 
 
       DATA DIVISION. 
       FILE SECTION. 
       FD  CUST-FILE. 
       01  CUSTOMER-RECORD. 
           05 CUST-ID              PIC X(08). 
           05 CUST-NAME            PIC X(32). 
           05 CUST-ADDRESS         PIC X(60). 
 
       WORKING-STORAGE SECTION. 
       01  WS-STATUS              PIC 99 VALUE 0. 
       01  WS-EOF                 PIC X VALUE 'N'. 
       01  WS-COUNTER             PIC 9(08) VALUE 1. 
       01  WS-ID-NUM              PIC 9(08). 
       01  WS-ID-STR              PIC X(8). 
       01  WS-SEARCH-ID           PIC X(08). 
 
       PROCEDURE DIVISION. 
       MAIN-PARA. 
           PERFORM INIT-PARA 
           PERFORM WRITE-PARA UNTIL WS-COUNTER > 20 
           PERFORM DISPLAY-PARA 
           PERFORM SEARCH-PARA 
           PERFORM CLOSE-PARA 
           STOP RUN. 
 
       INIT-PARA. 
           OPEN OUTPUT CUST-FILE 
           IF WS-STATUS NOT = 0 
              DISPLAY "FILE OPEN ERROR: " WS-STATUS 
              STOP RUN 
           END-IF. 
 
       WRITE-PARA. 
           MOVE SPACES TO CUSTOMER-RECORD 
           MOVE WS-COUNTER TO WS-ID-NUM 
           MOVE WS-ID-NUM TO CUST-ID 
           STRING "Customer Name " WS-COUNTER 
               DELIMITED BY SIZE INTO CUST-NAME 
           STRING "Address " WS-COUNTER " Street, City" 
               DELIMITED BY SIZE INTO CUST-ADDRESS 
           WRITE CUSTOMER-RECORD 
           IF WS-STATUS NOT = 0 
              DISPLAY "WRITE ERROR: " WS-STATUS 
              STOP RUN 
           END-IF 
           ADD 1 TO WS-COUNTER. 
 
 
       DISPLAY-PARA. 
           CLOSE CUST-FILE 
           OPEN INPUT CUST-FILE 
           MOVE 1 TO WS-COUNTER 
           DISPLAY "CUSTOMER RECORDS:" 
           DISPLAY "==================" 
           PERFORM UNTIL WS-COUNTER > 20 
               MOVE WS-COUNTER TO WS-ID-NUM 
               MOVE WS-ID-NUM TO CUST-ID 
               READ CUST-FILE KEY IS CUST-ID 
                   INVALID KEY 
                       DISPLAY "RECORD NOT FOUND: " CUST-ID 
                   NOT INVALID KEY 
                       DISPLAY "ID: " CUST-ID 
                           " NAME: " CUST-NAME 
                           " ADDRESS: " CUST-ADDRESS 
               END-READ 
               ADD 1 TO WS-COUNTER 
           END-PERFORM. 
 
       SEARCH-PARA. 
           ACCEPT WS-SEARCH-ID FROM SYSIN 
           OPEN INPUT CUST-FILE 
           MOVE WS-SEARCH-ID TO CUST-ID 
           READ CUST-FILE KEY IS CUST-ID 
               INVALID KEY 
                   DISPLAY "RECORD NOT FOUND: " CUST-ID 
               NOT INVALID KEY 
                   DISPLAY " " 
                   DISPLAY "found the data set" 
                   DISPLAY "ID: " CUST-ID 
                       " NAME: " CUST-NAME 
                       " ADDRESS: " CUST-ADDRESS 
           END-READ. 
       CLOSE-PARA. 
           CLOSE CUST-FILE.
