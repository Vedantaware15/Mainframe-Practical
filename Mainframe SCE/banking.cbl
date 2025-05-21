       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING-SYSTEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANK-FILE ASSIGN TO "BANK.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-FILE ASSIGN TO "TEMP.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD BANK-FILE.
       01 BANK-RECORD.
           05 ACCOUNT-NUMBER   PIC 9(5).
           05 CUSTOMER-NAME    PIC A(20).
           05 BALANCE          PIC 9(7)V99.

       FD TEMP-FILE.
       01 TEMP-RECORD.
           05 TEMP-ACCOUNT-NUMBER   PIC 9(5).
           05 TEMP-CUSTOMER-NAME    PIC A(20).
           05 TEMP-BALANCE          PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01 WS-CHOICE           PIC 9.
       01 WS-EOF              PIC X VALUE 'N'.
       01 WS-FOUND            PIC X VALUE 'N'.
       01 WS-ACCOUNT-NUMBER   PIC 9(5).
       01 WS-CUSTOMER-NAME    PIC A(20).
       01 WS-BALANCE          PIC 9(7)V99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM UNTIL WS-CHOICE = 5
               DISPLAY "=============================="
               DISPLAY "     BANKING SYSTEM MENU      "
               DISPLAY "=============================="
               DISPLAY "1. Add Account"
               DISPLAY "2. View Accounts"
               DISPLAY "3. Update Account"
               DISPLAY "4. Delete Account"
               DISPLAY "5. Exit"
               DISPLAY "Enter your choice: "
               ACCEPT WS-CHOICE

               EVALUATE WS-CHOICE
                   WHEN 1
                       PERFORM ADD-ACCOUNT
                   WHEN 2
                       PERFORM VIEW-ACCOUNTS
                   WHEN 3
                       PERFORM UPDATE-ACCOUNT
                   WHEN 4
                       PERFORM DELETE-ACCOUNT
                   WHEN OTHER
                       DISPLAY "Exiting program..."
               END-EVALUATE
           END-PERFORM
           STOP RUN.

      * ADD NEW ACCOUNT
       ADD-ACCOUNT.
           OPEN EXTEND BANK-FILE
           DISPLAY "Enter Account Number: "
           ACCEPT WS-ACCOUNT-NUMBER
           DISPLAY "Enter Customer Name: "
           ACCEPT WS-CUSTOMER-NAME
           DISPLAY "Enter Initial Balance: "
           ACCEPT WS-BALANCE
           MOVE WS-ACCOUNT-NUMBER TO ACCOUNT-NUMBER
           MOVE WS-CUSTOMER-NAME TO CUSTOMER-NAME
           MOVE WS-BALANCE TO BALANCE
           WRITE BANK-RECORD
           CLOSE BANK-FILE
           DISPLAY "Account added successfully!".


      * VIEW ALL ACCOUNTS
       VIEW-ACCOUNTS.
           OPEN INPUT BANK-FILE
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-EOF = 'Y'
               READ BANK-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       DISPLAY "Account Number: " ACCOUNT-NUMBER
                       DISPLAY "Customer Name : " CUSTOMER-NAME
                       DISPLAY "Balance       : " BALANCE
                       DISPLAY "--------------------------"
               END-READ
           END-PERFORM
           CLOSE BANK-FILE.

      * UPDATE EXISTING ACCOUNT
       UPDATE-ACCOUNT.
           DISPLAY "Enter Account Number to Update: "
           ACCEPT WS-ACCOUNT-NUMBER
           MOVE 'N' TO WS-EOF
           OPEN INPUT BANK-FILE
           OPEN OUTPUT TEMP-FILE
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ BANK-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF ACCOUNT-NUMBER = WS-ACCOUNT-NUMBER
                           DISPLAY "Enter New Customer Name: "
                           ACCEPT WS-CUSTOMER-NAME
                           DISPLAY "Enter New Balance: "
                           ACCEPT WS-BALANCE
                           MOVE WS-ACCOUNT-NUMBER TO TEMP-ACCOUNT-NUMBER
                           MOVE WS-CUSTOMER-NAME TO TEMP-CUSTOMER-NAME
                           MOVE WS-BALANCE TO TEMP-BALANCE
                       ELSE
                           MOVE ACCOUNT-NUMBER TO TEMP-ACCOUNT-NUMBER
                           MOVE CUSTOMER-NAME TO TEMP-CUSTOMER-NAME
                           MOVE BALANCE TO TEMP-BALANCE
                       END-IF
                       WRITE TEMP-RECORD
               END-READ
           END-PERFORM

           CLOSE BANK-FILE
           CLOSE TEMP-FILE

           MOVE 'N' TO WS-EOF
           OPEN INPUT TEMP-FILE
           OPEN OUTPUT BANK-FILE

           PERFORM UNTIL WS-EOF = 'Y'
               READ TEMP-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE TEMP-ACCOUNT-NUMBER TO ACCOUNT-NUMBER
                       MOVE TEMP-CUSTOMER-NAME TO CUSTOMER-NAME
                       MOVE TEMP-BALANCE TO BALANCE
                       WRITE BANK-RECORD
               END-READ
           END-PERFORM

           CLOSE TEMP-FILE
           CLOSE BANK-FILE
           DISPLAY "Account updated successfully!".

      * DELETE AN ACCOUNT
       DELETE-ACCOUNT.
           DISPLAY "Enter Account Number to Delete: "
           ACCEPT WS-ACCOUNT-NUMBER
           MOVE 'N' TO WS-EOF
           OPEN INPUT BANK-FILE
           OPEN OUTPUT TEMP-FILE

           PERFORM UNTIL WS-EOF = 'Y'
               READ BANK-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF ACCOUNT-NUMBER NOT = WS-ACCOUNT-NUMBER
                           MOVE ACCOUNT-NUMBER TO TEMP-ACCOUNT-NUMBER
                           MOVE CUSTOMER-NAME TO TEMP-CUSTOMER-NAME
                           MOVE BALANCE TO TEMP-BALANCE
                           WRITE TEMP-RECORD
                       END-IF
               END-READ
           END-PERFORM

           CLOSE BANK-FILE
           CLOSE TEMP-FILE

           MOVE 'N' TO WS-EOF
           OPEN INPUT TEMP-FILE
           OPEN OUTPUT BANK-FILE

           PERFORM UNTIL WS-EOF = 'Y'
               READ TEMP-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE TEMP-ACCOUNT-NUMBER TO ACCOUNT-NUMBER
                       MOVE TEMP-CUSTOMER-NAME TO CUSTOMER-NAME
                       MOVE TEMP-BALANCE TO BALANCE
                       WRITE BANK-RECORD
               END-READ
           END-PERFORM

           CLOSE TEMP-FILE
           CLOSE BANK-FILE
           DISPLAY "Account deleted successfully!".
