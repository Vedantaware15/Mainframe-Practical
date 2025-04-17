//JCLASS6  JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*---------------------------------------------------------------*
//* STEP 1: DEFINE GDG BASE
//*---------------------------------------------------------------*
//STEP01  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE GDG (NAME(Z67427.TRAN.LOGS) LIMIT(10) SCRATCH NOEMPTY)
/*
//*----------------------------------------------------------------*
//* STEP 2.1: CREATE FIRST GDG GENERATION AND WRITE SAMPLE RECORDS
//*----------------------------------------------------------------*
//STEP021  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
TXN0000001 2025-04-14         Bought office supplies
TXN0000002 2025-04-14         Received payment from client
TXN0000003 2025-04-14         Electricity bill paid
TXN0000004 2025-04-14         Client refund issued
TXN0000005 2025-04-14         Internal transfer
TXN0000006 2025-04-14         Payroll processed
TXN0000007 2025-04-14         Loan repayment received
TXN0000008 2025-04-14         New equipment ordered
TXN0000009 2025-04-14         Vendor payment made
TXN0000010 2025-04-14         Travel reimbursement
/*
//SYSUT2   DD DSN=Z67427.TRAN.LOGS(+1),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1))
//SYSIN    DD DUMMY
//*----------------------------------------------------------------*
//* STEP 2.2: CREATE SECOND GDG GENERATION AND WRITE SAMPLE RECORDS
//*----------------------------------------------------------------*
//STEP022  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
TXN0000011 2025-04-15         Stock purchase - Item A
TXN0000012 2025-04-15         Received insurance claim
TXN0000013 2025-04-15         Paid quarterly tax
TXN0000014 2025-04-15         Maintenance service fee
TXN0000015 2025-04-15         Office supplies return
TXN0000016 2025-04-15         Software license renewal
TXN0000017 2025-04-15         Dividend received
TXN0000018 2025-04-15         Petty cash top-up
TXN0000019 2025-04-15         Advertising expense
TXN0000020 2025-04-15         Consultancy fee paid
/*
//SYSUT2   DD DSN=Z67427.TRAN.LOGS(+1),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1))
//SYSIN    DD DUMMY
//*----------------------------------------------------------------*
//* STEP 2.3: CREATE THIRD GDG GENERATION AND WRITE SAMPLE RECORDS
//*----------------------------------------------------------------*
//STEP023  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
TXN0000021 2025-04-16         Reimbursed customer
TXN0000022 2025-04-16         Subscribed to cloud service
TXN0000023 2025-04-16         Purchased new printer
TXN0000024 2025-04-16         Data backup completed
TXN0000025 2025-04-16         Salary credited
TXN0000026 2025-04-16         Income from affiliate
TXN0000027 2025-04-16         Meeting expense
TXN0000028 2025-04-16         Project advance received
TXN0000029 2025-04-16         Delivery charges
TXN0000030 2025-04-16         Subscription refund
/*
//SYSUT2   DD DSN=Z67427.TRAN.LOGS(+1),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1))
//SYSIN    DD DUMMY
//*----------------------------------------------------------------*
//* STEP 3: LIST ALL EXISTING GENERATIONS
//*----------------------------------------------------------------*
//STEP030  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTC ENT(Z67427.TRAN.LOGS) ALL
/*
//*----------------------------------------------------------------*
//* STEP 4: ARCHIVE LAST 3 GENERATIONS TO A FLAT FILE
//*----------------------------------------------------------------*
//ARCHIVE EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=Z67427.TRAN.LOGS(-3),DISP=SHR
//         DD DSN=Z67427.TRAN.LOGS(-2),DISP=SHR
//         DD DSN=Z67427.TRAN.LOGS(-1),DISP=SHR
//SYSUT2   DD DSN=Z67427.TRAN.ARCHIVE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1))
//SYSIN    DD DUMMY
//*----------------------------------------------------------------*
//* STEP 5: DELETE LAST 3 GENERATIONS (CLEANUP)
//*----------------------------------------------------------------*
//CLEANUP  EXEC PGM=IEFBR14
//DELGEN1  DD DSN=Z67427.TRAN.LOGS(-3),DISP=(MOD,DELETE,DELETE)
//DELGEN2  DD DSN=Z67427.TRAN.LOGS(-2),DISP=(MOD,DELETE,DELETE)
//DELGEN3  DD DSN=Z67427.TRAN.LOGS(-1),DISP=(MOD,DELETE,DELETE)
