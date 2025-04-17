//JCLASS7  JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*---------------------------------------------------------------*
//* STEP 1: CREATE AND POPULATE PS(FLAT) FILE
//*---------------------------------------------------------------*
//STEP1    EXEC PGM=IEBGENER
//SYSUT1   DD *
0000000001Purchased office stationery                    2025-04-14TXN01
0000000002Client payment received                        2025-04-14TXN02
0000000003Paid electricity bill                          2025-04-14TXN03
0000000004Refunded client overpayment                    2025-04-14TXN04
0000000005Transferred funds to investment account        2025-04-14TXN05
0000000006Processed monthly payroll                      2025-04-14TXN06
0000000007Received loan repayment                        2025-04-14TXN07
0000000008Ordered new laptops for staff                  2025-04-14TXN08
0000000009Paid vendor invoice                            2025-04-14TXN09
0000000010Travel reimbursement for employee              2025-04-14TXN10
/*
//SYSUT2   DD DSN=Z67419.PS.TRAN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*---------------------------------------------------------------*
//* STEP 2: CREATE VSAM KSDS USING IDCAMS
//*---------------------------------------------------------------*
//STEP2    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  SET MAXCC = 0
  DEFINE CLUSTER (NAME(Z67427.ASS7.KSDS)  -
  INDEXED -
  KEYS(10 0) -
  RECSZ(100 100) -
  TRACKS(1,1) -
  CISZ(4096) -
  FREESPACE(5 5)) -
  DATA (NAME(Z67419.ASS7.KSDS.DATA)) -
  INDEX (NAME(Z67419.ASS7.KSDS.INDEX))
/*
//*---------------------------------------------------------------*
//* STEP 3: LOAD DATA FROM PS FILE TO KSDS
//*---------------------------------------------------------------*
//STEP3    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  REPRO -
    INFILE(INFILE) -
    OUTFILE(OUTFILE)
/*
//INFILE   DD DSN=Z67419.PS.TRAN,DISP=SHR
//OUTFILE  DD DSN=Z67419.ASS7.KSDS,DISP=SHR
//*---------------------------------------------------------------*
//* STEP 4: LISTCAT KSDS
//*---------------------------------------------------------------*
//STEP4    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTC ENT(Z67419.ASS7.KSDS) ALL
/*
