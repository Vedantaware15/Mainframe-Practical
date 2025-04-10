//Z67429S JOB (ACCT),'SORT JOB',CLASS=A,MSGCLASS=A,NOTIFY=&SYSUID
//* Step 1 - Sort input file in descending order
//STEP01   EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=Z67429.JCL.SORTIN1,DISP=SHR
//SORTOUT  DD DSN=Z67429.JCL.SORTOUT1,DISP=OLD
//SYSIN    DD *
  SORT FIELDS=(1,5,CH,D)
/*
//*
//* Step 2 - Count input records
//CNTIN    EXEC PGM=ICETOOL
//TOOLMSG  DD SYSOUT=*
//DFSMSG   DD SYSOUT=*
//IN1      DD DSN=Z67429.JCL.SORTIN1,DISP=SHR
//TOOLIN   DD *
  COUNT FROM(IN1) EMPTY
/*
//*
//* Step 3 - Count output records
//CNTOUT   EXEC PGM=ICETOOL
//TOOLMSG  DD SYSOUT=*
//DFSMSG   DD SYSOUT=*
//IN2      DD DSN=Z67429.JCL.SORTOUT1,DISP=SHR
//TOOLIN   DD *
  COUNT FROM(IN2) EMPTY
/*
//*
//* Step 4 - If previous steps failed (counts don't match), delete output
//DELSTEP  EXEC PGM=IEFBR14,COND=((0,NE,CNTIN),(0,NE,CNTOUT))
//SORTOUT  DD DSN=Z67429.JCL.SORTOUT1,DISP=(MOD,DELETE,DELETE)
//*
//* Step 5 - Show error message if record counts mismatched
//MSGSTEP  EXEC PGM=IEBGENER,COND=((0,NE,CNTIN),(0,NE,CNTOUT))
//SYSIN    DD DUMMY
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
!! ERROR: Record count mismatch! Sorted output discarded.
/*
//SYSUT2   DD SYSOUT=*

