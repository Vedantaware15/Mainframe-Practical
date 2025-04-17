//JCLASS8  JOB MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*---------------------------------------------------------------*
//* STEP 1: CREATE GDG BASE
//*---------------------------------------------------------------*
//GDGBASE  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE GDG (NAME(Z67427.ASS8.GDGBASE) LIMIT(4) SCRATCH NOEMPTY)
/*
//*---------------------------------------------------------------*
//* STEP 2: WRITE 1ST SET OF RECORDS TO GDG(+1)
//*---------------------------------------------------------------*
//WRITE1   EXEC PGM=IEBGENER
//SYSUT1   DD *
A111111111111111111111111111111111111111
B222222222222222222222222222222222222222
C333333333333333333333333333333333333333
D444444444444444444444444444444444444444
/*
//SYSUT2   DD DSN=Z67419.ASS8.GDGBASE(+1),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*---------------------------------------------------------------*
//* STEP 3: WRITE 2ND SET OF RECORDS TO GDG(+2)
//*---------------------------------------------------------------*
//WRITE2   EXEC PGM=IEBGENER
//SYSUT1   DD *
E555555555555555555555555555555555555555
F666666666666666666666666666666666666666
G777777777777777777777777777777777777777
H888888888888888888888888888888888888888
/*
//SYSUT2   DD DSN=Z67419.ASS8.GDGBASE(+2),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*---------------------------------------------------------------*
//* STEP 4: WRITE 3RD SET OF RECORDS TO GDG(+3)
//*---------------------------------------------------------------*
//WRITE3   EXEC PGM=IEBGENER
//SYSUT1   DD *
Eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
Ffffffffffffffffffffffffffffffffffffffff
Gggggggggggggggggggggggggggggggggggggggg
Hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
/*
//SYSUT2   DD DSN=Z67419.ASS8.GDGBASE(+3),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*---------------------------------------------------------------*
//* STEP 5: WRITE 4TH SET OF RECORDS TO GDG(+4)
//*---------------------------------------------------------------*
//WRITE4   EXEC PGM=IEBGENER
//SYSUT1   DD *
Aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
Bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
Cccccccccccccccccccccccccccccccccccccccc
Dddddddddddddddddddddddddddddddddddddddd
/*
//SYSUT2   DD DSN=Z67419.ASS8.GDGBASE(+4),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*---------------------------------------------------------------*
//* STEP 6A: CONCATENATE GDG(+1) AND GDG(+4)
//*---------------------------------------------------------------*
//CONCAT14 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTJNF1 DD DSN=Z67419.ASS8.GDGBASE(+1),DISP=SHR
//SORTJNF2 DD DSN=Z67419.ASS8.GDGBASE(+4),DISP=SHR
//SORTOUT  DD DSN=Z67419.ASS8.PS14OUT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE)
//SYSIN    DD *
  JOINKEYS FILE=F1,FIELDS=(1,1,A)
  JOINKEYS FILE=F2,FIELDS=(1,1,A)
  REFORMAT FIELDS=(F1:1,40,F2:1,40)
  OPTION COPY
/*
//*---------------------------------------------------------------*
//* STEP 6B: CONCATENATE GDG(+2) AND GDG(+3)
//*---------------------------------------------------------------*
//CONCAT23 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTJNF1 DD DSN=Z67419.ASS8.GDGBASE(+2),DISP=SHR
//SORTJNF2 DD DSN=Z67419.ASS8.GDGBASE(+3),DISP=SHR
//SORTOUT  DD DSN=Z67419.ASS8.PS23OUT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE)
//SYSIN    DD *
  JOINKEYS FILE=F1,FIELDS=(1,1,A)
  JOINKEYS FILE=F2,FIELDS=(1,1,A)
  REFORMAT FIELDS=(F1:1,40,F2:1,40)
  OPTION COPY
/*
//
