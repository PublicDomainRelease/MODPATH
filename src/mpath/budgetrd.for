C  MODPATH release: Version 4.00 (V4, Release 1, 2-2000)
C    New routines to read MODFLOW-2000 budget files
C
C***** SUBROUTINES *****
C     RDBUDG
C     RDBDNM
C***********************

C***** SUBROUTINE *****
      SUBROUTINE RDBUDG (BUFF,TEXT,NCOL,NROW,NLAY,IU,IO,LPER,
     1                   LSTP,IBUFF)
      DIMENSION BUFF(NCOL,NROW,NLAY),IBUFF(NCOL,NROW,NLAY)
      DIMENSION VAL(6)
      CHARACTER*16 TEXT
C
C....THIS ROUTINE READS THROUGH BUDGET FILE AND RETURNS THE NEXT SET OF
C....DATA THAT MATCHES THE STRESS PERIOD AND TIME STEP. THIS ROUTINE READS
C....DATA INTO BUFF EVEN IF THE DATA ARE POINT DATA.
C....Budget file types as indicated in NBTYPE
C      0 for 3-D array in original format
C      1 for 3-D array with extra header
C      2 for list without auxiliary variables
C      3 for 1-layer data array and 1-layer indicator array
C      4 for layer-1 array
C      5 for list with auxiliary data
C
20    CONTINUE
C... READ HEADER FOR ONE BUDGET TERM
      CALL RDBDNM(TEXT,IU,IO,LPER,LSTP,NLAY,NROW,NCOL,
     1          NBTYPE,NVAL,NIFACE,NLST)
      IF(TEXT.EQ.'END DATA') THEN
         WRITE(IO,*) 'Premature end of file reading budget data'
         WRITE(IO,*) 'Trying to read data for (Period, Step)',LPER,LSTP
         STOP
      END IF
C
C.....IF THE TIME STEP IS TOO EARLY, SKIP THE DATA AND TRY AGAIN.
      IF (TEXT.EQ.'EARLY DATA') THEN
         IF(NBTYPE.EQ.0 .OR. NBTYPE.EQ.1) THEN
C            READ(IU,END=30,ERR=50) BUFF
            READ(IU,ERR=30) BUFF
         ELSE IF(NBTYPE.EQ.2 .OR. NBTYPE.EQ.5) THEN
            IF(NLST.GT.0) THEN
               DO 22 N=1,NLST
C               READ(IU,END=30,ERR=50) ICELL,(VAL(I),I=1,NVAL)
               READ(IU,ERR=30) ICELL,(VAL(I),I=1,NVAL)
22             CONTINUE
            END IF
         ELSE IF(NBTYPE.EQ.4) THEN
C            READ(IU,END=30,ERR=50) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
            READ(IU,ERR=30) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
         ELSE
C            READ(IU,END=30,ERR=50) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
C            READ(IU,END=30,ERR=50) ((IBUFF(J,I,1),J=1,NCOL),I=1,NROW)
            READ(IU,ERR=30) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
            READ(IU,ERR=30) ((IBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         END IF
         GO TO 20
      END IF
C
C  Depending on file type, read the budget data into BUFF.
      DO 25 K=1,NLAY
      DO 25 I=1,NROW
      DO 25 J=1,NCOL
      BUFF(J,I,K)=0.
25    CONTINUE
      NRC=NROW*NCOL
      IF(NBTYPE.EQ.0 .OR. NBTYPE.EQ.1) THEN
C         READ(IU,END=30,ERR=50) BUFF
         READ(IU,ERR=30) BUFF
      ELSE IF(NBTYPE.EQ.2 .OR. NBTYPE.EQ.5) THEN
         IF(NLST.GT.0) THEN
            DO 26 N=1,NLST
C            READ(IU,END=30,ERR=50) ICELL,(VAL(I),I=1,NVAL)
            READ(IU,ERR=30) ICELL,(VAL(I),I=1,NVAL)
            K= (ICELL-1)/NRC + 1
            I= ( (ICELL - (K-1)*NRC)-1 )/NCOL + 1
            J= ICELL - (K-1)*NRC - (I-1)*NCOL
            BUFF(J,I,K)=BUFF(J,I,K)+VAL(1)
26          CONTINUE
         END IF
      ELSE IF(NBTYPE.EQ.4) THEN
C         READ(IU,END=30,ERR=50) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
         READ(IU,ERR=30) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
      ELSE
C         READ(IU,END=30,ERR=50) ((IBUFF(J,I,1),J=1,NCOL),I=1,NROW)
C         READ(IU,END=30,ERR=50) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
         READ(IU,ERR=30) ((IBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         READ(IU,ERR=30) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
         DO 28 I=1,NROW
         DO 28 J=1,NCOL
         IF(IBUFF(J,I,1).NE.1) THEN
            BUFF(J,I,IBUFF(J,I,1))=BUFF(J,I,1)
            BUFF(J,I,1)=0.
         END IF
28       CONTINUE
      END IF
      RETURN
C 
30    WRITE(IO,5000) IU
5000  FORMAT(' UNIT ',I3,': END OF FILE ON MODFLOW BUDGET FILE.'/
     1  'BUDGET DATA INCOMPLETE. STOP')
      STOP
50    WRITE(IO,5300) IU
5300  FORMAT(' ERROR READING BUDGET DATA ON UNIT ',
     1I3,' STOP.')
      WRITE(IO,5301) TEXT,LPER,LSTP
5301  FORMAT(1X,A,' PERIOD:',I5,'   STEP:',I5)
      STOP
C
      END

C***** SUBROUTINE *****
      SUBROUTINE RDBDNM(TEXT,IU,IO,KPER,KSTP,NLAY,NROW,NCOL,
     1          NBTYPE,NVAL,NIFACE,NLST)
C     ******************************************************************
C     Read a budget header.
C     If it matches the specified time, return information about the
C        budget term.
C     If it exceeds the time, rewind to the starting location
C     ******************************************************************
      CHARACTER*16 TXTSAV
      SAVE IOLD,KS,KP,TXTSV,NC,NR,NLCODE
      CHARACTER*16 TEXT,CTMP(5)
      DATA IOLD/0/
C
C      READ(IU,END=50,ERR=1000) KS,KP,TEXT,NC,NR,NLCODE
      IF(IOLD.EQ.0) THEN
         READ(IU,ERR=50) KS,KP,TXTSAV,NC,NR,NLCODE
      END IF
      TEXT=TXTSAV
      IF(KP.GT.KPER .OR. (KP.EQ.KPER .AND. KS.GT.KSTP)) THEN
         IOLD=1
         TEXT='END DATA'
         RETURN
      ELSE
         IOLD=0
      END IF
      NL=NLCODE
      IF(NL.LT.0) NL=-NL
      IF(NC.NE.NCOL .OR. NR.NE.NROW .OR. NL.NE.NLAY) THEN
         WRITE(IO,*) 'Budget file does not match the grid dimensions'
         STOP
      END IF
C
      NBTYPE=0
      NVAL=1
      NIFACE=0
      NLST=0
      IF(NLCODE.LT.0) THEN
C         READ(IU,END=50,ERR=1000) NBTYPE,DELT,PERTIM,TOTIM
         READ(IU,ERR=50) NBTYPE,DELT,PERTIM,TOTIM
         IF(NBTYPE.EQ.5) THEN
C            READ(IU,END=50,ERR=1000) NVAL
            READ(IU,ERR=50) NVAL
            IF(NVAL.GT.1) THEN
C               READ(IU,END=50,ERR=1000) (CTMP(I),I=1,NVAL-1)
               READ(IU,ERR=50) (CTMP(I),I=1,NVAL-1)
               DO 10 I=1,NVAL-1
               IF(CTMP(I).EQ.'IFACE') NIFACE=I+1
10             CONTINUE
            END IF
         END IF
         IF(NBTYPE.EQ.5 .OR. NBTYPE.EQ.2) THEN
C            READ(IU,END=50,ERR=1000) NLST
            READ(IU,ERR=50) NLST
         END IF
      END IF
      IF(KP.LT.KPER) THEN
         TEXT='EARLY DATA'
         RETURN
      END IF
      IF(KP.EQ.KPER .AND. KS.LT.KSTP) THEN
         TEXT='EARLY DATA'
         RETURN
      END IF
      RETURN
C
50    TEXT='END DATA'
      RETURN
1000  WRITE(IO,*) ' Error reading budget file header'
      STOP
      END
