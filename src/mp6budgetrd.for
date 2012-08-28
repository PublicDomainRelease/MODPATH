C  MODPATH release: Version 4.00 (V4, Release 1, 2-2000)
C    New routines to read MODFLOW-2000 budget files
C
C***** SUBROUTINES *****
C     RDBUDG
C     RDBDNM
C***********************

C***** SUBROUTINE *****
      SUBROUTINE RDBUDG (BUFF,TEXT,NCOL,NROW,NLAY,IU,IO,LPER,
     1                   LSTP,IBUFF,IBYTES,IOTYPE)
      USE DOUBLEBUDGET, ONLY: IPRBUD
      USE GLOBAL, ONLY:KIND8I
C
      DIMENSION BUFF(NCOL,NROW,NLAY),IBUFF(NCOL,NROW,NLAY)
      CHARACTER*16 TEXT
      INTEGER :: IBYTES,ISUM
      INTEGER ::IOTYPE
      CHARACTER(LEN=200) ::MESSAGE
C
      IBYTES = 0
      IF(IPRBUD.EQ.1) THEN
        CALL RDBUDGSNG(BUFF,TEXT,NCOL,NROW,NLAY,IU,IO,LPER,
     1                   LSTP,IBUFF,IBYTES,IOTYPE)
      ELSE IF(IPRBUD.EQ.2) THEN
         CALL RDBUDGDBL(BUFF,TEXT,NCOL,NROW,NLAY,IU,IO,LPER,
     1                   LSTP,IBUFF,IBYTES,IOTYPE)
      END IF
      
      RETURN
      
      END



C***** SUBROUTINE *****
      SUBROUTINE RDBDNM(TEXT,IU,IO,KPER,KSTP,NLAY,NROW,NCOL,
     1          NBTYPE,NVAL,NIFACE,NLST,IRESET,IBYTES,IOTYPE)
      USE DOUBLEBUDGET, ONLY: IPRBUD
      USE GLOBAL, ONLY: KIND8I
C     ******************************************************************
C     Read a budget header.
C     If it matches the specified time, return information about the
C        budget term.
C     If it exceeds the time, rewind to the starting location
C     ******************************************************************
      CHARACTER*16 TEXT
      INTEGER ::IOTYPE,IBYTES
C
      IBYTES = 0
      IF(IPRBUD.EQ.1) THEN
         CALL RDBDNMSNG(TEXT,IU,IO,KPER,KSTP,NLAY,NROW,NCOL,
     1          NBTYPE,NVAL,NIFACE,NLST,IRESET,IBYTES,IOTYPE)

      ELSE IF(IPRBUD.EQ.2) THEN
         CALL RDBDNMDBL(TEXT,IU,IO,KPER,KSTP,NLAY,NROW,NCOL,
     1          NBTYPE,NVAL,NIFACE,NLST,IRESET,IBYTES,IOTYPE)
      END IF
      
      RETURN
      END
      
      
C***** SUBROUTINE *****
      SUBROUTINE RDBUDGSNG (BUFF,TEXT,NCOL,NROW,NLAY,IU,IO,LPER,
     1                   LSTP,IBUFF,IBYTES,IOTYPE)
      USE DOUBLEBUDGET, ONLY: SNGBUFF
      USE GLOBAL, ONLY: KIND8I
      REAL(KIND=4) VALS(20)
C
      DIMENSION BUFF(NCOL,NROW,NLAY),IBUFF(NCOL,NROW,NLAY)
      CHARACTER*16 TEXT
      CHARACTER(LEN=200) ::MESSAGE
      INTEGER :: IBYTES,ISUM
      INTEGER ::IOTYPE
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
      IBYTES = 0
20    CONTINUE
C... READ HEADER FOR ONE BUDGET TERM
      ISUM = 0
      CALL RDBDNMSNG(TEXT,IU,IO,LPER,LSTP,NLAY,NROW,NCOL,
     1          NBTYPE,NVAL,NIFACE,NLST,0,ISUM,IOTYPE)
      IF(TEXT.EQ.'END DATA') THEN
         WRITE(IO,*) 'Trying to read data for (Period, Step)',LPER,LSTP
         CALL USTOP('Premature end of file reading budget data')
      END IF
C
C.....IF THE TIME STEP IS TOO EARLY, SKIP THE DATA AND TRY AGAIN.
      IF (TEXT.EQ.'EARLY DATA') THEN
         IF(NBTYPE.EQ.0 .OR. NBTYPE.EQ.1) THEN
            READ(IU,END=30,ERR=50) SNGBUFF
         ELSE IF(NBTYPE.EQ.2 .OR. NBTYPE.EQ.5) THEN
            IF(NLST.GT.0) THEN
               DO 22 N=1,NLST
               READ(IU,END=30,ERR=50) ICELL,(VALS(I),I=1,NVAL)
22             CONTINUE
            END IF
         ELSE IF(NBTYPE.EQ.4) THEN
            READ(IU,END=30,ERR=50) ((SNGBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         ELSE
            READ(IU,END=30,ERR=50) ((SNGBUFF(J,I,1),J=1,NCOL),I=1,NROW)
            READ(IU,END=30,ERR=50) ((IBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         END IF
         GO TO 20
      END IF
C
C  Depending on file type, read the budget data into BUFF.
C
C  Add header bytes
C
      IBYTES = IBYTES + ISUM
C
      DO 25 K=1,NLAY
      DO 25 I=1,NROW
      DO 25 J=1,NCOL
      BUFF(J,I,K)=0.
25    CONTINUE
      NRC=NROW*NCOL
      IF(NBTYPE.EQ.0 .OR. NBTYPE.EQ.1) THEN
         READ(IU,END=30,ERR=50) SNGBUFF
         IBYTES = IBYTES + 4*NCOL*NROW*NLAY
         BUFF=SNGBUFF
      ELSE IF(NBTYPE.EQ.2 .OR. NBTYPE.EQ.5) THEN
         IF(NLST.GT.0) THEN
            DO 26 N=1,NLST
            READ(IU,END=30,ERR=50) ICELL,(VALS(I),I=1,NVAL)
            IBYTES = IBYTES + 4 + 4*NVAL
            K= (ICELL-1)/NRC + 1
            I= ( (ICELL - (K-1)*NRC)-1 )/NCOL + 1
            J= ICELL - (K-1)*NRC - (I-1)*NCOL
            BUFF(J,I,K)=BUFF(J,I,K)+VALS(1)
26          CONTINUE
         END IF
      ELSE IF(NBTYPE.EQ.4) THEN
         READ(IU,END=30,ERR=50) ((SNGBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         IBYTES = IBYTES + 4*NCOL*NROW
         DO 27 I=1,NROW
         DO 27 J=1,NCOL
         BUFF(J,I,1)=SNGBUFF(J,I,1)
27       CONTINUE
      ELSE
         READ(IU,END=30,ERR=50) ((IBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         IBYTES = IBYTES + 4*NCOL*NROW
         READ(IU,END=30,ERR=50) ((SNGBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         IBYTES = IBYTES + 4*NCOL*NROW
         DO 28 I=1,NROW
         DO 28 J=1,NCOL
         BUFF(J,I,IBUFF(J,I,1))=SNGBUFF(J,I,1)
28       CONTINUE
      END IF
      RETURN
C 
30    WRITE(MESSAGE,5000) IU
5000  FORMAT(' UNIT ',I3,': END OF FILE ON MODFLOW BUDGET FILE.'/
     1  'BUDGET DATA INCOMPLETE. STOP')
      CALL USTOP(MESSAGE)
50    WRITE(MESSAGE,5300) IU
5300  FORMAT(' ERROR READING BUDGET DATA ON UNIT ',
     1I3,' STOP.')
      WRITE(IO,5301) TEXT,LPER,LSTP
5301  FORMAT(1X,A,' PERIOD:',I5,'   STEP:',I5)
      CALL USTOP(MESSAGE)
C
      END
     
C***** SUBROUTINE *****
      SUBROUTINE RDBUDGDBL (BUFF,TEXT,NCOL,NROW,NLAY,IU,IO,LPER,
     1                   LSTP,IBUFF,IBYTES,IOTYPE)
      USE DOUBLEBUDGET, ONLY: DBLBUFF
      USE GLOBAL, ONLY: KIND8I
      DOUBLE PRECISION VALD(20)
C
      DIMENSION BUFF(NCOL,NROW,NLAY),IBUFF(NCOL,NROW,NLAY)
      CHARACTER*16 TEXT
      CHARACTER(LEN=200) ::MESSAGE
      INTEGER :: IBYTES,ISUM
      INTEGER ::IOTYPE
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
      IBYTES = 0
20    CONTINUE
C... READ HEADER FOR ONE BUDGET TERM
      ISUM = 0
      CALL RDBDNMDBL(TEXT,IU,IO,LPER,LSTP,NLAY,NROW,NCOL,
     1          NBTYPE,NVAL,NIFACE,NLST,0,ISUM,IOTYPE)
      IF(TEXT.EQ.'END DATA') THEN
         WRITE(IO,*) 'Trying to read data for (Period, Step)',LPER,LSTP
         CALL USTOP('Premature end of file reading budget data')
      END IF
C
C.....IF THE TIME STEP IS TOO EARLY, SKIP THE DATA AND TRY AGAIN.
      IF (TEXT.EQ.'EARLY DATA') THEN
         IF(NBTYPE.EQ.0 .OR. NBTYPE.EQ.1) THEN
            READ(IU,END=30,ERR=50) DBLBUFF
         ELSE IF(NBTYPE.EQ.2 .OR. NBTYPE.EQ.5) THEN
            IF(NLST.GT.0) THEN
               DO 22 N=1,NLST
               READ(IU,END=30,ERR=50) ICELL,(VALD(I),I=1,NVAL)
22             CONTINUE
            END IF
         ELSE IF(NBTYPE.EQ.4) THEN
            READ(IU,END=30,ERR=50) ((DBLBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         ELSE
            READ(IU,END=30,ERR=50) ((DBLBUFF(J,I,1),J=1,NCOL),I=1,NROW)
            READ(IU,END=30,ERR=50) ((IBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         END IF
         GO TO 20
      END IF
C
C  Depending on file type, read the budget data into BUFF.
C
C  Add header bytes
C
      IBYTES = IBYTES + ISUM
C
      DO 25 K=1,NLAY
      DO 25 I=1,NROW
      DO 25 J=1,NCOL
      BUFF(J,I,K)=0.
25    CONTINUE
      NRC=NROW*NCOL
      IF(NBTYPE.EQ.0 .OR. NBTYPE.EQ.1) THEN
         READ(IU,END=30,ERR=50) DBLBUFF
         IBYTES = IBYTES + 8*NCOL*NROW*NLAY
         BUFF=DBLBUFF
      ELSE IF(NBTYPE.EQ.2 .OR. NBTYPE.EQ.5) THEN
         IF(NLST.GT.0) THEN
            DO 26 N=1,NLST
            READ(IU,END=30,ERR=50) ICELL,(VALD(I),I=1,NVAL)
            IBYTES = IBYTES + 4 + 8*NVAL
            K= (ICELL-1)/NRC + 1
            I= ( (ICELL - (K-1)*NRC)-1 )/NCOL + 1
            J= ICELL - (K-1)*NRC - (I-1)*NCOL
            BUFF(J,I,K)=BUFF(J,I,K)+VALD(1)
26          CONTINUE
         END IF
      ELSE IF(NBTYPE.EQ.4) THEN
         READ(IU,END=30,ERR=50) ((DBLBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         IBYTES = IBYTES + 8*NCOL*NROW
         DO 27 I=1,NROW
         DO 27 J=1,NCOL
         BUFF(J,I,1)=DBLBUFF(J,I,1)
27       CONTINUE
      ELSE
         READ(IU,END=30,ERR=50) ((IBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         IBYTES = IBYTES + 4*NCOL*NROW
         READ(IU,END=30,ERR=50) ((DBLBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         IBYTES = IBYTES + 8*NCOL*NROW
         DO 28 I=1,NROW
         DO 28 J=1,NCOL
         BUFF(J,I,IBUFF(J,I,1))=DBLBUFF(J,I,1)
28       CONTINUE
      END IF
      RETURN
C 
30    WRITE(MESSAGE,5000) IU
5000  FORMAT(' UNIT ',I3,': END OF FILE ON MODFLOW BUDGET FILE.'/
     1  'BUDGET DATA INCOMPLETE. STOP')
      CALL USTOP(MESSAGE)
50    WRITE(MESSAGE,5300) IU
5300  FORMAT(' ERROR READING BUDGET DATA ON UNIT ',
     1I3,' STOP.')
      WRITE(IO,5301) TEXT,LPER,LSTP
5301  FORMAT(1X,A,' PERIOD:',I5,'   STEP:',I5)
      CALL USTOP(MESSAGE)
C
      END

      SUBROUTINE RDBDNMSNG(TEXT,IU,IO,KPER,KSTP,NLAY,NROW,NCOL,
     1          NBTYPE,NVAL,NIFACE,NLST,IRESET,IBYTES,IOTYPE)
      USE GLOBAL, ONLY: KIND8I
      REAL(KIND=4) DELTS,PERTIMS,TOTIMS,VALS(20)
C     ******************************************************************
C     Read a budget header.
C     If it matches the specified time, return information about the
C        budget term.
C     If it exceeds the time, rewind to the starting location
C     ******************************************************************
      CHARACTER*16 TXTSAV
      SAVE IOLD,KS,KP,TXTSAV,NC,NR,NLCODE
      CHARACTER*16 TEXT,CTMP(20)
      INTEGER :: IBYTES
      INTEGER ::IOTYPE
      DATA IOLD/0/
C
      IBYTES=0
      IF (IRESET.EQ.1) THEN
        IOLD = 0
        RETURN
      END IF
      
      IF(IOLD.EQ.0) THEN
         READ(IU,END=50,ERR=1000) KS,KP,TXTSAV,NC,NR,NLCODE
      END IF
      TEXT=TXTSAV
    5 FORMAT(1X,'Reading ',A,' for Period',I5,'   Time Step',I5)
      IF(KP.GT.KPER .OR. (KP.EQ.KPER .AND. KS.GT.KSTP)) THEN
         IOLD=1
         TEXT='END DATA'
         RETURN
      ELSE
         IOLD=0
         IBYTES=36
      END IF
      IF(IOTYPE.EQ.1) WRITE(IO,5) TEXT,KP,KS
      NL=NLCODE
      IF(NL.LT.0) NL=-NL
      IF(NC.NE.NCOL .OR. NR.NE.NROW .OR. NL.NE.NLAY) THEN
         CALL USTOP('Budget file does not match the grid dimensions')
      END IF
C
      NBTYPE=0
      NVAL=1
      NIFACE=0
      NLST=0
      IF(NLCODE.LT.0) THEN
         READ(IU,END=50,ERR=1000) NBTYPE,DELTS,PERTIMS,TOTIMS
         IBYTES=IBYTES + 16
         IF(NBTYPE.EQ.5) THEN
            READ(IU,END=50,ERR=1000) NVAL
            IBYTES=IBYTES + 4
            IF(NVAL.GT.1) THEN
               READ(IU,END=50,ERR=1000) (CTMP(I),I=1,NVAL-1)
               IBYTES=IBYTES + 16*(NVAL-1)
               DO 10 I=1,NVAL-1
               IF(CTMP(I).EQ.'IFACE') NIFACE=I+1
10             CONTINUE
            END IF
         END IF
         IF(NBTYPE.EQ.5 .OR. NBTYPE.EQ.2) THEN
            READ(IU,END=50,ERR=1000) NLST
            IBYTES=IBYTES + 4
         END IF
      END IF
      IF(KP.LT.KPER) THEN
         TEXT='EARLY DATA'
         IBYTES = 0
         RETURN
      END IF
      IF(KP.EQ.KPER .AND. KS.LT.KSTP) THEN
         TEXT='EARLY DATA'
         IBYTES = 0
         RETURN
      END IF
      RETURN
C
50    TEXT='END DATA'
      IBYTES=0
      RETURN
1000  CONTINUE
      CALL USTOP('Error reading budget file header')
      END

     
C***** SUBROUTINE *****
      SUBROUTINE RDBDNMDBL(TEXT,IU,IO,KPER,KSTP,NLAY,NROW,NCOL,
     1          NBTYPE,NVAL,NIFACE,NLST,IRESET,IBYTES,IOTYPE)
      USE GLOBAL, ONLY: KIND8I
      DOUBLE PRECISION DELTD,PERTIMD,TOTIMD,VALD(20)
C     ******************************************************************
C     Read a budget header.
C     If it matches the specified time, return information about the
C        budget term.
C     If it exceeds the time, rewind to the starting location
C     ******************************************************************
      CHARACTER*16 TXTSAV
      SAVE IOLD,KS,KP,TXTSAV,NC,NR,NLCODE
      CHARACTER*16 TEXT,CTMP(20)
      INTEGER :: IBYTES
      INTEGER ::IOTYPE
      DATA IOLD/0/
C
      IBYTES=0
      IF (IRESET.EQ.1) THEN
        IOLD = 0
        RETURN
      END IF
      
      IF(IOLD.EQ.0) THEN
         READ(IU,END=50,ERR=1000) KS,KP,TXTSAV,NC,NR,NLCODE
      END IF
      TEXT=TXTSAV
    5 FORMAT(1X,'Reading ',A,' for Period',I5,'   Time Step',I5)
      IF(KP.GT.KPER .OR. (KP.EQ.KPER .AND. KS.GT.KSTP)) THEN
         IOLD=1
         TEXT='END DATA'
         RETURN
      ELSE
         IOLD=0
         IBYTES=36
      END IF
      IF(IOTYPE.EQ.1) WRITE(IO,5) TEXT,KP,KS
      NL=NLCODE
      IF(NL.LT.0) NL=-NL
      IF(NC.NE.NCOL .OR. NR.NE.NROW .OR. NL.NE.NLAY) THEN
         CALL USTOP('Budget file does not match the grid dimensions')
      END IF
C
      NBTYPE=0
      NVAL=1
      NIFACE=0
      NLST=0
      IF(NLCODE.LT.0) THEN
         READ(IU,END=50,ERR=1000) NBTYPE,DELTD,PERTIMD,TOTIMD
         IBYTES=IBYTES + 28
         IF(NBTYPE.EQ.5) THEN
            READ(IU,END=50,ERR=1000) NVAL
            IBYTES=IBYTES + 4
            IF(NVAL.GT.1) THEN
               READ(IU,END=50,ERR=1000) (CTMP(I),I=1,NVAL-1)
               IBYTES=IBYTES + 16*(NVAL-1)
               DO 10 I=1,NVAL-1
               IF(CTMP(I).EQ.'IFACE') NIFACE=I+1
10             CONTINUE
            END IF
         END IF
         IF(NBTYPE.EQ.5 .OR. NBTYPE.EQ.2) THEN
            READ(IU,END=50,ERR=1000) NLST
            IBYTES=IBYTES + 4
         END IF
      END IF
      IF(KP.LT.KPER) THEN
         TEXT='EARLY DATA'
         IBYTES = 0
         RETURN
      END IF
      IF(KP.EQ.KPER .AND. KS.LT.KSTP) THEN
         TEXT='EARLY DATA'
         IBYTES = 0
         RETURN
      END IF
      RETURN
C
50    TEXT='END DATA'
      IBYTES=0
      RETURN
1000  CONTINUE
      CALL USTOP('Error reading budget file header')
      END

