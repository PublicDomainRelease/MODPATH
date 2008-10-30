      MODULE DOUBLEBUDGET
         INTEGER, SAVE ::IPREC
         DOUBLE PRECISION, POINTER, SAVE, DIMENSION(:,:,:) ::DBLBUFF
      END MODULE
C
      SUBROUTINE HEADPRECISION(IU,IOUT,NCOL,NROW,NLAY)
      USE DOUBLEBUDGET, ONLY: IPREC,DBLBUFF
C  Determine single or double precision file type for a MODFLOW binary
C  head file:  0=unrecognized, 1=single, 2=double.
      DOUBLE PRECISION PERTIMD,TOTIMD
      CHARACTER*16 TEXT
C
      IF(IPREC.EQ.1 .OR. IPREC.EQ.2) RETURN
C
C  SINGLE check
      READ(IU,ERR=50,END=50) KSTP,KPER,PERTIM,TOTIM,TEXT
      IF(TEXT.EQ.'            HEAD') THEN
         IPREC=1
         GO TO 100
      END IF
C
C  DOUBLE check
50    REWIND(IU)
      READ(IU,ERR=100,END=100) KSTP,KPER,PERTIMD,TOTIMD,TEXT
      IF(TEXT.EQ.'            HEAD') THEN
         IPREC=2
         ALLOCATE (DBLBUFF(NCOL,NROW,NLAY))
      END IF
C
100   IF(IPREC.EQ.1) THEN
         WRITE(IOUT,*)
     1   'Single Precision Binary Files determined from Head file'
      ELSE IF(IPREC.EQ.2) THEN
         WRITE(IOUT,*)
     1   'Double Precision Binary Files determined from Head file'
      ELSE
         WRITE(IOUT,*)
     1  ' Unable to determine the precision of the Head File'
         STOP
      END IF
      REWIND(IU)
      RETURN
      END
      SUBROUTINE BUDGETPRECISION(IU,NCOL,NROW,NLAY,BUFF,IOUT)
C  Determine single or double precision file type for a MODFLOW
C  budget file:  0=unrecognized, 1=single, 2=double.
      USE DOUBLEBUDGET, ONLY:   IPREC,DBLBUFF
      DIMENSION BUFF(NCOL,NROW,NLAY)
      DOUBLE PRECISION DELTD,PERTIMD,TOTIMD,VALD
      CHARACTER*16 TEXT1,TEXT2
C
C
      IF(IPREC.EQ.1 .OR. IPREC.EQ.2) RETURN
C
C  SINGLE check
      READ(IU,ERR=100,END=100) KSTP,KPER,TEXT1,NC,NR,NL
      ICODE=0
      IF(NL.LT.0) THEN
        NL=-NL
        READ(IU,ERR=50,END=50) ICODE,DELT,PERTIM,TOTIM
      END IF
      IF(NC.NE.NCOL .OR. NR.NE.NROW .OR. NL.NE.NLAY) GO TO 100
C
C  Read data depending on ICODE.  ICODE 0,1, or 2 are the only allowed
C  values because the first budget terms must be from the internal
C  flow package (BCF,LPF, or HUF).
      IF(ICODE.EQ.0 .OR. ICODE.EQ.1) THEN
         READ(IU,ERR=50,END=50) BUFF
      ELSE IF(ICODE.EQ.2) THEN
         READ(IU,ERR=50,END=50) NLST
         IF(NLST.GT.0) THEN
            DO 22 N=1,NLST
            READ(IU,END=50,ERR=50) ICELL,VAL
22          CONTINUE
         END IF
      ELSE
         GO TO 100
      END IF
C
C  Read 2nd header and check for valid type.
      READ(IU,ERR=50,END=50) KSTP,KPER,TEXT2
      IF(TEXT1.EQ.'         STORAGE' .AND.
     1   TEXT2.EQ.'   CONSTANT HEAD') THEN
           IPREC=1
           GO TO 100
      ELSE IF(TEXT1.EQ.'   CONSTANT HEAD' .AND.
     1        TEXT2.EQ.'FLOW RIGHT FACE ') THEN
           IPREC=1
           GO TO 100
      END IF
C
C  DOUBLE check
50    REWIND(IU)
      IF(.NOT.ASSOCIATED(DBLBUFF)) ALLOCATE (DBLBUFF(NCOL,NROW,NLAY))
      READ(IU,ERR=100,END=100) KSTP,KPER,TEXT1,NC,NR,NL
      ICODE=0
      IF(NL.LT.0) THEN
        NL=-NL
        READ(IU,ERR=100,END=100) ICODE,DELTD,PERTIMD,TOTIMD
      END IF
C
C  Read data depending on ICODE.  ICODE 0,1, or 2 are the only allowed
C  values because the first budget terms must be from the internal
C  flow package (BCF,LPF, or HUF).
      IF(ICODE.EQ.0 .OR. ICODE.EQ.1) THEN
         READ(IU,ERR=100,END=100) DBLBUFF
      ELSE IF(ICODE.EQ.2) THEN
         READ(IU,ERR=100,END=100) NLST
         IF(NLST.GT.0) THEN
            DO 72 N=1,NLST
            READ(IU,END=100,ERR=100) ICELL,VALD
72          CONTINUE
         END IF
      ELSE
         GO TO 100
      END IF
C
C  Read 2nd header and check for valid type.
      READ(IU,ERR=100,END=100) KSTP,KPER,TEXT2
      IF(TEXT1.EQ.'         STORAGE' .AND.
     1   TEXT2.EQ.'   CONSTANT HEAD') THEN
           IPREC=2
      ELSE IF(TEXT1.EQ.'   CONSTANT HEAD' .AND.
     1        TEXT2.EQ.'FLOW RIGHT FACE ') THEN
           IPREC=2
      END IF
C
100   IF(IPREC.EQ.1) THEN
         WRITE(IOUT,*)
     1   'Single Precision Binary Files determined from Budget file'
      ELSE IF(IPREC.EQ.2) THEN
         WRITE(IOUT,*)
     1   'Double Precision Binary Files determined from Budget file'
      ELSE
         WRITE(IOUT,*)
     1  ' Unable to determine the precision of the budget file'
         STOP
      END IF
      REWIND(IU)
      RETURN
      END
