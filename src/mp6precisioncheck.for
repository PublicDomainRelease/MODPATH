      MODULE DOUBLEBUDGET
         INTEGER, SAVE ::IPRBUD,IPRHEAD
         DOUBLE PRECISION, POINTER, SAVE, DIMENSION(:,:,:) ::DBLBUFF
         REAL(KIND=4), POINTER, SAVE, DIMENSION(:,:,:) ::SNGBUFF
      END MODULE
C
      SUBROUTINE HEADPRECISION(IU,IOUT,NCOL,NROW,NLAY)
      USE DOUBLEBUDGET, ONLY: IPRHEAD,DBLBUFF,SNGBUFF
C  Determine single or double precision file type for a MODFLOW binary
C  head file:  0=unrecognized, 1=single, 2=double.
      DOUBLE PRECISION PERTIMD,TOTIMD
      REAL(KIND=4) PERTIMS,TOTIMS
      CHARACTER*16 TEXT
      INTEGER IOUT
C
      IF(IPRHEAD.EQ.1 .OR. IPRHEAD.EQ.2) RETURN
C
C  SINGLE check
      READ(IU,ERR=50,END=50) KSTP,KPER,PERTIMS,TOTIMS,TEXT
      IF(TEXT.EQ.'            HEAD') THEN
         IPRHEAD=1
         GO TO 100
      END IF
C
C  DOUBLE check
50    REWIND(IU)
      READ(IU,ERR=100,END=100) KSTP,KPER,PERTIMD,TOTIMD,TEXT
      IF(TEXT.EQ.'            HEAD') THEN
         IPRHEAD=2
         ALLOCATE (DBLBUFF(NCOL,NROW,NLAY))
      END IF
C
100   IF(IPRHEAD.EQ.1) THEN
         WRITE(IOUT,*)
     1   'Single Precision Binary Head file'
      ELSE IF(IPRHEAD.EQ.2) THEN
         WRITE(IOUT,*)
     1   'Double Precision Binary Head file'
      ELSE
        CALL USTOP('Unable to determine the precision of the Head File')
      END IF
      REWIND(IU)
      RETURN
      END
      SUBROUTINE BUDGETPRECISION(IU,NCOL,NROW,NLAY,BUFF,IOUT)
C  Determine single or double precision file type for a MODFLOW
C  budget file:  0=unrecognized, 1=single, 2=double.
      USE DOUBLEBUDGET, ONLY:   IPRBUD,DBLBUFF,SNGBUFF
      DIMENSION BUFF(NCOL,NROW,NLAY)
      DOUBLE PRECISION DELTD,PERTIMD,TOTIMD,VALD
      REAL(KIND=4) DELTS,PERTIMS,TOTIMS,VALS
      CHARACTER*16 TEXT1,TEXT2
      INTEGER IOUT
C
C
      IF(IPRBUD.EQ.1 .OR. IPRBUD.EQ.2) RETURN
C
C  SINGLE check
      IF(.NOT.ASSOCIATED(SNGBUFF)) ALLOCATE (SNGBUFF(NCOL,NROW,NLAY))
      READ(IU,ERR=100,END=100) KSTP,KPER,TEXT1,NC,NR,NL
      ICODE=0
      IF(NL.LT.0) THEN
        NL=-NL
        READ(IU,ERR=50,END=50) ICODE,DELTS,PERTIMS,TOTIMS
      END IF
      IF(NC.NE.NCOL .OR. NR.NE.NROW .OR. NL.NE.NLAY) GO TO 100
C
C  Read data depending on ICODE.  ICODE 0,1, or 2 are the only allowed
C  values because the first budget terms must be from the internal
C  flow package (BCF,LPF, or HUF).
      IF(ICODE.EQ.0 .OR. ICODE.EQ.1) THEN
         READ(IU,ERR=50,END=50) SNGBUFF
      ELSE IF(ICODE.EQ.2) THEN
         READ(IU,ERR=50,END=50) NLST
         IF(NLST.GT.0) THEN
            DO 22 N=1,NLST
            READ(IU,END=50,ERR=50) ICELL,VALS
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
           IPRBUD=1
           GO TO 100
      ELSE IF(TEXT1.EQ.'   CONSTANT HEAD' .AND.
     1        TEXT2.EQ.'FLOW RIGHT FACE ') THEN
           IPRBUD=1
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
           IPRBUD=2
      ELSE IF(TEXT1.EQ.'   CONSTANT HEAD' .AND.
     1        TEXT2.EQ.'FLOW RIGHT FACE ') THEN
           IPRBUD=2
      END IF
C
100   IF(IPRBUD.EQ.1) THEN
         IF(IOUT.GT.0) WRITE(IOUT,*)
     1   'Single Precision Binary Budget file'
      ELSE IF(IPRBUD.EQ.2) THEN
         IF(IOUT.GT.0) WRITE(IOUT,*)
     1   'Double Precision Binary Budget file'
      ELSE
        CALL USTOP(
     1  'Unable to determine the precision of the budget file')
      END IF
      REWIND(IU)
      RETURN
      END
