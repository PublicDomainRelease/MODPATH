C  MODPATH release: Version 4.00 (V4, Release 3, 1-2002)
C    New routines to read MODFLOW-2000 discretization file
C
      SUBROUTINE DIS2AL(INDIS,IOUT,LAYCBD,NCBL,IGRID,NCOL,NROW,NLAY,
     1     NPER)
C
C     ******************************************************************
C     DIS data allocation
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION LAYCBD(200)
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
      MXPER=1000
C
C------Check for existence of discretization file
      IF(INDIS.LE.0) THEN
         WRITE(IOUT,*) ' DIS file must be specified for MODFLOW to run'
         STOP
      END IF
      WRITE(IOUT,590) INDIS
  590 FORMAT(1X,/1X,'DISCRETIZATION INPUT DATA READ FROM UNIT',I3)
C
C5------READ NUMBER OF LAYERS, ROWS, COLUMNS, STRESS PERIODS, AND
C5------ITMUNI USING FREE OR FIXED FORMAT.
C
C2------READ FIRST RECORD AND WRITE
      CALL URDCOM(INDIS,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NROW,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCOL,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPER,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMUNI,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,LENUNI,R,IOUT,IN)
C
C6------PRINT # OF LAYERS, ROWS, COLUMNS AND STRESS PERIODS.
      WRITE(IOUT,600) NLAY,NROW,NCOL
  600 FORMAT(1X,I4,' LAYERS',I10,' ROWS',I10,' COLUMNS')
      WRITE(IOUT,610) NPER
  610 FORMAT(1X,I3,' STRESS PERIOD(S) IN SIMULATION')
      IF(NPER.GT.MXPER) THEN
         WRITE(IOUT,620) MXPER
  620    FORMAT(1X,'THE MAXIMUM NUMBER OF STRESS PERIODS IS:',/
     1      1X,'ABORTING BECAUSE THE MAXIMUM IS EXCEEDED')
         STOP
      END IF
C
C7------SELECT AND PRINT A MESSAGE SHOWING TIME UNIT.
      IF(ITMUNI.LT.0 .OR. ITMUNI.GT.5) ITMUNI=0
      IF(ITMUNI.EQ.0) THEN
         WRITE(IOUT,630)
  630    FORMAT(1X,'MODEL TIME UNIT IS UNDEFINED')
      ELSE IF(ITMUNI.EQ.1) THEN
         WRITE(IOUT,640)
  640    FORMAT(1X,'MODEL TIME UNIT IS SECONDS')
      ELSE IF(ITMUNI.EQ.2) THEN
         WRITE(IOUT,650)
  650    FORMAT(1X,'MODEL TIME UNIT IS MINUTES')
      ELSE IF(ITMUNI.EQ.3) THEN
         WRITE(IOUT,660)
  660    FORMAT(1X,'MODEL TIME UNIT IS HOURS')
      ELSE IF(ITMUNI.EQ.4) THEN
         WRITE(IOUT,670)
  670    FORMAT(1X,'MODEL TIME UNIT IS DAYS')
      ELSE
         WRITE(IOUT,680)
  680    FORMAT(1X,'MODEL TIME UNIT IS YEARS')
      END IF
C
C7------SELECT AND PRINT A MESSAGE SHOWING LENGTH UNIT.
      IF(LENUNI.LT.0 .OR. LENUNI.GT.3) LENUNI=0
      IF(LENUNI.EQ.0) THEN
         WRITE(IOUT,690)
  690    FORMAT(1X,'MODEL LENGTH UNIT IS UNDEFINED')
      ELSE IF(LENUNI.EQ.1) THEN
         WRITE(IOUT,700)
  700    FORMAT(1X,'MODEL LENGTH UNIT IS FEET')
      ELSE IF(LENUNI.EQ.2) THEN
         WRITE(IOUT,710)
  710    FORMAT(1X,'MODEL LENGTH UNIT IS METERS')
      ELSE IF(LENUNI.EQ.3) THEN
         WRITE(IOUT,720)
  720    FORMAT(1X,'MODEL LENGTH UNIT IS CENTIMETERS')
      END IF
C
C------Read confining bed information
      READ(INDIS,*) (LAYCBD(K),K=1,NLAY)
      LAYCBD(NLAY)=0
      WRITE(IOUT,*) ' Confining bed flag for each layer:'
      WRITE(IOUT,'(20I4)') (LAYCBD(K),K=1,NLAY)
C
C  Count confining bed layers and set IGRID for startigraphic 3-D geometry
      IGRID=0
      NCBL=0
      DO 800 I=1,NLAY
      IF(LAYCBD(I).NE.0) NCBL=NCBL+1
800   CONTINUE
C
C------RETURN.
      RETURN
      END
C=======================================================================
      SUBROUTINE DIS2RP(IN,IOUT,DELR,DELC,ZTOP,ZBOT,LAYCBD,NCON,PERLEN,
     1          NUMTS,TIMX,ISS,NCOL,NROW,NLAY,NPER,ISSFLG)
C
C     ******************************************************************
C     READ GLOBAL ARRAYS; ASSIGN IPAR.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION DELR(NCOL),DELC(NROW),ZTOP(NCOL,NROW,NLAY),
     1   ZBOT(NCOL,NROW,NLAY),LAYCBD(200),NCON(NLAY)
      DIMENSION PERLEN(NPER),NUMTS(NPER),TIMX(NPER),ISSFLG(NPER)
      CHARACTER*200 LINE
      CHARACTER*24 ANAME(5)
      DATA ANAME(1) /'                    DELR'/
      DATA ANAME(2) /'                    DELC'/
      DATA ANAME(3) /'TOP ELEVATION OF LAYER 1'/
      DATA ANAME(4) /'  MODEL LAYER BOTTOM EL.'/
      DATA ANAME(5) /'BOT. EL. OF QUASI-3D BED'/
C     ------------------------------------------------------------------
      NN=0
      DO 10 N=1,NLAY
      IF(LAYCBD(N).EQ.0) THEN
         NCON(N)=0
      ELSE
         NN=NN+1
         NCON(N)=NN
      END IF
10    CONTINUE
C
C------Read the DELR and DELC arrays.
      CALL U1DREL(DELR,ANAME(1),NCOL,IN,IOUT)
      CALL U1DREL(DELC,ANAME(2),NROW,IN,IOUT)
C
C------Read the top elevation of layer 1.
      CALL U2DREL(ZTOP(1,1,1),ANAME(3),NROW,NCOL,0,IN,IOUT)
C
C------Read the bottom elevations.
      DO 20 K=1,NLAY
      KK=K
      CALL U2DREL(ZBOT(1,1,K),ANAME(4),NROW,NCOL,KK,IN,IOUT)
      IF(K.NE.NLAY) THEN
         IF(LAYCBD(K).NE.0) THEN
            CALL U2DREL(ZTOP(1,1,K+1),ANAME(5),NROW,NCOL,KK,IN,IOUT)
          ELSE
            DO 15 I=1,NROW
            DO 15 J=1,NCOL
            ZTOP(J,I,K+1)=ZBOT(J,I,K)
15          CONTINUE
         END IF
      END IF
   20 CONTINUE
C
C1------READ AND WRITE LENGTH OF STRESS PERIOD, NUMBER OF TIME STEPS,
C1------TIME STEP MULTIPLIER, AND STEADY-STATE FLAG..
      WRITE(IOUT,61)
   61 FORMAT(1X,//1X,'STRESS PERIOD     LENGTH       TIME STEPS',
     1            '     MULTIPLIER FOR DELT    SS FLAG',/1X,76('-'))
      ISSX=0
      ITR=0
      DO 200 N=1,NPER
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,PERLEN(N),IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMTS(N),R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TIMX(N),IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF (LINE(ISTART:ISTOP).EQ.'TR') THEN
         ISSFLG(N)=0
         ITR=1
      ELSE IF (LINE(ISTART:ISTOP).EQ.'SS') THEN
         ISSFLG(N)=1
         ISSX=1
      ELSE
         WRITE(IOUT,62)
   62    FORMAT(' SSFLAG MUST BE EITHER "SS" OR "TR"',
     1      ' -- STOP EXECUTION (GLOBALRP)')
         STOP
      END IF
      WRITE (IOUT,63) N,PERLEN(N),NUMTS(N),TIMX(N),LINE(ISTART:ISTOP)
   63 FORMAT(1X,I8,1PG21.7,I7,0PF25.3,A11)
C
C1A-----STOP IF NUMTS LE 0, PERLEN LE 0., OR TIMX LE 0.
      IF(NUMTS(N).LE.0) THEN
         WRITE(IOUT,160)
  160    FORMAT(1X,/1X,
     1  'THERE MUST BE AT LEAST ONE TIME STEP IN EVERY STRESS PERIOD')
         STOP
      END IF
      ZERO=0.
      IF(PERLEN(N).EQ.ZERO .AND. ISSFLG(N).EQ.0) THEN
         WRITE(IOUT,165)
  165    FORMAT(1X,/1X,
     1  'PERLEN MUST NOT BE 0.0 FOR TRANSIENT STRESS PERIODS')
         STOP
      END IF
      IF(TIMX(N).LE.ZERO) THEN
         WRITE(IOUT,170)
  170    FORMAT(1X,/1X,'TSMULT MUST BE GREATER THAN 0.0')
         STOP
      END IF
      IF(PERLEN(N).LT.ZERO) THEN
         WRITE(IOUT,175)
  175    FORMAT(1X,/1X,
     1  'PERLEN CANNOT BE LESS THAN 0.0 FOR ANY STRESS PERIOD')
         STOP
      END IF
  200 CONTINUE
C
C  Assign ISS
      IF(ISSX.EQ.0 .AND. ITR.NE.0) THEN
         WRITE(IOUT,70)
   70    FORMAT(/,1X,'TRANSIENT SIMULATION')
         ISS=0
      ELSE IF(ISSX.NE.0 .AND. ITR.EQ.0) THEN
         WRITE(IOUT,75)
   75    FORMAT(/,1X,'STEADY-STATE SIMULATION')
         ISS=1
      ELSE
         WRITE(IOUT,80)
   80    FORMAT(/,1X,'COMBINED STEADY-STATE AND TRANSIENT SIMULATION')
         ISS=0
      END IF
C
      WRITE(IOUT,'(//)')
C
      RETURN
      END
