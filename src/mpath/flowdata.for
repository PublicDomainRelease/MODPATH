C  MODPATH release: Version 4.00
C    Changes to work with MODFLOW-2000 -- IFACE values come from budget file
C    rather than from MODFLOW stress files.
C       Bug Fixes:
C         1. ADDQAR -- added code to multiply IBOUND by 1000 for weak sinks
C         2. ADDQAR, ver. 4.3 -- Corrected error in which RCH and EVT were
C                  applied to the wrong layer when using COMPCAT BUDGET
C                  and the layer is not layer 1
C
C MODPATH Version 3.00 (V3, Release 2, 5-99)
C
C HISTORY:
C    Previous version: MODPATH Version 3.00 (V3, Release 1, 9-94)
C    Changes:
C       Bug Fixes:
C             1. fixed a problem that caused MODPATH to fail when trying
C                to read the MODFLOW head file for cases where MODFLOW-96
C                saved the heads as a single 2D cross section when the
C                XSECTION option was in effect. The fix involved the
C                following changes:
C
C                  A. modified subroutine HQDATA in module FLOWDATA to
C                     correctly read heads saved in the cross-section format
C                     by MODFLOW-96.
C                  B. added subroutine HEDFMT to modlue FLOWDATA. This routine
C                     reads the first header record of the MODFLOW head output
C                     file to determine if heads are saved by layer or
C                     by cross-section for a single row model. The MODFLOW
C                     head output file contains a negative value for layer
C                     number as a flag indicating that heads are saves as
C                     a single cross section.
C
C             2. modified subroutine FLOWS in module FLOWDATA so that the
C                cell-by-cell budget file is read properly when list-oriented
C                stress package (e.g. rivers, drains, etc) have zero entries
C                for one or more stress periods. Previously, MODPATH
C                tried to read flow terms for that case, however MODFLOW
C                does not write the flow terms for list-oriented packages
C                when all entries are zero. That caused an end-of-file
C                error to occur when reading the budget file. This
C                change fixes that problem. This fix also involved minor
C                changes to subroutine RDBUDG in module UTILMP.
C
C       Enhancements: none
C***** SUBROUTINES *****
C     HQDATA
C     MAKEHQ
C     READHQ
C     FACTYP
C     FLOWS
C     HEADS
C     FLUXBC
C     CBFHED
C     RSETIB
C     MODIBN
C     CBFSIZ
C     HEDFMT
C     ADDQLS
C     ADDQAR
C***********************
 
C***** SUBROUTINE *****
      SUBROUTINE HQDATA(HEAD,QX,QY,QZ,BUFF,IUNIT,IBOUND,IBUFF,QSTO,QSS,
     1 DELR,DELC,
     2 LAYCON,KKPER,KKSTP,INHQ,FNAME,PERLEN,NUMTS,TIMX,IBSTRT,
     3 HDRY,HNOFLO,ISSFLG)
C
      INCLUDE 'idat1.inc'
      COMMON /IDAT2/ IRCHTP,IEVTTP,NRCHOP,NEVTOP
      COMMON /MAXSIZ/ MAXSIZ
C
      DIMENSION HEAD(NCOL,NROW,NLAY),QX(NCP1,NROW,NLAY),
     1 QY(NCOL,NRP1,NLAY),QZ(NCOL,NROW,NLP1),BUFF(NCOL,NROW,NLAY),
     2 IUNIT(NUNIT),IBOUND(NCOL,NROW,NLAY),IBUFF(NCOL,NROW,NLAY),
     3 QSTO(NCOL,NROW,NLAY),QSS(NCOL,NROW,NLAY),DELR(NCOL),DELC(NROW),
     6 LAYCON(NLAY),PERLEN(NPER),NUMTS(NPER),TIMX(NPER),ISSFLG(NPER),
     7 IBSTRT(NCOL,NROW,NLAY)
C
      INTEGER NHLAY,NSTPSV
      CHARACTER*80 LINE
      CHARACTER*(*) FNAME
C
C... OPTION KEYS:
C                INHQ = 1 --> STEADY STATE
C                INHQ = 2 --> TRANSIENT, IMPORT FLOW TERMS FROM EXISTING
C                             DIRECT ACCESS FILE
C                INHQ = 3 --> TRANSIENT, READ MODFLOW BUDGET FILE AND
C                             CONSTRUCT DIRECT ACCESS FILE
C
C                IREADQ = 0 --> TIME STEP IS NOT WITHIN RANGE SAVED IN
C                               THE MODFLOW BUDGET FILE. BUDGET FILE
C                               IS NOT READ FOR THAT TIME STEP.
C                IREADQ = 1 --> TIME STEP IS WITHIN RANGE SAVED IN MODFLOW
C                               BUDGET FILE AND FILE IS READ FOR THAT TIME
C                               STEP.
 
C... INITIALIZE SOME VARIABLES
      IUMAIN=I1
      TOTSIM=0.0
      NSTPSV=0
C
      LREC= 4*(NCOL+1)
      IF(IBYTES.NE.1) THEN
         IRM= MOD(LREC,IBYTES)
         IF(IRM.NE.0) THEN
         WRITE(I7,*) 'RECORD LENGTH FOR DIRECT ACCESS FILE NOT EQUAL TO 
     1EVEN MULTIPLE OF 2. STOP'
             STOP
         END IF
         LREC= LREC/IBYTES
      END IF
C
      IF(INHQ.EQ.2) THEN
        OPEN(UNIT=I10,FILE=FNAME,ERR=5100,STATUS='OLD',ACCESS='DIRECT',
     1       FORM='UNFORMATTED',RECL=LREC)
      END IF
 
C... LOOP THROUGH AND READ MODFLOW BUDGET FILE FOR STEADY STATE OR AN
C    INITIAL TRANSIENT SIMULATION
      IF (INHQ.NE.2) THEN
 
C... IF THIS IS A TRANSIENT RUN THAT WILL READ MODFLOW BUDGET FILE, SET
C    MAXPER = NPER, OTHERWISE SET MAXPER=1
         IF(INHQ.EQ.3) THEN
            MAXPER=NPER
         ELSE
            MAXPER=1
         END IF
 
C... FOR TRANSIENT SIMULATION, GO BACK TO MAIN DATA FILE AND GET
C    THE MININUM AND MAXIMUM TIME STEPS THAT DEFINE THE RANGE OF
C    DATA SAVED IN THE MODFLOW BUDGET FILE.
C  Main data file input item 10C.
         IF(ISS.EQ.0) THEN
            READ(IUMAIN,'(A)') LINE
            ICOL=1
            CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,2,LPERMN,RDUMMY,I7,
     1             IUMAIN)
            CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,2,LSTPMN,RDUMMY,I7,
     1             IUMAIN)
            CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,2,LPERMX,RDUMMY,I7,
     1             IUMAIN)
            CALL URWORD(LINE,ICOL,IWSTRT,IWLAST,2,LSTPMX,RDUMMY,I7,
     1             IUMAIN)
            IF(LPERMX.GT.NPER) THEN
               LPERMX=NPER
               LSTPMX=NUMTS(NPER)
            ELSE IF(LSTPMX.GT.NUMTS(LPERMX)) THEN
               LSTPMX=NUMTS(LPERMX)
            END IF
            CALL GETSTP(LPERMN,LSTPMN,NUMTS,NPER,I7,NFSV)
            CALL GETSTP(LPERMX,LSTPMX,NUMTS,NPER,I7,NLSV)
         END IF
 
         IF(INHQ.EQ.3) THEN
           CALL CBFSIZ(NFSV,NLSV,LAYCON,NROW,NCOL,NLAY,MAXSIZ,
     1              IERR)
           IF(IERR.NE.0) THEN
             WRITE(I7,*)
     1       'RUN STOPPED. COMPOSITE BUDGET FILE SIZE TOO LARGE.'
             WRITE(*,*)
     1       'RUN STOPPED. COMPOSITE BUDGET FILE SIZE TOO LARGE.'
             STOP
           END IF
           OPEN(UNIT=I10,FILE=FNAME,ERR=5000,STATUS='UNKNOWN',
     1       ACCESS='DIRECT',FORM='UNFORMATTED',RECL=LREC)
         END IF
 
         DO 150 IPER=1,MAXPER
C
         IF(ISS.EQ.0) THEN
            MAXSTP=NUMTS(IPER)
         ELSE
            MAXSTP=1
         END IF
C
         DO 150 ISTP=1,MAXSTP
         CALL RSETIB(IBOUND,IBSTRT,NCOL,NROW,NLAY)
         IREADQ=1
         IF(ISS.EQ.0) THEN
            CALL GETSTP(IPER,ISTP,NUMTS,NPER,I7,NSTEP)
            IF(NSTEP.LT.NFSV .OR. NSTEP.GT.NLSV) IREADQ=0
         END IF
C 
         IF(IREADQ.NE.0) THEN
C
C  READ HEADS
            CALL HEADS(HEAD,BUFF,IUNIT,IBOUND,LAYCON,IPER,ISTP,
     1             HDRY,HNOFLO)
C  Read CBC flows
            CALL FLOWS (QX,QY,QZ,BUFF,IUNIT,IBOUND,IBUFF,QSTO,
     1        DELR,DELC,NCOL,NROW,NLAY,NCP1,NRP1,NLP1,NUNIT,QSS,I7,
     2        IPER,ISTP,IRCHTP,IEVTTP,ISSFLG)
         END IF
C
         IF (ISS.EQ.0 .AND. INHQ.EQ.3) THEN
            CALL STPSIZ(PERLEN(IPER),ISTP,NUMTS(IPER),TIMX(IPER),DELT)
            TOTSIM=TOTSIM+DELT
            IF(IREADQ.NE.0) THEN
               IF(NSTEP.GE.NFSV .AND. NSTEP.LE.NLSV) THEN
                  NSTPSV=NSTPSV+1
                  CALL MAKEHQ(QX,QY,QZ,QSS,QSTO,HEAD,IBOUND,LAYCON,NCOL,
     1              NROW,NLAY,NCP1,NRP1,NLP1,NSTPSV,I10,IPER,ISTP,
     2              DELT,TOTSIM,NUMTS(IPER),I7)
               END IF
            END IF
         END IF
C
150      CONTINUE
C
C  Finish direct access file
         IF (INHQ.EQ.3) THEN
            NHLAY=0
            DO 10 N=1,NLAY
10          IF(LAYCON(N).NE.0) NHLAY=NHLAY+1
            WRITE(I10,REC=1) NHLAY,NSTPSV
            CALL GETSTP(KKPER,KKSTP,NUMTS,NPER,I7,NSTEP)
            CALL READHQ(QX,QY,QZ,QSS,QSTO,HEAD,IBOUND,BUFF,NCOL,
     1        NROW,NLAY,NCP1,NRP1,NLP1,I10,NSTEP,DELT,TOTSIM,IERR,
     2        NUMTS,NPER,NSTEPF,NSTEPL,KPER,KSTP,1,I7)
            IF(IERR.EQ.1) GO TO 5200
            IF(IERR.EQ.2) GO TO 5201
         END IF
C
C... ELSE IF THIS IS A TRANSIENT RUN FROM AN EXISTING DIRECT ACCESS FILE,
C    WRITE A MESSAGE AND READ THE FLOW DATA FOR (KKPER,KKSTP).
      ELSE IF (INHQ.EQ.2) THEN
         WRITE(I7,*)
     1'-------------------------------------------------------------'
         WRITE(I7,*)
     1'FLOW AND HEAD DATA WILL BE READ FROM COMPOSITE BUDGET FILE...'
         WRITE(I7,*)
     1'-------------------------------------------------------------'
         CALL GETSTP(KKPER,KKSTP,NUMTS,NPER,I7,NSTEP)
         CALL READHQ(QX,QY,QZ,QSS,QSTO,HEAD,IBOUND,BUFF,NCOL,
     1      NROW,NLAY,NCP1,NRP1,NLP1,I10,NSTEP,DELT,TOTSIM,IERR,
     2      NUMTS,NPER,NSTEPF,NSTEPL,KPER,KSTP,1,I7)
         IF(IERR.EQ.1) GO TO 5200
         IF(IERR.EQ.2) GO TO 5201
      END IF
C
      RETURN
5000  WRITE(*,*) 'ERROR OPENING COMPOSITE BUDGET FILE.'
      STOP
5100  WRITE(*,*) 'ERROR OPENING COMPOSITE BUDGET FILE.'
      WRITE(*,*) '   FILE DOES NOT EXIST. STOP.'
      STOP
5200  WRITE(*,*)'TIME STEP NOT PRESENT IN COMPOSITE BUDGET FILE. STOP.'
      STOP
5201  WRITE(*,*)
     1'TIME STEP INCONSISTENCY IN COMPOSITE BUDGET FILE. STOP.'
      STOP
      END
 
C***** SUBROUTINE *****
      SUBROUTINE MAKEHQ(QX,QY,QZ,QSS,QSTO,HEAD,IBOUND,LAYCON,NCOL,NROW,
     1 NLAY,NCP1,NRP1,NLP1,NSTEP,IU,KPER,KSTP,DELT,TOTSIM,NSTPS,I7)
C
      DIMENSION QX(NCP1,NROW,NLAY),QY(NCOL,NRP1,NLAY),
     1QZ(NCOL,NROW,NLP1),QSS(NCOL,NROW,NLAY),QSTO(NCOL,NROW,NLAY),
     2HEAD(NCOL,NROW,NLAY),LAYCON(NLAY),IBOUND(NCOL,NROW,NLAY)
      COMMON /MAXSIZ/ MAXSIZ
C
      INTEGER KKPER,KKSTP,NNSTP,LAYER,MAXSIZ
      KKPER=KPER
      KKSTP=KSTP
      NNSTP=NSTPS
C
      NHLAY=0
      DO 1 N=1,NLAY
1     IF(LAYCON(N).NE.0) NHLAY=NHLAY+1
      NRPTS= (6*NROW*NLAY) + (NROW*NHLAY) + NROW + NLAY
      NREC= 1 + (NSTEP-1)*(NRPTS+1)
      NCHECK= NREC + NRPTS + 1
 
      NBYTES= 4*(NCOL+1)*NCHECK
      IF(NBYTES.GT.MAXSIZ) THEN
      WRITE(*,6000) MAXSIZ
      WRITE(I7,6000) MAXSIZ
6000  FORMAT(1X,'COMPOSITE BUDGET FILE WILL EXCEED SIZE LIMIT OF ',I10,
     1' BYTES.'/
     21X,'SPECIFY A LARGER VALUE FOR <MAXSIZ> IN THE MAIN DATA FILE,'/
     31X,'OR, INCREASE DEFAULT VALUE OF <MAXSIZ> IN MAIN PROGRAM'/)
      CLOSE(IU)
      STOP
      END IF
C
C... WRITE TIME STEP HEADER
      NREC=NREC+1
      WRITE(IU,REC=NREC) KKPER,KKSTP,DELT,TOTSIM,NNSTP
C... WRITE QX FLOWS
      DO 10 K=1,NLAY
      DO 10 I=1,NROW
      NREC=NREC+1
      WRITE(IU,REC=NREC) (QX(J,I,K),J=1,NCP1)
10    CONTINUE
C... WRITE QY FLOWS
      DO 20 K=1,NLAY
      DO 20 I=1,NRP1
      NREC=NREC+1
      WRITE(IU,REC=NREC) (QY(J,I,K),J=1,NCOL)
20    CONTINUE
C... WRITE QZ FLOWS
      DO 30 K=1,NLP1
      DO 30 I=1,NROW
      NREC=NREC+1
      WRITE(IU,REC=NREC) (QZ(J,I,K),J=1,NCOL)
30    CONTINUE
C... WRITE QSS FLOWS
      DO 40 K=1,NLAY
      DO 40 I=1,NROW
      NREC=NREC+1
      WRITE(IU,REC=NREC) (QSS(J,I,K),J=1,NCOL)
40    CONTINUE
C... WRITE QSTO FLOWS
      DO 50 K=1,NLAY
      DO 50 I=1,NROW
      NREC=NREC+1
      WRITE(IU,REC=NREC) (QSTO(J,I,K),J=1,NCOL)
50    CONTINUE
C... WRITE IBOUND
      DO 60 K=1,NLAY
      DO 60 I=1,NROW
      NREC=NREC+1
      WRITE(IU,REC=NREC) (IBOUND(J,I,K),J=1,NCOL)
60    CONTINUE
C... WRITE HEAD
      DO 70 K=1,NLAY
      IF (LAYCON(K).NE.0) THEN
      LAYER=K
      DO 75 I=1,NROW
      NREC=NREC+1
      WRITE(IU,REC=NREC) (HEAD(J,I,K),J=1,NCOL),LAYER
75    CONTINUE
      END IF
70    CONTINUE
C
C... CHECK NUMBER OF RECORDS WRITTEN
      IF(NREC.NE.NCHECK) THEN
      WRITE(*,*) 'ERROR IN WRITING COMPOSITE BUDGET FILE. STOP.'
      WRITE(*,5000) NREC,NCHECK
5000  FORMAT(1X,'NREC=',I10,'    NCHECK=',I10)
      STOP
      END IF
C
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE READHQ(QX,QY,QZ,QSS,QSTO,HEAD,IBOUND,BUFF,NCOL,
     1 NROW,NLAY,NCP1,NRP1,NLP1,IU,NSTEP,DELT,TOTSIM,IERR,NUMTS,
     2 NPER,NSTEPF,NSTEPL,KPER,KSTP,IOPT,I7)
C
      DIMENSION QX(NCP1,NROW,NLAY),QY(NCOL,NRP1,NLAY),
     1QZ(NCOL,NROW,NLP1),QSS(NCOL,NROW,NLAY),QSTO(NCOL,NROW,NLAY),
     2HEAD(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY),
     3NUMTS(NPER)
C
      INTEGER KKPER,KKSTP,NNSTP,LAYER
C
      IERR=0
C
      READ(IU,REC=1,ERR=9000) NHLAY,NSTPSV
C
      NRPTS= (6*NROW*NLAY) + (NROW*NHLAY) + NROW + NLAY
C
C... READ TIME STEP HEADERS FOR FIRST AND LAST STEPS & COMPUTE STEP NUMB
C    RETURN WITH ERROR CODE = 1 IF REQUESTED STEP IS OUT OF RANGE
      READ(IU,REC=2) IPER,ISTP,DELT,TOTSIM,NNSTP
      CALL GETSTP(IPER,ISTP,NUMTS,NPER,I7,NSTEPF)
      NREC= 2 + (NSTPSV-1)*(NRPTS+1)
      READ(IU,REC=NREC) IPER,ISTP,DELT,TOTSIM,NNSTP
      CALL GETSTP(IPER,ISTP,NUMTS,NPER,I7,NSTEPL)
      IF(IOPT.EQ.0) RETURN
      IF(IOPT.EQ.2) CALL GETSTP(KPER,KSTP,NUMTS,NPER,I7,NSTEP)
      IF(NSTEP.LT.NSTEPF .OR. NSTEP.GT.NSTEPL) THEN
      IERR=1
      RETURN
      END IF
 
      N=NSTEP - NSTEPF + 1
C... READ TIME STEP HEADER
      NREC= 2 + (N-1)*(NRPTS+1)
      READ(IU,REC=NREC) KKPER,KKSTP,DELT,TOTSIM,NNSTP
      IPER=KKPER
      ISTP=KKSTP
      IF(IOPT.EQ.2) THEN
      IF(KPER.NE.IPER .OR. KSTP.NE.ISTP) THEN
      IERR=2
      RETURN
      END IF
      END IF
      KPER=IPER
      KSTP=ISTP
C
C... READ QX FLOWS
      DO 10 K=1,NLAY
      DO 10 I=1,NROW
      NREC=NREC+1
      READ(IU,REC=NREC) (QX(J,I,K),J=1,NCP1)
10    CONTINUE
C... READ QY FLOWS
      DO 20 K=1,NLAY
      DO 20 I=1,NRP1
      NREC=NREC+1
      READ(IU,REC=NREC) (QY(J,I,K),J=1,NCOL)
20    CONTINUE
C... READ QZ FLOWS
      DO 30 K=1,NLP1
      DO 30 I=1,NROW
      NREC=NREC+1
      READ(IU,REC=NREC) (QZ(J,I,K),J=1,NCOL)
30    CONTINUE
C... READ QSS FLOWS
      DO 40 K=1,NLAY
      DO 40 I=1,NROW
      NREC=NREC+1
      READ(IU,REC=NREC) (QSS(J,I,K),J=1,NCOL)
40    CONTINUE
C... READ QSTO FLOWS
      DO 50 K=1,NLAY
      DO 50 I=1,NROW
      NREC=NREC+1
      READ(IU,REC=NREC) (QSTO(J,I,K),J=1,NCOL)
50    CONTINUE
C... READ IBOUND
      DO 60 K=1,NLAY
      DO 60 I=1,NROW
      NREC=NREC+1
      READ(IU,REC=NREC) (IBOUND(J,I,K),J=1,NCOL)
60    CONTINUE
C... READ HEAD
      DO 77 K=1,NLAY
      DO 77 I=1,NROW
      DO 77 J=1,NCOL
77    HEAD(J,I,K)=0.0
      DO 70 N=1,NHLAY
      DO 75 I=1,NROW
      NREC=NREC+1
      READ(IU,REC=NREC) (BUFF(J,I,1),J=1,NCOL),LAYER
      K=LAYER
      DO 76 J=1,NCOL
76    HEAD(J,I,K)=BUFF(J,I,1)
75    CONTINUE
70    CONTINUE
C
C
      RETURN
9000  WRITE(*,*) 'ERROR READING COMPOSITE BUDGET FILE. STOP.'
      STOP
      END
 
C***** SUBROUTINE *****
      SUBROUTINE FACTYP (N,NBOUND,JNXT,INXT,KNXT,NBD,IBOUND,NCOL,NROW,
     1 NLAY,J,I,K,IFACE,TEXT,IO)
      DIMENSION IBOUND (NCOL,NROW,NLAY)
      CHARACTER*(*) TEXT
C
      NBD=0
      IF(N.EQ.NBOUND) THEN
      NBD=1
      ELSE
      IF (IBOUND(JNXT,INXT,KNXT).EQ.0) THEN
      NBD=1
      ELSE
      WRITE (IO,5000) IFACE,J,I,K,TEXT
5000  FORMAT('FACE',I2,' OF (J,I,K) = ',I4,',',I4,',',I4,
     1' IS NOT A BOUNDARY FACE. ',A,' FLOW TREATED AS SOURCE/SINK TERM')
      END IF
      END IF
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE FLOWS (QX,QY,QZ,BUFF,IUNIT,IBOUND,IBUFF,QSTO,
     1 DELR,DELC,NCOL,NROW,NLAY,NCP1,NRP1,NLP1,NUNIT,QSS,I7,
     2 KPER,KSTP,IRCHTP,IEVTTP,ISSFLG)
C
      DIMENSION DELC(NROW),DELR(NCOL),QX(NCP1,NROW,NLAY),
     1 QY(NCOL,NRP1,NLAY),QZ(NCOL,NROW,NLP1),BUFF(NCOL,NROW,NLAY),
     2 IUNIT(NUNIT),IBUFF(NCOL,NROW,NLAY),ISSFLG(KPER),
     3 IBOUND(NCOL,NROW,NLAY),QSS(NCOL,NROW,NLAY),QSTO(NCOL,NROW,NLAY)
      CHARACTER*16 TEXT
C
C
C  ZERO ARRAY CONTAINING NET FLOW RATE TO SOURCES & SINKS
C
      DO 1 K=1,NLAY
      DO 1 I=1,NROW
      DO 1 J=1,NCOL
1     QSS(J,I,K)=0.0
C 
C
C  GENERATE PROCESSED FACE FLOW TERMS
C
      WRITE(I7,1100) KPER,KSTP
      IUCBC=IUNIT(7)
C
C  READ STORAGE IF STRESS PERIOD IS TRANSIENT
C
      IF(ISSFLG(KPER).EQ.0) THEN
         CALL RDBUDG (QSTO,TEXT,NCOL,NROW,NLAY,IUCBC,I7,KPER,KSTP,
     1           IBUFF)
         IF(TEXT.NE.'         STORAGE') GO TO 100
      ELSE
         DO 2 K=1,NLAY
         DO 2 I=1,NROW
         DO 2 J=1,NCOL
2        QSTO(J,I,K)=0.0
      END IF
C
C  READ CONSTANT HEAD FLOWS
C
      CALL RDBUDG (BUFF,TEXT,NCOL,NROW,NLAY,IUCBC,I7,KPER,KSTP,
     1           IBUFF)
      IF(TEXT.NE.'   CONSTANT HEAD') GO TO 100
C
C  POTENTIAL DISCHARGE CELLS ARE FLAGGED IN THE IBOUND ARRAY
C  BY MULTIPLYING IBOUND VALUE BY 1000. THIS LOOP FLAGS CH
C  CELLS THAT HAVE NET DISCHARGE.
C
      DO 10 K=1,NLAY
      DO 10 I=1,NROW
      DO 10 J=1,NCOL
      IF(IBOUND(J,I,K).GE.0) GO TO 10
      QSS(J,I,K)=BUFF(J,I,K)
      IF(BUFF(J,I,K).LE.0.0) IBOUND(J,I,K)=1000*IBOUND(J,I,K)
10    CONTINUE
C
C  READ X FACE FLOWS
C
      IF(NCOL.GT.1) THEN
         CALL RDBUDG (BUFF,TEXT,NCOL,NROW,NLAY,IUCBC,I7,KPER,KSTP,
     1           IBUFF)
         IF(TEXT.NE.'FLOW RIGHT FACE ') GO TO 100
      ELSE
         CALL ZERO (BUFF,NCOL,NROW,NLAY)
      END IF
      DO 20 K=1,NLAY
      DO 20 I=1,NROW
      QX(1,I,K)= 0.0
      DO 20 J=1,NCOL
20    QX(J+1,I,K)= BUFF(J,I,K)
C
C  READ Y FACE FLOWS
C
      IF(NROW.GT.1) THEN
         CALL RDBUDG (BUFF,TEXT,NCOL,NROW,NLAY,IUCBC,I7,KPER,KSTP,
     1           IBUFF)
         IF(TEXT.NE.'FLOW FRONT FACE ') GO TO 100
      ELSE
         CALL ZERO (BUFF,NCOL,NROW,NLAY)
      END IF
      DO 30 K=1,NLAY
      DO 30 J=1,NCOL
      QY(J,1,K)= 0.0
      DO 30 I=1,NROW
30    QY(J,I+1,K)= -BUFF(J,I,K)
C
C  READ Z FACE FLOWS
C
      IF(NLAY.GT.1) THEN
         CALL RDBUDG (BUFF,TEXT,NCOL,NROW,NLAY,IUCBC,I7,KPER,KSTP,
     1           IBUFF)
         IF(TEXT.NE.'FLOW LOWER FACE ') GO TO 100
      ELSE
         CALL ZERO (BUFF,NCOL,NROW,NLAY)
      END IF
      DO 40 I=1,NROW
      DO 40 J=1,NCOL
      QZ(J,I,1)= 0.0
      DO 40 K=1,NLAY
40    QZ(J,I,K+1)= -BUFF(J,I,K)
      WRITE(I7,*) '  FLOW RATES BETWEEN CELLS HAVE BEEN READ'
C
C
C  Stress flows
C  Read a flow term
50    CALL RDBDNM(TEXT,IUCBC,I7,KPER,KSTP,NLAY,NROW,NCOL,
     1       NBTYPE,NVAL,NIFACE,NLST)
      IF(TEXT.EQ.'END DATA') RETURN
      IF(TEXT.EQ.'EARLY DATA') THEN
         WRITE(I7,*) 'Budget file is out of sync. with MODPATH'
         STOP
      END IF
C
C  Process the flow term
      IF(NBTYPE.EQ.2 .OR. NBTYPE.EQ.5) THEN
         CALL ADDQLS(IUCBC,NVAL,NIFACE,NLST,QSS,QX,QY,QZ,
     1           DELR,DELC,IBOUND,I7,TEXT)
      ELSE
         ITOP=0
         IF(TEXT.EQ.'        RECHARGE') ITOP=IRCHTP
         IF(TEXT.EQ.'              ET') ITOP=IEVTTP
         CALL ADDQAR(IUCBC,NBTYPE,BUFF,QSS,QZ,
     1            IBOUND,I7,TEXT,ITOP,IBUFF)
      END IF
C
C  Process next term
      GO TO 50
C     
C
C 
100   WRITE(I7,*)
     1  'DATA IN MODFLOW BUDGET FILE IS MISSING OR OUT OF ORDER. STOP'
      STOP
C
1000  FORMAT(1X,A,' DATA BEING READ FOR STRESS PERIOD ',I4,' , TIME STEP
     1 ',I4)
1100  FORMAT(1X,'READING FLOW PACKAGE BUDGET DATA FOR STRESS PERIOD ',
     1   I4,' , TIME STEP ',I4,' ...')
      END
C
C***** SUBROUTINE *****
      SUBROUTINE HEADS(HEAD,BUFF,IUNIT,IBOUND,LAYCON,IPER,ISTP,
     1             HDRY,HNOFLO)
C  Read heads for one time step
C
      INCLUDE 'idat1.inc'
      DIMENSION HEAD(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY),
     1    IUNIT(NUNIT),IBOUND(NCOL,NROW,NLAY),LAYCON(NLAY)
      CHARACTER*16 TEXT
C     ------------------------------------------------------------------
C
C  READ HEADS
C
      DO 110 K=1,NLAY
      DO 110 I=1,NROW
      DO 110 J=1,NCOL
110   HEAD(J,I,K)=0.
C
 
      IF(IUNIT(8).NE.0 .AND. IUNIT(11).NE.0) THEN
         WRITE(I7,*)
     1'CANNOT SPECIFIY BOTH FORMATTED AND UNFORMATTED HEAD FILES. STOP'
         STOP
      END IF
C
      IUHED=0
      IF(IUNIT(8).NE.0) THEN
         IUHED= -IUNIT(8)
      ELSE IF(IUNIT(11).NE.0) THEN
         IUHED=IUNIT(11)
      END IF
C
C  read heads if there is a file unit for heads
      IF (IUHED.NE.0) THEN
C
         WRITE(I7,*) ' '
         write(i7,*) ' ---------------'
C
         IUHEDA=IUHED
         IF(IUHEDA.LT.0) IUHEDA= -IUHEDA
         IHTYPE=0
         IF(NROW.EQ.1) CALL HEDFMT(IHTYPE,IUHED,I7,IUHEDA)
C
C
C... IF TRANSIENT, FIND FIRST LAYER THAT IS NOT A STANDARD CONFINED
C      LAYER (LAYCON=0). TRANSIENT SIMULATIONS ONLY READ HEADS FOR
C      LAYERS WITH LAYCON>0 OR LAYCON<0. LAYCON<0 IS A SPECIAL FLAG
C      DENOTING A CONFINED LAYER THAT SHOULD HAVE HEADS READ AND
C      SAVED IN THE COMPOSITE BUDGET FILE OF A TRANSIENT SIMULATION.
         IF(ISS.EQ.0) THEN
            LAYER=0
            DO 115 N=1,NLAY
            IF(LAYCON(N).NE.0) THEN
               LAYER=N
               GO TO 116
            END IF
115         CONTINUE
116         CONTINUE
            IF(LAYER.EQ.0) GO TO 130
         END IF
C
         TEXT= 'HEAD'
C
C  If head is stored by layer in standard style, read layer by layer
         IF(IHTYPE.EQ.0) THEN
C
120         CONTINUE
            CALL GETLAY(BUFF,KSTP,KPER,PERTIM,TOTIM,TEXT,NC,NR,K,
     1                NCOL,NROW,IUHED,I7,IEND,0)
            IF(IEND.EQ.1) GO TO 130
C     SET LAYER=K FOR STEADY STATE SIMULATIONS. I.E. READ ANY HEADS
C       THAT ARE IN THE FILE.
            IF(ISS.EQ.1) LAYER=K
C
            IF(KPER.EQ.IPER .AND. KSTP.EQ.ISTP .AND. K.EQ.LAYER) THEN
               WRITE(I7,5160) K,KPER,KSTP
5160    FORMAT(' HEADS WERE READ FOR LAYER',I4,', STRESS PERIOD',I4,
     1         ' TIME STEP',I5)
               DO 121 I=1,NROW
               DO 121 J=1,NCOL
121            HEAD(J,I,K)= BUFF(J,I,1)
               IF(LAYCON(K).GT.0) THEN
                  CALL MODIBN(HEAD,HDRY,HNOFLO,IBOUND,K,NCOL,NROW,NLAY)
               END IF
               N2=NLAY-1
C
C... IF TRANSIENT, FIND NEXT LAYER THAT MUST HAVE HEAD SAVED AND SET
C      LAYER EQUAL TO THAT VALUE.
               IF(ISS.EQ.0) THEN
                  LAYER=0
                  IF(K.LT.NLAY) THEN
                     DO 122 N=K,N2
                     IF(LAYCON(N+1).NE.0) THEN
                        LAYER=N+1
                        GO TO 124
                     END IF
122                  CONTINUE
124                  CONTINUE
                  END IF
               END IF
C
            END IF
C
            IF(LAYER.NE.0) GO TO 120
C
C  Otherwise, read head for a single cross section
         ELSE IF(IHTYPE.EQ.1) THEN
            CALL GETLAY(BUFF,KSTP,KPER,PERTIM,TOTIM,TEXT,NC,NR,K,
     1            NCOL,NLAY,IUHED,I7,IEND,0)
            IF(IEND.EQ.1) GO TO 130
            DO 125 KK=1,NLAY
            DO 125 JJ=1,NCOL
125         HEAD(JJ,1,KK)=BUFF(JJ,1,KK)
            DO 126 KK=1,NLAY
            IF(LAYCON(KK).GT.0) CALL MODIBN(HEAD,HDRY,HNOFLO,
     1                   IBOUND,KK,NCOL,NROW,NLAY)
126         CONTINUE
         END IF
C
      END IF
130   CONTINUE
      WRITE(I7,*) 'HEADS HAVE BEEN READ'
C
C
      RETURN
      END

C***** SUBROUTINE *****
      SUBROUTINE FLUXBC (Q,QSS,QX,QY,IBOUND,DELR,DELC,J,I,K,NCOL,
     1NROW,NLAY,NCP1,NRP1)
C
      DIMENSION DELR(NCOL),DELC(NROW),IBOUND(NCOL,NROW,NLAY),
     1 QSS(NCOL,NROW,NLAY),QX(NCP1,NROW,NLAY),QY(NCOL,NRP1,NLAY)
      DIMENSION QSIDE(4)
C
      SUM=0.0E+0
      DO 10 N=1,4
10    QSIDE(N)=0.0
C
      IF(J.EQ.1) THEN
      SUM=SUM+DELC(I)
      QSIDE(1)=DELC(I)
      ELSE IF(IBOUND(J-1,I,K).EQ.0) THEN
      SUM=SUM+DELC(I)
      QSIDE(1)=DELC(I)
      END IF
C
      IF(J.EQ.NCOL) THEN
      SUM=SUM+DELC(I)
      QSIDE(2)=DELC(I)
      ELSE IF (IBOUND(J+1,I,K).EQ.0) THEN
      SUM=SUM+DELC(I)
      QSIDE(2)=DELC(I)
      END IF
C
      IF(I.EQ.1) THEN
      SUM=SUM+DELR(J)
      QSIDE(4)=DELR(J)
      ELSE IF (IBOUND(J,I-1,K).EQ.0) THEN
      SUM=SUM+DELR(J)
      QSIDE(4)=DELR(J)
      END IF
C
      IF(I.EQ.NROW) THEN
      SUM=SUM+DELR(J)
      QSIDE(3)=DELR(J)
      ELSE IF (IBOUND(J,I+1,K).EQ.0) THEN
      SUM=SUM+DELR(J)
      QSIDE(3)=DELR(J)
      END IF
C
      ASUM= ABS(SUM)
      IF (ASUM.LT.1.0E-20) THEN
      QSS(J,I,K)=QSS(J,I,K) + Q
      IF(Q.LE.0.0.AND.IBOUND(J,I,K).LT.1000)
     1  IBOUND(J,I,K)=1000*IBOUND(J,I,K)
      RETURN
      END IF
C
      DO 20 N=1,4
20    QSIDE(N)= Q*(QSIDE(N)/SUM)
C
      QX(J,I,K)= QX(J,I,K) + QSIDE(1)
      QX(J+1,I,K)= QX(J+1,I,K) - QSIDE(2)
      QY(J,I+1,K)= QY(J,I+1,K) + QSIDE(3)
      QY(J,I,K)= QY(J,I,K) - QSIDE(4)
C
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE CBFHED(HEAD,NCOL,NROW,NLAY,IU,NSTEP,IERR,NUMTS,
     2 NPER,NSTEPF,NSTEPL,KPER,KSTP,IOPT,IUSUM)
C
      DIMENSION HEAD(NCOL,NROW,NLAY),NUMTS(NPER)
C
      INTEGER KKPER,KKSTP,NNSTP
C
      IERR=0
C
      READ(IU,REC=1,ERR=9000) NHLAY,NSTPSV
C
      NRPTS= (6*NROW*NLAY) + (NROW*NHLAY) + NROW + NLAY
C
C... READ TIME STEP HEADERS FOR FIRST AND LAST STEPS & COMPUTE STEP NUMB
C    RETURN WITH ERROR CODE = 1 IF REQUESTED STEP IS OUT OF RANGE
      READ(IU,REC=2) IPER,ISTP,DELT,TOTSIM,NNSTP
      CALL GETSTP(IPER,ISTP,NUMTS,NPER,IUSUM,NSTEPF)
      NREC= 2 + (NSTPSV-1)*(NRPTS+1)
      READ(IU,REC=NREC) IPER,ISTP,DELT,TOTSIM,NNSTP
      CALL GETSTP(IPER,ISTP,NUMTS,NPER,IUSUM,NSTEPL)
      IF(IOPT.EQ.0) RETURN
      IF(IOPT.EQ.2) CALL GETSTP(KPER,KSTP,NUMTS,NPER,IUSUM,NSTEP)
      IF(NSTEP.LT.NSTEPF .OR. NSTEP.GT.NSTEPL) THEN
      IERR=1
      RETURN
      END IF
 
      N=NSTEP - NSTEPF + 1
C... READ TIME STEP HEADER
      NREC= 2 + (N-1)*(NRPTS+1)
      READ(IU,REC=NREC) KKPER,KKSTP,DELT,TOTSIM,NNSTP
      IPER=KKPER
      ISTP=KKSTP
      IF(IOPT.EQ.2) THEN
      IF(KPER.NE.IPER .OR. KSTP.NE.ISTP) THEN
      IERR=2
      RETURN
      END IF
      END IF
      KPER=IPER
      KSTP=ISTP
 
C... SKIP OVER EVERTHING ELSE TO GET TO HEAD DATA
      NREC=NREC + (6*NROW*NLAY) + NROW + NLAY
 
C... READ HEAD
      DO 10 K=1,NLAY
      DO 10 I=1,NROW
      DO 10 J=1,NCOL
10    HEAD(J,I,K)=0.0
c
      DO 20 N=1,NHLAY
      DO 30 I=1,NROW
      NREC=NREC+1
      READ(IU,REC=NREC) (HEAD(J,I,NLAY),J=1,NCOL),K
      DO 40 J=1,NCOL
40    HEAD(J,I,K)=HEAD(J,I,NLAY)
      IF(N.EQ.NHLAY .AND. K.NE.NLAY) THEN
        DO 45 II=1,NROW
        DO 45 JJ=1,NCOL
45      HEAD(JJ,II,NLAY)=0.0
      END IF
30    CONTINUE
20    CONTINUE
C
      RETURN
9000  WRITE(IUSUM,*) 'ERROR READING COMPOSITE BUDGET FILE. STOP.'
      STOP
      END
 
C***** SUBROUTINE *****
      SUBROUTINE RSETIB(IBOUND,IBSTRT,NCOL,NROW,NLAY)
      DIMENSION IBOUND(NCOL,NROW,NLAY),IBSTRT(NCOL,NROW,NLAY)
C
      DO 10 K=1,NLAY
      DO 10 I=1,NROW
      DO 10 J=1,NCOL
        IF(IBOUND(J,I,K).LT.0) THEN
        IBOUND(J,I,K)= -1
        ELSE IF(IBSTRT(J,I,K).EQ.0) THEN
        IBOUND(J,I,K)=0
        ELSE
        IBOUND(J,I,K)=1
        END IF
10    CONTINUE
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE MODIBN(HEAD,HDRY,HNOFLO,IBOUND,K,NCOL,NROW,NLAY)
      DIMENSION HEAD(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY)
C
      DO 140 I=1,NROW
      DO 140 J=1,NCOL
      IF (HEAD(J,I,K).EQ.HDRY .OR. HEAD(J,I,K).EQ.HNOFLO) THEN
      IBOUND(J,I,K)=0
      END IF
140   CONTINUE
      RETURN
      END
 
C***** SUBROUTINE *****
      SUBROUTINE CBFSIZ(NFSV,NLSV,LAYCON,NROW,NCOL,NLAY,MAXSIZ,
     1                  IERR)
      DIMENSION LAYCON(NLAY)
 
      IERR=0
 
      NHLAY=0
      DO 1 N=1,NLAY
1     IF(LAYCON(N).GT.0) NHLAY=NHLAY+1
 
      NSTEPS= NLSV-NFSV+1
 
      NRPTS= (6*NROW*NLAY) + (NROW*NHLAY) + NROW + NLAY
      NREC= (1 + (1+NRPTS)*NSTEPS)
      NBYTES= 4*(NCOL+1)*NREC
 
      IF(NBYTES.LE.MAXSIZ) THEN
        RETURN
      ELSE
        SIZKB= FLOAT(NBYTES)/1024.0
        SIZMB= SIZKB/1024.0
        IF(SIZKB.LT.500.0) THEN
          WRITE(*,1000) NBYTES,SIZKB
        ELSE
          WRITE(*,1100) NBYTES,SIZMB
        END IF
        write(*,*) ' '
        WRITE(*,*) ' YOU CAN CONTINUE OR STOP NOW.'
        WRITE(*,*) ' SELECT AN OPTION:'
        WRITE(*,*) '      1 = CONTINUE'
        WRITE(*,*) '      2 = STOP NOW, DO NOT GENERATE THE FILE'
        READ(*,*) IANS
        IF(IANS.EQ.1) THEN
          MAXSIZ=NBYTES
        ELSE
          IERR=1
        END IF
      END IF
1000  FORMAT(1X,'THIS RUN WILL GENERATE A COMPOSITE BUDGET FILE THAT CON
     1TAINS:'/1X,I10,' BYTES  (',F8.2,' Kb)')
1100  FORMAT(1X,'THIS RUN WILL GENERATE A COMPOSITE BUDGET FILE THAT CON
     1TAINS:'/1X,I10,' BYTES  (',F8.2,' Mb)')
      RETURN
      END
 
 
      SUBROUTINE HEDFMT(IHTYPE,IUHED,IO,IU)
      CHARACTER*16 TEXT
      CHARACTER*132 LINE
      IHTYPE=0
      IF(IUHED.LT.0) THEN
        READ(-IUHED) KSTP,KPER,PERTIM,TOTIM,TEXT,NC,NR,K
        IF(K.LT.0) IHTYPE=1
        REWIND(-IUHED)
      ELSE
        READ(IUHED,'(A)') LINE
        ICOL=1
        CALL URWORD(LINE,ICOL,IWFRST,IWLAST,2,KSTP,RDUMMY,IO,IU)
        CALL URWORD(LINE,ICOL,IWFRST,IWLAST,2,KPER,RDUMMY,IO,IU)
        CALL URWORD(LINE,ICOL,IWFRST,IWLAST,3,IDUMMY,PERTIM,IO,IU)
        CALL URWORD(LINE,ICOL,IWFRST,IWLAST,3,IDUMMY,TOTIM,IO,IU)
        CALL URWORD(LINE,ICOL,IWFRST,IWLAST,1,IDUMMY,RDUMMY,IO,IU)
        CALL URWORD(LINE,ICOL,IWFRST,IWLAST,2,NC,RDUMMY,IO,IU)
        CALL URWORD(LINE,ICOL,IWFRST,IWLAST,2,NR,RDUMMY,IO,IU)
        CALL URWORD(LINE,ICOL,IWFRST,IWLAST,2,K,RDUMMY,IO,IU)
        CALL URWORD(LINE,ICOL,IWFRST,IWLAST,1,IDUMMY,RDUMMY,IO,IU)
        IF(K.LT.0) IHTYPE=1
        REWIND(IUHED)
      END IF
      RETURN
      END
      SUBROUTINE ADDQLS(IU,NVAL,NIFACE,NLST,QSS,QX,QY,QZ,
     1            DELR,DELC,IBOUND,IO,TEXT)
C     ******************************************************************
C     Add a list budget term to QSS or QX, QY, and QZ
C     ******************************************************************
      INCLUDE 'idat1.inc'
      DIMENSION QSS(NCOL,NROW,NLAY),QX(NCP1,NROW,NLAY),
     1 QY(NCOL,NRP1,NLAY),QZ(NCOL,NROW,NLP1),DELR(NCOL),DELC(NROW),
     2 IBOUND(NCOL,NROW,NLAY)
      DIMENSION VAL(6)
      CHARACTER*(*) TEXT
C     ------------------------------------------------------------------
      NRC=NROW*NCOL
      IFACE=0
      IF(NLST.GT.0) THEN
         DO 100 N=1,NLST
         READ(IU,ERR=1000) ICELL,(VAL(I),I=1,NVAL)
         Q=VAL(1)
         K= (ICELL-1)/NRC + 1
         I= ( (ICELL - (K-1)*NRC)-1 )/NCOL + 1
         J= ICELL - (K-1)*NRC - (I-1)*NCOL
         IF(NIFACE.GT.0) IFACE=VAL(NIFACE)
C
         IB=IBOUND(J,I,K)
         IF(IFACE.LT.0) THEN
            CALL FLUXBC(Q,QSS,QX,QY,IBOUND,DELR,DELC,J,I,K,
     1              NCOL,NROW,NLAY,NCP1,NRP1)
C
         ELSE IF(IFACE.EQ.0.OR.IFACE.GT.6) THEN
            IF(Q.LT.0.0.AND.ABS(IB).LT.1000) IBOUND(J,I,K)= 1000*IB
            QSS(J,I,K)=QSS(J,I,K) + Q
C
         ELSE IF(IFACE.EQ.1) THEN
            CALL FACTYP(J,1,J-1,I,K,NBD,IBOUND,NCOL,NROW,NLAY,J,I,K,
     1                 IFACE,TEXT,I7)
            IF (NBD.EQ.1) THEN
               QX(J,I,K)= QX(J,I,K) + Q
            ELSE
               IF(Q.LT.0.0.AND.ABS(IB).LT.1000) IBOUND(J,I,K)= 1000*IB
               QSS(J,I,K)=QSS(J,I,K) + Q
            END IF
C
         ELSE IF(IFACE.EQ.2) THEN
            CALL FACTYP(J,NCOL,J+1,I,K,NBD,IBOUND,NCOL,NROW,NLAY,J,I,K,
     1                      IFACE,TEXT,I7)
            IF (NBD.EQ.1) THEN
            QX(J+1,I,K)= QX(J+1,I,K) - Q
            ELSE
            IF(Q.LT.0.0.AND.ABS(IB).LT.1000) IBOUND(J,I,K)= 1000*IB
            QSS(J,I,K)=QSS(J,I,K) + Q
            END IF
C
         ELSE IF(IFACE.EQ.3) THEN
            CALL FACTYP(I,NROW,J,I+1,K,NBD,IBOUND,NCOL,NROW,NLAY,J,I,K,
     1                      IFACE,TEXT,I7)
            IF (NBD.EQ.1) THEN
               QY(J,I+1,K)= QY(J,I+1,K) + Q
            ELSE
               IF(Q.LT.0.0.AND.ABS(IB).LT.1000) IBOUND(J,I,K)= 1000*IB
               QSS(J,I,K)=QSS(J,I,K) + Q
            END IF
C
         ELSE IF(IFACE.EQ.4) THEN
            CALL FACTYP(I,1,J,I-1,K,NBD,IBOUND,NCOL,NROW,NLAY,J,I,K,
     1                     IFACE,TEXT,I7)
            IF (NBD.EQ.1) THEN
               QY(J,I,K)= QY(J,I,K) - Q
            ELSE
               IF(Q.LT.0.0.AND.ABS(IB).LT.1000) IBOUND(J,I,K)= 1000*IB
               QSS(J,I,K)=QSS(J,I,K) + Q
            END IF
C
         ELSE IF(IFACE.EQ.5) THEN
            CALL FACTYP(K,NLAY,J,I,K+1,NBD,IBOUND,NCOL,NROW,NLAY,J,I,K,
     1                     IFACE,TEXT,I7)
            IF (NBD.EQ.1) THEN
               QZ(J,I,K+1)= QZ(J,I,K+1) + Q
            ELSE
               IF(Q.LT.0.0.AND.ABS(IB).LT.1000) IBOUND(J,I,K)= 1000*IB
               QSS(J,I,K)=QSS(J,I,K) + Q
            END IF
C
         ELSE IF(IFACE.EQ.6) THEN
            CALL FACTYP(K,1,J,I,K-1,NBD,IBOUND,NCOL,NROW,NLAY,J,I,K,
     1                   IFACE,TEXT,I7)
            IF (NBD.EQ.1) THEN
               QZ(J,I,K)= QZ(J,I,K) - Q
            ELSE
               IF(Q.LT.0.0.AND.ABS(IB).LT.1000) IBOUND(J,I,K)= 1000*IB
               QSS(J,I,K)=QSS(J,I,K) + Q
            END IF
         END IF
100      CONTINUE
      END IF
C
      RETURN
C
1000  WRITE(IO,*) ' ADDQLS -- error reading CBC budget file'
      STOP
      END
      SUBROUTINE ADDQAR(IU,NBTYPE,BUFF,QSS,QZ,IBOUND,IO,TEXT,ITOP,IBUFF)
C     ******************************************************************
C     Add an array budget term to QSS or QZ
C     ******************************************************************
      INCLUDE 'idat1.inc'
      DIMENSION QSS(NCOL,NROW,NLAY),QZ(NCOL,NROW,NLP1),
     1 IBOUND(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY),IBUFF(NCOL,NROW,NLAY)
      CHARACTER*(*) TEXT
C     ------------------------------------------------------------------
      IF(NBTYPE.EQ.4) THEN
         NL=1
         READ(IU,ERR=1000) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
      ELSE IF(NBTYPE.EQ.3) THEN
         NL=1
         READ(IU,ERR=1000) ((IBUFF(J,I,1),J=1,NCOL),I=1,NROW)
         READ(IU,ERR=1000) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
      ELSE
         NL=NLAY
         READ(IU,ERR=1000) BUFF
      END IF
C
      DO 100 KK=1,NL
      K=KK
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
      IF(NBTYPE.EQ.3) K=IBUFF(J,I,1)
      IF(BUFF(J,I,KK).NE.0.0) THEN
         IF(ITOP.EQ.0) THEN
            IF(BUFF(J,I,KK).LT.0.0.AND.ABS(IBOUND(J,I,K)).LT.1000)
     1                      IBOUND(J,I,K)= 1000*IBOUND(J,I,K)
            QSS(J,I,K)=QSS(J,I,K) + BUFF(J,I,KK)
         ELSE
            CALL FACTYP(K,1,J,I,K-1,NBD,IBOUND,NCOL,NROW,NLAY,J,I,K,6,
     1            TEXT,IO)
            IF(NBD.EQ.1) THEN
               QZ(J,I,K)=QZ(J,I,K) - BUFF(J,I,KK)
            ELSE
               IF(BUFF(J,I,KK).LT.0.0.AND.ABS(IBOUND(J,I,K)).LT.1000)
     1                      IBOUND(J,I,K)= 1000*IBOUND(J,I,K)
               QSS(J,I,K)=QSS(J,I,K) + BUFF(J,I,KK)
            END IF
         END IF
      END IF
C
100   CONTINUE
C
      RETURN
C
1000  WRITE(IO,*) ' ADDQAR -- error reading CBC budget file'
      STOP
      END
