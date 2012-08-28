      SUBROUTINE MP6MPSIM1RP()
C***********************************************************************
C  Read and Prepare MODPATH simulation file data
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE GLOBAL, ONLY:IZONE,RFAC,LAYCBD,LBOTM,NROW,NCOL,NLAY
      USE MPDATA, ONLY:ITRACKDIR,IOLIST,INMPSIM,ISIMTYPE,FENDPT,FPLINE,
     1            FTSERS,ISTOPT,STOPTIME,REFTIME,NTPTS,TIMEPTS,IBDOPT,
     2            NCELLBUD,IDTRACE,IOTRACE,IOBUDCHK,NGRIDS,BUDGETCELLS,
     3            IOEPT,IOPART,ISINK,ISOURCE,ISTOPZONE,ISTOPZONE2,
     4            IADVOBS,FADVOB,IOAOBS
      USE PARTICLEDATA, ONLY:NPGROUPS,PGROUPS
      INTEGER ::N,IRELEASE,NRP,I,J,K,NT,IMIN,IMAX,JMIN,JMAX,IDMAX,GRID
      INTEGER ::KMIN,KMAX,NI,NJ,NK,NG,NPART,NCELLS,IU,IPGOPT,ITPOPT
      INTEGER ::IRTOPT,IRZONE,IRRFAC,KPER,KSTP,ITEMP
      INTEGER, DIMENSION(3,6) ::IFO
      REAL ::RPLEN,RTIME,TINC,T,FRAC
      CHARACTER (LEN=16) ::TEXT
      CHARACTER (LEN=200) ::FNAME,FBUDCHK
      CHARACTER*24 ANAME(3)
      DATA ANAME(1) /'              ZONE ARRAY'/
      DATA ANAME(2) /'                 RFACTOR'/
      DATA ANAME(3) /' RFACTOR OF QUASI-3D BED'/
C-----------------------------------------------------------------------
      FENDPT = ''
      FPLINE = ''
      FTSERS = ''
      IU = INMPSIM
      IADVOBS = 1
      
C     OPTION FLAGS: 
C     ISIMTYPE:  SIMULATION TYPE
C     ITRACKDIR: TRACKING DIRECTION
C     ISINK:     WEAK SINK OPTION
C     ISOURCE:   WEAK SOURCE OPTION
C     IRTOPT:    REFERENCE TIME OPTION
C     ISTOPT:    STOPPING TIME OPTION
C     IPGOPT:    PARTICLE GENERATION OPTION
C     ITPOPT:    TIME POINTS OPTION
C     IBDOPT:    BUDGET OUTPUT OPTION
C     IRZONE:    ZONE DATA OPTION
C     IRRFAC:    RETARDATION FACTOR OPTION
C     IADVOBS:   ADVECTIVE OBSERVATIONS OPTION
      READ(IU,*) ISIMTYPE,ITRACKDIR,ISINK,ISOURCE,IRTOPT,ISTOPT,
     1           IPGOPT,ITPOPT,IBDOPT,IRZONE,IRRFAC,IADVOBS
      WRITE(IOLIST,*)

C     SIMULATION TYPE
      SELECT CASE(ISIMTYPE)
        CASE(1)
          WRITE(IOLIST,'(A,I2,A)')
     1      'ENDPOINT ANALYSIS (ISIMTYPE =',ISIMTYPE,')'
          READ(IU,*) FENDPT
          ICOL=1
          CALL URWORD(FENDPT,ICOL,ISTART,ISTOP,0,N,R,0,0)
          FENDPT=FENDPT(ISTART:ISTOP)
        CASE(2)
          WRITE(IOLIST,'(A,I2,A)')
     1      'PATHLINE ANALYSIS (ISIMTYPE =',ISIMTYPE,')'
          READ(IU,*) FENDPT
          ICOL=1
          CALL URWORD(FENDPT,ICOL,ISTART,ISTOP,0,N,R,0,0)
          FENDPT=FENDPT(ISTART:ISTOP)
          READ(IU,*) FPLINE
          ICOL=1
          CALL URWORD(FPLINE,ICOL,ISTART,ISTOP,0,N,R,0,0)
          FPLINE=FPLINE(ISTART:ISTOP)
        CASE(3)
          WRITE(IOLIST,'(A,I2,A)')
     1      'TIME SERIES ANALYSIS (ISIMTYPE =',ISIMTYPE,')'
          READ(IU,*) FENDPT
          ICOL=1
          CALL URWORD(FENDPT,ICOL,ISTART,ISTOP,0,N,R,0,0)
          FENDPT=FENDPT(ISTART:ISTOP)
          READ(IU,*) FTSERS
          ICOL=1
          CALL URWORD(FTSERS,ICOL,ISTART,ISTOP,0,N,R,0,0)
          FTSERS=FTSERS(ISTART:ISTOP)
          IF(IADVOBS.EQ.2) THEN
            READ(IU,*) FADVOB
            ICOL=1
            CALL URWORD(FADVOB,ICOL,ISTART,ISTOP,0,N,R,0,0)
            FADVOB=FADVOB(ISTART:ISTOP)
          END IF
        CASE DEFAULT
          CALL USTOP('INVALID SIMULATION TYPE. STOP.')
      END SELECT

C     TRACKING DIRECTION      
      SELECT CASE(ITRACKDIR)
        CASE(1)
          WRITE(IOLIST,'(A,I2,A)') 
     1   'FORWARD TRACKING (ITRACKDIR = ',ITRACKDIR,')'
        CASE(2)
          WRITE(IOLIST,'(A,I2,A)') 
     1    'BACKWARD TRACKING (ITRACKDIR =',ITRACKDIR,')'
        CASE DEFAULT
          CALL USTOP('INVALID TRACKING DIRECTION CODE. STOP.')
      END SELECT
      
C     WEAK SINK OPTION
      SELECT CASE(ISINK)
        CASE(1)
          WRITE(IOLIST,'(A)') 
     1      'LET PARTICLES PASS THROUGH WEAK SINK CELLS (ISINK = 1)'
        CASE(2)
          WRITE(IOLIST,'(A)') 
     1    'STOP PARTICLES WHEN THEY ENTER WEAK SINK CELLS. (ISINK = 2)'
        CASE DEFAULT
          CALL USTOP('INVALID WEAK SINK OPTION.')
      END SELECT

C     WEAK SOURCE OPTION      
      SELECT CASE(ISOURCE)
        CASE(1)
          WRITE(IOLIST,'(A,A)') 
     1    'LET PARTICLES PASS THROUGH WEAK SOURCE CELLS FOR ',
     2    'BACKTRACKING SIMULATIONS (ISOURCE = 1)'
        CASE(2)
          WRITE(IOLIST,'(A,A)')
     1    'STOP PARTICLES WHEN THEY ENTER WEAK SOURCE CELLS ',
     2    'FOR BACKTRACKING SIMULATIONS (ISOURCE = 2)'
        CASE DEFAULT
          CALL USTOP('INVALID WEAK SOURCE OPTION.')
       END SELECT
      
C     ADVECTIVE OBSERVATIONS OPTION
      SELECT CASE(IADVOBS)
        CASE(1)
          WRITE(IOLIST,'(A,A,I2,A)')
     1    'DO NOT SAVE ADVECTIVE OBSERVATIONS FOR TIME SERIES ',
     2    'SIMULATIONS (IADVOBS = ',IADVOBS,')'
        CASE(2)
          WRITE(IOLIST,'(A,A,I2,A)')
     1    'SAVE ADVECTIVE OBSERVATIONS FOR TIME SERIES ',
     2    'SIMULATIONS (IADVOBS = ',IADVOBS,')'
        CASE DEFAULT
          CALL USTOP('INVALID ADVECTION OBSERVATIONS CODE. STOP.')
      END SELECT
 
C     OPEN PARTICLE OUTPUT FILES     
      WRITE(IOLIST,*)
      WRITE(IOLIST,'(A)') 'PARTICLE OUTPUT FILES:'
      
C     OPEN ENDPOINT FILE      
      OPEN(UNIT=IOEPT,FILE=FENDPT,FORM='FORMATTED',
     1     ACCESS='SEQUENTIAL',ACTION='WRITE',STATUS='REPLACE')
      WRITE(IOLIST,'(A,A)') 'ENDPOINT FILE:  ',FENDPT
      
C     OPEN PATHLINE OR TIME SERIES FILE
      IF(ISIMTYPE.EQ.2) THEN
        OPEN(UNIT=IOPART,FILE=FPLINE,FORM='FORMATTED',
     1    ACCESS='SEQUENTIAL',ACTION='WRITE',STATUS='REPLACE')
        WRITE(IOLIST,'(A,A)') 'PATHLINE FILE:  ',FPLINE
      END IF
      
      IF(ISIMTYPE.EQ.3) THEN
        OPEN(UNIT=IOPART,FILE=FTSERS,FORM='FORMATTED',
     1    ACCESS='SEQUENTIAL',ACTION='WRITE',STATUS='REPLACE')
        WRITE(IOLIST,'(A,A)') 'TIME SERIES FILE:  ',FTSERS
        IF(IADVOBS.EQ.2) THEN
          OPEN(UNIT=IOAOBS,FILE=FADVOB,FORM='FORMATTED',
     1    ACCESS='SEQUENTIAL',ACTION='WRITE',STATUS='REPLACE')
          WRITE(IOLIST,'(A,A)') 'ADVECTIVE OBSERVATIONS FILE:  ',FADVOB
        END IF
      END IF
      
C     REFERENCE TIME OPTION (IRTOPT)
      WRITE(IOLIST,*) 
      SELECT CASE(IRTOPT)
        CASE(1)
          READ(IU,*) REFTIME
          WRITE(IOLIST,'(A,E15.7)') 'REFERENCE TIME = ',REFTIME
        CASE(2)
          READ(IU,*) KPER,KSTP,FRAC
          CALL FINDREFTIME(KPER,KSTP,FRAC,REFTIME)
          WRITE(IOLIST,'(A,I6,A,I6,A,F10.7)')
     1    'REFERENCE TIME CALCULATED FOR: KPER =',KPER,'  KSTP =',KSTP,
     2    '  RELATIVE POSITION WITHIN TIME STEP =',FRAC
          WRITE(IOLIST,'(A,E15.7)') 'COMPUTED REFERENCE TIME = ',REFTIME
        CASE DEFAULT
          CALL USTOP('INVALID REFERENCE TIME INPUT OPTION.')
      END SELECT

C     READ STOPPING OPTION (ISTOPT)
      STOPTIME = 1.0E+30
      SELECT CASE(ISTOPT)
        CASE(1)
          WRITE(IOLIST,'(A,A,I2,A)')
     2    'STOP TRACKING AT THE BEGINNING OR END OF THE',
     3    ' MODFLOW SIMULATION (ISTOPT = ',ISTOPT,')'
        CASE(2)
          WRITE(IOLIST,'(A,A,I2,A)')
     1    'EXTEND INITIAL OR FINAL STEADY-STATE TIMESTEP AND ',
     2    'CONTINUE TRACKING (ISTOPT = ',ISTOPT,')'
        CASE(3)
          WRITE(IOLIST,'(A,I2,A)')
     1    'SPECIFY A LIMIT FOR TRACKING TIME (ISTOPT = ',ISTOPT,
     2    ')'
          READ(IU,*) STOPTIME
          WRITE(IOLIST,'(A,E15.7)') 'STOP TIME = ',STOPTIME
        CASE DEFAULT
          CALL USTOP('INVALID STOP TIME CODE. STOP.')
      END SELECT
      
C     PARTICLE INPUT            
      SELECT CASE(IPGOPT)
        CASE (1)
C         READ NUMBER OF PARTICLE GROUPS
          READ(IU,*) NPGROUPS
          WRITE(IOLIST,'(A,I5)') 'NUMBER OF PARTICLE GROUPS = ',NPGROUPS
          
          IDMAX = 0
          IF(NPGROUPS.GT.0) THEN
            ALLOCATE(PGROUPS(NPGROUPS))
            DO NG=1,NPGROUPS
              PGROUPS(NG)%GROUP = NG
              CALL READPARTICLEGROUP(IU,NG,IDMAX)
            END DO
          END IF
          
        CASE (2)
          CALL READSTARTINGLOC(IU)
          
        CASE DEFAULT
          CALL USTOP('INVALID PARTICLE GENERATION OPTION (IPGOPT)')
          
      END SELECT
      
C     COMPUTE TOTAL NUMBER OF PARTICLES. ALSO ALLOCATE AND INITIALIZE
C     ADVECTIVE OBSERVATIONS
      NPART = 0
      IF(NPGROUPS.GT.0) THEN
        DO NG=1,NPGROUPS
          NPART = NPART + PGROUPS(NG)%COUNT
          IF(ISIMTYPE.EQ.3 .AND. IADVOBS.EQ.2) THEN
            DO N=1,PGROUPS(NG)%COUNT
              ALLOCATE(PGROUPS(NG)%PARTICLES(N)%EXITVEL(3))
              PGROUPS(NG)%PARTICLES(N)%EXITVEL(1) = 0.0
              PGROUPS(NG)%PARTICLES(N)%EXITVEL(2) = 0.0
              PGROUPS(NG)%PARTICLES(N)%EXITVEL(3) = 0.0
            END DO
          END IF
        END DO
      END IF  
      
      WRITE(IOLIST,*)
      WRITE(IOLIST,'(A,I10)') 
     1 'TOTAL NUMBER OF PARTICLES FOR ALL GROUPS = ',NPART
       
C     TIME POINTS
C       (ITPOPT: 1 = NO TIME POINTS; 2 = CONSTANT INTERVAL; 3 = READ FROM FILE)
      NTPTS = 0
      IF(ISIMTYPE.EQ.1 .AND. ITPOPT.GT.1) THEN
        CALL USTOP('ITPOPT MUST EQUAL 1 FOR ENDPOINT SIMULATION.')
      END IF
      
      SELECT CASE(ITPOPT)
        CASE(1)
C         NO TIME POINTS SPECIFIED
          NTPTS = 0
          ALLOCATE(TIMEPTS(0:NTPTS))        
        CASE(2)     
C         READ NUMBER OF TIME POINTS TO COMPUTE AND THE TIME INCREMENT
          READ(IU,*) NTPTS
          READ(IU,*) TINC
          ALLOCATE(TIMEPTS(0:NTPTS))
          TIMEPTS(0) = 0.0
          IF(NTPTS .GT. 0) THEN
            DO N = 1,NTPTS
              TIMEPTS(N) = TIMEPTS(N-1) + TINC
            END DO
          END IF
        CASE(3)
C         READ A LIST OF TIME POINTS
          READ(IU,*) NTPTS
          ALLOCATE(TIMEPTS(0:NTPTS))
          TIMEPTS(0) = 0.0
          IF(NTPTS .GT. 0) THEN
            READ(IU,*) (TIMEPTS(N), N=1,NTPTS)
          END IF
          WRITE(IOLIST,*)
          WRITE(IOLIST,'(I6,A)') NTPTS,
     1      'Time points will be read from a list.'
          WRITE(IOLIST,'(10E15.5)') (TIMEPTS(N),N=1,NTPTS)
        CASE DEFAULT
          NTPTS = 0
          ALLOCATE(TIMEPTS(0:NTPTS))
      END SELECT

C     BUDGET OUTPUT
C        (IBDOPT: 1 = NO BUDGET OUTPUT,  2 = SUMMARY ONLY, 3 = CELL LIST, 4 = TRACE MODE)
      IDTRACE = 0
      NCELLBUD = 0
      SELECT CASE(IBDOPT)
        CASE(1)
          WRITE(IOLIST,*)
          WRITE(IOLIST,'(A,I2,A)') 
     1    'DO NOT PERFORM BUDGET CHECKS (IBDOPT = ',IBDOPT,')'
        CASE(2)
          WRITE(IOLIST,*)
          WRITE(IOLIST,'(A,A,I2,A)') 
     1    'CHECK CELL BUDGETS FOR ALL CELLS AND PRINT SUMMARY ',
     2    '(IBDOPT = ',IBDOPT,')'
        CASE(3)
          WRITE(IOLIST,*)
          WRITE(IOLIST,'(A,I2,A)') 
     1    'CHECK CELL BUDGETS FOR SPECIFIED CELLS (IBDOPT = ',
     2    IBDOPT,')'
          READ(IU,*) NCELLBUD
          WRITE(IOLIST,'(A,I4,a)') 'NUMBER OF CELL BUDGETS = ',
     1      NCELLBUD,':'
          ALLOCATE(BUDGETCELLS(0:NCELLBUD))
          WRITE(IOLIST,*)
          WRITE(IOLIST,'(A)') '     BUDGET CELLS'
          WRITE(IOLIST,'(A)') 'GRID  ROW  COLUMN  LAYER'
          DO N=1,NCELLBUD
            READ(IU,*) GRID,K,I,J
            WRITE(IOLIST,'(I4,I5,I8,I7)') GRID,K,I,J
            BUDGETCELLS(N)%GRID = GRID
            BUDGETCELLS(N)%I = I
            BUDGETCELLS(N)%J = J
            BUDGETCELLS(N)%K = K
          END DO
        CASE(4)
          READ(IU,*) FNAME
          ICOL=1
          CALL URWORD(FNAME,ICOL,ISTART,ISTOP,0,N,R,0,0)
          FNAME=FNAME(ISTART:ISTOP)
          OPEN(UNIT=IOTRACE,FILE=FNAME,STATUS='REPLACE',
     1         FORM='FORMATTED',ACCESS='SEQUENTIAL')
          READ(IU,*) IDTRACE
          WRITE(IOLIST,*)
          WRITE(IOLIST,'(A,I10)') 
     1      'GENERATE A BUDGET TRACE FOR PARTICLE ',IDTRACE
          WRITE(IOLIST,'(A,A)') 
     1      'TRACE DATA WILL BE WRITTEN TO FILE: ',FNAME
        CASE DEFAULT
        
      END SELECT

C     ZONE ARRAY
      ISTOPZONE = 0
      ISTOPZONE2 = 0
      IF(IRZONE.GT.1) THEN
        WRITE(IOLIST,*)
        READ(IU,*) ISTOPZONE
        IF(ISTOPZONE.LT.1) THEN
          ISTOPZONE = 0
          ISTOPZONE2 = 0
          WRITE(IOLIST,'(A)') 
     1    'Particles will be allowed to pass through all zones.'
        ELSE
          ISTOPZONE2 = ISTOPZONE
          WRITE(IOLIST,'(A,A,I5)') 
     1    'Particles will be terminated when they enter cells ',
     2    'with a zone numbers equal to ',ISTOPZONE
        END IF
        DO IGRID=1,NGRIDS
          CALL SELECTGRID(IGRID)
          DO K=1,NLAY
          KK=K
          CALL U2DINT(IZONE(:,:,KK),ANAME(1),NROW,NCOL,KK,IU,IOLIST)
          END DO
        END DO
      ELSE
        WRITE(IOLIST,*)
        WRITE(IOLIST,'(A)') 'THE ZONE VALUE FOR ALL CELLS = 1'
        DO IGRID=1,NGRIDS
          CALL SELECTGRID(IGRID)
          IZONE = 1
        END DO
      END IF
      
C     RETARDATION FACTOR ARRAY
      IF(IRRFAC.GT.1) THEN
        DO IGRID=1,NGRIDS
          CALL SELECTGRID(IGRID)
          DO K=1,NLAY
          KK=K
          CALL U2DREL(RFAC(:,:,LBOTM(K)),ANAME(2),NROW,NCOL,KK,IU,
     1          IOLIST)
          IF(LAYCBD(K).NE.0) CALL U2DREL(RFAC(:,:,LBOTM(K)+1),ANAME(3),
     1          NROW,NCOL,KK,INMPBAS,IOUT)
          END DO
        END DO
      ELSE
        WRITE(IOLIST,*)
        WRITE(IOLIST,'(A)')'NO RETARDATION. FACTOR = 1 FOR ALL CELLS'
        DO IGRID=1,NGRIDS
          CALL SELECTGRID(IGRID)
          RFAC = 1.0
        END DO
      END IF

      RETURN
      END

            
      
      SUBROUTINE MP6MPBAS1AR(IIN,CUNIT,IUDIS,IGRID,HEADING,
     1                       VERSION,MFVNAM)
C***********************************************************************
C  Allocate and Read MODPATH Basic Package(MPBAS)
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE GLOBAL, ONLY:NIUNIT,NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,
     1                ITMUNI,LENUNI,IXSEC,ITRSS,
     2                IFREFM,NODES,IUNIT,HEAD,LBOTM,LAYCBD,LAYTYP,
     3                PERLEN,NSTP,NBFPOS,TSMULT,ISSFLG,DELR,DELC,
     4                BOTM,IBOUND,IZONE,POR,BUFF,IBUFF,QX,QY,
     5                QZ,QSINK,QSOURCE,QSTO,FILENAME,RFAC,IFACEASP,
     6                NAREALSP,IGRIDNUM,IBSTART,CAREALSP

      USE MPBAS, ONLY:MSUM,MAXCBF,ISCHILD,NPRBEG,NPREND,
     1                      NPCBEG,NPCEND,NPLBEG,NPLEND,NCPP,
     2                      IRCHTOP,IEVTTOP,DELT,PERTIM,TOTIM,
     3                      HNOFLO,HDRY,NCPPL

      USE MPDATA, ONLY:NTSTEPS,NTPER,NTSTP,NTOFF,SIMTIME,
     1                 IUMPBAS,IOLIST
      
      INTEGER M,N,NSTEPS,KP,KS,IUOC,IIN,IOUT,INBAS,INMPBAS
      INTEGER IVAL,ICOL,ISTART,ISTOP,IERROR,LENGTH,IFACE
      REAL RVAL
      CHARACTER*16 CUNIT(NIUNIT)
      CHARACTER*80 HEADNG(2)
      CHARACTER*200 LINE
      CHARACTER(LEN=5) CODE
      CHARACTER(LEN=*) ::VERSION
      CHARACTER(LEN=*) ::MFVNAM
      CHARACTER(LEN=200) ::MESSAGE
      CHARACTER(LEN=200) ::FMPLIST
      CHARACTER(LEN=16) TXT
      DOUBLE PRECISION HNF
      CHARACTER*24 ANAME(4)
      DATA ANAME(1) /'          BOUNDARY ARRAY'/
      DATA ANAME(2) /'            INITIAL HEAD'/
      DATA ANAME(3) /'                POROSITY'/
      DATA ANAME(4) /'POROSITY OF QUASI-3D BED'/
C-----------------------------------------------------------------------
      
C-----Allocate scalars
      ALLOCATE(IGRIDNUM)
      IGRIDNUM= IGRID      
      ALLOCATE(NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD)
      ALLOCATE(ITMUNI,LENUNI,IXSEC,ITRSS)
      ALLOCATE(IFREFM,NODES)
      ALLOCATE(MSUM,MAXCBF,ISCHILD,NPRBEG,NPREND)
      ALLOCATE(NPCBEG,NPCEND,NPLBEG,NPLEND,NCPP)
      ALLOCATE(IRCHTOP,IEVTTOP,DELT,PERTIM,TOTIM)
      ALLOCATE(HNOFLO,HDRY)
      ALLOCATE(NAREALSP)
      
C-----Allocate unit number and file name array
      ALLOCATE(IUNIT(NIUNIT))
      ALLOCATE(FILENAME(NIUNIT))
      
C-----Read and open files in NAME file for grid IGRID
      CALL SMP6MPBAS1OPEN(IGRID,IIN,IOLIST,IUNIT,CUNIT,
     1              NIUNIT,VERSION)
      INMPBAS = IUNIT(IUMPBAS)
      
C5------Allocate and read discretization data.
      CALL SMP6MPBAS1ARDIS(IUDIS,IOLIST)
      NODES=NCOL*NROW*NLAY
C
C6------Allocate space for global arrays except discretization data.
      ALLOCATE (HEAD(NCOL,NROW,NLAY))
      ALLOCATE (IBOUND(NCOL,NROW,NLAY))
      ALLOCATE (IBSTART(NCOL,NROW,NLAY))
      ALLOCATE (IZONE(NCOL,NROW,NLAY))
      ALLOCATE (BUFF(NCOL,NROW,NLAY))
      ALLOCATE (IBUFF(NCOL,NROW,NLAY))
      ALLOCATE (QX(NCOL+1,NROW,NLAY))
      ALLOCATE (QY(NCOL,NROW+1,NLAY))
      ALLOCATE (QZ(NCOL,NROW,NLAY+1))
      ALLOCATE (QSINK(NCOL,NROW,NLAY))
      ALLOCATE (QSOURCE(NCOL,NROW,NLAY))
      ALLOCATE (QSTO(NCOL,NROW,NLAY))
      ALLOCATE (POR(NCOL,NROW,NBOTM))
      ALLOCATE (RFAC(NCOL,NROW,NBOTM))
      ALLOCATE (LAYTYP(NLAY))
C
      IF(IGRID.EQ.1) THEN
        NTSTEPS=0
        DO N=1,NPER
          NTSTEPS=NTSTEPS + NSTP(N)
        END DO
        ALLOCATE(NTPER(NTSTEPS))
        ALLOCATE(NTSTP(NTSTEPS))
        ALLOCATE(SIMTIME(0:NTSTEPS))
        ALLOCATE(NTOFF(NPER))
        N=0
        NTOFF(1)=0
        DO KP=1,NPER
          IF(KP.GT.1) NTOFF(KP)=NTOFF(KP-1) + NSTP(KP-1)
          DO KS=1,NSTP(KP)
            N=N+1
            NTPER(N)=KP
            NTSTP(N)=KS
          END DO
        END DO
        SIMTIME(0) = 0.0
      END IF

C
C-----READ MPBAS DATA
C-----READ AND PRINT COMMENTS.  SAVE THE FIRST TWO COMMENTS IN HEADNG.
      HEADNG(1)=' '
      HEADNG(2)=' '
      WRITE(IOLIST,*)
      READ(INMPBAS,'(A)') LINE
      IF(LINE(1:1).NE.'#') GO TO 20
      HEADNG(1)=LINE(1:80)
      WRITE(IOLIST,'(1X,A)') HEADNG(1)
      READ(INMPBAS,'(A)') LINE
      IF(LINE(1:1).NE.'#') GO TO 20
      HEADNG(2)=LINE(1:80)
      WRITE(IOLIST,'(1X,A)') HEADNG(2)
      CALL URDCOM(INMPBAS,IOLIST,LINE)
20    CONTINUE   
         
      READ(LINE,*) HNOFLO,HDRY
      WRITE(IOLIST,3) HNOFLO
    3 FORMAT(1X,/1X,'AQUIFER HEAD IS SET TO ',1PG11.5,
     1       ' AT ALL NO-FLOW NODES (IBOUND=0).')
      WRITE(IOLIST,'(1X,A,1PG11.5,A)') 
     1'AQUIFER HEAD IS SET TO ',HDRY,' AT ALL DRY NODES.'
      WRITE(IOLIST,*)
      
C     READ NUMBER OF STRESS PACKAGES THAT DO NOT HAVE AUXILIARY IFACE SUPPORT
      READ(INMPBAS,*) NAREALSP
      ALLOCATE (IFACEASP(NAREALSP))
      ALLOCATE (CAREALSP(NAREALSP))
      
      IF(NAREALSP.GT.0) THEN
C       READ BUDGET LABELS AND DEFAULT IFACE
        DO N=1,NAREALSP
          READ(INMPBAS,'(A)') LINE
          CALL UTRIMALL(LINE)
          LENGTH=LEN_TRIM(LINE)
          IF(LENGTH.EQ.0) THEN
            CALL USTOP(
     1      'Stress package labels cannot be blank strings.')
          ELSE
            IF(LENGTH.GT.16) LENGTH = 16
            CAREALSP(N) = LINE
            CALL UPCASE(CAREALSP(N))
          END IF

C         READ DEFAULT IFACE          
          READ(INMPBAS,*) IFACEASP(N)
          
        END DO

C       Check for duplicate labels        
        DO N=1,NAREALSP
          TXT = CAREALSP(N)
          DO M=1,NAREALSP
            IF(M.NE.N) THEN
              IF(TXT .EQ. CAREALSP(M)) THEN
               CALL USTOP(
     1         'Duplicate stress package labels are not allowed.')
              END IF
            END IF
          END DO
        END DO
        
        WRITE(IOLIST,*)
        WRITE(IOLIST,'(1X,A,A)') 'Default stress package ',
     1  'boundary face options (IFACE):'
        IF(NAREALSP.EQ.0) THEN
          WRITE(IOLIST,'(3X,A)') 'None were specified'
        ELSE
          DO N=1,NAREALSP
            IFACE = IFACEASP(N)
            IF(IFACE.EQ.0) THEN
              WRITE(IOLIST,'(3X,A,A)') TRIM(CAREALSP(N)),
     1        ' will be treated as internal stresses (IFACE = 0)'
            ELSE IF(IFACE.LT.0 .OR. IFACE.GT.6) THEN
              CALL USTOP(
     1        ' IFACE must have a value between 0 and 6.')
            ELSE
              WRITE(IOLIST,'(3X,A,A,I2)') TRIM(CAREALSP(N)),
     1        ' will be assigned to face ',IFACE
            END IF
          END DO
        END IF
        
      END IF

C     LAYTYP
      WRITE(IOLIST,*)
      WRITE(IOLIST,'(1X,A)') 'LAYER TYPE(LAYTYP)'
      READ(INMPBAS,*) (LAYTYP(K),K=1,NLAY)
      WRITE(IOLIST,'(1X,80I2)') (LAYTYP(K),K=1,NLAY)
      DO K = 1,NLAY
        IF(LAYTYP(K).LT.0) LAYTYP(K) = 1
      END DO

C8E-----READ INITIAL BOUNDARY ARRAY(IBOUND).
      DO 280 K=1,NLAY
      KK=K
      CALL U2DINT(IBOUND(:,:,KK),ANAME(1),NROW,NCOL,KK,INMPBAS,IOLIST)
  280 CONTINUE
      CALL SET3DARRAYI(IBOUND,IBSTART,NCOL,NROW,NLAY)

C     POROSITY
      DO K=1,NLAY
      KK=K
      CALL U2DREL(POR(:,:,LBOTM(K)),ANAME(3),NROW,NCOL,KK,INMPBAS,
     1          IOLIST)
      IF(LAYCBD(K).NE.0) CALL U2DREL(POR(:,:,LBOTM(K)+1),ANAME(4),
     1          NROW,NCOL,KK,INMPBAS,IOLIST)
      END DO
      
C
C-----SAVE POINTER DATA
      CALL SMP6MPBAS1PSV(IGRID)

      RETURN
      END
      
      SUBROUTINE SMP6MPBAS1ARDIS(IUDIS,IOUT)
C***********************************************************************
C  Allocate and read DIS data
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,ITMUNI,
     1                 LENUNI,IUNIT,LBOTM,LAYCBD,ITRSS,PERLEN,
     3                 NSTP,TSMULT,ISSFLG,DELR,DELC,BOTM,NBFPOS,
     4                 XMIN,YMIN
C
      CHARACTER*200 LINE
      CHARACTER*24 ANAME(5)
      DATA ANAME(1) /'                    DELR'/
      DATA ANAME(2) /'                    DELC'/
      DATA ANAME(3) /'TOP ELEVATION OF LAYER 1'/
      DATA ANAME(4) /'  MODEL LAYER BOTTOM EL.'/
      DATA ANAME(5) /'BOT. EL. OF QUASI-3D BED'/
C-----------------------------------------------------------------------

C1------Check for existence of discretization file
      INDIS=IUNIT(IUDIS)
      IF(INDIS.LE.0) THEN
         WRITE(IOUT,*) ' DIS file must be specified for MODFLOW to run'
         CALL USTOP(' ')
      END IF
      WRITE(IOUT,11) INDIS
   11 FORMAT(1X,/1X,'DISCRETIZATION INPUT DATA READ FROM UNIT ',I4)
C
C
C2------Read comments and the first line following the comments.
      CALL URDCOM(INDIS,IOUT,LINE)
C
C3------Get the number of layers, rows, columns, stress periods,
C3------ITMUNI, and LENUNI from the line.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLAY,R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NROW,R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCOL,R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPER,R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMUNI,R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,LENUNI,R,IOUT,INDIS)
C
C4------PRINT # OF LAYERS, ROWS, COLUMNS AND STRESS PERIODS.
      WRITE(IOUT,15) NLAY,NROW,NCOL
   15 FORMAT(1X,I4,' LAYERS',I10,' ROWS',I10,' COLUMNS')
      WRITE(IOUT,20) NPER
   20 FORMAT(1X,I4,' STRESS PERIOD(S) IN SIMULATION')
C
C5------SELECT AND PRINT A MESSAGE SHOWING TIME UNIT.
      IF(ITMUNI.LT.0 .OR. ITMUNI.GT.5) ITMUNI=0
      IF(ITMUNI.EQ.0) THEN
         WRITE(IOUT,30)
   30    FORMAT(1X,'MODEL TIME UNIT IS UNDEFINED')
      ELSE IF(ITMUNI.EQ.1) THEN
         WRITE(IOUT,40)
   40    FORMAT(1X,'MODEL TIME UNIT IS SECONDS')
      ELSE IF(ITMUNI.EQ.2) THEN
         WRITE(IOUT,50)
   50    FORMAT(1X,'MODEL TIME UNIT IS MINUTES')
      ELSE IF(ITMUNI.EQ.3) THEN
         WRITE(IOUT,60)
   60    FORMAT(1X,'MODEL TIME UNIT IS HOURS')
      ELSE IF(ITMUNI.EQ.4) THEN
         WRITE(IOUT,70)
   70    FORMAT(1X,'MODEL TIME UNIT IS DAYS')
      ELSE
         WRITE(IOUT,80)
   80    FORMAT(1X,'MODEL TIME UNIT IS YEARS')
      END IF
C
C6------SELECT AND PRINT A MESSAGE SHOWING LENGTH UNIT.
      IF(LENUNI.LT.0 .OR. LENUNI.GT.3) LENUNI=0
      IF(LENUNI.EQ.0) THEN
         WRITE(IOUT,90)
   90    FORMAT(1X,'MODEL LENGTH UNIT IS UNDEFINED')
      ELSE IF(LENUNI.EQ.1) THEN
         WRITE(IOUT,91)
   91    FORMAT(1X,'MODEL LENGTH UNIT IS FEET')
      ELSE IF(LENUNI.EQ.2) THEN
         WRITE(IOUT,93)
   93    FORMAT(1X,'MODEL LENGTH UNIT IS METERS')
      ELSE IF(LENUNI.EQ.3) THEN
         WRITE(IOUT,95)
   95    FORMAT(1X,'MODEL LENGTH UNIT IS CENTIMETERS')
      END IF
C
C7------ALLOCATE LAYER FLAGS.
      ALLOCATE(LBOTM(NLAY))
      ALLOCATE(LAYCBD(NLAY))
C
C8------Read confining bed information
      READ(INDIS,*) (LAYCBD(K),K=1,NLAY)
      LAYCBD(NLAY)=0
      WRITE(IOUT,*) ' Confining bed flag for each layer:'
      WRITE(IOUT,'(20I4)') (LAYCBD(K),K=1,NLAY)
C
C9------Count confining beds, setup the pointer to each layer's
C9------bottom array (LBOTM), and setup LAYCBD to be the confining
C9------bed number for each layer.
      NCNFBD=0
      DO 100 K=1,NLAY
      LBOTM(K)=K+NCNFBD
      IF(LAYCBD(K).NE.0) THEN
         NCNFBD=NCNFBD+1
         LAYCBD(K)=NCNFBD
      END IF
  100 CONTINUE
      NBOTM=NLAY+NCNFBD
C
C10-----Allocate space for discretization arrays
C10-----Note that NBOTM+1 arrays are allocated for BOTM
C10-----because BOTM(J,I,0) contains the top elevation of layer 1.
      ALLOCATE (DELR(NCOL),XMIN(NCOL))
      ALLOCATE (DELC(NROW),YMIN(NROW))
      ALLOCATE (BOTM(NCOL,NROW,0:NBOTM))
      ALLOCATE (PERLEN(NPER),NSTP(NPER),TSMULT(NPER),ISSFLG(NPER))
C
C11-----Read the DELR and DELC arrays.
      CALL U1DREL(DELR,ANAME(1),NCOL,INDIS,IOUT)
      CALL U1DREL(DELC,ANAME(2),NROW,INDIS,IOUT)
C
C-------Compute XMIN and YMIN arrays
      XMIN(1) = 0.0
      DO J = 2, NCOL
        XMIN(J) = XMIN(J-1) + DELR(J-1)
      END DO
      
      YMIN(NROW) = 0.0
      DO I = NROW-1, 1, -1
        YMIN(I) = YMIN(I+1) + DELC(I+1)
      END DO
      
C
C12-----Read the top elevation of layer 1.
      CALL U2DREL(BOTM(:,:,0),ANAME(3),NROW,NCOL,0,INDIS,IOUT)
C
C13-----Read the bottom elevations.
      DO 120 K=1,NLAY
      KK=K
      CALL U2DREL(BOTM(:,:,LBOTM(K)),ANAME(4),NROW,NCOL,KK,INDIS,IOUT)
      IF(LAYCBD(K).NE.0) CALL U2DREL(BOTM(:,:,LBOTM(K)+1),ANAME(5),
     1          NROW,NCOL,KK,INDIS,IOUT)
  120 CONTINUE
C
C14-----READ AND WRITE LENGTH OF STRESS PERIOD, NUMBER OF TIME STEPS,
C14-----TIME STEP MULTIPLIER, AND STEADY-STATE FLAG..
      WRITE(IOUT,161)
  161 FORMAT(1X,//1X,'STRESS PERIOD     LENGTH       TIME STEPS',
     1            '     MULTIPLIER FOR DELT    SS FLAG',/1X,76('-'))
      ISS=0
      ITR=0
      NSTEPS=0
      DO 200 N=1,NPER
      READ(INDIS,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,PERLEN(N),IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSTP(N),R,IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TSMULT(N),IOUT,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,INDIS)
      NSTEPS=NSTEPS + NSTP(N)
      IF (LINE(ISTART:ISTOP).EQ.'TR') THEN
         ISSFLG(N)=0
         ITR=1
      ELSE IF (LINE(ISTART:ISTOP).EQ.'SS') THEN
         ISSFLG(N)=1
         ISS=1
      ELSE
         WRITE(IOUT,162)
  162    FORMAT(' SSFLAG MUST BE EITHER "SS" OR "TR"',
     1      ' -- STOP EXECUTION (SMP6MAIN1ARDIS)')
         CALL USTOP(' ')
      END IF
      WRITE (IOUT,163) N,PERLEN(N),NSTP(N),TSMULT(N),LINE(ISTART:ISTOP)
  163 FORMAT(1X,I8,1PG21.7,I7,0PF25.3,A11)
C
C15-----STOP IF NSTP LE 0, PERLEN EQ 0 FOR TRANSIENT STRESS PERIODS,
C15-----TSMULT LE 0, OR PERLEN LT 0..
      IF(NSTP(N).LE.0) THEN
         WRITE(IOUT,164)
  164    FORMAT(1X,/1X,
     1  'THERE MUST BE AT LEAST ONE TIME STEP IN EVERY STRESS PERIOD')
         CALL USTOP(' ')
      END IF
      ZERO=0.
      IF(PERLEN(N).EQ.ZERO .AND. ISSFLG(N).EQ.0) THEN
         WRITE(IOUT,165)
  165    FORMAT(1X,/1X,
     1  'PERLEN MUST NOT BE 0.0 FOR TRANSIENT STRESS PERIODS')
         CALL USTOP(' ')
      END IF
      IF(TSMULT(N).LE.ZERO) THEN
         WRITE(IOUT,170)
  170    FORMAT(1X,/1X,'TSMULT MUST BE GREATER THAN 0.0')
         CALL USTOP(' ')
      END IF
      IF(PERLEN(N).LT.ZERO) THEN
         WRITE(IOUT,175)
  175    FORMAT(1X,/1X,
     1  'PERLEN CANNOT BE LESS THAN 0.0 FOR ANY STRESS PERIOD')
         CALL USTOP(' ')
      END IF
  200 CONTINUE
      
C  Allocate arrays to hold the head file and budget file time step position pointers  
      ALLOCATE(NBFPOS(NSTEPS))
      
C
C16-----Assign ITRSS.
      IF(ISS.EQ.0 .AND. ITR.NE.0) THEN
         ITRSS=1
         WRITE(IOUT,270)
  270    FORMAT(/,1X,'TRANSIENT SIMULATION')
      ELSE IF(ISS.NE.0 .AND. ITR.EQ.0) THEN
         ITRSS=0
         WRITE(IOUT,275)
  275    FORMAT(/,1X,'STEADY-STATE SIMULATION')
      ELSE
         ITRSS=-1
         WRITE(IOUT,280)
  280    FORMAT(/,1X,'COMBINED STEADY-STATE AND TRANSIENT SIMULATION')
      END IF
C
C17-----RETURN.
      RETURN
      END
      
      SUBROUTINE SMP6MPBAS1OPEN(IGRID,IIN,IOUT,IUNIT,CUNIT,
     1              NIUNIT,VERSION)
C***********************************************************************
C  Read file information for the specified grid and open files.
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE GLOBAL, ONLY:FILENAME
      USE MPDATA, ONLY:IUMPBAS,IUDIS,IUHEAD,IUBUDGET,INMPSIM
      INCLUDE 'MP6Openspec.inc'
      INTEGER IIN,IOUT
      DIMENSION IUNIT(NIUNIT)
      CHARACTER*4 CUNIT(NIUNIT)
      CHARACTER*7 FILSTAT
      CHARACTER*20 FILACT, FMTARG, ACCARG
      CHARACTER*40 SPACES
      CHARACTER*300 LINE, FNAME
      CHARACTER*20 FILTYP
      CHARACTER (LEN=*) ::VERSION
      LOGICAL LOP
C-----------------------------------------------------------------------

C1------INITIALIZE CONSTANTS.
      NFILE=0
      IEND = 0
      DO 5 I=1,NIUNIT
      IUNIT(I)=0
5     CONTINUE
      SPACES=' '

      IF(IGRID.GT.1) THEN
          WRITE(IOUT,*) ' '
          WRITE(IOUT,'(A)')    
     1               '--------------------------------------------'
          WRITE(IOUT,'(A,I2)') 
     1               'Allocating and reading data for grid ',IGRID
          WRITE(IOUT,'(A)')    
     1               '--------------------------------------------'
      ENDIF
C
C2------READ A LINE; IGNORE BLANK LINES AND PRINT COMMENT LINES.
10    READ(IIN,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 10
      IF(LINE(1:4).EQ.'GRID') THEN
        IEND = 1
        GO TO 1000
      END IF
      
      IF(LINE(1:1).EQ.'#') THEN
        IF(NFILE.NE.0 .AND. IOUT.NE.0) WRITE(IOUT,'(A)') LINE
        GO TO 10
      END IF
C
C3------DECODE THE FILE TYPE, UNIT NUMBER, AND NAME.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUT,IIN)
      FILTYP=LINE(ITYP1:ITYP2)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,IIN)
      CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,IOUT,IIN)
      IFLEN=INAM2-INAM1+1
      FNAME(1:IFLEN)=LINE(INAM1:INAM2)
      INQUIRE(UNIT=IU,OPENED=LOP)
      IF(LOP) THEN
         IF(IOUT.EQ.0) THEN
            WRITE(*,11) FNAME(1:IFLEN),IU
   11       FORMAT(1X,/1X,'CANNOT OPEN ',A,' ON UNIT',I4,
     1              ' BECAUSE UNIT IS ALREADY BEING USED')
         ELSE
            WRITE(IOUT,11) FNAME(1:IFLEN),IU
         END IF
         CALL USTOP(' ')
      END IF
C
C5------SET DEFAULT FILE ATTRIBUTES.
      FMTARG='FORMATTED'
      ACCARG='SEQUENTIAL'
      FILSTAT='UNKNOWN'
      FILACT=' '
      IRECL = 4
C
C6------SPECIAL CHECK FOR 1ST FILE.
      IF(NFILE.EQ.0 .AND. IGRID.EQ.1) THEN
        WRITE(IOUT,*) ' '
        WRITE(IOUT,'(A)')    
     1             '--------------------------------------------'
        WRITE(IOUT,'(A,I2)') 
     1             'Allocating and reading data for grid ',IGRID
        WRITE(IOUT,'(A)')    
     1             '--------------------------------------------'
C  Get next file name
        NFILE=1
      END IF
C
C-------CHECK FOR "MPSIM" FILE TYPE IF IGRID = 1
      IF(FILTYP.EQ.'MPSIM' .AND. IGRID.EQ.1) THEN
         FILSTAT='OLD    '
         FILACT=ACTION(1)
         INMPSIM=IU
         FILENAME(5)(1:IFLEN) = FNAME(1:IFLEN)
C
C8------CHECK FOR "MPBAS" FILE TYPE.
      ELSE IF(FILTYP.EQ.'MPBAS') THEN
         IUNIT(IUMPBAS)=IU
         FILSTAT='OLD    '
         FILACT=ACTION(1)
C
C-------CHECK FOR "DIS" FILE TYPE
      ELSE IF(FILTYP.EQ.'DIS') THEN
         FILSTAT='OLD    '
         FILACT=ACTION(1)
         IUNIT(IUDIS)=IU
         FILENAME(2)(1:IFLEN) = FNAME(1:IFLEN)
C
C8A-----CHECK FOR BINARY HEAD FILE TYPE.
      ELSE IF(FILTYP.EQ.'HEAD') THEN
         FMTARG=FORM
         ACCARG=ACCESS
         FILSTAT='OLD    '
         FILACT=ACTION(1)
         IUNIT(IUHEAD)=IU
         FILENAME(3)(1:IFLEN) = FNAME(1:IFLEN)
C
C-------CHECK FOR "BUDGET" FILE TYPE
      ELSE IF(FILTYP.EQ.'BUDGET') THEN
         FMTARG=FORM
         ACCARG=ACCESS
         FILSTAT='OLD    '
         FILACT=ACTION(1)
         IUNIT(IUBUDGET)=IU
         FILENAME(4)(1:IFLEN) = FNAME(1:IFLEN)
C
C9------CHECK FOR "UNFORMATTED" FILE TYPE.
      ELSE IF(FILTYP.EQ.'DATA(BINARY)') THEN
         FMTARG=FORM
         ACCARG=ACCESS
C
C10-----CHECK FOR "FORMATTED" FILE TYPE.
      ELSE IF(FILTYP.EQ.'DATA') THEN
         FMTARG='FORMATTED'
         ACCARG='SEQUENTIAL'
C
C11-----CHECK FOR MAJOR OPTIONS.
      ELSE
        DO 20 I=1,NIUNIT
           IF(LINE(ITYP1:ITYP2).EQ.CUNIT(I)) THEN
              IUNIT(I)=IU
              FILSTAT='OLD    '
              FILACT=ACTION(1)
              GO TO 30
           END IF
20      CONTINUE
        WRITE(IOUT,21) LINE(ITYP1:ITYP2)
21      FORMAT(1X,'ILLEGAL FILE TYPE IN NAME FILE: ',A)
        CALL USTOP(' ')
30      CONTINUE
      END IF
C
C12-----FOR DATA FILES, CHECK FOR "REPLACE" OR "OLD" OPTION
      IF (FILSTAT.EQ.'UNKNOWN') THEN
        CALL URWORD(LINE,LLOC,IOPT1,IOPT2,1,N,R,IOUT,IIN)
        IF (LINE(IOPT1:IOPT2).EQ.'REPLACE' .OR.
     &      LINE(IOPT1:IOPT2).EQ.'OLD')
     &      FILSTAT = LINE(IOPT1:IOPT2)
      ENDIF
      IF (FILACT.EQ.' ') FILACT=ACTION(2)
C
C13-----WRITE THE FILE NAME AND OPEN IT.
      WRITE(IOUT,50) FNAME(1:IFLEN),
     1     LINE(ITYP1:ITYP2),IU,FILSTAT,FMTARG,ACCARG
50    FORMAT(1X,/1X,'OPENING ',A,/
     &  1X,'FILE TYPE:',A,'   UNIT ',I4,3X,'STATUS:',A,/
     &  1X,'FORMAT:',A,3X,'ACCESS:',A)
     
C Open files
      OPEN(UNIT=IU,FILE=FNAME(1:IFLEN),FORM=FMTARG,
     1    ACCESS=ACCARG,STATUS=FILSTAT,ACTION=FILACT,ERR=2000)
      NFILE=NFILE+1
      GO TO 10
C
C14-----END OF NAME FILE.  RETURN PROVIDED THAT LISTING FILE AND MPBAS
C14-----FILES HAVE BEEN OPENED.
1000  IF(NFILE.EQ.0) THEN
         WRITE(*,'(A,I3)') ' NAME FILE IS EMPTY FOR GRID ',IGRID
         CALL USTOP(' ')
      ELSE IF(IUNIT(IUMPBAS).EQ.0) THEN
         WRITE(IOUT,'(A,I3)')
     1' MODPATH MPBAS FILE HAS NOT BEEN OPENED FOR GRID ',IGRID
         CALL USTOP(' ')
      END IF
      IF (IEND.EQ.0) THEN
          CLOSE (UNIT=IIN)
      END IF
C
      RETURN
C
C15-----ERROR OPENING FILE.
 2000 CONTINUE
      WRITE(*,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
      WRITE(IOUT,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
 2010 FORMAT(/,1X,'*** ERROR OPENING FILE "',A,'" ON UNIT ',I5,/,
     &7X,'SPECIFIED FILE STATUS: ',A,/
     &7X,'SPECIFIED FILE FORMAT: ',A,/
     &7X,'SPECIFIED FILE ACCESS: ',A,/
     &7X,'SPECIFIED FILE ACTION: ',A,/
     &2X,'-- STOP EXECUTION (SMP6MAIN1OPEN)')
      CALL USTOP(' ')
C
      END
      
      SUBROUTINE MP6MPDATADA()
C***********************************************************************
C  Deallocate MPDATA
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE MPDATA
C-----------------------------------------------------------------------
      DEALLOCATE(NTPER)
      DEALLOCATE(NTSTP)
      DEALLOCATE(NTOFF)
      DEALLOCATE(SIMTIME)
      DEALLOCATE(TIMEPTS)
      RETURN
      END
      
      SUBROUTINE MP6MPBAS1DA(IGRID)
C  DEALLOCATE GLOBAL DATA
      USE GLOBAL
      USE MPBAS
C
        DEALLOCATE(GLOBALDAT(IGRID)%NCOL)
        DEALLOCATE(GLOBALDAT(IGRID)%NROW)
        DEALLOCATE(GLOBALDAT(IGRID)%NLAY)
        DEALLOCATE(GLOBALDAT(IGRID)%NPER)
        DEALLOCATE(GLOBALDAT(IGRID)%NBOTM)
        DEALLOCATE(GLOBALDAT(IGRID)%NCNFBD)
        DEALLOCATE(GLOBALDAT(IGRID)%ITMUNI)
        DEALLOCATE(GLOBALDAT(IGRID)%LENUNI)
        DEALLOCATE(GLOBALDAT(IGRID)%IXSEC)
        DEALLOCATE(GLOBALDAT(IGRID)%ITRSS)
        DEALLOCATE(GLOBALDAT(IGRID)%IFREFM)
        DEALLOCATE(GLOBALDAT(IGRID)%NODES)
        DEALLOCATE(GLOBALDAT(IGRID)%IGRIDNUM)
        DEALLOCATE(GLOBALDAT(IGRID)%NAREALSP)
C
        DEALLOCATE(GLOBALDAT(IGRID)%IUNIT)
        DEALLOCATE(GLOBALDAT(IGRID)%FILENAME)
        DEALLOCATE(GLOBALDAT(IGRID)%CAREALSP)
        DEALLOCATE(GLOBALDAT(IGRID)%LAYCBD)
        DEALLOCATE(GLOBALDAT(IGRID)%LAYTYP)
        DEALLOCATE(GLOBALDAT(IGRID)%NSTP)
        DEALLOCATE(GLOBALDAT(IGRID)%NBFPOS)
        DEALLOCATE(GLOBALDAT(IGRID)%TSMULT)
        DEALLOCATE(GLOBALDAT(IGRID)%ISSFLG)
        DEALLOCATE(GLOBALDAT(IGRID)%DELR)
        DEALLOCATE(GLOBALDAT(IGRID)%DELC)
        DEALLOCATE(GLOBALDAT(IGRID)%XMIN)
        DEALLOCATE(GLOBALDAT(IGRID)%YMIN)
        DEALLOCATE(GLOBALDAT(IGRID)%IFACEASP)
        DEALLOCATE(GLOBALDAT(IGRID)%BOTM)
        DEALLOCATE(GLOBALDAT(IGRID)%LBOTM)
        DEALLOCATE(GLOBALDAT(IGRID)%HEAD)
        DEALLOCATE(GLOBALDAT(IGRID)%IBOUND)
        DEALLOCATE(GLOBALDAT(IGRID)%IBSTART)
        DEALLOCATE(GLOBALDAT(IGRID)%IZONE)
        DEALLOCATE(GLOBALDAT(IGRID)%POR)
        DEALLOCATE(GLOBALDAT(IGRID)%RFAC)
        DEALLOCATE(GLOBALDAT(IGRID)%BUFF)
        DEALLOCATE(GLOBALDAT(IGRID)%IBUFF)
        DEALLOCATE(GLOBALDAT(IGRID)%QX)
        DEALLOCATE(GLOBALDAT(IGRID)%QY)
        DEALLOCATE(GLOBALDAT(IGRID)%QZ)
        DEALLOCATE(GLOBALDAT(IGRID)%QSINK)
        DEALLOCATE(GLOBALDAT(IGRID)%QSOURCE)
        DEALLOCATE(GLOBALDAT(IGRID)%QSTO)
C
        DEALLOCATE(MPBASDAT(IGRID)%MSUM)
        DEALLOCATE(MPBASDAT(IGRID)%MAXCBF)
        DEALLOCATE(MPBASDAT(IGRID)%ISCHILD)
        DEALLOCATE(MPBASDAT(IGRID)%NPRBEG)
        DEALLOCATE(MPBASDAT(IGRID)%NPREND)
        DEALLOCATE(MPBASDAT(IGRID)%NPCBEG)
        DEALLOCATE(MPBASDAT(IGRID)%NPCEND)
        DEALLOCATE(MPBASDAT(IGRID)%NPLBEG)
        DEALLOCATE(MPBASDAT(IGRID)%NPLEND)
        DEALLOCATE(MPBASDAT(IGRID)%NCPP)
        DEALLOCATE(MPBASDAT(IGRID)%NCPPL)
        DEALLOCATE(MPBASDAT(IGRID)%IRCHTOP)
        DEALLOCATE(MPBASDAT(IGRID)%IEVTTOP)
        DEALLOCATE(MPBASDAT(IGRID)%DELT)
        DEALLOCATE(MPBASDAT(IGRID)%PERTIM)
        DEALLOCATE(MPBASDAT(IGRID)%TOTIM)
        DEALLOCATE(MPBASDAT(IGRID)%HNOFLO)
        DEALLOCATE(MPBASDAT(IGRID)%HDRY)
C
      RETURN
      END

      SUBROUTINE SMP6MPBAS1PNT(IGRID)
C***********************************************************************
C  Change global data to a different grid
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE GLOBAL
      USE MPBAS
C-----------------------------------------------------------------------
        NCOL=>GLOBALDAT(IGRID)%NCOL
        NROW=>GLOBALDAT(IGRID)%NROW
        NLAY=>GLOBALDAT(IGRID)%NLAY
        NPER=>GLOBALDAT(IGRID)%NPER
        NBOTM=>GLOBALDAT(IGRID)%NBOTM
        NCNFBD=>GLOBALDAT(IGRID)%NCNFBD
        ITMUNI=>GLOBALDAT(IGRID)%ITMUNI
        LENUNI=>GLOBALDAT(IGRID)%LENUNI
        IXSEC=>GLOBALDAT(IGRID)%IXSEC
        ITRSS=>GLOBALDAT(IGRID)%ITRSS
        IFREFM=>GLOBALDAT(IGRID)%IFREFM
        NODES=>GLOBALDAT(IGRID)%NODES
        IGRIDNUM=>GLOBALDAT(IGRID)%IGRIDNUM
        NAREALSP=>GLOBALDAT(IGRID)%NAREALSP
C
        IUNIT=>GLOBALDAT(IGRID)%IUNIT
        FILENAME=>GLOBALDAT(IGRID)%FILENAME
        CAREALSP=>GLOBALDAT(IGRID)%CAREALSP
        LAYCBD=>GLOBALDAT(IGRID)%LAYCBD
        LAYTYP=>GLOBALDAT(IGRID)%LAYTYP
        PERLEN=>GLOBALDAT(IGRID)%PERLEN
        NSTP=>GLOBALDAT(IGRID)%NSTP
        NBFPOS=>GLOBALDAT(IGRID)%NBFPOS
        TSMULT=>GLOBALDAT(IGRID)%TSMULT
        ISSFLG=>GLOBALDAT(IGRID)%ISSFLG
        DELR=>GLOBALDAT(IGRID)%DELR
        DELC=>GLOBALDAT(IGRID)%DELC
        XMIN=>GLOBALDAT(IGRID)%XMIN
        YMIN=>GLOBALDAT(IGRID)%YMIN
        IFACEASP=>GLOBALDAT(IGRID)%IFACEASP
        BOTM=>GLOBALDAT(IGRID)%BOTM
        LBOTM=>GLOBALDAT(IGRID)%LBOTM
        HEAD=>GLOBALDAT(IGRID)%HEAD
        IBOUND=>GLOBALDAT(IGRID)%IBOUND
        IBSTART=>GLOBALDAT(IGRID)%IBSTART
        IZONE=>GLOBALDAT(IGRID)%IZONE
        POR=>GLOBALDAT(IGRID)%POR
        RFAC=>GLOBALDAT(IGRID)%RFAC
        BUFF=>GLOBALDAT(IGRID)%BUFF
        IBUFF=>GLOBALDAT(IGRID)%IBUFF
        QX=>GLOBALDAT(IGRID)%QX
        QY=>GLOBALDAT(IGRID)%QY
        QZ=>GLOBALDAT(IGRID)%QZ
        QSINK=>GLOBALDAT(IGRID)%QSINK
        QSOURCE=>GLOBALDAT(IGRID)%QSOURCE
        QSTO=>GLOBALDAT(IGRID)%QSTO
C
        MSUM=>MPBASDAT(IGRID)%MSUM
        MAXCBF=>MPBASDAT(IGRID)%MAXCBF
        ISCHILD=>MPBASDAT(IGRID)%ISCHILD
        NPRBEG=>MPBASDAT(IGRID)%NPRBEG
        NPREND=>MPBASDAT(IGRID)%NPREND
        NPCBEG=>MPBASDAT(IGRID)%NPCBEG
        NPCEND=>MPBASDAT(IGRID)%NPCEND
        NPLBEG=>MPBASDAT(IGRID)%NPLBEG
        NPLEND=>MPBASDAT(IGRID)%NPLEND
        NCPP=>MPBASDAT(IGRID)%NCPP
        NCPPL=>MPBASDAT(IGRID)%NCPPL
        IRCHTOP=>MPBASDAT(IGRID)%IRCHTOP
        IEVTTOP=>MPBASDAT(IGRID)%IEVTTOP
        DELT=>MPBASDAT(IGRID)%DELT
        PERTIM=>MPBASDAT(IGRID)%PERTIM
        TOTIM=>MPBASDAT(IGRID)%TOTIM
        HNOFLO=>MPBASDAT(IGRID)%HNOFLO
        HDRY=>MPBASDAT(IGRID)%HDRY
C
      RETURN
      END

      SUBROUTINE SMP6MPBAS1PSV(IGRID)
C***********************************************************************
C  Save global data for a grid
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE GLOBAL
      USE MPBAS
C-----------------------------------------------------------------------
        GLOBALDAT(IGRID)%NCOL=>NCOL
        GLOBALDAT(IGRID)%NROW=>NROW
        GLOBALDAT(IGRID)%NLAY=>NLAY
        GLOBALDAT(IGRID)%NPER=>NPER
        GLOBALDAT(IGRID)%NBOTM=>NBOTM
        GLOBALDAT(IGRID)%NCNFBD=>NCNFBD
        GLOBALDAT(IGRID)%ITMUNI=>ITMUNI
        GLOBALDAT(IGRID)%LENUNI=>LENUNI
        GLOBALDAT(IGRID)%IXSEC=>IXSEC
        GLOBALDAT(IGRID)%ITRSS=>ITRSS
        GLOBALDAT(IGRID)%IFREFM=>IFREFM
        GLOBALDAT(IGRID)%NODES=>NODES
        GLOBALDAT(IGRID)%IGRIDNUM=>IGRIDNUM
        GLOBALDAT(IGRID)%NAREALSP=>NAREALSP
C
        GLOBALDAT(IGRID)%IUNIT=>IUNIT
        GLOBALDAT(IGRID)%FILENAME=>FILENAME
        GLOBALDAT(IGRID)%CAREALSP=>CAREALSP
        GLOBALDAT(IGRID)%LAYCBD=>LAYCBD
        GLOBALDAT(IGRID)%LAYTYP=>LAYTYP
        GLOBALDAT(IGRID)%PERLEN=>PERLEN
        GLOBALDAT(IGRID)%NSTP=>NSTP
        GLOBALDAT(IGRID)%NBFPOS=>NBFPOS
        GLOBALDAT(IGRID)%TSMULT=>TSMULT
        GLOBALDAT(IGRID)%ISSFLG=>ISSFLG
        GLOBALDAT(IGRID)%DELR=>DELR
        GLOBALDAT(IGRID)%DELC=>DELC
        GLOBALDAT(IGRID)%XMIN=>XMIN
        GLOBALDAT(IGRID)%YMIN=>YMIN
        GLOBALDAT(IGRID)%IFACEASP=>IFACEASP
        GLOBALDAT(IGRID)%BOTM=>BOTM
        GLOBALDAT(IGRID)%LBOTM=>LBOTM
        GLOBALDAT(IGRID)%HEAD=>HEAD
        GLOBALDAT(IGRID)%IBOUND=>IBOUND
        GLOBALDAT(IGRID)%IBSTART=>IBSTART
        GLOBALDAT(IGRID)%IZONE=>IZONE
        GLOBALDAT(IGRID)%POR=>POR
        GLOBALDAT(IGRID)%RFAC=>RFAC
        GLOBALDAT(IGRID)%BUFF=>BUFF
        GLOBALDAT(IGRID)%IBUFF=>IBUFF
        GLOBALDAT(IGRID)%QX=>QX
        GLOBALDAT(IGRID)%QY=>QY
        GLOBALDAT(IGRID)%QZ=>QZ
        GLOBALDAT(IGRID)%QSINK=>QSINK
        GLOBALDAT(IGRID)%QSOURCE=>QSOURCE
        GLOBALDAT(IGRID)%QSTO=>QSTO
C
        MPBASDAT(IGRID)%MSUM=>MSUM
        MPBASDAT(IGRID)%MAXCBF=>MAXCBF
        MPBASDAT(IGRID)%ISCHILD=>ISCHILD
        MPBASDAT(IGRID)%NPRBEG=>NPRBEG
        MPBASDAT(IGRID)%NPREND=>NPREND
        MPBASDAT(IGRID)%NPCBEG=>NPCBEG
        MPBASDAT(IGRID)%NPCEND=>NPCEND
        MPBASDAT(IGRID)%NPLBEG=>NPLBEG
        MPBASDAT(IGRID)%NPLEND=>NPLEND
        MPBASDAT(IGRID)%NCPP=>NCPP
        MPBASDAT(IGRID)%NCPPL=NCPPL
        MPBASDAT(IGRID)%IRCHTOP=>IRCHTOP
        MPBASDAT(IGRID)%IEVTTOP=>IEVTTOP
        MPBASDAT(IGRID)%DELT=>DELT
        MPBASDAT(IGRID)%PERTIM=>PERTIM
        MPBASDAT(IGRID)%TOTIM=>TOTIM
        MPBASDAT(IGRID)%HNOFLO=>HNOFLO
        MPBASDAT(IGRID)%HDRY=>HDRY
C
      RETURN
      END
      
      SUBROUTINE SELECTGRID(IGRID)
C***********************************************************************
C  Select a grid as the active grid if it is not already the active grid
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE GLOBAL, ONLY:IGRIDNUM
      USE MPDATA, ONLY:NGRIDS,IOLIST
      INTEGER ::IGRID
C-----------------------------------------------------------------------
      IF(IGRID.GE.1 .AND. IGRID.LE.NGRIDS) THEN
        IF(IGRID.NE.IGRIDNUM) CALL SMP6MPBAS1PNT(IGRID)
      ELSE
        CALL USTOP('INVALID GRID NUMBER IN CALL TO SELECTGRID. STOP.')
      END IF
      
      RETURN
      END

