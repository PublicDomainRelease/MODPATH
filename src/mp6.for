C***********************************************************************
C  Main code for USGS MODPATH particle tracking model - Version 6
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE GLOBAL, ONLY:NAREALSP,NIUNIT,ISSFLG,NPER
      USE MPDATA, ONLY:NTSTEPS,NTPER,NTSTP,SIMTIME,ITRACKDIR,NGRIDS,
     1                 ISIMTYPE,ISTOPT,STOPTIME,IOLIST,INUNIT,
     2                 IUMPBAS,IUDIS,IUHEAD,IUBUDGET,INMPSIM,IOTRACE,
     3                 IOBUDCHK,REFTIME,TIMEPTS,NTPTS,FENDPT,FPLINE,
     4                 FTSERS,BUDGETCELLS,IBDOPT,NCELLBUD,CELL,IOLOG,
     5                 IOEPT,IOPART,ZONETOTALS,HEADEREP,IOAOBS,
     6                 DEFAULTREALKIND,DEFAULTREALPRECISION
      USE PARTICLEDATA, ONLY:NPGROUPS,PGROUPS,PARTICLE,PARTICLEGROUP
      INCLUDE 'MP6Openspec.inc'
      CHARACTER*40 VERSION
      CHARACTER*10 MFVNAM
      PARAMETER (VERSION='6.0.01  August 24, 2012')
      CHARACTER*80 HEADNG(2)
      CHARACTER*200 FNAME
      CHARACTER*200 FMPNAM
      CHARACTER*200 FMPSIM
      CHARACTER*200 FLOG
      CHARACTER*200 FMPLIST
      CHARACTER*16 CUNIT(NIUNIT)
      CHARACTER*132 MESSAGE
      INTEGER ::KTIME,KFIRST,KLAST,KINCR,KPER,KSTP,N,NT
      INTEGER ::IGRID,ILGR,ITEND,ITP,IACTIVE,IPENDING,ISS
      REAL ::T1,T2,TMAX,TSMAX,TT,T
      TYPE(CELL) ::CMAX
      TYPE(HEADEREP) ::EPHEADER
      DATA CUNIT/100*'                '/
C-----------------------------------------------------------------------    
      IUMPBAS = 1
      IUDIS = 2
      IUHEAD = 3
      IUBUDGET = 4
      CUNIT(IUMPBAS) = 'MPBAS           '
      CUNIT(IUDIS) = 'DIS             '
      CUNIT(IUHEAD) = 'HEAD            '
      CUNIT(IUBUDGET) = 'BUDGET          '

C     Set dedicated file unit numbers
      INUNIT = 99
      IOPART = 98
      IOEPT = 97
      IOLIST = 96
      INMPSIM = 95
      IOTRACE = 94
      IOBUDCHK = 93
      IOAOBS = 92
      IOLOG = 90
      
C     Initialized variables
      NGRIDS = 0
      ZONETOTALS = 0

C     Set the flag to hold the default KIND value of the REAL data type 
C     Check variable T which is declared as default type REAL. 
C     For the INTEL FORTRAN compiler, the following values will be returned:
C
C     Single Precision: DEFAULTREALKIND = 4,  DEFAULTREALPRECISION = 6
C     Double Precision: DEFAULTREALKIND = 8,  DEFAULTREALPRECISION = 15
C
      DEFAULTREALKIND= KIND(T)
      DEFAULTREALPRECISION=PRECISION(T)
      
C     Open LOG file
      FLOG='MPATH6.LOG'
      OPEN(UNIT=IOLOG,FILE=FLOG,STATUS='REPLACE',FORM='FORMATTED',
     1     ACCESS='SEQUENTIAL')
      
C     Open the MODPATH simulation file and read the names of the MPNAM and MPLIST files
      MESSAGE='Open sim file; read NAME and LIST file names'
      CALL ULOG(MESSAGE)
      CALL GETMPSIM(FMPSIM,FMPNAM,FMPLIST,VERSION)
      
C     Open the NAME file and read the number of grids
      MESSAGE='Open the NAME file and read the number of grids'
      CALL ULOG(MESSAGE)
      CALL GETNAMFIL(ILGR,NGRIDS,FMPNAM)

C     Multiple grids are not yet supported
      IF(NGRIDS.GT.1) THEN
       MESSAGE='This version of MODPATH does not support multiple grids'
       CALL USTOP(MESSAGE)
      END IF 

C     Process input data for each grid      
      MESSAGE='Process the input data for each grid'
      CALL ULOG(MESSAGE)
      DO IGRID = 1, NGRIDS
        IF(NGRIDS.GT.1) WRITE(*,'(A,I2,A)') 'Grid ',IGRID,':'
        WRITE(*,'(A,I2)') 'Processing basic data ...'
        CALL MP6MPBAS1AR(INUNIT,CUNIT,2,IGRID,HEADING,
     1                 VERSION,MFVNAM)
        
        WRITE(*,'(A,I2)') 'Checking head file ...'        
        CALL CHECKHEADFILE(IGRID,1)
        WRITE(*,'(A,I2)') 'Checking budget file and building index ...'
        CALL BUILDBUDGETINDEX(IGRID,1,NAREALSP)
      END DO      
    
C     Read MODPATH simulation file and prepare simulation data
      MESSAGE='Read MODPATH simulation file and prepare simulation data'
      CALL ULOG(MESSAGE)
      CALL MP6MPSIM1RP()

C     Compute range of time steps. Prepare for time step loop
      MESSAGE='Compute range of time steps. Prepare for time step loop'
      CALL ULOG(MESSAGE)
      IF(ITRACKDIR.EQ.1) THEN
        KLAST= NTSTEPS
        IF(ISTOPT.EQ.3) 
     1    CALL FINDSTEPFROMTIME(KLAST,REFTIME+STOPTIME)
        CALL FINDSTEPFROMTIME(KFIRST,REFTIME)
        KINCR= 1
      ELSE
        KLAST= 1
        IF(ISTOPT.EQ.3) 
     1    CALL FINDSTEPFROMTIME(KLAST,REFTIME-STOPTIME)
        CALL FINDSTEPFROMTIME(KFIRST,REFTIME)
        KINCR= -1
      END IF      

      WRITE(*,*)
      WRITE(*,'(A)') 'Run particle tracking simulation ...'    
      WRITE(IOLIST,*)
      WRITE(IOLIST,*)
      WRITE(IOLIST,'(1X,A)') 'Run particle tracking simulation ...'
    
C     TIME_STEP_LOOP
C     Loop through MODFLOW time steps. Load and process data for the time step. Then track particles
C     through the system for the specified time intervals that fall within the time step.
      NT = 0
      T = 0.0
      ITRACESEG = 0
      MESSAGE='Begin time step loop.'
      CALL ULOG(MESSAGE)
 
C     Write the pathline or timeseries file header
      IF(ISIMTYPE .EQ. 2) THEN
        CALL WRITEHEADERPL(IOPART)
      ELSE IF(ISIMTYPE .EQ. 3) THEN
        CALL WRITEHEADERTS(IOPART)
      END IF

      TIME_STEP_LOOP: DO KTIME= KFIRST,KLAST,KINCR
        KPER= NTPER(KTIME)
        KSTP= NTSTP(KTIME)
        ISS = ISSFLG(KPER)
        
        WRITE(MESSAGE,'(A,I5,A,I5,A,1PE12.5)') 
     1  'Processing Time Step ',KSTP,' Period ',
     2  KPER,'.  Time = ',SIMTIME(KTIME)
        CALL ULOG(MESSAGE)
        WRITE(*,'(A)') MESSAGE
        WRITE(IOLIST,*)
        WRITE(IOLIST,'(1X,A)') '----------------------------------------
     1------------------------------------------------------'
        WRITE(IOLIST,'(A,A,I6,A)') MESSAGE,'  (Cumulative step = ',
     1                             KTIME,')'
        WRITE(IOLIST,'(1X,A)') '----------------------------------------
     1------------------------------------------------------'
     
C       Read, process, and load the data for this time step
        MESSAGE='Read, process, and load the data for this time step'
        CALL ULOG(MESSAGE)
        CALL LOADTIMESTEP(KTIME,NAREALSP)

C       Check budget
        IF(IBDOPT.GT.1) THEN
          WRITE(MESSAGE,'(A,I3)') 'Check budget: IBDOPT = ',
     1      IBDOPT
          CALL ULOG(MESSAGE)
          DO IGRID = 1,NGRIDS
            CALL CHECKBUDGET(IGRID,CMAX)
            WRITE(IOLIST,*)
            WRITE(IOLIST,'(1X,A)') 'INDIVIDUAL CELL BUDGETS:'
            WRITE(IOLIST,'(3X,A)') 
     1      'CELL WITH MAXIMUM VOLUMETRIC BALANCE ERROR:'
            CALL CHECKCELLBUDGET(CMAX,IGRID,KTIME,
     1                           KPER,KSTP)
            IF(IBDOPT.EQ.3) THEN
              ICOUNT = 0
              IF(NCELLBUD.GT.0) THEN
                DO N = 1, NCELLBUD
                  IF(BUDGETCELLS(N)%GRID.EQ.IGRID) THEN
                   ICOUNT = ICOUNT + 1
                   IF(ICOUNT.EQ.1) THEN
                     WRITE(IOLIST,'(3X,A)') 'SPECIFIED CELLS:'
                   END IF
                   CALL CHECKCELLBUDGET(BUDGETCELLS(N),IGRID,KTIME,
     1                                  KPER,KSTP)
                  END IF
                END DO
              END IF
            END IF
            
            CALL HEADSTATS(IGRID)
            
          END DO
          
          
        END IF     
        
C       Compute the tracking time corresponding to the end or beginning 
C       of this MODFLOW time step (depending on whether this is a forward 
C       or backward tracking run.)
        MESSAGE = 'Compute TSMAX'
        CALL ULOG(MESSAGE)
        IF(ITRACKDIR .EQ. 1) THEN
          TSMAX = SIMTIME(KTIME) - REFTIME
          IF(TSMAX.GT.STOPTIME) THEN
            TSMAX = STOPTIME
          ELSE IF(ISTOPT.EQ.2) THEN
            IF(KTIME.EQ.NTSTEPS .AND. ISSFLG(NPER).EQ.1) THEN
              TSMAX = STOPTIME
            END IF
          END IF
        ELSE
          TSMAX = REFTIME - SIMTIME(KTIME-1)
          IF(TSMAX.GT.STOPTIME) THEN
            TSMAX = STOPTIME
          ELSE IF(ISTOPT.EQ.2) THEN
            IF(KTIME.EQ.1 .AND. ISSFLG(1).EQ.1) THEN
              TSMAX = STOPTIME
            END IF
          END IF         
        END IF
        
C       TRACKING_INTERVAL_LOOP: 
C       Loop through all the required time points that fall within the
C       current MODFLOW time step. For runs that do not have any specified 
C       time points, there will only be one time point that corresponds either 
C       to the beginning or end of the current MODFLOW time step or to the 
C       specified stop time for the MODPATH analysis.
        ITEND = 0
        MESSAGE = 'Begin TRACKING_INTERVAL_LOOP'
        CALL ULOG(MESSAGE)
        TRACKING_INTERVAL_LOOP: DO WHILE (ITEND .EQ. 0)
          ITEND = 1
          ITP = -1
          TMAX = TSMAX
          IF (NT+1 .LE. NTPTS) THEN
            IF (TIMEPTS(NT+1) .LE. TSMAX) THEN
              NT = NT + 1
              TMAX = TIMEPTS(NT)
              ITEND = 0
              ITP = NT
              IF(TMAX .EQ. TSMAX) ITEND = 1
            END IF
          END IF          
          
C         Track all particles up through tracking time TMAX
          MESSAGE = 'Call TRACK'
          CALL ULOG(MESSAGE)
          
C         Write initial particle locations if it is a timeseries simulation          
          IF(ISIMTYPE.EQ.3 .AND. T.EQ.0) THEN
            CALL WRITEINITIALTS(KTIME)
          END IF
          
          CALL TRACK(T,TMAX,ITP,IACTIVE,IPENDING,ISS,KTIME,KPER,KSTP)

C         Update tracking time          
          T = TMAX
        
C         Check to see if there are any particles remaining to track in the next
C         pass. If not, exit the loop.
          IF(IACTIVE .EQ. 0 .AND. IPENDING .EQ. 0) EXIT TIME_STEP_LOOP
          
        END DO TRACKING_INTERVAL_LOOP
        
        MESSAGE = 'Exit TRACKING_INTERVAL_LOOP'
        CALL ULOG(MESSAGE)
       
C       Exit TIME_STEP_LOOP if the tracking time has reached the specified stop time.
        IF(T .EQ. STOPTIME) EXIT TIME_STEP_LOOP
        
      END DO TIME_STEP_LOOP
      
      MESSAGE = 'Exit TIME_STEP_LOOP'
      CALL ULOG(MESSAGE)
      
C     Write particle data to the Endpoint file
      MESSAGE = 'Particle tracking complete. Writing endpoint file ...'
      CALL ULOG(MESSAGE)
      
      WRITE(*,'(A)') MESSAGE
      CALL WRITEENDPOINTS(EPHEADER)
      CALL WRITESUMMARY(EPHEADER)
      
C     End simulation
      CALL USTOP('End of MODPATH simulation. Normal termination.')
      END

      SUBROUTINE GETMPSIM(FMPSIM,FMPNAM,FMPLIST,VERSION)
C***********************************************************************
C  Get the name of the MODPATH simulation file. Open the file and read
C  the file to get the names of the MODPATH name file and list file.
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE MPDATA, ONLY:IOLOG,IOLIST,INMPSIM
      INTEGER ::IU,NHEADERS,N
      CHARACTER*(*) FMPSIM
      CHARACTER*(*) FMPNAM
      CHARACTER*(*) FMPLIST
      CHARACTER*200 COMLIN,LINE
      CHARACTER*40 VERSION
      CHARACTER (LEN=200), DIMENSION(20) ::HEADERS
      LOGICAL EXISTS
      INCLUDE 'MP6Openspec.inc'
C-----------------------------------------------------------------------      
C Get name file from command line or user interaction.
        FMPSIM=' '
        FMPLIST=' '
        COMLIN=' '
C *** Subroutines GETARG and GETCL are extensions to Fortran 90/95 that
C *** allow a program to retrieve command-line arguments.  To enable
C *** MODPATH-6 to read the name of a simulation file from the command
C *** line, either GETARG or GETCL must be called, but not both.  As
C *** distributed, the call to GETARG is uncommented.  For compilers
C *** that support GETCL but not GETARG, comment out the call to GETARG
C *** and uncomment the call to GETCL.  The calls to both GETARG and
C *** GETCL may be commented out for compilers that do not support
C *** either extension.
        CALL GETARG(1,COMLIN)
C        CALL GETCL(COMLIN)
        ICOL = 1
        IF(COMLIN.NE.' ') THEN
          FMPSIM=COMLIN
        ELSE
   15     WRITE (*,*) ' Enter the MODPATH simulation file: '
          READ (*,'(A)') FMPSIM
          CALL URWORD(FMPSIM,ICOL,ISTART,ISTOP,0,N,R,0,0)
          FMPSIM=FMPSIM(ISTART:ISTOP)
          IF (FMPSIM.EQ.' ') GOTO 15
        ENDIF  
        INQUIRE (FILE=FMPSIM,EXIST=EXISTS)
        IF(.NOT.EXISTS) THEN
          NC = INDEX(FMPSIM,' ')
          FMPSIM(NC:NC+5)='.mpsim'
          INQUIRE (FILE=FMPSIM,EXIST=EXISTS)
          IF(.NOT.EXISTS) THEN
            WRITE (*,480) FMPSIM(1:NC)
  480       FORMAT(1X,'Can''t find MODPATH simulation file ',A)
            CALL USTOP(' ')
          ENDIF
        ENDIF
        FMPSIM = TRIM(FMPSIM)
        
C     OPEN MODPATH SIMULATION FILE        
      OPEN(UNIT=INMPSIM,FILE=FMPSIM,STATUS='OLD',ACTION=ACTION(1))

C     READ COMMENT LINES AND THE NAME FILE NAME
      NHEADERS = 0
      DO WHILE(.TRUE.)
        READ(INMPSIM,'(A)') LINE
        IF(LINE(1:1).NE.'#') EXIT
        IF(NHEADERS.LT.20) THEN
          NHEADERS = NHEADERS + 1
          HEADERS(NHEADERS) = LINE
        END IF
      END DO
      
      FMPNAM = LINE
      CALL URWORD(FMPNAM,ICOL,ISTART,ISTOP,0,N,R,0,0)
      
C     READ LIST FILE NAME
      READ (INMPSIM,'(A)') FMPLIST
      CALL URWORD(FMPLIST,ICOL,ISTART,ISTOP,0,N,R,0,0)
      
      OPEN(UNIT=IOLIST,FILE=FMPLIST,STATUS='REPLACE',FORM='FORMATTED',
     1     ACCESS='SEQUENTIAL')

      WRITE (IOLIST,1) VERSION
1     FORMAT (/,'MODPATH - Version ',A/,
     &    'U.S. GEOLOGICAL SURVEY PARTICLE TRACKING ',
     &    'MODEL FOR MODFLOW',/)
     
      WRITE (IOLIST,'(A,A)') 'Simulation File: ',FMPSIM
      IF(NHEADERS.GT.0) THEN
        DO N=1,NHEADERS
          CALL UWRCOM(IOLIST,HEADERS(N))
        END DO
      END IF
            
      RETURN
      END
            
      SUBROUTINE GETNAMFIL(ILGR,NGRIDS,FMPNAM)
C***********************************************************************
C  Get the name of the MODPATH name file
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE MPDATA, ONLY:INUNIT,IOLOG,IOLIST
      CHARACTER*(*) FMPNAM
      CHARACTER*200 COMLIN,LINE
      LOGICAL EXISTS
      INCLUDE 'MP6Openspec.inc'
C-----------------------------------------------------------------------

C1A-----READ THE NUMBER OF GRIDS AND THEN LEAVE FILE OPEN FOR PARSING
C
      OPEN(UNIT=INUNIT,FILE=FMPNAM,STATUS='OLD',ACTION=ACTION(1))
      CALL URDCOM(INUNIT,0,LINE)
      ICOL=1
      
      IF(LINE(1:3).NE.'LGR') THEN
        CLOSE(UNIT=INUNIT)
        OPEN(UNIT=INUNIT,FILE=FMPNAM,STATUS='OLD',ACTION=ACTION(1))
        ILGR=0
        NGRIDS=1
        RETURN
      END IF
      
      CALL URWORD(LINE,ICOL,ISTART,ISTOP,2,NGR,R,0,INUNIT)
      IF (NGR .LT. 1) THEN
          WRITE (*,*) ' Number of grids cannot be less than 1. '
          CALL USTOP(' ')
      ELSEIF (NGR .EQ. 1) THEN
          ILGR = 0
          NGRIDS = NGR
          WRITE (*,*) ' Running MODPATH for a single-grid system. '
      ELSE
          ILGR = 1
          NGRIDS = NGR
          WRITE (*, '(A, I2, A)') 
     *' Running MODPATH for a mult-grid system with ', NGRIDS, ' grids.'
      ENDIF
C
      RETURN
      END
      
      SUBROUTINE SETFILEPOSITION(IU,IPOS,IOUT)
C***********************************************************************
C  Set the position of the file cursor for the file connected in STREAM
C  access mode to unit IU
C
C    SPECIFICATIONS:
c-----------------------------------------------------------------------
      USE GLOBAL, ONLY:KIND8I
      INTEGER(KIND8I) ::IPOS
      INTEGER ::IOUT
C-----------------------------------------------------------------------      
      READ(IU,POS=IPOS,ERR=1000)
      RETURN
      
1000  CONTINUE
      IF(IOUT.GT.0) THEN
        WRITE(IOUT,1100) IU
      ELSE
        WRITE(*,1100) IU
      END IF
1100  FORMAT('ERROR SETTING FILE POSITION ON UNIT ',I5)
      CALL USTOP('ERROR SETTING FILE POSITION. STOP.')
      END
