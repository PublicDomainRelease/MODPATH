C
      SUBROUTINE BUILDBUDGETINDEX(IGRID,IOTYPE,NAREALSP)
      USE GLOBAL, ONLY:KIND8I,IUNIT,NPER,NSTP,NIUNIT,NBFPOS
      USE MPDATA, ONLY:IOLIST,IUBUDGET
      INTEGER I,J,K,N,KPER,KSTP,IGRID,IUBUD,IOTYPE
      INTEGER(KIND8I) ::IPOS,NBYTES
      LOGICAL ISOPEN,ISOK
!      CHARACTER (LEN=*), DIMENSION(NAREALSP) ::CADSTX
      CHARACTER (LEN=200) ::MESSAGE
      
C       Select grid
        CALL SMP6MPBAS1PNT(IGRID)
        
        IF(IOTYPE.EQ.1) THEN
        WRITE(IOLIST,*)
        WRITE(IOLIST,'(1X,A,I3)') 
     1    'CHECKING AND INDEXING BUDGET FILE FOR GRID ',IGRID
        END IF
        
C       Check file status
        IUBUD=IUNIT(IUBUDGET)
        IF(IUBUD.EQ.0) THEN
          CALL USTOP('No budget file was assigned. Stop.')
        ELSE
          INQUIRE(UNIT=IUBUD,OPENED=ISOPEN)
          IF(.NOT. ISOPEN) THEN
            CALL USTOP('The budget file was not opened. Stop.')
          END IF
        END IF
C       Set position index to the beginning of the file
        IPOS=1
        READ(IUBUD,POS=IPOS)
        NBYTES=0
        N=0
C       Read budget data and calculate index positions
        DO KPER=1, NPER
        DO KSTP=1,NSTP(KPER)
          N = N + 1
          IPOS=IPOS + NBYTES
          IF(IOTYPE.EQ.1) THEN
            WRITE(IOLIST,'(1X,A,I10)') 'Index position = ',IPOS
          END IF
          NBFPOS(N)= IPOS
          CALL READTIMESTEPBUDGET(KPER,KSTP,NBYTES,0,IOTYPE,0,NAREALSP)
        END DO
        END DO
C       Check the integrity of the budget index        
        CALL CHECKBUDGETINDEX(IGRID,ISOK)
        IF(ISOK) THEN
          IF(IOTYPE.EQ.1) THEN
           WRITE(IOLIST,'(1X,A,I3,A)') 'THE BUDGET INDEX FOR GRID ',
     1     IGRID,' WAS SUCCESSFULLY CREATED.'
          END IF
        ELSE
          WRITE(MESSAGE,'(1X,A,I3,A)') 'THE BUDGET INDEX FOR GRID ',
     1    IGRID,' DOES NOT MATCH THE DATA IN THE FILE. STOP.'
          CALL USTOP(MESSAGE)
        END IF
      RETURN
      END
      
      SUBROUTINE CHECKHEADFILE(IGRID,IOTYPE)
      USE GLOBAL, ONLY:IUNIT,NBFPOS,NPER,NSTP,KIND8I
      USE MPDATA, ONLY:SIMTIME,IOLIST
      
      INTEGER(KIND8I) ::IPOS
      INTEGER ::KSTP,KPER,KS,KP,IOTYPE,N
      CHARACTER(LEN=16) ::TEXT
      CHARACTER(LEN=200) ::MESSAGE
      REAL ::PERTIM,TOTIM
      LOGICAL ISOK
      
      ISOK=.FALSE.
C     Select grid
      CALL SMP6MPBAS1PNT(IGRID)
      
      IF(IOTYPE.EQ.1) THEN
      WRITE(IOLIST,*)
      WRITE(IOLIST,'(1X,A,I3)') 'CHECKING THE HEAD FILE FOR GRID ',IGRID
      END IF
      
      N=0
      DO KPER=1,NPER
        DO KSTP=1,NSTP(KPER)
          CALL READTIMESTEPHEAD(KSTP,KPER,KS,KP,PERTIM,TOTIM,ISOK,
     1                          IOTYPE)
          IF(KS.NE.KSTP .OR. KP.NE.KPER) THEN
            WRITE(IOLIST, '(1X,A)') 
     1      'FOUND UNEXPECTED TIME STEP IN HEAD FILE. STOP.'
            ISOK=.FALSE.
            RETURN
          END IF
          IF(.NOT. ISOK) THEN
            WRITE(MESSAGE,'(1X,A,I3,A)') 'THE HEAD FILE FOR GRID ',
     1                                 IGRID,' IS NOT COMPLETE. STOP'
            CALL USTOP(MESSAGE)
          END IF
          N=N+1
C   Store the simulation time for the end of this time step in the SIMTIME array
          IF(IGRID.EQ.1) THEN
            SIMTIME(N)= SIMTIME(0) + TOTIM
          ELSE
            IF((SIMTIME(0)+TOTIM).NE.SIMTIME(N)) THEN
              WRITE(IOLIST,'(1X,A,I2,A)') 
     1        'INCONSISTENT TIME STEP STRUCTURE FOR GRID ',IGRID,
     2        ' STOP.'
              CALL USTOP('INCONSISTENT TIME STEP STRUCTURE. STOP.')
            END IF
          END IF
        END DO
      END DO
      
      ISOK=.TRUE.
      IF(IOTYPE.EQ.1) THEN
        WRITE(IOLIST,'(1X,A,I3,A)') 'THE HEAD FILE FOR GRID ',
     1                                   IGRID,' IS COMPLETE.'
      END IF
     
      RETURN
      END
      
      SUBROUTINE CHECKBUDGETINDEX(IGRID,ISOK)
      USE GLOBAL, ONLY:IUNIT,NBFPOS,NPER,NSTP,KIND8I
      USE MPDATA, ONLY:IUBUDGET
      INTEGER ::KPER,KSTP,N,IU,KP,KS,ISTAT
      INTEGER(KIND8I) ::IPOS
      CHARACTER(LEN=16) ::TEXT
      LOGICAL ISOK
        
        ISOK=.FALSE.
C       Select grid
        CALL SMP6MPBAS1PNT(IGRID)
        
        IU=IUNIT(IUBUDGET)
        N=0
        DO KPER=1,NPER
          DO KSTP=1,NSTP(KPER)
            N=N+1
            IPOS=NBFPOS(N)
            READ(IU,POS=IPOS,IOSTAT=ISTAT) KS,KP
            IF(ISTAT.NE.0) THEN
              ISOK=.FALSE.
              RETURN
            END IF
            IF(KS.NE.KSTP .OR. KP.NE.KPER) THEN
              ISOK=.FALSE.
              RETURN
            END IF
          END DO
        END DO
        
        ISOK=.TRUE.
      
      RETURN
      END
      
      SUBROUTINE READTIMESTEPHEAD(KSTP,KPER,KS,KP,PERTIM,TOTIM,ISOK,
     1                            IOTYPE)
      USE GLOBAL, ONLY:NIUNIT,NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,
     1                 ITMUNI,LENUNI,IXSEC,ITRSS,
     2                 IFREFM,NODES,IUNIT,HEAD,LBOTM,LAYCBD,
     3                 PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,BOTM,
     4                 IBOUND,IZONE,POR,BUFF,IBUFF,QX,QY,QZ,QSINK,
     5                 QSOURCE,QSTO,KIND8I      
      USE DOUBLEBUDGET, ONLY: IPRHEAD,DBLBUFF
      USE MPDATA, ONLY:IOLIST,IUHEAD
      REAL ::PERTIM,TOTIM,RDUMMY
      INTEGER(KIND8I) ::IPOS
      INTEGER ::ISTEP,IU,KS,KP,IOTYPE
      LOGICAL ISOK
      
      ISOK=.FALSE.
      
      IF(IUNIT(IUHEAD).GT.0) THEN
          IU=IUNIT(IUHEAD)
      ELSE
          WRITE(IOLIST,*) 'NO MODFLOW HEAD FILE IS OPENED. STOP.'
          ISOK=.FALSE.
          RETURN
      END IF
      
      IF(IPRHEAD.EQ.0) THEN
        CALL HEADPRECISION(IU,IOLIST,NCOL,NROW,NLAY)
      END IF
      
      IF(IPRHEAD.EQ.2) THEN
        IRECL=NLAY*(52 + 8*NCOL*NROW)
      ELSE IF(IPRHEAD.EQ.1) THEN
        IRECL=NLAY*(44 + 4*NCOL*NROW)
      ELSE
        CALL USTOP(
     1  'THE PRECISION OF THE HEAD FILE COULD NOT BE DETERMINED. STOP.')
      END IF
      
      CALL GETGLOBALTIMESTEP(ISTEP,KPER,KSTP)
      IPOS= (ISTEP-1)*IRECL + 1
      
C     Set the file position to the beginning of the time step
      READ(IU,POS=IPOS)
      
      CALL READHEAD(KS,KP,PERTIM,TOTIM,ISOK)
      IF(IOTYPE.EQ.1) WRITE(IOLIST,'(1X,A,I5,A,I5,A,1PE14.6,A,1PE14.6)')
     1'READ HEAD FOR PERIOD ',KP,' STEP ',KS,'  PERTIM = ',PERTIM,
     2'  TOTIM = ',TOTIM  
      
      RETURN
      END
      
      SUBROUTINE READHEAD(KSTP,KPER,PERTIM,TOTIM,ISOK)
      USE GLOBAL, ONLY:NIUNIT,NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,
     1                 ITMUNI,LENUNI,IXSEC,ITRSS,
     2                 IFREFM,NODES,IUNIT,HEAD,LBOTM,LAYCBD,
     3                 PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,BOTM,
     4                 IBOUND,IZONE,POR,BUFF,IBUFF,QX,QY,QZ,QSINK,
     5                 QSOURCE,QSTO,KIND8I      
      USE DOUBLEBUDGET, ONLY: IPRHEAD,DBLBUFF,SNGBUFF
      USE MPDATA, ONLY:IOLIST,IUHEAD
      DOUBLE PRECISION    ::PERTIMD,TOTIMD
      CHARACTER (LEN=132) ::LINE
      CHARACTER (LEN=20)  ::FMT
      CHARACTER (LEN=16)  ::STRING,TEXT
      INTEGER ::LTEXT,IFORM,IU,IEND,NC,NR,K,N,
     1          L,ICOL,IWFRST,IWLAST,IDUMMY,ISTEP,IRECL
      REAL ::PERTIM,TOTIM
      REAL(KIND=4) ::PERTIMS,TOTIMS
      INTEGER(KIND8I) ::IPOS
      LOGICAL ISOK
      
      ISOK=.FALSE.
      TEXT = '            HEAD'
      
      IF(IUNIT(IUHEAD).GT.0) THEN
          IU=IUNIT(IUHEAD)
      ELSE
          WRITE(IOLIST,*) 'NO MODFLOW HEAD FILE IS OPENED. STOP.'
          ISOK=.FALSE.
          RETURN
      END IF
      
      IF(IPRHEAD.EQ.1) THEN
       IF(.NOT.ASSOCIATED(SNGBUFF)) ALLOCATE (SNGBUFF(NCOL,NROW,NLAY))
      ELSE IF(IPRHEAD.EQ.2) THEN
       IF(.NOT.ASSOCIATED(DBLBUFF)) ALLOCATE (DBLBUFF(NCOL,NROW,NLAY))
      END IF
      
      DO L = 1, NLAY
          IF(IPRHEAD.EQ.2) THEN
            READ(IU,END=10,ERR=30) KSTP,KPER,PERTIMD,TOTIMD,STRING,
     1                             NC,NR,K
            PERTIM=PERTIMD
            TOTIM=TOTIMD
          ELSE
            READ(IU,END=10,ERR=30) KSTP,KPER,PERTIMS,TOTIMS,STRING,
     1                              NC,NR,K
            PERTIM=PERTIMS
            TOTIM=TOTIMS
          END IF
          IF(K.NE.L) THEN
              WRITE(IOLIST,1400) L,K
              ISOK=.FALSE.
              RETURN
          END IF
          
          IF(STRING.NE.TEXT) THEN
              WRITE(IOLIST,1000) TEXT(1:LTEXT)
              ISOK=.FALSE.
              RETURN
          END IF
          
          IF(IPRHEAD.EQ.2) THEN
            READ(IU,ERR=60,END=70) ((DBLBUFF(J,I,1),J=1,NCOL),I=1,NROW)
            DO I=1,NROW
                DO J=1,NCOL
                    HEAD(J,I,K)=DBLBUFF(J,I,1)
                END DO
            END DO
          ELSE
            READ(IU,ERR=60,END=70) ((SNGBUFF(J,I,1),J=1,NCOL),I=1,NROW)
            DO I=1,NROW
                DO J=1,NCOL
                    HEAD(J,I,K)=SNGBUFF(J,I,1)
                END DO
            END DO
          END IF
      END DO
      
      IEND=0
      ISOK=.TRUE.
      RETURN
      
C...  END OF FILE REACHED
10    IEND=1
      ISOK=.FALSE.
      RETURN
      
C... PROBLEM READING HEADER RECORD
30    WRITE(IOLIST,1100) TEXT(1:LTEXT)
      ISOK=.FALSE.
      RETURN
40    WRITE(IOLIST,1200) TEXT(1:LTEXT)
      ISOK=.FALSE.
      RETURN
C... END OF FILE REACHED      
50    WRITE(IOLIST,1300) IU
      ISOK=.FALSE.
      RETURN
C... PROBLEM READING ARRAY
60    WRITE(IOLIST,1500)
      ISOK=.FALSE.
      RETURN
70    WRITE(IOLIST,1300) IU
      ISOK=.FALSE.
      RETURN
      
1000  FORMAT(1X,A,' FILE DOES NOT HAVE VALID HEADER RECORD. STOP.')
1100  FORMAT(' ERROR READING HEADER RECORD IN UNFORMATTED ',A,
     1' FILE. STOP.')
1200  FORMAT(' ERROR READING HEADER RECORD IN FORMATTED ',A,
     1' FILE. STOP.')
1300  FORMAT(1X,'END-OF-FILE ON UNIT ',I3,'. STOP.')
1400  FORMAT(1X,'LAYER ',I3,'READING HEAD FILE. ',
     1'EXPECTED HEAD FOR LAYER ',I3,'. FOUND HEAD FOR LAYER ',I3,
     2'. STOP.')
1500  FORMAT(1X,'ERROR READING HEAD ARRAY. STOP.')
      END 
      
      SUBROUTINE READTIMESTEPBUDGET(KPER,KSTP,NBYTES,IPROCESS,IOTYPE,
     1                              IRESET,NAREALSP)
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,IUNIT,ISSFLG,IBOUND,BUFF,CAREALSP,
     1      IBUFF,QX,QY,QZ,QSINK,QSOURCE,QSTO,IFACEASP,KIND8I
      USE MPBAS, ONLY: IRCHTOP,IEVTTOP
      USE DOUBLEBUDGET, ONLY: IPRBUD
      USE MPDATA, ONLY:IOLIST,IUBUDGET
      INTEGER ::KPER,KSTP,IUCBC,IFACE,IPROCESS,IOTYPE,IBYTES
      INTEGER ::IO,NAREALSP,ISTART,ISTOP,LENGTH
      INTEGER(KIND=KIND8I) NBYTES
      CHARACTER (LEN=16) TEXT,SPLABEL
!      CHARACTER (LEN=*), DIMENSION(NAREALSP) ::CADSTX
      
      IBYTES=0
      NBYTES=0
      IO=IOLIST
      IF(IOTYPE.GT.1) IO=0
      
      IF(IRESET.EQ.1) THEN
        CALL RDBDNM(TEXT,IUCBC,IOLIST,KPER,KSTP,NLAY,NROW,NCOL,
     1       NBTYPE,NVAL,NIFACE,NLST,IRESET,IBYTES,IOTYPE)
      END IF
C
C  ZERO ARRAYS FOR STORAGE, INTERNAL SOURCES, AND INTERNAL SINKS
C
      QSOURCE = 0.0
      QSINK = 0.0
      QSTO = 0.0
C
C  GENERATE PROCESSED FACE FLOW TERMS
C
      IF(IOTYPE.EQ.1) THEN
        IF(IPROCESS.EQ.0) THEN
          WRITE(IOLIST,1100) KPER,KSTP
        ELSE
          WRITE(IOLIST,1200) KPER,KSTP
        END IF
      END IF
      IUCBC=IUNIT(IUBUDGET)
C
C  READ STORAGE IF STRESS PERIOD IS TRANSIENT
C
      IF(IPRBUD.EQ.0) CALL BUDGETPRECISION(IUCBC,NCOL,NROW,NLAY,BUFF,
     1           IO)
      IF(ISSFLG(KPER).EQ.0) THEN
         CALL RDBUDG (BUFF,TEXT,NCOL,NROW,NLAY,IUCBC,IOLIST,KPER,KSTP,
     1           IBUFF,IBYTES,IOTYPE)
         IF(TEXT.NE.'         STORAGE') GO TO 100
         NBYTES=NBYTES + IBYTES
         IF(IPROCESS.NE.0) THEN
             DO K=1,NLAY
             DO I=1,NROW
             DO J=1,NCOL
               QSTO(J,I,K)=BUFF(J,I,K)
             END DO
             END DO
             END DO
         END IF
      END IF
C
C  READ CONSTANT HEAD FLOWS
C
      CALL RDBUDG (BUFF,TEXT,NCOL,NROW,NLAY,IUCBC,IOLIST,KPER,KSTP,
     1           IBUFF,IBYTES,IOTYPE)
      IF(TEXT.NE.'   CONSTANT HEAD') GO TO 100
      NBYTES=NBYTES + IBYTES
      IF(IPROCESS.NE.0) THEN
        DO K=1,NLAY
        DO I=1,NROW
        DO J=1,NCOL
          IF (BUFF(J,I,K).GE.0.0) THEN
            QSOURCE(J,I,K) = QSOURCE(J,I,K) + BUFF(J,I,K)
          ELSE
            QSINK(J,I,K) = QSINK(J,I,K) + BUFF(J,I,K)
          END IF
        END DO
        END DO
        END DO
      END IF
C
C  READ X FACE FLOWS
C
      IF(NCOL.GT.1) THEN
         CALL RDBUDG (BUFF,TEXT,NCOL,NROW,NLAY,IUCBC,IOLIST,KPER,KSTP,
     1           IBUFF,IBYTES,IOTYPE)
         IF(TEXT.NE.'FLOW RIGHT FACE ') GO TO 100
         NBYTES=NBYTES + IBYTES
      ELSE
         BUFF = 0.0
      END IF
      IF(IPROCESS.NE.0) THEN
        DO K=1,NLAY
        DO I=1,NROW
        QX(1,I,K)= 0.0
        DO J=1,NCOL
          QX(J+1,I,K)= BUFF(J,I,K)
        END DO
        END DO
        END DO
      END IF
C
C  READ Y FACE FLOWS
C
      IF(NROW.GT.1) THEN
         CALL RDBUDG (BUFF,TEXT,NCOL,NROW,NLAY,IUCBC,IOLIST,KPER,KSTP,
     1           IBUFF,IBYTES,IOTYPE)
         IF(TEXT.NE.'FLOW FRONT FACE ') GO TO 100
         NBYTES=NBYTES + IBYTES
      ELSE
         BUFF = 0.0
      END IF
      IF(IPROCESS.NE.0) THEN
        DO K=1,NLAY
        DO J=1,NCOL
        QY(J,1,K)= 0.0
        DO I=1,NROW
          QY(J,I+1,K)= -BUFF(J,I,K)
        END DO
        END DO
        END DO
      END IF
C
C  READ Z FACE FLOWS
C
      IF(NLAY.GT.1) THEN
         CALL RDBUDG (BUFF,TEXT,NCOL,NROW,NLAY,IUCBC,IOLIST,KPER,KSTP,
     1           IBUFF,IBYTES,IOTYPE)
         IF(TEXT.NE.'FLOW LOWER FACE ') GO TO 100
         NBYTES=NBYTES + IBYTES
      ELSE
         BUFF = 0.0
      END IF
      IF(IPROCESS.NE.0) THEN
        DO I=1,NROW
        DO J=1,NCOL
        QZ(J,I,1)= 0.0
        DO K=1,NLAY
          QZ(J,I,K+1)= -BUFF(J,I,K)
        END DO
        END DO
        END DO
      ELSE
          IF(IOTYPE.EQ.1) THEN
           WRITE(IOLIST,*) '  FLOW RATES BETWEEN CELLS HAVE BEEN READ'
          END IF
      END IF
C
C
C  Stress flows
C  Read a flow term
      DO
        CALL RDBDNM(TEXT,IUCBC,IOLIST,KPER,KSTP,NLAY,NROW,NCOL,
     1       NBTYPE,NVAL,NIFACE,NLST,0,IBYTES,IOTYPE)
        IF(TEXT.EQ.'END DATA') RETURN
        IF(TEXT.EQ.'EARLY DATA') THEN
          CALL USTOP('Budget file is out of sync with MODPATH. Stop.')
        END IF
        NBYTES=NBYTES + IBYTES
        
      IF(NBTYPE.EQ.5) THEN
C        Process stress packages that support IFACE through an auxilliary variable in the compact budget file
         IF(IPROCESS.EQ.1) THEN
           WRITE(IOLIST,'(1X,A)') 
     1     '     IFACE values will be read from a compact budget list.'
         END IF
         CALL ADDFLOWSLIST(IUCBC,NVAL,NIFACE,NLST,TEXT,IBYTES,IPROCESS)
         NBYTES=NBYTES + IBYTES      
      ELSE
C        Process all other stress packages that do not support IFACE through an auxilliary variable in the compact budget file
C        Start by looking to see if the budget text flag was specified in the MPBAS file. If so, get the IFACE value
C        from the IFACEASP array. If the package text label is not specified, set IFACE to 0.
         IFACE=0
         IF(IPROCESS.EQ.1) THEN
           SPLABEL = TEXT
           CALL UTRIMALL(SPLABEL)
           CALL UPCASE(SPLABEL)
           
           DO N=1,NAREALSP
             IF(SPLABEL.EQ.CAREALSP(N)) THEN
               IF(IFACEASP(N).GT.0 .AND. IFACEASP(N).LE.6) THEN
                IFACE=IFACEASP(N)
                WRITE(IOLIST,'(1X,A,I2)')
     1          '     Flows will be assigned to face ',IFACE
               ELSE
                WRITE(IOLIST,'(1X,A)')
     1          '     Flows will be treated as internally distributed'
               END IF
               EXIT
             END IF
             IF(N.EQ.NAREALSP) THEN
                WRITE(IOLIST,'(1X,A,A)') TRIM(SPLABEL),
     1          '     Flows will be treated as internally distributed'
             END IF
           END DO
           
         END IF
C       Incorporate the flow terms
        IF(NBTYPE.EQ.2) THEN
          CALL ADDFLOWSLIST(IUCBC,NVAL,-IFACE,NLST,TEXT,IBYTES,IPROCESS)
          NBYTES=NBYTES + IBYTES      
        ELSE
          CALL ADDFLOWS2D3D(IUCBC,NBTYPE,TEXT,IFACE,IBYTES,IPROCESS)
          NBYTES=NBYTES + IBYTES
        END IF
        
      END IF

C  Process next term
      END DO
C 
100   CONTINUE
      CALL USTOP(
     1'DATA IN MODFLOW BUDGET FILE IS MISSING OR OUT OF ORDER. STOP')
C
1000  FORMAT(1X,A,' DATA BEING READ FOR STRESS PERIOD ',I4,' , TIME STEP
     1 ',I4)
1100  FORMAT(1X,'READ FLOW PACKAGE BUDGET DATA FOR STRESS PERIOD ',
     1   I4,' , TIME STEP ',I4,' ...')
1200  FORMAT(1X,'PROCESS FLOW PACKAGE BUDGET DATA FOR ',
     1'STRESS PERIOD ',I4,' , TIME STEP ',I4,' ...')
C
      END
      
      SUBROUTINE LOADTIMESTEP(ITIMESTEP,NAREALSP)
      USE GLOBAL, ONLY:NIUNIT,NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,
     1                 ITMUNI,LENUNI,IXSEC,ITRSS,
     2                 IFREFM,NODES,IUNIT,HEAD,LBOTM,LAYCBD,
     3                 PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,BOTM,
     4                 IBOUND,IZONE,POR,BUFF,IBUFF,QX,QY,QZ,QSINK,
     5                 QSOURCE,QSTO,IGRIDNUM,NBFPOS,KIND8I,IBSTART
      USE MPBAS, ONLY:HNOFLO,HDRY  
      USE MPDATA, ONLY:NTPER,NTSTP,NGRIDS,IOLIST,IUBUDGET    
      INTEGER ::ITIMESTEP,IU,KPER,KSTP,IOTYPE,N,KS,KP
      INTEGER ::NAREALSP
      INTEGER(KIND8I) ::IPOS,NBYTES
      REAL ::PERTIM,TOTIM
      LOGICAL ::ISOK
!      CHARACTER (LEN=*), DIMENSION(NAREALSP) ::CADSTX
C
      
      KPER= NTPER(ITIMESTEP)
      KSTP= NTSTP(ITIMESTEP)
      IOTYPE= 1
      DO N=1,NGRIDS
C  Select grid
!        CALL SMP6MPBAS1PNT(N)
        CALL SELECTGRID(N)
        IU= IUNIT(IUBUDGET)
        
        WRITE(IOLIST,'(1X,A,I3)') 
     1  'PROCESSING HEAD AND BUDGET DATA FOR GRID ',N
     
C  Read head
        CALL READTIMESTEPHEAD(KSTP,KPER,KS,KP,PERTIM,TOTIM,ISOK,IOTYPE)
        IF(.NOT. ISOK) THEN
          WRITE(IOLIST, '(1X,A,I5,A,I5)') 
     1    'A PROBLEM OCCURRED READING HEAD FOR PERIOD ',KPER,
     2    ' STEP ',KSTP
          CALL USTOP('ERROR READING HEAD FILE. STOP.')
        END IF
C  Update IBOUND for no-flow and dry cells
        IF(IOTYPE.EQ.1) WRITE(IOLIST,'(1X,A)') 
     1'UPDATE IBOUND FOR NO-FLOW AND DRY CELLS.'
        CALL UPDATEIBOUND(IBOUND,IBSTART,HEAD,NCOL,NROW,NLAY,HNOFLO,
     1                    HDRY)
C  Read and process budget data
        IPOS= NBFPOS(ITIMESTEP)
        CALL SETFILEPOSITION(IU,IPOS,IOLIST)
        
        CALL READTIMESTEPBUDGET(KPER,KSTP,NBYTES,1,IOTYPE,1,NAREALSP)
      END DO
C
      RETURN
      END
      
      SUBROUTINE ADDFLOWSLIST(IU,NVAL,NIFACE,NLST,TEXT,IBYTES,IPROCESS)
C     ******************************************************************
C     Read a list budget term and add to QSOURCE/QSINK, or QX, QY, and QZ
C     ******************************************************************
      USE GLOBAL, ONLY:NIUNIT,NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,
     1                 ITMUNI,LENUNI,IXSEC,ITRSS,
     2                 IFREFM,NODES,IUNIT,HEAD,LBOTM,LAYCBD,
     3                 PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,BOTM,
     4                 IBOUND,IZONE,POR,BUFF,IBUFF,QX,QY,QZ,QSINK,
     5                 QSOURCE,QSTO      
      USE DOUBLEBUDGET, ONLY: IPRBUD
      USE MPDATA, ONLY:IOLIST
      INTEGER ::NRC,IFACE,ICELL,I,J,K,IB,IU,NVAL,NIFACE,NLST
      INTEGER ::IBYTES,IPROCESS
      REAL ::Q
      DOUBLE PRECISION VALD(20)
      REAL(KIND=4) VALS(20)
      REAL VAL(20)
      CHARACTER (LEN=16) TEXT
C
      IBYTES=0
      NRC=NROW*NCOL
      IFACE=0
      IF(NIFACE.LT.0) IFACE = -NIFACE
      
      IF(NLST.GT.0) THEN
         DO N=1,NLST
           IF(IPRBUD.EQ.2) THEN
             READ(IU,ERR=1000) ICELL,(VALD(I),I=1,NVAL)
             IBYTES=IBYTES + 4 + 8*NVAL
             DO I=1,NVAL
               VAL(I)=VALD(I)
             END DO
           ELSE
             READ(IU,ERR=1000) ICELL,(VALS(I),I=1,NVAL)
             IBYTES=IBYTES + 4*(NVAL+1)
             DO I=1,NVAL
               VAL(I)=VALS(I)
             END DO
           END IF
         
           IF(IPROCESS.NE.0) THEN
             Q=VAL(1)
             K= (ICELL-1)/NRC + 1
             I= ( (ICELL - (K-1)*NRC)-1 )/NCOL + 1
             J= ICELL - (K-1)*NRC - (I-1)*NCOL
             IF(NIFACE.GT.0) IFACE=VAL(NIFACE)
C
             IB=IBOUND(J,I,K)
             CALL ADDCELLFLOW(Q,J,I,K,IFACE,TEXT)
           END IF
           
         END DO
         
      END IF
C
      RETURN
C
1000  WRITE(IO,*) ' ADDFLOWSLIST -- error reading CBC budget file'
      CALL USTOP('ERROR READING BUDGET FILE. STOP.')
      END
      SUBROUTINE ADDFLOWS2D3D(IU,NBTYPE,TEXT,IFACE,IBYTES,IPROCESS)
C     ******************************************************************
C     Add an areal array budget term to QSOURCE/QSINK or QZ
C     ******************************************************************
      USE GLOBAL, ONLY:NIUNIT,NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,
     1                 ITMUNI,LENUNI,IXSEC,ITRSS,
     2                 IFREFM,NODES,IUNIT,HEAD,LBOTM,LAYCBD,
     3                 PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,BOTM,
     4                 IBOUND,IZONE,POR,BUFF,IBUFF,QX,QY,QZ,QSINK,
     5                 QSOURCE,QSTO      
      USE DOUBLEBUDGET, ONLY: IPRBUD,DBLBUFF,SNGBUFF
      USE MPDATA, ONLY:IOLIST
      INTEGER ::NL,IU,NBTYPE,IFACE,I,J,K,KK,IBYTES,IPROCESS
      REAL Q
      CHARACTER (LEN=16) TEXT
C
      IBYTES=0
      IF(IPRBUD.EQ.2) THEN
C  Double precision
        IF(NBTYPE.EQ.4) THEN
           NL=1
           READ(IU,ERR=1000) ((DBLBUFF(J,I,1),J=1,NCOL),I=1,NROW)
           IBYTES=IBYTES + 8*NCOL*NROW
           DO I=1,NROW
             DO J=1,NCOL
               BUFF(J,I,1)=DBLBUFF(J,I,1)
             END DO
           END DO
        ELSE IF(NBTYPE.EQ.3) THEN
           NL=1
           READ(IU,ERR=1000) ((IBUFF(J,I,1),J=1,NCOL),I=1,NROW)
           IBYTES=IBYTES + 4*NCOL*NROW
           READ(IU,ERR=1000) ((DBLBUFF(J,I,1),J=1,NCOL),I=1,NROW)
           IBYTES=IBYTES + 8*NCOL*NROW
           DO I=1,NROW
             DO J=1,NCOL
               BUFF(J,I,1)=DBLBUFF(J,I,1)
             END DO
           END DO
        ELSE
           NL=NLAY
           READ(IU,ERR=1000) DBLBUFF
           IBYTES=IBYTES + 8*NCOL*NROW*NLAY
           BUFF=DBLBUFF
        END IF
      ELSE
C  Single precision
        IF(NBTYPE.EQ.4) THEN
           NL=1
           READ(IU,ERR=1000) ((SNGBUFF(J,I,1),J=1,NCOL),I=1,NROW)
           IBYTES=IBYTES + 4*NCOL*NROW
           DO I=1,NROW
             DO J=1,NCOL
               BUFF(J,I,1)=SNGBUFF(J,I,1)
             END DO
           END DO
        ELSE IF(NBTYPE.EQ.3) THEN
           NL=1
           READ(IU,ERR=1000) ((IBUFF(J,I,1),J=1,NCOL),I=1,NROW)
           IBYTES=IBYTES + 4*NCOL*NROW
           READ(IU,ERR=1000) ((SNGBUFF(J,I,1),J=1,NCOL),I=1,NROW)
           IBYTES=IBYTES + 4*NCOL*NROW
           DO I=1,NROW
             DO J=1,NCOL
               BUFF(J,I,1)=SNGBUFF(J,I,1)
             END DO
           END DO
        ELSE
           NL=NLAY
           READ(IU,ERR=1000) SNGBUFF
           IBYTES=IBYTES + 4*NCOL*NROW*NLAY
           BUFF=SNGBUFF
        END IF
      END IF
C
      IF(IPROCESS.NE.0) THEN
      
      DO 100 KK=1,NL
      K=KK
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
      IF(NBTYPE.EQ.3) K=IBUFF(J,I,1)
      IF(BUFF(J,I,KK).NE.0.0) THEN
         CALL ADDCELLFLOW(BUFF(J,I,KK),J,I,K,IFACE,TEXT)
      END IF
C
100   CONTINUE

      END IF
C
      RETURN
C
1000  WRITE(IOLIST,*) ' ADDFLOWS2D3D -- error reading CBC budget file'
      CALL USTOP('ERROR READING BUDGET FILE. STOP.')
      END
      
      SUBROUTINE ADDCELLFLOW(Q,J,I,K,IFACE,TEXT)
      USE GLOBAL, ONLY:QX,QY,QZ,NROW,NCOL,NLAY
      REAL Q
      INTEGER J,I,K,IFACE,NBD
      CHARACTER(LEN=16) TEXT

C     If IFACE is negative, check all potential lateral boundary faces (1-4)
C     and uniformly distribute the flow over those faces.
      IF(IFACE.LT.0) THEN
        CALL FLUXBC(Q,J,I,K)
        RETURN
      END IF
      
C     If IFACE equals 0 through 6, assign the flows according to the specified IFACE
      IF(IFACE.EQ.0 .OR. IFACE.GT.6) THEN
         CALL ADDSOURCESINK(Q,J,I,K)
      ELSE
         IF(IFACE.EQ.1) THEN
            CALL FACTYP(J,1,J-1,I,K,NBD,J,I,K,IFACE,TEXT)
            IF (NBD.EQ.1) THEN
               QX(J,I,K)= QX(J,I,K) + Q
            ELSE
               CALL ADDSOURCESINK(Q,J,I,K)
            END IF
         ELSE IF(IFACE.EQ.2) THEN
            CALL FACTYP(J,NCOL,J+1,I,K,NBD,J,I,K,IFACE,TEXT)
            IF (NBD.EQ.1) THEN
               QX(J+1,I,K)= QX(J+1,I,K) - Q
            ELSE
               CALL ADDSOURCESINK(Q,J,I,K)
            END IF
         ELSE IF(IFACE.EQ.3) THEN
            CALL FACTYP(I,NROW,J,I+1,K,NBD,J,I,K,IFACE,TEXT)
            IF (NBD.EQ.1) THEN
               QY(J,I+1,K)= QY(J,I+1,K) + Q
            ELSE
               CALL ADDSOURCESINK(Q,J,I,K)
            END IF
         ELSE IF(IFACE.EQ.4) THEN
            CALL FACTYP(I,1,J,I-1,K,NBD,J,I,K,IFACE,TEXT)
            IF (NBD.EQ.1) THEN
               QY(J,I,K)= QY(J,I,K) - Q
            ELSE
               CALL ADDSOURCESINK(Q,J,I,K)
            END IF
         ELSE IF(IFACE.EQ.5) THEN
            CALL FACTYP(K,NLAY,J,I,K+1,NBD,J,I,K,IFACE,TEXT)
            IF (NBD.EQ.1) THEN
               QZ(J,I,K+1)= QZ(J,I,K+1) + Q
            ELSE
               CALL ADDSOURCESINK(Q,J,I,K)
            END IF
         ELSE IF(IFACE.EQ.6) THEN
            CALL FACTYP(K,1,J,I,K-1,NBD,J,I,K,IFACE,TEXT)
            IF (NBD.EQ.1) THEN
               QZ(J,I,K)= QZ(J,I,K) - Q
            ELSE
               CALL ADDSOURCESINK(Q,J,I,K)
            END IF
         END IF
      END IF
      
      RETURN
      END
      
      
      SUBROUTINE FLUXBC (Q,J,I,K)
C
      USE GLOBAL, ONLY:NIUNIT,NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,
     1                 ITMUNI,LENUNI,IXSEC,ITRSS,
     2                 IFREFM,NODES,IUNIT,HEAD,LBOTM,LAYCBD,
     3                 PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,BOTM,
     4                 IBOUND,IZONE,POR,BUFF,IBUFF,QX,QY,QZ,QSINK,
     5                 QSOURCE,QSTO
      USE MPDATA, ONLY:IOLIST
      INTEGER :: N
      REAL :: TOTALLENGTH
      REAL, DIMENSION(4) :: QSIDE,SIDELEN
C
      TOTALLENGTH=0.0E+0
      DO N=1,4
        QSIDE(N)=0.0
        SIDELEN(N)=0.0
      END DO
C
      IF(J.EQ.1) THEN
        TOTALLENGTH=TOTALLENGTH+DELC(I)
        SIDELEN(1)=DELC(I)
      ELSE IF(IBOUND(J-1,I,K).EQ.0) THEN
        TOTALLENGTH=TOTALLENGTH+DELC(I)
        SIDELEN(1)=DELC(I)
      END IF
C
      IF(J.EQ.NCOL) THEN
        TOTALLENGTH=TOTALLENGTH+DELC(I)
        SIDELEN(2)=DELC(I)
      ELSE IF (IBOUND(J+1,I,K).EQ.0) THEN
        TOTALLENGTH=TOTALLENGTH+DELC(I)
        SIDELEN(2)=DELC(I)
      END IF
C
      IF(I.EQ.1) THEN
        TOTALLENGTH=TOTALLENGTH+DELR(J)
        SIDELEN(4)=DELR(J)
      ELSE IF (IBOUND(J,I-1,K).EQ.0) THEN
        TOTALLENGTH=TOTALLENGTH+DELR(J)
        SIDELEN(4)=DELR(J)
      END IF
C
      IF(I.EQ.NROW) THEN
        TOTALLENGTH=TOTALLENGTH+DELR(J)
        SIDELEN(3)=DELR(J)
      ELSE IF (IBOUND(J,I+1,K).EQ.0) THEN
        TOTALLENGTH=TOTALLENGTH+DELR(J)
        SIDELEN(3)=DELR(J)
      END IF
C
      IF (ABS(TOTALLENGTH).LT.1.0E-20) THEN
          IF (Q.GE.0.0) THEN
              QSOURCE(J,I,K) = QSOURCE(J,I,K) + Q
          ELSE
              QSINK(J,I,K) = QSINK(J,I,K) + Q
          END IF
          RETURN
      END IF
C
      DO N=1,4
        QSIDE(N)= Q*(SIDELEN(N)/TOTALLENGTH)
      END DO
C
      QX(J,I,K)= QX(J,I,K) + QSIDE(1)
      QX(J+1,I,K)= QX(J+1,I,K) - QSIDE(2)
      QY(J,I+1,K)= QY(J,I+1,K) + QSIDE(3)
      QY(J,I,K)= QY(J,I,K) - QSIDE(4)
C
      RETURN
      END
      
      SUBROUTINE ADDSOURCESINK(Q,J,I,K)
      USE GLOBAL, ONLY:NIUNIT,NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,
     1                 ITMUNI,LENUNI,IXSEC,ITRSS,
     2                 IFREFM,NODES,IUNIT,HEAD,LBOTM,LAYCBD,
     3                 PERLEN,NSTP,TSMULT,ISSFLG,DELR,DELC,BOTM,
     4                 IBOUND,IZONE,POR,BUFF,IBUFF,QX,QY,QZ,QSINK,
     5                 QSOURCE,QSTO
      USE MPDATA, ONLY:IOLIST
      INTEGER :: I,J,K
      REAL :: Q
      
      IF (IBOUND(J,I,K).NE.0) THEN
          IF (Q.GE.0.0) THEN
              QSOURCE(J,I,K) = QSOURCE(J,I,K) + Q
          ELSE
              QSINK(J,I,K) = QSINK(J,I,K) + Q
          END IF
      END IF
      
      RETURN
      END
      
      SUBROUTINE FACTYP(N,NBOUND,JNXT,INXT,KNXT,NBD,J,I,K,IFACE,TEXT)
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,IBOUND
      USE MPDATA, ONLY:IOLIST
      INTEGER :: N,NBOUND,JNXT,INXT,KNXT,NBD,J,I,K,IFACE
      CHARACTER (LEN=16) TEXT
C
      NBD=0
      IF(N.EQ.NBOUND) THEN
          NBD=1
      ELSE
          IF (IBOUND(JNXT,INXT,KNXT).EQ.0) THEN
              NBD=1
          ELSE
              WRITE (IOLIST,5000) IFACE,J,I,K,TEXT
          END IF
      END IF
      RETURN
5000  FORMAT('FACE',I2,' OF (J,I,K) = ',I4,',',I4,',',I4,
     1' IS NOT A BOUNDARY FACE. ',A,' FLOW TREATED AS SOURCE/SINK TERM')
      END

      
      SUBROUTINE UPDATEIBOUND(IBOUND,IBSTART,HEAD,NCOL,NROW,NLAY,
     1                        HNOFLO,HDRY)
      INTEGER ::NCOL,NROW,NLAY,I,J,K
      INTEGER, DIMENSION(NCOL,NROW,NLAY) ::IBOUND
      INTEGER, DIMENSION(NCOL,NROW,NLAY) ::IBSTART
      REAL ::HNOFLO,HDRY,DELTA,TOL
      REAL, DIMENSION(NCOL,NROW,NLAY) ::HEAD
      
      IBOUND= IBSTART
      TOL = 0.0001

C  Check for HNOFLO
      IF(HNOFLO .EQ. 0.0) THEN
          DO K=1,NLAY
          DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).GE.0) THEN
              IF(HEAD(J,I,K) .EQ. 0.0) THEN
                IBOUND(J,I,K)=0
              ELSE
                IBOUND(J,I,K)=1
              END IF
            END IF
          END DO
          END DO
          END DO
      ELSE
          DO K=1,NLAY
          DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).GE.0) THEN
              DELTA = (HEAD(J,I,K) - HNOFLO)/HNOFLO
              IF (DELTA .LT. 0.0) DELTA = -DELTA
              IF(DELTA .LT. TOL) THEN
                IBOUND(J,I,K)=0
              ELSE
                IBOUND(J,I,K)=1
              END IF
            END IF
          END DO
          END DO
          END DO
      END IF

C  Check for HDRY      
      IF(HDRY .EQ. 0.0) THEN
          DO K=1,NLAY
          DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).GT.0) THEN
              IF(HEAD(J,I,K) .EQ. 0.0) THEN
                IBOUND(J,I,K)=0
              ELSE
                IBOUND(J,I,K)=1
              END IF
            END IF
          END DO
          END DO
          END DO
      ELSE
          DO K=1,NLAY
          DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).GT.0) THEN
              DELTA = (HEAD(J,I,K) - HDRY)/HDRY
              IF (DELTA .LT. 0.0) DELTA = -DELTA
              IF(DELTA .LT. TOL) THEN
                IBOUND(J,I,K)=0
              ELSE
                IBOUND(J,I,K)=1
              END IF
            END IF
          END DO
          END DO
          END DO
      END IF

      RETURN
      END
      
      SUBROUTINE CHECKBUDGET(IGRID,CMAX)
      USE GLOBAL, ONLY:NIUNIT,IGRIDNUM,NCOL,NROW,NLAY,NPER,
     1            ITRSS,LAYCBD,ISSFLG,IBOUND
      USE MPDATA, ONLY:CELL,CELLDATA,CELLBUDGET,IOLIST,IOBUDCHK
      TYPE(CELL) ::C,CMAX
      TYPE(CELLDATA) ::CD
      TYPE(CELLBUDGET) ::CB
      INTEGER ::IGRID,IU,I,J,K
      INTEGER ::NVC
      INTEGER, DIMENSION(5) ::NC
      CHARACTER (LEN = 50), DIMENSION(5) :: MESSAGE
      REAL ::MAXBAL
      
      MESSAGE(1) = '0.01 AND 0.1 PERCENT'
      MESSAGE(2) = '0.1 AND 1.0 PERCENT'
      MESSAGE(3) = '1.0 AND 10.0 PERCENT'
      MESSAGE(4) = '10.0 AND 50.0 PERCENT'
      MESSAGE(5) = '50.0 PERCENT'
      
      CALL SELECTGRID(IGRID)
      C%GRID = IGRID
      CMAX%GRID = IGRID
      
      NVC = 0
      NC = 0
      
      MAXBAL = -1.0
      DO K=1,NLAY
        C%K = K
        DO I=1,NROW
          C%I = I
          DO J=1,NCOL
            C%J = J
            IF(IBOUND(J,I,K).GT.0) THEN
              NVC = NVC + 1
              CALL GETCELLBUDGET(C,CD,CB)
              
              IF(CB%BALANCE.GE.50.0) THEN
                NC(5) = NC(5) + 1
              ELSE IF(CB%BALANCE.GE.10.0) THEN
                NC(4) = NC(4) + 1
              ELSE IF(CB%BALANCE.GE.1.0) THEN
                NC(3) = NC(3) + 1
              ELSE IF(CB%BALANCE.GE.0.1) THEN
                NC(2) = NC(2) + 1
              ELSE IF(CB%BALANCE.GE.0.01) THEN
                NC(1) = NC(1) + 1
              END IF
              
              IF(CB%BALANCE.GT.MAXBAL) THEN
                MAXBAL = CB%BALANCE
                CMAX%I = C%I
                CMAX%J = C%J
                CMAX%K = C%K
              END IF
              
            END IF
          END DO
        END DO
      END DO
      
      WRITE(IOLIST,*)
      WRITE(IOLIST,'(1X,A,I3)') 'CHECKING BUDGET DATA FOR GRID ',IGRID
      WRITE(IOLIST,'(3X,A,I10)') 'NUMBER OF VARIABLE HEAD CELLS =',NVC
      DO N=1,4
        WRITE(IOLIST,'(3X,I10,A,A)') NC(N),
     1  ' CELLS HAD BALANCE ERRORS BETWEEN ',MESSAGE(N)
      END DO
      WRITE(IOLIST,'(3X,I10,A,A)') NC(5),
     1 ' CELLS HAD BALANCE ERRORS GREATER THAN ',MESSAGE(5)
      WRITE(IOLIST,*)
      WRITE(IOLIST,'(3X,A,F12.6,A,I5,A,I5,A,I5)') 
     1 'A MAXIMUM ERROR OF ',MAXBAL,' PERCENT OCCURRED IN LAYER',CMAX%K,
     2 '  ROW',CMAX%I,'  COLUMN',CMAX%J
     
      RETURN
      END
      
      SUBROUTINE CHECKCELLBUDGET(C,IGRID,KTIME,KPER,KSTP)
      USE GLOBAL, ONLY:NIUNIT,IGRIDNUM,NCOL,NROW,NLAY,NPER,
     1            ITRSS,LAYCBD,ISSFLG
      USE MPDATA, ONLY:CELL,CELLDATA,CELLBUDGET,IOLIST,IOBUDCHK
      TYPE(CELL) ::C
      TYPE(CELLDATA) ::CD
      TYPE(CELLBUDGET) ::CB
      INTEGER IGRID,KTIME,KPER,KSTP,IB
      REAL RES,BAL,QFACEIN,QFACEOUT
      CHARACTER(LEN=5), DIMENSION(6) ::ATEXT
      CHARACTER(LEN=5) AIN,AOUT
      
      IF(C%GRID .NE. IGRID) RETURN
      CALL SELECTGRID(IGRID)
      
      CALL GETCELLDATA(C%I,C%J,C%K,CD)
      CALL GETCELLBUDGET(C,CD,CB)
      
      ATEXT = '   '
      AIN =  '(IN) '
      AOUT = '(OUT)'
      
      DO N=1,6
        IF(CB%FACEDIR(N).EQ.1) THEN
          ATEXT(N) = AIN
        ELSE IF(CB%FACEDIR(N).EQ.2) THEN
          ATEXT(N) = AOUT
        END IF
      END DO
       
      WRITE(IOLIST,'(3X,A,I2,A,I4,A,I4,A,I4)') 'GRID ',C%GRID,
     1  '   LAYER ',C%K,'   ROW ',C%I,'   COLUMN ',C%J  
      WRITE(IOLIST,'(5X,A)') 'FACE FLOWS:'
      WRITE(IOLIST,1000) 'QX1 = ',CD%QX1,ATEXT(1),'QX2 = ',CD%QX2,
     1                   ATEXT(2)
      WRITE(IOLIST,1000) 'QY1 = ',CD%QY1,ATEXT(3),'QY2 = ',CD%QY2,
     1                   ATEXT(4)
      WRITE(IOLIST,1000) 'QZ1 = ',CD%QZ1,ATEXT(5),'QZ2 = ',CD%QZ2,
     1                   ATEXT(6)
      WRITE(IOLIST,1010) 'CELL FACE FLOWS:  FLOW IN = ',CB%QFACEIN,
     1 '  FLOW OUT = ',CB%QFACEOUT,'NET INFLOW (IN - OUT) = ',
     2 CB%QFACENET
      WRITE(IOLIST,1010) 'FLOW TO SINKS = ',CD%QSINK,
     1 'FLOW FROM SOURCES = ',CD%QSOURCE,'FLOW FROM STORAGE = ',CD%QSTO
      WRITE(IOLIST,1020) 'VOLUMETRIC RESIDUAL = ',CB%QRESIDUAL,
     1 'VOLUMETRIC BALANCE(%) = ',CB%BALANCE
      WRITE(IOLIST,*)
      
1000  FORMAT(10X,A,E15.7,A5,5X,A,E15.7,1X,A5)
1010  FORMAT(5X,A,E15.7,3X,A,E15.7,3X,A,E15.7)
1020  FORMAT(5X,A,E15.7,3X,A,F12.7)

      RETURN
      END
      
      SUBROUTINE GETCELLBUDGET(C,CD,CB)
      USE GLOBAL, ONLY:NIUNIT,IGRIDNUM,NCOL,NROW,NLAY,NPER,
     1            ITRSS,LAYCBD,ISSFLG
      USE MPDATA, ONLY:CELL,CELLDATA,CELLBUDGET,IOLIST,IOBUDCHK
      TYPE(CELL) ::C
      TYPE(CELLDATA) ::CD
      TYPE(CELLBUDGET) ::CB
      INTEGER IGRID,KTIME,KPER,KSTP
      REAL RES,BAL,QFACEIN,QFACEOUT
      
      CALL GETCELLDATA(C%I,C%J,C%K,CD)
      CB%QFACEIN = 0.0
      CB%QFACEOUT = 0.0
      
      DO N=1,6
        CB%FACEDIR(N) = 0
      END DO
      
      IF(CD%QX1.NE.0.0) THEN
        IF(CD%QX1.GT.0.0) THEN
          CB%QFACEIN = CB%QFACEIN + CD%QX1
          CB%FACEDIR(1) = 1
        ELSE
          CB%QFACEOUT = CB%QFACEOUT - CD%QX1
          CB%FACEDIR(1) = 2
        END IF
      END IF
      
      IF(CD.QX2.NE.0.0) THEN
        IF(CD%QX2.GT.0.0) THEN
          CB%QFACEOUT = CB%QFACEOUT + CD%QX2
          CB%FACEDIR(2) = 2
        ELSE
          CB%QFACEIN = CB%QFACEIN - CD%QX2
          CB%FACEDIR(2) = 1
        END IF
      END IF
      
      IF(CD%QY1.NE.0.0) THEN
        IF(CD%QY1.GE.0.0) THEN
          CB%QFACEIN = CB%QFACEIN + CD%QY1
          CB%FACEDIR(3) = 1
        ELSE
          CB%QFACEOUT = CB%QFACEOUT - CD%QY1
          CB%FACEDIR(3) = 2
        END IF
      END IF
      
      IF(CD%QY2.NE.0.0) THEN
        IF(CD%QY2.GE.0.0) THEN
          CB%QFACEOUT = CB%QFACEOUT + CD%QY2
          CB%FACEDIR(4) = 2
        ELSE
          CB%QFACEIN = CB%QFACEIN - CD%QY2
          CB%FACEDIR(4) = 1
        END IF
      END IF
      
      IF(CD%QZ1.NE.0.0) THEN
        IF(CD%QZ1.GE.0.0) THEN
          CB%QFACEIN = CB%QFACEIN + CD%QZ1
          CB%FACEDIR(5) = 1
        ELSE
          CB%QFACEOUT = CB%QFACEOUT - CD%QZ1
          CB%FACEDIR(5) = 2
        END IF
      END IF
      
      IF(CD%QZ2.NE.0.0) THEN
        IF(CD%QZ2.GE.0.0) THEN
          CB%QFACEOUT = CB%QFACEOUT + CD%QZ2
          CB%FACEDIR(6) = 2
        ELSE
          CB%QFACEIN = CB%QFACEIN - CD%QZ2
          CB%FACEDIR(6) = 1
        END IF
      END IF
      
      CB%QFACENET = CB%QFACEIN - CB%QFACEOUT
      CB%QRESIDUAL = CB%QFACENET + CD%QSINK + CD%QSOURCE + CD%QSTO
      CB%QAVE = CB%QFACEIN + CB%QFACEOUT + CD%QSOURCE - CD%QSINK 
      IF(QTO.GE.0.0) THEN
        CB%QAVE = CB%QAVE + CD%QSTO
      ELSE
        CB%QAVE = CB%QAVE - CD%QSTO
      END IF
      CB%QAVE = CB%QAVE/2.0
      CB%BALANCE = ABS(100.0*CB%QRESIDUAL/CB%QAVE)
      
      RETURN
      END
      
      SUBROUTINE HEADSTATS(IGRID)
      USE GLOBAL, ONLY:HEAD,NCOL,NROW,NLAY
      USE MPBAS, ONLY:HNOFLO,HDRY
      USE MPDATA, ONLY:IOLIST
      INTEGER ::I,J,K,N,NN,INITFLAG,IGRID
      INTEGER ::NOFLOWCOUNT,DRYCOUNT,INACTIVECOUNT,LAYERCELLCOUNT
      REAL ::MINHEAD,MAXHEAD
      REAL ::H,DH
      REAL, DIMENSION(5) ::HRANGE
      INTEGER, DIMENSION(5) ::RANGECOUNT
      
      CALL SELECTGRID(IGRID)

C     Loop through layers and process head statistics for the layer
      DO K = 1,NLAY
        WRITE(IOLIST,*)
        WRITE(IOLIST,1001) K
C     Count no-flow and dry cells. 
C     Initialize the minimum and maximum head values to the first cell active cell that is encountered in each layer.
        INITFLAG = 0
        NOFLOWCOUNT = 0
        DRYCOUNT = 0
        RANGECOUNT = 0
        DO I = 1,NROW
          DO J = 1,NCOL
            H = HEAD(J,I,K)
            IF(H .EQ. HNOFLO) THEN
              NOFLOWCOUNT = NOFLOWCOUNT + 1
            ELSE IF(H .EQ. HDRY) THEN
              DRYCOUNT = DRYCOUNT + 1
            ELSE
              IF(INITFLAG .EQ. 0) THEN
                INITFLAG = 1
                MINHEAD = H
                MAXHEAD = MINHEAD
              ELSE
                IF(H .LT. MINHEAD) MINHEAD = H
                IF(H .GT. MAXHEAD) MAXHEAD = H
              END IF
            END IF
          END DO
        END DO
        
        IF(HNOFLO .NE. HDRY) THEN
          WRITE(IOLIST,1010) NOFLOWCOUNT,HNOFLO
          WRITE(IOLIST,1020) DRYCOUNT,HDRY
        ELSE
          INACTIVECOUNT = NOFLOWCOUNT + DRYCOUNT
          WRITE(IOLIST,1030) INACTIVECOUNT,HNOFLO
        END IF
        
        IF(INITFLAG .EQ. 1) THEN
          IF(MINHEAD .EQ. MAXHEAD) THEN
            HRANGE = MAXHEAD
            NN = 1
          ELSE
            NN = 5
            DH = (MAXHEAD - MINHEAD)/5.0
            HRANGE(1) = MINHEAD + DH
            HRANGE(5) = MAXHEAD
            DO N = 2,4
              HRANGE(N) = HRANGE(N-1) + DH
            END DO
          END IF
          
          DO I = 1,NROW
            DO J = 1,NCOL
              H = HEAD(J,I,K)
              IF((H .NE. HNOFLO) .AND. (H .NE. HDRY)) THEN
                IF(H .LE. HRANGE(1)) THEN
                  RANGECOUNT(1) = RANGECOUNT(1) + 1
                ELSE IF(H .LE. HRANGE(2)) THEN
                  RANGECOUNT(2) = RANGECOUNT(2) + 1
                ELSE IF(H .LE. HRANGE(3)) THEN
                  RANGECOUNT(3) = RANGECOUNT(3) + 1
                ELSE IF(H .LE. HRANGE(4)) THEN
                  RANGECOUNT(4) = RANGECOUNT(4) + 1
                ELSE IF(H .LE. HRANGE(5)) THEN
                  RANGECOUNT(5) = RANGECOUNT(5) + 1
              END IF
              END IF
            END DO
          END DO
          
          WRITE(IOLIST,*)
          WRITE(IOLIST,1002) MINHEAD,MAXHEAD
          H = MINHEAD
          DO N = 1,NN
            WRITE(IOLIST,1040) RANGECOUNT(N),H,HRANGE(N)
            H = HRANGE(N)
          END DO
          
        ELSE
          WRITE(IOLIST,*)
          WRITE(IOLIST,1003)
        END IF
        
        
      END DO
      
1001  FORMAT(1X,'Head statistics for layer',I4)
1002  FORMAT(12X,'Minimum head = ',E15.7,'  Maximum head = ',E15.7)
1003  FORMAT(12X,'The layer does not contain any active head cells.')
1010  FORMAT(1X,I10,' cells are inactive no-flow cells (HNOFLO = '
     1 ,E15.7,')')
1020  FORMAT(1X,I10,' cells are inactive dry cells (HDRY = ',E15.7,')')
1030  FORMAT(1X,I10,' cells are inactive no-flow or dry cells '
     1 '(HNOFLO = HDRY = ',E15.7,')')
1040  FORMAT(1X,I10,' cells have heads in the range ',E15.7,' to '
     1  ,E15.7)

      
      RETURN
      END
