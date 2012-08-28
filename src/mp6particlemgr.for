      SUBROUTINE READPARTICLEGROUP(IU,IGROUP,IDMAX)
C***********************************************************************
C  READ PARTICLE DATA FOR A PARTICLE GROUP AND GENERATE THE PARTICLES
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE MPDATA, ONLY:ITRACKDIR,IOLIST,SIMTIME,NTSTEPS,CELLBLOCK
      USE PARTICLEDATA, ONLY:NPGROUPS,PGROUPS,PARTICLE,PARTICLEGROUP
      USE GLOBAL, ONLY:IBOUND,IBUFF,NCOL,NROW,NLAY
      INTEGER ::N,ILOCOPT,IRELEASE,NRP,I,J,K,NT,IMIN,IMAX,JMIN,JMAX
      INTEGER ::KMIN,KMAX,NI,NJ,NK,NG,NCELLS,IPG,GRID,IPNUM,IDMAX
      INTEGER ::II,JJ,KK,NPARRAY,IPASS,IGEN,IPCH,IOPTREL,IAROPT,KDRAPE
      REAL ::RPLEN,RTIME,TINC,T,XL,YL,ZL,DELROW,DELCOL,TBEGIN,TEND
      REAL ::RTSTART,RTEND
      REAL, ALLOCATABLE ::RT(:)
      CHARACTER (LEN=16) ::NAME
      CHARACTER*24 ANAME(1)
      DATA ANAME(1) /'              MASK ARRAY'/
      TYPE(PARTICLE), POINTER ::PCL
      TYPE(CELLBLOCK) ::CB
C-----------------------------------------------------------------------
      READ(IU,*) PGROUPS(IGROUP)%NAME
      READ(IU,*) GRID,IAROPT,ILOCOPT,RTSTART,IOPTREL,IPCH
      NRP = 0
      RPLEN = 0.0
      IF(IOPTREL.EQ.1) THEN
        ALLOCATE(RT(0:0))
        RT(0) = RTSTART
        RTEND = RTSTART
      ELSE IF(IOPTREL.EQ.2) THEN
        READ(IU,*) RPLEN,NRP
        ALLOCATE(RT(0:NRP))
        RT(0) = RTSTART
        IF(NRP.GT.0) THEN
          IF (NRP.GT.0) TINC = RPLEN/REAL(NRP)
          DO N=1,NRP
            RT(N) = RT(N-1) + TINC
          END DO
        END IF
        RTEND= RT(NRP)
      ELSE IF(IOPTREL.EQ.3) THEN
        READ(IU,*) NRP
        ALLOCATE(RT(0:NRP))
        RT(0) = RTSTART
        IF(NRP.GT.0) THEN
          READ(IU,*) (RT(N), N=1,NRP)
        END IF
        RTEND = RT(NRP)
        
      END IF
      
      WRITE(IOLIST,*)
      WRITE(IOLIST,'(1X,A,I5)')
     1'PARTICLE GROUP ',IGROUP
      WRITE(IOLIST,'(5X,A,A)') 'NAME: ',PGROUPS(IGROUP)%NAME
      WRITE(IOLIST,'(5X,A,I4)') 'PARTICLES WILL BE GENERATED FOR GRID',
     1  GRID
      IF(IPCH.EQ.1) THEN
        WRITE(IOLIST,'(5X,A)') 
     1  'PARTICLES ARE NOT GENERATED FOR CONSTANT HEAD CELLS'
      ELSE
        WRITE(IOLIST,'(5X,A)') 
     1  'PARTICLES ARE GENERATED FOR CONSTANT HEAD CELLS'        
      END IF
      
C  Select grid
      CALL SMP6MPBAS1PNT(GRID)

C  Generate particles
      CB%GRID = GRID
      KDRAPE = 0
      SELECT CASE (IAROPT)
        CASE (1)
          READ(IU,*) CB%KMIN,CB%IMIN,CB%JMIN,CB%KMAX,CB%IMAX,CB%JMAX
          IBUFF = 1
        CASE (2)
          CB%KMIN = 1
          CB%IMIN = 1
          CB%JMIN = 1
          CB%KMAX = NLAY
          CB%IMAX = NROW
          CB%JMAX = NCOL
          DO K = 1,NLAY
            KK=K
            CALL U2DINT(IBUFF(:,:,KK),ANAME(1),NROW,NCOL,KK,IU,IOLIST)
          END DO
        CASE (3)
          READ(IU,*) KK
          IF(KK .LT. 1) THEN
            KK = 1
            KDRAPE = 1
          END IF
          CB%KMIN = KK
          CB%IMIN = 1
          CB%JMIN = 1
          CB%KMAX = KK
          CB%IMAX = NROW
          CB%JMAX = NCOL
          IBUFF = 0
          CALL U2DINT(IBUFF(:,:,KK),ANAME(1),NROW,NCOL,KK,IU,IOLIST)
        CASE DEFAULT
          CALL USTOP('INVALID PARTICLE GROUP INPUT OPTION (IAROPT).')
      END SELECT
      
      SELECT CASE (ILOCOPT)
        CASE (1)
          CALL FACEPARTICLES(IU,IGROUP,NRP,RTSTART,RT,IDMAX,CB,IPCH,
     1                       KDRAPE)
        CASE (2)
          CALL INTERNALPARTICLES(IU,IGROUP,NRP,RTSTART,RT,IDMAX,CB,
     1                           IPCH,KDRAPE)
        CASE DEFAULT
          CALL USTOP('INVALID PARTICLE STARTING LOCATION OPTION.')
      END SELECT
      
      IF(IOPTREL.EQ.1) THEN
        WRITE(IOLIST,'(5X,A)') 'SINGLE RELEASE:'
        WRITE(IOLIST,'(5X,A,1PE15.6)') 
     1    'RELEASE TIME = ',RTSTART
      ELSE IF(IOPTREL.EQ.2) THEN
        WRITE(IOLIST,'(5X,A)') 
     1   'MULTIPLE RELEASE WITH CONSTANT TIME INTERVALS:'
        WRITE(IOLIST,'(5X,A,1PE15.6)') 
     1    'INITIAL RELEASE TIME = ',RTSTART
        WRITE(IOLIST,'(5X,A,1PE15.6)') 'RELEASE PERIOD LENGTH = ',RPLEN
        WRITE(IOLIST,'(5X,A,I5)') 'NUMBER OF RELEASE INTERVALS = ',NRP
        WRITE(IOLIST,'(5X,A,1PE15.6)') 
     1    'FINAL RELEASE TIME = ',RTEND
      ELSE IF(IOPTREL.EQ.3) THEN
        WRITE(IOLIST,'(5X,A)') 
     1   'MULTIPLE RELEASE WITH VARIABLE TIME INTERVALS:'
        WRITE(IOLIST,'(5X,A,1PE15.6)') 
     1    'INITIAL RELEASE TIME = ',RTSTART
        WRITE(IOLIST,'(5X,A,I5)') 'NUMBER OF RELEASE INTERVALS = ',NRP
        WRITE(IOLIST,'(5X,A,1PE15.6)') 
     1    'FINAL RELEASE TIME = ',RTEND

      END IF
        

      WRITE(IOLIST,'(5X,A,I9)') 'NUMBER OF PARTICLES = ',
     1 PGROUPS(IGROUP)%COUNT
     
      RETURN
      END
      
      SUBROUTINE FACEPARTICLES(IU,IGROUP,NRP,RTSTART,RT,IDMAX,CB,
     1                         IPCH,KDRAPE)
C***********************************************************************
C  GENERATE A 2D ARRAYS OF PARTICLES ON CELL FACES FOR A SPECIFIED
C  REGION OF GRID CELLS
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE MPDATA, ONLY:ITRACKDIR,IOLIST,SIMTIME,NTSTEPS,CELLBLOCK
      USE PARTICLEDATA, ONLY:NPGROUPS,PGROUPS,PARTICLE,PARTICLEGROUP
      USE GLOBAL, ONLY:IBOUND,IBUFF
      INTEGER ::N,ILOCOPT,IRELEASE,NRP,I,J,K,NT,IMIN,IMAX,JMIN,JMAX
      INTEGER ::KMIN,KMAX,NI,NJ,NK,NG,NCELLS,IPG,IGRID,IPNUM,IDMAX
      INTEGER ::II,JJ,NPARRAY,IPASS,IGEN,IPCH,IOPTREL,NFACES,NPROW,NPCOL
      INTEGER ::KDRAPE
      INTEGER, DIMENSION(3,6) ::IFO
      REAL ::RPLEN,RTIME,T,XL,YL,ZL,DELROW,DELCOL,TBEGIN,TEND
      REAL ::RT(0:NRP)
      CHARACTER (LEN=16) ::NAME
      TYPE(PARTICLE), POINTER ::PCL
      TYPE(CELLBLOCK) ::CB
      
      IGRID = CB%GRID
      KMIN = CB%KMIN
      IMIN = CB%IMIN
      JMIN = CB%JMIN
      KMAX = CB%KMAX
      IMAX = CB%IMAX
      JMAX = CB%JMAX
      
      WRITE(IOLIST,'(5X,A)')
     1  'PARTICLES ARE GENERATED ON CELL FACES.'
        WRITE(IOLIST,'(5X,A,I4,A,I4,A,I4,A,I4,A,I4,A,I4,A)')
     1'REGION DEFINED BY CELLS (LAYER ',KMIN,', ROW ',IMIN,' COLUMN ',
     2JMIN,') TO (LAYER ',KMAX,', ROW ',IMAX,', COLUMN ',JMAX,')'
     
      IFO = 0
      READ(IU,*) NFACES
      DO N=1,NFACES
        READ(IU,*) IFACE,NPROW,NPCOL
        IFO(1,IFACE) = 1
        IFO(2,IFACE) = NPROW
        IFO(3,IFACE) = NPCOL
        IF(IFACE.LT.5) THEN
          WRITE(IOLIST,'(5X,A,I2,A,I5,A,I5,A)') 'FACE ',IFACE,': ',
     1   IFO(2,IFACE),' HORIZONTAL ROWS, ',IFO(3,IFACE),
     2   ' VERTICAL COLUMNS'
        ELSE
          WRITE(IOLIST,'(5X,A,I2,A,I5,A,I5,A)') 'FACE ',IFACE,': ',
     1    IFO(2,IFACE),' ROWS, ',IFO(3,IFACE),' COLUMNS'
        END IF
      END DO

      NCELLS=0
      IPNUM = 0
        
      DO IPASS = 1,2
        IF(IPASS.EQ.2) THEN
          IF(ALLOCATED(PGROUPS(IGROUP)%PARTICLES)) THEN
            DEALLOCATE(PGROUPS(IGROUP)%PARTICLES)
          ENDIF
          ALLOCATE(PGROUPS(IGROUP)%PARTICLES(0:IPNUM))
          PGROUPS(IGROUP)%COUNT = IPNUM
          IPNUM = 0
        ENDIF
        
        DO NT=0,NRP
          DO K=KMIN,KMAX
          DO I=IMIN,IMAX
          DO J=JMIN,JMAX
          IF(IBUFF(J,I,K).GT.0) THEN
            IGEN = 1
            IF(KDRAPE .EQ. 0) THEN
              IF(IBOUND(J,I,K).EQ.0) THEN
                IGEN = 0
              ELSEIF(IBOUND(J,I,K).LT.0 .AND. IPCH.EQ.1) THEN
                IGEN = 0
              ENDIF
            END IF
            
            IF(IGEN.EQ.1) THEN
              DO N=1,6
               IF(IFO(1,N).EQ.1) THEN
                DO II=1,IFO(2,N)
                DO JJ=1,IFO(3,N)
                 IPNUM=IPNUM + 1
C  ASSIGN PARTICLE ON THE SECOND PASS THROUGH THE LOOP
                 IF(IPASS.EQ.2) THEN
                 IDMAX = IDMAX + 1
                 PCL=> PGROUPS(IGROUP)%PARTICLES(IPNUM)
                 PCL%LABEL = PGROUPS(IGROUP)%NAME
                 PCL%GRID= IGRID
                 PCL%GROUP= IGROUP
                 PCL%INDEX= IPNUM
                 PCL%ID = IDMAX
                 PCL%I=I
                 PCL%J=J
                 PCL%K=0
                 IF(KDRAPE .EQ. 0) THEN
                   PCL%K=K
                 END IF
                 CALL GENLOCALXYZF(N,IFO(2,N),IFO(3,N),II,JJ,XL,YL,ZL)
                 PCL%XL=XL
                 PCL%YL=YL
                 PCL%ZL=ZL
                 PCL%GRIDB = PCL%GRID
                 PCL%IB = PCL%I
                 PCL%JB = PCL%J
                 PCL%KB = PCL%K
                 PCL%XLB = PCL%XL
                 PCL%YLB = PCL%YL
                 PCL%ZLB = PCL%ZL
                 PCL%FACEB = N
                 PCL%FACE = N
                 PCL%TIME = RT(NT)
                 PCL%TIMEB = PCL%TIME
                 PCL%STATUS = 0
                 PCL%CELLCOUNT = 1
                 PCL%LINENUM = 0
                 END IF
                END DO
                END DO
               END IF
              END DO
            END IF
C              
          END IF
          END DO
          END DO
          END DO
        END DO
      END DO
      
      END
      
      SUBROUTINE INTERNALPARTICLES(IU,IGROUP,NRP,RTSTART,RT,IDMAX,CB,
     1                             IPCH,KDRAPE)
C***********************************************************************
C  GENERATE A 3D ARRAY OF PARTICLES FOR A SPECIFIED REGION OF GRID CELLS
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE MPDATA, ONLY:ITRACKDIR,IOLIST,SIMTIME,NTSTEPS,CELLBLOCK
      USE PARTICLEDATA, ONLY:NPGROUPS,PGROUPS,PARTICLE,PARTICLEGROUP
      USE GLOBAL, ONLY:IBOUND,IBUFF
      INTEGER ::N,ILOCOPT,IRELEASE,NRP,I,J,K,NT,IMIN,IMAX,JMIN,JMAX
      INTEGER ::KMIN,KMAX,NI,NJ,NK,NG,NCELLS,IPG,IGRID,IPNUM,IDMAX
      INTEGER ::II,JJ,NPARRAY,IPASS,IGEN,IPCH,IOPTREL,KDRAPE
      INTEGER, DIMENSION(3,6) ::IFO
      REAL ::RPLEN,RTIME,T,XL,YL,ZL,DELROW,DELCOL,TBEGIN,TEND
      REAL ::RT(0:NRP)
      CHARACTER (LEN=16) ::NAME
      TYPE(PARTICLE), POINTER ::PCL
      TYPE(CELLBLOCK) ::CB
C-----------------------------------------------------------------------
      IGRID = CB%GRID
      KMIN = CB%KMIN
      IMIN = CB%IMIN
      JMIN = CB%JMIN
      KMAX = CB%KMAX
      IMAX = CB%IMAX
      JMAX = CB%JMAX
      
      WRITE(IOLIST,'(5X,A)')
     1  'PARTICLES ARE GENERATED INTERNALLY WITHIN CELLS.'
      WRITE(IOLIST,'(5X,A,I4,A,I4,A,I4,A,I4,A,I4,A,I4,A)')
     1'REGION DEFINED BY CELLS (LAYER ',KMIN,', ROW ',IMIN,' COLUMN ',
     2JMIN,') TO (LAYER ',KMAX,', ROW ',IMAX,', COLUMN ',JMAX,')'
        READ(IU,*) NK,NI,NJ
        WRITE(IOLIST,'(1X,I5,A,I5,A,I5,A)') 
     1NK,' LAYERS,',NI,' ROWS,',NJ,' COLUMNS'

      NCELLS=0
      IPNUM = 0
        
        DO IPASS = 1,2
          IF(IPASS.EQ.2) THEN
            IF(ALLOCATED(PGROUPS(IGROUP)%PARTICLES)) THEN
              DEALLOCATE(PGROUPS(IGROUP)%PARTICLES)
            ENDIF
            ALLOCATE(PGROUPS(IGROUP)%PARTICLES(0:IPNUM))
            PGROUPS(IGROUP)%COUNT = IPNUM
            IPNUM = 0
          ENDIF
          
          DO NT=0,NRP
            DO K=KMIN,KMAX
            DO I=IMIN,IMAX
            DO J=JMIN,JMAX
            IF(IBUFF(J,I,K).GT.0) THEN
             IGEN = 1
             IF(KDRAPE .EQ. 0) THEN
               IF(IBOUND(J,I,K).EQ.0) THEN
                 IGEN = 0
               ELSEIF(IBOUND(J,I,K).LT.0 .AND. IPCH.EQ.1) THEN
                 IGEN = 0
               END IF
             END IF
             
              IF(IGEN.EQ.1) THEN
                DO KK=1,NK
                DO II=1,NI
                DO JJ=1,NJ
                IPNUM=IPNUM + 1
                IF(IPASS.EQ.2) THEN
                 IDMAX = IDMAX + 1
                 PCL=> PGROUPS(IGROUP)%PARTICLES(IPNUM)
                 PCL%LABEL= PGROUPS(IGROUP)%NAME
                 PCL%GRID= IGRID
                 PCL%GROUP= IGROUP
                 PCL%INDEX= IPNUM
                 PCL%ID = IDMAX
                 PCL%I=I
                 PCL%J=J
                 PCL%K=0
                 IF(KDRAPE .EQ. 0) THEN
                   PCL%K=K
                 END IF
                 CALL GENLOCALXYZI(NI,NJ,NK,II,JJ,KK,XL,YL,ZL)    
                 PCL%XL=XL
                 PCL%YL=YL
                 PCL%ZL=ZL
                 PCL%GRIDB = PCL%GRID
                 PCL%IB = PCL%I
                 PCL%JB = PCL%J
                 PCL%KB = PCL%K
                 PCL%XLB = PCL%XL
                 PCL%YLB = PCL%YL
                 PCL%ZLB = PCL%ZL
                 PCL%FACEB = 0
                 PCL%FACE = 0
                 PCL%TIME = RT(NT)
                 PCL%TIMEB = PCL%TIME
                 PCL%STATUS = 0
                 PCL%CELLCOUNT = 1
                 PCL%LINENUM = 0
                END IF
                END DO
                END DO
                END DO
              END IF
            END IF
            END DO
            END DO
            END DO
          END DO
        END DO
      
      END
      
      SUBROUTINE GENLOCALXYZF(IFACE,NROW,NCOL,IROW,JCOL,XL,YL,ZL)
C***********************************************************************
C  GENERATE LOCAL COORDINATES FOR A SPECIFIED LOCATION WITHIN A
C  2D SUBDIVISION OF A CELL FACE HAVING NROW AND NCOL SUBDIVISIONS.
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      REAL ::DELROW,DELCOL,XL,YL,ZL
      INTEGER ::IFACE,NROW,NCOL,IROW,JCOL
C-----------------------------------------------------------------------     
      DELROW=1.0/FLOAT(NROW)
      DELCOL=1.0/FLOAT(NCOL)
      SELECT CASE (IFACE)
        CASE (1)
          XL=0.0
          ZL=DELROW*FLOAT(IROW) - (DELROW/2.0)
          YL=DELCOL*FLOAT(JCOL) - (DELCOL/2.0)
        CASE (2)
          XL=1.0
          ZL=DELROW*FLOAT(IROW) - (DELROW/2.0)
          YL=DELCOL*FLOAT(JCOL) - (DELCOL/2.0)
        CASE (3)
          YL=0.0
          ZL=DELROW*FLOAT(IROW) - (DELROW/2.0)
          XL=DELCOL*FLOAT(JCOL) - (DELCOL/2.0)
        CASE (4)
          YL=1.0
          ZL=DELROW*FLOAT(IROW) - (DELROW/2.0)
          XL=DELCOL*FLOAT(JCOL) - (DELCOL/2.0)
        CASE (5)
          ZL=0.0
          YL=DELROW*FLOAT(IROW) - (DELROW/2.0)
          XL=DELCOL*FLOAT(JCOL) - (DELCOL/2.0)
        CASE (6)
          ZL=1.0
          YL=DELROW*FLOAT(IROW) - (DELROW/2.0)
          XL=DELCOL*FLOAT(JCOL) - (DELCOL/2.0)
      END SELECT
      
      RETURN
      END
 
      SUBROUTINE GENLOCALXYZI(NROW,NCOL,NLAY,IROW,JCOL,KLAY,XL,YL,ZL)
C***********************************************************************
C  GENERATE LOCAL COORDINATES FOR A SPECIFIED LOCATION WITHIN A
C  3D SUBDIVISION OF A CELL HAVING NROW, NCOL, AND NLAY SUBDIVISIONS.
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      REAL ::DELROW,DELCOL,DELLAY,XL,YL,ZL
      INTEGER ::NROW,NCOL,NLAY,IROW,JCOL,KLAY
C-----------------------------------------------------------------------
      DELROW=1.0/FLOAT(NROW)
      DELCOL=1.0/FLOAT(NCOL)
      DELLAY=1.0/FLOAT(NLAY)
      XL=DELCOL*FLOAT(JCOL) - (DELCOL/2.0)
      YL=DELROW*FLOAT(IROW) - (DELROW/2.0)
      ZL=DELLAY*FLOAT(KLAY) - (DELLAY/2.0)
      RETURN
      END 
      
      SUBROUTINE READSTARTINGLOC2(IU,IOUT)
C***********************************************************************
C  READ STARTING LOCATIONS FILE USING OPTION TO GENERATE PARTICLE ID
C  AUTOMATICALLY
C***********************************************************************
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE PARTICLEDATA, ONLY:NPGROUPS,PGROUPS,PARTICLE,PARTICLEGROUP
      INCLUDE 'MP6Openspec.inc'
      INTEGER N,NN,NP,IGRID,NRP,IOFF,IU,IOUT,ID,IFACE,IP,NPL,IRELOPT
      CHARACTER (LEN=40) ::LABEL
      REAL RPLEN,TINC,RTSTART
      REAL, ALLOCATABLE ::RT(:)
      TYPE(PARTICLE), POINTER ::PCL,PCLI
C-----------------------------------------------------------------------      
      READ(IU,*) NPGROUPS
      ALLOCATE(PGROUPS(NPGROUPS))
      
      ID = 0
      DO N = 1,NPGROUPS
        IF(ALLOCATED(RT)) DEALLOCATE(RT)
      
        PGROUPS(N)%GROUP = N
        READ(IU,*) PGROUPS(N)%NAME
        READ(IU,*) NPL,RTSTART,IRELOPT
        NRP = 0
        RPLEN = 0.0
        
        IF(IRELOPT.EQ.1) THEN
          ALLOCATE(RT(0:NRP))
          RT(0) = RTSTART
        ELSE IF(IRELOPT.EQ.2) THEN
          READ(IU,*) NRP,RPLEN
          ALLOCATE(RT(0:NRP))
          RT(0) = RTSTART
          IF(NRP.GT.0) THEN
            TINC = RPLEN/REAL(NRP)
            DO NN=1,NRP
              RT(NN) = RT(NN-1) + TINC
            END DO
          END IF
        ELSE IF(IRELOPT.EQ.3) THEN
          READ(IU,*) NRP
          ALLOCATE(RT(0:NRP))
          RT(0) = RTSTART
          READ(IU,*) (RT(NN), NN=1,NRP)
        END IF
        NP= NPL*(NRP + 1)
        PGROUPS(N)%COUNT = NP
        ALLOCATE(PGROUPS(N)%PARTICLES(0:NP))
        DO IP = 1,NPL
          READ(IU,*) IGRID,K,I,J,XL,YL,ZL,LABEL
          ID = ID + 1
          PCL => PGROUPS(N)%PARTICLES(IP)
          PCL%LABEL= LABEL
          PCL%GRID= IGRID
          PCL%GROUP= N
          PCL%INDEX= IP
          PCL%ID = ID
          PCL%I=I
          PCL%J=J
          PCL%K=K
          PCL%XL=XL
          PCL%YL=YL
          PCL%ZL=ZL
          CALL  FINDFACE(XL,YL,ZL,IFACE)
          PCL%FACE = IFACE
          PCL%FACEB = IFACE
          PCL%GRIDB = PCL%GRID
          PCL%IB = PCL%I
          PCL%JB = PCL%J
          PCL%KB = PCL%K
          PCL%XLB = PCL%XL
          PCL%YLB = PCL%YL
          PCL%ZLB = PCL%ZL
          PCL%TIMEB = RT(0)
          PCL%TIME = PCL%TIMEB
          PCL%STATUS = 0
          PCL%CELLCOUNT = 1
          PCL%LINENUM = 0              
        END DO
        
        IF(NRP.GT.0) THEN
          DO NN = 1,NRP
            IOFF = NN*NPL
            DO IP = 1,NPL
              ID = ID + 1
              PCLI => PGROUPS(N)%PARTICLES(IP)
              PCL => PGROUPS(N)%PARTICLES(IP + IOFF)
              PCL%LABEL = PCLI%LABEL
              PCL%ID = ID
              PCL%INDEX = IP + IOFF
              PCL%TIMEB = RT(NN)
              PCL%TIME = PCL%TIMEB
              PCL%GRID= PCLI%GRID
              PCL%GROUP= PCLI%GROUP
              PCL%I= PCLI%I
              PCL%J= PCLI%J
              PCL%K= PCLI%K
              PCL%XL= PCLI%XL
              PCL%YL= PCLI%YL
              PCL%ZL= PCLI%ZL
              PCL%FACE = PCLI%FACE
              PCL%FACEB = PCLI%FACEB
              PCL%GRIDB = PCLI%GRIDB
              PCL%IB = PCLI%IB
              PCL%JB = PCLI%JB
              PCL%KB = PCLI%KB
              PCL%XLB = PCLI%XLB
              PCL%YLB = PCLI%YLB
              PCL%ZLB = PCLI%ZLB
              PCL%STATUS = PCLI%STATUS
              PCL%CELLCOUNT = PCLI%CELLCOUNT
              PCL%LINENUM = PCLI%LINENUM          
            END DO
          END DO
        END IF
      END DO
      RETURN
      END 
      
      SUBROUTINE READSTARTINGLOC1(IU,IOUT)
C***********************************************************************
C  READ STARTING LOCATIONS FILE USING OPTION TO READ PARTICLE ID VALUES
C  FROM THE FILE.
C***********************************************************************
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE PARTICLEDATA, ONLY:NPGROUPS,PGROUPS,PARTICLE,PARTICLEGROUP
      INCLUDE 'MP6Openspec.inc'
      INTEGER N,I,NPART,NP,IGRID,IPNUM,IGROUP,IU,ID,IFACE,IDMAX,IOUT
      INTEGER, DIMENSION(:), ALLOCATABLE ::IDX
      REAL RT,RINC
      CHARACTER (LEN=200) ::FNAME
      CHARACTER (LEN=40) ::LABEL
      TYPE(PARTICLE), POINTER ::PCL
C-----------------------------------------------------------------------
      IDMAX = 0
      
      READ(IU,*) NPGROUPS
      ALLOCATE(PGROUPS(NPGROUPS))
      
      NPART = 0
      DO N = 1,NPGROUPS
        PGROUPS(N)%GROUP = N
        READ(IU,*) PGROUPS(N)%NAME
        READ(IU,*) NP
        PGROUPS(N)%COUNT = NP
        ALLOCATE(PGROUPS(N)%PARTICLES(0:NP))
        NPART = NPART + NP
      END DO
      
      IF (NPART.GT.0) THEN
        ALLOCATE(IDX(NPGROUPS))
        IDX = 0
        DO N = 1,NPART
          READ(IU,*) ID,IGROUP,IGRID,K,I,J,XL,YL,ZL,RT,LABEL
          IF(ID.LE.IDMAX) THEN
            CALL USTOP
     1    ('PARTICLE ID VALUES NOT IN ASCENDING ORDER. STOP.')
          END IF
          IDMAX = ID
          IDX(IGROUP) = IDX(IGROUP) + 1
          PCL => PGROUPS(IGROUP)%PARTICLES(IDX(IGROUP))
          PCL%LABEL= LABEL
          PCL%GRID= IGRID
          PCL%GROUP= IGROUP
          PCL%INDEX= IDX(IGROUP)
          PCL%ID = ID
          PCL%I=I
          PCL%J=J
          PCL%K=K
          PCL%XL=XL
          PCL%YL=YL
          PCL%ZL=ZL
          CALL  FINDFACE(XL,YL,ZL,IFACE)
          PCL%FACE = IFACE
          PCL%FACEB = IFACE
          PCL%GRIDB = PCL%GRID
          PCL%IB = PCL%I
          PCL%JB = PCL%J
          PCL%KB = PCL%K
          PCL%XLB = PCL%XL
          PCL%YLB = PCL%YL
          PCL%ZLB = PCL%ZL
          PCL%TIME = RT
          PCL%TIMEB = PCL%TIME
          PCL%STATUS = 0
          PCL%CELLCOUNT = 1
          PCL%LINENUM = 0              
        END DO
        DEALLOCATE(IDX)
      END IF
      RETURN
      END
      
      SUBROUTINE READSTARTINGLOC3(IU,IOUT)
C***********************************************************************
C  READ STARTING LOCATIONS FROM A FILE USING THE FULLY AUTOMATIC
C  OPTION.
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE PARTICLEDATA, ONLY:NPGROUPS,PGROUPS
      INTEGER IDMAX,NG,IU,IOUT
C-----------------------------------------------------------------------
C         READ NUMBER OF PARTICLE GROUPS
          READ(IU,*) NPGROUPS
          
          IDMAX = 0
          IF(NPGROUPS.GT.0) THEN
            ALLOCATE(PGROUPS(NPGROUPS))
            DO NG=1,NPGROUPS
              PGROUPS(NG)%GROUP = NG
              CALL READPARTICLEGROUP(IU,NG,IDMAX)
            END DO
          END IF

      RETURN
      END
      
      SUBROUTINE READSTARTINGLOC(IU)
C***********************************************************************
C  READ STARTING LOCATIONS FILE OPTION FROM THE STARTING LOCATIONS FILE
C  AND CALL THE APPROPRIATE SUBROUTINE TO READ THE LOCATIONS RECORDS.
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      USE PARTICLEDATA, ONLY:NPGROUPS,PGROUPS,PARTICLE,PARTICLEGROUP
      USE MPDATA, ONLY:INUNIT,IOLIST
      INCLUDE 'MP6Openspec.inc'
      INTEGER N,I,NN,NPART,NP,IGRID,IXYZOPT,IPNUM,IPNLAST,IGROUP,NRP
      INTEGER IU,ISLFOPT,ID,IDPREV,IFACE,IG,IP,ICOL,ISTART,ISTOP
      INTEGER, DIMENSION(:), ALLOCATABLE ::IDX
      REAL RT,RINC,R
      CHARACTER (LEN=200) ::FNAME,LINE,TEXT1,TEXT2
      TYPE(PARTICLE), POINTER ::PCL
C-----------------------------------------------------------------------     
C  Read the starting locations file name and open the file for reading      
      READ(IU,*) FNAME
      ICOL=1
      CALL URWORD(FNAME,ICOL,ISTART,ISTOP,0,N,R,0,0)
      FNAME=FNAME(ISTART:ISTOP)
      OPEN(UNIT=INUNIT,FILE=FNAME,STATUS='OLD',ACTION=ACTION(1))
C  Read comments and the first line after the comments      
      CALL URDCOM(INUNIT,0,LINE)
C  Read particle generation option
C    1 = Explicit particle generation
C    2 = Semi-automatic particle generation
C    3 = Fully automatic particle generation
      READ(LINE,*) ISLFOPT
      
      TEXT1 = 'Particle starting locations will be read from file: '
      TEXT2 = 'Starting locations file format option (ISLFOPT) = '
      
      IF(ISLFOPT.GE.1 .AND. ISLFOPT.LE.3) THEN
        WRITE(IOLIST,'(1X,A,A)') TEXT1,FNAME
        WRITE(IOLIST,'(3X,A,I2)') TEXT2,ISLFOPT
      ELSE
        CLOSE(UNIT=INUNIT)
        CALL USTOP('INVALID STARTING LOCATIONS FILE FORMAT OPTION.')
      END IF
      
      SELECT CASE (ISLFOPT)
        CASE (1)
          CALL READSTARTINGLOC1(INUNIT,IOLIST)
        CASE (2)
          CALL READSTARTINGLOC2(INUNIT,IOLIST)
        CASE (3)
          CALL  READSTARTINGLOC3(INUNIT,IOLIST)
      END SELECT
     
      CLOSE(UNIT=INUNIT)
      
      
      RETURN
      END

      SUBROUTINE FINDFACE(XL,YL,ZL,IFACE)
C***********************************************************************
C  FIND THE CELL FACE FOR A PARTICLE LOCATION BASED ON LOCAL COORDINATES
C
C    SPECIFICATIONS:
C-----------------------------------------------------------------------
      INTEGER IFACE
      REAL XL,YL,ZL
C-----------------------------------------------------------------------
      IFACE = 0
      IF(XL.EQ.0.0) THEN
        IFACE = 1
      ELSE IF(XL.EQ.1.0) THEN
        IFACE = 2
      ELSE IF(YL.EQ.0.0) THEN
        IFACE = 3
      ELSE IF(YL.EQ.1.0) THEN
        IFACE = 4
      ELSE IF(ZL.LE.0.0 .AND. ZL.GE.-1.0) THEN
        IFACE = 5
      ELSE IF(ZL.EQ.1.0) THEN
        IFACE = 6
      END IF
      
      RETURN
      END
