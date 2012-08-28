      
      SUBROUTINE TRACK(T,TMAX,ITP,IACTIVE,IPENDING,ISS,KTIME,KPER,KSTP)
      USE GLOBAL, ONLY:IBOUND,NCOL,NROW,NLAY
      USE MPDATA, ONLY:ITRACKDIR,NGRIDS,REFTIME,ISIMTYPE,IOPART,IBDOPT,
     1                 IDTRACE,IOTRACE,IADVOBS,NTPTS
      USE PARTICLEDATA, ONLY:NPGROUPS,PGROUPS,PARTICLE
      REAL ::T,TMAX
      INTEGER ::IGROUP,IPART,IFACE,EXITTIME,ITP,IACTIVE,IPENDING,ISS
      INTEGER ::IFIRST,KTIME,KPER,KSTP,ITERMZONE,INIT,ITIME,K
      TYPE(PARTICLE), POINTER ::P
      
      IACTIVE = 0
      IPENDING = 0
      IF(NPGROUPS.EQ.0) RETURN
      
      DO IGROUP=1,NPGROUPS
        DO IPART = 1,PGROUPS(IGROUP)%COUNT
          INIT = 0
          P => PGROUPS(IGROUP)%PARTICLES(IPART)
          
C         Skip over particles unless they are active or pending release         
          IF(P%STATUS .GT. 1) THEN
            IF(ISIMTYPE.EQ.3 .AND. IADVOBS.GE.2) THEN
              IF(P%STATUS.EQ.2 .AND. ITP.GT.0) THEN
                IF(IADVOBS.EQ.2) THEN
                  CALL WRITERECADVOBS(P,ITP)
                ELSE
                  IF(ITP.EQ.NTPTS) THEN
                    CALL WRITERECADVOBS(P,ITP)
                  END IF
                END IF
              END IF
            END IF
            CYCLE
          END IF
          
C         If a particle is pending release (STATUS = 0), check to see if it should
C         be set to active and released on this pass. If the particle is pending
C         release and its release time is earlier than the starting time of this
C         pass, then mark the particle status as permanently unreleased 
C         (STATUS = 4).
          IF(P%STATUS .EQ. 0) THEN
            IF(P%TIMEB .LT. T) THEN
              P%STATUS = 4
            ELSE IF(P%TIMEB .LE. TMAX) THEN
C             Check to see if the particle layer is 0, which indicates it should be draped on
C             the top-most variable head cell (IBOUND > 0)
              IF(P%K .GT. 0) THEN
C               The particle is not draped.
                P%STATUS = 1
              ELSE
C               The particle is draped. Find the top-most active layer and assign the particle to that layer
C               If there is not layer with IBOUND > 0, set the status flag to permanently unreleased (4)
                P%STATUS = 4
                DO K = 1,NLAY
                  IF(IBOUND(P%J,P%I,K) .GT. 0) THEN
                    P%STATUS = 1
                    P%K = K
                    P%KB = K
                    EXIT
                  END IF
                END DO
              END IF
            END IF
          END IF
     
C         Count the number of particles that are currently active or pending
C         release at the beginning of this pass.         
          IF(P%STATUS .EQ. 0) IPENDING = IPENDING + 1
          IF(P%STATUS .EQ. 1) IACTIVE = IACTIVE + 1
          
C         Track the particle if it is active
          IF (P%STATUS .EQ. 1) THEN
C           If the simulation type is PATHLINE, write the starting location of the
C           pathline for this particle on this time pass if it is active.
            IF(ISIMTYPE.EQ.2) THEN
C             Increment the particle LINENUM index.
              P%LINENUM = P%LINENUM + 1
C             Set ITIME = 0 if this is the initial location (T = 0.0). Otherwise, set ITIME = -1 .
              ITIME = -1
              IF(T .EQ. 0.0) ITIME = 0
C             Write pathline record
              CALL WRITERECPL(P,IOPART,ITIME,KTIME,1)
            END IF
C
            CALL TRACKPARTICLE(P,TMAX,ITP,ISS,KTIME,KPER,KSTP)
C
          END IF
          
        END DO
      END DO
      
      RETURN
      END
      
      SUBROUTINE WRITEINITIALTS(KTIME)
      USE MPDATA, ONLY:ITRACKDIR,NGRIDS,REFTIME,ISIMTYPE,IOPART,IBDOPT,
     1                 IDTRACE,IOTRACE,IADVOBS
      USE PARTICLEDATA, ONLY:NPGROUPS,PGROUPS,PARTICLE
      INTEGER ::IGROUP,IPART,IFACE,EXITTIME,ITP,IACTIVE,IPENDING,ISS
      INTEGER ::IFIRST,KTIME,KPER,KSTP,ITERMZONE
      TYPE(PARTICLE), POINTER ::P
      
      IF(NPGROUPS.EQ.0) RETURN
      
      DO IGROUP=1,NPGROUPS
        DO IPART = 1,PGROUPS(IGROUP)%COUNT
          P => PGROUPS(IGROUP)%PARTICLES(IPART)
          
C         Skip over particles unless they are active or pending release         
          IF(P%STATUS .GT. 1) CYCLE
          
          IF(P%STATUS.EQ.0 .AND. P%TIMEB.EQ.0.0 ) THEN
            P%STATUS = 1
          END IF
          
C         If the simulation type is TIMESERIES, write the initial location of the
C         particle if it is active at time 0.
          IF (P%STATUS .EQ. 1) THEN
            CALL WRITERECTS(P,IOPART,0,KTIME,1)
          END IF
          
        END DO
      END DO
      
      RETURN
      END
      
      SUBROUTINE TRACKPARTICLE(P,TMAX,ITP,ISS,KTIME,KPER,KSTP)
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,IUNIT,ISSFLG,IBOUND,BUFF,
     1                 IBUFF,QX,QY,QZ,QSINK,QSOURCE,QSTO,HEAD,
     2                 IFACEASP,LAYTYP,BOTM,LBOTM,LAYCBD,DELR,DELC,
     3                 IGRIDNUM
      USE MPBAS, ONLY:HDRY
      USE MPDATA, ONLY:CELLDATA,ISINK,ISOURCE,IOPART,ISIMTYPE,IOTRACE,
     1                 IBDOPT,IDTRACE,ITRACESEG,ITRACKDIR,IADVOBS
      USE PARTICLEDATA, ONLY:PARTICLE
      TYPE(PARTICLE) ::P
      TYPE(CELLDATA) ::C
      INTEGER ::NSEGS,JP,IP,KP,ISS,IEXIT,II,JJ,KK,ITIME,KTIME,KPER,KSTP
      REAL ::HED,XP,YP,ZP,XL,YL,ZL,TOP,BOT,BOTCB,TSTART,XL0,YL0,ZL0
      
      IEXIT = 0
      II = 0
      JJ = 0
      KK = 0
     
      DO WHILE(P%STATUS.EQ.1 .AND. P%TIME.LT.TMAX)
      
C       Select a new grid if P%GRID does not already point to the current selected grid
        CALL SELECTGRID(P%GRID)
        
C       Track the particle through the current cell until it either exits the
C       cell or the maximum time (TMAX) is reached. First check the cell type
C       to see if this is a regular cell or a boundary cell in a parent grid
C       that connects to a child grid and requires subdivision into ghost cells.
C       ICELLTYPE = 1 is a regular cell
C       ICELLTYPE = 2 is a boundary cell requiring subdivision into ghost cells.
        CALL CHECKCELLTYPE(P,ICELLTYPE)
        
        IF(P%J.NE.JJ .OR. P%I.NE.II .OR. P%K.NE.KK) THEN
          JJ = P%J
          II = P%I
          KK = P%K
          CALL GETCELLDATA(II,JJ,KK,C)
        END IF

C       Save the current time and location before the call to TRACKCELL
C       These values are output for TRACE mode.        
        TSTART = P%TIME
        XL0 = P%XL
        YL0 = P%YL
        ZL0 = P%ZL
        
        SELECT CASE(ICELLTYPE)
          CASE (1)
            CALL TRACKCELL(P,TMAX,ITRACKDIR,ISS,C,ISINK,ISOURCE,
     1                     IEXIT)
          CASE DEFAULT
            CALL USTOP('UNSUPPORTED CELL TYPE. STOP.')
        END SELECT
        
C     If TRACE mode was selected (IBDOPT = 4), write the trace information for the specified
C     particle for this tracking segment.
        IF(IBDOPT.EQ.4 .AND. P%ID.EQ.IDTRACE) THEN
          ITRACESEG = ITRACESEG + 1
          CALL WRITETRACE(P,C,KTIME,KPER,KSTP,TMAX,TSTART,XL0,YL0,ZL0,
     1                    IEXIT)
        END IF
        
C     Check exit flag (IEXIT) that is returned by subroutine TRACKCELL.
C     If IEXIT equals 0, the particle remains within the current cell.
C     If IEXIT equals 1, the particle has reached a cell face and should be moved to an adjacent
C     cell. Subroutine SWITCHCELL is called to carry out that process. If the adjacent cell is outside 
C     the active grid, the particle is kept in the current cell and its STATUS flag is set to indicate 
C     it is terminated. 
      IF(IEXIT.EQ.1 .AND. P%STATUS.EQ.1) THEN
        CALL SWITCHCELL(P)
      END IF

C     Record the exit velocity used to compute the particle observations for particles that just 
C     terminated on this pass
      IF(ISIMTYPE.EQ.3 .AND. IADVOBS.EQ.2 .AND. P%STATUS.EQ.2) THEN
        IF(P%ZL.GE.0.0) THEN
          P%EXITVEL(1) = ((1.0 - P%XL)*C%VX1 + P%XL*C%VX2)/C%RFAC
          P%EXITVEL(2) = ((1.0 - P%YL)*C%VY1 + P%YL*C%VY2)/C%RFAC
          P%EXITVEL(3) = ((1.0 - P%ZL)*C%VZ1 + P%ZL*C%VZ2)/C%RFAC
        ELSE
          P%EXITVEL(1) = 0.0
          P%EXITVEL(2) = 0.0
          P%EXITVEL(3) = C%VZCB/C%RFACCB
        END IF
        IF(ITRACKDIR.EQ.2) THEN
          P%EXITVEL(1) = -1.0*P%EXITVEL(1)
          P%EXITVEL(2) = -1.0*P%EXITVEL(2)
          P%EXITVEL(3) = -1.0*P%EXITVEL(3)
        END IF
      END IF
      
      IF(IBDOPT.EQ.4 .AND. P%ID.EQ.IDTRACE .AND. P%STATUS.GT.1) THEN
        WRITE(IOTRACE,'(A,A,I10)') 
     1 'The particle status was changed from active to inactive: ',
     2 'STATUS =',P%STATUS
      END IF
      
C       Set the ITIME flag:
C       If ITIME > -1, the point is a specified time point and the value of ITIME is the index of 
C       the time point in the time point array.  If ITIME = -1, the point is not a specified time point. 
C       The value of ITP will either have a value of -1 or a value > 0 that indicates the specified 
C       time point index. Initial locations, which have an ITIME = 0, have already been written to the
C       pathline or timeseries file.
        ITIME = -1
        IF(ITP.GT.-1 .AND. P%TIME.EQ.TMAX) ITIME = ITP
        
        SELECT CASE(ISIMTYPE)
          CASE (2)
C           WRITE PATHLINE FILE
            CALL WRITERECPL(P,IOPART,ITIME,KTIME,1)
          CASE (3)
            IF(ITIME.GT.-1) THEN
C             WRITE TIMESERIES FILE
              CALL WRITERECTS(P,IOPART,ITIME,KTIME,1)
              IF(IADVOBS.EQ.2 .AND. ITIME.GT.0) THEN
                 CALL WRITERECADVOBS(P,ITIME)
              END IF
            END IF
        END SELECT
      
      END DO
      
      RETURN
      END
      
      
      
      SUBROUTINE TRACKCELL(P,TMAX,IDIR,ISS,C,ISINK,ISOURCE,IEXIT)
      USE PARTICLEDATA, ONLY:PARTICLE
      USE MPDATA, ONLY:CELLDATA,ISTOPZONE,ISTOPZONE2
      TYPE(PARTICLE) ::P
      TYPE(CELLDATA) ::C
      INTEGER ::ICB,IWEAK,ISS,IEXIT,ISINK,ISOURCE,IDIR
      REAL ::XP,YP,ZP,XL,YL,ZL,TOP,BOT,BOTCB,XXL,YYL,ZZL,
     1       VX1,VX2,VY1,VY2,VZ1,VZ2,VZCB,DX,DY,DZ,DT,T,ZZ,TMAX
      
C     Assign scalar variables for convenience
      IEXIT = 0      
      XL = P%XL
      YL = P%YL
      ZL = P%ZL
      DX = C%DX
      DY = C%DY
      TOP = C%TOP
      BOT = C%BOT
      BOTCB = C%BOTCB
      DZ = TOP - BOT
      VX1 = C%VX1
      VX2 = C%VX2
      VY1 = C%VY1
      VY2 = C%VY2
      VZ1 = C%VZ1
      VZ2 = C%VZ2
      VZCB = C%VZCB
      
C     Terminate particle and return if it is in an inactive cell
      IF(C%ACTIVE.EQ.0) THEN
        P%STATUS = 5
        RETURN
      END IF

C     Scale the velocities by the retardation factor if it is not equal to 1
      IF(C%RFAC.NE.1.0 .AND. C%RFAC.NE.0.0) THEN
        VX1 = VX1/C%RFAC
        VX2 = VX2/C%RFAC
        VY1 = VY1/C%RFAC
        VY2 = VY2/C%RFAC
        VZ1 = VZ1/C%RFAC
        VZ2 = VZ2/C%RFAC        
      END IF
     
      IF(C%RFACCB.NE.1.0 .AND. C%RFACCB.NE.0.0) THEN
        VZCB = VZCB/C%RFACCB
      END IF
            
C     Reverse the sign of the velocity components if the tracking direction
C     is backward.
      IF(IDIR.EQ.2) THEN
        VX1 = -VX1
        VX2 = -VX2
        VY1 = -VY1
        VY2 = -VY2
        VZ1 = -VZ1
        VZ2 = -VZ2
        VZCB = -VZCB
      END IF

C     If BOT = BOTCB then either there is no quasi-3d confining bed for this cell or its thickness
C     is 0 at this location. In this case, if ZL is negative, set ZL = 0 to assure that the computations
C     for the confining bed are skipped.
      IF(BOT.EQ.BOTCB .AND. ZL.LT.0.0) ZL = 0.0
      
C     If a quasi-3d confining bed is present, check to see if the current particle
C     position is located within that confining bed. If so, move it vertically 
C     through the confining bed. If there is a confining bed and ZL < 0, or if
C     the particle is moving downward and ZL = 0, then the particle is located
C     in the confining bed and the confining bed computations are performed. 
C     Otherwise, the particle is located in the main body of the cell and the confining 
C     bed computations are skipped.
      IF(BOT.GT.BOTCB) THEN
        IF((ZL.EQ.0.0 .AND. VZCB.LT.0.0) .OR. (ZL.LT.0.0)) THEN         
          IF(VZCB.EQ.0.0) THEN
C           Particle is in confining bed but has zero velocity. Keep the particle
C           coordinates constant and set the time equal to TMAX. Then return.
            P%TIME = TMAX
            P%FACE = 5
            RETURN
          ELSE
            ZP = -ZL*BOTCB + (1.0 + ZL)*BOT
            IF(VZCB.LT.0.0) THEN
C             Particle is in confining bed and is moving downward. 
C             Calculate time required for particle to reach the bottom of
C             the CB.
              DT = (BOTCB - ZP)/VZCB
              IF((P%TIME + DT).LE.TMAX) THEN
C               The time at which the particle would reach the bottom of
C               the CB is less than or equal to TMAX, so calculate the new position
C               and time corresponding to the particle at the bottom of the
C               CB. Set the exit face flag equal to 5. Then return.
                P%TIME = P%TIME + DT
                P%ZL = -1.0
                P%FACE = 5
                IEXIT = 1
              ELSE
C               The particle moves down through the CB but is still located in
C               the CB at TMAX. Calculate the new location at TMAX. Then return.
                ZZ = ZP + VZCB*(TMAX-P%TIME)
                P%TIME = TMAX
                P%FACE = 5
                P%ZL = ((ZZ-BOTCB)/(BOT-BOTCB)) - 1.0
                IF(P%ZL.LT.-1.0) P%ZL = -1.0
              ENDIF
              RETURN
            ELSE
C             Particle is in confining bed and is moving upward. 
C             Calculate time required for particle to reach the top of
C             the CB.
              DT = (BOT - ZP)/VZCB
              IF((P%TIME + DT).LE.TMAX) THEN
C               The time at which the particle would reach the top of
C               the CB is less than or equal to TMAX, so calculate the new position
C               and time corresponding to the particle at the top of the
C               CB. The particle is now at the bottom of the current cell.
                P%TIME = P%TIME + DT
                P%ZL = 0.0
                P%FACE = 5
              ELSE
C               The particle moves up through the CB but is still located in
C               the CB at TMAX. Calculate the new location at TMAX. Then return.
                ZZ = ZP + VZCB*(TMAX-P%TIME)
                P%TIME = TMAX
                P%ZL = ((ZZ-BOTCB)/(BOT-BOTCB)) - 1.0
                IF(P%ZL.GT.0.0) P%ZL = 0.0
                P%FACE = 5
              END IF
              RETURN
            END IF
          END IF
        END IF
      END IF
      
C     If this point is reached, it means the particle is located within the main body
C     of the current cell.
C     First, check to see if the particle meets one of the termination conditions.
C     If so, set the STATUS flag and return.
     
C     1. Check for automatic termination zone. 
C     Terminate particle and return if it is in an automatic discharge zone
      IF(ISTOPZONE.GT.0) THEN
        IF(C%ZONE.GE.ISTOPZONE .AND. C%ZONE.LE.ISTOPZONE2) THEN
          P%STATUS = 3
          RETURN
        END IF
      END IF
C     2. Check for weak sink termination. Flag as a potential weak sink if
C        the cell contains an internal sink. Do not flag the cell if the
C        tracking direction is backward and the particle is still in its
C        initial cell.
      IWEAK = 0
      IF(ISINK.EQ.2) THEN
        IF(IDIR.EQ.1) THEN
          IF(C%QSINK .LT. 0.0) IWEAK = IWEAK + 1
        ELSE
          IF(P%CELLCOUNT.GT.1) THEN
            IF(C%QSINK .LT. 0.0) IWEAK = IWEAK + 1
          END IF
        END IF
      END IF
      
C     3. If tracking direction is backward, check for weak source termination.
C        Flag as a potential weak source if the cell contains an internal source.
      IF(ISOURCE.EQ.2) THEN
        IF(IDIR.EQ.2) THEN
          IF(C%QSOURCE .GT. 0.0) IWEAK = IWEAK + 1
        ELSE
          IF(P%CELLCOUNT.GT.1) THEN
            IF(C%QSOURCE .GT. 0.0) IWEAK = IWEAK + 1
          END IF
        END IF
      END IF

C     Terminate the particle if a weak sink/source condition was detected and
C     the appropriate weak sink/source termination option is in effect.   
C     This check will also catch any strong sink or strong source cells.
C     If the weak sink/source checks are turned off, there is another check later
C     that will also catch any strong sinks/sources.
      IF(IWEAK.GT.0) THEN
        P%STATUS = 2
        RETURN
      END IF

      
C     If the particle was not terminated, move it through the current grid cell 
C     until an exit point is reached or the tracking time reaches TMAX.

C
C  COMPUTE CELL TRANSIT TIMES IN X, Y, AND Z DIRECTIONS
C
      CALL DTCALC (VX1,VX2,VX,DVXDX,DX,XL,DTX,IVXFLG)
      CALL DTCALC (VY1,VY2,VY,DVYDY,DY,YL,DTY,IVYFLG)
      CALL DTCALC (VZ1,VZ2,VZ,DVZDZ,DZ,ZL,DTZ,IVZFLG)

C
C  DETERMINE EXIT FACE IF ONE EXISTS. IF AN EXIT FACE IS FOUND, SET DT EQUAL
C  TO THE TRAVEL TIME REQUIRED TO REACH THE FACE.
C
      IX = 0
      IY = 0
      IZ = 0
      IF(IVXFLG.LE.1 .OR. IVYFLG.LE.1 .OR. IVZFLG.LE.1) THEN
        DT=DTX
        IX = 1
        IY = 0
        IZ = 0
        IF(DTY.LT.DT) THEN
          DT=DTY
          IY = 1
          IX = 0
          IZ = 0
        END IF
        IF(DTZ.LT.DT) THEN
          DT=DTZ
          IZ = 1
          IX = 0
          IY = 0
        END IF
        IF(IX.EQ.1) THEN
          IF(VX.LT.0.0) IX = -IX
        ELSE IF(IY.EQ.1) THEN
          IF(VY.LT.0.0) IY = -IY
        ELSE IF(IZ.EQ.1) THEN
          IF(VZ.LT.0.0) IZ = -IZ
        END IF
      END IF
            
C     If the particle has no potential exit face. Check to see 
C     if it should be terminated.
      IF(IX.EQ.0 .AND. IY.EQ.0 .AND. IZ.EQ.0) THEN
C       Only terminate the particle if this is a steady-state time step.
        IF(ISS.EQ.1) THEN
          P%STATUS = 2
        ELSE
          IF(IDIR.EQ.1 .AND. C%QSINK.LT.0.0) THEN
C           For transient time steps in a forward tracking run, terminate
C           the particle if the cell contains internal sink discharge.
            P%STATUS = 2
          ELSE IF(IDIR.EQ.2 .AND. C%QSOURCE.GT.0.0) THEN
C           For transient time steps in a backward tracking run, terminate
C           the particle if the cell contains internal source recharge.
            P%STATUS = 2
          ELSE
C           For all other conditions with transient time steps, compute the new 
C           location within the cell at time TMAX.
            DT = TMAX - P%TIME
            CALL NEWXYZ(VX,DVXDX,VX1,VX2,DT,XL,DX,XXL,IVXFLG)
            CALL NEWXYZ(VY,DVYDY,VY1,VY2,DT,YL,DY,YYL,IVYFLG)
            CALL NEWXYZ(VZ,DVZDZ,VZ1,VZ2,DT,ZL,DZ,ZZL,IVZFLG)
            P%TIME = TMAX
            P%XL = XXL
            P%YL = YYL
            P%ZL = ZZL
            P%FACE = 0
          END IF
        END IF
        RETURN
      END IF

C     If the particle has a potential exit face but does not reach the exit face 
C     before the time TMAX, compute the particle location within the current cell 
C     at time TMAX and then return.
      T = P%TIME + DT
      IF(T.GT.TMAX) THEN
        DT = TMAX - P%TIME   
        CALL NEWXYZ(VX,DVXDX,VX1,VX2,DT,XL,DX,XXL,IVXFLG)
        CALL NEWXYZ(VY,DVYDY,VY1,VY2,DT,YL,DY,YYL,IVYFLG)
        CALL NEWXYZ(VZ,DVZDZ,VZ1,VZ2,DT,ZL,DZ,ZZL,IVZFLG)
        P%TIME = TMAX
        P%XL = XXL
        P%YL = YYL
        P%ZL = ZZL
        P%FACE = 0
        RETURN
      END IF

C     If the particle does reach an exit face before time TMAX, compute its
C     new location at that time.
      IF(IX.NE.0) THEN
C       The particle exits one of the 2 X faces. 
        IEXIT = 1
        CALL NEWXYZ(VY,DVYDY,VY1,VY2,DT,YL,DY,YYL,IVYFLG)
        CALL NEWXYZ(VZ,DVZDZ,VZ1,VZ2,DT,ZL,DZ,ZZL,IVZFLG)
        P%TIME = P%TIME + DT
        P%YL = YYL
        P%ZL = ZZL
        IF(IX.LT.0) THEN
          P%FACE = 1
          P%XL = 0.0
        ELSE
          P%FACE = 2
          P%XL = 1.0
        END IF
       RETURN
      ELSE IF(IY.NE.0) THEN
C       The particle exits one of the 2 Y faces. 
        IEXIT = 1
        CALL NEWXYZ(VX,DVXDX,VX1,VX2,DT,XL,DX,XXL,IVXFLG)
        CALL NEWXYZ(VZ,DVZDZ,VZ1,VZ2,DT,ZL,DZ,ZZL,IVZFLG)
        P%TIME = P%TIME + DT
        P%XL = XXL
        P%ZL = ZZL
        IF(IY.LT.0) THEN
          P%FACE = 3
          P%YL = 0.0
        ELSE
          P%FACE = 4
          P%YL = 1.0
        END IF
        RETURN
      ELSE IF(IZ.NE.0) THEN
C       The particle exits either the bottom or top face.
C       If the particle exits the bottom face and the cell
C       has either no underlying confining bed or a confining
C       bed of zero thickness, set the exit flag IEXIT to 1.
C       Otherwise, if there is an underlying confining bed
C       of finite thickness, leave IEXIT = 0 to indicate the 
C       the particle remains in the current cell.
        CALL NEWXYZ(VX,DVXDX,VX1,VX2,DT,XL,DX,XXL,IVXFLG)
        CALL NEWXYZ(VY,DVYDY,VY1,VY2,DT,YL,DY,YYL,IVYFLG)
        P%TIME = P%TIME + DT
        P%XL = XXL
        P%YL = YYL
        IEXIT = 1
        IF(IZ.LT.0) THEN
          IF(C%LAYCBD.GT.0) THEN
            IF(C%BOT.GT.C%BOTCB) IEXIT = 0
          END IF
          P%ZL = 0.0
          P%FACE = 5
        ELSE
          P%ZL = 1.0
          P%FACE = 6
        END IF
        RETURN
      END IF

      
      END
      
      
      SUBROUTINE CHECKCELLTYPE(P,ICELLTYPE)
      USE PARTICLEDATA, ONLY: PARTICLE
      INTEGER ::ICELLTYPE
      TYPE(PARTICLE) ::P
      
      ICELLTYPE = 1
      
      RETURN
      END
      
      SUBROUTINE GETGLOBALCOORD(XL,YL,ZL,I,J,K,X,Y,Z)
      USE GLOBAL, ONLY: XMIN,YMIN,DELR,DELC,BOTM,LBOTM,LAYTYP,LAYCBD,
     1                  HEAD
      REAL XL,YL,ZL,X,Y,Z,X0,Y0,DX,DY,BOTCB,BOT,TOP
      INTEGER I,J,K
      
      X0 = XMIN(J)
      Y0 = YMIN(I)
      DX = DELR(J)
      DY = DELC(I)
      BOT = BOTM(J,I,LBOTM(K))
      BOTCB = BOT
      IF(K.EQ.1) THEN
        TOP = BOTM(J,I,0)
      ELSE
        IF(LAYCBD(K-1).EQ.0) THEN
          TOP = BOTM(J,I,LBOTM(K-1))
        ELSE
          TOP = BOTM(J,I,LBOTM(K-1) + 1)
        END IF
      END IF
      IF(LAYTYP(K).GT.0 .AND. HEAD(J,I,K).LT.TOP) THEN
        TOP = HEAD(J,I,K)
      END IF
      IF(LAYCBD(K).NE.0) BOTCB = BOTM(J,I,LBOTM(K) + 1)
      
      X = X0 + XL*DX
      Y = Y0 + YL*DY
      IF(ZL.GE.0.0) THEN
        Z = ZL*TOP + (1.0 - ZL)*BOT
      ELSE
        Z = (1.0 + ZL)*BOT - ZL*BOTCB
      END IF
      RETURN
      
      END
      
      SUBROUTINE NEWXYZ (V,DVDX,V1,V2,DT,X,DX,XNEW,IVFLG)
C
      REAL ::X,DX,V,DVDX,V1,V2,XNEW
      INTEGER ::IVFLG,ISIDE
      
      SELECT CASE(IVFLG)
        CASE(1)
          XNEW= X*DX + V1*DT
          XNEW = XNEW/DX
        CASE(2)
          XNEW = X
        CASE DEFAULT
          XNEW= X*DX + V*(EXP(DVDX*DT) - 1.0E+0)/DVDX
          XNEW = XNEW/DX
      END SELECT
      
      IF(XNEW.LT.0.0) XNEW = 0.0
      IF(XNEW.GT.1.0) XNEW = 1.0E+00
      IF(XNEW.EQ.0.0.AND.V1.EQ.0.0E+0) XNEW = 1.0E-3
      IF(XNEW.EQ.1.0.AND.V2.EQ.0.0E+0) XNEW = 0.999
      
      RETURN
      END
      
      SUBROUTINE GETCELLDATA(I,J,K,C)
      USE GLOBAL, ONLY:DELR,DELC,IBOUND,IZONE,LAYCBD,BOTM,LBOTM,HEAD,
     1            QX,QY,QZ,QSINK,QSOURCE,QSTO,POR,RFAC,XMIN,YMIN,LAYTYP
      USE MPDATA, ONLY:CELLDATA
      REAL ::DX,DY,BOTCB,BOT,TOP,PHI,R
      TYPE(CELLDATA) ::C
      
      C%HEAD = HEAD(J,I,K)
      C%ACTIVE = IBOUND(J,I,K)
      C%ZONE = IZONE(J,I,K)
      C%LAYCBD = LAYCBD(K)
      C%LAYTYP = LAYTYP(K)
      C%XMIN = XMIN(J)
      C%YMIN = YMIN(I)
      C%DX = DELR(J)
      C%DY = DELC(I)
      C%BOT = BOTM(J,I,LBOTM(K))
      IF(C%LAYCBD.EQ.0) THEN
        C%BOTCB = C%BOT
      ELSE
        C%BOTCB = BOTM(J,I,LBOTM(K) + 1)
      END IF
      IF(K.EQ.1) THEN
        C%TOP = BOTM(J,I,0)
      ELSE
        IF(LAYCBD(K-1).EQ.0) THEN
          C%TOP = BOTM(J,I,LBOTM(K-1))
        ELSE
          C%TOP = BOTM(J,I,LBOTM(K-1) + 1)
        END IF
      END IF
      IF(C%LAYTYP.GT.0 .AND. HEAD(J,I,K).LT.C%TOP) THEN
        C%TOP = HEAD(J,I,K)
      END IF
      
      C%VZCB = 0.0
      C%RFACCB = 1.0
      C%PORCB = 1.0
      IF(C%LAYCBD.NE.0) THEN
        C%RFACCB = RFAC(J,I,LBOTM(K) + 1)
        C%PORCB = POR(J,I,LBOTM(K) + 1)
        C%VZCB = QZ(J,I,K+1)/C%PORCB/C%DX/C%DY
      END IF
      
      C%RFAC = RFAC(J,I,LBOTM(K))
      C%POR = POR(J,I,LBOTM(K))
      
      C%QX1 = QX(J,I,K)
      C%QX2 = QX(J+1,I,K)
      C%QY1 = QY(J,I+1,K)
      C%QY2 = QY(J,I,K)
      C%QZ1 = QZ(J,I,K+1)
      C%QZ2 = QZ(J,I,K)
      C%QSINK = QSINK(J,I,K)
      C%QSOURCE = QSOURCE(J,I,K)
      C%QSTO = QSTO(J,I,K)
      
      DZ = C%TOP - C%BOT
      C%VX1 = C%QX1/C%POR/C%DY/DZ
      C%VX2 = C%QX2/C%POR/C%DY/DZ
      C%VY1 = C%QY1/C%POR/C%DX/DZ
      C%VY2 = C%QY2/C%POR/C%DX/DZ
      C%VZ1 = C%QZ1/C%POR/C%DY/C%DX
      C%VZ2 = C%QZ2/C%POR/C%DY/C%DX
      
      RETURN
      END
      
 
      SUBROUTINE DTCALC (V1,V2,V,DVDX,DX,XL,DT,IVFLG)
      REAL ::V1,V2,V,DVDX,DX,XL,DT
      REAL ::V1A,V2A,DV,DVA,X
      INTEGER ::IVFLG
C
      IVFLG=0
      DT= 1.0E+20
      V2A= ABS(V2)
      V1A= ABS(V1)
      DV= V2-V1
      DVA= ABS(DV)
      X = XL*DX
C
C  CHECK FOR A UNIFORM ZERO VELOCITY IN THIS DIRECTION
C  IF SO, SET DT=1E+20 AND IVFLG=2, THEN RETURN
C
      V=0.0
      IF(V2A.LE.1.0E-15.AND.V1A.LE.1.0E-15) THEN
        IVFLG=2
        RETURN
      END IF
C
C  CHECK FOR A CONSTANT UNIFORM NON-ZERO VELOCITY IN THIS DIRECTION.
C  VARIABLE VVV CAN ONLY BE << 0 IF V1 AND V2 ARE IN THE SAME
C  DIRECTION. IF VVV < 1E-4, V1 AND V2 ARE ESSENTIALLY EQUAL. IF
C  THIS CONDITION EXISTS, SET V=V1 AND COMPUTE TRAVEL TIME USING 
C  CONSTANT VELOCITY. SET IVFLG=1.
      VV=V1A
      IF(V2A.GT.VV) VV=V2A
      VVV= DVA/VV
      IF(VVV.LE.1.0E-4) THEN
        IVFLG=1
        ZRO= 1.0E-15
        ZROM= -ZRO
        V=V1
        IF(V1.GT.ZRO) DT= (DX-X)/V1
        IF(V1.LT.ZROM) DT= -X/V1
        RETURN
      END IF
C
C  COMPUTE VELOCITY CORRESPONDING TO PARTICLE'S POSTION
C
      DVDX= DV/DX
      V= (1.0E+0-XL)*V1 + XL*V2
C
C  IF FLOW IS INTO CELL FROM BOTH SIDES THERE IS NO OUTFLOW, SO
C  SET IVFLG=3 AND RETURN
C
      IF(V1.GE.0.0E+0.AND.V2.LE.0.0E+0) THEN
        IVFLG=3
        RETURN
      END IF
C
C  IF FLOW IS OUT OF CELL ON BOTH SIDES, FIND LOCATION OF DIVIDE,
C  COMPUTE TRAVEL TIME TO EXIT FACE AND RETURN WITH IVFLG=0
C
      IF (V1.LE.0.0.AND.V2.GE.0.0) THEN
        IF (ABS(V).LE.0.0) THEN
          V= 1.0E-20
          IF (V2.LE.0.0) V= -V
        END IF
      END IF
      
      VR1= V1/V
      VR2= V2/V
      VR=VR1
      IF(VR.LE.0.0E+0) VR=VR2
      V1V2= V1*V2
      IF(V1V2.GT.0.0) THEN
        IF(V.GT.0.0) VR=VR2
        IF(V.LT.0.0) VR=VR1
      END IF
      DT= ALOG(VR)/DVDX
      RETURN
      
      END
      
      
      SUBROUTINE SWITCHCELL(P)
      USE GLOBAL, ONLY:IBOUND,LAYCBD,NLAY,NROW,NCOL
      USE PARTICLEDATA, ONLY:PARTICLE
      INTEGER ::JP,IP,KP,IEXIT,IFACE
      TYPE(PARTICLE) ::P
      
      JP = P%J
      IP = P%I
      KP = P%K
      IFACE = P%FACE
      
      SELECT CASE(IFACE)
        CASE (1)
          IF(JP.GT.1) THEN
            IF(IBOUND(JP-1,IP,KP).EQ.0) THEN
              P%STATUS = 2
            ELSE
              P%J = JP - 1
              P%FACE = 2
              P%XL = 1.0
              P%CELLCOUNT = P%CELLCOUNT + 1
            END IF
          ELSE
            IF(P%GRID.GT.1) THEN
C             ADD CODE TO MOVE PARTICLE FROM CHILD GRID TO THE ADJACENT PARENT GRID CELL
            ELSE
              P%STATUS = 2               
            END IF
          END IF
        CASE (2)
          IF(JP.LT.NCOL) THEN
            IF(IBOUND(JP+1,IP,KP).EQ.0) THEN
              P%STATUS = 2
            ELSE
              P%J = JP + 1
              P%FACE = 1
              P%XL = 0.0
              P%CELLCOUNT = P%CELLCOUNT + 1
            END IF
          ELSE
            IF(P%GRID.GT.1) THEN
C             ADD CODE TO MOVE PARTICLE FROM CHILD GRID TO THE ADJACENT PARENT GRID CELL
            ELSE
              P%STATUS = 2               
            END IF
          END IF
        CASE (3)
          IF(IP.LT.NROW) THEN
            IF(IBOUND(JP,IP+1,KP).EQ.0) THEN
              P%STATUS = 2
            ELSE
              P%I = IP + 1
              P%FACE = 4
              P%YL = 1.0
              P%CELLCOUNT = P%CELLCOUNT + 1
            END IF
          ELSE
            IF(P%GRID.GT.1) THEN
C             ADD CODE TO MOVE PARTICLE FROM CHILD GRID TO THE ADJACENT PARENT GRID CELL
            ELSE
              P%STATUS = 2               
            END IF              
          END IF
        CASE (4)
           IF(IP.GT.1) THEN
            IF(IBOUND(JP,IP-1,KP).EQ.0) THEN
              P%STATUS = 2
            ELSE
              P%I = IP - 1
              P%FACE = 3
              P%YL = 0.0
              P%CELLCOUNT = P%CELLCOUNT + 1
            END IF
          ELSE
            IF(P%GRID.GT.1) THEN
C             ADD CODE TO MOVE PARTICLE FROM CHILD GRID TO THE ADJACENT PARENT GRID CELL
            ELSE
              P%STATUS = 2               
            END IF
          END IF         
        CASE (5)
          IF(KP.LT.NLAY) THEN
            IF(IBOUND(JP,IP,KP+1).EQ.0) THEN
              P%STATUS = 2
            ELSE
              P%K = KP + 1
              P%FACE = 6
              P%ZL = 1.0
              P%CELLCOUNT = P%CELLCOUNT + 1
            END IF
          ELSE 
            IF(P%GRID.GT.1) THEN
C             ADD CODE TO MOVE PARTICLE FROM CHILD GRID TO THE ADJACENT PARENT GRID CELL
            ELSE
              P%STATUS = 2               
            END IF              
          END IF
        CASE (6)
          IF(KP.GT.1) THEN
            IF(IBOUND(JP,IP,KP-1).EQ.0) THEN
              P%STATUS = 2
            ELSE
              P%K = KP - 1
              P%FACE = 5
              IF(LAYCBD(KP-1).EQ.0) THEN
                P%ZL = 0.0
              ELSE
                P%ZL = -1.0
              END IF
              P%CELLCOUNT = P%CELLCOUNT + 1
            END IF
          ELSE
            P%STATUS = 2               
          END IF
      END SELECT
      
      RETURN
      END
      
      SUBROUTINE WRITESUMMARY(HEADER)
      USE GLOBAL, ONLY:IZONE
      USE MPDATA, ONLY:HEADEREP,ZONETOTALS,IOLIST,DEFAULTREALPRECISION
      USE PARTICLEDATA, ONLY:PARTICLE,PARTICLEGROUP,PGROUPS,NPGROUPS
      USE DOUBLEBUDGET, ONLY:IPRBUD,IPRHEAD
      INTEGER ::IZ,NG,N,IREALPREC
      TYPE(HEADEREP) ::HEADER
      TYPE(PARTICLE), POINTER ::P
      
      DO NG=1,NPGROUPS
        DO N=1,PGROUPS(NG)%COUNT
          P => PGROUPS(NG)%PARTICLES(N)
          CALL SELECTGRID(P%GRID)
          IF(P%STATUS.EQ.2 .OR. P%STATUS.EQ.3) THEN
            IZ = IZONE(P%J,P%I,P%K)
            IF(IZ.GT.0.AND.IZ.LE.100) THEN
              ZONETOTALS(IZ) = ZONETOTALS(IZ) + 1
            ELSE IF(IZ.GT.100) THEN
              ZONETOTALS(0) = ZONETOTALS(0) + 1
            END IF
          END IF
        END DO
      END DO
      
      WRITE(IOLIST,*)
      WRITE(IOLIST,'(A)') 'Particle summary:'
      WRITE(IOLIST,'(A)') '-----------------'
      WRITE(IOLIST,'(I10,A)') HEADER%STATUSCOUNT(1),
     1    ' particles remain active'
      DO N=1,100
        IF(ZONETOTALS(N).GT.0) THEN
          WRITE(IOLIST,'(I10,A,I5)') 
     1      ZONETOTALS(N),' particles terminated in zone',N
        END IF
      END DO 
      IF(ZONETOTALS(0).GT.0) THEN
        WRITE(IOLIST,'(I10,A)') ZONETOTALS(0),
     1    ' particles terminated in cells with zone numbers > 100'
      END IF
      WRITE(IOLIST,'(I10,A)') HEADER%STATUSCOUNT(5),
     1  ' particles were stranded'
      WRITE(IOLIST,*)
      
      WRITE(IOLIST,'(A,I10)') 'Default real number precision = ',
     1                        DEFAULTREALPRECISION
     
      IF(IPRHEAD .EQ. 1) THEN
        WRITE(IOLIST,'(A)') 
     1  'Heads input from single-precision head file.'
      ELSE IF(IPRHEAD .EQ. 2) THEN
        WRITE(IOLIST,'(A)') 
     1  'Heads input from double-precision head file.'
      END IF
      
      IF(IPRBUD .EQ. 1) THEN
        WRITE(IOLIST,'(A)') 
     1  'Flows input from single-precision compact budget file.'
      ELSE IF(IPRBUD .EQ. 2) THEN
        WRITE(IOLIST,'(A)') 
     1  'Flows input from double-precision compact budget file.'
      END IF
      
      
      WRITE(IOLIST,*)
      
      RETURN
      END
      
      SUBROUTINE WRITEENDPOINTS(HEADER)
      USE MPDATA, ONLY:IOEPT,HEADEREP,REFTIME,ZONETOTALS,IOLIST,
     1 ITRACKDIR
      USE PARTICLEDATA, ONLY:PARTICLE,PARTICLEGROUP,PGROUPS,NPGROUPS
      TYPE(HEADEREP) HEADER
      INTEGER N,NG,STATUS,COUNT,ID,IOFLAG,ITERMZONE
      REAL XB,YB,ZB,X,Y,Z,X0,Y0
      TYPE(PARTICLE), POINTER ::P
      
      HEADER%LABEL = 'MODPATH_ENDPOINT_FILE 6 0'
      HEADER%REFTIME = REFTIME
      HEADER%TRACKDIR = ITRACKDIR
      HEADER%TOTALCOUNT = 0
      HEADER%STATUSCOUNT = 0
      HEADER%MAXID = 0
      COUNT = 0

C     LOOP THROUGH PARTICLES TO GET PARTICLE STATUS COUNTS AND TO
C     FIND THE MAXIMUM PARTICLE ID NUMBER FOR REFERENCE      
      DO NG=1,NPGROUPS
        DO N=1,PGROUPS(NG)%COUNT
          COUNT = COUNT + 1
          STATUS = PGROUPS(NG)%PARTICLES(N)%STATUS
          HEADER%STATUSCOUNT(STATUS) = HEADER%STATUSCOUNT(STATUS) + 1
          ID = PGROUPS(NG)%PARTICLES(N)%ID
          IF(ID.GT.HEADER%MAXID) HEADER%MAXID = ID
        END DO
      END DO
      
      HEADER%TOTALCOUNT = COUNT
      HEADER%RELEASECOUNT = HEADER%STATUSCOUNT(1)
      HEADER%RELEASECOUNT = HEADER%RELEASECOUNT + HEADER%STATUSCOUNT(2)
      HEADER%RELEASECOUNT = HEADER%RELEASECOUNT + HEADER%STATUSCOUNT(3)
      HEADER%RELEASECOUNT = HEADER%RELEASECOUNT + HEADER%STATUSCOUNT(5)

C     WRITE THE HEADER INFORMATION 
      CALL WRITEHEADEREP(IOEPT,HEADER)
      
C     WRITE THE PARTICLE ENDPOINT RECORDS     
      DO NG=1,NPGROUPS
        DO N=1,PGROUPS(NG)%COUNT
          P => PGROUPS(NG)%PARTICLES(N)
          STATUS = P%STATUS
          IOFLAG = 1
          IF(STATUS.EQ.0 .OR. STATUS.EQ.4) IOFLAG = 0
          IF(IOFLAG.EQ.1) THEN
           CALL WRITERECEP(P,IOEPT,IFMT,ITERMZONE)
          END IF
        END DO
      END DO
      
  
      RETURN
      END
      
      SUBROUTINE WRITEHEADEREP(IU,HEADER)
      USE MPDATA, ONLY:HEADEREP,DEFAULTREALPRECISION
      USE PARTICLEDATA, ONLY:PARTICLE,PARTICLEGROUP,PGROUPS,NPGROUPS
      TYPE(HEADEREP) HEADER
      INTEGER N
      CHARACTER(LEN=40) FMT
      
      IF(DEFAULTREALPRECISION .GE. 15) THEN
        FMT= '(I2,1X,3(I10,1X),E23.15)'
      ELSE
        FMT= '(I2,1X,3(I10,1X),E15.7)'
      END IF

      WRITE(IU,'(A)') HEADER%LABEL
      WRITE(IU,FMT) HEADER%TRACKDIR,HEADER%TOTALCOUNT,
     1 HEADER%RELEASECOUNT,HEADER%MAXID,HEADER%REFTIME
      WRITE(IU,'(5(I10,1X),I10)') (HEADER%STATUSCOUNT(N), N=0,5)
      WRITE(IU,'(I10)') NPGROUPS
      
      DO N=1,NPGROUPS
        WRITE(IU,'(A)') PGROUPS(N)%NAME
      END DO
      
      WRITE(IU,'(A)') 'END HEADER'
      RETURN
      END
      
      SUBROUTINE WRITERECEP(P,IU,IFMT,IZ)
      USE PARTICLEDATA, ONLY:PARTICLE
      USE MPDATA, ONLY:DEFAULTREALPRECISION
      USE GLOBAL, ONLY:IZONE
      TYPE(PARTICLE) ::P
      INTEGER ::IU,IFMT,IZB,IZ,LENLABEL
      REAL XB,YB,ZB,X,Y,Z

      CALL SELECTGRID(P%GRIDB)
      CALL GETGLOBALCOORD(P%XLB,P%YLB,P%ZLB,P%IB,P%JB,P%KB,XB,YB,ZB)
      IZB = IZONE(P%JB,P%IB,P%KB)
      CALL SELECTGRID(P%GRID)
      CALL GETGLOBALCOORD(P%XL,P%YL,P%ZL,P%I,P%J,P%K,X,Y,Z)
      IZ = IZONE(P%J,P%I,P%K)
      
      LENLABEL=LEN_TRIM(P%LABEL)
      
      IF(DEFAULTREALPRECISION .GE. 15) THEN
        IF(LENLABEL .LE. 0) THEN
          WRITE(IU,1200) P%ID,P%GROUP,P%STATUS,P%TIMEB,P%TIME,P%GRIDB,
     1    P%KB,P%IB,P%JB,P%FACEB,IZB,P%XLB,P%YLB,P%ZLB,XB,YB,ZB,P%GRID,
     2    P%K,P%I,P%J,P%FACE,IZ,P%XL,P%YL,P%ZL,X,Y,Z
        ELSE
          WRITE(IU,1300) P%ID,P%GROUP,P%STATUS,P%TIMEB,P%TIME,P%GRIDB,
     1    P%KB,P%IB,P%JB,P%FACEB,IZB,P%XLB,P%YLB,P%ZLB,XB,YB,ZB,P%GRID,
     2    P%K,P%I,P%J,P%FACE,IZ,P%XL,P%YL,P%ZL,X,Y,Z,P%LABEL(1:LENLABEL)
        END IF
      ELSE
        IF(LENLABEL .LE. 0) THEN
          WRITE(IU,1000) P%ID,P%GROUP,P%STATUS,P%TIMEB,P%TIME,P%GRIDB,
     1    P%KB,P%IB,P%JB,P%FACEB,IZB,P%XLB,P%YLB,P%ZLB,XB,YB,ZB,P%GRID,
     2    P%K,P%I,P%J,P%FACE,IZ,P%XL,P%YL,P%ZL,X,Y,Z
        ELSE
          WRITE(IU,1100) P%ID,P%GROUP,P%STATUS,P%TIMEB,P%TIME,P%GRIDB,
     1    P%KB,P%IB,P%JB,P%FACEB,IZB,P%XLB,P%YLB,P%ZLB,XB,YB,ZB,P%GRID,
     2    P%K,P%I,P%J,P%FACE,IZ,P%XL,P%YL,P%ZL,X,Y,Z,P%LABEL(1:LENLABEL)
        END IF
      END IF
      
1000  FORMAT(1X,I10,1X,I5,1X,I2,1X,2(E15.7,1X),I2,1X,3(I4,1X),I1,1X,
     1   I10,1X,6(E15.7,1X),I2,1X,3(I4,1X),I1,1X,I10,1X,5(E15.7,1X),
     2   E15.7)
1100  FORMAT(1X,I10,1X,I5,1X,I2,1X,2(E15.7,1X),I2,1X,3(I4,1X),I1,1X,
     1   I10,1X,6(E15.7,1X),I2,1X,3(I4,1X),I1,1X,I10,1X,5(E15.7,1X),
     2   E15.7,1X,A)
1200  FORMAT(1X,I10,1X,I5,1X,I2,1X,2(E23.15,1X),I2,1X,3(I4,1X),I1,1X,
     1   I10,1X,3(E15.7,1X),3(E23.15,1X),I2,1X,3(I4,1X),I1,1X,I10,1X,
     2   3(E15.7,1X),2(E23.15,1X),E23.15)
1300  FORMAT(1X,I10,1X,I5,1X,I2,1X,2(E23.15,1X),I2,1X,3(I4,1X),I1,1X,
     1   I10,1X,3(E15.7,1X),3(E23.15,1X),I2,1X,3(I4,1X),I1,1X,I10,1X,
     2   3(E15.7,1X),3(E23.15,1X),A)
      
      RETURN
      END
      
      SUBROUTINE WRITEHEADERPL(IU)
      USE MPDATA, ONLY:HEADERPL,ITRACKDIR,REFTIME,DEFAULTREALPRECISION
      TYPE(HEADERPL) ::HEADER
      CHARACTER(LEN=40) FMT
      
      IF(DEFAULTREALPRECISION .GE. 15) THEN
        FMT= '(I2,1X,E23.15)'
      ELSE
        FMT= '(I2,1X,E15.7)'
      END IF
      HEADER%LABEL = 'MODPATH_PATHLINE_FILE 6 0'
      HEADER%TRACKDIR = ITRACKDIR
      HEADER%REFTIME = REFTIME
      WRITE(IU,'(A)') HEADER%LABEL
      WRITE(IU,FMT) HEADER%TRACKDIR, HEADER%REFTIME
      WRITE(IU,'(A)') 'END HEADER'
      RETURN
      END
      
      SUBROUTINE WRITEHEADERTS(IU)
      USE MPDATA, ONLY:HEADERTS,ITRACKDIR,REFTIME,DEFAULTREALPRECISION
      TYPE(HEADERTS) ::HEADER
      CHARACTER(LEN=40) FMT
      
      IF(DEFAULTREALPRECISION .GE. 15) THEN
        FMT= '(I2,1X,E23.15)'
      ELSE
        FMT= '(I2,1X,E15.7)'
      END IF
      HEADER%LABEL = 'MODPATH_TIMESERIES_FILE 6 0'
      HEADER%TRACKDIR = ITRACKDIR
      HEADER%REFTIME = REFTIME
      WRITE(IU,'(A)') HEADER%LABEL
      WRITE(IU,FMT) HEADER%TRACKDIR,HEADER%REFTIME
      WRITE(IU,'(A)') 'END HEADER'
      RETURN
      END
      
      SUBROUTINE WRITERECPL(P,IU,ITIME,KTIME,IFMT)
      USE PARTICLEDATA, ONLY:PARTICLE
      USE MPDATA, ONLY:DEFAULTREALPRECISION
      TYPE(PARTICLE) ::P
      INTEGER ::IU,ITIME,KTIME,IFMT
      REAL ::X,Y,Z

      CALL SELECTGRID(P%GRID)
      CALL GETGLOBALCOORD(P%XL,P%YL,P%ZL,P%I,P%J,P%K,X,Y,Z)
      
      IF(DEFAULTREALPRECISION .GE. 15) THEN
        WRITE(IU,1100) P%ID,P%GROUP,ITIME,KTIME,P%TIME,X,Y,Z,P%K,P%I,
     1  P%J,P%GRID,P%XL,P%YL,P%ZL,P%LINENUM
      ELSE
        WRITE(IU,1000) P%ID,P%GROUP,ITIME,KTIME,P%TIME,X,Y,Z,P%K,P%I,
     1  P%J,P%GRID,P%XL,P%YL,P%ZL,P%LINENUM
      END IF
      
1000  FORMAT(I10,1X,3(I5,1X),4(E15.7,1X),3(I4,1X),I2,3(E15.7,1X),I5)
1100  FORMAT(I10,1X,3(I5,1X),4(E23.15,1X),3(I4,1X),I2,3(E15.7,1X),I5)
      
      RETURN
      END

      SUBROUTINE WRITERECTS(P,IU,ITIME,KTIME,IFMT)
      USE PARTICLEDATA, ONLY:PARTICLE
      USE MPDATA, ONLY:DEFAULTREALPRECISION
      TYPE(PARTICLE) ::P
      INTEGER ::IU,ITIME,KTIME,IFMT
      REAL ::X,Y,Z

      CALL SELECTGRID(P%GRID)
      CALL GETGLOBALCOORD(P%XL,P%YL,P%ZL,P%I,P%J,P%K,X,Y,Z)
      
      IF(DEFAULTREALPRECISION .GE. 15) THEN
        WRITE(IU,1100) ITIME,KTIME,P%TIME,P%ID,P%GROUP,X,Y,Z,P%GRID,P%K,
     1  P%I,P%J,P%XL,P%YL,P%ZL
      ELSE
        WRITE(IU,1000) ITIME,KTIME,P%TIME,P%ID,P%GROUP,X,Y,Z,P%GRID,P%K,
     1  P%I,P%J,P%XL,P%YL,P%ZL
      END IF
      
1000  FORMAT(I5,1X,I5,1X,E15.7,1X,I10,1X,I5,1X,3(E15.7,1X),I2,1X,
     1  3(I4,1X),2(E15.7,1X),E15.7)
1100  FORMAT(I5,1X,I5,1X,E23.15,1X,I10,1X,I5,1X,3(E23.15,1X),I2,1X,
     1  3(I4,1X),2(E15.7,1X),E15.7)
      
      RETURN
      END
      
      SUBROUTINE WRITERECADVOBS(P,ITIME)
      USE MPDATA, ONLY:IOAOBS,TIMEPTS
      USE PARTICLEDATA, ONLY:PARTICLE
      TYPE(PARTICLE) ::P
      INTEGER ITIME
      REAL ::X,Y,Z,DT,T
      
      IF(P%STATUS .LT. 1) RETURN
      IF(P%STATUS .GT. 2) RETURN
      
      CALL SELECTGRID(P%GRID)
      CALL GETGLOBALCOORD(P%XL,P%YL,P%ZL,P%I,P%J,P%K,X,Y,Z)
      
      T = P%TIME
      IF(P%STATUS .EQ. 2) THEN
        T = TIMEPTS(ITIME)
        DT = T - P%TIME
        X = X + P%EXITVEL(1)*DT
        Y = Y + P%EXITVEL(2)*DT
        Z = Z + P%EXITVEL(3)*DT
      END IF
      
      WRITE(IOAOBS,1000) P%ID,P%STATUS,X,Y,Z,T,P%EXITVEL(1),
     1                   P%EXITVEL(2),P%EXITVEL(3)
1000  FORMAT(I10,1X,I4,7(1X,E15.7))

      RETURN
      END
      
      SUBROUTINE WRITETRACE(P,C,KTIME,KPER,KSTP,TMAX,TSTART,
     1  XL0,YL0,ZL0,IEXIT)
      USE MPDATA, ONLY:IOTRACE,IDTRACE,CELLDATA,ITRACESEG
      USE PARTICLEDATA, ONLY:PARTICLE
      TYPE(PARTICLE) ::P
      TYPE(CELLDATA) ::C
      INTEGER KTIME,KPER,KSTP,IEXIT
      REAL TMAX,TSTART,DT
      
      DT = P%TIME - TSTART
      WRITE(IOTRACE,'(A,A)') 
     1 '************************************************************',
     2 '************************************************************'
      WRITE(IOTRACE,'(A,I10,A,I6)') 
     1 'Particle ID =',P%ID,'   Tracking segment =',ITRACESEG
      WRITE(IOTRACE,'(A,E15.7,A,E15.7,A,E15.7)')
     1 'Initial time =',TSTART,' Final time =',P%TIME,
     2 ' Time interval =',DT
      WRITE(IOTRACE,'(A,I5,A,I5)') 'Stress Period =',KPER,
     1 ' Time Step =',KSTP
      WRITE(IOTRACE,'(A,I4,A,I4,A,I4,A,I4)') 'Layer',P%K,' Row',P%I,
     1  ' Column',P%J,' Grid',P%GRID
      WRITE(IOTRACE,'(A,3F12.7)') 
     1 'Initial local coordinates (X, Y, Z): ',XL0,YL0,ZL0
      WRITE(IOTRACE,'(A,3F12.7)') 
     1 'Final local coordinates (X, Y, Z):   ',P%XL,P%YL,P%ZL
     
      WRITE(IOTRACE,'(A,I10,A)') 
     1  'This particle has passed through',P%CELLCOUNT,
     2  ' Cells (CELLCOUNT)'
      IF(IEXIT.EQ.0) THEN
        WRITE(IOTRACE,'(A)') 'Particle remains within the current cell.'
      ELSE
        WRITE(IOTRACE,'(A,I2)') 'Particle exits across face',P%FACE
      END IF
      
      WRITE(IOTRACE,'(A,E15.7,A,E15.7,A,E15.7,A,E15.7,A,E15.7,A,E15.7)')
     1  'Cell dimensions:  DX =',C%DX,' DY =',C%DY,' BOTTOM-CB =',
     2  C%BOTCB,' BOTTOM =',C%BOT,' TOP =',C%TOP,' HEAD =',C%HEAD
      WRITE(IOTRACE,'(A,E15.7,A,E15.7)') 'Cell corner: XMIN =',C%XMIN,
     1  '  YMIN =',C%YMIN
      WRITE(IOTRACE,'(A,I5)') 'Layer type: LAYTYP =',C%LAYTYP
      IF(C%LAYCBD.EQ.0) THEN
        WRITE(IOTRACE,'(A,A)') 'The cell does not have an underlying ',
     1    'quasi-3d confining bed (LAYCBD = 0)'
      ELSE
        WRITE(IOTRACE,'(A,I2,A)') 
     1  'The cell has an underlying confining bed (LAYCBD =',
     2  C%LAYCBD,')'
      END IF
      WRITE(IOTRACE,'(A,I10,A,I10)') 'IBOUND =',C%ACTIVE,' ZONE =',
     1  C%ZONE
      IF(C%LAYCBD.EQ.0) THEN
        WRITE(IOTRACE,'(A,E15.7,A,E15.7)') 'Pososity =',C%POR,
     1    ' Retardation factor =',C%RFAC
      ELSE
        WRITE(IOTRACE,'(A,E15.7,A,E15.7)') 'Pososity =',C%POR,
     1    ' Porosity of confining bed =',C%PORCB
        WRITE(IOTRACE,'(A,E15.7,A,E15.7)') 'Retardation factor =',
     1    C%RFAC,' Retardation factor of confining bed =',
     2    C%RFACCB
      END IF
      
      WRITE(IOTRACE,'(A)') 'Cell face volumetric flow rates:'
      WRITE(IOTRACE,'(5X,A,E15.7,A,E15.7)')
     1 'Face 1: QX1 =',C%QX1,'  Face 2: QX2 =',C%QX2
      WRITE(IOTRACE,'(5X,A,E15.7,A,E15.7)')
     1 'Face 3: QY1 =',C%QY1,'  Face 4: QY2 =',C%QY2
      WRITE(IOTRACE,'(5X,A,E15.7,A,E15.7)')
     1 'Face 5: QZ1 =',C%QZ1,'  Face 6: QZ2 =',C%QZ2
     
      WRITE(IOTRACE,'(A)') 'Cell face velocity:'
      WRITE(IOTRACE,'(5X,A,E15.7,A,E15.7)')
     1 'Face 1: VX1 =',C%VX1,'  Face 2: VX2 =',C%VX2
      WRITE(IOTRACE,'(5X,A,E15.7,A,E15.7)')
     1 'Face 3: VY1 =',C%VY1,'  Face 4: VY2 =',C%VY2
      WRITE(IOTRACE,'(5X,A,E15.7,A,E15.7)')
     1 'Face 5: VZ1 =',C%VZ1,'  Face 6: VZ2 =',C%VZ2
      IF(C%BOT.GT.C%BOTCB) THEN
        WRITE(IOTRACE,'(5X,A,E15.7)') 'Confining bed velocity:  VZCB =',
     1    C%VZCB
      END IF
      
      WRITE(IOTRACE,'(A,E15.7,A,E15.7,A,E15.7)') 
     1 'Internal sinks: QSINK =',C%QSINK,' Internal sources: QSOURCE =',
     2 C%QSOURCE,' Storage: QSTO =',C%QSTO
     
      RETURN
      END
      
