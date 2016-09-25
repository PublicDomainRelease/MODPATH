!  MPath7.f90 
!  PROGRAM: MPath7

    program MPath7
!*********************************************************************************
! Main program code for USGS MODPATH particle tracking model - Version 7
!
!   Specifiactions:
!---------------------------------------------------------------------------------
    use GlobalDataModule,only : niunit, narealsp, issflg, nper, mpbasUnit, disUnit, tdisUnit, mpugridUnit, &
        headUnit, headuUnit, budgetUnit, inUnit, pathlineUnit, endpointUnit, timeseriesUnit, binPathlineUnit, mplistUnit, &
        traceUnit, budchkUnit, aobsUnit, logUnit, mpsimUnit, traceModeUnit, mpnamFile, mplistFile, mpbasFile, disFile, & 
        tdisFile, gridFile, headFile, budgetFile, mpsimFile, traceFile, particleGroupCount, gridFileType
    use UtilMiscModule,only : ulog
    use utl8module,only : freeunitnumber, ustop
    use ModpathCellDataModule,only : ModpathCellDataType
    use ModpathBasicDataModule,only : ModpathBasicDataType
    use ModpathSimulationDataModule,only : ModpathSimulationDataType
    use BudgetReaderModule,only : BudgetReaderType
    use HeadReaderModule,only : HeadReaderType
    use RectangularUnstructuredGridModule,only : RectangularUnstructuredGridType
    use TimeDiscretizationDataModule,only : TimeDiscretizationDataType
    use ParticleTrackingEngineModule,only : ParticleTrackingEngineType
    use TrackPathResultModule,only : TrackPathResultType
    use ParticleLocationModule,only : ParticleLocationType
    use ParticleCoordinateModule,only : ParticleCoordinateType
    use ParticleGroupModule,only : ParticleGroupType
    use ParticleModule,only : ParticleType
    use ParticleManagerModule
    use BudgetRecordHeaderModule,only : BudgetRecordHeaderType
    implicit none
    
    ! Variables declarations
    type(HeadReaderType),allocatable :: headReader
    type(BudgetReaderType), allocatable :: budgetReader
    type(RectangularUnstructuredGridType), allocatable :: usgGrid
    type(TimeDiscretizationDataType), allocatable :: tdisData
    type(ParticleTrackingEngineType), allocatable,target :: trackingEngine
    type(ModpathBasicDataType), allocatable, target :: basicData
    type(ModpathSimulationDataType), allocatable, target :: simulationData
    type(ModpathCellDataType), allocatable, target :: cellData
    type(TrackPathResultType), target :: trackPathResult
    type(ParticleLocationType) :: pLoc
    type(ParticleCoordinateType),pointer :: pCoordFirst, pCoordLast, pCoordTP
    type(ParticleCoordinateType) :: pCoord
    type(ParticleGroupType),pointer :: pGroup
    type(ParticleType),pointer :: p
    type(BudgetRecordHeaderType) :: budgetRecordHeader
    integer,dimension(:),allocatable :: cellsPerLayer
    doubleprecision,dimension(:),allocatable :: timePoints
    doubleprecision,dimension(:),allocatable :: tPoint
    integer,dimension(7) :: budgetIntervalBins
    doubleprecision,dimension(6) :: budgetIntervalBreaks
    logical :: traceModeOn
    integer :: budgetIntervalBreakCount, maxErrorCell
    doubleprecision :: maxError
    integer :: clockCountStart, clockCountStop, clockCountRate, clockCountMax
    doubleprecision :: elapsedTime
    logical :: ssFlag
    integer :: cellCount, layerCount, groupIndex, particleIndex, pendingCount, activeCount, timePointCount, tPointCount, pathlineRecordCount
    integer :: stressPeriodCount, recordHeaderCount
    integer :: n, m, ktime, kfirst, klast, kincr, period, step, nt, count, plCount, tsCount, status, itend, particleID, topActiveCellNumber, auxCount
    doubleprecision :: t, stoptime, maxTime, tsMax, time
    character(len=132) message
    character(len=10) version
    character(len=30) versionDate
    character(len=75) terminationMessage
    
!---------------------------------------------------------------------------------
    
    ! Set version
    version = '7.1.000'
    versionDate = ' (September 26, 2016)'
    
    write(*,'(1x/a,a,a)') 'MODPATH Version ', version, versionDate
    write(*,*)
    
    ! Set the default termination message
    terminationMessage = "Normal termination."
    
    ! Assign dedicated file unit numbers
     disUnit = 101
     endpointUnit = 102
     pathlineUnit = 103
     timeseriesUnit = 104
     mplistUnit = 105
     traceUnit = 106
     budchkUnit = 107
     aobsUnit = 108
     logUnit = 109
     mpsimUnit = 110
     tdisUnit = 111
     mpbasUnit = 112
     headUnit = 113
     budgetUnit = 114
     traceModeUnit = 115
     binPathlineUnit = 116
     
    ! Open the log file
    open(unit=logUnit, file='mpath7.log', status='replace', form='formatted', access='sequential')
    
    ! Get the name of the MODPATH simulation file
    call ulog('Get the name of the MODPATH simulation file.', logUnit)
    call GetSimulationFile(mpsimFile)
    if(len_trim(mpsimFile) .eq. 0) then
        write(*, '(a)') 'The simulation file could not be found. Stop.'
        goto 100
    end if
    
    ! Read the first two records of the simulation file to get the names of the 
    ! name file (mpnamFile) and the listing file (mplistFile)
    allocate(simulationData)
    open(unit=mpsimUnit, file=mpsimFile, status='old', form='formatted', access='sequential')
    call ulog('Read the first two records of the simulation file to get mpnamFile and mplistFile ...', logUnit)
    call simulationData%ReadFileHeaders(mpsimUnit)
    mpnamFile = simulationData%NameFile
    mplistFile = simulationData%ListingFile
    
    ! Open the MODPATH output listing file
    open(unit=mplistUnit, file=mplistFile, status='replace', form='formatted', access='sequential')
    
    write(mplistUnit,'(1x/a,a,a)') 'MODPATH Version ', version, versionDate
    
    ! Read the MODPATH name file
    call ReadNameFile(mpnamFile, mplistUnit, gridFileType)
    
    ! Process spatial and time discretization data
    call ulog('Allocate rectangular unstructured grid component.', logUnit)
    allocate(usgGrid)
    call ulog('Allocate time discretization data component ...', logUnit)
    allocate(tdisData)
    
    write(mplistUnit, '(1x/a)') 'Grid data'
    write(mplistUnit, '(a)')    '---------'
      
    select case (gridFileType)
        case (1) 
            ! MODFLOW-2005 discretization file
            ! Read spatial and time discretization. 
            write(mplistUnit, '(a,1x)') 'Grid file type: MODFLOW-2005 discretization file (DIS)'
            call ulog('Read rectangular unstructured grid data.', logUnit)
            call usgGrid%ReadFileType1(disUnit, mplistUnit, stressPeriodCount)
            call tdisData%ReadFileType1(disUnit, mplistUnit, stressPeriodCount)
            
            ! Close discretization files
            close(disUnit)
            
        case (2)
            ! MODPATH spatial(MPUGRID) and time (TDIS) discretization files 
            ! Read spatial discretization
            write(mplistUnit, '(a,1x)') 'Grid file type: MODPATH unstructured rectangular grid file.'
            call ulog('Read rectangular unstructured grid data.', logUnit)
            call usgGrid%ReadFileType2(disUnit, mplistUnit)
            
            ! Read time discretization file
            if(len_trim(tdisFile) .gt. 0) then
                write(mplistUnit, '(a,1x)') 'Time discretization file type: MODPATH time discretization file.'
                call ulog('Read time discretization data component ...', logUnit)
                call tdisData%ReadFileType2(tdisUnit, mplistUnit)
            else
                call ulog('The time discretization file was not specified.', logUnit)
                call ustop('The time discretization file was not specified.')
            end if
            
            ! Close discretization files
            close(disUnit)
            close(tdisUnit)
        
        case default
            write(mplistUnit, '(1x,a)') 'Unknown grid file type. Stop.'
            stop
            
    end select
    
    ! Fill the cellsPerLayer array
    layerCount = usgGrid%GetLayerCount()
    cellCount = usgGrid%GetCellCount()
    allocate(cellsPerLayer(layerCount))
    m = 0
    do n = 1, layerCount
        cellsPerLayer(n) = usgGrid%GetLayerCellCount(n)
        m = m + cellsPerLayer(n)
    end do
    if(m .ne. cellCount) then
        call ustop('The sum of cells per layer does not match the number of cells in the grid. Stop.')
    end if
    
    ! Initialize the budgetReader component
    call ulog('Allocate budget reader component.', logUnit)
    allocate(budgetReader)
    call ulog('Open budget file in budget reader.', logUnit)
    write(mplistUnit, *)
    call budgetReader%OpenBudgetFile(budgetFile, budgetUnit, mplistUnit)
    if(budgetReader%GetFileOpenStatus()) then
        write(mplistUnit, '(1x,a)') 'The budget file was opened successfully.'
    else
        call ustop('An error occurred processing the budget file. Stopping.')
    end if
    
    ! Initialize the headReader component
    call ulog('Allocate head reader component.', logUnit)
    allocate(headReader)
    call ulog('Open head file in head reader component.', logUnit)
    call headReader%OpenFile(headFile, headUnit, mplistUnit)
    
    ! Read the MODPATH basic data file
    call ulog('Allocate MODPATH basic data component.', logUnit)
    allocate(basicData)
    call ulog('Read MODPATH basic data component.', logUnit)   
    call basicData%ReadData(mpbasUnit, mplistUnit, cellsPerLayer, layerCount, cellCount, usgGrid)
    
    ! Read the remainder of the MODPATH simulation file
    call ulog('Read the remainder of the MODPATH simulation data component.', logUnit)
    call simulationData%ReadData(mpsimUnit, mplistUnit, cellsPerLayer, layerCount, cellCount, basicData%IBound, tdisData, usgGrid)
        
    ! Budget File Data Summary
    ! If budget output option = 2, then write a list of budget record headers.
    call WriteBudgetFileInfo(mplistUnit, budgetReader) 
    if(simulationData%BudgetOutputOption .eq. 2) call WriteBudgetRecordHeaders(mplistUnit, budgetReader)
        
    ! Prepare to stop if there are no particles to track
    if(simulationData%TotalParticleCount .eq. 0) then
        terminationMessage = 'The simulation was terminated because there are no particles to track.'
        goto 100
    end if
    
    ! Initialize the particle tracking engine:
    call ulog('Allocate particle tracking engine component.', logUnit)
    allocate(trackingEngine)
    call trackingEngine%Initialize(headReader, budgetReader, usgGrid, basicData%HNoFlow, basicData%HDry, simulationData%TrackingOptions)
    call trackingEngine%SetIBound(basicData%IBound,usgGrid%GetCellCount())
    call trackingEngine%SetPorosity(basicData%Porosity, usgGrid%GetCellCount())
    call trackingEngine%SetLayerTypes(basicData%LayerTypes, usgGrid%GetLayerCount() )
    call trackingEngine%SetZones(simulationData%Zones, usgGrid%GetCellCount())
    call trackingEngine%SetRetardation(simulationData%Retardation, usgGrid%GetCellCount())
    call trackingEngine%SetDefaultIface(basicData%DefaultIfaceLabels, basicData%DefaultIfaceValues, basicData%DefaultIfaceCount)
    ! The trackingEngine initialization is complete
    
    ! Compute range of time steps to use in the time step loop
    message ='Compute range of time steps. Prepare for time step loop'
    call ulog(message, logUnit)
    !
    kfirst = tdisData%FindContainingTimeStep(simulationData%ReferenceTime)
    if(simulationData%TrackingDirection .eq. 1) then
        klast = tdisData%CumulativeTimeStepCount
        kincr = 1
    else
        klast = 1
        kincr = -1
    end if 
    
    ! Set the appropriate value of stoptime. Start by setting stoptime to correspond to the start or the
    ! end of the simulation (depending on the tracking direction)
    if(simulationData%TrackingDirection .eq. 1) then
        stoptime = tdisData%TotalTimes(tdisData%CumulativeTimeStepCount) - simulationData%ReferenceTime
        ssFlag = tdisData%SteadyStateFlags(tdisData%CumulativeTimeStepCount)
    else
        stoptime = simulationData%ReferenceTime
        ssFlag = tdisData%SteadyStateFlags(1)
    end if
    !
    if(simulationData%StoppingTimeOption .eq. 2) then
        ! Set stoptime to 1.0d+30 if the EXTEND option is on and the boundary time step is steady state.
        ! If the boundary time step is transient, leave stoptime set to correspond to the beginning or 
        ! end of the simulation.
        if(ssFlag) stoptime = 1.0d+30
    else if(simulationData%StoppingTimeOption .eq. 3) then
        ! If a specific stoptime was specified, always apply it if ssFlag indicates a steady-state time step at the beginning
        ! or end of the time domain of the simulation.
        if(ssFlag) then
            stoptime = simulationData%StopTime
        else
        ! If the boundary time step is transient, do not set stoptime to the specified value if it would extend beyond
        ! the time domain of the simulation.
            if(simulationData%StopTime .lt. stoptime) stoptime = simulationData%StopTime
        end if
    end if
    
    write(mplistUnit, '(1x/a,e15.7)') 'The simulation will be run with stoptime = ', stoptime

    write(*,*)
    write(*,'(A)') 'Run particle tracking simulation ...'    
    write(mplistUnit, *)
    write(mplistUnit, *)
    write(mplistUnit,'(1X,A)') 'Run particle tracking simulation ...'
    
    ! Allocate tPoint array
    tPointCount = 0
    if(simulationData%SimulationType .eq. 2) then
        tPointCount = simulationData%TimePointCount
        if(tPointCount .gt. 0) tPointCount = 1
    else if(simulationData%SimulationType .ge. 3) then
        tPointCount = 1
    end if
    if(allocated(tPoint)) deallocate(tPoint)
    allocate(tPoint(tPointCount))
    
    ! Open particle output files
    open(unit=endpointUnit, file=simulationData%EndpointFile, status='replace', form='formatted', access='sequential')
    if((simulationData%SimulationType .eq. 2) .or. (simulationData%SimulationType .eq. 4)) then
        open(unit=pathlineUnit, file=simulationData%PathlineFile, status='replace', form='formatted', access='sequential')
!        open(unit=consolidatedPathlineUnit, file='consolidated.pathline7', status='replace', form='formatted', access='sequential')
        open(unit=binPathlineUnit, status='scratch', form='binary', access='stream', action='readwrite')
        call WritePathlineHeader(pathlineUnit, simulationData%TrackingDirection, simulationData%ReferenceTime)
    end if
    if((simulationData%SimulationType .eq. 3) .or. (simulationData%SimulationType .eq. 4)) then
        open(unit=timeseriesUnit, file=simulationData%TimeseriesFile, status='replace', form='formatted', access='sequential')
        call WriteTimeseriesHeader(timeseriesUnit, simulationData%TrackingDirection, simulationData%ReferenceTime)
    end if
    if(simulationData%TraceMode .gt. 0) then
        open(unit=traceModeUnit, file=simulationData%TraceFile, status='replace', form='formatted', access='sequential')
        write(traceModeUnit, '(1X,A,I10)') 'Particle group: ',simulationData%TraceGroup
        write(traceModeUnit, '(1X,A,I10)') 'Particle ID: ',simulationData%TraceID
    end if
    
    ! Begin time step loop
    pathlineRecordCount = 0
    time = 0.0d0
    nt = 0
    if(allocated(cellData)) deallocate(cellData)
    allocate(cellData)
    
    call ulog('Begin TIME_STEP_LOOP', logUnit)
    ! Call system_clock to get the start of the time step loop
    call system_clock(clockCountStart, clockCountRate, clockCountMax)
    TIME_STEP_LOOP: do ktime= kfirst,klast,kincr
    
    ! Get the stress period and time step from the cummulative time step
    call tdisData%GetPeriodAndStep(ktime, period, step)
    
    write(message,'(A,I5,A,I5,A,1PE12.5)') 'Processing Time Step ',step,' Period ',period,'.  Time = ',tdisData%TotalTimes(ktime)
    write(*,'(A)') message
    write(mplistUnit, *)
    write(mplistUnit,'(1X,A)') '----------------------------------------------------------------------------------------------'
    write(mplistUnit,'(A,A,I6,A)') message,'  (Cumulative step = ', ktime,')'
    write(mplistUnit,'(1X,A)') '----------------------------------------------------------------------------------------------'
    
    ! Load data for the current time step
    call trackingEngine%LoadTimeStep(period, step)
    
    ! Check water balance summary for the current time step
    if(simulationData%BudgetOutputOption .gt. 0) call WriteWaterBalanceSummary(mplistUnit, trackingEngine, cellData)
    
    ! Check cell-by-cell budgets for this time step
    if(simulationData%BudgetCellsCount .gt. 0) then
        write(mplistUnit, *) 
        write(mplistUnit, '(1X,A,I10,A)') 'Cell data will be printed for', simulationData%BudgetCellsCount, ' cells.'
        do n = 1, simulationData%BudgetCellsCount
            call trackingEngine%FillCellBuffer(simulationData%BudgetCells(n), cellData)
            call trackingEngine%WriteCellBuffer(mplistUnit, cellData, simulationData%TrackingOptions%BackwardTracking)
        end do
    end if
    
    ! Compute the tracking time corresponding to the end or beginning 
    ! of this MODFLOW time step (depending on whether this is a forward 
    ! or backward tracking run.)
    message = 'Compute TSMAX'
    call ulog(message, logUnit)
    if(simulationData%TrackingDirection .eq. 1) then
      ! Forward trackine
      tsMax = tdisData%TotalTimes(ktime) - simulationData%ReferenceTime
      if(simulationData%StoppingTimeOption .eq. 2) then
          if(ktime .eq. tdisData%CumulativeTimeStepCount) tsMax = stoptime
      else if(simulationData%StoppingTimeOption .eq. 3) then
          if(ktime .eq. tdisData%CumulativeTimeStepCount) then
              tsMax = stoptime
          else
              if(tsMax .gt. stoptime) tsMax = stoptime    
          end if 
      end if
    else
      ! Backward tracking
      if(ktime .gt. 1) then
          tsMax = simulationData%ReferenceTime - tdisData%TotalTimes(ktime-1)
      else
          tsMax = simulationData%ReferenceTime
      end if
      if(simulationData%StoppingTimeOption .eq. 2) then
          if(ktime .eq. 1) tsMax = stoptime
      else if(simulationData%StoppingTimeOption .eq. 3) then
          if(ktime .eq. 1) then
              tsMax = stoptime
          else
              if(tsMax .gt. stoptime) tsMax = stoptime    
          end if 
      end if
    end if
    
    ! If simulation type is TIMESERIES, write initial locations of all particles active at tracking time = 0
    if((simulationData%SimulationType .ge.3) .and. (ktime .eq. kfirst)) then
        do groupIndex =1, simulationData%ParticleGroupCount
            do particleIndex = 1, simulationData%ParticleGroups(groupIndex)%TotalParticleCount
                ! Add code
                  p => simulationData%ParticleGroups(groupIndex)%Particles(particleIndex)
                  if((p%Status .eq. 0) .and. (p%InitialTrackingTime .eq. 0.0d0)) then
                      pCoord%CellNumber = p%CellNumber
                      pCoord%Layer = p%Layer
                      pCoord%LocalX = p%LocalX
                      pCoord%LocalY = p%LocalY
                      pCoord%LocalZ = p%LocalZ
                      pCoord%TrackingTime = p%TrackingTime
                      call usgGrid%ConvertToGlobalXYZ(pCoord%CellNumber, pCoord%LocalX, pCoord%LocalY, pCoord%LocalZ, pCoord%GlobalX, pCoord%GlobalY, pCoord%GlobalZ)
                      p%InitialGlobalZ = pCoord%GlobalZ
                      p%GlobalZ = p%InitialGlobalZ
                      call WriteTimeseriesRecord(p%SequenceNumber, p%ID, groupIndex, ktime, 0, pCoord, timeseriesUnit)
                  end if
            end do
        end do
    end if
    
    

    ! TRACKING_INTERVAL_LOOP: 
    ! Loop through all the required time points that fall within the
    ! current MODFLOW time step. For runs that do not have any specified 
    ! time points, there will only be one time point that corresponds either 
    ! to the beginning or end of the current MODFLOW time step or to the 
    ! specified stop time for the MODPATH analysis.
    itend = 0
    call ulog('Begin TRACKING_INTERVAL_LOOP', logUnit)
    TRACKING_INTERVAL_LOOP: do while (itend .eq. 0)
    
    itend = 1
    maxTime = tsMax
    if(simulationData%SimulationType .gt. 1) then     
        ! For timeseries and pathline runs, find out if maxTime should be set to the value of the
        ! next time point or the time at the end of the time step
        if (nt+1 .le. simulationData%TimePointCount) then
            if (simulationData%TimePoints(nt+1) .le. tsMax) then
              nt = nt + 1
              maxTime = simulationData%TimePoints(nt)
              tPoint(1) = maxTime
              itend = 0
              if(maxTime .eq. tsMax) itend = 1
            end if
        end if
    end if
    
    ! Track particles
    pendingCount = 0
    activeCount = 0
    if(simulationData%ParticleGroupCount .gt. 0) then
        do groupIndex = 1, simulationData%ParticleGroupCount
            do particleIndex = 1, simulationData%ParticleGroups(groupIndex)%TotalParticleCount
                p => simulationData%ParticleGroups(groupIndex)%Particles(particleIndex)
                ! Check particle status. 
                ! Skip over particles unless they are active or pending release         
                if(p%Status .gt. 1) then
                    ! Add code here later to deal with advective observations
                    ! For now, just cycle to the next particle
                    cycle
                end if
                
                ! Check to see if trace mode should be turned on for this particle
                traceModeOn = .false.
                if(simulationData%TraceMode .gt. 0) then 
                    if((p%Group .eq. simulationData%TraceGroup) .and. (p%ID .eq. simulationData%TraceID)) traceModeOn = .true.
                end if
                
                ! If a particle is pending release (STATUS = 0), check to see if it should
                ! be set to active and released on this pass. If the particle is pending
                ! release and its release time is earlier than the starting time of this
                ! pass, then mark the particle status as permanently unreleased 
                ! (STATUS = 8).
                if(p%Status .eq. 0) then
                    if(p%InitialTrackingTime .lt. time) then
                        p%Status = 8
                    else if(p%InitialTrackingTime .le. maxTime) then
                        p%Status = 1
                        if(p%Drape .eq. 0) then
                            ! Drape option is not in effect.
                            if(trackingEngine%IboundTS(p%CellNumber) .eq. 0) then
                                p%Status = 7
                            end if
                        else
                            ! Drape option is in effect. Find the top-most active cell starting with the initial cell number. 
                            ! If no active cell is found, leave the cell number set to its original value and set the Status = 7
                            ! to indicate it is stranded in an inactive cell.
                            topActiveCellNumber = trackingEngine%GetTopMostActiveCell(p%CellNumber)
                            if(topActiveCellNumber .gt. 0) then
                                p%CellNumber = topActiveCellNumber
                            else
                                p%Status = 7
                            end if
                        end if
                        call usgGrid%ConvertToGlobalZ(p%InitialCellNumber, p%InitialLocalZ, p%InitialGlobalZ, .true.)
                        p%GlobalZ = p%InitialGlobalZ
                    end if
                end if
         
                ! Count the number of particles that are currently active or pending
                ! release at the beginning of this pass.         
                if(p%Status .EQ. 0) pendingCount = pendingCount + 1
                if(p%Status .EQ. 1) activeCount = activeCount + 1
                
                ! Track the particle if it is active
                if(p%Status .eq. 1) then
                    ! Set particle location buffer
                    pLoc%CellNumber = p%CellNumber
                    pLoc%Layer = p%Layer
                    pLoc%LocalX = p%LocalX
                    pLoc%LocalY = p%LocalY
                    pLoc%LocalZ = p%LocalZ
                    pLoc%TrackingTime = p%TrackingTime
                    
                    ! Call TrackPath
                    call trackingEngine%TrackPath(trackPathResult, traceModeOn, traceModeUnit, p%Group, p%ID, p%SequenceNumber, pLoc, maxTime, tPoint, tPointCount)
                    
                    ! Update endpoint data. The Face property will only be updated when the endpoint file is written
                    plCount = trackPathResult%ParticlePath%Pathline%GetItemCount()
                    tsCount = trackPathResult%ParticlePath%Timeseries%GetItemCount()
                    pCoordLast => trackPathResult%ParticlePath%Pathline%Items(plCount)
                    pCoordFirst => trackPathResult%ParticlePath%Pathline%Items(1)
                    p%CellNumber =  pCoordLast%CellNumber
                    p%Layer = pCoordLast%Layer
                    p%LocalX = pCoordLast%LocalX
                    p%LocalY = pCoordLast%LocalY
                    p%LocalZ = pCoordLast%LocalZ
                    p%GlobalZ = pCoordLast%GlobalZ
                    p%TrackingTime = pCoordLast%TrackingTime
                    
                    ! Update particle status
                    status = trackPathResult%Status
                    if(  status .eq. trackPathResult%Status_ReachedBoundaryFace()) then
                        p%Status = 2
                    else if(status .eq. trackPathResult%Status_StopAtWeakSink()) then
                        p%Status = 3
                    else if(status .eq. trackPathResult%Status_StopAtWeakSource()) then
                        p%Status = 4
                    else if(status .eq. trackPathResult%Status_NoExitPossible()) then
                        p%Status = 5
                    else if(status .eq. trackPathResult%Status_StopZoneCell()) then
                        p%Status = 6
                    else if(status .eq. trackPathResult%Status_InactiveCell()) then
                        p%Status = 7
                    else if(status .eq. trackPathResult%Status_Undefined()) then
                        p%Status = 9
                    else
                        ! Leave status set to active (status = 1)
                    end if
                    
                    ! Write particle output
                    if((simulationData%SimulationType .eq. 2) .or. (simulationData%SimulationType .eq. 4)) then
                        ! Write pathline to pathline file
                        if(plCount .gt. 1) then
                            pathlineRecordCount = pathlineRecordCount + 1
                            select case (simulationData%PathlineFormatOption)
                                case (1)
                                    call WriteBinaryPathlineRecord(trackPathResult, binPathlineUnit, period, step)
                                case (2)
                                    call WritePathlineRecord(trackPathResult, pathlineUnit, period, step)
                            end select
                            
                        end if
                    end if              
                    if(simulationData%SimulationType .ge. 3) then
                        if(tsCount .gt. 0) then
                        ! Write timeseries record to the timeseries file
                            pCoordTP => trackPathResult%ParticlePath%Timeseries%Items(1)
                            call WriteTimeseriesRecord(p%SequenceNumber, p%ID, groupIndex, ktime, nt, pCoordTP, timeseriesUnit)
                        end if
                    end if
                end if
                
            end do
        end do
    end if
    
    ! Update tracking time
    time = maxTime
    
    ! Check to see if there are any particles remaining to track in the next
    ! pass. If not, exit the loop.
    if(simulationData%ParticleGroupCount .gt. 0) then
        if(activeCount .eq. 0 .and. pendingCount .eq. 0) then
            call ulog('No active particles remain. Exit TRACKING_INTERVAL_LOOP.', logUnit)
            exit TIME_STEP_LOOP
        end if
    end if

    end do TRACKING_INTERVAL_LOOP   
    call ulog('Exit TRACKING_INTERVAL_LOOP', logUnit)
       
    ! Exit TIME_STEP_LOOP if the tracking time has reached the specified stop time.
    IF(time .ge. stoptime) exit TIME_STEP_LOOP

    end do TIME_STEP_LOOP
    call ulog('Exit TIME_STEP_LOOP', logUnit)
    
    ! Stop timer
    call system_clock(clockCountStop, clockCountRate, clockCountMax)
    
    ! Write endpoint file
    if(simulationData%ParticleGroupCount .gt. 0) then
        call ulog('Write endpoint file.', logUnit)
        call WriteEndpoints(simulationData, usgGrid, endpointUnit)
    end if
    
    ! Finalize and process binary pathline file if pathline format option = 1
    if((simulationData%SimulationType .eq. 2) .or. (simulationData%SimulationType .eq. 4)) then
        if(simulationData%PathlineFormatOption .eq. 1) then
            call ulog('Consolidating pathline segments.', logUnit)
            call ConsolidatePathlines(binPathlineUnit, pathlineUnit, pathlineRecordCount, simulationData%TotalParticleCount) 
        end if
    end if
    
    ! Write particle summary information
    call WriteParticleSummaryInfo(simulationData, mplistUnit)
    
100 continue    
    ! Deallocate major components
    call ulog('Begin memory deallocation.', logUnit)
    if(allocated(headReader)) deallocate(headReader)
    if(allocated(budgetReader)) deallocate(budgetReader)
    if(allocated(usgGrid)) deallocate(usgGrid)
    if(allocated(tdisData)) deallocate(tdisData)
    if(allocated(trackingEngine)) deallocate(trackingEngine)
    if(allocated(basicData)) deallocate(basicData)
    if(allocated(simulationData)) deallocate(simulationData)
    call ulog('Memory deallocation complete.', logUnit)
    
    write(*, '(a)') terminationMessage
    write(mplistUnit, '(1x/,a)', err=200) terminationMessage
    elapsedTime = dble(clockCountStop - clockCountStart) / dble(clockCountRate)
    write(mplistUnit, '(1X,A,E15.5,A)') 'Elapsed time = ', elapsedTime, ' seconds'
    
    ! Close files
200 continue    
    close(mplistUnit)
    close(logUnit)

    ! Uncomment the following pause statement when running in debug mode within Visual Studio.
    ! The pause statement keeps the command window from immediately closing when the MODPATH run completes.
    ! For a compiled executable, do not use the pause statement, but run the executable from inside a batch
    ! file to keep the window from closing immediately.
    
    !pause
    
    contains
    
    subroutine GetSimulationFile(mpsimFile)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
    use utl7module,only : urword
    implicit none
    character*(*),intent(inout) :: mpsimFile
    character*200 comlin, line
    integer :: icol, istart, istop, n, nc
    real(kind=4) :: r
    logical :: exists
!---------------------------------------------------------------------------------------------------------------
   
    mpsimFile = ' '
    comlin = ' '
    
    call getarg(1,comlin)
    
    icol = 1
    if(comlin .ne. ' ') then
      mpsimFile = comlin
    else
15     write (*,*) ' Enter the MODPATH simulation file: '
      read (*,'(a)') mpsimFile
      call urword(mpsimFile,icol,istart,istop,0,n,r,0,0)
      mpsimFile = mpsimFile(istart:istop)
      IF (mpsimFile .eq. ' ') goto 15
    end if 
    inquire (file=mpsimFile, exist=exists)
    if(.not. exists) then
      nc = index(mpsimFile,' ')
      mpsimFile(nc:nc+5)='.mpsim'
      inquire (file=mpsimFile, exist=exists)
      if(.not. exists) return
    end if
    mpsimFile = trim(mpsimFile)
    
    end subroutine GetSimulationFile
    
    subroutine ReadNameFile(filename, outUnit, gridFileType)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
    use UTL8MODULE,only : urword, ustop
    implicit none
    character*(*),intent(in) :: filename
    integer,intent(in) :: outUnit
    integer,intent(inout) :: gridFileType
    character(len=200) :: line
    character(len=150) :: fname
    character(len=16) :: filtyp
    character(len=30) :: gridFileTypeString
    integer,dimension(5) :: nfiltyp
    integer :: inUnit, n, icol, ityp1, ityp2, inam1, inam2, nc, iflen, numflag, istart, istop
    doubleprecision :: r
    logical :: complete
!---------------------------------------------------------------------------------------------------------------
    
    do n = 1, 5
        nfiltyp(n) = 0
    end do
    gridFile = ' '
    tdisFile = ' '
    mpbasFile = ' '
    headFile = ' '
    budgetFile = ' '
    
    inUnit = 99
    open(unit=inUnit, file=filename, status='old', form='formatted', access='sequential')
    
    write(outUnit, '(1x/a)') 'MODPATH name file data'
    write(outUnit, '(a)')    '----------------------'
    
        gridFileType = 0
    do
        read(inUnit, '(a)', end=1000) line
        ! Check for comment lines
        if(line(1:1) .eq. '#') cycle
        if(line(1:1) .eq. '!') cycle
        if(line(1:2) .eq. '//') cycle
        
        ! Check for unit numbers
        numflag = 0
        read(line, *, err=200) filtyp, n
        numflag = 1
        goto 200
200     continue
        
        icol=1
        call urword(line, icol, istart, istop, 1, n, r, outUnit, inUnit)
        filtyp = line(istart:istop)
        if(numflag .eq. 1) then
            call urword(line, icol, istart, istop, 2, n, r, outUnit, inUnit)
        end if
        call urword(line, icol, istart, istop, 0, n, r, outUnit, inUnit)
        iflen = istop - istart + 1
        fname(1:iflen) = line(istart:istop)
        
        if(filtyp .eq. 'DIS') then
            gridFile = fname(1:iflen)
            open(unit=disUnit,file=gridFile,status='old', form='formatted', access='sequential')
            write(outUnit,'(A15,A)') 'DIS File: ', gridFile(1:iflen)
            nfiltyp(1) = 1
            nfiltyp(2) = 1
            gridFileType = 1
            cycle
        end if
        
        if(filtyp .eq. 'MPUGRID') then
            gridFile = fname(1:iflen)
            open(unit=disUnit,file=gridFile,status='old', form='formatted', access='sequential')
            write(outUnit,'(A15,A)') 'MPUGRID File: ', gridFile(1:iflen)
            nfiltyp(1) = 2
            gridFileType = 2
            cycle
        end if
        
        if(filtyp .eq. 'TDIS') then
            if((gridFileType .eq. 2) .or. (gridFileType .eq. 3)) then
                tdisFile = fname(1:iflen)
                open(unit=tdisUnit,file=tdisFile,status='old', form='formatted', access='sequential')
                write(outUnit,'(A15,A)') 'TDIS File: ', tdisFile(1:iflen)
                nfiltyp(2) = 1
            end if
            cycle
        end if
        
        if(filtyp .eq. 'MPBAS') then
            mpbasFile = fname(1:iflen)
            open(unit=mpbasUnit,file=mpbasFile,status='old', form='formatted', access='sequential')
            write(outUnit,'(A15,A)') 'MPBAS File: ', mpbasFile(1:iflen)
            nfiltyp(3) = 1
            cycle
        end if
        
        if(filtyp .eq. 'HEAD') then
            headFile = fname(1:iflen)
            write(outUnit,'(A15,A)') 'HEAD File: ', headFile(1:iflen)
            nfiltyp(4) = 1
            cycle
        end if
        
        if(filtyp .eq. 'BUDGET') then
            budgetFile = fname(1:iflen)
            write(outUnit,'(A15,A)') 'BUDGET File: ', budgetFile(1:iflen)
            nfiltyp(5) = 1
            cycle
        end if
          
    end do
    
1000 continue
     
     if(gridFileType .eq. 0) then
        call ustop('No valid grid file type was specified in the name file. Stop.')
     else
         complete = .true.
         do n = 1, 5
             if(nfiltyp(n) .eq. 0) complete = .false.
         end do
         if(.not. complete) then
             call ustop('The name file is not complete. Stop.')
         end if
     end if
    
100 continue
   
    end subroutine
    
    subroutine WriteBudgetFileInfo(outUnit, budgetReader)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
    use BudgetReaderModule,only : BudgetReaderType
    use BudgetRecordHeaderModule,only : BudgetRecordHeaderType
    integer,intent(in) :: outUnit
    type(BudgetReaderType),intent(in) :: budgetReader
    
    write(outUnit,*)
    write(outUnit, '(1x,a)') 'Budget File Data'
    write(outUnit, '(1x,a)') '----------------'
    if(budgetReader%GetBudgetType() .eq. 1) then
        write(outUnit, '(1x,a20,2x,a)') 'Budget file type:',  'Structured grid'
    else if(budgetReader%GetBudgetType() .eq. 2) then
        write(outUnit, '(1x,a20,2x,a)') 'Budget file type:',  'Unstructured grid' 
    else
        write(outUnit, '(1x,a20,2x,a)') 'Budget file type:',  'Undetermined type'
        return
    end if
    if(budgetReader%GetBudgetFileFormat() .eq. 1) then
        write(outUnit, '(1x,a20,2x,a)') 'Budget file format:',  'Standard'
    else if(budgetReader%GetBudgetFileFormat() .eq. 2) then
        write(outUnit, '(1x,a20,2x,a)') 'Budget file format:',  'Compact'           
    end if
    if(budgetReader%GetPrecisionType() .eq. 1) then
            write(outUnit, '(1x,a20,2x,a)') 'Budget precision:',  'Single'
    else if(budgetReader%GetPrecisionType() .eq. 2) then
            write(outUnit, '(1x,a20,2x,a)') 'Budget precision:',  'Double'            
    end if
    
    return
    
    end subroutine WriteBudgetFileInfo
    
    subroutine WriteBudgetRecordHeaders(outUnit, budgetReader)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
    use BudgetReaderModule,only : BudgetReaderType
    use BudgetRecordHeaderModule,only : BudgetRecordHeaderType
    integer,intent(in) :: outUnit
    type(BudgetReaderType),intent(in) :: budgetReader
    integer :: n, m, auxCount, recordHeaderCount
    type(BudgetRecordHeaderType) :: budgetRecordHeader
    
    write(outUnit,*)
    write(outUnit, '(1x,a)') 'Budget Record Headers:'
    if(budgetReader%GetBudgetFileFormat() .eq. 1) then
        write(outUnit, '(1x,a)') '    Record    Period      Step      Text label'
        recordHeaderCount = budgetReader%GetRecordHeaderCount()
        do n = 1, recordHeaderCount
            budgetRecordHeader = budgetReader%GetRecordHeader(n)
            write(outUnit, '(1x, 3i10,2x,a16)') n, budgetRecordHeader%StressPeriod, budgetRecordHeader%TimeStep, budgetRecordHeader%TextLabel
        end do
    else if(budgetReader%GetBudgetFileFormat() .eq. 2) then
        write(outUnit, '(1x,a)') '    Record    Period      Step      Text label      Method         Step length       Period length          Total time'
        recordHeaderCount = budgetReader%GetRecordHeaderCount()
        do n = 1, recordHeaderCount
            budgetRecordHeader = budgetReader%GetRecordHeader(n)
            write(outUnit, '(1x, 3i10,2x,a16,i10,3E20.12)') n, budgetRecordHeader%StressPeriod, budgetRecordHeader%TimeStep, budgetRecordHeader%TextLabel, budgetRecordHeader%Method, budgetRecordHeader%TimeStepLength,  budgetRecordHeader%StressPeriodLength, budgetRecordHeader%TotalTime
            if(budgetRecordHeader%Method .eq. 5) then
                auxCount = budgetRecordHeader%GetAuxiliaryNamesCount()
                write(outUnit, '(58x,a,i10,5x,a,i5)') 'List item count = ', budgetRecordHeader%ListItemCount, 'Auxiliary item count = ', auxCount
                do m = 1, auxCount
                    write(outUnit, '(58x,a)') budgetRecordHeader%AuxiliaryNames(m)
                end do
            else if(budgetRecordHeader%Method .eq. 2) then
                write(outUnit, '(58x,a,i10)') 'List item count = ', budgetRecordHeader%ListItemCount                    
            end if
        end do
    end if
    
    return
        
    end subroutine WriteBudgetRecordHeaders
    
    subroutine WriteWaterBalanceSummary(outUnit, trackingEngine, cellData)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
    use ParticleTrackingEngineModule,only : ParticleTrackingEngineType
    use ModpathCellDataModule,only : ModpathCellDataType
    integer,intent(in) :: outUnit
    type(ParticleTrackingEngineType),intent(in) :: trackingEngine
    type(ModpathCellDataType),intent(inout) :: cellData
    integer :: n, budgetIntervalBreakCount, maxErrorCell
    doubleprecision :: maxError
    doubleprecision,dimension(6) :: budgetIntervalBreaks
    integer,dimension(7) :: budgetIntervalBins
    
        budgetIntervalBreakCount = 6
        budgetIntervalBreaks(1) = 1.0d-02
        budgetIntervalBreaks(2) = 1.0d-01
        budgetIntervalBreaks(3) = 1.0d0
        budgetIntervalBreaks(4) = 1.0d+01
        budgetIntervalBreaks(5) = 5.0d+01
        budgetIntervalBreaks(6) = 1.0d+02
        maxErrorCell = 0
        maxError = 0.0d0
        do n = 1, budgetIntervalBreakCount  + 1
            budgetIntervalBins(n) = 0
        end do 
        
        call trackingEngine%GetVolumetricBalanceSummary(budgetIntervalBreakCount, budgetIntervalBreaks, budgetIntervalBins, maxError, maxErrorCell)
        
        write(outUnit, *)
        write(outUnit, '(1X,A)') 'Volumetric water balance summary:'
        write(outUnit, *)
        
        write(outUnit, '(1X,I10,A,F8.2,A)') budgetIntervalBins(1), ' cells had errors less than or equal to', budgetIntervalBreaks(1), ' percent'
        do n = 2, budgetIntervalBreakCount
            write(outUnit, '(1X,I10,A,F8.2,A,F8.2,A)') budgetIntervalBins(n), ' cells had errors between ', budgetIntervalBreaks(n-1), ' and ', budgetIntervalBreaks(n), ' percent'                
        end do
        write(outUnit, '(1X,I10,A,F8.2,A)') budgetIntervalBins(budgetIntervalBreakCount + 1), ' cells had errors greater than ', budgetIntervalBreaks(budgetIntervalBreakCount), ' percent'
        
        write(outUnit, *)
        write(outUnit, '(1X,A,E12.5,A,I10)') 'A maximum error of ', maxError, ' percent occurred in cell ', maxErrorCell
        write(outUnit, *)
        call trackingEngine%FillCellBuffer(maxErrorCell, cellData)
        call trackingEngine%WriteCellBuffer(outUnit, cellData, simulationData%TrackingOptions%BackwardTracking)
    
        return
        
    end subroutine WriteWaterBalanceSummary
    
    subroutine WriteParticleSummaryInfo(simulationData, outUnit)
!***************************************************************************************************************
! Description goes here
!***************************************************************************************************************
!
! Specifications
!---------------------------------------------------------------------------------------------------------------
    use ParticleModule,only : ParticleType
    implicit none
    type(ModpathSimulationDataType),target,intent(in) :: simulationData
    type(ParticleType),pointer :: p
    integer,intent(in) :: outUnit
    integer :: groupIndex, particleIndex, n
    integer,dimension(0:9) :: statusBins
    
    do n = 0, 9
        statusBins(n) = 0
    end do
    
    do groupIndex = 1, simulationData%ParticleGroupCount
        do particleIndex = 1, simulationData%ParticleGroups(groupIndex)%TotalParticleCount
            p => simulationData%ParticleGroups(groupIndex)%Particles(particleIndex)
            if(p%Status.ge.0 .and. p%Status.le.9) then
                statusBins(p%Status) = statusBins(p%Status) + 1
            else
                statusBins(9) = statusBins(9) + 1
            end if
        end do
    end do
    
    ! Write to listing file
    write(outUnit, '(1x/a)') 'Particle Summary:'
    write(outUnit, '(i10,1x,a)') statusBins(0), 'particles are pending release.'
    write(outUnit, '(i10,1x,a)') statusBins(1), 'particles remain active.'
    write(outUnit, '(i10,1x,a)') statusBins(2), 'particles terminated at boundary faces.'
    write(outUnit, '(i10,1x,a)') statusBins(3), 'particles terminated at weak sink cells.'
    write(outUnit, '(i10,1x,a)') statusBins(4), 'particles terminated at weak source cells.'
    write(outUnit, '(i10,1x,a)') statusBins(5), 'particles terminated at strong source/sink cells or other cells with no potential exit face.'
    write(outUnit, '(i10,1x,a)') statusBins(6), 'particles terminated in cells with a specified zone number.'    
    write(outUnit, '(i10,1x,a)') statusBins(7), 'particles were stranded in inactive or dry cells.'
    write(outUnit, '(i10,1x,a)') statusBins(8), 'particles were unreleased.'
    write(outUnit, '(i10,1x,a)') statusBins(9), 'particles have an unknown status.'
    write(outUnit, '(a)') ' '

    ! Write to screen
    write(*, '(1x/a)') 'Particle Summary:'
    write(*, '(i10,1x,a)') statusBins(0), 'particles are pending release.'
    write(*, '(i10,1x,a)') statusBins(1), 'particles remain active.'
    write(*, '(i10,1x,a)') statusBins(2), 'particles terminated at boundary faces.'
    write(*, '(i10,1x,a)') statusBins(3), 'particles terminated at weak sink cells.'
    write(*, '(i10,1x,a)') statusBins(4), 'particles terminated at weak source cells.'
    write(*, '(i10,1x,a)') statusBins(5), 'particles terminated at strong source/sink cells.'
    write(*, '(i10,1x,a)') statusBins(6), 'particles terminated in cells with a specified zone number.'    
    write(*, '(i10,1x,a)') statusBins(7), 'particles were stranded in inactive or dry cells.'
    write(*, '(i10,1x,a)') statusBins(8), 'particles were unreleased.'
    write(*, '(i10,1x,a)') statusBins(9), 'particles have an unknown status.'
    write(*, '(a)') ' '
    
    end subroutine WriteParticleSummaryInfo
    
    end program MPath7
