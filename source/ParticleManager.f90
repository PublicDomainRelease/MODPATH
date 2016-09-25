module ParticleManagerModule
  use ParticleGroupModule,only : ParticleGroupType
  use ParticleModule,only : ParticleType
  use ParticleCoordinateModule,only : ParticleCoordinateType
  use ModpathSimulationDataModule,only : ModpathSimulationDataType
  implicit none
  
  contains

  
  subroutine WriteEndpoints(simData, grid, outUnit)
  use RectangularUnstructuredGridModule,only : RectangularUnstructuredGridType
  implicit none
  integer,intent(in) :: outUnit
  integer :: version, subversion, pgIndex, pIndex, n, face, zone, initialZone, totalCount, releaseCount, maximumID
  doubleprecision :: initialGlobalX, initialGlobalY, initialGlobalZ, globalX, globalY, globalZ
  integer,dimension(0:9) :: statusSums
  type(ParticleType),pointer :: p
  type(RectangularUnstructuredGridType),intent(in) :: grid
  type(ModpathSimulationDataType),intent(in),target :: simData
  
  version = 7
  subversion = 0
  
  ! Compute status sums for all particles
  totalCount = 0
  releaseCount = 0
  maximumID = 0
  do n = 0, 9
      statusSums(n) = 0
  end do
  do pgIndex = 1, simData%ParticleGroupCount
      do pIndex = 1, simData%ParticleGroups(pgIndex)%TotalParticleCount
          totalCount = totalCount + 1
          p => simData%ParticleGroups(pgIndex)%Particles(pIndex)
          if((p%Status .ge. 0) .and. (p%Status .le. 9)) then
              statusSums(p%Status) = statusSums(p%Status) + 1
              if((p%Status .ne. 0) .and. (p%Status .ne. 8) .and. (p%ID .gt. maximumID)) maximumID = p%ID
          end if
      end do
  end do
  releaseCount = totalCount - statusSums(0) - statusSums(8)
  
  write(outUnit, '(a,2i10)') 'MODPATH_ENDPOINT_FILE', version, subversion
  write(outUnit, '(i5,3i10,1x,e18.10)') simData%TrackingDirection, totalCount, releaseCount, maximumID, simData%ReferenceTime
  write(outUnit, '(10i8)') (statusSums(n), n = 0, 9)
  write(outUnit, '(i10)') simData%ParticleGroupCount
  do n = 1, simData%ParticleGroupCount
      write(outUnit, '(a)') simData%ParticleGroups(n)%Name
  end do
  write(outUnit, '(a)') 'END HEADER'
  
  do pgIndex = 1, simData%ParticleGroupCount
      do pIndex = 1, simData%ParticleGroups(pgIndex)%TotalParticleCount
          p => simData%ParticleGroups(pgIndex)%Particles(pIndex)
          if((p%Status .ne. 0) .and. (p%Status .ne. 8) ) then
              call grid%ConvertToGlobalXYZ(p%InitialCellNumber, p%InitialLocalX, p%InitialLocalY, p%InitialLocalZ, initialGlobalX, initialGlobalY, initialGlobalZ)
              call grid%ConvertToGlobalXYZ(p%CellNumber, p%LocalX, p%LocalY, p%LocalZ, globalX, globalY, globalZ)
              face = FindFace(p%LocalX, p%LocalY, p%LocalZ)
              initialZone = simData%Zones(p%InitialCellNumber)
              zone = simData%Zones(p%CellNumber)
              write(outUnit, '(3i10,i5,2e18.10,i10,i5,6e18.10,2i5,i10,i5,6e18.10,2i5)') &
                   p%SequenceNumber, p%Group, p%ID, p%Status, p%InitialTrackingtime, p%TrackingTime, p%InitialCellNumber, p%InitialLayer, &
                   p%InitialLocalX, p%InitialLocalY, p%InitialLocalZ, initialGlobalX, initialGlobalY, p%InitialGlobalZ, initialZone, p%InitialFace, &
                   p%CellNumber, p%Layer, p%LocalX, p%LocalY, p%LocalZ, globalX, globalY, p%GlobalZ, zone, face
          end if
      end do
  end do
  
  
  end subroutine WriteEndpoints

  subroutine WriteTimeseriesHeader(outUnit, trackingDirection, referenceTime)
  implicit none
  integer,intent(in) :: outUnit, trackingDirection
  doubleprecision,intent(in) :: referenceTime
  integer :: version, subversion
  
  version = 7
  subversion = 0
  write(outUnit, '(a,2i10)') 'MODPATH_TIMESERIES_FILE', version, subversion
  write(outUnit, '(i5,1x,e18.10)') trackingDirection, referenceTime
  write(outUnit, '(a)') 'END HEADER'
  
  end subroutine WriteTimeseriesHeader
  
  subroutine WriteTimeseriesRecord(sequenceNumber, particleID, groupIndex, timeStep, timePointIndex, pCoord, outUnit)
  implicit none
  type(ParticleCoordinateType),intent(in) :: pCoord
  integer,intent(in) :: outUnit, particleID, timePointIndex, groupIndex, timeStep, sequenceNumber
  
  write(outUnit, '(2I8,e18.10,i10,i5,2i10,6e18.10,i10)') timePointIndex, timeStep, pCoord%TrackingTime, sequenceNumber, groupIndex, particleID, pCoord%CellNumber, &
                       pCoord%LocalX, pCoord%LocalY, pCoord%LocalZ, pCoord%GlobalX, pCoord%GlobalY, pCoord%GlobalZ, pCoord%Layer
  
  end subroutine WriteTimeseriesRecord

  subroutine WritePathlineHeader(outUnit, trackingDirection, referenceTime)
  implicit none
  integer,intent(in) :: outUnit, trackingDirection
  doubleprecision,intent(in) :: referenceTime
  integer :: version, subversion
  
  version = 7
  subversion = 0
  write(outUnit, '(a,2i10)') 'MODPATH_PATHLINE_FILE', version, subversion
  write(outUnit, '(i5,1x,e18.10)') trackingDirection, referenceTime
  write(outUnit, '(a)') 'END HEADER'
  
  end subroutine WritePathlineHeader
  
  subroutine WritePathlineRecord(tpResult, outUnit, stressPeriod, timeStep)
  use TrackPathResultModule,only : TrackPathResultType
  use ParticleCoordinateModule,only : ParticleCoordinateType
  implicit none
  integer,intent(in) :: outUnit, stressPeriod, timeStep
  type(TrackPathResultType),intent(in),target :: tpResult
  type(ParticleCoordinateType),pointer :: c
  integer :: n, count
  
  count = tpResult%ParticlePath%Pathline%GetItemCount()
  if(count .lt. 2) return
  
  write(outUnit, '(4i10)') tpResult%SequenceNumber, tpResult%Group, tpResult%ParticleID, count
  do n = 1, count
        c => tpResult%ParticlePath%Pathline%Items(n)
        write(outUnit, "(i10,7e16.8,3I10)") c%CellNumber, c%GlobalX, c%GlobalY, c%GlobalZ, c%TrackingTime, c%LocalX, c%LocalY, c%LocalZ, c%Layer, stressPeriod, timeStep
  end do
 
  end subroutine WritePathlineRecord
  
  subroutine WriteBinaryPathlineRecord(tpResult, outUnit, stressPeriod, timeStep)
  use TrackPathResultModule,only : TrackPathResultType
  use ParticleCoordinateModule,only : ParticleCoordinateType
  implicit none
  integer,intent(in) :: outUnit, stressPeriod, timeStep
  type(TrackPathResultType),intent(in),target :: tpResult
  type(ParticleCoordinateType),pointer :: c
  integer :: n, count, currentPosition, dataOffset
  
  count = tpResult%ParticlePath%Pathline%GetItemCount()
  if(count .lt. 2) return
  
  inquire(unit=outUnit, pos=currentPosition)
  write(outUnit) tpResult%SequenceNumber, count, tpResult%Group, tpResult%ParticleID
  do n = 1, count
        c => tpResult%ParticlePath%Pathline%Items(n)
        write(outUnit) c%CellNumber, c%GlobalX, c%GlobalY, c%GlobalZ, c%TrackingTime, c%LocalX, c%LocalY, c%LocalZ, c%Layer, stressPeriod, timeStep
  end do
  
  end subroutine WriteBinaryPathlineRecord
  
  subroutine ConsolidatePathlines(inUnit, outUnit, recordCount, particleCount)
  implicit none
  integer, intent(in) :: inUnit, outUnit, recordCount, particleCount
  integer, dimension(:), allocatable :: sequenceNumbers, recordPointCounts, recordPointers, particlePointCounts, particleRecordCounts
  integer :: pos, currentPos, dataOffset, n, m, i, count, ptr, group, id, stressPeriod, timeStep
  integer :: cellNumber, layer, pointCount
  doubleprecision :: globalX, globalY, globalZ, trackingTime, localX, localY, localZ
  
  allocate(sequenceNumbers(recordCount))
  allocate(recordPointCounts(recordCount))
  allocate(recordPointers(recordCount))
  allocate(particlePointCounts(particleCount))
  allocate(particleRecordCounts(particleCount))
  
  ! Initialize all particle point count and record count array elements to 0
  do n = 1, particleCount
      particlePointCounts(n) = 0
      particleRecordCounts(n) = 0
  end do
  
  do n = 1, recordCount
      sequenceNumbers(n) = 0
      recordPointCounts(n) = 0
      recordPointers(n) = 0
  end do 
  
  ! Read and save pathline record header index information
  pos = 1
  do n = 1, recordCount
      recordPointers(n) = pos
      read(inUnit, pos=pos) sequenceNumbers(n), recordPointCounts(n)
      m = sequenceNumbers(n)
      if(m .gt. 0) then
          particlePointCounts(m) = particlePointCounts(m) + recordPointCounts(n)
          particleRecordCounts(m) = particleRecordCounts(m) + 1
      end if
      dataOffset = 16 + (recordPointCounts(n)*72)
      pos = recordPointers(n) + dataOffset
  end do
  
  ! Loop through particles. For each particle, loop through the pathline record headers and read and 
  ! consolidate pathline points for all of the segments belonging to that particle. Then write the 
  ! consolidated pathline for the particle. Skip particles for which the particlePointCount equals 0.
  do n = 1, particleCount
      if(particlePointCounts(n) .gt. 0) then
          count = 0
          do m = 1, recordCount
              if(sequenceNumbers(m) .eq. n) then
                  ! Increment the record count for this particle
                  count = count + 1
                  
                  ! If this is the first pathline record for this particle, write the record header for the
                  ! consolidated pathline.
                  if(count .eq. 1) then
                      ! Read the rest of the record header
                      ptr = recordPointers(m) + 8
                      read(inUnit, pos=ptr) group, id
                      pointCount = particlePointCounts(n) - ParticleRecordCounts(n) + 1
                      write(outUnit, '(4i10)') n, group, id, pointCount
                  else
                      ptr = recordPointers(m) + 16
                      read(inUnit, pos=ptr)
                  end if
                  
                  do i = 1, recordPointCounts(m)
                      read(inUnit) cellNumber, globalX, globalY, globalZ, trackingTime, localX, localY, localZ, layer, stressPeriod, timeStep
                      if((count .gt. 1) .and. (i .eq. 1)) cycle
                      write(outUnit, "(i10,7e16.8,3I10)") cellNumber, globalX, globalY, globalZ, trackingTime, localX, localY, localZ, layer, stressPeriod, timeStep
                  end do
              end if
              ! Check to see if this was the last pathline record for this particle. If so, exit the record loop because there is no need 
              ! to read through the rest of the records for this particle.
              if(count .eq. particleRecordCounts(n)) exit
          end do
      end if
  end do
  
  end subroutine ConsolidatePathlines
  
  function FindFace(x, y, z) result(face)
  implicit none
  doubleprecision,intent(in) :: x, y, z
  integer :: face
  
  face = 0
  if(x .eq. 0.0d0) then
      face = 1
  else if(x .eq. 1.0d0) then
      face = 2
  else if(y .eq. 0.0d0) then
      face = 3
  else if(y .eq. 1.0d0) then
      face =4
  else if(z .eq. 0.0d0) then
      face = 5
  else if(z .eq. 1.0d0) then
      face = 6
  end if
  
  end function FindFace

end module ParticleManagerModule