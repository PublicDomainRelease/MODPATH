module HeadReaderModule
  use UTL8MODULE,only : freeunitnumber
  use HeadRecordHeaderModule,only : HeadRecordHeaderType
  use UtilMiscModule,only : TrimAll
  implicit none
  
! Set default access status to private
  private
  
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: HeadReaderType
    integer,private :: FileUnit = -1
    character(len=:),private,allocatable :: Filename
    integer,private :: OutputUnit = 0
    integer,private :: PrecisionType = 0
    integer,private :: LayerCount = 0
    integer,private :: CellCount = 0
    integer,private :: GridType = 0
    integer,private :: RecordCount = 0
    integer,private :: TimeStepCount = 0
    type(HeadRecordHeaderType),private,allocatable,dimension(:) :: RecordHeadersUsg
    doubleprecision,private,allocatable,dimension(:) :: TotalTimes
  contains
    procedure :: OpenFile=>pr_OpenFile
    procedure :: CloseFile=>pr_CloseFile
    procedure :: GetFileOpenStatus=>pr_GetFileOpenStatus
    procedure :: GetFileUnit=>pr_GetFileUnit
    procedure :: GetFilename=>pr_GetFilename
    procedure :: GetGridType=>pr_GetGridType
    procedure :: GetOutputUnit=>pr_GetOutputUnit
    procedure :: GetPrecisionType=>pr_GetPrecisionType
    procedure :: GetLayerCount=>pr_GetLayerCount
    procedure :: GetCellCount=>pr_GetCellCount
    procedure :: GetRecordCount=>pr_GetRecordCount
    procedure :: GetRecordHeaderData=>pr_GetRecordHeaderData
    procedure :: GetHeaderPosition=>pr_GetHeaderPosition
    procedure :: GetHeaderOffset=>pr_GetHeaderOffset
    procedure :: GetDataOffset=>pr_GetDataOffset
    procedure :: GetTimeStepCount=>pr_GetTimeStepCount
    procedure :: GetTime=>pr_GetTime
    procedure :: FindRecordIndex=>pr_FindRecordIndex
    procedure :: FillTimeStepHeadBuffer=>pr_FillTimeStepHeadBuffer
    procedure :: FillHeadBuffer=>pr_FillHeadBuffer
    
    ! Private methods
    procedure,private :: ProcessUnstructuredRecordHeaders=>pr_ProcessUnstructuredRecordHeaders
    procedure,private :: ProcessStructuredRecordHeaders=>pr_ProcessStructuredRecordHeaders
    procedure,private :: ReadUnstructuredRecordHeader=>pr_ReadUnstructuredRecordHeader
    procedure,private :: ReadStructuredRecordHeader=>pr_ReadStructuredRecordHeader
    procedure,private :: CountLayersAndCells=>pr_CountLayersAndCells
    !procedure,private :: pr_FillRecordDataBuffer
  end type

contains

  function pr_GetTimeStepCount(this) result(count)
  implicit none
  class(HeadReaderType) :: this
  integer :: count
  
  count = this%TimeStepCount  
  
  end function pr_GetTimeStepCount

  function pr_GetTime(this, cumulativeTimeStep) result(time)
  implicit none
  class(HeadReaderType) :: this
  integer,intent(in) :: cumulativeTimeStep
  doubleprecision :: time
  
  time = this%TotalTimes(cumulativeTimeStep)
  
  end function pr_GetTime

  function pr_FindRecordIndex(this, stressPeriod, timeStep, layer) result(index)
  implicit none
  class(HeadReaderType) :: this
  integer,intent(in) :: stressPeriod, timeStep, layer
  integer :: index, n
  
  index = -1
  
  if(this%RecordCount .lt. 1) return
  
  do n = 1, this%RecordCount
      if((this%RecordHeadersUsg(n)%StressPeriod .eq. stressPeriod) .and. (this%RecordHeadersUsg(n)%TimeStep .eq. timeStep) .and. (this%RecordHeadersUsg(n)%Layer .eq. layer)) then
          index = n
          return
      end if
  
  end do
  
  end function pr_FindRecordIndex
  
  subroutine pr_FillTimeStepHeadBuffer(this, stressPeriod, timeStep, buffer, bufferSize, spaceAssigned)
  implicit none
  class(HeadReaderType) :: this
  integer,intent(in) :: stressPeriod, timeStep, bufferSize
  integer,intent(inout) :: spaceAssigned
  doubleprecision,dimension(bufferSize),intent(inout) :: buffer
  integer :: firstIndex, lastIndex, n, nfirst, nlast, nsize
  
  spaceAssigned = 0
  firstIndex = this%FindRecordIndex(stressPeriod, timeStep, 1)
  if(firstIndex .lt. 1) return
  
  lastIndex = firstIndex + this%LayerCount - 1
  
  nsize = 0
  do n = firstIndex, lastIndex
      nfirst = this%RecordHeadersUsg(n)%FirstCellNumber
      nlast = this%RecordHeadersUsg(n)%LastCellNumber
      nsize = nsize + (nlast - nfirst + 1)
  end do
  
  if(bufferSize .lt. nsize) return
  
  do n = firstIndex, lastIndex
      nfirst = this%RecordHeadersUsg(n)%FirstCellNumber
      nlast = this%RecordHeadersUsg(n)%LastCellNumber
      call this%FillHeadBuffer(n, buffer, bufferSize, nfirst, nlast, spaceAssigned)
      if(spaceAssigned .eq. 0) return
  end do
  
  spaceAssigned = nsize
  
  end subroutine pr_FillTimeStepHeadBuffer
  
  !subroutine pr_FillTimeStepLayerHeadBuffer(this, stressPeriod, timeStep, layer, buffer, bufferSize, spaceAssigned)
  !implicit none
  !class(HeadReaderType) :: this
  !integer,intent(in) :: stressPeriod, timeStep, bufferSize
  !integer,intent(inout) :: spaceAssigned
  !doubleprecision,dimension(bufferSize),intent(inout) :: buffer
  !integer :: firstIndex, lastIndex, n, nfirst, nlast, nsize
  !
  !spaceAssigned = 0
  !n = this%FindRecordIndex(stressPeriod, timeStep, layer)
  !if(n .lt. 1) return
  !
  !nfirst = this%RecordHeadersUsg(n)%FirstCellNumber
  !nlast = this%RecordHeadersUsg(n)%LastCellNumber
  !nsize = nlast - nfirst + 1
  !
  !if(bufferSize .lt. nsize) return
  !
  !call this%FillHeadBuffer(n, buffer, bufferSize, nfirst, nlast)
  !if((nfirst .gt. 0) .and. (nlast .gt. 0)) spaceAssigned = nlast - nfirst + 1
  !
  !end subroutine pr_FillTimeStepLayerHeadBuffer

  function pr_GetHeaderPosition(this, recordIndex) result(position)
  implicit none
  class(HeadReaderType) :: this
  integer,intent(in) :: recordIndex
  integer(kind=8) :: position
  
  position = this%RecordHeadersUsg(recordIndex)%HeaderPosition
  
  end function pr_GetHeaderPosition

  function pr_GetHeaderOffset(this, recordIndex) result(offset)
  implicit none
  class(HeadReaderType) :: this
  integer,intent(in) :: recordIndex
  integer :: offset
  
  offset = this%RecordHeadersUsg(recordIndex)%HeaderOffset
  
  end function pr_GetHeaderOffset

  function pr_GetDataOffset(this, recordIndex) result(offset)
  implicit none
  class(HeadReaderType) :: this
  integer,intent(in) :: recordIndex
  integer :: offset
  
  offset = this%RecordHeadersUsg(recordIndex)%DataOffset
  
  end function pr_GetDataOffset
  
  subroutine pr_FillHeadBuffer(this, recordIndex, buffer, bufferSize, nfirst, nlast, spaceAssigned)
  implicit none
  class(HeadReaderType) :: this
  integer,intent(in) :: recordIndex, bufferSize, nfirst, nlast
  integer,intent(inout) :: spaceAssigned
  doubleprecision,dimension(bufferSize),intent(inout) :: buffer
  real(kind=4) :: valueSingle
  integer(kind=8) :: position
  integer :: n, dataElementCount
  
  spaceAssigned = 0
  dataElementCount = this%RecordHeadersUsg(recordIndex)%LastCellNumber - this%RecordHeadersUsg(recordIndex)%FirstCellNumber + 1
  n = nlast - nfirst +1
  if(n .ne. dataElementCount) goto 100
  if(bufferSize .lt. nlast) goto 100
  
  position = this%RecordHeadersUsg(recordIndex)%GetDataPosition()
  read(this%FileUnit, pos=position, err=100)
  if(this%PrecisionType .eq. 1) then
      do n = nfirst, nlast
          read(this%FileUnit, err=100) valueSingle
          buffer(n) = dble(valueSingle)
          spaceAssigned = spaceAssigned + 1
      end do
  else if(this%PrecisionType .eq. 2) then
      do n = nfirst, nlast
          read(this%FileUnit, err=100) buffer(n)
          spaceAssigned = spaceAssigned + 1
      end do
  else
      goto 100
  end if
  
  return
  
100 continue
  spaceAssigned = 0
  return
  
  end subroutine pr_FillHeadBuffer

  function pr_CheckComplete(this, stressPeriodCount, layerCount, timeStepCounts, stressPeriodLengths, layerCellCounts) result(isComplete)
  implicit none
  class(HeadReaderType) :: this
  integer,intent(in) :: stressPeriodCount, layerCount
  integer,dimension(stressPeriodCount),intent(in) :: timeStepCounts
  integer,dimension(layerCount),intent(in) :: layerCellCounts
  doubleprecision,dimension(stressPeriodCount),intent(in) :: stressPeriodLengths
  integer :: period, step, layer, recordCount, bufferSize
  doubleprecision :: periodLength, tol, error
  logical :: isComplete
  
  isComplete = .false.
  
  tol = 1.0d-4
  recordCount = 0
  do period = 1, stressPeriodCount
      do step = 1, timeStepCounts(period)
          do layer = 1, layerCount
              recordCount = recordCount + 1
              if(recordCount .gt. this%RecordCount) return
              if(this%RecordHeadersUsg(recordCount)%StressPeriod .ne. period) return
              if(this%RecordHeadersUsg(recordCount)%timeStep .ne. step) return
              if(this%RecordHeadersUsg(recordCount)%Layer .ne. layer) return
              bufferSize = 1 + (this%RecordHeadersUsg(recordCount)%LastCellNumber - this%RecordHeadersUsg(recordCount)%FirstCellNumber)
              if(bufferSize .ne. layerCellCounts(layer)) return
              if((step .eq. 1) .and. (layer .eq. 1) ) then
                  periodLength = this%RecordHeadersUsg(recordCount)%StressPeriodLength
                  error = 2.0d0 * (stressPeriodLengths(period) - periodLength) / (stressPeriodLengths(period) + periodLength)
                  if(error .lt. 0.0d0) error = -error
                  if(error .gt. tol) return
              end if
          end do
      end do
  end do
  
  if(recordCount .ne. this%RecordCount) return
  
  isComplete = .true.
  
  end function pr_CheckComplete

  function pr_GetCellCount(this) result(count)
  implicit none
  class(HeadReaderType) :: this
  integer :: count
  
  count = this%CellCount
  
  end function pr_GetCellCount

  function pr_GetLayerCount(this) result(count)
  implicit none
  class(HeadReaderType) :: this
  integer :: count
  
  count = this%LayerCount
  
  end function pr_GetLayerCount
  
  function pr_GetRecordCount(this) result(count)
  implicit none
  class(HeadReaderType) :: this
  integer :: count
  
  count = this%RecordCount
  
  end function pr_GetRecordCount
  
  function pr_GetGridType(this) result(gridType)
  implicit none
  class(HeadReaderType) :: this
  integer :: gridType
  
  gridType = this%GridType
  
  end function pr_GetGridType

  subroutine pr_CountLayersAndCells(this, layerCount, cellCount)
  implicit none
  class(HeadReaderType) :: this
  integer,intent(inout) :: layerCount, cellCount
  integer :: nextLayer, n, period, step, layer, layerCellCount
  
  cellCount = 0
  layerCount = 0
  nextLayer = 1
  ! Count the layers for period 1, step 1 and assume that is the same for all time steps
  do n = 1, this%RecordCount
      period = this%RecordHeadersUsg(n)%StressPeriod
      step = this%RecordHeadersUsg(n)%TimeStep
      layer = this%RecordHeadersUsg(n)%Layer
      layerCellCount = 1 + this%RecordHeadersUsg(n)%LastCellNumber - this%RecordHeadersUsg(n)%FirstCellNumber
      
      if((period .ne. 1) .or. (step .ne. 1)) exit
      if(layer .eq. nextLayer) then
          layerCount = layerCount + 1
          nextLayer = layerCount + 1
          cellCount = cellCount + layerCellCount
      else
          layerCount = 0
          cellCount = 0
          return
      end if
      
  end do
  
  end subroutine pr_CountLayersAndCells
  
  subroutine pr_GetRecordHeaderData(this, recordIndex, stressPeriod, timeStep, periodLength, totalTime, layer, firstCellNumber, lastCellNumber )
  implicit none
  class(HeadReaderType) :: this
  integer,intent(in) :: recordIndex 
  integer,intent(inout) :: firstCellNumber, lastCellNumber, stressPeriod, timeStep, layer
  doubleprecision,intent(inout) :: totalTime, periodLength
  integer :: n, period, step, lay
  
  stressPeriod = 0
  timeStep = 0
  layer = 0
  periodLength = 0.0d0
  totalTime = 0.0d0
  firstCellNumber = 0
  lastCellNumber = 0
  if((recordIndex .lt. 1) .or. (recordIndex .gt. this%RecordCount)) return
  
  if(this%GridType .eq. 1) then
      ! Add code
  else if(this%GridType .eq. 2) then
      stressPeriod = this%RecordHeadersUsg(recordIndex)%StressPeriod
      timeStep = this%RecordHeadersUsg(recordIndex)%TimeStep
      layer = this%RecordHeadersUsg(recordIndex)%Layer
      periodLength = this%RecordHeadersUsg(recordIndex)%StressPeriodLength
      totalTime = this%RecordHeadersUsg(recordIndex)%TotalTime
      firstCellNumber = this%RecordHeadersUsg(recordIndex)%FirstCellNumber
      lastCellNumber = this%RecordHeadersUsg(recordIndex)%LastCellNumber
  end if
  
  end subroutine pr_GetRecordHeaderData

!---------------------------------------------------------
  function pr_GetPrecisionType(this) result(precisionType)
  implicit none
  class(HeadReaderType) :: this
  integer :: precisionType
  
  precisionType = this%PrecisionType
  
  end function pr_GetPrecisionType

!---------------------------------------------------------
  function pr_GetFileOpenStatus(this) result(fileOpened)
  implicit none
  class(HeadReaderType) :: this
  logical :: fileOpened
  
  fileOpened = .false.
  if(this%FileUnit .ne. -1) fileOpened = .true.
  
  end function pr_GetFileOpenStatus

!---------------------------------------------------------
  function pr_GetFileUnit(this) result(unitNumber)
  implicit none
  class(HeadReaderType) :: this
  integer :: unitNumber
  
  unitNumber = this%FileUnit
  
  end function pr_GetFileUnit

!---------------------------------------------------------
  function pr_GetFilename(this) result(name)
  implicit none
  class(HeadReaderType) :: this
  character(len=:),allocatable :: name
  
  if(.not. allocated(this%Filename)) this%Filename = ''
  name = this%Filename
  
  end function pr_GetFilename

!---------------------------------------------------------
  function pr_GetOutputUnit(this) result(unit)
  implicit none
  class(HeadReaderType) :: this
  integer :: unit
  
  unit = this%OutputUnit
  
  end function pr_GetOutputUnit
  
!---------------------------------------------------------
  subroutine pr_OpenFile(this,filename, inUnit ,outputUnit)
  implicit none
  class(HeadReaderType) :: this
  integer,intent(in) :: inUnit, outputUnit
  character(len=*),intent(in) :: filename
  integer(kind=8) :: fileLength
  integer :: recordCount, layerCount, cellCount, timeStepCount, n, period, step
  character(len=:),allocatable :: openFileMessage
  
  this%Filename = trim(filename)
  this%OutputUnit = outputUnit
  this%FileUnit = inUnit
  
  ! Find a free file unit number and open the file for read unformatted stream access
!  call freeunitnumber(this%FileUnit)
  openFileMessage = ''
  open(unit=this%FileUnit,file=this%Filename,form='unformatted',access='stream',status='old',action='read',iomsg=openFileMessage,err=100)
  
  ! Find file size
  inquire(unit=this%FileUnit,size=fileLength)
  
  ! First try to process as an unstructured grid type
  ! Try to read as single precision
  call this%ProcessUnstructuredRecordHeaders(1)
  
  ! If the precision type is still undefined, try to read as double precision
  if(this%PrecisionType .eq. 0) then
      call this%ProcessUnstructuredRecordHeaders(2)
  end if
  
  ! Check to see if it was processed as a single or double precision unstructured file
  ! If not, try to process it as a structured file
  if((this%PrecisionType .eq. 0) .and. (this%GridType .eq. 0)) then
      call this%ProcessStructuredRecordHeaders(1)
      if(this%PrecisionType .eq. 0) then
          call this%ProcessStructuredRecordHeaders(2)
      end if
  end if
  
  ! If the grid type is still undefined it means the file could not a valid MODFLOW head file.
  if(this%PrecisionType .eq. 0) then
      ! Process the error and return.
      goto 100
  end if
  
  ! The file was opened successfully
  
  ! Count the layers for the first time step and store the value in LayerCount
  call this%CountLayersAndCells(layerCount, cellCount)
  this%LayerCount = layerCount
  this%CellCount = cellCount
  
  ! Count time steps and fill TotalTimes array
  if(allocated(this%TotalTimes)) deallocate(this%TotalTimes)
  allocate(this%TotalTimes(this%RecordCount))
  do n = 1, this%RecordCount
      this%TotalTimes(n) = 0.0d0
  end do
  timeStepCount = 0
  period = 0
  step = 0
  do n = 1, this%RecordCount
      if((this%RecordHeadersUsg(n)%StressPeriod .eq. period) .and. (this%RecordHeadersUsg(n)%TimeStep .eq. step)) cycle
      period = this%RecordHeadersUsg(n)%StressPeriod
      step = this%RecordHeadersUsg(n)%TimeStep
      timeStepCount = timeStepCount + 1
      this%TotalTimes(timeStepCount) = this%RecordHeadersUsg(n)%TotalTime
  end do
  this%TimeStepCount = timeStepCount
  
  return
  
100 continue
  call this%CloseFile()

  end subroutine pr_OpenFile

!---------------------------------------------------------
  subroutine pr_CloseFile(this)
  implicit none
  class(HeadReaderType) :: this
  logical :: opened
  
  if(this%FileUnit .ne. -1) then
      inquire(unit=this%FileUnit,opened=opened)
      if(opened) close(this%FileUnit)
  end if
  this%FileUnit = -1
  this%OutputUnit = 0
  this%PrecisionType = 0
  this%LayerCount = 0
  this%GridType = 0
  this%RecordCount = 0
  if(allocated(this%RecordHeadersUsg)) deallocate(this%RecordHeadersUsg)
  if(allocated(this%Filename)) deallocate(this%Filename)
  
  end subroutine pr_CloseFile

!---------------------------------------------------------
  subroutine pr_ProcessUnstructuredRecordHeaders(this, precisionType)
  implicit none
  class(HeadReaderType) :: this
  integer,intent(in) :: precisionType  
  integer(kind=8) :: position,fileSize
  character(len=16) :: text
  type(HeadRecordHeaderType) :: header
  integer :: n, recordCount
  
  ! Deallocate RecordHeaders array
  if(allocated(this%RecordHeadersUsg)) deallocate(this%RecordHeadersUsg)
  
  ! Find file size
  inquire(unit=this%FileUnit,size=fileSize)
  
  recordCount = 0
  position = 1
  do while (position .lt. fileSize)
      call this%ReadUnstructuredRecordHeader(position, header, precisionType)
      if(header%PrecisionType .ne. precisionType) goto 100
      recordCount = recordCount + 1
      position = header%GetNextHeaderPosition()
  end do
  
  ! Allocate header records array
  if(allocated(this%RecordHeadersUsg)) deallocate(this%RecordHeadersUsg)
  allocate(this%RecordHeadersUsg(recordCount))
  
  ! Read through file again to initialize and save the header records in the header records array
  position = 1
  do n = 1, recordCount
      call this%ReadUnstructuredRecordHeader(position, this%RecordHeadersUsg(n), precisionType)
      if(this%RecordHeadersUsg(n)%PrecisionType .eq. 0) goto 100
      position = this%RecordHeadersUsg(n)%GetNextHeaderPosition()
  end do
  
  this%RecordCount = recordCount
  this%Precisiontype = precisionType
  this%GridType = 2
  
  return

100 Continue
  this%GridType = 0
  this%PrecisionType = 0
  this%RecordCount = 0
  if(allocated(this%RecordHeadersUsg)) deallocate(this%RecordHeadersUsg)    
  end subroutine pr_ProcessUnstructuredRecordHeaders

!---------------------------------------------------------
  subroutine pr_ProcessStructuredRecordHeaders(this, precisionType)
  implicit none
  class(HeadReaderType) :: this
  integer,intent(in) :: precisionType  
  integer(kind=8) :: position,fileSize
  character(len=16) :: text
  type(HeadRecordHeaderType) :: header
  integer :: n, recordCount
  
  ! Deallocate RecordHeaders array
  if(allocated(this%RecordHeadersUsg)) deallocate(this%RecordHeadersUsg)
  
  ! Find file size
  inquire(unit=this%FileUnit,size=fileSize)
  
  recordCount = 0
  position = 1
  do while (position .lt. fileSize)
      call this%ReadStructuredRecordHeader(position, header, precisionType)
      if(header%PrecisionType .ne. precisionType) goto 100
      recordCount = recordCount + 1
      position = header%GetNextHeaderPosition()
  end do
  
  ! Allocate header records array
  if(allocated(this%RecordHeadersUsg)) deallocate(this%RecordHeadersUsg)
  allocate(this%RecordHeadersUsg(recordCount))
  
  ! Read through file again to initialize and save the header records in the header records array
  position = 1
  do n = 1, recordCount
      call this%ReadStructuredRecordHeader(position, this%RecordHeadersUsg(n), precisionType)
      if(this%RecordHeadersUsg(n)%PrecisionType .eq. 0) goto 100
      position = this%RecordHeadersUsg(n)%GetNextHeaderPosition()
  end do
  
  this%RecordCount = recordCount
  this%Precisiontype = precisionType
  this%GridType = 1
  
  return

100 Continue
  this%GridType = 0
  this%PrecisionType = 0
  this%RecordCount = 0
  if(allocated(this%RecordHeadersUsg)) deallocate(this%RecordHeadersUsg)    
  end subroutine pr_ProcessStructuredRecordHeaders

!---------------------------------------------------------
  subroutine pr_ReadUnstructuredRecordHeader(this, position, header, precisionType)
  class(HeadReaderType) :: this
  type(HeadRecordHeaderType),intent(inout) :: header
  integer(kind=8),intent(in) :: position
  integer,intent(in) :: precisionType
  integer(kind=8) :: headerOffset
  integer :: auxCount,n,realBytes,intBytes
  real(kind=4) :: stressPeriodLength, totalTime
  real(kind=8) :: stressPeriodLengthDbl, totalTimeDbl
  integer :: firstNonBlank, lastNonBlank, trimmedLength
  character(len=16) :: textLabel
  
  call header%Initialize()
  
  intBytes = 4
  if(precisionType .eq. 1) then
  realBytes = 4
  else if(precisionType .eq. 2) then
      realBytes = 8
  else
      return
  end if
  
  ! Set position
  read(this%FileUnit, pos=position, err=100)
  if(precisionType .eq. 1) then
      read(this%FileUnit, err=100) header%TimeStep, header%StressPeriod, stressPeriodLength, totalTime, textLabel, header%FirstCellNumber, header%LastCellNumber, header%Layer
      call TrimAll(textLabel, firstNonBlank, lastNonBlank, trimmedLength)
      if(firstNonBlank.eq.0 .or. lastNonBlank.eq.0) goto 100
      if(textLabel(firstNonBlank:lastNonBlank) .ne. 'HEADU') goto 100
      header%PrecisionType = 1
      header%StressPeriodLength = dble(stressPeriodLength)
      header%TotalTime = dble(totalTime)
      header%TextLabel = textLabel
  else if(precisionType .eq. 2) then
      read(this%FileUnit, err=100) header%TimeStep, header%StressPeriod, stressPeriodLengthDbl, totalTimeDbl, textLabel, header%FirstCellNumber, header%LastCellNumber, header%Layer
      call TrimAll(textLabel, firstNonBlank, lastNonBlank, trimmedLength)
      if(firstNonBlank.eq.0 .or. lastNonBlank.eq.0) goto 100
      if(textLabel(firstNonBlank:lastNonBlank) .ne. 'HEADU') goto 100
      header%PrecisionType = 2
      header%StressPeriodLength = stressPeriodLengthDbl
      header%TotalTime = totalTimeDbl
      header%TextLabel = textLabel
  else
      goto 100
  end if
  header%HeaderPosition = position
  header%HeaderOffset = 16 + (5 * intBytes) + 2 * realBytes
  header%DataOffset = (header%LastCellNumber - header%FirstCellNumber + 1) * realBytes
  
  return
  
100 Continue
  call header%Initialize() 
  
  end subroutine pr_ReadUnstructuredRecordHeader
  
!---------------------------------------------------------
  subroutine pr_ReadStructuredRecordHeader(this, position, header, precisionType)
  class(HeadReaderType) :: this
  type(HeadRecordHeaderType),intent(inout) :: header
  integer(kind=8),intent(in) :: position
  integer,intent(in) :: precisionType
  integer(kind=8) :: headerOffset
  integer :: auxCount,n,realBytes,intBytes,cellsPerLayer
  real(kind=4) :: stressPeriodLength, totalTime
  real(kind=8) :: stressPeriodLengthDbl, totalTimeDbl
  integer :: firstNonBlank, lastNonBlank, trimmedLength
  character(len=16) :: textLabel
  
  call header%Initialize()
  
  intBytes = 4
  if(precisionType .eq. 1) then
  realBytes = 4
  else if(precisionType .eq. 2) then
      realBytes = 8
  else
      return
  end if
  
  ! Set position
  read(this%FileUnit, pos=position, err=100)
  if(precisionType .eq. 1) then
      read(this%FileUnit, err=100) header%TimeStep, header%StressPeriod, stressPeriodLength, totalTime, textLabel, header%ColumnCount, header%RowCount, header%Layer
      call TrimAll(textLabel, firstNonBlank, lastNonBlank, trimmedLength)
      if(firstNonBlank.eq.0 .or. lastNonBlank.eq.0) goto 100
      if(textLabel(firstNonBlank:lastNonBlank) .ne. 'HEAD') goto 100
      header%PrecisionType = 1
      header%StressPeriodLength = dble(stressPeriodLength)
      header%TotalTime = dble(totalTime)
      header%TextLabel = textLabel
  else if(precisionType .eq. 2) then
      read(this%FileUnit, err=100) header%TimeStep, header%StressPeriod, stressPeriodLengthDbl, totalTimeDbl, textLabel, header%ColumnCount, header%RowCount, header%Layer
      call TrimAll(textLabel, firstNonBlank, lastNonBlank, trimmedLength)
      if(firstNonBlank.eq.0 .or. lastNonBlank.eq.0) goto 100
      if(textLabel(firstNonBlank:lastNonBlank) .ne. 'HEAD') goto 100
      header%PrecisionType = 2
      header%StressPeriodLength = stressPeriodLengthDbl
      header%TotalTime = totalTimeDbl
      header%TextLabel = textLabel
  else
      goto 100
  end if
  if((header%ColumnCount .lt. 1) .or. (header%RowCount .lt. 1)) goto 100
  cellsPerLayer = header%ColumnCount * header%RowCount
  header%HeaderPosition = position
  header%HeaderOffset = 16 + (5 * intBytes) + 2 * realBytes
  header%DataOffset = cellsPerLayer * realBytes
  header%FirstCellNumber = (header%Layer - 1)*cellsPerLayer + 1
  header%LastCellNumber = header%Layer * cellsPerLayer
  return
  
100 Continue
  call header%Initialize() 
  
  end subroutine pr_ReadStructuredRecordHeader
  
end module HeadReaderModule