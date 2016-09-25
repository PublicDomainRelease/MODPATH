module RectangularUnstructuredGridModule
  use UTL8MODULE
  !use RectangularCellDataModule,only : RectangularCellDataType
  use GridLocationModule,only : GridLocationType
  use ParticleLocationModule,only : ParticleLocationType
  use CoordinateModule,only : CoordinateType
  use UtilMiscModule,only : Sort
  
  implicit none
  
! Set default access status to private
  private

! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: RectangularUnstructuredGridType
    integer,private :: GridType = 0
    integer,private :: CellCount = 0
    integer,private :: LayerCount = 0
    integer,private :: RowCount = 0
    integer,private :: ColumnCount = 0
    integer,private :: PotentialConnectionCount = 0
    integer,private :: ReducedConnectionCount = 0
    logical,private :: HasClippedCells = .false.
    double precision,private,dimension(:),allocatable :: X
    double precision,private,dimension(:),allocatable :: Y
    double precision,private,dimension(:),allocatable :: Top
    double precision,private,dimension(:),allocatable :: SaturatedTop
    double precision,private,dimension(:),allocatable :: Bottom
    double precision,private,dimension(:),allocatable :: BaseDX
    double precision,private,dimension(:),allocatable :: BaseDY
    integer,private,dimension(:),allocatable :: Row
    integer,private,dimension(:),allocatable :: Column
    integer,private,dimension(:),allocatable :: Layer
    integer,private,dimension(:),allocatable :: RefinementLevel
    integer,private,dimension(:),allocatable :: LayerCellCount
    integer,private,dimension(:),allocatable :: LayerCellCountOffset
    integer,private,dimension(:),allocatable :: connections
    integer,private,dimension(:,:),allocatable :: offset
    integer,private,dimension(:,:),allocatable :: faceConnectionCount
    integer,private,dimension(:),allocatable :: ReducedCellConnectionCount
    integer,private,dimension(:),allocatable :: ReducedCellConnectionOffset
    integer,private,dimension(:),allocatable :: PotentialCellConnectionCount
    integer,private,dimension(:,:,:),allocatable :: BaseGridCellCounts
    integer,private,dimension(:,:,:),allocatable :: BaseGridFirstCellNumbers
    integer,dimension(:),allocatable :: ReducedConnections
  contains
    final :: pr_finalize
    procedure :: FillReducedCellConnectionsBuffer=>pr_FillReducedCellConnectionsBuffer
    procedure :: GetReducedConnectionCount=>pr_GetReducedConnectionCount
    procedure :: GetReducedCellConnectionCount=>pr_GetReducedCellConnectionCount
    procedure :: GetReducedCellConnectionOffset=>pr_GetReducedCellConnectionOffset
    procedure :: GetPotentialConnectionCount=>pr_GetPotentialConnectionCount
    procedure :: GetPotentialCellConnectionCount=>pr_GetPotentialCellConnectionCount
    procedure :: GetCellCount=>pr_GetCellCount
    procedure :: GetLayerCount=>pr_GetLayerCount
    procedure :: GetRowCount=>pr_GetRowCount
    procedure :: GetColumnCount=>pr_GetColumnCount
    procedure :: GetRefinementLevel=>pr_GetRefinementLevel
    procedure :: GetHasClippedCells=>pr_GetHasClippedCells
    procedure :: GetLayerCellCount=>pr_GetLayerCellCount
    procedure :: GetLayerCellCountOffset=>pr_GetLayerCellCountOffset
    procedure :: GetRow=>pr_GetRow
    procedure :: GetColumn=>pr_GetColumn
    procedure :: GetLayer=>pr_GetLayer
    procedure :: GetX=>pr_GetX
    procedure :: GetY=>pr_GetY
    procedure :: GetLeft=>pr_GetLeft
    procedure :: GetRight=>pr_GetRight
    procedure :: GetFront=>pr_GetFront
    procedure :: GetBack=>pr_GetBack
    procedure :: GetBottom=>pr_GetBottom
    procedure :: GetTop=>pr_GetTop
    procedure :: GetSaturatedTop=>pr_GetSaturatedTop
    procedure :: SetSaturatedTop=>pr_SetSaturatedTop
    procedure :: GetDX=>pr_GetDX
    procedure :: GetDY=>pr_GetDY
    procedure :: GetDZ=>pr_GetDZ
    procedure :: GetBaseDX=>pr_GetBaseDX
    procedure :: GetBaseDY=>pr_GetBaseDY
    procedure :: GetFaceConnectionCount=>pr_GetFaceConnectionCount
    procedure :: GetFaceConnection=>pr_GetFaceConnection
    procedure :: GetGridType=>pr_GetGridType
    procedure :: GetBaseGridCellCount=>pr_GetBaseGridCellCount
    procedure :: GetBaseGridFirstCellNumber=>pr_GetBaseGridFirstCellNumber
    procedure :: ConvertFromNeighbor=>pr_ConvertFromNeighbor
    procedure :: FindConnectionPosition=>pr_FindConnectionPosition
    procedure :: FindConnectionIndex=>pr_FindConnectionIndex
    procedure :: ConvertToGlobalXY=>pr_ConvertToGlobalXY
    procedure :: ConvertToLocalXY=>pr_ConvertToLocalXY
    procedure :: ConvertToGlobalXYZ=>pr_ConvertToGlobalXYZ
    procedure :: ConvertToGlobalZ=>pr_ConvertToGlobalZ
    procedure :: ReadFileType1=>pr_ReadFileType1
    procedure :: ReadFileType2=>pr_ReadFileType2
    procedure :: WriteModpathGridFile=>pr_WriteModpathGridFile
    
    !private methods
    procedure,private :: ComputeReducedCellConnCount=>pr_ComputeReducedCellConnCount
    procedure,private :: ComputePotentialCellConnCount=>pr_ComputePotentialCellConnCount
    procedure,private :: ComputeReducedCellConnections=>pr_ComputeReducedCellConnections
  end type

  contains
!
! module procedures
!------------------------------------------
  subroutine pr_finalize(grid)
  implicit none
  type(RectangularUnstructuredGridType) :: grid
  
! Add finalization code here
  
  end subroutine pr_finalize
  
!------------------------------------------
  subroutine pr_FillReducedCellConnectionsBuffer(this,cellNumber,buffer,bufferSize,reducedCellConnCount)
  implicit none
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber,bufferSize
  integer,intent(inout),dimension(bufferSize) :: buffer
  integer,intent(inout) :: reducedCellConnCount
  integer :: n,offset
  
  do n = 1, bufferSize
      buffer(n) = 0
  end do
  
  offset = this%ReducedCellConnectionOffset(cellNumber)
  reducedCellConnCount = this%ReducedCellConnectionCount(cellNumber)
  do n = 1, reducedCellConnCount
      buffer(n) = this%ReducedConnections(offset + n)
  end do
  
  end subroutine pr_FillReducedCellConnectionsBuffer

!------------------------------------------
  function pr_GetReducedCellConnectionOffset(this,cellNumber) result(count)
  implicit none
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber
  integer :: count
  
  count = this%ReducedCellConnectionOffset(cellNumber)
  
  end function
  
!------------------------------------------
  subroutine pr_ComputeReducedCellConnections(this,cellNumber,buffer,bufferSize,reducedConnectionCount)
  implicit none
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber,bufferSize
  integer,intent(inout),dimension(bufferSize) :: buffer
  integer,intent(inout) :: reducedConnectionCount
  integer :: n,face,conn,count,faceConnCount
  
  do n = 1, bufferSize
      buffer(n) = 0
  end do
  
  count = 1
  do face = 1, 6
      faceConnCount = this%GetFaceConnectionCount(cellNumber, face)
      do n = 1, faceConnCount
          conn = this%GetFaceConnection(cellNumber, face, n)
          if(conn .gt. 0) then
              count = count + 1
              buffer(count) = conn
          end if
      end do
  end do
  
  ! Check for error condition
  if(bufferSize .lt. count) then
      do n = 1, bufferSize
          buffer(n) = -1
      end do
      count = -1
  else
      ! Sort the connections if the data is valid
      call Sort(buffer, count) 
      buffer(1) = cellNumber
  end if
  
  reducedConnectionCount = count
  
  end subroutine pr_ComputeReducedCellConnections

!------------------------------------------
  function   pr_ComputeReducedCellConnCount(this,cellNumber) result(count)
  implicit none
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber
  integer :: count,face,conn,faceConnCount,n
  
  count = 1
  do face = 1, 6
      faceConnCount = this%GetFaceConnectionCount(cellNumber, face)
      do n = 1, faceConnCount
          conn = this%GetFaceConnection(cellNumber, face, n)
          if(conn .gt. 0) then
              count = count + 1
          end if
      end do
  end do
  
  end function pr_ComputeReducedCellConnCount

!------------------------------------------
  function   pr_ComputePotentialCellConnCount(this,cellNumber) result(count)
  implicit none
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber
  integer :: count,face
  
  count = 0
  do face = 1, 6
      count = count + this%GetFaceConnectionCount(cellNumber, face)
  end do
  
  end function pr_ComputePotentialCellConnCount

!------------------------------------------
  function pr_GetReducedCellConnectionCount(this,cellNumber) result(count)
  implicit none
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber
  integer :: count
  
  count = this%ReducedCellConnectionCount(cellNumber)
  
  end function pr_GetReducedCellConnectionCount

!------------------------------------------
  function pr_GetPotentialCellConnectionCount(this,cellNumber) result(count)
  implicit none
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber
  integer :: count
  
  count = this%PotentialCellConnectionCount(cellNumber)
  
  end function pr_GetPotentialCellConnectionCount
  
  
!----------------------------------------------------------------
! Read structured MODFLOW-2005 DIS file and create an equivalent 
! rectangular unstructured grid for internal use.
!----------------------------------------------------------------
  subroutine pr_ReadFileType1(this, iin, iout, stressPeriodCount)
  implicit none
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: iin, iout
  integer,intent(inout) :: stressPeriodCount
  character*20 :: controlrecordflag
  character (len=24) :: aname
  character (len=200) :: line
  integer :: cellNumber,i,j,k,n,m,layer,row,column,level,maxlevel,conn,nc,ptr,count,offset,istart,istop,ierr,lloc
  integer :: itmuni, lenuni, layerCellCount
  integer,dimension(7) :: reducedConnBuffer
  integer,dimension(:),allocatable :: laycbd
  doubleprecision:: dx,dy,top,bottom,constant,r,sumX,sumY,totalY
  doubleprecision,dimension(:,:),allocatable :: bufdbl2d
  
!   Deallocate arrays if they have been allocated previously  
    if(allocated(this%BaseDX)) then
      deallocate(this%BaseDX)
      deallocate(this%BaseDY)
      deallocate(this%X)
      deallocate(this%Y)
      deallocate(this%Top)
      deallocate(this%SaturatedTop)
      deallocate(this%Bottom)
      deallocate(this%Row)
      deallocate(this%Column)
      deallocate(this%Layer)
      deallocate(this%RefinementLevel)
      deallocate(this%LayerCellCount)
      deallocate(this%LayerCellCountOffset)
      deallocate(this%connections)
      deallocate(this%ReducedCellConnectionCount)
      deallocate(this%ReducedCellConnectionOffset)
      deallocate(this%PotentialCellConnectionCount)
      deallocate(this%BaseGridCellCounts)
      deallocate(this%BaseGridFirstCellNumbers)
    end if
    if(allocated(this%BaseGridCellCounts)) deallocate(this%BaseGridCellCounts)
    if(allocated(this%BaseGridFirstCellNumbers)) deallocate(this%BaseGridFirstCellNumbers)
    
!   Read and write comment lines. Return the first non-comment line in variable "line".
    call u8rdcom(iin,iout,line,ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 2, this%LayerCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, this%RowCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, this%ColumnCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, stressPeriodCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, itmuni, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, lenuni, r, iout, iin)
  
!   Compute the number of cells
    this%CellCount = this%LayerCount * this%RowCount * this%ColumnCount
    layerCellCount = this%RowCount * this%ColumnCount

!   Allocate and read confining bed layer flag array.
    allocate(laycbd(this%LayerCount))
    aname = "LAYCBD                  "
    call u1dint(laycbd,aname,this%LayerCount,iin,iout)
    
!   Check to be sure there are no quasi-3d confining beds    
    do n = 1, this%LayerCount
        if(laycbd(n) .ne. 0) then
            write(iout, '(1x,a)') 'Quasi-3d confining beds are not allowed. Stop.'
            stop
        end if
    end do
    
!   Allocate data pointers for arrays that depend only on grid dimensions
    allocate(this%BaseDX(this%ColumnCount))
    allocate(this%BaseDY(this%RowCount))
    allocate(this%X(this%CellCount))
    allocate(this%Y(this%CellCount))
    allocate(this%Top(this%CellCount))
    allocate(this%SaturatedTop(this%CellCount))
    allocate(this%Bottom(this%CellCount))
    allocate(this%Row(this%CellCount))
    allocate(this%Column(this%CellCount))
    allocate(this%Layer(this%CellCount))
    allocate(this%RefinementLevel(this%CellCount))
    allocate(this%LayerCellCount(this%LayerCount))
    allocate(this%LayerCellCountOffset(this%LayerCount))
    allocate(this%offset(6,this%CellCount))
    allocate(this%faceConnectionCount(6,this%CellCount))
    allocate(this%ReducedCellConnectionCount(this%CellCount))
    allocate(this%ReducedCellConnectionOffset(this%CellCount))
    allocate(this%PotentialCellConnectionCount(this%CellCount))
!   Read base grid spacing in the X direction (DELR, along a row)    
    aname = "BaseDX                  "
    call u1ddbl(this%BaseDX,aname,this%ColumnCount,iin,iout)
    
!   Read base grid spacing in the Y direction (DELC, along a column)
    aname = "BaseDY                  "
    call u1ddbl(this%BaseDY,aname,this%RowCount,iin,iout) 
    totalY = 0.0d0
    do n = 1, this%RowCount
        totalY = totalY + this%BaseDY(n)
    end do
    
!   Allocate double precision 2d buffer
    allocate(bufdbl2d(this%ColumnCount, this%RowCount))
    
!   Compute and assign layer cell count and layer cell count offset arrays
    do n = 1, this%LayerCount
        this%LayerCellCount(n) = layerCellCount
    end do
    
    this%LayerCellCountOffset(1) = 0
    do n = 2, this%LayerCount
        this%LayerCellCountOffset(n) = layerCellCount
    end do
    
!   Read top elevation for layer 1
    aname = "TOP ELEVATION"
    call u2drel(bufdbl2d, aname, this%RowCount, this%ColumnCount, 1, iin, iout)
    cellNumber = 0
    do row = 1, this%RowCount
        do column = 1, this%ColumnCount
            cellNumber = cellNumber + 1
            this%Top(cellNumber) = bufdbl2d(column, row)
            this%SaturatedTop(cellNumber) = this%Top(cellNumber)
        end do
    end do
    
!   Read bottom elevation for all layers
    cellNumber = 0
    aname = "BOTTOM ELEVATION"
    do n = 1, this%LayerCount
        call u2drel(bufdbl2d, aname, this%RowCount, this%ColumnCount, n, iin, iout)
        do row = 1, this%RowCount
            do column = 1, this%ColumnCount
                cellNumber = cellNumber + 1
                this%Bottom(cellNumber) = bufdbl2d(column, row)
            end do
        end do
    end do
    
!   Set top elevation for the rest of the layers
    do n = 1, this%CellCount - layerCellCount
        this%Top(n + layerCellCount) = this%Bottom(n)
        this%SaturatedTop(n + layerCellCount) = this%Top(n + layerCellCount)
    end do
    
!   Fill faceConnectionCount array    
    cellNumber = 0
    do layer = 1, this%LayerCount
        sumY = totalY - (this%BaseDY(1) / 2.0d0)
        do row = 1, this%RowCount
            if(row .gt. 1) sumY = sumY - ((this%BaseDY(row -1) + this%BaseDY(row)) / 2.0d0)
            sumX = this%BaseDX(1) / 2.0d0
            do column = 1, this%ColumnCount
                cellNumber = cellNumber + 1
                if(column .gt. 1) sumX = sumX +((this%BaseDX(column - 1) + this%BaseDX(column)) / 2.0d0)
                
                ! Build face connection counts
                ! Initially set the face connection count equal to 1 for all 6 faces
                do n = 1, 6
                    this%faceConnectionCount(n, cellNumber) = 1
                end do
                
                ! Next reset the connection count to 0 for faces that are at the edge of the grid
                if(layer .eq. 1) this%faceConnectionCount(6, cellNumber) = 0                
                if(layer .eq. this%LayerCount) this%faceConnectionCount(5, cellNumber) = 0                
                if(row .eq. 1) this%faceConnectionCount(4, cellNumber) = 0
                if(row .eq. this%RowCount) this%faceConnectionCount(3, cellNumber) = 0
                if(column .eq. 1) this%faceConnectionCount(1, cellNumber) = 0
                if(column .eq. this%ColumnCount) this%faceConnectionCount(2, cellNumber) = 0
                
                ! Compute and store the total potential cell connection count for the current cell
                m = 0
                do n = 1,6
                    if(this%faceConnectionCount(n, cellNumber) .eq. 1) m = m + 1
                end do
                this%PotentialCellConnectionCount(cellNumber) = m
                
                ! Store the rest of the cell data for the current cell
                this%Layer(cellNumber) = layer
                this%Row(cellNumber) = row
                this%Column(cellNumber) = column
                this%RefinementLevel(cellNumber) = 0
                this%X(cellNumber) = sumX
                this%Y(cellNumber) = sumY
                
            end do
        end do
    end do
    
!   Compute potential and reduced cell connection counts for the entire grid.
!   Note that the reduced connection count for a cell contains an extra element for the cell itself.
!   The potential connection count does not contain an extra element for the cell itself.
    this%ReducedConnectionCount = 0
    this%PotentialConnectionCount = 0
    do n = 1, this%CellCount
        ! Reduced connections
        this%ReducedCellConnectionCount(n) = this%PotentialCellConnectionCount(n) + 1
        this%ReducedConnectionCount = this%ReducedConnectionCount + this%ReducedCellConnectionCount(n)
        ! Potential connections
        this%PotentialConnectionCount = this%PotentialConnectionCount + this%PotentialCellConnectionCount(n)
    end do

!   Allocate and fill connections array
    allocate(this%connections(this%PotentialConnectionCount))
    nc = 0
    this%HasClippedCells = .false.
    do n = 1, this%CellCount
        if(this%faceConnectionCount(1, n) .eq. 1) then
            nc = nc + 1
            this%connections(nc) = n - 1
        end if
        if(this%faceConnectionCount(2, n) .eq. 1) then
            nc = nc + 1
            this%connections(nc) = n + 1
        end if
        if(this%faceConnectionCount(3, n) .eq. 1) then
            nc = nc + 1
            this%connections(nc) = n + this%ColumnCount
        end if
         if(this%faceConnectionCount(4, n) .eq. 1) then
            nc = nc + 1
            this%connections(nc) = n - this%ColumnCount
        end if
        if(this%faceConnectionCount(5, n) .eq. 1) then
            nc = nc + 1
            this%connections(nc) = n + layerCellCount
        end if
         if(this%faceConnectionCount(6, n) .eq. 1) then
            nc = nc + 1
            this%connections(nc) = n - layerCellCount
        end if
    end do
    
!   Compute connection offset values
    ptr = 0
    do n = 1, this%CellCount
      do i = 1, 6
        if(this%faceConnectionCount(i,n) .gt. 0) then
          this%offset(i,n) = ptr
          ptr = ptr + this%faceConnectionCount(i,n)
        else
          this%offset(i,n) = -1
        end if
      end do
    end do
    
!   Compute the reduced cell connection offsets. 
    this%ReducedCellConnectionOffset(1) = 0
    do n = 2, this%CellCount
         this%ReducedCellConnectionOffset(n) = this%ReducedCellConnectionOffset(n-1) + this%ReducedCellConnectionCount(n-1)
    end do
    
!   Allocate and fill the reduced connections array for the grid.
    allocate(this%ReducedConnections(this%ReducedConnectionCount))
    do n = 1, this%CellCount
         call this%ComputeReducedCellConnections(n, reducedConnBuffer, 17, count)
         offset = this%ReducedCellConnectionOffset(n)
         do m = 1, this%ReducedCellConnectionCount(n)
              this%ReducedConnections(offset + m) = reducedConnBuffer(m)
         end do
    end do

!   Set grid type flag
    this%GridType = 1
    
!   Write the MPUGRID file for debugging purposes
!   Remove the comments from the following lines 
    !open(unit=100,file='dis_2005.mpugrid', status='replace', form='formatted', access='sequential')
    !call this%WriteModpathGridFile(100)
    !close(100)
    
!   Write the reduced connections list for debugging purposes
    !open(unit=100,file='reduced_connections.dat', status='replace', form='formatted', access='sequential')
    !do n = 1, this%CellCount
    !     offset = this%ReducedCellConnectionOffset(n)
    !     write(100, '(i10,a,i10,a,17i10)') n, ', offset = ', offset, ': ', (this%ReducedConnections(offset + m), m = 1, this%ReducedCellConnectionCount(n))        
    !end do
    !close(100)
    
  end subroutine pr_ReadFileType1
    
  subroutine pr_ReadFileType2(this, iin, iout)
  implicit none
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: iin, iout
  character*20 :: controlrecordflag
  character (len=24) :: aname
  character (len=200) :: line
  double precision:: dx,dy,top,bottom,constant,r
  integer :: cellNumber,i,j,k,n,m,layer,row,column,level,maxlevel,conn,nc,ptr,count,offset,istart,istop,ierr,lloc
  integer,dimension(:),allocatable :: buf1
  integer,dimension(:),allocatable :: buf2
  integer,dimension(:),allocatable :: buf3
  integer,dimension(:),allocatable :: buf4
  integer,dimension(:),allocatable :: buf5
  integer,dimension(:),allocatable :: buf6
  integer,dimension(17) :: reducedConnBuffer
  
!   Deallocate arrays if they have been allocated previously  
    if(allocated(this%BaseDX)) then
      deallocate(this%BaseDX)
      deallocate(this%BaseDY)
      deallocate(this%X)
      deallocate(this%Y)
      deallocate(this%Top)
      deallocate(this%SaturatedTop)
      deallocate(this%Bottom)
      deallocate(this%Row)
      deallocate(this%Column)
      deallocate(this%Layer)
      deallocate(this%RefinementLevel)
      deallocate(this%LayerCellCount)
      deallocate(this%LayerCellCountOffset)
      deallocate(this%connections)
      deallocate(this%ReducedCellConnectionCount)
      deallocate(this%ReducedCellConnectionOffset)
      deallocate(this%PotentialCellConnectionCount)
    end if
    if(allocated(this%BaseGridCellCounts)) deallocate(this%BaseGridCellCounts)
    if(allocated(this%BaseGridFirstCellNumbers)) deallocate(this%BaseGridFirstCellNumbers)
    
!   Read and write comment lines. Return the first non-comment line in variable "line".
    call u8rdcom(iin,iout,line,ierr)
    lloc = 1
    call urword(line, lloc, istart, istop, 2, this%CellCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, this%LayerCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, this%RowCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, this%ColumnCount, r, iout, iin)
    call urword(line, lloc, istart, istop, 2, this%PotentialConnectionCount, r, iout, iin)
    
    write(iout, '(a,i10)') 'Cell count = ', this%CellCount
    write(iout, '(a,i5)')  'Layer count = ', this%LayerCount
    write(iout, '(a,i5)')  'Base grid row count = ', this%RowCount
    write(iout, '(a,i5)')  'Base grid column count = ', this%ColumnCount
    write(iout, '(a,i10)') 'Potential connection Count = ', this%PotentialConnectionCount
    
!   Allocate data pointers    
    allocate(this%BaseDX(this%ColumnCount))
    allocate(this%BaseDY(this%RowCount))
    allocate(this%X(this%CellCount))
    allocate(this%Y(this%CellCount))
    allocate(this%Top(this%CellCount))
    allocate(this%SaturatedTop(this%CellCount))
    allocate(this%Bottom(this%CellCount))
    allocate(this%Row(this%CellCount))
    allocate(this%Column(this%CellCount))
    allocate(this%Layer(this%CellCount))
    allocate(this%RefinementLevel(this%CellCount))
    allocate(this%LayerCellCount(this%LayerCount))
    allocate(this%LayerCellCountOffset(this%LayerCount))
    allocate(this%connections(this%PotentialConnectionCount))
    allocate(this%offset(6,this%CellCount))
    allocate(this%faceConnectionCount(6,this%CellCount))
    allocate(this%ReducedCellConnectionCount(this%CellCount))
    allocate(this%ReducedCellConnectionOffset(this%CellCount))
    allocate(this%PotentialCellConnectionCount(this%CellCount))
    allocate(this%BaseGridCellCounts(this%RowCount, this%ColumnCount, this%LayerCount))
    allocate(this%BaseGridFirstCellNumbers(this%RowCount, this%ColumnCount, this%LayerCount))
    
!   Read base grid spacing in the X direction (DELR, along a row)    
    aname = "BaseDX                  "
    call u1ddbl(this%BaseDX,aname,this%ColumnCount,iin,iout)
    
!   Read base grid spacing in the Y direction (DELC, along a column)
    aname = "BaseDY                  "
    call u1ddbl(this%BaseDY,aname,this%RowCount,iin,iout)    

!   Read basic cell data  
    maxlevel = 0
    do n = 1, this%LayerCount
        this%LayerCellCount(n) = 0
    end do
    do n = 1, this%CellCount
      read(iin, *) cellNumber, layer, this%Row(n), this%Column(n), this%RefinementLevel(n), this%X(n), this%Y(n), this%Bottom(n), this%Top(n)
      if(this%RefinementLevel(n) .gt. maxlevel) maxlevel = this%RefinementLevel(n)
      this%Layer(n) = layer
      this%LayerCellCount(layer) = this%LayerCellCount(layer) + 1
    end do 
    this%LayerCellCountOffset(1) = 0
    do n = 2, this%LayerCount
        this%LayerCellCountOffset(n) = this%LayerCellCountOffset(n-1) + this%LayerCellCount(n-1)
    end do
    
    write(iout, '(1x/a)') 'Cells per model layer:'
    do n = 1, this%LayerCount
        write(iout, '(a,i4,a,i10)') '   Layer ', n, ' cell count = ', this%LayerCellCount(n)
    
    end do
    
!   Allocate face connection buffers
    allocate(buf1(2**maxlevel))
    allocate(buf2(2**maxlevel))
    allocate(buf3(2**maxlevel))
    allocate(buf4(2**maxlevel))
    allocate(buf5(2**(maxlevel*2)))
    allocate(buf6(2**(maxlevel*2)))
    
!   Read cell connection data
    nc = 0
    this%HasClippedCells = .false.
    do n = 1, this%CellCount
      read(iin, *) cellNumber,(this%faceConnectionCount(i,n), i=1,6),(buf1(i), i=1,this%faceConnectionCount(1,n)),(buf2(i), i=1,this%faceConnectionCount(2,n)),(buf3(i), i=1,this%faceConnectionCount(3,n)),(buf4(i), i=1,this%faceConnectionCount(4,n)),(buf5(i), i=1,this%faceConnectionCount(5,n)),(buf6(i), i=1,this%faceConnectionCount(6,n))
      
      do i = 1,this%faceConnectionCount(1,n)
        nc = nc +1
        this%connections(nc) = buf1(i)
        if(this%connections(nc) .eq. 0) this%HasClippedCells = .true.
      end do
      
      do i = 1,this%faceConnectionCount(2,n)
        nc = nc +1
        this%connections(nc) = buf2(i)
        if(this%connections(nc) .eq. 0) this%HasClippedCells = .true.
      end do
      
      do i = 1,this%faceConnectionCount(3,n)
        nc = nc +1
        this%connections(nc) = buf3(i)
        if(this%connections(nc) .eq. 0) this%HasClippedCells = .true.
      end do
      
      do i = 1,this%faceConnectionCount(4,n)
        nc = nc +1
        this%connections(nc) = buf4(i)
        if(this%connections(nc) .eq. 0) this%HasClippedCells = .true.
      end do
      
      do i = 1,this%faceConnectionCount(5,n)
        nc = nc +1
        this%connections(nc) = buf5(i)
        if(this%connections(nc) .eq. 0) this%HasClippedCells = .true.
      end do
      
      do i = 1,this%faceConnectionCount(6,n)
        nc = nc +1
        this%connections(nc) = buf6(i)
        if(this%connections(nc) .eq. 0) this%HasClippedCells = .true.
      end do
    end do
    
!   Compute connection offset values
    ptr = 0
    do n = 1, this%CellCount
      do i = 1, 6
        if(this%faceConnectionCount(i,n) .gt. 0) then
          this%offset(i,n) = ptr
          ptr = ptr + this%faceConnectionCount(i,n)
        else
          this%offset(i,n) = -1
        end if
      end do
    end do
    
!   Compute potential and reduced cell connection counts for each cell and accumulate the sum totals
    this%ReducedConnectionCount = 0
    nc = 0
    do n = 1, this%CellCount
        ! Reduced connections
        this%ReducedCellConnectionCount(n) = this%ComputeReducedCellConnCount(n)
        this%ReducedConnectionCount = this%ReducedConnectionCount + this%ReducedCellConnectionCount(n)
        ! Potential connections
        this%PotentialCellConnectionCount(n) = this%ComputePotentialCellConnCount(n)
        nc = nc + this%PotentialCellConnectionCount(n)
    end do
    
!   Compare the potential connection count value read from the file with the computed count value
!   obtained by summing the potential connection count values read in for each cell.
    if(this%PotentialConnectionCount .ne. nc) then
        write(iout, '(1x,A)') 'Potential connection count read from the data file does not match computed count value.'
        write(iout, '(1x,A,i10)') 'Input count value = ', this%PotentialConnectionCount
        write(iout, '(1x,A,i10)') 'Computed count value = ', nc
        write(iout, '(1x,A)') 'Stop.'
        stop
    end if
    
!   Compute the reduced cell connection offsets
    this%ReducedCellConnectionOffset(1) = 0
    do n = 2, this%CellCount
         this%ReducedCellConnectionOffset(n) = this%ReducedCellConnectionOffset(n-1) + this%ReducedCellConnectionCount(n-1)
    end do
    
!   Allocate and fill the reduced connections array for the grid
    allocate(this%ReducedConnections(this%ReducedConnectionCount))
    do n = 1, this%CellCount
         call this%ComputeReducedCellConnections(n, reducedConnBuffer, 17, count)
         offset = this%ReducedCellConnectionOffset(n)
         do m = 1, this%ReducedCellConnectionCount(n)
              this%ReducedConnections(offset + m) = reducedConnBuffer(m)
         end do
    end do

!   Initialize base grid cell counts and first cell numbers
    do layer = 1, this%LayerCount
        do row = 1, this%RowCount
            do column = 1, this%ColumnCount
                this%BaseGridCellCounts(row, column, layer) = 0
                this%BaseGridFirstCellNumbers(row, column, layer) = 0
            end do
        end do
    end do

!   Set base grid cell counts and first cell numbers
    do n = 1, this%CellCount
        layer = this%Layer(n)
        row = this%Row(n)
        column = this%Column(n)
        this%BaseGridCellCounts(row, column, layer) = this%BaseGridCellCounts(row, column, layer) + 1
        if((this%BaseGridFirstCellNumbers(row, column, layer) .eq. 0) .or. (n .lt. this%BaseGridFirstCellNumbers(row, column, layer)))  then
            this%BaseGridFirstCellNumbers(row, column, layer) = n            
        end if
    end do
    
!   Set grid type flag
    this%GridType = 2
  
  end subroutine pr_ReadFileType2
  
  subroutine pr_WriteModpathGridFile(this, outUnit)
  implicit none
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: outUnit
  integer :: n,face,i
  
  write(outUnit, '(5i10)') this%CellCount, this%LayerCount, this%RowCount, this%ColumnCount, this%PotentialConnectionCount
  
  write(outUnit, '(a)') 'INTERNAL  1  (FREE)  0'
  write(outUnit, '(10(e15.7,1x))') (this%BaseDX(n), n = 1, this%ColumnCount)
  
  write(outUnit, '(a)') 'INTERNAL  1  (FREE)  0'
  write(outUnit, '(10(e15.7,1x))') (this%BaseDY(n), n = 1, this%RowCount)
  
  do n = 1, this%CellCount
      write(outUnit, '(i10,3(i5),i3,4(e22.14,1x))') n, this%Layer(n), this%Row(n), this%Column(n), this%RefinementLevel(n), this%X(n), this%Y(n), this%Bottom(n), this%Top(n)
  end do
  
  do n = 1, this%CellCount
      write(outUnit, '(i10, 6(i4,1x), 100(i10) )') n, (this%GetFaceConnectionCount(n, face), face = 1, 6), ( (this%GetFaceConnection(n, face, i), i = 1, this%GetFaceConnectionCount(n, face)), face = 1, 6)
  end do
  
  
  end subroutine pr_WriteModpathGridFile
  
!------------------------------------------
  function pr_GetGridType(this) result(fval)
    class(RectangularUnstructuredGridType) :: this
    integer :: fval
    fval = this%GridType
  end function pr_GetGridType
  
!------------------------------------------
  function pr_GetCellCount(this) result(fval)
    class(RectangularUnstructuredGridType) :: this
    integer :: fval
    fval = this%CellCount
  end function pr_GetCellCount
  
!------------------------------------------
  function pr_GetLayerCount(this) result(fval)
    class(RectangularUnstructuredGridType) :: this
    integer :: fval
    fval = this%LayerCount
  end function pr_GetLayerCount
  
!------------------------------------------
  function pr_GetRowCount(this) result(fval)
    class(RectangularUnstructuredGridType) :: this
    integer :: fval
    fval = this%RowCount
  end function pr_GetRowCount
  
!------------------------------------------
  function pr_GetColumnCount(this) result(fval)
    class(RectangularUnstructuredGridType) :: this
    integer :: fval
    fval = this%ColumnCount
  end function pr_GetColumnCount
  
!------------------------------------------
  function pr_GetHasClippedCells(this) result(fval)
    class(RectangularUnstructuredGridType) :: this
    logical :: fval
    fval = this%HasClippedCells
  end function pr_GetHasClippedCells

!------------------------------------------
  function pr_GetRefinementLevel(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: cellNumber, fval
  
  fval = this%RefinementLevel(cellNumber)
  
  end function pr_GetRefinementLevel
  
!------------------------------------------
  function pr_GetRow(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: cellNumber, fval
  
  fval = this%Row(cellNumber)
  
  end function pr_GetRow

!------------------------------------------
  function pr_GetColumn(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: cellNumber, fval
  
  fval = this%Column(cellNumber)
  
  end function pr_GetColumn
  
!------------------------------------------
  function pr_GetLayer(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: cellNumber, fval
  
  fval = this%Layer(cellNumber)
  
  end function pr_GetLayer

!------------------------------------------
  function pr_GetX(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: cellNumber
  double precision :: fval
  
  fval = this%X(cellNumber)
  
  end function pr_GetX

!------------------------------------------
  function pr_GetY(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: cellNumber
  double precision :: fval
  
  fval = this%Y(cellNumber)
  
  end function pr_GetY

!------------------------------------------
  function pr_GetTop(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: cellNumber
  double precision :: fval
  
  fval = this%Top(cellNumber)
  
  end function pr_GetTop

!------------------------------------------
  function pr_GetSaturatedTop(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: cellNumber
  double precision :: fval
  
  fval = this%SaturatedTop(cellNumber)
  
  end function pr_GetSaturatedTop

!------------------------------------------
  subroutine pr_SetSaturatedTop(this, cellNumber, saturatedTop)
  class(RectangularUnstructuredGridType) :: this
  integer :: cellNumber
  double precision :: saturatedTop
  
  this%SaturatedTop(cellNumber) = saturatedTop
  
  end subroutine pr_SetSaturatedTop

!------------------------------------------
  function pr_GetBottom(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: cellNumber
  double precision :: fval
  
  fval = this%Bottom(cellNumber)
  
  end function pr_GetBottom

!------------------------------------------
  function pr_GetLeft(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: cellNumber
  double precision :: fval
  
  fval = this%GetX(cellNumber) - (this%GetDX(cellNumber) / 2.0d0)
  
  end function pr_GetLeft

!------------------------------------------
  function pr_GetRight(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: cellNumber
  double precision :: fval
  
  fval = this%GetX(cellNumber) + (this%GetDX(cellNumber) / 2.0d0)
  
  end function pr_GetRight

!------------------------------------------
  function pr_GetFront(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: cellNumber
  double precision :: fval
  
  fval = this%GetY(cellNumber) - (this%GetDY(cellNumber) / 2.0d0)
  
  end function pr_GetFront

!------------------------------------------
  function pr_GetBack(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: cellNumber
  double precision :: fval
  
  fval = this%GetY(cellNumber) + (this%GetDY(cellNumber) / 2.0d0)
  
  end function pr_GetBack

!------------------------------------------
  function pr_GetBaseGridCellCount(this, layer, row, column) result(count)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: layer, row, column
  integer :: count
  
  count = 0
  if(this%GridType .eq. 1) then
      count = 1
  else if(this%GridType .eq. 2) then
      count = this%BaseGridCellCounts(row, column, layer)
  end if
  
  end function pr_GetBaseGridCellCount

!------------------------------------------
  function pr_GetBaseGridFirstCellNumber(this, layer, row, column) result(number)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: layer, row, column
  integer :: number
  
  number = 0
  if(this%GridType .eq. 1) then
      number = (layer-1)*this%RowCount*this%ColumnCount + (row-1)*this%ColumnCount + column
  else if(this%GridType .eq. 2) then
      number = this%BaseGridFirstCellNumbers(row, column, layer)
  end if
  
  end function pr_GetBaseGridFirstCellNumber
  
!------------------------------------------
  function pr_GetBaseDX(this, column) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: column
  double precision :: fval
  
  fval = this%BaseDX(column)
  
  end function pr_GetBaseDX
  
!------------------------------------------
  function pr_GetBaseDY(this, row) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer :: row
  double precision :: fval
  
  fval = this%BaseDY(row)
  
  end function pr_GetBaseDY

!------------------------------------------
  function pr_GetDX(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber
  double precision :: fval
  
  fval = this%BaseDX(this%Column(cellNumber)) / (2**this%RefinementLevel(cellNumber))
  
  end function pr_GetDX

!------------------------------------------
  function pr_GetDY(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber
  double precision :: fval
  
  fval = this%BaseDY(this%Row(cellNumber)) / (2**this%RefinementLevel(cellNumber))
  
  end function pr_GetDY

!------------------------------------------
  function pr_GetDZ(this, cellNumber) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber
  double precision :: fval
  
  fval = this%Top(cellNumber) - this%Bottom(cellNumber)
  
  end function pr_GetDZ

!------------------------------------------
  function pr_GetLayerCellCount(this, layer) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: layer
  double precision :: fval
  
  fval = this%LayerCellCount(layer)
  
  end function pr_GetLayerCellCount

!------------------------------------------
  function pr_GetLayerCellCountOffset(this, layer) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: layer
  double precision :: fval
  
  fval = this%LayerCellCountOffset(layer)
  
  end function pr_GetLayerCellCountOffset

!------------------------------------------
  function pr_GetPotentialConnectionCount(this) result(count)
  class(RectangularUnstructuredGridType) :: this
  integer :: count
  
  count = this%PotentialConnectionCount
  
  end function pr_GetPotentialConnectionCount

!------------------------------------------
  function pr_GetReducedConnectionCount(this) result(count)
  class(RectangularUnstructuredGridType) :: this
  integer :: count,n
  
  count = this%ReducedConnectionCount
  
  end function pr_GetReducedConnectionCount

!------------------------------------------
  function pr_GetFaceConnectionCount(this, cellNumber, face) result(fval)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber,face
  integer :: fval

  fval = this%faceConnectionCount(face, cellNumber)
  
  end function pr_GetFaceConnectionCount
  
!------------------------------------------
  function pr_GetFaceConnection(this, cellNumber, face, subface) result(conn)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber,face,subface
  integer :: conn,offset

  conn = -1
  offset = this%offset(face,cellNumber)
  if((subface .gt. 0) .and. (subface .le. this%faceConnectionCount(face,cellNumber))) then
    conn = this%connections(offset + subface)
  end if
  
  end function pr_GetFaceConnection
  
!------------------------------------------
  subroutine pr_ConvertFromNeighbor(this, toCellNumber, fromCellNumber, fromLocalX, fromLocalY, fromLocalZ, newLocation)
  implicit none
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: toCellNumber,fromCellNumber
  doubleprecision,intent(in) :: fromLocalX,fromLocalY,fromLocalZ
  class(GridLocationType),intent(inout) :: newLocation
  integer :: status,faceNumber,subFaceNumber
  doubleprecision :: localX,localY,localZ,globalX,globalY,tol
  
  newLocation%CellNumber = 0
  newLocation%Layer = 0
  newLocation%LocalX = 0.0d0
  newLocation%LocalY = 0.0d0
  newLocation%LocalZ = 0.0d0
  
  status = 0
  
  ! Check to make sure the cell connection specified by fromCellNumber is actually a connection for
  ! the cell specified by toCellNumber
  call this%FindConnectionPosition(toCellNumber, fromCellNumber, faceNumber, subFaceNumber)
  
  select case (faceNumber)
    case (0)
      status = 1
    case (1)
      if(fromLocalX .ne. 1.0d0) status = 2
    case (2)
      if(fromLocalX .ne. 0d0) status = 2
    case (3)
      if(fromLocalY .ne. 1.0d0) status = 2
    case (4)
      if(fromLocalY .ne. 0d0) status = 2
    case (5)
      if(fromLocalZ .ne. 1.0d0) status = 2
    case (6)
      if(fromLocalZ .ne. 0d0) status = 2
    case default
      status = 3
  end select
  
  if(status .gt. 0) return
  
  newLocation%CellNumber = toCellNumber
  if(this%RefinementLevel(fromCellNumber) .eq. this%RefinementLevel(toCellNumber)) then
      newLocation%LocalX = fromLocalX
      newLocation%LocalY = fromLocalY
      newLocation%LocalZ = fromLocalZ
      select case (faceNumber)
        case (1)
            newLocation%LocalX = 0.0d0
        case (2)
            newLocation%LocalX = 1.0d0
        case (3)
            newLocation%LocalY = 0.0d0
        case (4)
            newLocation%LocalY = 1.0d0
        case (5)
            newLocation%LocalZ = 0.0d0
        case (6)
            newLocation%LocalZ = 1.0d0
      end select
  else
      status = this%ConvertToGlobalXY(fromCellNumber, fromLocalX, fromLocalY, globalX, globalY)
      if(status .gt. 0) return
      status = this%ConvertToLocalXY(toCellNumber, globalX, globalY, localX, localY)
      if(status .gt. 0) return
      
      tol = 1.0d-3
      select case (faceNumber)
        case (1)
            call pr_SnapToValue(localX, 0d0, tol)
        case (2)
            call pr_SnapToValue(localX, 1.0d0, tol)
        case (3)
            call pr_SnapToValue(localY, 0d0, tol)
        case (4)
            call pr_SnapToValue(localY, 1.0d0, tol)
      end select
      
      newLocation%CellNumber = toCellNumber
      newLocation%LocalX = localX
      newLocation%LocalY = localY
      localZ = fromLocalZ
      if(faceNumber .eq. 5) localZ = 0.0d0
      if(faceNumber .eq. 6) localZ = 1.0d0
      newLocation%LocalZ = localZ
  end if
  
  newLocation%Layer = this%GetLayer(newLocation%CellNumber)
  
  end subroutine pr_ConvertFromNeighbor
  
!------------------------------------------
  subroutine pr_SnapToValue(dataValue, snapValue, tolerance)
  implicit none
  doubleprecision,intent(inout) :: dataValue
  doubleprecision,intent(in) :: tolerance,snapValue
  doubleprecision :: min, max
  
  min = snapValue - (tolerance / 2.0d0)
  max = snapValue + (tolerance / 2.0d0)
  if( (dataValue .ge. min) .and. (dataValue .le. max) ) dataValue = snapValue
  
  end subroutine pr_SnapToValue

!------------------------------------------
  subroutine pr_FindConnectionPosition(this, cellNumber, conn, faceNumber, subFaceNumber)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber,conn
  integer,intent(inout) :: faceNumber,subFaceNumber
  integer :: count,n,face
  
  faceNumber = 0
  subFaceNumber = 0
  
  do face = 1, 6
      count = this%GetFaceConnectionCount(cellNumber, face)
      if(count .gt. 0) then
        do n = 1, count
          if(this%GetFaceConnection(cellNumber, face, n) .eq. conn) then
            faceNumber = face
            subFaceNumber = n
            return
          end if
        end do
      end if
  end do
  
  end subroutine pr_FindConnectionPosition

!------------------------------------------
  function pr_FindConnectionIndex(this, cellNumber, conn) result(index)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber,conn
  integer :: count, n, cn, index, offset
  
  index = 0
  offset = this%ReducedCellConnectionOffset(cellNumber)
  count = this%ReducedCellConnectionCount(cellNumber)
  do n = 1, count
       cn = this%ReducedConnections(offset + n)
       if(conn .eq. cn) then
            index = offset + n
            return
       end if
  end do
  
  end function pr_FindConnectionIndex
!------------------------------------------
  subroutine pr_ConvertToGlobalZ(this, cellNumber, localZ, globalZ, useSaturatedTop)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber
  doubleprecision,intent(in) :: localZ
  doubleprecision,intent(inout) :: globalZ
  logical,intent(in) :: useSaturatedTop
  doubleprecision :: bottom,top
  
  bottom = this%GetBottom(cellNumber)
  if(useSaturatedTop) then
      top = this%GetSaturatedTop(cellNumber)
  else
      top = this%GetTop(cellNumber)      
  end if
  
  globalZ = ((1.0d0 - localZ) * bottom) + (localZ * top)
  if(globalZ .lt. bottom) globalZ = bottom
  if(globalZ .gt. top) globalZ = top
  
  end subroutine pr_ConvertToGlobalZ

!------------------------------------------
  subroutine pr_ConvertToGlobalXYZ(this, cellNumber, localX, localY, localZ, globalX, globalY, globalZ)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber
  doubleprecision,intent(in) :: localX,localY,localZ
  doubleprecision,intent(inout) :: globalX,globalY,globalZ
  doubleprecision :: left,right,front,back,bottom,top
  
  left = this%GetLeft(cellNumber)
  right = this%GetRight(cellNumber)
  front = this%GetFront(cellNumber)
  back = this%GetBack(cellNumber)
  bottom = this%GetBottom(cellNumber)
  top = this%GetSaturatedTop(cellNumber)
  
  globalX = ((1.0d0 - localX) * left) + (localX * right)
  if(globalX .lt. left) globalX = left
  if(globalX .gt. right) globalX = right
  
  globalY = ((1.0d0 - localY) * front) + (localY * back)
  if(globalY .lt. front) globalY = front
  if(globalY .gt. back) globalY = back
  
  globalZ = ((1.0d0 - localZ) * bottom) + (localZ * top)
  if(globalZ .lt. bottom) globalZ = bottom
  if(globalZ .gt. top) globalZ = top
  
  end subroutine pr_ConvertToGlobalXYZ

!------------------------------------------
  function pr_ConvertToGlobalXY(this, cellNumber, localX, localY, globalX, globalY) result(status)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber
  doubleprecision,intent(in) :: localX,localY
  doubleprecision,intent(inout) :: globalX,globalY
  doubleprecision :: left,right,front,back
  integer :: status
  
  left = this%GetLeft(cellNumber)
  right = this%GetRight(cellNumber)
  front = this%GetFront(cellNumber)
  back = this%GetBack(cellNumber)
  
  globalX = ((1.0d0 - localX) * left) + (localX * right)
  if(globalX .lt. left) globalX = left
  if(globalX .gt. right) globalX = right
  
  globalY = ((1.0d0 - localY) * front) + (localY * back)
  if(globalY .lt. front) globalY = front
  if(globalY .gt. back) globalY = back

  status = 0
  
  end function pr_ConvertToGlobalXY

!------------------------------------------
! Method: 
!------------------------------------------
  function pr_ConvertToLocalXY(this, cellNumber, globalX, globalY, localX, localY) result(status)
  class(RectangularUnstructuredGridType) :: this
  integer,intent(in) :: cellNumber
  doubleprecision,intent(in) :: globalX,globalY
  doubleprecision,intent(inout) :: localX,localY
  doubleprecision :: left,right,front,back
  integer :: status
  
  left = this%GetLeft(cellNumber)
  right = this%GetRight(cellNumber)
  front = this%GetFront(cellNumber)
  back = this%GetBack(cellNumber)
  
  localX = (globalX - left) / (right - left)
  if(localX .gt. 1.0d0) localX = 1.0d0
  if(localX .lt. 0d0) localX = 0d0
  
  localY = (globalY - front) / (back - front)
  if(localY .gt. 1.0d0) localY = 1.0d0
  if(localY .lt. 0d0) localY = 0d0

  status = 0
  
  end function pr_ConvertToLocalXY
  
end module RectangularUnstructuredGridModule