module RectangularCellDataModule
  use CoordinateModule,only : CoordinateType
  implicit none
  
! Set default access status to private
  private

! Private data type declarations
  integer,private :: RectangularCellDataType_LastObjectID = 0
  integer,private,dimension(0:8) :: SubDivisions = [1,2,4,8,16,32,64,128,256]
  
  integer,public :: RectangularCellDataType_LogUnit = 0
  
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: RectangularCellDataType
    integer :: ObjectID = 0
    integer :: CellNumber = 0
    double precision :: CenterX = 0.0d0
    double precision :: CenterY = 0.0d0
    double precision :: CenterZ = 0.0d0
    double precision :: Left = 0.0d0
    double precision :: Right = 0.0d0
    double precision :: Front = 0.0d0
    double precision :: Back = 0.0d0
    double precision :: Top = 0.0d0
    double precision :: Bottom = 0.0d0
    double precision :: BaseDX = 0.0d0
    double precision :: BaseDY = 0.0d0
    double precision :: DX = 0.0d0
    double precision :: DY = 0.0d0
    double precision :: DZ = 0.0d0
    integer :: Row = 0
    integer :: Column = 0
    integer :: Layer = 0
    integer :: RefinementLevel = 0
    integer,private,dimension(:),allocatable :: ConnectionsFace1
    integer,private,dimension(:),allocatable :: ConnectionsFace2
    integer,private,dimension(:),allocatable :: ConnectionsFace3
    integer,private,dimension(:),allocatable :: ConnectionsFace4
    integer,private,dimension(:),allocatable :: ConnectionsFace5
    integer,private,dimension(:),allocatable :: ConnectionsFace6
  contains
    final :: RectangularCellDataType_finalize
    procedure :: SetConnections=>RectangularCellDataType_SetConnections
    procedure :: Reset=>RectangularCellDataType_Reset
    procedure :: GetFaceConnectionCount=>RectangularCellDataType_GetFaceConnectionCount
    procedure :: GetTotalConnectionCount=>RectangularCellDataType_GetTotalConnectionCount
    procedure :: GetConnection=>RectangularCellDataType_GetConnection
    procedure :: GetConnectionsCopy=>RectangularCellDataType_GetConnectionsCopy
    procedure :: GetReducedConnectionCount=>RectangularCellDataType_GetReducedConnectionCount
    procedure :: FindConnectionPosition=>RectangularCellDataType_FindConnectionPosition
    procedure :: ConvertToGlobalCoordinate=>RectangularCellDataType_ConvertToGlobalCoordinate
    procedure :: ConvertToGlobalXY=>RectangularCellDataType_ConvertToGlobalXY
    procedure :: ConvertToLocalXY=>RectangularCellDataType_ConvertToLocalXY
  end type
  
! Public interface definitions
  public :: New_RectangularCellData
  interface New_RectangularCellData
    module procedure RectangularCellDataType_Create
  end interface New_RectangularCellData
  
contains
!!------------------------------------------
!  function RectangularCellDataType_GetConnectionsCopy(this) result(connections)
!  implicit none
!  class(RectangularCellDataType) :: this
!  integer,allocatable,dimension(:) :: connections
!  integer :: faceNumber, subFaceNumber, count, subFaceCount
!  
!  count = this%GetTotalConnectionCount()  
!  allocate(connections(count))
!  
!  count = 0
!  do faceNumber = 1, 6
!      subFaceCount = this%GetFaceConnectionCount(faceNumber)
!      do subFaceNumber = 1, subFaceCount
!          count = count + 1
!          connections(count) = this%GetConnection(faceNumber, subFaceNumber)
!      end do
!  end do
!  
!  end function RectangularCellDataType_GetConnectionsCopy
  
!------------------------------------------
  subroutine RectangularCellDataType_GetConnectionsCopy(this, connections)
  implicit none
  class(RectangularCellDataType) :: this
  integer,intent(inout),allocatable,dimension(:) :: connections
  integer :: faceNumber, subFaceNumber, count, subFaceCount
  
  count = this%GetTotalConnectionCount()  
  if(allocated(connections)) deallocate(connections)
  allocate(connections(count))
  
  count = 0
  do faceNumber = 1, 6
      subFaceCount = this%GetFaceConnectionCount(faceNumber)
      do subFaceNumber = 1, subFaceCount
          count = count + 1
          connections(count) = this%GetConnection(faceNumber, subFaceNumber)
      end do
  end do
  
  end subroutine RectangularCellDataType_GetConnectionsCopy
  
!------------------------------------------
  subroutine RectangularCellDataType_finalize(cellData)
    type(RectangularCellDataType) :: cellData
    if(RectangularCellDataType_LogUnit .gt. 0) then
      write(RectangularCellDataType_LogUnit, '(A,I10,A1)') "Destroying RectangularCellDatType object (ObjectID = ",cellData%ObjectID,")"
    end if
  end subroutine RectangularCellDataType_finalize
  
  function RectangularCellDataType_Create() result(cellData)
  implicit none
  type(RectangularCellDataType),allocatable,target :: cellData
  
  allocate(cellData)
  
  end function RectangularCellDataType_Create

!------------------------------------------
! subroutine: 
!------------------------------------------
  subroutine RectangularCellDataType_Reset(this)
  implicit none
  class(RectangularCellDataType) :: this
  integer :: n
  
    this%ObjectID = 0
    this%CellNumber = 0
    this%CenterX = 0.0d0
    this%CenterY = 0.0d0
    this%CenterZ = 0.0d0
    this%Left = 0.0d0
    this%Right = 0.0d0
    this%Front = 0.0d0
    this%Back = 0.0d0
    this%Top = 0.0d0
    this%Bottom = 0.0d0
    this%BaseDX = 0.0d0
    this%BaseDY = 0.0d0
    this%DX = 0.0d0
    this%DY = 0.0d0
    this%DZ = 0.0d0
    this%Row = 0
    this%Column = 0
    this%Layer = 0
    this%RefinementLevel = 0
    if(allocated(this%ConnectionsFace1)) deallocate(this%ConnectionsFace1)
    if(allocated(this%ConnectionsFace2)) deallocate(this%ConnectionsFace2)
    if(allocated(this%ConnectionsFace3)) deallocate(this%ConnectionsFace3)
    if(allocated(this%ConnectionsFace4)) deallocate(this%ConnectionsFace4)
    if(allocated(this%ConnectionsFace5)) deallocate(this%ConnectionsFace5)
    if(allocated(this%ConnectionsFace6)) deallocate(this%ConnectionsFace6)
    allocate(this%ConnectionsFace1(0))
    allocate(this%ConnectionsFace2(0))
    allocate(this%ConnectionsFace3(0))
    allocate(this%ConnectionsFace4(0))
    allocate(this%ConnectionsFace5(0))
    allocate(this%ConnectionsFace6(0))
 
  end subroutine RectangularCellDataType_Reset

!------------------------------------------
! subroutine: 
!------------------------------------------
  function RectangularCellDataType_GetConnection(this, faceNumber, subFaceNumber) result(fval)
  implicit none
  class(RectangularCellDataType) :: this
  integer,intent(in) :: faceNumber, subFaceNumber
  integer :: fval
  
  select case(faceNumber)
    case (1)
      fval = this%ConnectionsFace1(subFaceNumber)
    case (2)
      fval = this%ConnectionsFace2(subFaceNumber)    
    case (3)
      fval = this%ConnectionsFace3(subFaceNumber)   
    case (4)
      fval = this%ConnectionsFace4(subFaceNumber)  
    case (5)
      fval = this%ConnectionsFace5(subFaceNumber) 
    case (6)
      fval = this%ConnectionsFace6(subFaceNumber)    
    case default
      fval = 0
  end select
  
  end function RectangularCellDataType_GetConnection

!------------------------------------------
! subroutine: 
!------------------------------------------
  subroutine RectangularCellDataType_SetConnections(this, faceNumber, connections)
  implicit none
  class(RectangularCellDataType) :: this
  integer,intent(in) :: faceNumber
  integer,intent(in),dimension(:) :: connections
  integer count, n
  
  count = size(connections)
  
  select case (faceNumber)
    case (1)
      if(allocated(this%ConnectionsFace1)) deallocate(this%ConnectionsFace1)
      allocate(this%ConnectionsFace1(count))
      do n = 1, count
        this%ConnectionsFace1(n) = connections(n)
      end do
    case (2)
      if(allocated(this%ConnectionsFace2)) deallocate(this%ConnectionsFace2)
      allocate(this%ConnectionsFace2(count))
      do n = 1, count
        this%ConnectionsFace2(n) = connections(n)
      end do
    case (3)
      if(allocated(this%ConnectionsFace3)) deallocate(this%ConnectionsFace3)
      allocate(this%ConnectionsFace3(count))
      do n = 1, count
        this%ConnectionsFace3(n) = connections(n)
      end do
    case (4)
      if(allocated(this%ConnectionsFace4)) deallocate(this%ConnectionsFace4)
      allocate(this%ConnectionsFace4(count))
      do n = 1, count
        this%ConnectionsFace4(n) = connections(n)
      end do
    case (5)
      if(allocated(this%ConnectionsFace5)) deallocate(this%ConnectionsFace5)
      allocate(this%ConnectionsFace5(count))
      do n = 1, count
        this%ConnectionsFace5(n) = connections(n)
      end do
    case (6)
      if(allocated(this%ConnectionsFace6)) deallocate(this%ConnectionsFace6)
      allocate(this%ConnectionsFace6(count))
      do n = 1, count
        this%ConnectionsFace6(n) = connections(n)
      end do
  end select
  
  end subroutine RectangularCellDataType_SetConnections
  
!------------------------------------------
  function RectangularCellDataType_GetTotalConnectionCount(this) result(count)
  class(RectangularCellDataType) :: this
  integer :: count, n
  
  count = 0
  do n = 1, 6
      count = count + this%GetFaceConnectionCount(n)  
  end do

  end function RectangularCellDataType_GetTotalConnectionCount
  
!------------------------------------------
  function RectangularCellDataType_GetFaceConnectionCount(this,faceNumber) result(fval)
  class(RectangularCellDataType) :: this
  integer,intent(in) :: faceNumber
  integer :: fval
  
    fval = 0
    select case (faceNumber)
    case (1)
      if(allocated(this%ConnectionsFace1)) fval = size(this%ConnectionsFace1)
    case (2)
      if(allocated(this%ConnectionsFace2)) fval = size(this%ConnectionsFace2)
    case (3)
      if(allocated(this%ConnectionsFace3)) fval = size(this%ConnectionsFace3)
    case (4)
      if(allocated(this%ConnectionsFace4)) fval = size(this%ConnectionsFace4)
    case (5)
      if(allocated(this%ConnectionsFace5)) fval = size(this%ConnectionsFace5)
    case (6)
      if(allocated(this%ConnectionsFace6)) fval = size(this%ConnectionsFace6)
    end select
  
  end function RectangularCellDataType_GetFaceConnectionCount

  
!------------------------------------------
! function: 
!------------------------------------------
  function RectangularCellDataType_GetReducedConnectionCount(this) result(count)
  class(RectangularCellDataType) :: this
  integer :: n
  integer :: count

  count = 0
  do n = 1,6
    count = count + this%GetFaceConnectionCount(n)
  end do
  
  end function RectangularCellDataType_GetReducedConnectionCount  
!------------------------------------------
! function: 
!------------------------------------------
  function RectangularCellDataType_GetDX(this) result(fval)
  class(RectangularCellDataType) :: this
  double precision :: fval
  
  fval = this%BaseDX / (2**this%RefinementLevel)
  
  end function RectangularCellDataType_GetDX

  function RectangularCellDataType_GetDY(this) result(fval)
  class(RectangularCellDataType) :: this
  double precision :: fval
  
  fval = this%BaseDY / (2**this%RefinementLevel)
  
  end function RectangularCellDataType_GetDY

!------------------------------------------
! function: 
!------------------------------------------
  function RectangularCellDataType_GetDZ(this) result(fval)
  class(RectangularCellDataType) :: this
  double precision :: fval
  
  fval = this%Top - this%Bottom
  
  end function RectangularCellDataType_GetDZ

!------------------------------------------
! Method: 
!------------------------------------------
  function RectangularCellDataType_ConvertToGlobalCoordinate(this, localX, localY, localZ, globalCoordinate) result(status)
  class(RectangularCellDataType) :: this
  doubleprecision,intent(in) :: localX,localY,localZ
  type(CoordinateType),intent(inout) :: globalCoordinate
  integer :: status
  
  globalCoordinate%X = ((1.0d0 - localX) * this%Left) + (localX * this%Right)
  if(globalCoordinate%X .lt. this%Left) globalCoordinate%X = this%Left
  if(globalCoordinate%X .gt. this%Right) globalCoordinate%X = this%Right
  
  globalCoordinate%Y = ((1.0d0 - localY) * this%Front) + (localY * this%Back)
  if(globalCoordinate%Y .lt. this%Front) globalCoordinate%Y = this%Front
  if(globalCoordinate%Y .gt. this%Back) globalCoordinate%Y = this%Back
  
  globalCoordinate%Z = ((1.0d0 - localZ) * this%Bottom) + (localZ * this%Top)
  if(globalCoordinate%Z .lt. this%Bottom) globalCoordinate%Z = this%Bottom
  if(globalCoordinate%Z .gt. this%Top) globalCoordinate%Z = this%Top

  status = 0
  
  end function RectangularCellDataType_ConvertToGlobalCoordinate

!------------------------------------------
! Method: 
!------------------------------------------
  function RectangularCellDataType_ConvertToGlobalXY(this, localX, localY, globalX, globalY) result(status)
  class(RectangularCellDataType) :: this
  doubleprecision,intent(in) :: localX,localY
  doubleprecision,intent(inout) :: globalX,globalY
  integer :: status
  
  globalX = ((1.0d0 - localX) * this%Left) + (localX * this%Right)
  if(globalX .lt. this%Left) globalX = this%Left
  if(globalX .gt. this%Right) globalX = this%Right
  
  globalY = ((1.0d0 - localY) * this%Front) + (localY * this%Back)
  if(globalY .lt. this%Front) globalY = this%Front
  if(globalY .gt. this%Back) globalY = this%Back

  status = 0
  
  end function RectangularCellDataType_ConvertToGlobalXY

!------------------------------------------
! Method: 
!------------------------------------------
  function RectangularCellDataType_ConvertToLocalXY(this, globalX, globalY, localX, localY) result(status)
  class(RectangularCellDataType) :: this
  doubleprecision,intent(in) :: globalX,globalY
  doubleprecision,intent(inout) :: localX,localY
  integer :: status
  
  localX = (globalX - this%Left) / (this%Right - this%Left)
  if(localX .gt. 1.0d0) localX = 1.0d0
  if(localX .lt. 0d0) localX = 0d0
  
  localY = (globalY - this%Front) / (this%Back - this%Front)
  if(localY .gt. 1.0d0) localY = 1.0d0
  if(localY .lt. 0d0) localY = 0d0

  status = 0
  
  end function RectangularCellDataType_ConvertToLocalXY

!------------------------------------------
! Method: 
!------------------------------------------
  subroutine RectangularCellDataType_FindConnectionPosition(this, cellNumber, faceNumber, subFaceNumber)
  class(RectangularCellDataType) :: this
  integer,intent(in) :: cellNumber
  integer,intent(inout) :: faceNumber,subFaceNumber
  integer :: count,n,face
  
  faceNumber = 0
  subFaceNumber = 0
  
  do face = 1, 6
      count = this%GetFaceConnectionCount(face)
      if(count .gt. 0) then
        do n = 1, count
          if(this%GetConnection(face, n) .eq. cellNumber) then
            faceNumber = face
            subFaceNumber = n
            return
          end if
        end do
      end if
  end do
  
  end subroutine RectangularCellDataType_FindConnectionPosition
  
  
end module RectangularCellDataModule
  
  