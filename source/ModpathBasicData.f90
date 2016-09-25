module ModpathBasicDataModule
  use UtilMiscModule,only : utrimall
  use RectangularUnstructuredGridModule,only : RectangularUnstructuredGridType
  implicit none
  
! Set default access status to private
  private
  
! Public derived data type definitions
!--------------------------------------
! type: 
!--------------------------------------
  type,public :: ModpathBasicDataType
      integer :: DefaultIfaceCount
      doubleprecision :: HNoFlow
      doubleprecision :: HDry
      integer,dimension(:),allocatable :: Layertypes
      integer,dimension(:),allocatable :: IBound
      integer,dimension(:),allocatable :: DefaultIfaceValues
      character(len=16),dimension(:),allocatable :: DefaultIFaceLabels
      doubleprecision,dimension(:),allocatable :: Porosity
  contains
    procedure :: ReadData=>pr_ReadData
  end type

contains

  subroutine pr_ReadData(this, inUnit, outUnit, cellsPerLayer, layerCount, cellCount, grid)
  use utl7module,only : urdcom, upcase
  use UTL8MODULE,only : urword, ustop, u1dint, u1drel, u1ddbl, u8rdcom, u3dintmp, u3dintmpusg, u3ddblmp, u3ddblmpusg
  implicit none
  class(ModpathBasicDataType) :: this
  integer,intent(in) :: inUnit, outUnit, layerCount, cellCount
  integer,dimension(layerCount),intent(in) :: cellsPerLayer
  type(RectangularUnstructuredGridType),intent(in) :: grid
  !character(len=80),dimension(2) :: heading
  character(len=200) :: line
  character(len=16) :: txt
  integer :: n, m, nn, length, iface, errorCode
  character(len=24),dimension(2) :: aname
  data aname(1) /'          BOUNDARY ARRAY'/
  data aname(2) /'                POROSITY'/
  
  m = 0
  do n = 1, layerCount
      m = m + cellsPerLayer(n)
  end do
  if(m .ne. cellCount) then
      call ustop('The sum of cells per layer does not match the number of cells in the grid. Stop.')
  end if
  if(allocated(this%IBound)) deallocate(this%IBound)
  if(allocated(this%Porosity)) deallocate(this%Porosity)
  if(allocated(this%LayerTypes)) deallocate(this%LayerTypes)
  allocate(this%IBound(cellCount))
  allocate(this%Porosity(cellCount))
  allocate(this%LayerTypes(layerCount))
  
  ! Write header to the listing file
  write(outUnit, *)
  write(outUnit, '(1x,a)') 'MODPATH basic data file data'
  write(outUnit, '(1x,a)') '----------------------------'

  ! READ MPBAS DATA
  ! READ AND PRINT COMMENTS.
  call u8rdcom(inUnit,outUnit,line,errorCode)
 
  ! No flow and dry cell head flags         
  read(line,*) this%HNoFlow, this%HDry
  write(outUnit,'(1X,A,1PG12.5,A)') 'Aquifer head is set to ', this%HNoFlow, ' at all no-flow cells (IBOUND=0).'
  write(outUnit,'(1X,A,1PG12.5,A)') 'Aquifer head is set to ', this%Hdry, ' at all dry cells.'
  write(outUnit, *)
  
      
      ! READ NUMBER OF STRESS PACKAGES THAT DO NOT HAVE AUXILIARY IFACE SUPPORT
      read(inUnit, *) this%DefaultIfaceCount
      allocate (this%DefaultIfaceValues(this%DefaultIfaceCount))
      allocate (this%DefaultIfaceLabels(this%DefaultIfaceCount))
      
      if(this%DefaultIfaceCount .gt. 0) then
        ! READ BUDGET LABELS AND DEFAULT IFACE
        do n = 1, this%DefaultIfaceCount
          read(inUnit,'(A)') line
          call utrimall(line)
          length = len_trim(line)
          if(length .eq. 0) THEN
            call ustop('Stress package labels cannot be blank strings.')
          else
            if(length .gt. 16) length = 16
            this%DefaultIfaceLabels(n) = line
            call upcase(this%DefaultIfaceLabels(n))
          end if

          ! READ DEFAULT IFACE          
          read(inUnit,*) this%DefaultIfaceValues(n)
          
        end do

        ! Check for duplicate labels        
        do n = 1, this%DefaultIfaceCount
          txt = this%DefaultIfaceLabels(n)
          do m = 1, this%DefaultIfaceCount
            if(m .NE. n) then
              if(txt .eq. this%DefaultIfaceLabels(m)) THEN
               call ustop('Duplicate stress package labels are not allowed.')
              end if
            end if
          end do
        end do
        
        write(outUnit, *)
        write(outUnit,'(1X,A,A)') 'Default stress package boundary face options (IFACE):'
        if(this%DefaultIfaceCount .eq. 0) THEN
          write(outUnit,'(3X,A)') 'None were specified'
        else
          do n = 1, this%DefaultIfaceCount
            iface = this%DefaultIfaceValues(n)
            if(iface .eq. 0) THEN
              write(outUnit,'(3X,A,A)') trim(this%DefaultIfaceLabels(n)), ' will be treated as internal stresses (IFACE = 0)'
            else if((iface .lt. 0) .or. (iface .gt. 6)) THEN
              call ustop(' IFACE must have a value between 0 and 6.')
            else
              write(outUnit,'(3X,A,A,I2)') trim(this%DefaultIfaceLabels(n)), ' will be assigned to face ',IFACE
            end if
          end do
        end if
        
      end if
  
      ! LAYTYP
      write(outUnit, *)
      write(outUnit,'(1X,A)') 'Layer type (LAYTYP)'
      read(inUnit,*) (this%LayerTypes(n), n = 1, layerCount)
      write(outUnit,'(1X,80I2)') (this%LayerTypes(n), n = 1, layerCount)
      do n = 1, layerCount
        if(this%LayerTypes(n) .lt. 0) this%LayerTypes(n) = 1
      end do

      ! IBOUND
      if(grid%GetGridType() .eq. 1) then
            call u3dintmp(inUnit, outUnit, layerCount, grid%GetRowCount(), grid%GetColumnCount(), cellCount, this%IBound, ANAME(1))                      
      else if(grid%GetGridType() .eq. 2) then
          call u3dintmpusg(inUnit, outUnit, cellCount, layerCount, this%IBound, aname(1), cellsPerLayer)
      else
            write(outUnit,*) 'Invalid grid type specified when reading IBOUND array data.'
            write(outUnit,*) 'Stopping.'
            call ustop(' ')          
      end if

      ! POROSITY
      if(grid%GetGridType() .eq. 1) then
            call u3ddblmp(inUnit, outUnit, layerCount, grid%GetRowCount(), grid%GetColumnCount(), cellCount, this%Porosity, ANAME(2))                      
      else if(grid%GetGridType() .eq. 2) then
          call u3ddblmpusg(inUnit, outUnit, cellCount, layerCount, this%Porosity, aname(2), cellsPerLayer)
      else
            write(outUnit,*) 'Invalid grid type specified when reading POROSITY array data.'
            write(outUnit,*) 'Stopping.'
            call ustop(' ')          
      end if
  
  end subroutine pr_ReadData

end module  ModpathBasicDataModule     
