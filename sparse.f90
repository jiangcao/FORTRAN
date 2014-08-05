! A sparse matrix object
module sparse
use list
  implicit none
  private

  public :: sparse_lil_free
  public :: sparse_lil_t
  public :: sparse_lil
  public :: sparse_lil_put
  public :: sparse_lil_get

  
  type sparse_lil_t
     private
     integer :: nrow,ncol,nnz
     integer :: data_type
     character(len=1) :: main_axe
     type(list_t), allocatable, dimension(:) :: rows, cols
  end type sparse_lil_t

  type sparse_lil_data_t
     private
     integer :: ind
     integer, dimension(:), allocatable :: val 
  end type sparse_lil_data_t

contains

  ! Construct an empty LIL sparse matrix
  subroutine sparse_lil(self,nrow,ncol,axe,dtyp)
    type(sparse_lil_t), pointer :: self
    character(len=1),intent(in):: axe
    character(len=1),intent(in):: dtyp
    integer,intent(in) :: nrow, ncol
    self%nrow = nrow
    self%ncol = ncol
    self%nnz = 0
    self%main_axe = axe
    self%data_type = dtyp
    ! Main axe set to row, alloc a list of row pointer
    if (axe .eq. 'r') then
       allocate(self%rows(nrow))
       self%rows(:) => null()
    ! Main axe set to col, alloc a list of col pointer
    else 
       allocate(self%cols(ncol))
       self%cols(:) => null()
    end if
  end subroutine sparse_lil

  ! Free a LIL sparse matrix
  subroutine sparse_lil_free(self)
    type(sparse_lil_t),pointer :: self
    integer :: i
    if (self%main_axe .eq. 'r') then
       do i = 1, self%nrow
          call list_free(self%rows(i))
       end do
       deallocate(self%rows)
    else 
       do i = 1,self%ncol
          call list_free(self%cols(i))
       end do
       deallocate(self%cols)
    end if
  end subroutine sparse_lil_free

  ! Put a value in the LIL vue
  subroutine sparse_lil_put(self, row, col, val)
    type(sparse_lil_t),pointer :: self
    integer,intent(in) :: row, col
    integer, dimension(:), intent(in) :: val
    type(list_t),pointer :: cell
    type(sparse_lil_data_t) :: data
    if (self%main_axe .eq. 'r') then
       if ((row.gt.self%nrow).or.(row.lt.1)) then
          write(*,*) "ERROR, row out of range in Sparse"
          call abort
       else 
          cell => self%rows(row)
          data = transfer(list_get(cell),data)
          do while ((data%ind.lt.col).and.(associated(list_next(cell))))
             cell => list_next(cell)
             data = transfer(list_get(cell),data)
          end do
        
          if (data%ind.lt.col)then
             call list_insert(cell,data=
          
       end if
    else ! Col Main Axe

    end if
  end subroutine sparse_lil_put

  ! Get a value from the LIL vue
  function sparse_lil_get(self,row,col) result data
    type(sparse_lil_t),pointer :: self
    integer,intent(in) :: row, col
    integer, dimension(:), pointer :: data
  !  data => self%data    
  end function sparse_lil_get

end module sparse
