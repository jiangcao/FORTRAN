! A real sparse matrix object

!!! 
!  Sparse data module
!!!
module sparse_data_mod
implicit none
type sparse_data
    integer :: i
    real(8) :: r
end type sparse_data
contains
function DATA_CREATE(i,r) result(d_ptr)
    type(sparse_data),pointer     :: d_ptr
    real,intent(in) :: r
    integer,intent(in) :: i
    allocate(d_ptr)
    d_ptr%r = r
    d_ptr%i = i
    return
end function DATA_CREATE

function DATA_COMPARE(d1,d2) result(b)
    type(sparse_data),pointer :: d1,d2
    logical :: b
    if ((d1%r.eq.d2%r).and.(d1%i.eq.d2%i)) then
        b = .true.
    else 
        b = .false.
    end if
end function DATA_COMPARE

subroutine DATA_FREE(d)
    type(sparse_data),pointer :: d
    deallocate(d)
    nullify(d)
end subroutine DATA_FREE
end module sparse_data_mod
!!!!!


!!!
!  Sparse line module, using Linked List template
!!!
module sparse_line_mod
use sparse_data_mod, LIST_DATA => sparse_data
implicit none
include "linked_list_template.f90"
end module sparse_line_mod
!!!!!


!!!
!  Sparse Matrix module
!!!
module sparse_real_mod
use sparse_line_mod, LINE_t => LINKED_LIST
use sparse_data_mod
use debug
implicit none
private

! Data type
public :: sparse_real
! Constructor and Destructor
public :: sparse_free
public :: sparse_create

! Methods
public :: sparse_nnz 
public :: sparse_size
public :: sparse_put
public :: sparse_get
public :: sparse_Row
public :: sparse_Col
public :: sparse_to_csr


type line_ptr
    type(line_t),pointer :: p => null()
end type line_ptr

type sparse_real
    private
    integer :: nrow,ncol,nnz
    character(len=1) :: main_axe
    type(line_ptr), allocatable, dimension(:) :: rows, cols
end type sparse_real

contains

! Construct an empty LIL sparse matrix
function sparse_create(nrow,ncol,axe) result(self)
    type(sparse_real), pointer :: self
    character(len=1),intent(in):: axe
    integer,intent(in) :: nrow, ncol
    integer :: i
    type(sparse_data),pointer :: dptr
    allocate(self)
    self%nrow = nrow
    self%ncol = ncol
    self%nnz = 0
    self%main_axe = axe
    ! Main axe set to row, alloc a list of row pointer
    if (axe .eq. 'r') then
        allocate(self%rows(nrow))
        do i=1,nrow
            dptr => data_create(0,0.0d0)
            self%rows(i)%p => list_create(dptr)
        end do
        ! Main axe set to col, alloc a list of col pointer
    else 
        allocate(self%cols(ncol))
        do i=1,ncol
            dptr => data_create(0,0.0d0)
            self%cols(i)%p => list_create(dptr)
        end do
    end if
end function sparse_create

! Free a LIL sparse matrix
subroutine sparse_free(self)
    type(sparse_real),pointer :: self
    integer :: i
    if (self%main_axe .eq. 'r') then
        do i = 1, self%nrow
            call list_free(self%rows(i)%p)
        end do
        deallocate(self%rows)
    else 
        do i = 1,self%ncol
            call list_free(self%cols(i)%p)
        end do
        deallocate(self%cols)
    end if
end subroutine sparse_free

! Put a value in the LIL sparse matrix 
subroutine sparse_put(self, row, col, val)
    type(sparse_real),pointer :: self
    integer,intent(in) :: row, col
    type(line_t),pointer :: ptr
    type(sparse_data),pointer :: dptr,dptr2
    real(8),intent(in) :: val
    if (badIndex(self,row,col)) then 
        write(*,*) "ERROR, index out of range", row,col,self%nrow,self%ncol
        call myStop()
    end if
    if (self%main_axe .eq. 'r') then
        dptr2 => data_create(col,val)
        ptr => self%rows(row)%p
        dptr => list_get_from_node(ptr)
        do while ((dptr%i.lt.col).and.(associated(list_next(ptr))))
            ptr => list_next(ptr)
            dptr => list_get_from_node(ptr)
        end do
        if (dptr%i.lt.col)then
            call list_insert_after(ptr,dptr2)
            self%nnz = self%nnz + 1
        else if (dptr%i.eq.col)then
            call data_free(dptr)
            call list_put(ptr,dptr2)
        else 
            ptr => list_prev(ptr)
            call list_insert_after(ptr,dptr2)
            self%nnz = self%nnz + 1
        end if
    else ! Col Main Axe
        dptr2 => data_create(row,val)
        ptr => self%cols(col)%p 
        dptr => list_get_from_node(ptr)
        do while ((dptr%i.lt.row).and.(associated(list_next(ptr))))
            ptr => list_next(ptr)
            dptr => list_get_from_node(ptr)
        end do
        if (dptr%i.lt.row) then 
            call list_insert_after(ptr,dptr2) 
            self%nnz = self%nnz + 1
        else if (dptr%i.eq.row) then 
            call data_free(dptr)
            call list_put(ptr,dptr2) 
        else 
            ptr => list_prev(ptr) 
            call list_insert_after(ptr,dptr2) 
            self%nnz = self%nnz + 1
        end if
    end if
end subroutine sparse_put

! Get a value from the LIL sparse matrix
function sparse_get(self,row,col,find) result(val)
    type(sparse_real),pointer :: self
    integer,intent(in) :: row, col
    logical,intent(out),optional :: find 
    real(8) :: val
    type(line_t),pointer   :: cell
    type(sparse_data),pointer   :: dptr
    if (badIndex(self,row,col)) then 
        write(*,*) "ERROR, index out of range", row,col,self%nrow,self%ncol
        call myStop() 
    end if
    if (self%main_axe.eq.'r') then 
        cell => self%rows(row)%p 
        dptr => list_get_from_node(cell)
        do while ((dptr%i.lt.col).and.(associated(list_next(cell))))
            cell => list_next(cell)
            dptr => list_get_from_node(cell)
        end do
        if (dptr%i.eq.col) then 
            if (present(find)) find = .true.
            val = dptr%r
        else 
            if (present(find)) find = .false. 
            val = 0.0d0
        end if
    else 
        cell => self%cols(col)%p
        dptr => list_get_from_node(cell)
        do while ((dptr%i.lt.row).and.(associated(list_next(cell))))
            cell => list_next(cell)
            dptr => list_get_from_node(cell)
        end do
        if (dptr%i.eq.row) then 
            if (present(find)) find = .true.
            val = dptr%r
        else 
            if (present(find)) find = .false. 
            val = 0.0d0
        end if
    end if
end function sparse_get


! Get the NNZ of a LIL sparse matrix 
function sparse_nnz(self) result(nnz) 
    type(sparse_real),pointer :: self
    integer :: nnz 
    nnz = self%nnz 
end function sparse_nnz

! Get the size of a LIL sparse matrix 
function sparse_size(self, dim) result(n) 
    type(sparse_real),pointer :: self
    integer,intent(in) :: dim 
    integer :: n 
    if (dim.eq.1) then
        n = self%nrow 
    else 
        n = self%ncol 
    end if
end function sparse_size

! Verify is the index is in the bondary
function badIndex(self,row,col)
    type(sparse_real),pointer :: self
    integer, intent(in) :: row,col 
    logical :: badIndex
    if ((row.lt.1).or.(col.lt.1).or.(row.gt.self%nrow).or.(col.gt.self%ncol)) then 
        badIndex = .true.
    else 
        badIndex = .false.
    end if
end function badIndex

!Get CSR format of a sparse LIL matrix
subroutine sparse_to_csr(self,values,columns,rowIndex)
    type(sparse_real),pointer :: self
    real(8),dimension(:),intent(out) :: values
    integer,dimension(:),intent(out) :: columns,rowIndex
    type(line_t),pointer :: cell
    type(sparse_data),pointer :: dptr
    integer :: i,n
    if (size(values).ne.self%nnz) then
        write(*,*) "Vector values wrong length",size(values),self%nnz
        call myStop()
    end if
    if (size(columns).ne.self%nnz) then
        write(*,*) "Vector columns wrong length",size(columns),self%nnz
        call myStop()
    end if
    if (size(rowIndex).ne.(self%nrow+1)) then
        write(*,*) "Vector rowIndex wrong length",size(rowIndex),self%nrow+1
        call myStop()
    end if
    if (self%main_axe.eq.'r') then
        n = 1
        do i = 1,self%nrow
            rowIndex(i) = n
            cell => self%rows(i)%p
            do while (associated(list_next(cell)))
                cell => list_next(cell)
                dptr => list_get_from_node(cell)
                columns(n) = dptr%i
                values(n) = dptr%r
                n = n+1
            end do
        end do
        rowIndex(self%nrow+1) = self%nnz + 1
    else 
        write(*,*) "Use axe='r' when Initializing the sparse matrix"
        call myStop()
    end if
end subroutine sparse_to_csr

! Get the row array of a real sparse LIL matrix
subroutine sparse_Row(self,row,rowarray)
    type(sparse_real),pointer :: self 
    integer,intent(in) :: row 
    real(8),intent(out) :: rowarray(:)
    type(line_t),pointer :: cell
    type(sparse_data),pointer :: dptr
    integer :: ind
    if (size(rowarray).ne.self%ncol) then
        write(*,*) "Vector rowarray wrong length"
        call myStop()
    end if
    if (self%main_axe.eq.'r') then
        rowarray(:) = 0.0d0
        cell => self%rows(row)%p
        do while (associated(list_next(cell)))
            cell => list_next(cell)
            dptr => list_get_from_node(cell)
            ind = dptr%i
            rowarray(ind) = dptr%r
        end do
    else 
        do ind = 1,self%ncol
            rowarray(ind) = sparse_get(self,row,ind)
        end do
    end if
end subroutine sparse_Row

! Get the col array of a real sparse LIL matrix
subroutine sparse_Col(self,col,colarray)
    type(sparse_real),pointer :: self 
    integer,intent(in) :: col
    real(8),intent(out) :: colarray(:)
    type(line_t),pointer :: cell
    type(sparse_data),pointer :: dptr
    integer :: ind
    if (size(colarray).ne.self%nrow) then
        write(*,*) "Vector colarray wrong length"
        call myStop()
    end if
    if (self%main_axe.eq.'c') then
        colarray(:) = 0.0d0
        cell => self%cols(col)%p
        do while (associated(list_next(cell)))
            cell => list_next(cell)
            dptr => list_get_from_node(cell)
            ind = dptr%i
            colarray(ind) = dptr%r
        end do
    else 
        do ind = 1,self%nrow
            colarray(ind) = sparse_get(self,ind,col)
        end do
    end if
end subroutine sparse_Col
end module sparse_real_mod
