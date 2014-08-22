! A sparse matrix object
module sparse
    use list
    use debug
    implicit none
    private

    ! Data type
    public :: sparse_data
    public :: sparse_lil_t
    ! Basic methodes
    public :: sparse_lil_free
    public :: sparse_lil
    public :: sparse_lil_nnz 
    public :: sparse_lil_size

    ! Generic data operations
    public :: sparse_lil_put
    public :: sparse_lil_get

    ! Real operations
    public :: rsparse_lil_put
    public :: rsparse_lil_get
    public :: rsparse_lil_getRow
    public :: rsparse_lil_getCol
    public :: rsparse_lil_to_csr

    ! Complex operations



    ! A public variable to use as a MOLD for transfer()
    integer, dimension(:), allocatable :: sparse_data

    type list_ptr_t 
        type(list_t),pointer :: p => null()
    end type list_ptr_t

    type sparse_lil_t
        private
        integer :: nrow,ncol,nnz
        character(len=1) :: main_axe
        type(list_ptr_t), allocatable, dimension(:) :: rows, cols
    end type sparse_lil_t

    type sparse_lil_data_t
        private
        integer :: ind
        integer, dimension(:), allocatable :: val 
    end type sparse_lil_data_t

contains

    ! Construct an empty LIL sparse matrix
    subroutine sparse_lil(self,nrow,ncol,axe)
        type(sparse_lil_t), pointer :: self
        character(len=1),intent(in):: axe
        integer,intent(in) :: nrow, ncol
        integer :: i, data(1)
        allocate(self)
        self%nrow = nrow
        self%ncol = ncol
        self%nnz = 0
        self%main_axe = axe
        ! Main axe set to row, alloc a list of row pointer
        if (axe .eq. 'r') then
            allocate(self%rows(nrow))
            do i=1,nrow
                self%rows(i)%p => null()
                call list_init(self%rows(i)%p,data=data)
            end do
            ! Main axe set to col, alloc a list of col pointer
        else 
            allocate(self%cols(ncol))
            do i=1,ncol
                self%cols(i)%p => null()
                call list_init(self%cols(i)%p,data=data)
            end do
        end if
    end subroutine sparse_lil

    ! Free a LIL sparse matrix
    subroutine sparse_lil_free(self)
        type(sparse_lil_t),pointer :: self
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
    end subroutine sparse_lil_free

    ! Put a value in the LIL sparse matrix 
    subroutine sparse_lil_put(self, row, col, val)
        type(sparse_lil_t),pointer :: self
        integer,intent(in) :: row, col
        integer, dimension(:), intent(in) :: val
        type(list_t),pointer :: cell
        integer, dimension(:),pointer::val_in,data
        if (badIndex(self,row,col)) then 
            write(*,*) "ERROR, index out of range", row,col,self%nrow,self%ncol
            call myStop()
        end if
        if (self%main_axe .eq. 'r') then
            allocate(val_in(size(val)+1)) 
            val_in(2:) = val(:)
            val_in(1) = col
            cell => self%rows(row)%p
            data => list_get(cell)
            do while ((data(1).lt.col).and.(associated(list_next(cell))))
                cell => list_next(cell)
                data => list_get(cell)
            end do
            if (data(1).lt.col)then
                call list_insert(cell,data=val_in)
                self%nnz = self%nnz + 1
            else if (data(1).eq.col)then
                call list_put(cell,data=val_in)
            else 
                cell => list_prev(cell)
                call list_insert(cell,data=val_in)
                self%nnz = self%nnz + 1
            end if
            deallocate(val_in)
        else ! Col Main Axe
            allocate(val_in(size(val)+1)) 
            val_in(2:) = val(:)
            val_in(1) = row
            cell => self%cols(col)%p 
            data => list_get(cell)
            do while ((data(1).lt.row).and.(associated(list_next(cell))))
                cell => list_next(cell)
                data => list_get(cell)
            end do
            if (data(1).lt.row) then 
                call list_insert(cell,data=val_in) 
                self%nnz = self%nnz + 1
            else if (data(1).eq.row) then 
                call list_put(cell,data=val_in) 
            else 
                cell => list_prev(cell) 
                call list_insert(cell,data=val_in) 
                self%nnz = self%nnz + 1
            end if
            deallocate(val_in)
        end if
    end subroutine sparse_lil_put

    ! Get a value from the LIL sparse matrix
    function sparse_lil_get(self,row,col,find) result(val)
        type(sparse_lil_t),pointer :: self
        integer,intent(in) :: row, col
        logical,intent(out),optional :: find 
        integer,dimension(:),pointer ::val, data
        type(list_t),pointer   :: cell 
        if (badIndex(self,row,col)) then 
            write(*,*) "ERROR, index out of range", row,col,self%nrow,self%ncol
            call myStop() 
        end if
        if (self%main_axe.eq.'r') then 
            cell => self%rows(row)%p 
            data => list_get(cell)
            do while ((data(1).lt.col).and.(associated(list_next(cell))))
                cell => list_next(cell)
                data => list_get(cell)
            end do
            if (data(1).eq.col) then 
                if (present(find)) find = .true.
                val => data(2:size(data))
            else 
                if (present(find)) find = .false. 
                val => null()
            end if
        else 
            cell => self%cols(col)%p
            data => list_get(cell)
            do while ((data(1).lt.row).and.(associated(list_next(cell))))
                cell => list_next(cell)
                data => list_get(cell)
            end do
            if (data(1).eq.row) then 
                if (present(find)) find = .true.
                val => data(2:size(data))
            else 
                if (present(find)) find = .false. 
                val => null()
            end if
        end if
    end function sparse_lil_get


    ! Get the NNZ of a LIL sparse matrix 
    function sparse_lil_nnz(self) result(nnz) 
        type(sparse_lil_t),pointer :: self
        integer :: nnz 
        nnz = self%nnz 
    end function sparse_lil_nnz

    ! Get the size of a LIL sparse matrix 
    function sparse_lil_size(self, dim) result(n) 
        type(sparse_lil_t),pointer :: self
        integer,intent(in) :: dim 
        integer :: n 
        if (dim.eq.1) then
            n = self%nrow 
        else 
            n = self%ncol 
        end if
    end function sparse_lil_size

    ! Verify is the index is in the bondary
    function badIndex(self,row,col)
        type(sparse_lil_t),pointer :: self
        integer, intent(in) :: row,col 
        logical :: badIndex
        if ((row.lt.1).or.(col.lt.1).or.(row.gt.self%nrow).or.(col.gt.self%ncol)) then 
            badIndex = .true.
        else 
            badIndex = .false.
        end if
    end function badIndex


    ! Put a real number in the sparse LIL matrix
    subroutine rsparse_lil_put(self,row,col,num)
        type(sparse_lil_t),pointer :: self
        integer,intent(in) :: row, col
        real(8),intent(in) :: num 
        call sparse_lil_put(self,row,col,val=transfer(num,sparse_data))
    end subroutine rsparse_lil_put

    ! Get a real number from the sparse LIL matrix
    function rsparse_lil_get(self,row,col,find) result(num)
        type(sparse_lil_t),pointer :: self
        integer,intent(in) :: row, col
        logical,intent(out),optional :: find
        logical :: b
        real(8) :: num 
        integer,dimension(:),pointer :: data
        data => sparse_lil_get(self,row,col,find=b)
        if (b .eq. .false.) then
            num = 0.0d0
        else 
            num = transfer(data,real(8))
        end if
        if (present(find)) find = b 
    end function rsparse_lil_get

    !Get CSR format of a sparse LIL matrix
    subroutine rsparse_lil_to_csr(self,values,columns,rowIndex)
        type(sparse_lil_t),pointer :: self
        real(8),dimension(:),intent(out) :: values
        integer,dimension(:),intent(out) :: columns,rowIndex
        type(list_t),pointer :: cell
        integer,dimension(:),pointer :: data
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
                    data => list_get(cell)
                    columns(n) = data(1)
                    values(n) = transfer(data(2:),real(8))
                    n = n+1
                end do
            end do
            rowIndex(self%nrow+1) = self%nnz + 1
        else 
            write(*,*) "Use axe='r' when Initializing the sparse matrix"
            call myStop()
        end if
    end subroutine rsparse_lil_to_csr

    ! Get the row array of a real sparse LIL matrix
    subroutine rsparse_lil_getRow(self,row,rowarray)
        type(sparse_lil_t),pointer :: self 
        integer,intent(in) :: row 
        real(8),intent(out) :: rowarray(:)
        type(list_t),pointer :: cell
        integer :: ind
        integer,dimension(:),pointer :: data
        if (size(rowarray).ne.self%ncol) then
            write(*,*) "Vector rowarray wrong length"
            call myStop()
        end if
        if (self%main_axe.eq.'r') then
            rowarray(:) = 0.0d0
            cell => self%rows(row)%p
            do while (associated(list_next(cell)))
                cell => list_next(cell)
                data => list_get(cell)
                ind = data(1)
                rowarray(ind) = transfer(data(2:),real(8))
            end do
        else 
            do ind = 1,self%ncol
                rowarray(ind) = rsparse_lil_get(self,row,ind)
            end do
        end if
    end subroutine rsparse_lil_getRow

    ! Get the col array of a real sparse LIL matrix
    subroutine rsparse_lil_getCol(self,col,colarray)
        type(sparse_lil_t),pointer :: self 
        integer,intent(in) :: col
        real(8),intent(out) :: colarray(:)
        type(list_t),pointer :: cell
        integer :: ind
        integer,dimension(:),pointer :: data
        if (size(colarray).ne.self%nrow) then
            write(*,*) "Vector colarray wrong length"
            call myStop()
        end if
        if (self%main_axe.eq.'c') then
            colarray(:) = 0.0d0
            cell => self%cols(col)%p
            do while (associated(list_next(cell)))
                cell => list_next(cell)
                data => list_get(cell)
                ind = data(1)
                colarray(ind) = transfer(data(2:),real(8))
            end do
        else 
            do ind = 1,self%nrow
                colarray(ind) = rsparse_lil_get(self,ind,col)
            end do
        end if
    end subroutine rsparse_lil_getCol


end module sparse
