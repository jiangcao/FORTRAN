module matrix_arith_complex
!-----------------------------------------------------------------------------------------
!! Complex Matrix Library in order to simplify the life with matrices in Fortran.
!! 
!-----------------------------------------------------------------------------------------

implicit none

type type_matrix_complex
    complex(8),allocatable :: m(:,:)
end type type_matrix_complex

type type_vector_complex 
	complex(8),allocatable :: v(:)
end type type_vector_complex

type type_diagmatrix_complex 
	complex(8),allocatable :: v(:)  !! only save the diagonal terms
end type type_diagmatrix_complex

interface assignment(=)
	module procedure matrix_list_assign_scalar, matrix_assign_scalar,                     &
					 matrix_assign_array, matrix_assign_matrix, matrix_list_assign,       &
					 diagmatrix_assign_scalar, diagmatrix_assign_diagmatrix,              &
					 diagmatrix_assign_array
end interface

interface operator(+)
	module procedure matrix_add, matrix_add_array, matrix_added_by_array
end interface

interface operator(-)
	module procedure matrix_sub, matrix_sub_array, matrix_subed_by_array
end interface

interface operator(*)
	module procedure matrix_times_scalar_r, matrix_times_scalar_c, matrix_times_matrix_simple,      & 
				     matrix_times_array_simple, array_times_matrix_simple, diagmatrix_times_matrix, &
	 				 diagmatrix_times_array, diagmatrix_times_scalar_c, diagmatrix_times_scalar_r
end interface

interface operator(**)
	module procedure matrix_power, array_power, matrix_transpose, array_transpose
end interface

interface operator(.eqsize.)
	module procedure matrix_compare_size_with_array, matrix_compare_size
end interface

interface size 
	module procedure matrix_size, matrix_list_size, matrix_size_dim,diagmatrix_size,      &
		             blocs_matrix_size
end interface

interface SaveTxt 
	module procedure matrix_list_print, array_print, matrix_print, blocs_matrix_print
end interface SaveTxt

interface alloc 
	module procedure matrix_list_allocElem, blocs_matrix_alloc, matrix_alloc, diagmatrix_alloc,&
	                 blocs_matrix_alloc2,blocs_diagmatrix_alloc,blocs_diagmatrix_alloc2,  &
	                 matrix_alloc2		
end interface

interface free 
	module procedure matrix_free,diagmatrix_free
end interface

interface times
	module procedure matrix_times_matrix, array_times_array, matrix_times_array,          &
		             array_times_matrix	
end interface

interface eye 
	module procedure array_eye 
end interface

interface BlocEye 
	module procedure blocs_matrix_eye
end interface

interface eig
	module procedure matrix_eigen, array_eigen
end interface

interface diag 
	module procedure matrix_to_diag, array_to_diag, matrix_from_diag,diagmatrix_to_diag,  &
					 blocs_matrix_diag   
end interface

interface inv
	module procedure array_inverse, matrix_inverse, array_inverse2, matrix_inverse2	
end interface

interface tr
	module procedure matrix_trace, array_trace
end interface

interface mat_gen 
	module procedure matrix_generate_from_func
end interface


contains



pure subroutine diagmatrix_alloc(M,n)	
implicit none 
type(type_diagmatrix_complex), intent(out) :: M 
integer, intent(in)                        :: n
allocate(M%v(n))
M%v = dcmplx(0.0d0,0.0d0)
end subroutine diagmatrix_alloc

pure subroutine matrix_alloc(M,n,nn)	
implicit none 
type(type_matrix_complex), intent(out) :: M 
integer, intent(in)                    :: n,nn
allocate(M%m(n,nn))
M%m  = dcmplx(0.0d0,0.0d0)
end subroutine matrix_alloc


pure subroutine matrix_alloc2(M,n)	
implicit none 
type(type_matrix_complex), intent(out) :: M 
integer, intent(in)                    :: n(2)
allocate(M%m(n(1),n(2)))
M%m  = dcmplx(0.0d0,0.0d0)
end subroutine matrix_alloc2


pure subroutine blocs_matrix_eye(M,s)
implicit none
type(type_matrix_complex),allocatable, intent(out) :: M(:,:) 
integer ,intent(in) :: s(:,:,:)	
integer :: i,j 
allocate(M(size(s, dim=2),size(s, dim=2)))
do i = 1,size(M,1)
	allocate(M(i,i)%m(s(1,i,i),s(1,i,i)))
	M(i,i)%m = dcmplx(0.0d0,0.0d0)
	do j= 1, s(1,i,i)		
		M(i,i)%m(j,j) = dcmplx(1.0d0,0.0d0)
	enddo
enddo
endsubroutine blocs_matrix_eye

pure function blocs_matrix_diag(M) result(v)
type(type_matrix_complex), intent(in) :: M(:,:)
integer :: i,n
complex(8) :: v(sum((/ ( size(M(i,i)%m,1) ,i=1,size(M,1) ) /)))
n =0
do i = 1, size(M,1)
	v(n+1: n+size(M(i,i)%m,1)) = matrix_trace(M(i,i))
	n = n+size(M(i,i)%m,1)
enddo
end function blocs_matrix_diag

pure function array_eye(n) result (R)
implicit none
integer, intent(in) :: n
complex(8)::R(n,n)
INTEGER   :: ii 
R = dcmplx(0.0d0,0.0d0)
forall (ii=1:n) R(ii,ii) = dcmplx(1.0d0,0.0d0)
end function array_eye

subroutine matrix_list_allocElem(this,nx,nm,nn) 
	implicit none
	integer, intent(in) 						:: nx , nm(1:nx), nn(1:nx)
	type(type_matrix_complex),intent(out) 	    :: this (1:nx)
	integer :: ii 
	do ii=1,nx
		if (allocated(this(ii)%m)) then
			print *, "WARNING !! Try to allocate a list already allocated "
		else
			allocate(this(ii)%m(nm(ii),nn(ii))) 
		endif
	enddo
end subroutine matrix_list_allocElem

subroutine blocs_matrix_alloc(M,s)
implicit none 
type(type_matrix_complex),allocatable, intent(out) :: M(:,:)
integer, intent(in)                    :: s(:,:,:)
integer :: i, j
allocate(M(size(s, dim=2),size(s, dim=3)))
do j=1,size(s, dim=3)
	do i=1,size(s, dim=2)
		if ((s(1,i,j) /= 0).and.(s(2,i,j) /=0 )) then 
			allocate(M(i,j)%m(s(1,i,j),s(2,i,j)))
			M(i,j)%m = dcmplx(0.0d0,0.0d0)
		endif 
	enddo 
enddo
endsubroutine blocs_matrix_alloc


subroutine blocs_matrix_alloc2(M,s,n)
!! M(:,:,:)	
!!   ^ ^ ^-- Another index (ex. for Energy)
!!   |_|____ Bloc Indexes
!!
implicit none 
type(type_matrix_complex),allocatable, intent(out) :: M(:,:,:)
integer, intent(in)                    :: s(:,:,:)
integer, intent(in)					   :: n
integer :: i, j, k
allocate(M(size(s, dim=2),size(s, dim=3),n))
do k=1,n 
	do j=1,size(s, dim=3)
		do i=1,size(s, dim=2)
			if ((s(1,i,j) /= 0).and.(s(2,i,j) /=0 )) then 
				allocate(M(i,j,k)%m(s(1,i,j),s(2,i,j)))
				M(i,j,k)%m = dcmplx(0.0d0,0.0d0)
			endif 
		enddo 
	enddo
enddo
endsubroutine blocs_matrix_alloc2

subroutine blocs_diagmatrix_alloc(M,s)
implicit none 
type(type_diagmatrix_complex),allocatable, intent(out) :: M(:,:)
integer, intent(in)                    :: s(:,:,:)
integer :: i, j
allocate(M(size(s, dim=2),size(s, dim=3)))
do j=1,size(s, dim=3)
	do i=1,size(s, dim=2)
		if ((s(1,i,j) /= 0).and.(s(2,i,j) /=0 )) then 
			allocate(M(i,j)%v(s(1,i,j)))
			M(i,j)%v = dcmplx(0.0d0,0.0d0)
		endif 
	enddo 
enddo
endsubroutine blocs_diagmatrix_alloc

subroutine blocs_diagmatrix_alloc2(M,s,n)
!! M(:,:,:)	
!!   ^ ^ ^-- Another index (ex. for Energy)
!!   |_|____ Bloc Indexes
!!
implicit none 
type(type_diagmatrix_complex),allocatable, intent(out) :: M(:,:,:)
integer, intent(in)                    :: s(:,:,:)
integer, intent(in)					   :: n
integer :: i, j, k
allocate(M(size(s, dim=2),size(s, dim=3),n))
do k=1,n 
	do j=1,size(s, dim=3)
		do i=1,size(s, dim=2)
			if ((s(1,i,j) /= 0).and.(s(2,i,j) /=0 )) then 
				allocate(M(i,j,k)%v(s(1,i,j)))
				M(i,j,k)%v = dcmplx(0.0d0,0.0d0)
			endif 
		enddo 
	enddo
enddo
endsubroutine blocs_diagmatrix_alloc2


elemental subroutine matrix_free(this)
implicit none
type(type_matrix_complex),intent(out) :: this
 	if (allocated(this%m)) deallocate(this%m)
end subroutine matrix_free


elemental subroutine diagmatrix_free(this)
implicit none
type(type_diagmatrix_complex),intent(out) :: this
 	if (allocated(this%v)) deallocate(this%v)
end subroutine diagmatrix_free

pure function matrix_list_size(list,dim) result(nm)
	implicit none 
	type(type_matrix_complex), intent(in) :: list(:) 
	integer ,intent(in)					  :: dim
	INTEGER 							  :: nm(size(list))
	integer :: ii
	forall (ii=1:size(list)) nm(ii) = size(list(ii)%m,dim)
end function matrix_list_size

pure function matrix_size(this) result(s)
	implicit none
	type(type_matrix_complex),intent(in) :: this 
	integer :: s(2), ii
	forall(ii=1:2) s(ii) = size(this%m, dim=ii)
end function matrix_size

pure function blocs_matrix_size(M) result(s)
	implicit none 
	type(type_matrix_complex), intent(in) :: M(:,:)
	integer :: s(2,size(M, dim=1),size(M, dim=2))
	integer :: i,j
	s = 0
	do j=1,size(M, dim=2)
		do i=1,size(M, dim=1)
			if (allocated(M(i,j)%m)) then 
				s(1,i,j) = size(M(i,j)%m, dim=1)
				s(2,i,j) = size(M(i,j)%m, dim=2)
			endif
		enddo
	enddo
end function blocs_matrix_size




pure function matrix_size_dim(this,dim) result(s)
	implicit none
	type(type_matrix_complex), intent(in) :: this 
	integer, intent(in) :: dim
	integer :: s, ii
	s = size(this%m, dim)
end function matrix_size_dim


subroutine matrix_list_print(handle,this)
	implicit none
	type(type_matrix_complex), intent(in) 	:: this(:)
	integer, intent(in), optional 			:: handle
	integer :: ii,xx,yy
	if (present(handle)) then
		write(handle, '(3(i8),es15.4,es15.4)') (((ii,xx,yy,this(ii)%m(xx,yy),xx=1,size(this(ii)%m,1)),yy=1,size(this(ii)%m,2)),ii=1,size(this))
	else
		print '(3(i8),es15.4,es15.4)',(((ii,xx,yy,this(ii)%m(xx,yy),xx=1,size(this(ii)%m,1)),yy=1,size(this(ii)%m,2)),ii=1,size(this))
	endif
end subroutine matrix_list_print
subroutine array_print(handle,A)
	implicit none
	complex(8), intent(in) 					:: A(:,:)
	integer, intent(in), optional 			:: handle
	integer :: xx,yy
	if (present(handle)) then
		write(handle, '(2(i8),es15.4,es15.4)') ((xx,yy,A(xx,yy),xx=1,size(A,1)),yy=1,size(A,2))
	else
		print '(2(i8),es15.4,es15.4)',((xx,yy,A(xx,yy),xx=1,size(A,1)),yy=1,size(A,2))
	endif
end subroutine array_print
subroutine matrix_print(handle,A)
	implicit none
	type(type_matrix_complex), intent(in) 	:: A
	integer, intent(in), optional 			:: handle
	call array_print(handle,A%m)
end subroutine matrix_print
subroutine blocs_matrix_print(handle,A)
	implicit none 
	type(type_matrix_complex), intent(in)  :: A (:,:)
	integer, intent(in)                    :: handle
	integer :: sizes(2,0:size(A, dim=1),0:size(A, dim=2))
	integer :: i,j,n(size(A, dim=2)),m(size(A, dim=1)),x,y 
	sizes = 0
	n=0
	m=0
	sizes(:,1:,1:) = blocs_matrix_size(A)
	do i=1,size(A, dim=2)		
		sizes(2,0,i) = maxval(sizes(2,1:,i))
	enddo 
	do j=1,size(A, dim=1)		
		sizes(1,j,0) = maxval(sizes(1,j,1:))
	enddo 
	do i=1,size(A, dim=2)
		n(i) = sum(sizes(2,0,1:i-1))
	enddo		
	do j=1,size(A, dim=1)
		m(j) = sum(sizes(1,1:j-1,0))
	enddo
	do i=1,size(A, dim=2)
		do j=1,size(A, dim=1)
			if (allocated(A(j,i)%m)) then
				do x=1,size(A(j,i)%m, dim=2)
					do y=1,size(A(j,i)%m, dim=1)
						write(handle,'(2I8,2E15.5)') y+m(j),x+n(i),A(j,i)%m(y,x)
					enddo 
				enddo 
			endif
		enddo 
	enddo
end subroutine blocs_matrix_print

pure function array_testHermitian(M) result(b)
implicit none 
complex(8), intent(in) :: M(:,:)
logical :: b 
integer :: i,j 
real(8),parameter :: TOL = 1.0D-10
b = .true.
do i=1,size(M,2)
	do j=1,i 
		if(abs(M(i,j) - conjg(M(j,i))) .gt. TOL) then 
			b = .false.
			return 
		endif 
	enddo
enddo
end function array_testHermitian

pure function blocs_matrix_testHermitian(M) result(b)
	implicit none 
	type(type_matrix_complex) , intent(in) :: M(:,:)
	logical :: b 
	integer :: i,j,x,y 
	b = .true.
	do i = 1,size(M, dim=2)
		do j = 1,size(M, dim=1)
			if (allocated(M(j,i)%m)) then 
				do x = 1,size(M(j,i)%m,2)
					do y = 1,size(M(j,i)%m,1)
						if (.not. allocated(M(i,j)%m)) then 
							b = .false.
							return 
						endif 
						if (M(i,j)%m(x,y) /= conjg(M(j,i)%m(y,x))) then 
							b = .false.
							return 
						endif 
					enddo 
				enddo 
			endif 
		enddo 
	enddo
end function blocs_matrix_testHermitian




pure subroutine matrix_list_assign_scalar(this,z)
	implicit none
	type(type_matrix_complex),intent(inout) 	:: this(:) 
	complex(8), intent(in)						:: z
	integer :: ii 
	forall (ii = 1: size(this))		this(ii)%m = z 
end subroutine matrix_list_assign_scalar
pure subroutine matrix_list_assign(this, list)
	implicit none
	type(type_matrix_complex), intent(inout)	:: this(:)
	type(type_matrix_complex), intent(in)		:: list(size(this))
	integer ::ii 
	forall (ii = 1:size(this))		this(ii) = list(ii)		
end subroutine matrix_list_assign
pure subroutine matrix_assign_scalar(this,z)
	implicit none
	type(type_matrix_complex), intent(inout)	:: this 
	complex(8), intent(in)						:: z
	this%m = z 
end subroutine matrix_assign_scalar	
pure subroutine matrix_assign_array(this,z)
	implicit none
	type(type_matrix_complex), intent(inout)	:: this 
	complex(8), intent(in)						:: z(size(this%m, dim=1),size(this%m, dim=2))
	this%m = z 	
end subroutine matrix_assign_array
pure subroutine matrix_assign_matrix(this,B)
	implicit none
	type(type_matrix_complex), intent(inout)	:: this 
	type(type_matrix_complex), intent(in)		:: B
	this%m(1:size(B%m,1),1:size(B%m,2)) = B%m(:,:) 
end subroutine matrix_assign_matrix
pure subroutine diagmatrix_assign_scalar(this,z)
	implicit none 
	type(type_diagmatrix_complex),intent(inout) :: this 
	complex(8), intent(in)						:: z
	this%v = z 
endsubroutine diagmatrix_assign_scalar
pure subroutine diagmatrix_assign_diagmatrix(this,B)
	implicit none 
	type(type_diagmatrix_complex),intent(inout) :: this
	type(type_diagmatrix_complex),intent(in)    :: B
	this%v = B%v 
endsubroutine diagmatrix_assign_diagmatrix
pure subroutine diagmatrix_assign_array(this,v)
	implicit none 
	type(type_diagmatrix_complex),intent(inout) :: this
	complex(8), intent(in)                      :: v(:)
	this%v = v 
endsubroutine diagmatrix_assign_array

pure function matrix_compare_size(A,B) result(r)
	implicit none 
	type(type_matrix_complex), intent(in) 		:: A,B
	logical 									:: r 
	r = .not. ((size(A%m,1) .ne. size(B%m,1)) .or. (size(A%m,2) .ne. size(B%m,2))) 
end function matrix_compare_size
pure function matrix_compare_size_with_array(A,B) result(r)
	implicit none 
	type(type_matrix_complex), intent(in) 		:: A
	complex(8), intent(in)						:: B(:,:)
	logical 									:: r 
	r = .not. ((size(A%m,1) .ne. size(B,1)) .or. (size(A%m,2) .ne. size(B,2))) 
end function matrix_compare_size_with_array



pure function matrix_add(A,B) result(C)
	implicit none
	type(type_matrix_complex),intent(in)		:: A,B 
	complex(8)									:: C(max(size(A%m,1),size(B%m,1)),max(size(A%m,2),size(B%m,2))) 
	if (allocated(A%m)) C = A%m
	if (allocated(B%m)) C = C+B%m 
end function matrix_add
pure function matrix_sub(A,B) result(C)
	implicit none
	type(type_matrix_complex),intent(in)		:: A,B 
	complex(8)									:: C(size(A%m,1),size(A%m,2)) 
	C = A%m-B%m 
end function matrix_sub
pure function matrix_add_array(A,B) result(C)
	implicit none
	type(type_matrix_complex),intent(in)		:: A
	complex(8)				, intent(in)		:: B(size(A%m,1),size(A%m,2))
	complex(8)									:: C(size(A%m,1),size(A%m,2)) 
	C = A%m+B
end function matrix_add_array
pure function matrix_sub_array(A,B) result(C)
	implicit none
	type(type_matrix_complex),intent(in)		:: A
	complex(8)				, intent(in)		:: B(size(A%m,1),size(A%m,2))
	complex(8)									:: C(size(A%m,1),size(A%m,2)) 
	C = A%m-B
end function matrix_sub_array
pure function matrix_added_by_array(B,A) result(C)
	implicit none
	type(type_matrix_complex),intent(in)		:: A
	complex(8)				, intent(in)		:: B(size(A%m,1),size(A%m,2))
	complex(8)									:: C(size(A%m,1),size(A%m,2)) 
	C = A%m-B
end function matrix_added_by_array
pure function matrix_subed_by_array(B,A) result(C)
	implicit none
	type(type_matrix_complex),intent(in)		:: A
	complex(8)				, intent(in)		:: B(size(A%m,1),size(A%m,2))
	complex(8)									:: C(size(A%m,1),size(A%m,2)) 
	C = A%m-B
end function matrix_subed_by_array



pure function diagmatrix_times_matrix(D,A) result(B)
implicit none
type(type_diagmatrix_complex),intent(in) :: D 
type(type_matrix_complex), 	  intent(in) :: A 
complex(8)								 :: B(size(D%v),size(A%m, 2))
integer :: ii 
forall (ii=1:size(D%v)) B(ii,:) = D%v(ii)*A%m(ii,:)
end function diagmatrix_times_matrix

pure function diagmatrix_times_array(D,A) result(B)
implicit none
type(type_diagmatrix_complex),intent(in) :: D 
complex(8), 				  intent(in) :: A(:,:)
complex(8)								 :: B(size(D%v),size(A, 2))
integer :: ii 
forall (ii=1:size(D%v)) B(ii,:) = D%v(ii)*A(ii,:)
end function diagmatrix_times_array

pure function diagmatrix_times_scalar_c(this, z) result(v)
	implicit none 
	type(type_diagmatrix_complex),intent(in) :: this 
	complex(8),intent(in)                    :: z 
	complex(8)                               :: v(size(this%v))
	v(:) = this%v(:) * z 
end function diagmatrix_times_scalar_c
pure function diagmatrix_times_scalar_r(this, a) result(v)
	implicit none 
	type(type_diagmatrix_complex),intent(in) :: this 
	real(8),intent(in)                       :: a 
	complex(8)                               :: v(size(this%v))
	v(:) = this%v(:) * dcmplx(a,0.0d0) 
end function diagmatrix_times_scalar_r

pure function matrix_times_scalar_r(this, a) result(B)
	implicit none
	type(type_matrix_complex),intent(in)	:: this 
	real(8)		,intent(in)					:: a		
	complex(8)								:: B(size(this%m,1),size(this%m,2))
	B = this%m * dcmplx(a, 0.0D0) 
end function matrix_times_scalar_r
pure function matrix_times_scalar_c(this, a) result(B)
	implicit none
	type(type_matrix_complex),intent(in)	:: this 
	complex(8)		,intent(in)				:: a		
	complex(8)								:: B(size(this%m,1),size(this%m,2))
	B = this%m * a 
end function matrix_times_scalar_c

 function matrix_times_matrix(A,B,trA,trB,cjA,cjB) result(C)
	implicit none 
	type(type_matrix_complex),intent(in)	:: A,B 
	complex(8)								:: C(size(A,merge(2,1,trA)),size(B,merge(1,2,trB)))
	LOGICAL(KIND=4)		, intent(in)		:: trA,trB
	LOGICAL(KIND=4)		, intent(in), optional	:: cjA,cjB
	C = array_times_array(A%m,B%m,trA,trB,cjA,cjB)
end function matrix_times_matrix
 function array_times_matrix(A,B,trA,trB,cjA,cjB) result(C)
	implicit none 
	complex(8)			, intent(in)			:: A(:,:)
	type(type_matrix_complex),intent(in)		:: B 
	LOGICAL(KIND=4)		, intent(in)			:: trA,trB
	LOGICAL(KIND=4)		, intent(in), optional	:: cjA,cjB
	complex(8)									:: C(size(A,merge(2,1,trA)),size(B,merge(1,2,trB)))
	C = array_times_array(A,B%m,trA,trB,cjA,cjB)
end function array_times_matrix
 function matrix_times_array(A,B,trA,trB,cjA,cjB) result(C)
	implicit none 
	type(type_matrix_complex), intent(in)		:: A
	complex(8),intent(in)						:: B(:,:) 
	LOGICAL(KIND=4)		, intent(in)			:: trA,trB
	LOGICAL(KIND=4)		, intent(in), optional	:: cjA,cjB
	complex(8)									:: C(size(A,merge(2,1,trA)),size(B,merge(1,2,trB)))
	C = array_times_array(A%m,B,trA,trB,cjA,cjB)
end function matrix_times_array	
 function array_times_array(A,B,trA,trB,cjA,cjB) result(C)
	implicit none 
	complex(8)			, intent(in)			:: A(:,:),B(:,:)
	complex(8)									:: C(size(A,merge(2,1,trA)),size(B,merge(1,2,trB)))
	LOGICAL(KIND=4)		, intent(in)			:: trA,trB
	LOGICAL(KIND=4)		, intent(in), optional	:: cjA,cjB
	integer :: lda,ldb,k,m,kb,n
	character :: ctrA,ctrB
	interface 
		pure subroutine ZGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC )
			COMPLEX(8),intent(in) 			:: ALPHA,BETA
    	    INTEGER,intent(in) 				:: K,LDA,LDB,LDC,M,N
     		CHARACTER,intent(in) 			:: TRANSA,TRANSB
     		COMPLEX(8),intent(in) 			:: A(lda,*),B(ldb,*)
     		COMPLEX(8),intent(inout) 		:: C(ldc,*)
		end subroutine ZGEMM
	end interface
	lda = size(A,1)
	ldb = size(B,1)
	if (.not. trA) then 
		k = size(A,2)
		m = size(A,1)
		ctrA = 'n'
	else 		
		k = size(A,1)
		m = size(A,2)
		if (present(cjA) .and. cjA) then 
			ctrA = 'c'
		else 
			ctrA = 't'
		endif
	endif
	if (.not. trB)then
		kb = size(B,1)
		n = size(B,2)
		ctrB = 'n'
	else
		kb = size(B,2)
		n = size(B,1)
		if (present(cjB) .and. cjB) then 
			ctrB = 'c'
		else 
			ctrB = 't'
		endif		
	endif
	if (k /= kb) then 
		print *,'ERROR in array_times_array:','k=',k,'kb=',kb
		call abort
	endif
	call zgemm(ctrA,ctrB,m,n,k,dcmplx(1.0d0,0.0d0),A,lda,B,ldb,dcmplx(0.0d0,0.0d0),C,m)
end function array_times_array
 function array_times_matrix_simple(A,B) result(C)
	implicit none 
	type(type_matrix_complex),intent(in)	:: B
	complex(8),intent(in)					:: A(:,:)
	complex(8)								:: C(size(A,1),size(B%m,2))	
	C = array_times_array(A,B%m,.false.,.false.)
end function array_times_matrix_simple
 function matrix_times_array_simple(A,B) result(C)
	implicit none 
	type(type_matrix_complex),intent(in)	:: A
	complex(8),intent(in)					:: B(:,:)
	complex(8)								:: C(size(A%m,1),size(B,2))	
	C = array_times_array(A%m,B,.false.,.false.)
end function matrix_times_array_simple
 function matrix_times_matrix_simple(A,B) result(C)
	implicit none 
	type(type_matrix_complex),intent(in)	:: A,B 
	complex(8)								:: C(size(A%m,1),size(B%m,2))
	C = array_times_array(A%m,B%m,.false.,.false.)
end function matrix_times_matrix_simple


 function matrix_power(A,n) result (C)
	implicit none
	type(type_matrix_complex), intent(in) :: A
	integer , intent(in)				  :: n 
	type(type_matrix_complex)			  :: B
	complex(8)							  :: C(size(A%m,1),size(A%m,1))
	C = array_power(A%m,n)
end function matrix_power
 function array_power(A,n) result (C)
	implicit none
	complex(8), intent(in) 				  :: A(:,:)
	integer , intent(in)				  :: n 
	type(type_matrix_complex)			  :: B
	complex(8)							  :: C(size(A,1),size(A,1))
	integer :: ii
	if (n > 0) then
		allocate(B%m(size(A,1),size(A,1) ))
		B = A
		do ii = 2, n
			B = B * A
		enddo
		C = B%m
		deallocate(B%m)
	elseif (n == 0) then 
		C = array_eye(size(A, dim=1))
	elseif (n == -1) then
		C = array_inverse(A)
	else
		C = array_inverse(A)
		allocate(B%m(size(C,1),size(C,1) ))
		B = C
		do ii = 2, -n
			B = B * C
		enddo
		C = B%m
		deallocate(B%m)
	endif
end function array_power

pure function matrix_transpose(A,t) result (C)
	implicit none
	type(type_matrix_complex), intent(in) :: A
	character , intent(in)				  :: t
	complex(8)							  :: C(size(A%m,2),size(A%m,1))
	if ((t=='t').or.(t=='T')) then 
		C = Transpose(A%m)
	elseif((t=='c').or.(t=='C')) then 
		C = Transpose(Conjg(A%m))
	endif
end function matrix_transpose
pure function array_transpose(A,t) result (C)
	implicit none
	complex(8)	, intent(in) 			  :: A(:,:)
	character , intent(in)				  :: t
	complex(8)							  :: C(size(A,2),size(A,1))
	if ((t=='t').or.(t=='T')) then 
		C = Transpose(A)
	elseif((t=='c').or.(t=='C')) then 
		C = Transpose(Conjg(A))
	endif
end function array_transpose


function matrix_eigen(A,B,eigvec,itype,uplo) result(eig)
implicit none
	type(type_matrix_complex), intent(in) 			:: A 
	type(type_matrix_complex), intent(in),optional 	:: B
	real(8)								  			:: eig(size(A%m,1))	
	type(type_matrix_complex), intent(inout), optional :: eigvec
	integer , intent(in), optional 		  			:: itype
	CHARACTER,intent(in), optional 		  			:: uplo
! 	integer 		:: itypeop
! 	CHARACTER 		:: uploop
! 	uploop 	= merge(uplo,'U',present(uplo))
! 	itypeop = merge(itype, 1, present(itype))
	if (present(B))	then 
		if (present(eigvec)) then
			eig = array_eigen(A%m,B%m,eigvec%m,itype=itype,uplo=uplo)
		else 
			eig = array_eigen(A%m,B%m,itype=itype,uplo=uplo)
		endif
	else 
		if (present(eigvec)) then
			eig = array_eigen(A%m,eigvec=eigvec%m,itype=itype,uplo=uplo)
		else 
			eig = array_eigen(A%m,itype=itype,uplo=uplo)
		endif
	endif 
end function matrix_eigen
function array_eigen(A,B,eigvec,itype,uplo) result(eig)
	implicit none 
	complex(8), intent(in)				:: A(:,:)
	complex(8), intent(in), optional	:: B(:,:) 
	real(8)								:: eig(size(A,1))	
	complex(8), intent(inout),optional 	:: eigvec(size(A,1),size(A,2)) 
	integer , intent(in), optional 		:: itype
	CHARACTER,intent(in), optional 		:: uplo
	integer 		:: LDA, N, LDB, lwork, INFO, itypeop,ii
	CHARACTER 		:: jobz,uploop
	real(8)		  	:: W(size(A,1)), RWORK(3*size(A,2))
	complex(8)    	:: work( 1 + 4*size(A,2) + size(A,2)**2), C(size(A,1),size(A,2))
	C(:,:) = A(:,:)
	if (present(eigvec)) then 
		jobz = 'V'
	else
		jobz = 'N'
	endif
	uploop 	= merge(uplo,'U',present(uplo))
	itypeop = merge(itype, 1, present(itype))	
	N 	= size(A, dim=2)
	LDA = size(A, dim=1)
	LWORK = size(WORK)
	if (present(B)) then 
		LDB = size(B, dim=1)
		call zhegv(itypeop,jobz,uploop,N,C,LDA,B,LDB,eig,WORK,LWORK,RWORK,INFO)
		if (INFO .ne. 0) then 
			print *,'@array_eigen ZHEGV fails with INFO=',INFO 
		call abort
	endif
	else
		LDB = LDA
		call zheev(jobz,uploop,N,C,LDA,eig,WORK,LWORK,RWORK,INFO)
		if (INFO .ne. 0) then 
			print *,'@array_eigen ZHEEV fails with INFO=',INFO 
		call abort
	endif
	endif
	if (present(eigvec)) eigvec = C 
end function array_eigen

pure function matrix_from_diag(vec) result(A)
implicit none
complex(8) , intent(in) 	:: vec(:) 
complex(8) 					:: A(size(vec),size(vec))
integer :: ii
A(:,:) = dcmplx(0.0d0,0.0d0)
forall (ii=1:size(vec)) A(ii,ii) = vec(ii)
end function matrix_from_diag
pure function array_to_diag(A) result(diag)
implicit none
complex(8), intent(in) 		:: A(:,:)
complex(8)					:: diag(size(A,1))
integer :: ii 
forall (ii=1:size(A,1)) diag(ii) = A(ii,ii)
end function array_to_diag
pure function matrix_to_diag(A) result(diag)
implicit none
type(type_matrix_complex), intent(in) :: A
complex(8)					:: diag(size(A%m,1))
diag = array_to_diag(A%m)
end function matrix_to_diag

 function array_inverse2(A, UPLO) result(C) ! for Hermitian matrix
implicit none
complex(8), intent(in)		:: A(:,:)
complex(8)					:: C(size(A, dim=1),size(A, dim=1))
CHARACTER, intent(in) 		:: UPLO 
integer 	:: info,lda,lwork,n,nnz
integer 	:: ipiv(size(A,1))
complex(8) 	:: work(size(A,1)*size(A,1),size(A,1)*size(A,1))
 n = size(A,1)
 C(:,:) = A(:,:)
 LDA = size(A,2)
 call zhetrf(UPLO, n, C, LDA, ipiv, WORK, size(WORK), info)
 if (info .ne. 0) then 
 	print *,'@array_inverse2 ZHETRF fails with INFO=', info
 	call abort
 endif
 call zhetri(UPLO, n, C, LDA, ipiv, WORK, info)
 if (info .ne. 0) then 
 	print *,'@array_inverse2 ZHETRI fails with INFO=', info
 	call abort
 endif
end function array_inverse2
 function matrix_inverse2(A, UPLO) result(C)
implicit none
type(type_matrix_complex), intent(in)		:: A
CHARACTER,intent(in)						:: UPLO	
complex(8)									:: C(size(A%m, dim=1),size(A%m, dim=1))
 C = array_inverse2(A%m, UPLO)
end function matrix_inverse2

 function array_inverse(A) result(C) ! for General matrix
implicit none
complex(8), intent(in)		:: A(:,:)
complex(8)					:: C(size(A, dim=1),size(A, dim=1))
integer 	:: info,lda,lwork,n,nnz
integer 	:: ipiv(size(A,1))
complex(8) 	:: work(size(A,1)*size(A,1),size(A,1)*size(A,1))
! print *,'in array_inverse'
 n = size(A,1)
 if (n /= size(A,2)) then
 	print *,'@array_inverse, size not square',n,size(A,2)
 	call abort
 endif
 C(:,:) = A(:,1:size(A,1))
 call zgetrf(n,n,C,n,ipiv,info)
 if (info .ne. 0) then 
 	print *,'@array_inverse ZGETRF fails with INFO=', info
 	print *,n
 	print C
 	call abort
 endif
 call zgetri(n,C,n,ipiv,work,n*n,info)
 if (info .ne. 0) then 
 	print *,'@array_inverse ZGETRI fails with INFO=', info
 	call abort
 endif
end function array_inverse
 function matrix_inverse(A) result(C)
implicit none
type(type_matrix_complex), intent(in)		:: A
complex(8)									:: C(size(A%m, dim=1),size(A%m, dim=1))
! print *,'in matrix_inverse'
 C = array_inverse(A%m)
end function matrix_inverse

pure function array_trace(A) result (tr)
implicit none
complex(8), intent(in)		:: A(:,:)
complex(8)					:: tr 
integer :: ii
tr = sum((/(A(ii,ii),ii=1,size(A, 1))/))
end function array_trace
pure function matrix_trace(A) result(tr)
implicit none
type(type_matrix_complex), intent(in)		:: A
complex(8)									:: tr 
tr = array_trace(A%m)
end function matrix_trace

!                                       |-- A pure function with 2 integer arguments
!                                       v   func(i,j) returns matrix value at (i,j)
pure function matrix_generate_from_func(func,N,M) result(H)
complex(8)				:: H(N,M)
integer, intent(in)		:: N,M 
integer :: ii,jj
abstract interface
	complex(8) pure function func(ii,jj)
		integer, intent(in) :: ii, jj
	end function 
end interface
forall (ii= 1:N, jj= 1:M) H(ii,jj) = func(ii,jj)
end function matrix_generate_from_func

subroutine matrix_generate_from_diag_blocs(H,B)
type(type_matrix_complex), allocatable,intent(out) :: H (:,:)
!! Generated Block Square Matrix
type(type_matrix_complex), intent(in)              :: B (:,:)
!! Diagonal Blocks, 2nd index is the distance to diagonal plus the band width
integer :: i,j,nm,mm,mid
allocate(H(size(B,1),size(B,1))) 
mid = size(B,2)/2+1
do i=1,size(B,2)
	do j=1,size(B,1) - abs(size(B,2)/2+1-i)
		if (allocated(B(j,i)%m)) then
			nm = size(B(j,i)%m,1)
			mm = size(B(j,i)%m,2)
			if (j < mid) then
				allocate(H(j+mid-i,j)%m(nm,mm))
				H(j+(mid-i),j) = B(j,i)
			else 
				allocate(H(j,j+(i-mid))%m(mm,nm))
				H(j+(size(B,2)/2+1-i),j) = B(j,i)**'c'
			endif
		endif
	enddo 
enddo
end subroutine matrix_generate_from_diag_blocs

!-----------------------------------------------------------------------------------------
pure function diagmatrix_size(A) result(n)
!! Function returns the size of a diagonal matrix 
implicit none
type(type_diagmatrix_complex),intent(in) :: A 
!! A diagonal matrix 
integer :: n !! size of the matrix 
n = size(A%v)
!-----------------------------------------------------------------------------------------
endfunction diagmatrix_size

pure function diagmatrix_to_diag(this) result(v)
implicit none
type(type_diagmatrix_complex),intent(in) :: this 
complex(8)                               :: v(size(this%v))
v(:) = this%v 
endfunction diagmatrix_to_diag
end module matrix_arith_complex