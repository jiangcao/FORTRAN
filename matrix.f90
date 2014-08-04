module matrix
implicit none

!! safe allocate a matrix
interface malloc
   module procedure malloc_c
   module procedure malloc_r
end interface

!! Matrix Multiplication
interface MUX
   module procedure MUX_C
   module procedure MUX_R
end interface MUX

interface triMUX
   module procedure triMUX_C

end interface triMUX

!! Build tri diagonal bloc matrix
interface tridiagBloc
   module procedure tridiagBloc_C
   module procedure tridiagBloc_R 
end interface tridiagBloc

!! Build identity matrix
interface eye
   module procedure eye_c
end interface

!! Invert a matrix
interface invert
   module procedure invert_c
end interface

contains

subroutine invert_c(A)
implicit none    
integer :: info,lda,lwork,n    
integer,allocatable,dimension(:) :: ipiv
complex(8),intent(inout), dimension(:,:) :: A
complex(8),allocatable,dimension(:) :: work
if (size(A,1).ne.size(A,2))then
   write(*,*) 'Error in Invert! A is not square',size(A,1),size(A,2)
   stop
else
   n = size(A,1)
   allocate(ipiv(n))
   allocate(work(n*n))
   call zgetrf(n,n,A,n,ipiv,info)
   call zgetri(n,A,n,ipiv,work,n*n,info)
   deallocate(ipiv)
   deallocate(work)
end if
end subroutine invert_c

subroutine eye_c(Id,n)
complex(8),intent(inout),dimension(:,:),Allocatable :: Id
integer,intent(in) :: n
integer :: i
if (not(allocated(Id)))then
   allocate(Id(n,n))
else if((size(Id,1).ne.n).or.(size(Id,2).ne.n))then
   deallocate(Id)
   allocate(Id(n,n))
end if
Id=cmplx(0.0d0,0.0d0)
do i=1,n
   Id(i,i) = cmplx(1.0d0,0.0d0)
end do
end subroutine eye_c




subroutine tridiagBloc_C(H00,Hu, H)
COMPLEX(8),intent(in),dimension(:,:,:) :: H00,Hu
COMPLEX(8),intent(inout),allocatable:: H(:,:)
integer :: m,n,l
integer :: i
l = size(H00,3)
if (size(Hu,3).lt.(l-1))then
   write(*,*)"ERROR in tridiagBloc! Hu(:,:,l) l=",size(Hu,3),"H00(:,:,l) l=",size(H00,3)
   !stop
endif
m = size(H00,1)
n = size(H00,2)
if (size(Hu,1).ne.m)then
   write(*,*)"ERROR in tridiagBloc! Hu(m,:,:) m=",size(Hu,1),"H00(m,:,:) m=",m
!   stop
endif
if (size(Hu,2).ne.n)then
   write(*,*)"ERROR in tridiagBloc! Hu(:,n,:) n=",size(Hu,2),"H00(:,n,:) n=",n
 !  stop
endif
Allocate(H(m*l,n*l))
H = CMPLX(0.0d0,0.0d0)
do i=1,l
   H((i-1)*m+1:i*m,(i-1)*n+1:i*n) = H00(:,:,i)
   if (i>1)then
      H((i-2)*m+1:(i-1)*m,(i-1)*n+1:i*n) = Hu(:,:,i-1)
      H((i-1)*m+1:i*m,(i-2)*n+1:(i-1)*n) = conjg(TRANSPOSE(Hu(:,:,i-1)))
   end if
enddo
end subroutine tridiagBloc_C

subroutine tridiagBloc_R(H00,Hu, H)
real(8),intent(in),dimension(:,:,:) :: H00,Hu
real(8),intent(inout),Allocatable :: H(:,:)
integer :: m,n,l
integer :: i
l = size(H00,3)
if (size(Hu,3).lt.(l-1))then
   write(*,*)"ERROR in tridiagBloc! Hu(:,:,l) l=",size(Hu,3),"H00(:,:,l) l=",size(H00,3)
!   stop
endif
m = size(H00,1)
n = size(H00,2)
if (size(Hu,1).ne.m)then
   write(*,*)"ERROR in tridiagBloc! Hu(m,:,:) m=",size(Hu,1),"H00(m,:,:) m=",m
 !  stop
endif
if (size(Hu,2).ne.n)then
   write(*,*)"ERROR in tridiagBloc! Hu(:,n,:) n=",size(Hu,2),"H00(:,n,:) n=",n
  ! stop
endif
Allocate(H(m*l,n*l))
H = 0.0d0
do i=1,l
   H((i-1)*m+1:i*m,(i-1)*n+1:i*n) = H00(:,:,i)
   if (i>1)then
      H((i-2)*m+1:(i-1)*m,(i-1)*n+1:i*n) = Hu(:,:,i-1)
      H((i-1)*m+1:i*m,(i-2)*n+1:(i-1)*n) = TRANSPOSE(Hu(:,:,i-1))
   end if
enddo
end subroutine tridiagBloc_R

subroutine triMUX_C(U1,H,U2,R,trA,trB,trC)
complex(8),intent(in),dimension(:,:) :: H,U1,U2
complex(8),intent(inout),allocatable,dimension(:,:) :: R
character,intent(in) :: trA,trB,trC
complex(8),allocatable,dimension(:,:) :: tmp

call MUX(U1,H,trA,trB,tmp)
call MUX(tmp,U2,'n',trC,R)
!write(*,*)'tmp',size(tmp,1),size(tmp,2)
deallocate(tmp)
end subroutine triMUX_C


subroutine MUX_C(A,B,trA,trB,R) 
complex(8),intent(in) :: A(:,:), B(:,:)
complex(8),intent(inout),allocatable :: R(:,:)
CHARACTER, intent(in) :: trA, trB
integer :: n,m,k,kb, lda,ldb
if((trA.ne.'n').and.(trA.ne.'N').and.(trA.ne.'t').and.(trA.ne.'T').and.(trA.ne.'c').and.(trA.ne.'C'))then
   write(*,*) "ERROR in MUX_C! trA is wrong: ",trA
!   stop
endif
if((trB.ne.'n').and.(trB.ne.'N').and.(trB.ne.'t').and.(trB.ne.'T').and.(trB.ne.'c').and.(trB.ne.'C'))then
   write(*,*) "ERROR in MUX_C! trB is wrong: ",trB
 !  stop
endif
lda = size(A,1)
ldb = size(B,1)
if ((trA.eq.'n').or.(trA.eq.'N'))then
   k = size(A,2)
   m = size(A,1)
else
   k = size(A,1)
   m = size(A,2)
endif
if ((trB.eq.'n').or.(trB.eq.'N'))then
   kb = size(B,1)
   n = size(B,2)
else
   kb = size(B,2)
   n = size(B,1)
endif
if(k.ne.kb)then
   write(*,*) "ERROR in MUX_C! Matrix dimension not conform",k,kb
!   stop
end if
if (not(allocated(R))) then
   allocate(R(m,n))   
else if((size(R,1).ne.m).or.(size(R,2).ne.n)) then
   deallocate(R)
   allocate(R(m,n))   
end if   
call zgemm(trA,trB,m,n,k,cmplx(1.0d0,0.0d0),A,lda,B,ldb,cmplx(0.0d0,0.0d0),R,m)
end subroutine MUX_C


subroutine MUX_R(A,B,trA,trB,R) 
real(8),intent(in) :: A(:,:), B(:,:)
real(8),intent(inout),allocatable :: R(:,:)
CHARACTER, intent(in) :: trA, trB
integer :: n,m,k,kb, lda,ldb
if((trA.ne.'n').and.(trA.ne.'N').and.(trA.ne.'t').and.(trA.ne.'T').and.(trA.ne.'c').and.(trA.ne.'C'))then
   write(*,*) "ERROR in MUX_R! trA is wrong",trA
 !  stop
endif
if((trB.ne.'n').and.(trB.ne.'N').and.(trB.ne.'t').and.(trB.ne.'T').and.(trB.ne.'c').and.(trB.ne.'C'))then
   write(*,*) "ERROR in MUX_R! trB is wrong",trB
  ! stop
endif
lda = size(A,1)
ldb = size(B,1)
if ((trA.eq.'n').or.(trA.eq.'N'))then
   k = size(A,2)
   m = size(A,1)
else
   k = size(A,1)
   m = size(A,2)
endif
if ((trB.eq.'n').or.(trB.eq.'N'))then
   kb = size(B,1)
   n = size(B,2)
else
   kb = size(B,2)
   n = size(B,1)
endif
if(k.ne.kb)then
   write(*,*) "ERROR in MUX_R! Matrix dimension not conform",k,kb
   !stop
end if

if (not(allocated(R))) then
   allocate(R(m,n))   
else if((size(R,1).ne.m).or.(size(R,2).ne.n)) then
   deallocate(R)
   allocate(R(m,n))   
end if

call dgemm(trA,trB,m,n,k,1.0d0,A,lda,B,ldb,0.0d0,R,m)
end subroutine MUX_R


subroutine malloc_c(A,x,y)
complex(8), dimension(:,:),allocatable,intent(inout) :: A
integer,intent(in):: x,y
if(not(allocated(A)))then
   allocate(A(x,y))
else if ((size(A,1).ne.x).OR.(size(A,2).ne.y))then
   deallocate(A)
   allocate(A(x,y))
end if
end subroutine malloc_c


subroutine  malloc_r(A,x,y)
real(8), dimension(:,:),allocatable,intent(inout) :: A
integer,intent(in):: x,y
if(not(allocated(A)))then
   allocate(A(x,y))
else if ((size(A,1).ne.x).OR.(size(A,2).ne.y))then
   deallocate(A)
   allocate(A(x,y))
end if
end subroutine malloc_r

end module matrix
