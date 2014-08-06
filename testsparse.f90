program sparse_test 
use list 
use sparse 
implicit none 
integer :: i,j
real(8) :: b
real(8),allocatable :: x(:),a(:)
integer,allocatable :: ja(:),ia(:)
logical :: find

type(sparse_lil_t),pointer :: sA


write(*,*) "Test Sparse module"

call sparse_lil(sA,5,5,axe='r')
write(*,*) 'nnz',sparse_lil_nnz(sA)





write(*,*) 'put'
call rsparse_lil_put(sA,1,1,num=1.0d0)
call rsparse_lil_put(sA,1,2,num=-1.0d0)
call rsparse_lil_put(sA,1,4,num=-3.0d0)
call rsparse_lil_put(sA,2,1,num=-2.0d0)
call rsparse_lil_put(sA,2,2,num=5.0d0)
call rsparse_lil_put(sA,3,3,num=4.0d0)
call rsparse_lil_put(sA,3,4,num=6.0d0)
call rsparse_lil_put(sA,3,5,num=4.0d0)
call rsparse_lil_put(sA,4,1,num=-4.0d0)
call rsparse_lil_put(sA,4,3,num=2.0d0)
call rsparse_lil_put(sA,4,4,num=7.0d0)
call rsparse_lil_put(sA,5,2,num=8.0d0)
call rsparse_lil_put(sA,5,5,num=-5.0d0)

write(*,*)'get'
do i=1,5
   do j=1,5
      b = rsparse_lil_get(sA,i,j,find=find)
      write(*,*) i,j,b, find
   end do
End do

write(*,*)'get rows'
allocate(x(sparse_lil_size(sA,2)))
do i=1,5
   call rsparse_lil_getRow(sA,i,x)
   write(*,*) x
   write(*,*)
end do
deallocate(x)

write(*,*)'get cols'
allocate(x(sparse_lil_size(sA,1)))
do i=1,5
   call rsparse_lil_getCol(sA,i,x)
   write(*,*) x
   write(*,*)
end do
deallocate(x)

write(*,*)'To CSR format'
allocate(a(sparse_lil_nnz(sA)))
allocate(ia(sparse_lil_nnz(sA)))
allocate(ja(sparse_lil_size(sA,1)+1))
call rsparse_lil_to_csr(sA,a,ia,ja)
write(*,*) 'values = ',a
write(*,*) 'columns = ',ia
write(*,*) 'rowIndex = ',ja
deallocate(a,ia,ja)

write(*,*) "Free"
call sparse_lil_free(sA)
write(*,*) "End Test"
end program sparse_test
