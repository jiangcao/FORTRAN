program sparse_test 
use sparse_real 
implicit none 
integer :: i,j
real(8) :: b
real(8),allocatable :: x(:),a(:)
integer,allocatable :: ja(:),ia(:)
logical :: find

type(sparse_t),pointer :: sA


write(*,*) "Test Sparse module"

sA => sparse_create(5,5,axe='r')
write(*,*) 'nnz',sparse_nnz(sA)





write(*,*) 'put'
call sparse_put(sA,1,1,1.0d0)
call sparse_put(sA,1,2,-1.0d0)
call sparse_put(sA,1,4,-3.0d0)
call sparse_put(sA,2,1,-2.0d0)
call sparse_put(sA,2,2,5.0d0)
call sparse_put(sA,3,3,4.0d0)
call sparse_put(sA,3,4,6.0d0)
call sparse_put(sA,3,5,4.0d0)
call sparse_put(sA,4,1,-4.0d0)
call sparse_put(sA,4,3,2.0d0)
call sparse_put(sA,4,4,7.0d0)
call sparse_put(sA,5,2,8.0d0)
call sparse_put(sA,5,5,-5.0d0)

write(*,*)'get'
do i=1,5
   do j=1,5
      b = sparse_get(sA,i,j,find=find)
      write(*,*) i,j,b, find
   end do
End do

write(*,*)'get rows'
allocate(x(sparse_size(sA,2)))
do i=1,5
   call sparse_Row(sA,i,x)
   write(*,*) x
   write(*,*)
end do
deallocate(x)

write(*,*)'get cols'
allocate(x(sparse_size(sA,1)))
do i=1,5
   call sparse_Col(sA,i,x)
   write(*,*) x
   write(*,*)
end do
deallocate(x)

write(*,*)'To CSR format'
allocate(a(sparse_nnz(sA)))
allocate(ia(sparse_nnz(sA)))
allocate(ja(sparse_size(sA,1)+1))
call sparse_to_csr(sA,a,ia,ja)
write(*,*) 'values = ',a
write(*,*) 'columns = ',ia
write(*,*) 'rowIndex = ',ja
deallocate(a,ia,ja)

write(*,*) "Free"
call sparse_free(sA)
write(*,*) "End Test"
end program sparse_test
