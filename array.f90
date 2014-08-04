module array
   implicit none

   private
   public :: array_equal


contains

  function array_equal( array1, array2 ) result(equal)
    integer, dimension(:), intent(in) :: array1, array2
    integer :: i
    logical :: equal
    equal =size(array1) == size(array2)
    if ( equal ) then
       do i = 1,size(array1)
          equal = array1(i) == array2(i)
          if ( .not. equal )exit
       enddo
    endif
  end function array_equal

end module array
