module mydata_module
implicit none
type mydata
   real(8) :: r
end type mydata

end module mydata_module

module mydata_list
  use mydata_module, LIST_DATA => mydata
  include "linked_list_template.f90"

  function DATA_COMPARE(d1,d2) result(b)
     type(LIST_DATA),intent(in) :: d1,d2
     logical :: b
     if (d1%r.eq.d2%r) then
        b = .true.
     else 
        b = .false.
     end if
   end function DATA_COMPARE

   subroutine DATA_FREE(d)
     type(LIST_DATA),intent(in) :: d
   end subroutine DATA_FREE
end module mydata_list
