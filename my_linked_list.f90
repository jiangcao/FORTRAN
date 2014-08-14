module mydata_module
    implicit none
    type mydata
        real(8) :: r
    end type mydata
contains
    function DATA_CREATE(r) result(d_ptr)
        type(mydata),pointer     :: d_ptr
        real,intent(in),optional :: r
        allocate(d_ptr)
        if (present(r)) then
            d_ptr%r = r
        else
            d_ptr%r = 0.0d0
        end if
        return
    end function DATA_CREATE

    function DATA_COMPARE(d1,d2) result(b)
        type(mydata),pointer :: d1,d2
        logical :: b
        if (d1%r.eq.d2%r) then
            b = .true.
        else 
            b = .false.
        end if
    end function DATA_COMPARE

    subroutine DATA_FREE(d)
        type(mydata),pointer :: d
        deallocate(d)
        nullify(d)
    end subroutine DATA_FREE
end module mydata_module

module mydata_list
    use mydata_module, LIST_DATA => mydata
    implicit none
    include "linked_list_template.f90"

end module mydata_list
