module mydata_module
    implicit none
    type mydata
        real(8),allocatable :: r(:,:)
    end type mydata

contains
    function DATA_CREATE() result(d_ptr)
        type(mydata),pointer     :: d_ptr
        allocate(d_ptr)
        return
    end function DATA_CREATE

    subroutine DATA_FREE(d)
        type(mydata),pointer :: d
        if (allocated(d%r)) deallocate(d%r)
        deallocate(d)
        nullify(d)
    end subroutine DATA_FREE
end module mydata_module



module mygraph
    use mydata_module, GRAPH_ARC_DATA => mydata, GRAPH_NODE_DATA => mydata, NODE_data_free => data_free, ARC_data_free => data_free
    implicit none
    include "graph_template.f90"
end module mygraph



