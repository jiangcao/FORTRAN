! A linked graph object template
! Only use pointer to types, assignement =>
! User needs to define data type stored in the list :  GRAPH_ARC_DATA & GRAPH_NODE_DATA
! User needs to define Methods for the data type         
!         NODE_data_free(GRAPH_NODE_DATA,pointer :: d)
!         ARC_data_free(GRAPH_ARC_DATA,pointer :: d)
private
!!! TYPEs
public :: GRAPH
public :: GRAPH_ARC
public :: GRAPH_ARC_PTR
public :: GRAPH_ARC_LIST
public :: GRAPH_NODE
public :: GRAPH_NODE_PTR
public :: GRAPH_NODE_LIST
!!! Constructor
public :: graph_create
public :: graph_node_list_create
public :: graph_node_create
public :: graph_arc_create
public :: graph_arc_list_create
!!! Destructor
public :: graph_node_list_free
public :: graph_arc_list_free
public :: graph_node_free
public :: graph_arc_free
public :: graph_free
!!! Methodes
public :: graph_node_list_add
public :: graph_arc_list_add
public :: graph_node_list_get
public :: graph_arc_list_get
public :: graph_add_node
public :: graph_connect_nodes
public :: graph_node_get_data
public :: graph_node_put_data  
public :: graph_arc_get_data
public :: graph_arc_put_data
public :: graph_node_get_arc
public :: graph_node_count_arcs
public :: graph_arc_get_node
!!!!

type GRAPH_ARC 
    private
    type(GRAPH_ARC_DATA),pointer:: data => null()
    type(GRAPH_NODE),pointer    :: left_node => null(), right_node => null()
end type GRAPH_ARC

type GRAPH_ARC_PTR
    private
    type(GRAPH_ARC),pointer :: pt => null()
end type GRAPH_ARC_PTR

type GRAPH_ARC_LIST
    private
    type(GRAPH_ARC_PTR),allocatable :: arcs(:)
    integer :: logic_len
end type GRAPH_ARC_LIST

type GRAPH_NODE
    private
    type(GRAPH_NODE_DATA),pointer :: data => null() 
    type(GRAPH_ARC_LIST),pointer  :: left => null(), right => null()
end type GRAPH_NODE

type GRAPH_NODE_PTR
    private
    type(GRAPH_NODE),pointer :: pt => null()
end type GRAPH_NODE_PTR

type GRAPH_NODE_LIST
    private
    type(GRAPH_NODE_PTR), allocatable :: nodes(:)
    integer :: logic_len
end type GRAPH_NODE_LIST

type GRAPH
    type(GRAPH_NODE_LIST),pointer  :: node_list => null()
    type(GRAPH_ARC_LIST),pointer   :: arc_list => null()
end type GRAPH


contains

    !!!
    !!! Construction Methods
    !!!

    function graph_create() result(this)
        type(GRAPH),pointer :: this
        allocate(this)
        this%node_list => graph_node_list_create()
        this%arc_list => graph_arc_list_create()
        return
    end function graph_create

    function graph_node_list_create() result(this)
        type(GRAPH_NODE_LIST),pointer :: this
        allocate(this)
        allocate(this%nodes(4))
        this%logic_len = 0
    end function graph_node_list_create

    function graph_arc_list_create() result(this)
        type(GRAPH_ARC_LIST),pointer :: this
        allocate(this)
        allocate(this%arcs(4))
        this%logic_len = 0
    end function graph_arc_list_create

    function graph_node_create(data) result(this)
        type(GRAPH_NODE),pointer                :: this 
        type(GRAPH_NODE_DATA),pointer,optional  :: data
        allocate(this)
        if (present(data)) then 
            this%data => data 
        else
            nullify(this%data)
        end if
        this%left => graph_arc_list_create()
        this%right => graph_arc_list_create()
        return
    end function graph_node_create

    function graph_arc_create(data) result(this)
        type(GRAPH_ARC),pointer               :: this
        type(GRAPH_ARC_DATA),pointer,optional :: data
        allocate(this)
        if (present(data)) then
            this%data => data
        else
            nullify(this%data)
        end if
        return
    end function graph_arc_create

    !!!
    !!! Destruction Methods
    !!!

    subroutine graph_node_list_free(this)
        type(GRAPH_NODE_LIST),pointer :: this
        if (not(associated(this))) return
        deallocate(this%nodes)
        deallocate(this)
        nullify(this)
    end subroutine graph_node_list_free

    subroutine graph_arc_list_free(this)
        type(GRAPH_ARC_LIST),pointer :: this
        if (not(associated(this))) return
        deallocate(this%arcs)
        deallocate(this)
        nullify(this)
    end subroutine graph_arc_list_free

    subroutine graph_node_free(this,freedata) 
        type(GRAPH_NODE), pointer   :: this
        logical,intent(in)          :: freedata 
        if (not(associated(this))) return
        if (freedata .eq. .true.) then
            call NODE_data_free(this%data)
        end if
        call graph_arc_list_free(this%left)
        call graph_arc_list_free(this%right)
        deallocate(this)
        nullify(this)
    end subroutine graph_node_free

    subroutine graph_arc_free(this,freedata)
        type(GRAPH_ARC),pointer :: this
        logical,intent(in)      :: freedata
        if (not(associated(this))) return
        if (freedata .eq. .true.) then
            call ARC_data_free(this%data)
        end if
        deallocate(this)
        nullify(this)
    end subroutine graph_arc_free

    subroutine graph_free(this,freeNodeData,freeArcData)
        type(GRAPH),pointer :: this
        logical,intent(in)  :: freeNodeData,freeArcData
        integer :: i
        if (not(associated(this))) return
        do i = 1,this%node_list%logic_len  ! free nodes
        call graph_node_free(this%node_list%nodes(i)%pt,freeNodeData)
        end do
        call graph_node_list_free(this%node_list)      ! free nodes list
        do i = 1,this%arc_list%logic_len  ! free arcs
        call graph_arc_free(this%arc_list%arcs(i)%pt,freeArcData)
        end do
        call graph_arc_list_free(this%arc_list)       ! free arcs list
        deallocate(this)
        nullify(this)
    end subroutine graph_free

    !!!!
    !!!!
    !!!!


    subroutine graph_node_list_add(this, new_node)
        type(GRAPH_NODE_LIST),pointer :: this
        type(GRAPH_NODE),pointer      :: new_node
        type(GRAPH_NODE_PTR),allocatable:: tmp(:)
        integer :: alloc_len
        this%logic_len = this%logic_len + 1
        alloc_len = size(this%nodes)
        if (this%logic_len .gt. alloc_len) then 
            allocate(tmp(alloc_len * 2))
            tmp = this%nodes
            call move_alloc(tmp,this%nodes)
        end if
        this%nodes(this%logic_len)%pt => new_node 
    end subroutine graph_node_list_add

    function graph_node_list_get(this,ind) result(node)
        type(GRAPH_NODE_LIST),pointer :: this
        integer,intent(in)            :: ind
        type(GRAPH_NODE),pointer      :: node
        node => this%nodes(ind)%pt
        return
    end function graph_node_list_get


    subroutine graph_arc_list_add(this, new_arc)
        type(GRAPH_ARC_LIST),pointer :: this
        type(GRAPH_ARC),pointer      :: new_arc
        type(GRAPH_ARC_PTR),allocatable:: tmp(:)
        integer :: alloc_len
        this%logic_len = this%logic_len + 1
        alloc_len = size(this%arcs)
        if (this%logic_len .gt. alloc_len) then 
            allocate(tmp(alloc_len * 2))
            tmp = this%arcs
            call move_alloc(tmp,this%arcs)
        end if
        this%arcs(this%logic_len)%pt => new_arc
    end subroutine graph_arc_list_add

    function graph_arc_list_get(this, ind) result(arc)
        type(GRAPH_ARC_LIST),pointer :: this
        integer,intent(in)           :: ind
        type(GRAPH_ARC),pointer      :: arc
        arc => this%arcs(ind)%pt
        return
    end function graph_arc_list_get

    subroutine graph_add_node(this,new_node)
        type(GRAPH),pointer :: this 
        type(GRAPH_NODE),pointer :: new_node
        call graph_node_list_add(this%node_list,new_node)
    end subroutine graph_add_node

    subroutine graph_connect_nodes(this,new_arc,node1,node2)
        type(GRAPH),pointer             :: this 
        type(GRAPH_NODE),pointer        :: node1,node2
        type(GRAPH_ARC),pointer         :: new_arc
        type(GRAPH_ARC_PTR),allocatable :: tmp(:)
        call graph_arc_list_add(this%arc_list,new_arc)  ! save arc to graph
        call graph_arc_list_add(node1%right,new_arc)            ! connect node1%right to arc
        call graph_arc_list_add(node2%left,new_arc)             ! connect node2%left to arc
        new_arc%left_node => node1                      ! connect arc%left to node1 
        new_arc%right_node => node2                     ! connect arc%right to node2
    end subroutine graph_connect_nodes

    function graph_node_get_data(this) result(data)
        type(GRAPH_NODE),pointer        :: this 
        type(GRAPH_NODE_DATA),pointer   :: data 
        data => this%data
    end function graph_node_get_data

    subroutine graph_node_put_data(this,data) 
        type(GRAPH_NODE),pointer        :: this 
        type(GRAPH_NODE_DATA),pointer   :: data 
        this%data => data 
    end subroutine graph_node_put_data

    function graph_arc_get_data(this) result(data)
        type(GRAPH_ARC),pointer         :: this 
        type(GRAPH_ARC_DATA),pointer    :: data 
        data => this%data
    end function graph_arc_get_data

    subroutine graph_arc_put_data(this,data)
        type(GRAPH_ARC),pointer :: this 
        type(GRAPH_ARC_DATA),pointer :: data 
        this%data => data
    end subroutine graph_arc_put_data

    function graph_node_get_arc(this,side,ind) result(arc) 
        type(GRAPH_NODE),pointer    :: this 
        type(GRAPH_ARC),pointer     :: arc
        character(len=1),intent(in) :: side 
        integer,intent(in)          :: ind
        arc => null()
        if (side.eq.'l') then
            arc => this%left%arcs(ind)%pt
        else 
            arc => this%right%arcs(ind)%pt
        end if
        return
    end function graph_node_get_arc

    function graph_node_count_arcs(this,side) result(nb)
        type(GRAPH_NODE),pointer    :: this
        character(len=1),intent(in) :: side
        integer                     :: nb
        if (side.eq.'l') then
            nb = this%left%logic_len
        else
            nb = this%right%logic_len
        end if
        return
    end function graph_node_count_arcs

    function graph_arc_get_node(this,side) result(node)
        type(GRAPH_ARC), pointer :: this 
        type(GRAPH_NODE), pointer :: node
        character(len=1),intent(in) :: side 
        node => null()
        if (side.eq.'l') then 
            node => this%left_node
        else 
            node => this%right_node
        end if
        return
    end function graph_arc_get_node




