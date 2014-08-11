! A linked graph object template
! Only use pointer to type GRAPH & GRAPH_NODE & GRAPH_ARC, assignement =>
! User needs to define data type stored in the list :  GRAPH_ARC_DATA & GRAPH_NODE_DATA
! User needs to define Methods for the data type 
!         logical :: data_compare_NODE(GRAPH_NODE_DATA :: d1,d2)
!         logical :: data_compare_ARC(GRAPH_ARC_DATA :: d1,d2)
!         NODE_data_free(GRAPH_NODE_DATA :: d)
!         ARC_data_free(GRAPH_ARC_DATA :: d)
private
public

   type GRAPH_ARC 
      type(GRAPH_ARC_DATA)     :: data
      type(GRAPH_NODE),pointer :: left_node => null(), right_node => null()
   end type GRAPH_ARC

   type GRAPH_ARC_PTR
      type(GRAPH_ARC),pointer :: pt => null()
   end type GRAPH_ARC_PTR

   type GRAPH_ARC_LIST
      type(GRAPH_ARC_PTR),allocatable :: arcs(:)
      integer :: logic_len
   end type GRAPH_ARC_LIST

   type GRAPH_NODE
      type(GRAPH_NODE_DATA)            :: data 
      type(GRAPH_ARC_LIST),pointer     :: left, right
   end type GRAPH_NODE

   type GRAPH_NODE_PTR
      type(GRAPH_NODE),pointer :: pt => null()
   end type GRAPH_NODE_PTR
   
   type GRAPH_NODE_LIST
      type(GRAPH_NODE_PTR), allocatable :: nodes(:)
      integer :: logic_len
   end type GRAPH_NODE_LIST

   type GRAPH
      type(GRAPH_NODE_LIST),pointer  :: node_list
      type(GRAPH_ARC_LIST),pointer   :: arc_list
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
    type(GRAPH_NODE),pointer :: this 
    type(GRAPH_NODE_DATA),intent(in) :: data
    allocate(this)
    this%data = data 
    this%left => graph_arc_list_create()
    this%right => graph_arc_list_create()
    return
  end function graph_node_create

  function graph_arc_create(data) result(this)
    type(GRAPH_ARC),pointer :: this
    type(GRAPH_ARC_DATA),intent(in) :: data
    allocate(this)
    this%data = data
    return
  end function graph_arc_create

!!!
!!! Destruction Methods
!!!

  subroutine graph_node_list_free(this)
    type(GRAPH_NODE_LIST),pointer :: this
    if (not(associated(this))) exit
    deallocate(this%nodes)
    deallocate(this)
    nullify(this)
  end subroutine graph_node_list_free

  subroutine graph_arc_list_free(this)
    type(GRAPH_ARC_LIST),pointer :: this
    if (not(associated(this))) exit
    deallocate(this%arcs)
    deallocate(this)
  end subroutine graph_arc_list_free

  subroutine graph_node_free(this) 
    type(GRAPH_NODE), pointer :: this
    if (not(associated(this))) exit
    call NODE_data_free(this%data)
    call graph_arc_list_free(this%left)
    call graph_arc_list_free(this%right)
    deallocate(this)
    nullify(this)
  end subroutine graph_node_free

  subroutine graph_arc_free(this)
    type(GRAPH_ARC),pointer :: this
    if (not(associated(this))) exit
    call ARC_data_free(this%data)
    deallocate(this)
    nullify(this)
  end subroutine graph_arc_free

  subroutine graph_free(this)
    type(GRAPH),pointer :: this
    integer :: i
    if (not(associated(this))) exit
    do i = 1,this%node_list%logic_len  ! free nodes
       call graph_node_free(this%node_list%nodes(i)%pt)
    end do
    call graph_node_list_free(this%node_list)      ! free nodes list
    do i = 1,this%arc_list%logic_len  ! free arcs
       call graph_arc_free(this%arc_list%arcs(i)%pt)
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

  subroutine graph_add_node(this,new_node)
    type(GRAPH),pointer :: this 
    type(GRAPH_NODE),pointer :: new_node
    call graph_node_list_add(this%node_list,new_node)
  end subroutine graph_add_node

  subroutine graph_connect_nodes(this,new_arc,node1,node2)
    type(GRAPH),pointer             :: this 
    type(GRAPH_NODE),pointer        :: node1,node2
    type(GRAPH_ARC_PTR),allocatable :: tmp(:)
    call graph_arc_list_add(this%arc_list,new_arc)  ! save arc to graph
    call graph_arc_list_add(node1%right)            ! connect node1%right to arc
    call graph_arc_list_add(node2%left)             ! connect node2%left to arc
    new_arc%left_node => node1                      ! connect arc%left to node1 
    new_arc%right_node => node2                     ! connect arc%right to node2
  end subroutine graph_connect_nodes

  function graph_node_get_data(this) result(data)
    type(GRAPH_NODE),pointer :: this 
    type(GRAPH_NODE_DATA) :: data 
    data = this%data
  end function graph_node_get_data

  subroutine graph_node_put_data(this,data) 
    type(GRAPH_NODE),pointer :: this 
    type(GRAPH_NODE_DATA),intent(in) :: data 
    this%data = data 
  end subroutine graph_node_put_data

  function graph_arc_get_data(this) result(data)
    type(GRAPH_ARC),pointer :: this 
    type(GRAPH_ARC_DATA) :: data 
    data = this%data
  end function graph_arc_get_data

  subroutine graph_arc_put_data(this,data)
    type(GRAPH_ARC),pointer :: this 
    type(GRAPH_ARC_DATA),intent(in) :: data 
    this%data = data
  end subroutine graph_arc_put_data

  function graph_node_get_arcs(this,side) result(list) 
    type(GRAPH_NODE),pointer :: this 
    type(GRAPH_ARC_LIST),pointer :: list 
    character(len=1),intent(in) :: side 
    if (side.eq.'l') then 
       list => this%left
    else 
       list => this%right
    end if
    return
  end function graph_node_get_arcs

  function graph_arc_get_node(this,side) result(node)
    type(GRAPH_ARC), pointer :: this 
    type(GRAPH_NODE), pointer :: node 
    character(len=1),intent(in) :: side 
    if (side.eq.'l') then 
       node => this%left_node
    else 
       node => this%right_node
    end if
    return
  end function graph_arc_get_node

    

    
