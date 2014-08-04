 ! A generic graph object
module graph
use list
   implicit none

   public :: graph_t
   public :: graph_node_t
   public :: graph_arc_t
   public :: graph_data

   public :: graph_init
   public :: graph_node_add
   public :: graph_connect
   public :: graph_free
   public :: graph_list_node
   public :: graph_list_arc

   public :: node_decode
   public ::  arc_decode
   
   interface graph_get_slice   ! get a slice of nodes
      module procedure graph_get_slice_from_slice   ! Provide a list of nodes
      module procedure graph_get_slice_from_node    ! Provide a single node
   end interface graph_get_slice
   
   interface graph_encode
      module procedure node_encode,arc_encode
   end interface graph_encode

   interface graph_put
      module procedure graph_node_put, graph_arc_put  ! put DATA
   end interface

   interface graph_get
      module procedure graph_node_get,graph_arc_get  ! get DATA
      module procedure graph_node_get_arcs           ! get a list of node's arcs
      module procedure graph_arc_get_node            ! get node connected to arc
   end interface

   private :: graph_arc_ptr
   private :: graph_node_ptr

   ! A public variable to use as a MOLD for transfer()
   integer, dimension(:), allocatable :: graph_data

   ! Graph data type
   type :: graph_t
!      private
      type(list_t),pointer :: node_list, arc_list
   end type graph_t

   ! Graph node data type
   type :: graph_node_t
      private
      integer,dimension(:),pointer :: data => null()
      ! list of pointers of arcs
      type(list_t),pointer :: left => null()
      type(list_t),pointer :: right => null()
   end type graph_node_t
   ! Pointer to node, needed for storing pointer in the list object
   type :: graph_node_ptr
      private
      type(graph_node_t),pointer :: p
   end type graph_node_ptr

   ! Graph arc data type
   type :: graph_arc_t
      private
      integer,dimension(:),pointer :: data => null()
      type(graph_node_t),pointer :: left => null()
      type(graph_node_t),pointer :: right => null()
   end type graph_arc_t
   ! Pointer to arc, needed for storing pointer in the list object
   type :: graph_arc_ptr
      private
      type(graph_arc_t),pointer :: p
   end type graph_arc_ptr



contains

   ! Free a graph SELF
   subroutine graph_free(self)
     type(graph_t),pointer :: self
     type(list_t),pointer :: pt
     type(graph_node_t),pointer :: node
     type(graph_arc_t),pointer :: arc
     pt => self%node_list  ! remove all nodes
     do while (associated(pt))
        if(associated(list_get(pt)))then
           node => node_decode(list_get(pt))
           if(associated(node%data))then
              deallocate(node%data)
              nullify(node%data)
           end if
           call list_free(node%left)
           call list_free(node%right)
        end if
        pt => list_next(pt)
     end do
     pt => self%arc_list  ! remove all arcs
     do while (associated(pt))
        if(associated(list_get(pt)))then 
           arc => arc_decode(list_get(pt))
           if(associated(arc%data))then
              deallocate(arc%data)
              nullify(arc%data)
           end if
        end if
        pt => list_next(pt)
     end do
     call list_free(self%node_list)
     call list_free(self%arc_list)
   end subroutine graph_free


   ! Initialize a graph SELF
   subroutine graph_init(self)
     type(graph_t),pointer :: self
     allocate(self)
     nullify(self%node_list)
     nullify(self%arc_list)
   end subroutine graph_init
   
  
   ! Add a node in the graph SELF and optionally store the provided DATA and optionally return the pointer to the node
   subroutine graph_node_add(self,data,pt)
     type(graph_t),pointer :: self
     integer,dimension(:),intent(in),optional :: DATA
     type(graph_node_t),pointer,optional :: pt
     type(graph_node_t),pointer :: node => null()
     allocate(node)
     nullify(node%left)
     nullify(node%right)
     if (present(data))then
        allocate(node%data(size(data)))
        node%data = DATA
     else 
        nullify(node%data)
     end if
     !! Careful about the first node
     if (associated(self%node_list)) then
        call list_insert(self%node_list,0,data=node_encode(node))
     else 
        call list_init(self%node_list,data=node_encode(node))
     end if
     if (present(pt)) pt => node  ! return pointer to node
   end subroutine graph_node_add


   ! Add an arc by connecting LNODE and RNODE and optionally store the provided DATA and optionally return the pointer to the arc
   subroutine graph_connect(self,lnode,rnode,data,pt)
     type(graph_t),pointer :: self
     type(graph_node_t),pointer :: lnode,rnode 
     integer,dimension(:),intent(in),optional :: DATA
     type(graph_arc_t),pointer,optional :: pt
     type(graph_arc_t),pointer :: arc=>null()
     allocate(arc)
     arc%left => lnode
     arc%right => rnode 
     if(present(data)) then
        allocate(arc%data(size(data)))
        arc%data = DATA
     else
        nullify(arc%data)
     end if
     ! Careful for the first link
     if (associated(self%arc_list)) then 
        call list_insert(self%arc_list,0,data=arc_encode(arc))
     else 
        call list_init(self%arc_list,data=arc_encode(arc))
     end if
     if (associated(lnode%right)) then
        call list_insert(lnode%right,0, DATA=arc_encode(arc))
     else 
        call list_init(lnode%right, DATA=arc_encode(arc))
     end if
     if (associated(rnode%left)) then 
        call list_insert(RNODE%left,0,DATA=arc_encode(arc))
     else 
        call list_init(rnode%left, DATA=arc_encode(arc))
     end if
     if(present(pt)) pt => arc 
   end subroutine graph_connect

   ! Return all the nodes LIST_OUT connected to the provided NODE
   function graph_get_slice_from_node(node,char) result(list_out)
     type(graph_node_t),pointer :: node, node2
     character(len=1), intent(in) :: char
     type(list_t),pointer :: list_out, list_arcs, pt
     type(graph_arc_t),pointer :: arc
     logical :: b
     integer :: i
     call list_init(list_out)
     list_arcs => graph_node_get_arcs(node,char)
     pt => list_arcs
     do while (associated(pt))
        arc => arc_decode(list_get(pt))
        if(associated(arc)) then
           node2 => graph_arc_get_node(arc,char)
           if (not(list_contain(list_out,data=node_encode(node2)))) then
              ! Careful for the 1st element
              if (associated(list_get(list_out))) then
                 call list_insert(list_out,0,data=node_encode(node2))
              else 
                 call list_put(list_out,data=node_encode(node2))
              end if
           end if
        end if
        pt => list_next(pt)
     end do
   end function graph_get_slice_from_node

   ! Return the neighbour slice LIST_OUT of the provided slice LIST_IN
   function graph_get_slice_from_slice(list_in,char) result(list_out)
     type(list_t), pointer :: list_in, list_out, pt, list_arcs, pt2
     type(graph_node_t),pointer :: node, node2
     type(graph_arc_t),pointer :: arc
     character(len=1),intent(in) :: char
     call list_init(list_out)
     pt => list_in
     do while (associated(pt))
        node => node_decode(list_get(pt))
        list_arcs => graph_node_get_arcs(node,char)
        pt2 => list_arcs
        do while (associated(pt2))
           arc => arc_decode(list_get(pt2))
           if(associated(arc))then 
              node2 => graph_arc_get_node(arc,char)
              if(not(list_contain(list_out,node_encode(node2)))) then
                 ! Careful for the 1st element
                 if(associated(list_get(list_out))) then
                    call list_insert(list_out,0,data=node_encode(node2))
                 else 
                    call list_put(list_out,data=node_encode(node2))
                 end if
              end if
           end if
           pt2 => list_next(pt2)
        end do
        pt => list_next(pt)
     end do
   end function graph_get_slice_from_slice







   ! Return the DATA stored in the node SELF
   function graph_node_get(self) result(data)
     type(graph_node_t),pointer :: self
     integer, dimension(:),pointer :: DATA
     data => self%data
   end function graph_node_get
     
   ! Return the DATA stored in the arc SELF
   function graph_arc_get(self) result(data)
     type(graph_arc_t),pointer :: self
     integer, dimension(:),pointer :: DATA
     data => self%data
   end function graph_arc_get

   ! Store the encoded DATA in node SELF
   subroutine graph_node_put(self,data)
     type(graph_node_t),pointer :: self
     integer, dimension(:), intent(in) :: data 
     
     if (associated(self%data)) then
        deallocate(self%data) 
        nullify(self%data)
     end if
     allocate(self%data(size(data)))
     self%data = DATA
   end subroutine graph_node_put
   
   ! Store the encoded DATA in arc SELF
   subroutine graph_arc_put(self,data)
     type(graph_arc_t),pointer :: self
     integer, dimension(:), intent(in) :: data 
     
     if (associated(self%data)) then
        deallocate(self%data) 
        nullify(self%data)
     end if
     allocate(self%data(size(data)))
     self%data = DATA
   end subroutine graph_arc_put

   ! Get a list of arcs connected to the node SELF
   Function graph_node_get_arcs(self,char) result(arcs)
     type(graph_node_t),pointer :: self 
     type(list_t),pointer :: arcs
     character(len=1),intent(in) :: char
     if((char.eq.'l').or.(char.eq.'L'))then
        arcs => self%left
     else 
        arcs => self%right
     end if
   end Function graph_node_get_arcs

   ! Get the node connected to the arc SELF
   function graph_arc_get_node(SELF,char) result(node)
     type(graph_arc_t),pointer :: self
     character(len=1),intent(in) :: char
     type(graph_node_t),pointer ::node 
     if((char.eq.'l').or.(char.eq.'L'))then 
        node => self%left
     else 
        node => self%right
     end if
   end function graph_arc_get_node
     
   ! Encode a node pointer to generic data 
   function node_encode(pt) result(data)
     type(graph_node_t),pointer :: pt
     type(graph_node_ptr) :: ptr
     integer, dimension(:),allocatable :: data
     ptr%p => pt
     allocate(data(size(transfer(ptr,data))))
     data = transfer(ptr,data)
   end function node_encode
   
   ! Encode an arc pointer to generic data 
   function arc_encode(pt) result(data)
     type(graph_arc_t),pointer :: pt
     type(graph_arc_ptr) :: ptr
     integer, dimension(:),allocatable :: data 
     ptr%p => pt
     allocate(data(size(transfer(ptr,data))))
     data = transfer(ptr,data)
   end function arc_encode

   ! Decode a generic data to node pointer
   function node_decode(code) result(pt)
     integer, dimension(:),intent(in) :: code
     type(graph_node_t),pointer :: pt
     type(graph_node_ptr) :: ptr
     ptr = transfer(code,ptr)
     pt => ptr%p
   end function node_decode
   ! Decode a generic data to arc pointer
   function arc_decode(code) result(pt)
     integer, dimension(:),intent(in) :: code
     type(graph_arc_t),pointer :: pt
     type(graph_arc_ptr) :: ptr
     ptr = transfer(code,ptr)
     pt => ptr%p
   end function arc_decode

   ! Return the node list of graph SELF
   function graph_list_node(self) result(l)
     type(graph_t),pointer:: self
     type(list_t),pointer :: l
     l => self%node_list
   end function graph_list_node

   ! Return the arc list of graph SELF
   function graph_list_arc(self) result(l)
     type(graph_t),pointer:: self
     type(list_t),pointer:: l
     l => self%arc_list
   end function graph_list_arc
 end module graph
