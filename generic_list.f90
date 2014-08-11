! A generic linked-list object
module generic_list
use array
  implicit none

  ! Data type
  public :: list_t
  public :: list_data
  ! Methods
  public :: list_init
  public :: list_free
  public :: list_put
  public :: list_next
  public :: list_prev
  public :: list_count
  public :: list_node     
  public :: list_remove      
  public :: list_pop    

  interface list_contain
     module procedure list_contain_node, list_contain_data
  end interface list_contain

  interface list_insert
     module procedure list_insert_after_node, list_insert_at_index
  end interface list_insert

  interface list_get
     module procedure list_get_from_node,list_get_from_index
  end interface list_get
     

  ! A public variable to use as a MOLD for transfer()
  integer, dimension(:), allocatable :: list_data

  ! Linked List node data type
  type :: list_t
     private
     integer, dimension(:), pointer :: data => null()
     type(list_t), pointer :: next => null()
     type(list_t), pointer :: prev => null()
  end type list_t

contains

  ! Initialize a head node SELF and optionally store the provided DATA.
  subroutine list_init(self, data)
    type(list_t), pointer :: self
    integer, dimension(:), intent(in), optional :: data
    allocate(self)
    nullify(self%next)
    nullify(self%prev)
    if (present(data)) then
       allocate(self%data(size(data)))
       self%data = data
    else
       nullify(self%data)
    end if
  end subroutine list_init

  ! Free the entire list and all data, beginning at SELF
  subroutine list_free(self)
    type(list_t), pointer :: self
    type(list_t), pointer :: current
    type(list_t), pointer :: next
    current => self
    do while (associated(current))
       next => current%next
       if (associated(current%data)) then
          deallocate(current%data)
          nullify(current%data)
       end if
       deallocate(current)
       nullify(current)
       current => next
    end do
    nullify(self)
  end subroutine list_free

  ! Return the next node after SELF
  function list_next(self) result(next)
    type(list_t), pointer :: self
    type(list_t), pointer :: next
    next => self%next
  end function list_next

  ! Return the previous node before SELF
  function list_prev(self) result(prev)
    type(list_t), pointer :: self
    type(list_t), pointer :: prev
    prev => self%prev
  end function list_prev

  ! Insert a list node after SELF containing DATA (optional)
  subroutine list_insert_after_node(self, data)
    type(list_t), pointer :: self
    integer, dimension(:), intent(in), optional :: data
    type(list_t), pointer :: next,nextnext
    allocate(next)
    if (present(data)) then
       allocate(next%data(size(data)))
       next%data = data
    else
       nullify(next%data)
    end if
    nextnext => self%next
    next%next => nextnext
    self%next => next
    if (associated(nextnext)) nextnext%prev => next
    next%prev => self
  end subroutine list_insert_after_node

  ! Store the encoded DATA in list node SELF
  subroutine list_put(self, data)
    type(list_t), pointer :: self
    integer, dimension(:), intent(in) :: data
    if (associated(self%data)) then
       deallocate(self%data)
       nullify(self%data)
    end if
    allocate(self%data(size(data)))
    self%data = data
  end subroutine list_put

  ! Return the DATA stored in the node SELF
  function list_get_from_node(self) result(data)
    type(list_t), pointer :: self
    integer, dimension(:), pointer :: data
    data => self%data
  end function list_get_from_node
  ! Return the DATA stored in the node of INDEX
  function list_get_from_index(self,index) result(data)
    type(list_t), pointer :: self
    integer,intent(in) :: index
    integer, dimension(:), pointer :: data
    type(list_t),pointer :: node
    node => list_node(self,index)
    data => list_get_from_node(node)
  end function list_get_from_index

  ! Return the number of nodes in the list
  function list_count(self) result(nb)
    type(list_t), pointer :: self
    integer :: nb
    integer :: i
    type(list_t), pointer :: current    
    current => self
    i = 1
    do while (associated(current%next))
       current => current%next
       i = i+1
    end do
    nb = i
  end function list_count

  ! Find the ind-th node in the list
  ! if ind is greater than total number of nodes in the list, then return the last node in the list
  function list_node(self,ind) result(node)
    type(list_t),pointer :: self
    type(list_t),pointer :: node
    integer,intent(in) :: ind
    integer :: i
    type(list_t),pointer :: current
    current => self
    i = 1
    do while ((associated(current%next)).and.(i.lt.ind))
       current => current%next
       i = i+1
    end do
    node => current 
  end function list_node

  ! Insert a list node after an index containing DATA (optional)
  subroutine list_insert_at_index(self,ind,data)
    type(list_t),pointer :: self
    integer, intent(in) :: ind
    integer, dimension(:), intent(in),optional :: data
    type(list_t),pointer :: current     
    if (ind.eq.0) then   ! Insert a node at the head of the list
       allocate(current)
       if(present(data))then
          allocate(current%data(size(data)))
          current%data = DATA
       else 
          nullify(current%data)
       end if
       current%next => self
       nullify(current%prev)
       if (associated(self)) self%prev => current
       self => current
    else     
       current => list_node(self,ind)
       if (present(data)) then
          call list_insert_after_node(current,data)
       else
          call list_insert_after_node(current)
       end if
    end if
  end subroutine list_insert_at_index

  ! Remove the next node from the list, the DATA is deleted
  subroutine list_remove(self)
    type(list_t),pointer :: self
    type(list_t),pointer :: next_node,nextnext
    next_node => self%next
    nextnext => next_node%next
    self%next => next_node%next
    nextnext%prev => self
    if (associated(next_node%data)) then
       deallocate(next_node%data)
       nullify(next_node%data)
    end if
    deallocate(next_node)
    nullify(next_node)
  end subroutine list_remove

  ! Pop a node from list, index is passed in, return the node
  subroutine list_pop(self,ind,node)
    type(list_t),pointer :: self
    integer,intent(in) :: ind
    type(list_t),pointer :: node
    type(list_t),pointer :: current
    if (ind.eq.1) then
       node => self
       self => self%next
       nullify(self%prev)
    else           
       current => list_node(self,ind-1) ! find the node before the one to pop

       node => current%next
       current%next => node%next
       node%next%prev => current
    end if
  end subroutine list_pop
    
  ! To test if a NODE is in the list SELF
  function list_contain_node(self,node) result(b)
    type(list_t),pointer :: self, node 
    type(list_t),pointer :: ptr
    logical :: b, tmp
    ptr => self
    tmp = .false.
    do while (associated(ptr))
       if(associated(ptr,node)) then
          tmp = .true.
          exit
       end if
       ptr => ptr%next
    end do
    b = tmp
  end function list_contain_node

  ! To test if a DATA is in the list SELF
  function list_contain_data(self,data) result(b)
    type(list_t),pointer :: self, ptr
    integer,dimension(:),intent(in) :: data 
    integer,dimension(:),pointer :: data2
    logical :: b, tmp
    ptr => self
    tmp = .false.
    do while (associated(ptr))             
       if(array_equal(list_get(ptr),data)) then
          tmp = .true.
          exit
       end if
       ptr => ptr%next
    end do
    b = tmp
  end function list_contain_data
   
end module generic_list
