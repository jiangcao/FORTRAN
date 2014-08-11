! A linked-list object template
! Only use pointer to type LINKED_LIST, assignement =>
! User needs to define data type stored in the list :  LIST_DATA
! User needs to define Methods for the data type 
!         logical :: data_compare(LIST_DATA :: d1,d2)
!         data_free(LIST_DATA :: d)

private
public LINKED_LIST, list_create, list_free, list_next, list_prev, list_insert_after_node, list_insert_at_index, list_pop, list_contain_node,list_contain_data,list_get_from_node,list_get_from_index

  type LINKED_LIST
     type(LIST_DATA)       :: data
     type(LINKED_LIST),pointer :: next => null()
     type(LINKED_LIST),pointer :: prev => null()
  end type LINKED_LIST

contains

  ! Construct a head node THIS and store the provided DATA
  ! Arguments:
  !     this       Pointer to new linked-list
  !     data       The data for the first element
  function list_create(data) result(this)
    type(LINKED_LIST), pointer :: this
    type(LIST_DATA),intent(in) :: data
    allocate(this)
    nullify(this%next)
    nullify(this%prev)
    this%data = data
    return
  end function list_create

  ! Free the entire list and all data, beginning at THIS
  ! Arguments:
  !     this       Pointer to the list to be destroyed
  subroutine list_free(this)
    type(LINKED_LIST), pointer :: this
    type(LINKED_LIST), pointer :: current
    type(LINKED_LIST), pointer :: next
    current => this
    do while (associated(current))
       next => current%next
       call data_free(current%data)
       deallocate(current)
       nullify(current)
       current => next
    end do
  end subroutine list_free

  ! Return the next node after THIS
  function list_next(this) result(next)
    type(LINKED_LIST), pointer :: this
    type(LINKED_LIST), pointer :: next
    next => this%next
  end function list_next

  ! Return the previous node before THIS
  function list_prev(this) result(prev)
    type(LINKED_LIST), pointer :: this
    type(LINKED_LIST), pointer :: prev
    prev => this%prev
  end function list_prev

  ! Insert a list node after THIS containing DATA
  subroutine list_insert_after_node(this, data)
    type(LINKED_LIST), pointer :: this
    type(LIST_DATA),intent(in) :: data
    type(LINKED_LIST), pointer :: next,nextnext
    allocate(next)
    next%data = data
    nextnext  => this%next
    next%next => nextnext
    this%next => next
    if (associated(nextnext)) nextnext%prev => next
    next%prev => this
  end subroutine list_insert_after_node

  ! Store the encoded DATA in list node THIS
  subroutine list_put(this, data)
    type(LINKED_LIST), pointer :: this
    type(LIST_DATA),intent(in) :: data
    this%data = data
  end subroutine list_put

  ! Return the DATA stored in the node THIS
  function list_get_from_node(this) result(data)
    type(LINKED_LIST), pointer  :: this
    type(LIST_DATA)             :: data
    data = this%data
  end function list_get_from_node

  ! Return the DATA stored in the node of INDEX
  function list_get_from_index(this,index) result(data)
    type(LINKED_LIST), pointer  :: this
    integer,intent(in)          :: index
    type(LIST_DATA)             :: data
    type(LINKED_LIST), pointer  :: node
    node => list_node(this,index)
    data = list_get_from_node(node)
  end function list_get_from_index

  ! Return the number of nodes in the list
  function list_count(this) result(nb)
    type(LINKED_LIST), pointer :: this
    integer :: nb
    integer :: i
    type(LINKED_LIST), pointer :: current    
    current => this
    i = 1
    do while (associated(current%next))
       current => current%next
       i = i+1
    end do
    nb = i
  end function list_count

  ! Find the ind-th node in the list
  ! if ind is greater than total number of nodes in the list, then return the last node in the list
  function list_node(this,ind) result(node)
    type(LINKED_LIST),pointer :: this
    type(LINKED_LIST),pointer :: node
    integer,intent(in)        :: ind
    integer                   :: i
    type(LINKED_LIST),pointer :: current
    current => this
    i = 1
    do while ((associated(current%next)).and.(i.lt.ind))
       current => current%next
       i = i+1
    end do
    node => current 
  end function list_node

  ! Insert a list node after an index containing DATA
  subroutine list_insert_at_index(this,ind,data)
    type(LINKED_LIST),pointer  :: this
    integer, intent(in)        :: ind
    type(LIST_DATA),intent(in) :: data
    type(LINKED_LIST),pointer  :: current     
    if (ind.eq.0) then   ! Insert a node at the head of the list
       allocate(current)
       current%data = DATA
       current%next => this
       nullify(current%prev)
       if (associated(this)) this%prev => current
       this => current
    else     
       current => list_node(this,ind)
       call list_insert_after_node(current,data)
    end if
  end subroutine list_insert_at_index

  ! Pop a node from list, index is passed in, return the data (optional)
  subroutine list_pop(this,ind,data)
    type(LINKED_LIST),pointer            :: this
    integer,intent(in)                   :: ind
    type(LIST_DATA),intent(out),optional :: data
    type(LINKED_LIST),pointer            :: current,next
    if (ind.eq.1) then
       if (present(data)) data = this%data
       call data_free(this%data)
       current => this
       this => this%next
       nullify(this%prev)
       deallocate(current)
    else           
       current => list_node(this,ind-1) ! find the node before the one to pop
       if (present(data)) data = current%next%data
       call data_free(current%next%data)
       next => current%next
       current%next => current%next%next
       current%next%next%prev => current
       deallocate(next)
    end if
  end subroutine list_pop
    
  ! To test if a NODE is in the list THIS
  function list_contain_node(this,node) result(b)
    type(LINKED_LIST),pointer :: this, node 
    type(LINKED_LIST),pointer :: ptr
    logical :: b, tmp
    ptr => this
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

  ! To test if a DATA is in the list THIS
  function list_contain_data(this,data) result(b)
    type(LINKED_LIST),pointer :: this, ptr
    type(LIST_DATA),intent(in) :: data 
    logical :: b, tmp
    ptr => this 
    tmp = .false.
    do while (associated(ptr))             
       if(data_compare(ptr%data,data)) then
          tmp = .true.
          exit
       end if
       ptr => ptr%next
    end do
    b = tmp
  end function list_contain_data
   
