program test_list
  use mydata_list, only : mylist => LINKED_LIST, list_create, list_insert_after_node, list_insert_at_index, list_pop,list_free,list_next,list_get_from_node,list_contain_data
  use mydata_module, only : mydata

   type(mydata) :: d
   type(mylist),pointer :: l,ptr

   d%r = 1
   l => list_create(d)
   write(*,*) list_contain_data(l,d)

   d%r = 2
   call list_insert_after_node(l,d)
   write(*,*) list_contain_data(l,d)

   d%r = 3
   call list_insert_after_node(l,d)
   write(*,*) list_contain_data(l,d)

   ptr => l
   do while (associated(ptr))
      d = list_get_from_node(ptr)
      write(*,*) d%r
      ptr => list_next(ptr)
   end do
   write(*,*)

   call list_pop(l,1)
   ptr => l
   do while (associated(ptr))
      d = list_get_from_node(ptr)
      write(*,*) d%r
      ptr => list_next(ptr)
   end do
   write(*,*)

   d%r=4
   call list_insert_at_index(l,2,d)
   ptr => l
   do while (associated(ptr))
      d = list_get_from_node(ptr)
      write(*,*) d%r
      ptr => list_next(ptr)
   end do
   write(*,*)

   d%r=4
   call list_insert_at_index(l,0,d)
   ptr => l
   do while (associated(ptr))
      d = list_get_from_node(ptr)
      write(*,*) d%r
      ptr => list_next(ptr)
   end do
   write(*,*)


   call list_free(l)


end program test_list
