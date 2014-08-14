program test_list
  use mydata_list, mylist => LINKED_LIST
  use mydata_module

   type(mydata),pointer :: d
   type(mylist),pointer :: l,ptr

   d => data_create(1.0d0)
   l => list_create(d)
   write(*,*) list_find_data(l,d)

   d => data_create(2.0d0)
   call list_insert_after(l,d)
   write(*,*) list_find_data(l,d)

   d => data_create(3.0d0)
   call list_insert_after(l,d)
   write(*,*) list_find_data(l,d)

   ptr => l
   do while (associated(ptr))
      d => list_get_from_node(ptr)
      write(*,*) d%r
      ptr => list_next(ptr)
   end do
   write(*,*)

   call list_pop(l,1)
   ptr => l
   do while (associated(ptr))
      d => list_get_from_node(ptr)
      write(*,*) d%r
      ptr => list_next(ptr)
   end do
   write(*,*)

   d => data_create(4.0d0)
   call list_insert_at_index(l,2,d)
   ptr => l
   do while (associated(ptr))
      d => list_get_from_node(ptr)
      write(*,*) d%r
      ptr => list_next(ptr)
   end do
   write(*,*)

   d => data_create(4.0d0)
   call list_insert_at_index(l,0,d)
   ptr => l
   do while (associated(ptr))
      d => list_get_from_node(ptr)
      write(*,*) d%r
      ptr => list_next(ptr)
   end do
   write(*,*)


   call list_free(l)


end program test_list
