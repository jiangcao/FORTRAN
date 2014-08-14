program test_graph
      use mygraph
      use mydata_module

      type(mydata),pointer :: d
      type(graph),pointer :: g
      type(graph_node),pointer :: n1,n2,n
      type(graph_arc),pointer :: a

      d => data_create()
      allocate(d%r(1,1))
      d%r = 1.0d0
        
      g => graph_create()

      n => graph_node_create(d)
      n1 => n

      call graph_add_node(g,n)

      d => graph_node_get_data(n)
      write(*,*) d%r



      d => data_create()
      allocate(d%r(2,2))
      d%r = 2.0d0
    
      n => graph_node_create()
      n2 => n
      call graph_node_put_data(n,d)
      call graph_add_node(g,n)
      d => graph_node_get_data(n)
      write(*,*)
      write(*,*) d%r
    
      d => data_create()
      allocate(d%r(1,1))
      d%r = -1.0d0
       
      a => graph_arc_create(d)
      call graph_connect_nodes(g,a,n1,n2)
      write(*,*)graph_node_count_arcs(n1,'r')
      write(*,*)graph_node_count_arcs(n2,'l')
      a => graph_node_get_arc(n2,'r',2)
      if (associated(a)) then 
          d => graph_arc_get_data(a)
          write(*,*)d%r
      else 
          write(*,*)'null'
      end if







      call graph_free(g)


  end program test_graph

    
