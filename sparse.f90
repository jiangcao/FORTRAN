! A sparse matrix object
module sparse

  implicit none
  private
  
  ! Sparse matrix data type
  type :: sparse_t
     
  end type sparse_t

  ! Sparse matrix cell data type
  type :: sparse_cell_t
     private 
     integer, dimension(:), pointer :: data => null()

  end type sparse_cell_t

  ! Sparse matrix cell pointer type
  type :: sparse_cell_ptr
     type(sparse_cell_t),pointer :: p
  end type sparse_cell_ptr

contains




end module sparse
