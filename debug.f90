module debug
implicit none

  private

  public :: myStop

contains
  subroutine myStop()
    integer,allocatable :: p(:)
    deallocate(p)
  end subroutine myStop
end module debug
