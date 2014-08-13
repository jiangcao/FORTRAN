module debug
    implicit none

    private

    public :: myStop,isDebug

contains
    logical Function isDebug()
        character(len=1) :: arg
        isDebug=.false.
        if (command_argument_count().gt.0)then
            call get_command_argument(1,arg)
            if (arg.eq.'d')then
                isDebug=.true.
            endif
        endif
    end Function isDebug



    subroutine myStop()
        integer,allocatable :: p(:)
        deallocate(p)
    end subroutine myStop
end module debug
