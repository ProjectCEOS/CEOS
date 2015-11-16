module modTools

    contains

    subroutine GetArgs(Args,Show)

        use dflib
        implicit none
        character(len=*),dimension(:), allocatable :: Args
        logical,optional::Show

        integer::NARG
        integer(2)::i,status
        logical::ShowArg

        if (present(show)) then
            ShowArg=Show
        else
            ShowArg=.false.
        endif

        if (allocated(Args)) deallocate(Args)

        NARG = NARGS()
        if (NARG>1) then
            allocate(Args(NARG-1))
            do i=1,NARG-1
                CALL GETARG(i, args(i), status)
                IF (ShowArg) write(*,*) '['//args(i)//'] STATUS=',status
            enddo
        end if

    end subroutine


end module
