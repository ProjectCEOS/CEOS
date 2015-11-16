module modStatus

    type ClassStatus
        logical :: Error=.false.
        integer :: ErrorID = 0
        character(len=255)::ErrorDescription=''
    contains
        procedure :: SetError
        procedure :: SetSuccess
    end type
contains 
    subroutine SetError(this,ErrorID,ErrorDescription)
        class(ClassStatus)::this
        integer::ErrorID
        character(len=*)::ErrorDescription
        this%Error=.true.
        this%ErrorDescription = ErrorDescription
        this%ErrorID = ErrorID
    end subroutine

    subroutine SetSuccess(this)
        class(ClassStatus)::this
        this%Error=.false.
        this%ErrorDescription = ''
        this%ErrorID = 0
    end subroutine

end module
