module modNonLinearSystemOfEquations
    use modStatus
    use GlobalSparseMatrix

    type ClassNonLinearSystemOfEquations
        type(ClassStatus) :: Status
    contains
        procedure :: EvaluateSystem => EvaluateSystemBase
        procedure :: EvaluateGradientFull => EvaluateGradientBase
        procedure :: EvaluateGradientSparse => EvaluateGradientSparseBase
        generic   :: EvaluateGradient => EvaluateGradientFull , EvaluateGradientSparse
        procedure :: RotinaExtra => RotinaExtraBase
    end type

    contains

!__________________________________________________________________________________________________
    subroutine EvaluateSystemBase(this,x,R)
        class(ClassNonLinearSystemOfEquations)::this
        real(8),dimension(:)::x,R
        stop "EvaluateSystem Not Implemented"
    end subroutine
!__________________________________________________________________________________________________
    subroutine EvaluateGradientBase(this,x,R,G)
        class(ClassNonLinearSystemOfEquations)::this
        real(8),dimension(:)::x,R
        real(8),dimension(:,:),pointer::G
        stop "EvaluateGradient Not Implemented"
    end subroutine
!__________________________________________________________________________________________________
    subroutine EvaluateGradientSparseBase(this,x,R,G)
        class(ClassNonLinearSystemOfEquations)::this
        class(ClassGlobalSparseMatrix) , pointer :: G
        real(8),dimension(:)::x,R
        stop "EvaluateGradient Not Implemented"
    end subroutine
!__________________________________________________________________________________________________
! TODO (Thiago#1#10/19/15): Mudar o Nome da Rotina
    subroutine RotinaExtraBase(this,X)
        class(ClassNonLinearSystemOfEquations)::this
        real(8),dimension(:)::X
    end subroutine

end module
