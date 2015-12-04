module LinearSolverLU

    use LinearSolver
    implicit none

    type,extends(ClassLinearSolver) :: ClassLinearSolverLU

    contains
        procedure :: SolveFull => Solve_ClassLinearSolverLU
        procedure :: ReadSolverParameters => ReadSolverParameters_ClassLinearSolverLU

    end type

contains

    subroutine Solve_ClassLinearSolverLU(this,A,b,x)
        use MathRoutines
        class(ClassLinearSolverLU)::this
        real(8),dimension(:,:):: A
        real(8),dimension(:)::b,x
        call SolveLinearSystemLU(A,b,x)
    end subroutine

    subroutine ReadSolverParameters_ClassLinearSolverLU(this,DataFile)
        use Parser
        class(ClassLinearSolverLU)::this
        class(ClassParser) :: DataFile
    end subroutine


end module
