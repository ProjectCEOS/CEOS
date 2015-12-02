module FullLinearSolver
    use LinearSolver
    implicit none

    type,extends(ClassLinearSolver) :: ClassFullLinearSolver

    contains
        procedure :: SolveFull => Solve_ClassFullLinearSolver
        procedure :: ReadSolverParameters => ReadSolverParameters_ClassFullLinearSolver

    end type

contains

    subroutine Solve_ClassFullLinearSolver(this,A,b,x)
        use MathRoutines
        class(ClassFullLinearSolver)::this
        real(8),dimension(:,:):: A
        real(8),dimension(:)::b,x
        call SolveLinearSystemFull(A,b,x)
    end subroutine

    subroutine ReadSolverParameters_ClassFullLinearSolver(this,DataFile)
        use Parser
        class(ClassFullLinearSolver)::this
        class(ClassParser) :: DataFile
    end subroutine


end module
