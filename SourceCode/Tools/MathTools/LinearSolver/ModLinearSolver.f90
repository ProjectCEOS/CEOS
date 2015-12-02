module LinearSolver

    use ModStatus

	private :: SolveSparse , SolveFull

    type , abstract :: ClassLinearSolver

        type(ClassStatus) :: Status

        contains
            ! Class Methods
            !----------------------------------------------------------------------------------
            procedure  , private  :: SolveSparse => SolveSparse
            procedure  , private  :: SolveFull => SolveFull
            generic   :: Solve => SolveSparse,SolveFull
            procedure (ReadParameters) , deferred :: ReadSolverParameters

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	abstract interface
        subroutine ReadParameters(this,DataFile)
            use Parser
            import
            class(ClassLinearSolver)::this
            class(ClassParser) :: DataFile
        end subroutine
    end interface

    contains

		!==========================================================================================
        subroutine SolveSparse(this,A,b,x)
            use GlobalSparseMatrix
            class(ClassLinearSolver)::this
            type(ClassGlobalSparseMatrix) :: A
            real(8),dimension(:)::b,x
            stop "Error: Linear Solver not defined."
        end subroutine
        !==========================================================================================
        subroutine SolveFull(this,A,b,x)
            class(ClassLinearSolver)::this
            real(8),dimension(:,:):: A
            real(8),dimension(:)::b,x
            stop "Error: Linear Solver not defined."
        end subroutine
        !==========================================================================================

end module
