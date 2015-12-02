module LinearSolver

    use ModStatus


	private :: SolveSparse , SolveFull

    type ClassLinearSolver

        type(ClassStatus) :: Status

        contains
            ! Class Methods
            !----------------------------------------------------------------------------------
            procedure (SolveSparse) , private , deferred :: SolveSparse => SolveSparse
            procedure (SolveFull) , private , deferred :: SolveFull => SolveFull
            generic   :: Solve => SolveSparse,SolveFull
            !procedure , deferred :: SolverConstructor
            procedure (ReadParameters) , deferred :: ReadSolverParameters

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	abstract interface
        subroutine SolveSparse(this,A,b,x)
            import
            use GlobalSparseMatrix
            class(ClassLinearSolver)::this
            type(ClassGlobalSparseMatrix) :: A
            real(8),dimension(:)::b,x
        end subroutine

        subroutine SolveFull(this,A,b,x)
            import
            class(ClassLinearSolver)::this
            real(8),dimension(:,:):: A
            real(8),dimension(:)::b,x
        end subroutine

        subroutine ReadParameters(this,DataFile)
            import
            use Parser
            class(ClassLinearSolver)::this
            class(ClassParser) :: DataFile
        end subroutine
    end interface

    contains

		!==========================================================================================
!        subroutine SolveSparse(this,A,b,x)
!            use GlobalSparseMatrix
!            class(ClassLinearSolver)::this
!            type(ClassGlobalSparseMatrix) :: A
!            real(8),dimension(:)::b,x
!            stop "Error: Linear Solver not defined."
!        end subroutine
        !==========================================================================================
!        subroutine SolveFull(this,A,b,x)
!            class(ClassLinearSolver)::this
!            real(8),dimension(:,:):: A
!            real(8),dimension(:)::b,x
!            stop "Error: Linear Solver not defined."
!        end subroutine
        !==========================================================================================
!        subroutine  SolverConstructor(this,SolverParameters)
!            class(ClassLinearSolver)::this
!            real(8),dimension(:)::SolverParameters
!            stop "Error: Linear Solver not defined."
!        end subroutine
        !==========================================================================================



end module
