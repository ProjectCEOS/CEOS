
!##################################################################################################
! This module has the attributes and methods for a family of linear solvers.
!--------------------------------------------------------------------------------------------------
! Date: 2015/010
!
! Authors:  Jan-Michel Farias
!           Thiago Andre Carniel
!           Paulo Bastos de Castro
!!------------------------------------------------------------------------------------------------
! Modifications:
! Date:         Author:
!##################################################################################################
module LinearSolver

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! ClassSparseLinearSolver: definitions of the sparse linear solver
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

	private :: SolveSparse , SolveFull

    type ClassLinearSolver

        contains
            ! Class Methods
            !----------------------------------------------------------------------------------
            procedure , private :: SolveSparse => SolveSparse
            procedure , private :: SolveFull => SolveFull
            generic   :: Solve => SolveSparse,SolveFull
            procedure :: SolverConstructor

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

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
        subroutine  SolverConstructor(this,SolverParameters)
            class(ClassLinearSolver)::this
            real(8),dimension(:)::SolverParameters
            stop "Error: Linear Solver not defined."
        end subroutine
        !==========================================================================================



end module
