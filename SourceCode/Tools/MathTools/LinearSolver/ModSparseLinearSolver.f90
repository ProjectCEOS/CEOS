!##################################################################################################
! This module has the attributes and methods for a family of sparse linear solvers.
!--------------------------------------------------------------------------------------------------
! Date: 2014/02
!
! Authors:  Jan-Michel Farias
!           Thiago Andre Carniel
!           Paulo Bastos de Castro
!!------------------------------------------------------------------------------------------------
! Modifications:
! Date:         Author:
!##################################################################################################
module SparseLinearSolver

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! ClassSparseLinearSolver: definitions of the sparse linear solver
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type ClassSparseLinearSolver
    
        contains 
            ! Class Methods
            !----------------------------------------------------------------------------------
            procedure :: Solve 
            procedure :: SolverConstructor
    
    end type 
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

    contains 

		!==========================================================================================
        ! Dummy Procedures: To be used by the superclasses
        !==========================================================================================
        !==========================================================================================    
        subroutine Solve(this,A,b,x)
            use GlobalSparseMatrix
            class(ClassSparseLinearSolver)::this
            type(ClassGlobalSparseMatrix) :: A
            real(8),dimension(:)::b,x
            stop "Error: Sparse Solver not defined."
        end subroutine 
        !==========================================================================================
        subroutine  SolverConstructor(this,SolverParameters)
            class(ClassSparseLinearSolver)::this    
            real(8),dimension(:)::SolverParameters
            stop "Error: Sparse Solver not defined."
        end subroutine 
        !==========================================================================================
        
        !==========================================================================================

    
end module 