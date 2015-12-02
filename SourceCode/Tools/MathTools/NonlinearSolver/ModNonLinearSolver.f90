!##################################################################################################
! This module has the attributes and methods for a family of nonlinear solvers.
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
module NonlinearSolver

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! DECLARATIONS OF VARIABLES
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! Modules and implicit declarations
	! --------------------------------------------------------------------------------------------
    use SparseLinearSolverLibrary
    use modNonLinearSystemOfEquations
    use modStatus

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! ClassNonlinearSolver: definitions of the nonlinear solver
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , abstract :: ClassNonlinearSolver

		! Class Attributes
		!-----------------------------------------------------------------------------------------
        !class(ClassSparseLinearSolver) , pointer :: LinearSolver => null()
        class(ClassLinearSolver) , pointer :: LinearSolver => null()
        type(ClassStatus) :: Status

        contains

            ! Class Methods
            !----------------------------------------------------------------------------------
            procedure(NonLinearSolve) , deferred :: Solve
            !procedure , deferred :: Constructor
            procedure (ReadParameters) , deferred :: ReadSolverParameters

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

	abstract interface
        subroutine ReadParameters(this,DataFile)
            import
            use parser
            class(ClassNonLinearSolver) :: this
            type(ClassParser)::DataFile
        end interface
        subroutine NonLinearSolve(this,SOE,Xguess,X)
            class(ClassNonLinearSolver) :: this
            class(ClassNonLinearSystemOfEquations):: SOE
            real(8),dimension(:)          :: Xguess , X
        end subroutine
    end interface


    contains

!        subroutine ReadSolverParametersBase(this,DataFile)
!            use parser
!            class(ClassNonLinearSolver) :: this
!            type(ClassParser)::DataFile
!            stop "Error: Non Linear Solver not defined."
!        END subroutine
!
!        subroutine ConstructorBase (this)
!            class(ClassNonLinearSolver) :: this
!            stop "Error: Non Linear Solver not defined."
!        end subroutine
!
!        subroutine NonLinearSolveBase(this,SOE,Xguess,X)
!            class(ClassNonLinearSolver) :: this
!            class(ClassNonLinearSystemOfEquations):: SOE
!            real(8),dimension(:)          :: Xguess , X
!            stop "Error: Non Linear Solver not defined"
!        end subroutine

! TODO (Thiago#2#11/15/15): Como ficaria o código ao criar um solver não linear para resolver um ponto de gauss usando a classe de solvers não lineares? Criar um tamplate para ver se iria valer a pena. A ideia seria manter todas as rotinas necessárias do modelo material contidas em um único módulo.




end module
