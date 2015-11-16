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
! TODO (Thiago#1#03/03/15): Implementar NR com Line Search!!!
module NonlinearSolverLibrary

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! DECLARATIONS OF VARIABLES
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! Modules and implicit declarations
	! --------------------------------------------------------------------------------------------
    use NonlinearSolver
    use modNewtonRaphsonFull

    ! Nonlinear Solver Enumerator
	! ------------------------------------------------------------------------------------------
    type ClassNonLinearSolvers
        integer :: NewtonRaphsonFull = 1
        integer :: NewtonRaphsonLineSearch = 2
    end type

    type(ClassNonlinearSolvers),parameter::NonLinearSolvers = ClassNonLinearSolvers()

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

    contains

		!==========================================================================================
        ! Routine AllocateNewNonLinearSolver: Routine that allocates a new nonlinear solver.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine AllocateNewNonlinearSolver( Solver , SolverID )

			!************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------

            ! Input variables
            ! -----------------------------------------------------------------------------------
            integer , intent(in) :: SolverID

            ! Input/Output variables
            ! -----------------------------------------------------------------------------------
            class(ClassNonlinearSolver) , pointer , intent(inout) :: Solver

            ! Internal variables
            ! -----------------------------------------------------------------------------------
			type(ClassNewtonRaphsonFull)  , pointer :: NRFull => null()

		    !************************************************************************************

		    !************************************************************************************
            ! SELECTION OF THE NONLINEAR SOLVER
		    !************************************************************************************

            select case (SolverID)

                case (NonLinearSolvers %NewtonRaphsonFull)

                    allocate(NRFull)
                    Solver => NRFull

            case default
                call Error("AllocateNewNonlinearSolver : Nonlinear Solver not identified")

            end select

		    !************************************************************************************

        end subroutine
        !==========================================================================================

		!==========================================================================================
        ! Routine SolverIdentifier: Routine that identifies the nonlinear solver.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine SolverIdentifier( solver, solverID )

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            implicit none

            ! Input variables
            ! -----------------------------------------------------------------------------------
            character(len=*) , intent(in) :: solver

            ! Output variables
            ! -----------------------------------------------------------------------------------
            integer , intent(out) :: solverID

            !************************************************************************************


            !************************************************************************************
            ! DECODE THE STRING SUPPLIED BY GiD
		    !************************************************************************************
            select case (trim(solver))

                case ("newton_raphson_full")

                    solverID = NonLinearSolvers % NewtonRaphsonFull

                case default
                    call Error("Error: Nonlinear Solver not identified")

            end select
		    !************************************************************************************

        end subroutine
        !==========================================================================================


end module
