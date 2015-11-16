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
module SparseLinearSolverLibrary


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! DECLARATIONS OF VARIABLES
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! Modules and implicit declarations
	! --------------------------------------------------------------------------------------------
    use PardisoSolver

    ! Sparse Linear Solver ID used in the code:
	! ------------------------------------------------------------------------------------------
    integer , parameter :: PardisoLinearSolver = 1

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

    contains

		!==========================================================================================
        ! Routine AllocateNewSparseLinearSolver: Routine that allocates the element type.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine AllocateNewSparseLinearSolver( Solver , SolverID )

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
            class(ClassSparseLinearSolver) , pointer , intent(inout) :: Solver

            ! Internal variables
            ! -----------------------------------------------------------------------------------
			type(ClassPardisoSolver)  , pointer :: ParSol => null()

		    !************************************************************************************

		    !************************************************************************************
            ! SELECTION AND ALLOCATION OF THE ELEMENT TYPE
		    !************************************************************************************

            select case (SolverID)

                case (PardisoLinearSolver)

                    allocate(ParSol)
                    Solver => ParSol

            case default
                call Error("AllocateNewSparseLinearSolver :: Linear Solver not identified")

            end select

		    !************************************************************************************

        end subroutine
        !==========================================================================================
        subroutine LinearSolverIdentifier( solver, solverID )

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            implicit none

            ! Input variables
            ! -----------------------------------------------------------------------------------
            character(len=*)  :: solver

            ! Output variables
            ! -----------------------------------------------------------------------------------
            integer  :: solverID

            !************************************************************************************


            !************************************************************************************
            ! DECODE THE STRING SUPPLIED BY GiD
		    !************************************************************************************
            select case (trim(solver))

                case ("pardiso")

                    solverID = PardisoLinearSolver

                case default
                    call Error("Error: Linear Solver not identified: "//trim(solver))

            end select
		    !************************************************************************************

        end subroutine


end module
