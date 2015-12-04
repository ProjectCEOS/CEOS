module LinearSolverLibrary

    use LinearSolver
	use PardisoSolver
	use LinearSolverLU

    type ClassLinearSolvers
        integer :: LU = 1
        integer :: Pardiso = 2
    end type

    type(ClassLinearSolvers),parameter :: LinearSolvers = ClassLinearSolvers()


    contains

    subroutine AllocateNewLinearSolver( Solver , SolverID )

			integer , intent(in) :: SolverID

            ! Input/Output variables
            ! -----------------------------------------------------------------------------------
            class(ClassLinearSolver) , pointer , intent(inout) :: Solver

            ! Internal variables
            ! -----------------------------------------------------------------------------------
			type(ClassPardisoSolver)  , pointer :: ParSol => null()
			type(ClassLinearSolverLU) , pointer :: LUSol => null()


            select case (SolverID)

                case (LinearSolvers%LU)

                    allocate(LUSol)
                    Solver=> LUSol

                case (LinearSolvers%Pardiso)

                    allocate(ParSol)
                    Solver => ParSol

            case default
                call Error("AllocateNewLinearSolver :: Linear Solver not identified")

            end select

		    !************************************************************************************

        end subroutine
        !==========================================================================================
        subroutine LinearSolverIdentifier( solver, solverID )

            use Parser
            ! Input variables
            ! -----------------------------------------------------------------------------------
            character(len=*)  :: solver

            ! Output variables
            ! -----------------------------------------------------------------------------------
            integer  :: solverID
            !************************************************************************************

            type(ClassParser) :: Comp

            call Comp%Setup

            if (comp%CompareStrings(solver,"pardiso")) then
                solverID = LinearSolvers%Pardiso
            elseif (comp%CompareStrings(solver,"LU")) then
                solverID = LinearSolvers%LU
            else
                call Error("Error: Linear Solver not identified: "//trim(solver))
            endif

        end subroutine


end module

