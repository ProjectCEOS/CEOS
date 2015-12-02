module LinearSolverLibrary

    use LinearSolver
	use PardisoSolver
	use FullLinearSolver

    ! TODO (Jan#1#12/01/15): Verificar com o Thiago se ao invés de chamar FULL não deviamos chamar de LU ou pelo menos LU_full
    type ClassLinearSolvers
        integer :: FULL = 1
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
			type(ClassFullLinearSolver) , pointer :: FullSol => null()


            select case (SolverID)

                case (LinearSolvers%FULL)

                    allocate(FullSol)
                    Solver=> FullSol

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
            elseif (comp%CompareStrings(solver,"full")) then
                solverID = LinearSolvers%FULL
            else
                call Error("Error: Linear Solver not identified: "//trim(solver))
            endif

        end subroutine


end module

