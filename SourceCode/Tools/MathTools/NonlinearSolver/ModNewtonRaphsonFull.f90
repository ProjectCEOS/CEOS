module modNewtonRaphsonFull
    use modNonLinearSystemOfEquations
    use NonlinearSolver
    implicit none

    type, extends(ClassNonlinearSolver) :: ClassNewtonRaphsonFull
        real(8) :: tol
        integer :: itmax
        integer :: NormType = 2 , MatrixType
        logical :: ShowInfo = .true.

    contains
        procedure :: Solve => NewtonRaphsonFull_Solve
        procedure :: Constructor => NewtonRaphsonFull_Constructor
        procedure :: ReadSolverParameters => NewtonRaphsonFull_ReadSolverParameters
    end type

    type ClassNewtonRaphsonFullErrors
        integer :: MaxNumberOfIteration = 1
        integer :: UserEvaluateSystemReportedError = 2
        integer :: UserEvaluateGradientReportedError = 3
        integer :: LinearSystemError = 4
    end type

    type ClassNewtonRaphsonFullNormTypes
        integer :: L2Norm = 1
        integer :: MaximumAbsoluteValue=2
    end type

    type ClassNewtonRaphsonFullMatrixTypes
        integer :: Full = 1
        integer :: Sparse = 2
    end type


    type (ClassNewtonRaphsonFullNormTypes)   , parameter :: NewtonRaphsonFull_NormTypes = ClassNewtonRaphsonFullNormTypes()
    type (ClassNewtonRaphsonFullErrors)      , parameter :: NewtonRaphsonFull_Errors = ClassNewtonRaphsonFullErrors()
    type (ClassNewtonRaphsonFullMatrixTypes) , parameter :: NewtonRaphsonFull_MatrixTypes = ClassNewtonRaphsonFullMatrixTypes()


contains
!-----------------------------------------------------------------

    subroutine NewtonRaphsonFull_Solve(this,SOE,Xguess,X)

        use GlobalSparseMatrix
        use MathRoutines
        class(ClassNewtonRaphsonFull) :: this
        class(ClassNonLinearSystemOfEquations)      :: SOE
        real(8),dimension(:)          :: Xguess , X

        integer :: it
        real(8) :: normR , R(size(X)) , DX(size(X))
        real(8),dimension(:,:),pointer :: GFull
        class(ClassGlobalSparseMatrix),pointer :: GSparse


        call SOE%Status%SetSuccess
        call this%Status%SetSuccess

        it = 0
        X=Xguess

        LOOP: do while (.true.)

            call SOE%EvaluateSystem(X,R)

            if (SOE%Status%Error) then
                call this%Status%SetError(NewtonRaphsonFull_Errors%UserEvaluateSystemReportedError,'Error Evaluating system')
                return
            endif



            call SOE%EvaluateGradient(X,R,GSparse)

            if (SOE%Status%error) then
                call this%Status%SetError(NewtonRaphsonFull_Errors%UserEvaluateGradientReportedError,'Error Evaluating Gradient')
                return
            endif


           select case (this%normtype)
                case (NewtonRaphsonFull_NormTypes%L2Norm)
                    normR = norm(R)
                case (NewtonRaphsonFull_NormTypes%MaximumAbsoluteValue)
                    normR = maxval( dabs(R))
                case default
                    stop "NewtonRaphsonFull_Solve :: NormType not set"
            end select


            if (this%ShowInfo) write(*,'(12x,a,i3,a,e16.9)') 'IT: ',IT ,'  NORM: ',normR

            if (normR<this%tol) then
                call this%Status%SetSuccess()
                if (this%ShowInfo) write(*,'(12x,a,i3,a)')'Converged in ',IT,' iterations'
                return
            elseif (it>= this%itmax) then
                call this%Status%SetError(NewtonRaphsonFull_Errors%MaxNumberOfIteration,'Maximum Number of Iterations reached!')
                return
            endif

            it=it+1

            call this%LinearSolver%Solve(GSparse, -R, DX)

           ! if erro sistema linear
                !call this%Status%SetError(NewtonRaphsonFull_Errors%LinearSystemError,'Error Solving Linear System')
            !    return
            !endif

            X = X + DX

            call SOE%RotinaExtra(X)

        end do LOOP

    end subroutine

    subroutine NewtonRaphsonFull_ReadSolverParameters(this,DataFile)
            use Parser
		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! ---------------------------------------------------------------------------------
            class(ClassNewtonRaphsonFull) :: this


            ! Input variables
            ! ---------------------------------------------------------------------------------
            type(ClassParser)::DataFile

		    !************************************************************************************

		    character(len=100),dimension(2)::ListOfOptions,ListOfValues

            !************************************************************************************
            ! READ THE MATERIAL PARAMETERS
		    !************************************************************************************
		    ListOfOptions=["tol","maxiter"]

		    call DataFile%FillListOfOptions(ListOfOptions,ListOfValues)

            call DataFile%ConvertToDouble(ListOfValues(1),this%tol)
            call DataFile%ConvertToInteger(ListOfValues(2),this%itmax)

        end subroutine
        !==========================================================================================
        subroutine NewtonRaphsonFull_Constructor (this)
		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! ---------------------------------------------------------------------------------
            class(ClassNewtonRaphsonFull) :: this
		    !************************************************************************************

        endsubroutine

    end module

