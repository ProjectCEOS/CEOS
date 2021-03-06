!##################################################################################################
! This module has a FEM Analysis
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
module FEMAnalysis

	! Modules and implicit declarations
	! ---------------------------------------------------------------------------------------------
    use ElementLibrary
    use Nodes
    use Analysis
    use BoundaryConditions
    use GlobalSparseMatrix
    use NonLinearSolver

    implicit none




	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! ClassFEMAnalysis: Definitions of FEM analysis
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type ClassFEMAnalysis

		! Class Attributes
		!----------------------------------------------------------------------------------------
        type  (ClassElementsWrapper)    , pointer     , dimension(:) :: ElementList
        type  (ClassNodes)              , pointer     , dimension(:) :: GlobalNodesList
        type  (ClassAnalysis)           , pointer                    :: AnalysisSettings
        class (ClassBoundaryConditions) , pointer                    :: BC
        type  (ClassGlobalSparseMatrix) , pointer                    :: Kg


        class (ClassNonLinearSolver)    , pointer                    :: NLSolver

        ! Para usar no Probe...
        real(8), pointer, dimension(:) :: U => null()
        real (8) :: Time
        integer :: LoadCase

        contains

            ! Class Methods
            !----------------------------------------------------------------------------------
            procedure :: ReadInputData
            procedure :: AllocateGlobalSparseStiffnessMatrix
            procedure :: Solve => SolveFEMAnalysis
            procedure :: AdditionalMaterialModelRoutine

    end type

    contains


        !==========================================================================================
        ! Method ClassFEMAnalysis:
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine  ReadInputData(this,FileName)


		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use modReadInputFile , only : readinputfile

            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassFEMAnalysis) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            character (len=*) :: FileName
		    !************************************************************************************

 		    !************************************************************************************
            ! SELECT PARAMETERS OF THE analysis type
		    !************************************************************************************

            allocate(this%BC)
            allocate(this%Kg)
            ! Reading the input files
            !************************************************************************************
            call ReadInputFile( FileName, this%AnalysisSettings , this%GlobalNodesList , this%ElementList , &
                                this%BC , this%NLSolver )

		    !************************************************************************************

        end subroutine
        !==========================================================================================


        !##################################################################################################
        ! This routine pre-allocates the size of the global stiffness matrix in the sparse format.
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
        subroutine AllocateGlobalSparseStiffnessMatrix (this)

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use SparseMatrixRoutines
            use Analysis
            use Element
            use GlobalSparseMatrix

            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassFEMAnalysis) :: this

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            type(SparseMatrix) :: KgSparse
            real(8) , pointer , dimension(:,:)  :: Ke
            integer , pointer , dimension(:)    :: GM
            integer ::  e, nDOFel, nDOF

            !************************************************************************************


            !************************************************************************************
            ! PRE-ALLOCATING THE GLOBAL STIFFNESS MATRIX
            !************************************************************************************

            !Allocating memory for the sparse matrix (pre-assembling)
            !************************************************************************************
            call this%AnalysisSettings%GetTotalNumberOfDOF (this%GlobalNodesList, nDOF)

            !Element stiffness matrix used to allocate memory (module Analysis)
            Ke_Memory = 1.0d0

            !Initializing the sparse global stiffness matrix
            call SparseMatrixInit( KgSparse , nDOF )

            !Loop over elements to mapping the local-global positions in the sparse stiffness matrix
            do e=1,size( this%ElementList )

                call this%ElementList(e)% El%GetElementNumberDOF(this%AnalysisSettings , nDOFel)

                Ke => Ke_Memory( 1:nDOFel , 1:nDOFel )
                GM => GM_Memory( 1:nDOFel )

                call this%ElementList(e)%El%GetGlobalMapping( this%AnalysisSettings , GM )

                call SparseMatrixSetArray( GM, GM, Ke, KgSparse, OPT_SET )

            enddo

            !Converting the sparse matrix to coordinate format (used by Pardiso Sparse Solver)
            call ConvertToCoordinateFormat( KgSparse , this%Kg%Row , this%Kg%Col , this%Kg%Val , this%Kg%RowMap)

            !Releasing memory
            call SparseMatrixKill(KgSparse)

            !************************************************************************************

        end subroutine




        !==========================================================================================
        ! Method ClassFEMAnalysis:
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine  SolveFEMAnalysis( this )

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassFEMAnalysis) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            integer :: nDOF

 		    !************************************************************************************
            ! SELECT PARAMETERS OF THE analysis type
		    !************************************************************************************


            ! Calling the quasi-static analysis routine
            !************************************************************************************
            select case ( this%AnalysisSettings%AnalysisType )

                case ( AnalysisTypes%Quasi_Static )

                    call this%AdditionalMaterialModelRoutine()

                    call QuasiStaticAnalysisFEM( this%ElementList, this%AnalysisSettings, this%GlobalNodesList , &
                                                 this%BC, this%Kg, this%NLSolver )

                case default
                    stop "Error in AnalysisType - ModFEMAnalysis"
            end select



		    !************************************************************************************

        end subroutine
        !==========================================================================================



        !==========================================================================================
        ! Method ClassFEMAnalysis:
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine  WriteFEMResults( U, Time, LC, ST, CutBack, SubStep, Flag_EndStep, FileID, NumberOfIterations )

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            !class(ClassFEMAnalysis) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            real(8) :: Time
            real(8), dimension(:) :: U
            integer :: i, FileID, LC, ST, CutBack, SubStep, Flag_EndStep, NumberOfIterations

 		    !************************************************************************************
            ! WRITING RESULTS
		    !************************************************************************************
            write(FileID,*) 'TIME =', Time
            write(FileID,*) 'LOAD CASE =', LC
            write(FileID,*) 'STEP =', ST
            write(FileID,*) 'CUT BACK =', CutBack
            write(FileID,*) 'SUBSTEP =', SubStep
            write(FileID,*) 'FLAG END STEP =', Flag_EndStep
            write(FileID,*) 'NUMBER OF ITERATIONS TO CONVERGE =', NumberOfIterations
            do i = 1,size(U)
                write(FileID,*) U(i)
            enddo

		    !************************************************************************************

        end subroutine
        !==========================================================================================

        !##################################################################################################
        ! This routine contains the procedures to solve a quasi-static analysis based in a incremental-
        ! iterative approach.
        !##################################################################################################
        subroutine QuasiStaticAnalysisFEM( ElementList , AnalysisSettings , GlobalNodesList , BC  , &
                                           Kg , NLSolver )

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use ElementLibrary
            use Analysis
            use Nodes
            use BoundaryConditions
            use GlobalSparseMatrix
            use NonLinearSolver
            use Interfaces
            use MathRoutines
            use LoadHistoryData
            use modFEMSystemOfEquations

            implicit none

            ! Input variables
            ! -----------------------------------------------------------------------------------
            type (ClassAnalysis)                                    :: AnalysisSettings
            type (ClassElementsWrapper),     pointer, dimension(:)  :: ElementList
            type (ClassNodes),               pointer, dimension(:)  :: GlobalNodesList
            class (ClassBoundaryConditions),  pointer               :: BC
            type (ClassGlobalSparseMatrix),  pointer                :: Kg
            class(ClassNonLinearSolver),     pointer                :: NLSolver

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8), allocatable, dimension(:) :: U , R , DeltaFext, DeltaUPresc, Fext_alpha0, Ubar_alpha0, Uconverged
            real(8) :: DeltaTime , Time_alpha0
            real(8) :: alpha, alpha_max, alpha_min, alpha_aux
            integer :: LC , ST , nSteps, nLoadCases ,  CutBack, SubStep, e,gp, nDOF, FileID_FEMAnalysisResults, Flag_EndStep
            real(8), parameter :: GR= (1.0d0 + dsqrt(5.0d0))/2.0d0

            type(ClassFEMSystemOfEquations) :: FEMSoE

            FileID_FEMAnalysisResults = 42
            open (FileID_FEMAnalysisResults,file='FEMAnalysis.result',status='unknown')

            !************************************************************************************

            !************************************************************************************
            ! QUASI-STATIC ANALYSIS
            !***********************************************************************************
            call AnalysisSettings%GetTotalNumberOfDOF (GlobalNodesList, nDOF)

            write(FileID_FEMAnalysisResults,*) 'Total Number of DOF = ', nDOF

            FEMSoE % ElementList => ElementList
            FEMSoE % AnalysisSettings = AnalysisSettings
            FEMSoE % GlobalNodesList => GlobalNodesList
            FEMSoE % BC => BC
            femsoe % Kg => Kg
            allocate( FEMSoE % Fint(nDOF) , FEMSoE % Fext(nDOF) , FEMSoE % Ubar(nDOF) )


            ! Allocating arrays
            allocate(R(nDOF) , DeltaFext(nDOF), Fext_alpha0(nDOF))
            allocate( U(nDOF)  , DeltaUPresc(nDOF), Ubar_alpha0(nDOF), Uconverged(nDOF)  )


            U = 0.0d0
            Ubar_alpha0 = 0.0d0

            nLoadCases = BC%GetNumberOfLoadCases()

            ! Escrevendo os resultados para o tempo zero
            ! NOTE (Thiago#1#11/19/15): OBS.: As condi��es de contorno iniciais devem sair do tempo zero.
            Flag_EndStep = 1
            call WriteFEMResults( U, 0.0d0, 1, 1, 0, 0, Flag_EndStep, FileID_FEMAnalysisResults, NumberOfIterations=0  )

            !LOOP - LOAD CASES
            LOAD_CASE:  do LC = 1 , nLoadCases

                write(*,'(a,i3)')'Load Case: ',LC
                write(*,*)''

                nSteps = BC%GetNumberOfSteps(LC)

               ! LOOP - STEPS
                STEPS:  do ST = 1 , nSteps

                    write(*,'(4x,a,i3,a,i3,a)')'Step: ',ST,' (LC: ',LC,')'
                    write(*,*)''

                    call BC%GetBoundaryConditions(AnalysisSettings, LC, ST, Fext_alpha0, DeltaFext,FEMSoE%DispDOF, U, DeltaUPresc)

                    call BC%GetTimeInformation(LC,ST,Time_alpha0,DeltaTime)

                    ! Prescribed Incremental Displacement
                    Ubar_alpha0 = U
                    Uconverged = U

                    alpha_max = 1.0d0 ; alpha_min = 0.0d0
                    alpha = alpha_max

                    CutBack = 0 ; SubStep = 0

                    SUBSTEPS: do while(.true.)


                        write(*,'(8x,a,i3)') 'Cut Back: ',CutBack
                        write(*,'(12x,a,i3,a,f7.4,a)') 'SubStep: ',SubStep,' (Alpha: ',alpha,')'


                        FEMSoE % Time = Time_alpha0 + alpha*DeltaTime
                        FEMSoE % Fext = Fext_alpha0 + alpha*DeltaFext
                        FEMSoE % Ubar = Ubar_alpha0 + alpha*DeltaUPresc


                        call NLSolver%Solve( FEMSoE , XGuess = Uconverged , X = U )

                        IF (NLSolver%Status%Error) then

                            write(*,'(12x,a)') 'Not Converged - '//Trim(NLSolver%Status%ErrorDescription)
                            write(*,'(12x,a)') Trim(FEMSoE%Status%ErrorDescription)
                            write(*,*)''

                            alpha = alpha_min + (1.0d0-1.0d0/GR)*( alpha - alpha_min )

                            U = Uconverged

                            ! Update Mesh Coordinates
                            if (AnalysisSettings%NLAnalysis == .true.) then
                                call UpdateMeshCoordinates(GlobalNodesList,AnalysisSettings,U)
                            endif

                            CutBack = CutBack + 1
                            SubStep = 1
                            if ( CutBack .gt. AnalysisSettings%MaxCutBack ) then
                                write(*,'(a,i3,a,i3,a,i3,a)') 'Load Case: ',LC,' Step: ', ST , ' did not converge with ', AnalysisSettings%MaxCutBack, ' cut backs.'
                                stop
                            endif

                            write(*,'(8x,a,i3)') 'Cut Back: ',CutBack
                            write(*,'(12x,a,i3,a,f7.4,a)') 'SubStep: ',SubStep,' (Alpha: ',alpha,')'

                            !---------------------------------------------------------------------------
                        ELSEIF (alpha==1.0d0) then

                            SubStep = SubStep + 1

                            Flag_EndStep = 1
                            call WriteFEMResults( U, FEMSoE%Time, LC, ST, CutBack, SubStep, Flag_EndStep, &
                                                  FileID_FEMAnalysisResults, NLSolver%NumberOfIterations )

                            exit SUBSTEPS

                            !---------------------------------------------------------------------------
                        ELSE

                            SubStep = SubStep + 1

                            alpha_aux = alpha_min

                            alpha_min = alpha

                            alpha = min(alpha + GR*(alpha - alpha_aux),1.0d0)

                            Uconverged = U

                            write(*,'(12x,a,i3,a,f7.4,a)') 'SubStep: ',SubStep,' (Alpha: ',alpha,')'

                            Flag_EndStep = 0
                            call WriteFEMResults( U, FEMSoE % Time, LC, ST, CutBack, SubStep, Flag_EndStep, &
                                                  FileID_FEMAnalysisResults,  NLSolver%NumberOfIterations  )

                        ENDIF


                    enddo SUBSTEPS



                    ! -----------------------------------------------------------------------------------
                    ! SWITCH THE CONVERGED STATE: StateVariable_n := StateVariable_n+1
                    ! -----------------------------------------------------------------------------------
                    do e=1,size(elementlist)
                        do gp=1,size(elementlist(e)%el%GaussPoints)
                            call ElementList(e)%el%GaussPoints(gp)%SwitchConvergedState()
                        enddo
                    enddo
                    ! -----------------------------------------------------------------------------------




                    write(*,'(4x,a,i3)')'End Step: ',ST
                    write(*,*)''

                enddo STEPS

                write(*,'(a,i3)')'End Load Case: ',LC
                write(*,*)''
                write(*,*)''

            enddo LOAD_CASE

            close (FileID_FEMAnalysisResults)
            !************************************************************************************


        end subroutine



        !==========================================================================================
        ! Method ClassFEMAnalysis:
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine  AdditionalMaterialModelRoutine( this )

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassFEMAnalysis) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------

 		    !************************************************************************************
            ! SELECT PARAMETERS OF THE analysis type
		    !************************************************************************************


            !
            !***********************************************************************************


		    !************************************************************************************

        end subroutine
        !==========================================================================================


end module


































