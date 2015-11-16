!##################################################################################################
! This module has the attributes and methods to select the parameters of the analysis type choosen.
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
module Analysis

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! DECLARATIONS OF VARIABLES
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! IDs of the analysis settings.
    !----------------------------------------------------------------------------------------------
    !Problem Type
    integer , parameter :: Mechanical=1 , Thermal=2

    !Analysis Type
    integer , parameter :: PlaneStress=1 , PlaneStrain=2 , Axisymmetric=3 , ThreeDimensional=4

    !Element Technology
    integer , parameter :: Full_Integration=1, Mean_Dilatation=2



    ! Parameters of the analysis type.
    !----------------------------------------------------------------------------------------------
    integer , parameter :: MaxElementNumberDOF=200 , MaxTensorComponents=6, MaxElementNodes=100

    ! Arrays used to allocate memory
    !----------------------------------------------------------------------------------------------
    real(8) , target , dimension( MaxElementNumberDOF , MaxElementNumberDOF)    :: Ke_Memory
    real(8) , target , dimension( MaxTensorComponents , MaxElementNumberDOF)    :: B_Memory
    real(8) , target , dimension( 9 , MaxElementNumberDOF)                      :: G_Memory
    real(8) , target , dimension( 9 , 9)                                        :: S_Memory
    real(8) , target , dimension( MaxTensorComponents , MaxTensorComponents)    :: D_Memory
    real(8) , target , dimension( MaxElementNumberDOF )                         :: SF_Memory
    real(8) , target , dimension( MaxElementNumberDOF )                         :: Fe_Memory
    real(8) , target , dimension( MaxTensorComponents )                         :: Stress_Memory
    real(8) , target , dimension( MaxElementNumberDOF , MaxElementNumberDOF)    :: DifSF_Memory
    integer , target , dimension( MaxElementNumberDOF )                         :: GM_Memory

    real(8) , target , dimension( MaxTensorComponents , MaxElementNumberDOF)    :: DB_Memory
    real(8) , target , dimension( 9 , MaxElementNumberDOF)                      :: SG_Memory
    real(8) , target , dimension( 1, MaxElementNumberDOF )                      :: Bdiv_Memory


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

! TODO (Thiago#1#03/27/15): Calcular o nDOF dentro da classe Analysis
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! ClassAnalysis: Definitions of the analysis type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type ClassAnalysis

		! Class Attributes
		!----------------------------------------------------------------------------------------
        integer ::  ProblemType
        integer ::  AnalysisType
        integer ::  ElementTech
        logical ::  NLAnalysis

        integer ::  NDOFnode   , AnalysisDimension
        integer ::  BRowSize   , DSize
        integer ::  StressSize , StrainSize
        integer ::  GRowSize   , SSize
        integer ::  MaxCutBack
        integer ::  ErrorNumber = 0
        character (len=255) :: ErrorDescription =''

        !Multiscale Settings
        logical :: IsAMicrostructure=.false., IsTheInitialScale=.false.
        real(8) :: Fmacro(3,3)=0.0d0, Umacro(3)=0.0d0


        contains

            ! Class Methods
            !----------------------------------------------------------------------------------
            procedure :: ClassAnalysisConstructor
            procedure :: ResetError

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


    contains

        !==========================================================================================
        ! Method ClassAnalysisConstructor: Routine that constructs the analysis type
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine  ClassAnalysisConstructor(this,FlagAnalysisSettings)


		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassAnalysis) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            integer , intent(in) :: FlagAnalysisSettings
		    !************************************************************************************

 		    !************************************************************************************
            ! SELECT PARAMETERS OF THE analysis type
		    !************************************************************************************

            this % AnalysisType = FlagAnalysisSettings

            select case (FlagAnalysisSettings)

                case (PlaneStrain)
                    this % NDOFNode = 2
                    this % AnalysisDimension = 2
                    this % BRowSize = 3
                    this % GRowSize = 4
                    this % SSize = 4
                    this % DSize = 3
                    this % StressSize = 4
                    this % StrainSize = 3

                case (PlaneStress)
                    this % NDOFNode = 2
                    this % AnalysisDimension = 2
                    this % BRowSize = 3
                    this % GRowSize = 4
                    this % SSize = 4
                    this % DSize = 3
                    this % StressSize = 3
                    this % StrainSize = 4

                case (Axisymmetric)
                    this % NDOFNode = 2
                    this % AnalysisDimension = 2
                    this % BRowSize = 4
                    this % GRowSize = 5
                    this % SSize = 5
                    this % DSize = 4
                    this % StressSize = 4
                    this % StrainSize = 4

                case (ThreeDimensional)
                    this % NDOFNode = 3
                    this % AnalysisDimension = 3
                    this % BRowSize = 6
                    this % GRowSize = 9
                    this % SSize = 9
                    this % DSize = 6
                    this % StressSize = 6
                    this % StrainSize = 6

                case default
                    stop "Error: analysis type not identified."

            end select

		    !************************************************************************************

        end subroutine
        !==========================================================================================

       !==========================================================================================
        ! Method ResetError: Routine that constructs the analysis type
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine  ResetError(this)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassAnalysis) :: this

 		    !************************************************************************************
            ! SELECT PARAMETERS OF THE analysis type
		    !************************************************************************************

            this%ErrorNumber = 0
            this%ErrorDescription = ''

		    !************************************************************************************

        end subroutine
        !==========================================================================================

end module

