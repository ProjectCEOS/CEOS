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


    ! Enumerators
    !----------------------------------------------------------------------------------------------

    !Problem Type
    type ClassProblemTypes
        integer  :: Mechanical=1 , Thermal=2
    end type
    type (ClassProblemTypes), parameter :: ProblemTypes = ClassProblemTypes()


    !Analysis Type
    type ClassAnalysisTypes
        integer  :: Quasi_Static=1 , Transient=2
    end type
    type (ClassAnalysisTypes), parameter :: AnalysisTypes = ClassAnalysisTypes()


    !Hypothesis of Analysis
    type ClassHypothesisOfAnalysis
        integer :: PlaneStress=1 , PlaneStrain=2 , Axisymmetric=3 , ThreeDimensional=4
    end type
    type (ClassHypothesisOfAnalysis), parameter :: HypothesisOfAnalysis = ClassHypothesisOfAnalysis()


    !Element Technology
    type ClassElementTechnologies
        integer :: Full_Integration=1, Mean_Dilatation=2
    end type
    type (ClassElementTechnologies), parameter :: ElementTechnologies = ClassElementTechnologies()



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

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! ClassAnalysis: Definitions of the analysis type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type ClassAnalysis

		! Class Attributes
		!----------------------------------------------------------------------------------------
        integer ::  ProblemType
        integer ::  AnalysisType
        integer ::  Hypothesis
        integer ::  ElementTech
        logical ::  NLAnalysis
        logical ::  MultiscaleAnalysis

        integer ::  NDOFnode   , AnalysisDimension
        integer ::  BRowSize   , DSize
        integer ::  StressSize , StrainSize
        integer ::  GRowSize   , SSize
        integer ::  MaxCutBack


        contains

            ! Class Methods
            !----------------------------------------------------------------------------------
            procedure :: ClassAnalysisConstructor
            !procedure :: GetTotalNumberOfDOF

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

            this % Hypothesis = FlagAnalysisSettings

            select case (FlagAnalysisSettings)

                case (HypothesisOfAnalysis%PlaneStrain)
                    this % NDOFNode = 2
                    this % AnalysisDimension = 2
                    this % BRowSize = 3
                    this % GRowSize = 4
                    this % SSize = 4
                    this % DSize = 3
                    this % StressSize = 4
                    this % StrainSize = 3

                case (HypothesisOfAnalysis%PlaneStress)
                    this % NDOFNode = 2
                    this % AnalysisDimension = 2
                    this % BRowSize = 3
                    this % GRowSize = 4
                    this % SSize = 4
                    this % DSize = 3
                    this % StressSize = 3
                    this % StrainSize = 4

                case (HypothesisOfAnalysis%Axisymmetric)
                    this % NDOFNode = 2
                    this % AnalysisDimension = 2
                    this % BRowSize = 4
                    this % GRowSize = 5
                    this % SSize = 5
                    this % DSize = 4
                    this % StressSize = 4
                    this % StrainSize = 4

                case (HypothesisOfAnalysis%ThreeDimensional)
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
        ! Method GetTotalNumberOfDOF: Routine that
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
      !  subroutine  GetTotalNumberOfDOF(this, GlobalNodesList, TotalnDOF)
      !
      !
		    !!************************************************************************************
      !      ! DECLARATIONS OF VARIABLES
		    !!************************************************************************************
      !      ! Modules and implicit declarations
      !      ! -----------------------------------------------------------------------------------
      !      use Nodes
      !      implicit none
      !
      !      ! Object
      !      ! -----------------------------------------------------------------------------------
      !      class(ClassAnalysis) :: this
      !
      !      ! Input variables
      !      ! -----------------------------------------------------------------------------------
      !      type (ClassNodes), dimension(:)  :: GlobalNodesList
      !
      !      ! Output variables
      !      ! -----------------------------------------------------------------------------------
      !      integer :: TotalnDOF
		    !!************************************************************************************
      !
 		   ! !************************************************************************************
      !      ! TOTAL NUMBER OF DOF
		    !!************************************************************************************
      !
      !      TotalnDOF = size( GlobalNodesList ) * this%NDOFnode
      !
		    !!************************************************************************************
      !
      !  end subroutine
        !==========================================================================================





end module

