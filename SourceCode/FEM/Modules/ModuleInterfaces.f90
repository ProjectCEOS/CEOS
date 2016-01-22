!##################################################################################################
! This module contains the explicit interfaces used in the code.
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
module Interfaces

    !==============================================================================================
    interface

        !==========================================================================================
        subroutine ElementConstructor( this, ElementNodes, ElementType, GlobalNodesList )

	        !************************************************************************************
	        ! DECLARATIONS OF VARIABLES
	        !************************************************************************************
	        ! Modules and implicit declarations
	        ! -----------------------------------------------------------------------------------
	        use ElementLibrary
	        use Nodes

	        implicit none

	        ! Object
	        ! -----------------------------------------------------------------------------------
	        class(ClassElement) , pointer :: this

	        ! Input variables
	        ! -----------------------------------------------------------------------------------
	        type(ClassNodes) , dimension(:) , pointer , intent(in) :: GlobalNodesList
	        integer				 , intent(in) :: ElementType
	        integer,dimension(:) , intent(in) :: ElementNodes

        end subroutine
        !==========================================================================================


        !==========================================================================================
        subroutine AssembleGlobalMatrix( GM , Ke , Kg )

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use GlobalSparseMatrix
            implicit none

            ! Input variables
            ! -----------------------------------------------------------------------------------
            integer , dimension(:) , intent(in)   :: GM
            real(8) , dimension(:,:) , intent(in) :: Ke

            ! Input/Output variables
            ! -----------------------------------------------------------------------------------
            type(ClassGlobalSparseMatrix) :: Kg

        end subroutine
        !==========================================================================================

        !==========================================================================================
        !subroutine ApplyBoundaryConditions( Kg , R , Presc_Disp_DOF , Fixed_Disp_DOF , Ubar , U )
        !
        !    !************************************************************************************
        !    ! DECLARATIONS OF VARIABLES
        !    !************************************************************************************
        !    ! Modules and implicit declarations
        !    ! -----------------------------------------------------------------------------------
        !    use GlobalSparseMatrix
        !    implicit none
        !
        !    ! Input variables
        !    ! -----------------------------------------------------------------------------------
        !    integer , dimension(:) , intent(in) ::  Presc_Disp_DOF , Fixed_Disp_DOF
        !
        !    ! Input/Output variables
        !    ! -----------------------------------------------------------------------------------
        !    real(8) , dimension(:) , intent(inout) ::  R  , Ubar , U
        !    type(ClassGlobalSparseMatrix) :: Kg
        !
        !end subroutine
        !==========================================================================================

        !==========================================================================================
        !subroutine ExportResultFile( Time , u, nDOFnode, TotalnNodes , ElementList , GiDFile)
        !    use Element
        !    use GiDResultFile
        !    implicit none
        !    type (ClassElementsWrapper) , dimension(:) :: ElementList
        !    type(ClassGiDResultFile)                                       :: GidFile
        !    integer :: nDOFnode, TotalnNodes
        !    real(8) , dimension(:) :: u
        !    Real(8)::Time
        !end subroutine
        !==========================================================================================

        !==========================================================================================
        subroutine SolveConstitutiveModel( ElementList , AnalysisSettings , Time, U, Status)

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use ElementLibrary
            use Analysis
            use ModStatus

            implicit none

            ! Input variables
            ! -----------------------------------------------------------------------------------
            type(ClassElementsWrapper) , dimension(:) :: ElementList
            type(ClassAnalysis)                       :: AnalysisSettings
            type(ClassStatus)                         :: Status
            real(8)                    , dimension(:) :: U

            real(8)                                   :: Time
            !************************************************************************************

        end subroutine
        !==========================================================================================

        !==========================================================================================
        subroutine InternalForce( ElementList, AnalysisSettings, Fint, Status )

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use Analysis
            use ElementLibrary
            use ModStatus

            implicit none

            ! Input variables
            ! -----------------------------------------------------------------------------------
            type(ClassElementsWrapper) , dimension(:) , intent(in) :: ElementList
            type(ClassAnalysis)                       , intent(in) :: AnalysisSettings
            type(ClassStatus)                                      :: Status

            ! Output variables
            ! -----------------------------------------------------------------------------------
            real(8) , dimension(:) , intent(out) :: Fint

        end subroutine
        !==========================================================================================

        !==========================================================================================
!        subroutine ExternalForce( BC, LC, nSteps, Fext, DeltaFext )
!
!            !************************************************************************************
!            ! DECLARATIONS OF VARIABLES
!            !************************************************************************************
!            ! Modules and implicit declarations
!            ! -----------------------------------------------------------------------------------
!            use BoundaryConditions
!
!            implicit none
!
!            ! Input variables
!            ! -----------------------------------------------------------------------------------
!            type(ClassBoundaryConditions)  , intent(in)  :: BC
!            integer                        , intent(in)  :: nSteps , LC
!
!            ! Output variables
!            ! -----------------------------------------------------------------------------------
!            real(8) , dimension(:)  , intent(out)  :: Fext , DeltaFext
!
!        end subroutine
        !==========================================================================================

        !==========================================================================================
!        subroutine PrescribedDisplacement ( BC , LC , ST, NodalDispDOF, U, DeltaUPresc )
!
!            !************************************************************************************
!            ! DECLARATIONS OF VARIABLES
!            !************************************************************************************
!            ! Modules and implicit declarations
!            ! -----------------------------------------------------------------------------------
!            use BoundaryConditions
!
!            implicit none
!
!            ! Input variables
!            ! -----------------------------------------------------------------------------------
!            type(ClassBoundaryConditions)  :: BC
!            integer                        :: LC, ST
!
!            ! Output variables
!            ! -----------------------------------------------------------------------------------
!            real(8) , dimension(:)              :: U, DeltaUPresc
!            integer , pointer , dimension(:)    :: NodalDispDOF
!
!            !************************************************************************************
!        end subroutine
        !==========================================================================================

        !==========================================================================================
        subroutine TangentStiffnessMatrix( AnalysisSettings , ElementList , Kg )

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use Analysis
            use ElementLibrary
            use GlobalSparseMatrix

            implicit none

            ! Input variables
            ! -----------------------------------------------------------------------------------
            type(ClassAnalysis)                       , intent(in) :: AnalysisSettings
            type(ClassElementsWrapper) , dimension(:) , intent(in) :: ElementList
            type(ClassGlobalSparseMatrix)             , intent(in) :: Kg

            !************************************************************************************

        end subroutine
        !==========================================================================================


        !==========================================================================================
        subroutine UpdateMeshCoordinates(GlobalNodesList,AnalysisSettings,DeltaU)
             use Nodes
             use Analysis
             implicit none
             type (ClassNodes)    , pointer , dimension(:) :: GlobalNodesList
             real(8) , dimension(:)                        :: DeltaU
             type(ClassAnalysis)                           :: AnalysisSettings
        end subroutine
        !==============================================================================================


        !==============================================================================================
        subroutine MicroscopicDisplacement ( AnalysisSettings, GlobalNodesList, U, Umicro )

             use Nodes
             use Analysis

             implicit none

             type(ClassAnalysis)                           :: AnalysisSettings
             type (ClassNodes)    , pointer , dimension(:) :: GlobalNodesList
             real(8) , dimension(:)                        :: U, Umicro

         endsubroutine
        !==============================================================================================


        !==============================================================================================
        subroutine MaterialConstructor( Element, ElementList, GlobalNodesList, Material, AnalysisSettings )

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use Analysis
            use ElementLibrary
            use Nodes
            use ConstitutiveModelLibrary

            implicit none

            ! Input variables
            ! -----------------------------------------------------------------------------------
            class(ClassElement) , pointer                         :: Element
            type (ClassElementsWrapper) , pointer , dimension(:)  :: ElementList
            type(ClassAnalysis)                                   :: AnalysisSettings
            type(ClassNodes) , dimension(:) , pointer             :: GlobalNodesList
            class(ClassConstitutiveModelWrapper)  , pointer :: Material

        end subroutine
        !==============================================================================================



    end interface



end module
