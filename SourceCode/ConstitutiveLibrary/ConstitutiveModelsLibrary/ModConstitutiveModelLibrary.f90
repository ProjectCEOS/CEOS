!##################################################################################################
! This module is used to register a new Constitutive Model.
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
module ConstitutiveModelLibrary

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! DECLARATIONS OF VARIABLES
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! Modules and implicit declarations
	! ---------------------------------------------------------------------------------------------
    use ConstitutiveModel
    use ModGeneralizedHookesLaw
    use J2Plasticity
    use NeoHookean
    use NeoHookeanQ1P0
    use HyperelasticQ1P0
    use StVenantKirchhoff
    use CompressibleNeoHookean
    use NeoHookeanIsochoric

    ! Constitutive Models ID registered:
    type ClassConstitutiveModels
        integer   :: GeneralizedHookesLawModel      = 1
        integer   :: J2PlasticityModel              = 2
        integer   :: NeoHookeanModel                = 3
        integer   :: NeoHookeanQ1P0Model            = 4
        integer   :: StVenantKirchhoffModel         = 5
        integer   :: HyperelasticQ1P0Model          = 6
        integer   :: CompressibleNeoHookeanModel    = 7
        integer   :: NeoHookeanIsochoricModel       = 8
    end type

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

	type(ClassConstitutiveModels),parameter :: ConstitutiveModels=ClassConstitutiveModels()

    contains

		!==========================================================================================
        ! Routine AllocateConstitutiveModel: Routine that allocates the Constitutive Model
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine AllocateConstitutiveModel( MaterialModel , AnalysisSettings , nGP ,  GaussPoints )

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use Analysis

            ! Input variables
            ! -----------------------------------------------------------------------------------
            type(ClassAnalysis) , intent(in) :: AnalysisSettings
            integer , intent(in) :: MaterialModel , nGP

            ! Output variables
            ! -----------------------------------------------------------------------------------
            class(ClassConstitutiveModel),pointer,dimension(:),intent(out) :: GaussPoints

            ! Internal variables: Instance of each available Constitutive Model.
            ! -----------------------------------------------------------------------------------
            type(ClassGeneralizedHookesLaw_3D)          , pointer , dimension(:) :: GHL_3D

            type(ClassJ2Plasticity_PlaneStrain)        , pointer , dimension(:) :: VM_PlaneStrain
            type(ClassJ2Plasticity_3D)                 , pointer , dimension(:) :: VM_3D

            type(ClassNeoHookean_3D)                   , pointer , dimension(:) :: NH_3D
            type(ClassNeoHookean_Axisymmetric)         , pointer , dimension(:) :: NH_Axisymmetric

            type(ClassNeoHookeanQ1P0_ThreeDimensional) , pointer , dimension(:) :: NHQ1P0_ThreeDimensional
            type(ClassNeoHookeanQ1P0_Axisymmetric)     , pointer , dimension(:) :: NHQ1P0_Axisymmetric

            type(ClassStVenantKirchhoff_3D)            , pointer , dimension(:) :: StVK_ThreeDimensional
            type(ClassStVenantKirchhoff_Axisymmetric)  , pointer , dimension(:) :: StVK_Axisymmetric
            type(ClassStVenantKirchhoff_PlaneStrain)   , pointer , dimension(:) :: StVK_PlaneStrain

            type(ClassHyperelasticQ1P0_3D)             , pointer , dimension(:) :: HEQ1P0_3D
            type(ClassHyperelasticQ1P0_Axisymmetric)   , pointer , dimension(:) :: HEQ1P0_Axisymmetric

            type(ClassCompressibleNeoHookean_3D)       , pointer , dimension(:) :: CNH_3D
            type(ClassCompressibleNeoHookean_PlaneStrain) , pointer , dimension(:) :: CNH_PlaneStrain

            type(ClassNeoHookeanIsochoric_PlaneStrain) , pointer , dimension(:) :: NHI_PlaneStrain

! TODO (Thiago#1#02/13/15): Trocar threeDimensional para 3D

		    !************************************************************************************

            !************************************************************************************
            ! CONSTRUCT THE CONSTITUTIVE MODEL VARIABLES IN THE GAUSS POINTS

		    !************************************************************************************

            select case (MaterialModel)

                ! -------------------------------------------------------------------------
                ! Generalized Hooke's Law
                ! -------------------------------------------------------------------------------
                case (ConstitutiveModels % GeneralizedHookesLawModel)


                     if ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%ThreeDimensional ) then

                            allocate( GHL_3D(nGP) )
                            GaussPoints => GHL_3D

                    else
                            call Error("Error: Generalized Hooke's Model - analysis type not available.")

                    endif

                ! -------------------------------------------------------------------------
                ! J2 Plasticity Model (von Mises)
                ! -------------------------------------------------------------------------------
                case (ConstitutiveModels % J2PlasticityModel)

                    if ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%PlaneStrain ) then

                            allocate( VM_PlaneStrain(nGP) )
                            GaussPoints => VM_PlaneStrain

                    elseif ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%ThreeDimensional ) then

                            allocate( VM_3D(nGP) )
                            GaussPoints => VM_3D

                    else
                            call Error("Error: J2 Plasticity Model - analysis type not available.")

                    endif

                ! -------------------------------------------------------------------------------
                ! Neo-Hookean Model
                ! -------------------------------------------------------------------------------
                case (ConstitutiveModels % NeoHookeanModel)

                    if ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%ThreeDimensional ) then

                            allocate( NH_3D(nGP) )
                            GaussPoints => NH_3D

                    elseif ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%Axisymmetric ) then

                            allocate( NH_Axisymmetric(nGP) )
                            GaussPoints => NH_Axisymmetric

                    else
                            call Error("Error: Neo Hookean Model - analysis type not available.")

                    endif
                ! -------------------------------------------------------------------------------

                ! -------------------------------------------------------------------------------
                ! Neo-Hookean Model - Mean Dilatation
                ! -------------------------------------------------------------------------------
                case (ConstitutiveModels % NeoHookeanQ1P0Model)

                    if ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%ThreeDimensional ) then

                            allocate( NHQ1P0_ThreeDimensional(nGP) )
                            GaussPoints => NHQ1P0_ThreeDimensional

                    elseif ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%Axisymmetric ) then

                            allocate( NHQ1P0_Axisymmetric(nGP) )
                            GaussPoints => NHQ1P0_Axisymmetric

                    else
                            call Error("Error: Neo Hookean Q1P0 Model - analysis type not available.")

                    endif
                ! ------------------------------------------------------------------------------

                ! -------------------------------------------------------------------------------
                !  St. Venant-Kirchhoff Model
                ! -------------------------------------------------------------------------------
                case (ConstitutiveModels % StVenantKirchhoffModel)

                    if ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%ThreeDimensional ) then

                            allocate( StVK_ThreeDimensional(nGP) )
                            GaussPoints => StVK_ThreeDimensional

                    elseif ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%Axisymmetric ) then

                            allocate( StVK_Axisymmetric(nGP) )
                            GaussPoints => StVK_Axisymmetric

                    elseif ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%PlaneStrain ) then

                            allocate( StVK_PlaneStrain(nGP) )
                            GaussPoints => StVK_PlaneStrain

                    else
                            call Error("Error: St. Venant-Kirchhoff Model - analysis type not available.")

                    endif
                ! -----------------------------------------------------------------------------

                ! -------------------------------------------------------------------------------
                ! Hyperelastic Model - Mean Dilatation
                ! -------------------------------------------------------------------------------
                case (ConstitutiveModels % HyperelasticQ1P0Model)

                    if ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%ThreeDimensional ) then

                            allocate( HEQ1P0_3D(nGP) )
                            GaussPoints => HEQ1P0_3D

                    elseif ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%Axisymmetric ) then

                            allocate( HEQ1P0_Axisymmetric(nGP) )
                            GaussPoints => HEQ1P0_Axisymmetric

                    else
                            call Error("Error: Hyperelastic Q1P0 Model - analysis type not available.")

                    endif
                ! ------------------------------------------------------------------------------

                ! -------------------------------------------------------------------------------
                ! Compressible Neo-Hookean Model
                ! -------------------------------------------------------------------------------
                case (ConstitutiveModels % CompressibleNeoHookeanModel)

                    if ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%ThreeDimensional ) then

                            allocate( CNH_3D(nGP) )
                            GaussPoints => CNH_3D

                    elseif ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%PlaneStrain ) then

                            allocate( CNH_PlaneStrain(nGP) )
                            GaussPoints => CNH_PlaneStrain

                    else
                            call Error("Error: Compressible Neo Hookean Model - analysis type not available.")

                    endif
                ! -------------------------------------------------------------------------------

                ! -------------------------------------------------------------------------------
                ! Neo-Hookean Isochoric Model
                ! -------------------------------------------------------------------------------
                case (ConstitutiveModels % NeoHookeanIsochoricModel)

                    if ( AnalysisSettings%Hypothesis == HypothesisOfAnalysis%PlaneStrain ) then

                            allocate( NHI_PlaneStrain(nGP) )
                            GaussPoints => NHI_PlaneStrain

                    else
                            call Error("Error: Neo Hookean Isochoric Model - analysis type not available.")

                    endif
                ! -------------------------------------------------------------------------------

                case default

                    call Error( "Error: Constitutive Model not registered.")

            end select


            ! Construct the Constitutive Model
            ! -----------------------------------------------------------------------------------
           ! do i=1,nGP
           !     call GaussPoints(i)%ConstitutiveModelConstructor(AnalysisSettings)
           ! enddo

		    !************************************************************************************

        end subroutine
        !==========================================================================================

		!==========================================================================================
        ! Routine ConstitutiveModelIdentifier: Routine that identifies the Constitutive Model
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine ConstitutiveModelIdentifier( model, AnalysisSettings, modelID )

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use Analysis
            use Parser
            implicit none

            ! Input variables
            ! -----------------------------------------------------------------------------------
            type(ClassAnalysis) , intent(in) :: AnalysisSettings
            character(len=*) , intent(in)    :: model

            ! Output variables
            ! -----------------------------------------------------------------------------------
            integer , intent(out) :: modelID

            type(ClassParser) :: Comp

            !************************************************************************************


            !************************************************************************************
            ! DECODE THE STRING SUPPLIED BY GiD
		    !************************************************************************************

            call Comp%Setup()



            if ( Comp%CompareStrings('Generalized_Hookes_Law', model).and. (AnalysisSettings%ElementTech == ElementTechnologies%Full_Integration) ) then

                modelID = ConstitutiveModels % GeneralizedHookesLawModel

            elseif ( Comp%CompareStrings('j2_plasticity', model) .and. (AnalysisSettings%ElementTech == ElementTechnologies%Full_Integration) ) then

                modelID = ConstitutiveModels % J2PlasticityModel

            elseif ( Comp%CompareStrings('neo_hookean', model) .and. (AnalysisSettings%ElementTech == ElementTechnologies%Full_Integration) ) then

                modelID = ConstitutiveModels % NeoHookeanModel

            elseif ( Comp%CompareStrings('neo_hookean', model) .and. (AnalysisSettings%ElementTech == ElementTechnologies%Mean_Dilatation) ) then

                modelID = ConstitutiveModels % NeoHookeanQ1P0Model

            elseif ( Comp%CompareStrings('st_venant_kirchhoff', model) .and. (AnalysisSettings%ElementTech == ElementTechnologies%Full_Integration) ) then

                modelID = ConstitutiveModels % StVenantKirchhoffModel

            elseif ( Comp%CompareStrings('hyperelastic', model) .and. (AnalysisSettings%ElementTech == ElementTechnologies%Mean_Dilatation) ) then

                modelID = ConstitutiveModels % HyperelasticQ1P0Model

            elseif ( Comp%CompareStrings('compressible_neo_hookean', model) .and. (AnalysisSettings%ElementTech == ElementTechnologies%Full_Integration) ) then

                modelID = ConstitutiveModels%CompressibleNeoHookeanModel

            elseif ( Comp%CompareStrings('neo_hookean_isochoric', model) .and. (AnalysisSettings%ElementTech == ElementTechnologies%Full_Integration) ) then

                modelID = ConstitutiveModels%NeoHookeanIsochoricModel

            else
                call Error( "Error: Material Model not identified: "//trim(model))
            endif

		    !************************************************************************************

        end subroutine
        !==========================================================================================

end module



