!##################################################################################################
! This module has the attributes and methods to all Constitutive Models.
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
module ConstitutiveModel


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! ClassConstitutiveModel: Common definitions to all Constitutive Models
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type ClassConstitutiveModel

        real(8) , pointer , dimension(:)    :: Stress => null()
        real(8)                             :: F(3,3)=0.0d0
        real(8)                             :: T
        real(8)                             :: Jbar
        real(8)                             :: Time = 0.0d0



        contains

            ! Class Methods
            !------------------------------------------------------------------------------------

            !Dummy Procedures: To be used by the superclasses
            !------------------------------------------------------------------------------------
            procedure :: UpdateStressAndStateVariables => UpdateStressAndStateVariablesBase
            procedure :: GetTangentModulus => GetTangentModulusBase
            procedure :: SaveConvergedState => SaveConvergedStateBase
            procedure :: ConstitutiveModelConstructor => ConstitutiveModelConstructorBase
            procedure :: ReadMaterialParameters => ReadMaterialParametersBase
            procedure :: GetResult => GetResultBase
            procedure :: GetMatrixOfStresses => GetMatrixOfStressesBase
            procedure :: SecondDerivativesOfPSI_Jbar => SecondDerivativesOfPSI_JbarBase
            procedure :: CopyProperties => CopyPropertiesBase

            procedure :: LoadProperties => LoadPropertiesBase
            procedure :: LoadInternalVariables => LoadInternalVariablesBase
            procedure :: ExportInternalVariables => ExportInternalVariablesBase
            procedure :: LoadDataFromAnsys => LoadDataFromAnsysBase

        end type




	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type ClassConstitutiveModelWrapper

        class(ClassConstitutiveModel) , pointer , dimension(:) :: Mat => null()
        integer                                                :: MaterialID = -999
        integer                                                :: ModelEnumerator = -999

    end type
    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


    contains


		!==========================================================================================
        ! Dummy Procedures: To be used by the superclasses
        !==========================================================================================
        !==========================================================================================
            subroutine ConstitutiveModelConstructorBase(this,AnalysisSettings)
                use Analysis
                type(ClassAnalysis)::AnalysisSettings
                class(ClassConstitutiveModel)::this
                stop "Error: ConstitutiveModelConstructor"
            end subroutine
            !==========================================================================================
            subroutine GetTangentModulusBase(this,D)
                class(ClassConstitutiveModel)::this
                real(8),dimension(:,:),intent(inout)::D
                stop "Error: GetTangentModulus"
            end subroutine
            !==========================================================================================
            subroutine UpdateStressAndStateVariablesBase(this)
                class(ClassConstitutiveModel)::this
                stop "Error: ConstitutiveAnalysis "
            end subroutine
            !==========================================================================================
            subroutine SaveConvergedStateBase(this)
                class(ClassConstitutiveModel)::this
                stop "Error: UpdateStateVariables "
            end subroutine
            !==========================================================================================
            subroutine ReadMaterialParametersBase(this,DataFile)
                use Parser
                class(ClassConstitutiveModel)::this
                type(ClassParser)::DataFile
                !integer , intent(in):: FileNum
                stop "Error: ReadMaterialParameters"
            end subroutine
            !==========================================================================================
            ! TODO (Thiago#1#03/11/15): Passar o Analysis Settings - obter informações dependendo do tipo de análise
! TODO (Thiago#1#11/03/15): Implementar uma classe de Resultados
!

            subroutine GetResultBase(this, ID , Name , Length , Variable , VariableType )
                class(ClassConstitutiveModel) :: this
                integer                       :: ID,Length,VariableType
                character(len=*)              :: Name
                real(8) , dimension(:)        :: Variable
                stop "Error: GetResult"
            end subroutine
            !==========================================================================================
            subroutine SecondDerivativesOfPSI_JbarBase(this,d2PSIvol_dJbar2)
                class(ClassConstitutiveModel) :: this
                real (8) :: d2PSIvol_dJbar2
                stop "Error: SecondDerivativesOfPSI_Jbar"
            end subroutine
            !==========================================================================================
            subroutine CopyPropertiesBase(this,Reference)
                class(ClassConstitutiveModel) :: this , Reference
                stop "Error: CopyProperties"
            end subroutine
            !==========================================================================================
            subroutine LoadPropertiesBase(this, Props)
                class(ClassConstitutiveModel) :: this
                real(8),dimension(:) :: Props
                stop "Error: LoadPropertiesBase"
            end subroutine
            !==========================================================================================
            subroutine LoadInternalVariablesBase(this, IntVars)
                class(ClassConstitutiveModel) :: this
                real(8),dimension(:) :: IntVars
                stop "Error: LoadInternalVariablesBase"
            end subroutine
             !==========================================================================================
            subroutine ExportInternalVariablesBase(this, IntVars)
                class(ClassConstitutiveModel) :: this
                real(8),dimension(:) :: IntVars
                stop "Error: ExportInternalVariablesBase"
            end subroutine
            !==========================================================================================
            subroutine LoadDataFromAnsysBase(this, Time,dTime,Temp,dTemp,defgrad , defGrad_t )
                class(ClassConstitutiveModel) :: this
                real(8) :: Time , dTime , Temp , dTemp
                real(8) , dimension(3,3) :: defGrad , defGrad_t
                stop "Error: LoadDataFromAnsysBase"
            end subroutine


!____________________________________________________________________________________________________________________________________________________

        subroutine GetMatrixOfStressesBase(this,AnalysisSettings,S)


		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use Analysis

            implicit none


            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassConstitutiveModel) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            type(ClassAnalysis) , intent(in) :: AnalysisSettings

            real(8), dimension(:,:) :: S

           S = 0.0d0
           select case (AnalysisSettings%Hypothesis)

                !case (HypothesisOfAnalysis%PlaneStrain)

                    !S(1,1) = this%Stress(1)
                    !S(2,2) = this%Stress(2)
                    !S(1,2) = this%Stress(3)
                    !S(2,1) = this%Stress(3)

                    !S(3,3) = S(1,1)
                    !S(4,4) = S(2,2)
                    !S(3,4) = S(1,2)
                    !S(4,3) = S(1,2)

                !case (HypothesisOfAnalysis%PlaneStress)


                case (HypothesisOfAnalysis%Axisymmetric)

                    ! Upper Triangular!!!
                    S(1,1) = this%Stress(1)
                    S(2,2) = this%Stress(2)
                    S(1,2) = this%Stress(4)
                    S(2,1) = this%Stress(4)

                    S(3,3) = this%Stress(1)
                    S(4,4) = this%Stress(2)
                    S(3,4) = this%Stress(4)
                    S(4,3) = this%Stress(4)

                    S(5,5) = this%Stress(3)



                case (HypothesisOfAnalysis%ThreeDimensional)

                    ! Upper Triangular!!!
                    S(1,1) = this%Stress(1)
                    S(2,2) = this%Stress(2)
                    S(3,3) = this%Stress(3)
                    S(1,2) = this%Stress(4)
                    S(2,3) = this%Stress(5)
                    S(1,3) = this%Stress(6)

                    S(4,4) = this%Stress(1)
                    S(5,5) = this%Stress(2)
                    S(6,6) = this%Stress(3)
                    S(4,5) = this%Stress(4)
                    S(5,6) = this%Stress(5)
                    S(4,6) = this%Stress(6)

                    S(7,7) = this%Stress(1)
                    S(8,8) = this%Stress(2)
                    S(9,9) = this%Stress(3)
                    S(7,8) = this%Stress(4)
                    S(8,9) = this%Stress(5)
                    S(7,9) = this%Stress(6)


                case default
                    stop "Error: subroutine GetMatrixOfStresses Hypothesis not identified."

            end select

        end subroutine

end module
