!##################################################################################################
! This module has the attributes and methods for the Hyperelastic material model.
!--------------------------------------------------------------------------------------------------
! Date: 2015/03
!
! Authors:  Jan-Michel Farias
!           Thiago Andre Carniel
!           Paulo Bastos de Castro
!!------------------------------------------------------------------------------------------------
! Modifications:
! Date:         Author:
!##################################################################################################
module Microstructure

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! DECLARATIONS OF VARIABLES
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Modules and implicit declarations
    ! --------------------------------------------------------------------------------------------
    use ConstitutiveModel
    use FEMAnalysis
    implicit none


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! "NameOfTheMaterialModel"Properties: Material Properties
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type MicrostructureProperties

        ! Settings File of Microstructure
        !----------------------------------------------------------------------------------------------
        character(len=255) :: SettingsFile = ''

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel": Attributes and methods of the constitutive model
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , extends(ClassConstitutiveModel) :: ClassMicrostructure

        !Obs.: The ClassConstitutiveModel already has the variables:
        ! - stress (Voigt notation)
        ! - F (Deformation Gradient) (3x3 Tensor Components)
        ! - Jbar - Mean Dilation variable related to Simo-Taylor-Pister Variational Approach


		! Class Attributes : Usually the internal variables
		!----------------------------------------------------------------------------------------


		! Class Attributes : Material Properties
		!----------------------------------------------------------------------------------------
        type (MicrostructureProperties), pointer :: Properties => null()

		! Class Attributes : Microstructure FEM Analysis
		!----------------------------------------------------------------------------------------
        type (ClassFEMAnalysis), pointer :: MicroFEMAnalysis => null()

        contains

            ! Class Methods
            !----------------------------------------------------------------------------------
             procedure :: ConstitutiveModelConstructor => ConstitutiveModelConstructor_Microstructure
             procedure :: ReadMaterialParameters       => ReadMaterialParameters_Microstructure
             procedure :: GetResult                    => GetResult_Microstructure
             procedure :: SwitchConvergedState           => SwitchConvergedState_Microstructure
             procedure :: SecondDerivativesOfPSI_Jbar  => SecondDerivativesOfPSI_Jbar_Microstructure
             procedure :: CopyProperties               => CopyProperties_Microstructure

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel"_3D: Attributes and methods of the constitutive model
    ! in Three-Dimensional analysis.
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , extends(ClassMicrostructure) :: ClassMicrostructure_3D

         contains
            ! Class Methods
            !----------------------------------------------------------------------------------
             procedure :: UpdateStressAndStateVariables  =>  UpdateStressAndStateVariables_Microstructure_3D
             procedure :: GetTangentModulus              =>  GetTangentModulus_Microstructure_3D

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



    contains


        !==========================================================================================
        ! Method ReadMaterialParameters_"NameOfTheMaterialModel": Routine that reads the material
        ! parameters
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine ReadMaterialParameters_Microstructure(this,DataFile)


		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! ---------------------------------------------------------------------------------
            use Parser

            ! Object
            ! ---------------------------------------------------------------------------------
            class(ClassMicrostructure) :: this

            ! Input variables
            ! ---------------------------------------------------------------------------------
            type(ClassParser) :: DataFile

            ! Internal variables
            ! ---------------------------------------------------------------------------------
		    character(len=100), dimension(1) :: ListOfOptions, ListOfValues
		    integer                          :: i
		    !************************************************************************************

		    !___________________   WARNIG! DO NOT CHANGE OR ERASE THIS BLOCK    _________________
		    ! All constitutive models must allocate its own properties!
		    allocate (this%Properties)
		    !____________________________________________________________________________________

            !************************************************************************************
            ! READ THE MATERIAL PARAMETERS
		    !************************************************************************************

            ! Inform how the properties are shown in the "Settings" file.
            !------------------------------------------------------------------------------------
            ListOfOptions=["Settings File"]
            !------------------------------------------------------------------------------------

		    !___________________   WARNIG! DO NOT CHANGE OR ERASE THIS BLOCK    _________________
            call DataFile%FillListOfOptions(ListOfOptions,ListOfValues)
		    !____________________________________________________________________________________


            ! Obs.: ListOfValues index must match with ListOfOptions index
            !------------------------------------------------------------------------------------
            this%Properties%SettingsFile = ListOfValues(1)
            !------------------------------------------------------------------------------------


            !************************************************************************************

        end subroutine
        !==========================================================================================



        !==========================================================================================
        ! Method ConstitutiveModelConstructor_"NameOfTheMaterialModel": Routine that constructs the
        ! Constitutive Model
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine ConstitutiveModelConstructor_Microstructure(this,AnalysisSettings)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use Analysis

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassMicrostructure) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            type(ClassAnalysis) :: AnalysisSettings

		    !************************************************************************************

 		    !************************************************************************************
            ! ALLOCATE THE STATE VARIABLES
		    !************************************************************************************

            allocate( this%Stress( AnalysisSettings%StressSize ) ) ; this%Stress= 0.0d0

		    !************************************************************************************

 		    !************************************************************************************
            ! CONTRUCT THE FEM MICROSTRUCTURE ANALYSIS
		    !************************************************************************************

            !Read the "Settings File" of microstructure and construct de FEM analysis
            call this%MicroFEMAnalysis%ReadInputData( this%Properties%SettingsFile )

            ! Allocating memory for the sparse matrix (pre-assembling)
            call FEMAnlysis1%AllocateGlobalSparseStiffnessMatrix

		    !************************************************************************************

        end subroutine
        !==========================================================================================



        !==========================================================================================
        ! Method CopyProperties_"NameOfTheMaterialModel": Routine that associates the material
        ! parameters in the Gauss Points
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine CopyProperties_Microstructure(this,Reference)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! ---------------------------------------------------------------------------------
            class(ClassMicrostructure) :: this

            ! Input variables
            ! ---------------------------------------------------------------------------------
            class(ClassConstitutiveModel) :: Reference

		    !************************************************************************************

            ! Change field: "class is ( Class"NameOfTheMaterialModel"Q1P0 )"
            !-----------------------------------------------------------------------------------
             select type ( Reference )

                 class is ( ClassMicrostructure )
                    this%Properties => Reference%Properties
                 class default
                     stop "Error: Subroutine CopyProperties"
            end select
            !-----------------------------------------------------------------------------------

            !************************************************************************************

        end subroutine
        !==========================================================================================

        !==========================================================================================
        ! Internal Subroutine:  Compute the first derivatives of isochoric Strain Energy function
        !                       with respect to:
        !                       - Isochoric Right-Cauchy Green Strain (dPSIiso_dCiso)
        !                       - Mean Dilatation (dPSIvol_dJbar)
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine FirstDerivativesOfPSI ( Properties, Jbar, Ciso, dPSIiso_dCiso, dPSIvol_dJbar  )

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! ---------------------------------------------------------------------------------
            use MathRoutines

            ! Object
            ! -----------------------------------------------------------------------------------
            type(HyperelasticQ1P0Properties) :: Properties

            ! Input variables
            ! -----------------------------------------------------------------------------------
            real (8) :: Jbar, Ciso(3,3)

            ! Output variables
            ! -----------------------------------------------------------------------------------
            real (8) :: dPSIiso_dCiso(3,3), dPSIvol_dJbar

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real (8) :: C10, BulkModulus, I(3,3)

            !************************************************************************************
            ! TANGENT MODULUS
		    !************************************************************************************

            ! Optional: Retrieve Properties
            ! -----------------------------------------------------------------------------------
		    C10         = Properties%C10
		    BulkModulus = Properties%BulkModulus
            ! -----------------------------------------------------------------------------------

            ! Identity
            I = 0.0d0
            I(1,1) = 1.0d0
            I(2,2) = 1.0d0
            I(3,3) = 1.0d0


            ! Fist Derivative with respect to Isochoric Right-Cauchy Green Strain
            !--------------------------------------------------------------------
            dPSIiso_dCiso = C10*I
            !--------------------------------------------------------------------


            ! Fist Derivative with respect to Mean Dilatation
            !--------------------------------------------------------------------
		    dPSIvol_dJbar = 3.0d0*BulkModulus*( Jbar**(-2.0d0/3.0d0) )*( Jbar**(1.0d0/3.0d0) - 1.0d0 )
            !--------------------------------------------------------------------

		    !************************************************************************************

        end subroutine
        !==========================================================================================

        !==========================================================================================
        ! Internal Subroutine:  Compute the second derivative of isochoric Strain Energy function
        !                       with respect to Isochoric Right-Cauchy Green Strain (dPSIiso_dCiso)
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine SecondDerivativesOfPSI_Ciso ( Properties, Ciso, d2PSIiso_dCiso2  )

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! ---------------------------------------------------------------------------------
            use MathRoutines

            ! Object
            ! -----------------------------------------------------------------------------------
            type(HyperelasticQ1P0Properties) :: Properties

            ! Input variables
            ! -----------------------------------------------------------------------------------
            real (8) :: Ciso(6)

            ! Output variables
            ! -----------------------------------------------------------------------------------
            real (8) :: d2PSIiso_dCiso2(6,6)

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real (8) :: C10, BulkModulus

            !************************************************************************************
            ! TANGENT MODULUS
		    !************************************************************************************

            ! Optional: Retrieve Properties
            ! -----------------------------------------------------------------------------------
		    C10         = Properties%C10
		    BulkModulus = Properties%BulkModulus
            ! -----------------------------------------------------------------------------------


            ! Second Derivative with respect to Isochoric Right-Cauchy Green Strain
            !--------------------------------------------------------------------
            d2PSIiso_dCiso2 = 0.0d0
            !--------------------------------------------------------------------


		    !************************************************************************************

        end subroutine
        !==========================================================================================

        !==========================================================================================
        ! Method UpdateStateVariables_"NameOfTheMaterialModel":  Compute the second derivatives of
        ! volumetric Strain Energy function with respect to Mean Dilatation
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine SecondDerivativesOfPSI_Jbar_HyperelasticQ1P0 ( this, d2PSIvol_dJbar2   )

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! ---------------------------------------------------------------------------------
            use MathRoutines

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassHyperelasticQ1P0) :: this

            ! Output variables
            ! -----------------------------------------------------------------------------------
            real (8) ::  d2PSIvol_dJbar2

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real (8) :: C10, BulkModulus

            !************************************************************************************
            ! TANGENT MODULUS
		    !************************************************************************************

            ! Optional: Retrieve Properties
            ! -----------------------------------------------------------------------------------
		    C10         = this%Properties%C10
		    BulkModulus = this%Properties%BulkModulus
            ! -----------------------------------------------------------------------------------


            ! Second Derivative with respect to Mean Dilatation
            !--------------------------------------------------------------------
		    d2PSIvol_dJbar2 = ( -this%Properties%BulkModulus*this%Jbar**(-5.0d0/3.0d0) ) * &
                              ( this%Jbar**(1.0d0/3.0d0) - 2.0d0  )

            !--------------------------------------------------------------------

		    !************************************************************************************

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method UpdateStateVariables_"NameOfTheMaterialModel"_Axisymmetric: Routine that
        ! contains the algorithm employed to update the state variables.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine UpdateStressAndStateVariables_HyperelasticQ1P0_Axisymmetric(this)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! ---------------------------------------------------------------------------------
            use MathRoutines

            ! Object
            ! ---------------------------------------------------------------------------------
            class(ClassHyperelasticQ1P0_Axisymmetric) :: this

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: F(3,3), C(3,3), Cinv(3,3), Ciso(3,3), I(3,3), S(3,3), Sfric(3,3), devSfric(3,3)

            real(8) :: dPSIiso_dCiso(3,3), dPSIvol_dJbar

            real(8) :: aux(6)

            real(8) :: J, Jbar, pbar, BulkModulus, C10

		    !************************************************************************************

            !___________________________________________________________________________________
            !______________________________    REMARK    _______________________________________
            !
            !  DUE TO THE UPDATED LAGRANGIAN FORMULATION, THE OUTPUT STRESS MUST BE THE
            !  CAUCHY STRESS IN VOIGT NOTATION.
            !___________________________________________________________________________________


            !************************************************************************************
            ! ALGORITHM THAT UPDATES STRESS AND STATE VARIABLES
		    !************************************************************************************

            ! Optional: Retrieve Variables
            ! -----------------------------------------------------------------------------------
            BulkModulus = this%Properties%BulkModulus
            C10         = this%Properties%C10
            F           = this%F
            Jbar        = this%Jbar
            ! -----------------------------------------------------------------------------------

            ! Right-Cauchy Green Strain - Calculated in 3D Tensorial Format
            ! -----------------------------------------------------------------------------------
            ! Identity
            I = 0.0d0
            I(1,1) = 1.0d0
            I(2,2) = 1.0d0
            I(3,3) = 1.0d0

            ! Right-Cauchy Green Strain
            C = matmul(transpose(F),F)
            ! -----------------------------------------------------------------------------------


            ! Modified Second Piola-Kirchhoff Stress - Calculated in 3D Tensorial Format
            ! -----------------------------------------------------------------------------------
            ! Jacobian
            J = det(F)

            ! Inverse of Right-Cauchy Green Strain
            Cinv = inverse(C)

            ! Isochoric part of the Right-Cauchy Green Strain
            Ciso = (J**(-2.0d0/3.0d0))*C

            ! Computations of the first derivatives of isochoric Strain Energy function with respected to:
            ! dPSIiso_dCiso = Isochoric Right-Cauchy Green Strain
            ! dPSIvol_dJbar = Mean Dilatation
            call FirstDerivativesOfPSI ( this%Properties, Jbar, Ciso, dPSIiso_dCiso, dPSIvol_dJbar  )

            ! Second Piola-Kirchhoff Frictional
            Sfric = 2.0d0*dPSIiso_dCiso

            ! Deviatoric part of the Second Piola-Kirchhoff Frictional
            devSfric = Sfric - (1.0d0/3.0d0)*Tensor_Inner_Product(Sfric,C)*Cinv

            ! Modified Hydrostatic Pressure
            pbar = dPSIvol_dJbar

            ! Modified Second Piola-Kirchhoff Stress
            S = (J**(-2.0d0/3.0d0))*devSfric + J*pbar*Cinv
            ! -----------------------------------------------------------------------------------

            ! Modified Cauchy Stress - Calculated in 3D Tensorial Format and converted to Voigt
            ! notation.
            ! -----------------------------------------------------------------------------------
            S = matmul(matmul(F,S),transpose(F))/J

            aux = Convert_to_Voigt(S)

            this%Stress = aux(1:4)
            ! -----------------------------------------------------------------------------------


		    !************************************************************************************

        end subroutine
        !==========================================================================================



        !==========================================================================================
        ! Method GetTangentModulus_"NameOfTheMaterialModel"_Axisymmetric: Routine that evaluates the
        ! Tangente Modulus.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine GetTangentModulus_HyperelasticQ1P0_Axisymmetric(this,D)


		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! ---------------------------------------------------------------------------------
            use MathRoutines

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassHyperelasticQ1P0_Axisymmetric) :: this

            ! Output variables
            ! -----------------------------------------------------------------------------------
            real(8) , dimension(:,:) :: D

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: J, Jbar, pbar, D2psiDJ2, BulkModulus, C10
            real(8) :: F(3,3), C(3,3),Cinv(3,3), Ciso(3,3), Sfric(3,3)


            real(8) :: CV(6), CinvV(6), CisoV(6), SfricV(6), devSfricV(6), SisoV(6)
            real(8) :: PmV(6,6) , PV(6,6), Diso(6,6), Dp(6,6), Dbar(6,6), Is(6,6)
            real(8) :: dPSIiso_dCiso(3,3), dPSIvol_dJbar , d2PSIiso_dCiso2(6,6)


            real(8) :: auxV(6) , auxT(6,6)

		    !************************************************************************************

            !************************************************************************************
            ! TANGENT MODULUS
		    !************************************************************************************

            ! Optional: Retrieve Variables
            ! -----------------------------------------------------------------------------------
            BulkModulus = this%Properties%BulkModulus
            C10         = this%Properties%C10
            F           = this%F
            Jbar        = this%Jbar
            ! -----------------------------------------------------------------------------------

            ! Quantities calculated in 3D Tensorial Format
            ! -----------------------------------------------------------------------------------
            ! Right-Cauchy Green Strain
            C = matmul(transpose(F),F)

            ! Inverse of Right-Cauchy Green Strain
            Cinv = inverse(C)

            ! Isochoric part of the Right-Cauchy Green Strain
            Ciso = (J**(-2.0d0/3.0d0))*C

            ! Jacobian
            J = det(F)

            ! Computations of the first derivatives of isochoric Strain Energy function with respected to:
            ! dPSIiso_dCiso = Isochoric Right-Cauchy Green Strain
            ! dPSIvol_dJbar = Mean Dilatation
            call FirstDerivativesOfPSI ( this%Properties, Jbar, Ciso, dPSIiso_dCiso, dPSIvol_dJbar  )

            ! Second Piola-Kirchhoff Frictional
            Sfric = 2.0d0*dPSIiso_dCiso

            ! Modified Hydrostatic Pressure
            pbar = dPSIvol_dJbar

            ! -----------------------------------------------------------------------------------


            ! -----------------------------------------------------------------------------------
            ! The subsequent computations are made in Voigt notation
            ! -----------------------------------------------------------------------------------


            ! Material tangent modulus in referential configuration
            ! -----------------------------------------------------------------------------------

            ! Right-Cauchy Green Strain
            CV = Convert_to_Voigt(C)

            ! Inverse of Right-Cauchy Green Strain
            CinvV = Convert_to_Voigt(Cinv)

            ! Isochoric part of the Right-Cauchy Green Strain
            CisoV = Convert_to_Voigt(Ciso)

            ! Second Piola-Kirchhoff Frictional
            SfricV = Convert_to_Voigt(Sfric)

            ! Deviatoric part of the Second Piola-Kirchhoff Frictional
            devSfricV = SfricV - (1.0d0/3.0d0)*Inner_Product_Voigt(SfricV,CV)*CinvV

            ! Isochoric part of the Second Piola-Kirchhoff
            SisoV = (J**(-2.0d0/3.0d0))*devSfricV

            ! Projection Operator
            PV = IsymV() - (1.0d0/3.0d0)*Ball_Voigt(CinvV,CV)

            ! Modified Projection Operator
            PmV = Square_Voigt(CinvV,CinvV) - (1.0d0/3.0d0)*Ball_Voigt(CinvV,CinvV)

            ! Computations of the first derivatives of isochoric Strain Energy function with respected to:
            ! dPSIiso_dCiso = Isochoric Right-Cauchy Green Strain
            ! dPSIvol_dJbar = Mean Dilatation
            call SecondDerivativesOfPSI_Ciso ( this%Properties, CisoV, d2PSIiso_dCiso2 )


            ! Isochoric part of the material tangent modulus in referential configuration
            Diso = Tensor_4_Double_Contraction_Voigt( Tensor_4_Double_Contraction_Voigt(PV,d2PSIiso_dCiso2),transpose(Pv)) + &
                    (2.0d0/3.0d0)*(J**(-2.0d0/3.0d0))*Inner_Product_Voigt(SfricV,CV)*PmV - &
                    (2.0d0/3.0d0)*( Ball_Voigt(SisoV,CinvV) + Ball_Voigt(CinvV,SisoV) )

            ! Pressure component of the material tangent modulus in referential configuration
            Dp  = J*pbar*( Ball_Voigt(CinvV,CinvV) - 2.0d0*Square_Voigt(CinvV,CinvV)  )

            ! Modified material tangent modulus in referential configuration
            Dbar = Diso + Dp
            ! -----------------------------------------------------------------------------------

            ! Push-Forward:
            ! Computation of the modified spatial tangent modulus
            ! -----------------------------------------------------------------------------------
            auxT = Push_Forward_Voigt(Dbar,F)

            ! Axisymmetric mapping
            D = auxT(1:4,1:4)
            ! -----------------------------------------------------------------------------------


		    !************************************************************************************

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method UpdateStateVariables_"NameOfTheMaterialModel"_3D: Routine that
        ! contains the algorithm employed to update the state variables.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine UpdateStressAndStateVariables_HyperelasticQ1P0_3D(this)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! ---------------------------------------------------------------------------------
            use MathRoutines

            ! Object
            ! ---------------------------------------------------------------------------------
            class(ClassHyperelasticQ1P0_3D) :: this

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: F(3,3), C(3,3), Cinv(3,3),Ciso(3,3), I(3,3), S(3,3), Sfric(3,3), devSfric(3,3)
            real(8) :: dPSIiso_dCiso(3,3), dPSIvol_dJbar
            real(8) :: J, Jbar, pbar, BulkModulus, C10

		    !************************************************************************************

            !___________________________________________________________________________________
            !______________________________    REMARK    _______________________________________
            !
            !  DUE TO THE UPDATED LAGRANGIAN FORMULATION, THE OUTPUT STRESS MUST BE THE
            !  CAUCHY STRESS IN VOIGT NOTATION.
            !___________________________________________________________________________________


            !************************************************************************************
            ! ALGORITHM THAT UPDATES STRESS AND STATE VARIABLES
		    !************************************************************************************

            ! Optional: Retrieve Variables
            ! -----------------------------------------------------------------------------------
            BulkModulus = this%Properties%BulkModulus
            C10         = this%Properties%C10
            F           = this%F
            Jbar        = this%Jbar
            ! -----------------------------------------------------------------------------------

            ! Right-Cauchy Green Strain - Calculated in 3D Tensorial Format
            ! -----------------------------------------------------------------------------------
            ! Identity
            I = 0.0d0
            I(1,1) = 1.0d0
            I(2,2) = 1.0d0
            I(3,3) = 1.0d0

            ! Right-Cauchy Green Strain
            C = matmul(transpose(F),F)
            ! -----------------------------------------------------------------------------------


            ! Modified Second Piola-Kirchhoff Stress - Calculated in 3D Tensorial Format
            ! -----------------------------------------------------------------------------------
            ! Jacobian
            J = det(F)

            ! Inverse of Right-Cauchy Green Strain
            Cinv = inverse(C)

            ! Isochoric part of the Right-Cauchy Green Strain
            Ciso = (J**(-2.0d0/3.0d0))*C

            ! Computations of the first derivatives of isochoric Strain Energy function with respected to:
            ! dPSIiso_dCiso = Isochoric Right-Cauchy Green Strain
            ! dPSIvol_dJbar = Mean Dilatation
            call FirstDerivativesOfPSI ( this%Properties, Jbar, Ciso, dPSIiso_dCiso, dPSIvol_dJbar  )

            ! Second Piola-Kirchhoff Frictional
            Sfric = 2.0d0*dPSIiso_dCiso

            ! Deviatoric part of the Second Piola-Kirchhoff Frictional
            devSfric = Sfric - (1.0d0/3.0d0)*Tensor_Inner_Product(Sfric,C)*Cinv

            ! Modified Hydrostatic Pressure
            pbar = dPSIvol_dJbar

            ! Modified Second Piola-Kirchhoff Stress
            S = (J**(-2.0d0/3.0d0))*devSfric + J*pbar*Cinv
            ! -----------------------------------------------------------------------------------

            ! Modified Cauchy Stress - Calculated in 3D Tensorial Format and converted to Voigt
            ! notation.
            ! -----------------------------------------------------------------------------------
            S = matmul(matmul(F,S),transpose(F))/J

            this%Stress = Convert_to_Voigt(S)
            ! -----------------------------------------------------------------------------------


		    !************************************************************************************

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method GetTangentModulus_"NameOfTheMaterialModel"_3D: Routine that evaluates the
        ! Tangente Modulus.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine GetTangentModulus_HyperelasticQ1P0_3D(this,D)


		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! ---------------------------------------------------------------------------------
            use MathRoutines

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassHyperelasticQ1P0_3D) :: this

            ! Output variables
            ! -----------------------------------------------------------------------------------
            real(8) , dimension(:,:) :: D

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: J, Jbar, pbar, D2psiDJ2, BulkModulus, C10
            real(8) :: F(3,3), C(3,3),Cinv(3,3), Ciso(3,3), Sfric(3,3)


            real(8) :: CV(6), CinvV(6), CisoV(6), SfricV(6), devSfricV(6), SisoV(6)
            real(8) :: PmV(6,6) , PV(6,6), Diso(6,6), Dp(6,6), Dbar(6,6), Is(6,6)
            real(8) :: dPSIiso_dCiso(3,3), dPSIvol_dJbar, d2PSIiso_dCiso2(6,6)

		    !************************************************************************************

            !************************************************************************************
            ! TANGENT MODULUS
		    !************************************************************************************

            ! Optional: Retrieve Variables
            ! -----------------------------------------------------------------------------------
            BulkModulus = this%Properties%BulkModulus
            C10         = this%Properties%C10
            F           = this%F
            Jbar        = this%Jbar
            ! -----------------------------------------------------------------------------------

            ! Quantities calculated in 3D Tensorial Format
            ! -----------------------------------------------------------------------------------
            ! Right-Cauchy Green Strain
            C = matmul(transpose(F),F)

            ! Inverse of Right-Cauchy Green Strain
            Cinv = inverse(C)

            ! Isochoric part of the Right-Cauchy Green Strain
            Ciso = (J**(-2.0d0/3.0d0))*C

            ! Jacobian
            J = det(F)

            ! Computations of the first derivatives of isochoric Strain Energy function with respected to:
            ! dPSIiso_dCiso = Isochoric Right-Cauchy Green Strain
            ! dPSIvol_dJbar = Mean Dilatation
            call FirstDerivativesOfPSI ( this%Properties, Jbar, Ciso, dPSIiso_dCiso, dPSIvol_dJbar  )

            ! Second Piola-Kirchhoff Frictional
            Sfric = 2.0d0*dPSIiso_dCiso

            ! Modified Hydrostatic Pressure
            pbar = dPSIvol_dJbar

            ! -----------------------------------------------------------------------------------


            ! -----------------------------------------------------------------------------------
            ! The subsequent computations are made in Voigt notation
            ! -----------------------------------------------------------------------------------


            ! Material tangent modulus in referential configuration
            ! -----------------------------------------------------------------------------------

            ! Right-Cauchy Green Strain
            CV = Convert_to_Voigt(C)

            ! Inverse of Right-Cauchy Green Strain
            CinvV = Convert_to_Voigt(Cinv)

            ! Isochoric part of the Right-Cauchy Green Strain
            CisoV = Convert_to_Voigt(Ciso)

            ! Second Piola-Kirchhoff Frictional
            SfricV = Convert_to_Voigt(Sfric)

            ! Deviatoric part of the Second Piola-Kirchhoff Frictional
            devSfricV = SfricV - (1.0d0/3.0d0)*Inner_Product_Voigt(SfricV,CV)*CinvV

            ! Isochoric part of the Second Piola-Kirchhoff
            SisoV = (J**(-2.0d0/3.0d0))*devSfricV

            ! Projection Operator
            PV = IsymV() - (1.0d0/3.0d0)*Ball_Voigt(CinvV,CV)

            ! Modified Projection Operator
            PmV = Square_Voigt(CinvV,CinvV) - (1.0d0/3.0d0)*Ball_Voigt(CinvV,CinvV)

            ! Computations of the first derivatives of isochoric Strain Energy function with respected to:
            ! dPSIiso_dCiso = Isochoric Right-Cauchy Green Strain
            ! dPSIvol_dJbar = Mean Dilatation
            call SecondDerivativesOfPSI_Ciso ( this%Properties, CisoV, d2PSIiso_dCiso2 )


            ! Isochoric part of the material tangent modulus in referential configuration
            Diso = Tensor_4_Double_Contraction_Voigt( Tensor_4_Double_Contraction_Voigt(PV,d2PSIiso_dCiso2),transpose(Pv)) + &
                    (2.0d0/3.0d0)*(J**(-2.0d0/3.0d0))*Inner_Product_Voigt(SfricV,CV)*PmV - &
                    (2.0d0/3.0d0)*( Ball_Voigt(SisoV,CinvV) + Ball_Voigt(CinvV,SisoV) )

            ! Pressure component of the material tangent modulus in referential configuration
            Dp  = J*pbar*( Ball_Voigt(CinvV,CinvV) - 2.0d0*Square_Voigt(CinvV,CinvV)  )

            ! Modified material tangent modulus in referential configuration
            Dbar = Diso + Dp
            ! -----------------------------------------------------------------------------------

            ! Push-Forward:
            ! Computation of the modified spatial tangent modulus
            ! -----------------------------------------------------------------------------------
            D = Push_Forward_Voigt(Dbar,F)
            ! -----------------------------------------------------------------------------------


		    !************************************************************************************

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method SwitchConvergedState_"NameOfTheMaterialModel": Routine that save de converged state.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine SwitchConvergedState_HyperelasticQ1P0(this)
            class(ClassHyperelasticQ1P0) :: this
        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method GetTangentModulus_"NameOfTheMaterialModel"_3D: Routine that evaluates the
        ! Tangente Modulus.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine GetResult_HyperelasticQ1P0(this, ID , Name , Length , Variable , VariableType  )

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! ---------------------------------------------------------------------------------
            use MathRoutines

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassHyperelasticQ1P0) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            integer :: ID

            ! Output variables
            ! -----------------------------------------------------------------------------------
            character(len=*)            :: Name
            integer                     :: Length, VariableType
            real(8) , dimension(:)      :: Variable

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            integer, parameter :: Scalar=1,Vector=2,Tensor=3
            real (8) :: h , c(6), I(3,3), e(3,3), eV(6)
		    !************************************************************************************

		    !___________________   WARNIG! DO NOT CHANGE OR ERASE THIS BLOCK    _________________
		    ! Initializing variable name.
		    Name = ''
		    !____________________________________________________________________________________


            ! Template to Export Result to GiD
            !------------------------------------------------------------------------------------

            !case(0)
                ! Inform the number of results
                !Length = 3
            !case(1)
                !Name = 'Name of the Variable'
                !VariableType = 'Type of the Variable (Scalar,Vector,Tensor(in Voigt Notation))'
                !Length = 'Size of the Variable'
                !Variable = Result to be informed. Inform a Gauss Point result or compute a new
                !           variable.

            !------------------------------------------------------------------------------------

            ! Identity
            I = 0.0d0
            I(1,1) = 1.0d0
            I(2,2) = 1.0d0
            I(3,3) = 1.0d0

            select case (ID)

                case(0)

                    Length = 2

                case(1)

                    Name='Cauchy Stress'
                    VariableType=Tensor
                    Length=size(this%Stress)
                    Variable(1:Length) = this%Stress

                case (2)

                    Name='Almansi Strain'
                    VariableType = Tensor
                    Length=size(this%Stress)
                    !-------------------------------------------------------------
                    !Almansi Strain
                    !-------------------------------------------------------------
                    e = 0.50d0*( I - matmul(this%F, transpose(this%F) ))
                    eV = Convert_to_Voigt(e)
                    Variable(1:Length) = eV(1:Length)
                    !-------------------------------------------------------------

                case default
                    call Error("Error retrieving result :: GetResult")
            end select

		    !************************************************************************************

        end subroutine
        !==========================================================================================




    end module

