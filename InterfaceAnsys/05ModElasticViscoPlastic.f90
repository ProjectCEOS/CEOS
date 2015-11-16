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
    module ElasticViscoPlastic

    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! DECLARATIONS OF VARIABLES
    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Modules and implicit declarations
    ! --------------------------------------------------------------------------------------------
    use ConstitutiveModel
    implicit none


    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! "NameOfTheMaterialModel"Properties: Material Properties
    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type ElasticViscoPlasticProperties

    ! Variables of material parameters
    !----------------------------------------------------------------------------------------------

    ! Hyperelastic
    real(8) :: Mi(6), Ni(6), BulkModulus

    ! Viscoplastic
    real(8) :: Kfa, Kc, Keta, SigmaY0, Kh

    ! Isotropic Hardening
    real(8) :: Knh, KHiso
    integer :: FlagHardening

    ! Damage
    real(8) :: Knd, Km, KR, Kg, KS, KN, Threshold
    integer :: FlagPlasDam, FlagHidrDam

    end type
    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel": Attributes and methods of the constitutive model
    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , extends(ClassConstitutiveModel) :: ClassElasticViscoPlastic

    !Obs.: The ClassConstitutiveModel already has the variables:
    ! - stress (Voigt notation)
    ! - F (Deformation Gradient) (3x3 Tensor Components)
    ! - Jbar - Mean Dilation variable related to Simo-Taylor-Pister Variational Approach


    ! Class Attributes : Usually the internal variables
    !----------------------------------------------------------------------------------------
        real (8) :: vi_new(4) = 0.0d0 , vi_old(4) = 0.0d0
        real(8)  :: vidam_new(4) , vidam_old(4), etrial(3)
        real (8) :: Time_old
        real (8) :: Fp_old(3,3), Fp_new(3,3), dWdCiso(3,3), DEV_dWdCiso(3,3)
        real (8) :: Ea(3,3,3)
        integer :: flag_ELAST



    ! Class Attributes : Material Properties
    !----------------------------------------------------------------------------------------
    type (ElasticViscoPlasticProperties), pointer :: Properties => null()


    contains

    ! Class Methods
    !----------------------------------------------------------------------------------
    !procedure :: ConstitutiveModelConstructor => ConstitutiveModelConstructor_ElasticViscoPlastic
    !procedure :: ReadMaterialParameters       => ReadMaterialParameters_ElasticViscoPlastic
    !procedure :: GetResult                    => GetResult_ElasticViscoPlastic
    procedure :: SaveConvergedState           => SaveConvergedState_ElasticViscoPlastic
    ! procedure :: SecondDerivativesOfPSI_Jbar  => SecondDerivativesOfPSI_Jbar_ElasticViscoPlastic
    !procedure :: CopyProperties               => CopyProperties_ElasticViscoPlastic

    end type
    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel"_3D: Attributes and methods of the constitutive model
    ! in Three-Dimensional analysis.
    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , extends(ClassElasticViscoPlastic) :: ClassElasticViscoPlastic_3D

    contains
    ! Class Methods
    !----------------------------------------------------------------------------------
    procedure :: UpdateStressAndStateVariables  =>  UpdateStressAndStateVariables_ElasticViscoPlastic_3D
    procedure :: GetTangentModulus              =>  GetTangentModulus_ElasticViscoPlastic_3D

    end type
    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel"_Axisymmetric: Attributes and methods of the constitutive model
    ! in Axisymmetric analysis.
    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    !type , extends(ClassElasticViscoPlastic) :: ClassElasticViscoPlastic_Axisymmetric
    !
    !     contains
    !        ! Class Methods
    !        !----------------------------------------------------------------------------------
    !         procedure :: UpdateStressAndStateVariables  =>  UpdateStressAndStateVariables_ElasticViscoPlastic_Axisymmetric
    !         procedure :: GetTangentModulus              =>  GetTangentModulus_ElasticViscoPlastic_Axisymmetric
    !
    !end type
    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

    contains

    !==========================================================================================
    ! Method ConstitutiveModelConstructor_"NameOfTheMaterialModel": Routine that constructs the
    ! Constitutive Model
    !------------------------------------------------------------------------------------------
    ! Modifications:
    ! Date:         Author:
    !==========================================================================================
!    subroutine ConstitutiveModelConstructor_ElasticViscoPlastic(this) !,AnalysisSettings)
!
!    !************************************************************************************
!    ! DECLARATIONS OF VARIABLES
!    !************************************************************************************
!    ! Modules and implicit declarations
!    ! -----------------------------------------------------------------------------------
!    !use Analysis
!
!    ! Object
!    ! -----------------------------------------------------------------------------------
!    class(ClassElasticViscoPlastic) :: this
!
!    ! Input variables
!    ! -----------------------------------------------------------------------------------
!    !type(ClassAnalysis) :: AnalysisSettings
!
!    !************************************************************************************
!
!    !************************************************************************************
!    ! ALLOCATE THE STATE VARIABLES
!    !************************************************************************************
!
!    allocate( this%Stress( 6 ) ) ; this%Stress= 0.0d0
!
!    this%vi_new = 0.0d0
!
!    this%vi_old = 0.0d0
!
!    this%Time_old = 0.0d0
!
!    this%vidam_new = 0.0d0
!    this%vidam_new(1) = 1.0d0
!
!    this%vidam_old = 0.0d0
!    this%vidam_old(1) = 1.0d0
!
!    this%etrial = 0.0d0
!
!    this%Fp_old = 0.0d0
!    this%Fp_old(1,1)=1.0d0; this%Fp_old(2,2)=1.0d0; this%Fp_old(3,3)=1.0d0
!
!    this%Fp_new = this%Fp_old
!
!    this%dWdCiso = 0.0d0
!    this%DEV_dWdCiso = 0.0d0
!    this%Ea = 0.0d0
!    this%flag_ELAST = 1
!    !************************************************************************************
!
!    end subroutine
    !==========================================================================================


    !==========================================================================================
    ! Method ReadMaterialParameters_"NameOfTheMaterialModel": Routine that reads the material
    ! parameters
    !------------------------------------------------------------------------------------------
    ! Modifications:
    ! Date:         Author:
    !==========================================================================================
!    subroutine ReadMaterialParameters_ElasticViscoPlastic(this,DataFile)
!
!
!    !************************************************************************************
!    ! DECLARATIONS OF VARIABLES
!    !************************************************************************************
!    ! Modules and implicit declarations
!    ! ---------------------------------------------------------------------------------
!    use Parser
!
!    ! Object
!    ! ---------------------------------------------------------------------------------
!    class(ClassElasticViscoPlastic) :: this
!
!    ! Input variables
!    ! ---------------------------------------------------------------------------------
!    type(ClassParser) :: DataFile
!
!    ! Internal variables
!    ! ---------------------------------------------------------------------------------
!    character(len=100), dimension(30) :: ListOfOptions, ListOfValues
!    logical, dimension(30)            :: FoundOption
!    integer                           :: i
!    !************************************************************************************
!
!    !___________________   WARNING! DO NOT CHANGE OR ERASE THIS BLOCK    _________________
!    ! All constitutive models must allocate its own properties!
!    allocate (this%Properties)
!    !____________________________________________________________________________________
!
!    !************************************************************************************
!    ! READ THE MATERIAL PARAMETERS
!    !************************************************************************************
!
!    ! Inform how the properties are shown in the "Settings" file.
!    !------------------------------------------------------------------------------------
!    ListOfOptions(1:6)   = ["Mi_1","Mi_2","Mi_3","Mi_4","Mi_5","Mi_6"]
!    ListOfOptions(7:12)  = ["Ni_1","Ni_2","Ni_3","Ni_4","Ni_5","Ni_6"]
!    ListOfOptions(13)	 = "BulkModulus"
!    ListOfOptions(14:18) = ["Kfa", "Kh", "Kc", "Keta", "SigmaY0"]
!    ListOfOptions(19:21) = ["Knh", "KHiso" , "FlagHardening"]
!    ListOfOptions(22:25) = ["Knd", "Km" , "KR", "Kg"]
!    ListOfOptions(26:27) = ["KS", "KN"]
!    ListOfOptions(28:30) = ["Threshold", "FlagPlasDam", "FlagHidrDam"]
!    !------------------------------------------------------------------------------------
!
!    !___________________   WARNING! DO NOT CHANGE OR ERASE THIS BLOCK    _________________
!    call DataFile%FillListOfOptions(ListOfOptions,ListOfValues)
!    !____________________________________________________________________________________
!
!    ! Set the material properties: this%Properties%"NameOfTheProperty"
!    ! Obs.: ListOfValues index must match with ListOfOptions index
!    !------------------------------------------------------------------------------------
!    call DataFile%ConvertToDouble(ListOfValues(1),this%Properties% Mi(1))
!    call DataFile%ConvertToDouble(ListOfValues(2),this%Properties% Mi(2))
!    call DataFile%ConvertToDouble(ListOfValues(3),this%Properties% Mi(3))
!    call DataFile%ConvertToDouble(ListOfValues(4),this%Properties% Mi(4))
!    call DataFile%ConvertToDouble(ListOfValues(5),this%Properties% Mi(5))
!    call DataFile%ConvertToDouble(ListOfValues(6),this%Properties% Mi(6))
!    call DataFile%ConvertToDouble(ListOfValues(7),this%Properties% Ni(1))
!    call DataFile%ConvertToDouble(ListOfValues(8),this%Properties% Ni(2))
!    call DataFile%ConvertToDouble(ListOfValues(9),this%Properties% Ni(3))
!    call DataFile%ConvertToDouble(ListOfValues(10),this%Properties% Ni(4))
!    call DataFile%ConvertToDouble(ListOfValues(11),this%Properties% Ni(5))
!    call DataFile%ConvertToDouble(ListOfValues(12),this%Properties% Ni(6))
!    call DataFile%ConvertToDouble(ListOfValues(13),this%Properties% BulkModulus)
!    call DataFile%ConvertToDouble(ListOfValues(14),this%Properties% Kfa)
!    call DataFile%ConvertToDouble(ListOfValues(15),this%Properties% Kh)
!    call DataFile%ConvertToDouble(ListOfValues(16),this%Properties% Kc)
!    call DataFile%ConvertToDouble(ListOfValues(17),this%Properties% Keta)
!    call DataFile%ConvertToDouble(ListOfValues(18),this%Properties% SigmaY0)
!    call DataFile%ConvertToDouble(ListOfValues(19),this%Properties% Knh)
!    call DataFile%ConvertToDouble(ListOfValues(20),this%Properties% KHiso)
!    call DataFile%ConvertToInteger(ListOfValues(21),this%Properties% FlagHardening)
!    call DataFile%ConvertToDouble(ListOfValues(22),this%Properties% Knd)
!    call DataFile%ConvertToDouble(ListOfValues(23),this%Properties% Km )
!    call DataFile%ConvertToDouble(ListOfValues(24),this%Properties% KR)
!    call DataFile%ConvertToDouble(ListOfValues(25),this%Properties% Kg)
!    call DataFile%ConvertToDouble(ListOfValues(26),this%Properties% KS)
!    call DataFile%ConvertToDouble(ListOfValues(27),this%Properties% KN)
!    call DataFile%ConvertToDouble(ListOfValues(28),this%Properties% Threshold)
!    call DataFile%ConvertToInteger(ListOfValues(29),this%Properties% FlagPlasDam)
!    call DataFile%ConvertToInteger(ListOfValues(30),this%Properties% FlagHidrDam)
!    !------------------------------------------------------------------------------------
!
!    !************************************************************************************
!
!    end subroutine
    !==========================================================================================

    !==========================================================================================
    ! Method CopyProperties_"NameOfTheMaterialModel": Routine that associates the material
    ! parameters in the Gauss Points
    !------------------------------------------------------------------------------------------
    ! Modifications:
    ! Date:         Author:
    !==========================================================================================
!    subroutine CopyProperties_ElasticViscoPlastic(this,Reference)
!
!    !************************************************************************************
!    ! DECLARATIONS OF VARIABLES
!    !************************************************************************************
!    ! Object
!    ! ---------------------------------------------------------------------------------
!    class(ClassElasticViscoPlastic) :: this
!
!    ! Input variables
!    ! ---------------------------------------------------------------------------------
!    class(ClassConstitutiveModel) :: Reference
!
!    !************************************************************************************
!
!    ! Change field: "class is ( Class"NameOfTheMaterialModel"Q1P0 )"
!    !-----------------------------------------------------------------------------------
!    select type ( Reference )
!
!    class is ( ClassElasticViscoPlastic )
!        this%Properties => Reference%Properties
!        class default
!        stop "Error: Subroutine CopyProperties"
!    end select
!    !-----------------------------------------------------------------------------------
!
!    !************************************************************************************
!
!    end subroutine
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
    !  subroutine FirstDerivativesOfPSI ( Properties, Jbar, Ciso, dPSIiso_dCiso, dPSIvol_dJbar  )
    !
    !!************************************************************************************
    !      ! DECLARATIONS OF VARIABLES
    !!************************************************************************************
    !      ! Modules and implicit declarations
    !      ! ---------------------------------------------------------------------------------
    !      use MathRoutines
    !
    !      ! Object
    !      ! -----------------------------------------------------------------------------------
    !      type(ElasticViscoPlasticProperties) :: Properties
    !
    !      ! Input variables
    !      ! -----------------------------------------------------------------------------------
    !      real (8) :: Jbar, Ciso(3,3)
    !
    !      ! Output variables
    !      ! -----------------------------------------------------------------------------------
    !      real (8) :: dPSIiso_dCiso(3,3), dPSIvol_dJbar
    !
    !      ! Internal variables
    !      ! -----------------------------------------------------------------------------------
    !      real (8) :: C10, BulkModulus, I(3,3)
    !
    !      !************************************************************************************
    !      ! TANGENT MODULUS
    !!************************************************************************************
    !
    !      ! Optional: Retrieve Properties
    !      ! -----------------------------------------------------------------------------------
    !C10         = Properties%C10
    !BulkModulus = Properties%BulkModulus
    !      ! -----------------------------------------------------------------------------------
    !
    !      ! Identity
    !      I = 0.0d0
    !      I(1,1) = 1.0d0
    !      I(2,2) = 1.0d0
    !      I(3,3) = 1.0d0
    !
    !
    !      ! First Derivative with respect to Isochoric Right-Cauchy Green Strain
    !      !--------------------------------------------------------------------
    !      dPSIiso_dCiso = C10*I
    !      !--------------------------------------------------------------------
    !
    !
    !      ! Fist Derivative with respect to Mean Dilatation
    !      !--------------------------------------------------------------------
    !dPSIvol_dJbar = 3.0d0*BulkModulus*( Jbar**(-2.0d0/3.0d0) )*( Jbar**(1.0d0/3.0d0) - 1.0d0 )
    !      !--------------------------------------------------------------------
    !
    !!************************************************************************************
    !
    !  end subroutine
    !==========================================================================================

    !==========================================================================================
    ! Internal Subroutine:  Compute the second derivative of isochoric Strain Energy function
    !                       with respect to Isochoric Right-Cauchy Green Strain (dPSIiso_dCiso)
    !------------------------------------------------------------------------------------------
    ! Modifications:
    ! Date:         Author:
    !==========================================================================================
    !  subroutine SecondDerivativesOfPSI_Ciso ( Properties, Ciso, d2PSIiso_dCiso2  )
    !
    !!************************************************************************************
    !      ! DECLARATIONS OF VARIABLES
    !!************************************************************************************
    !      ! Modules and implicit declarations
    !      ! ---------------------------------------------------------------------------------
    !      use MathRoutines
    !
    !      ! Object
    !      ! -----------------------------------------------------------------------------------
    !      type(ElasticViscoPlasticProperties) :: Properties
    !
    !      ! Input variables
    !      ! -----------------------------------------------------------------------------------
    !      real (8) :: Ciso(6)
    !
    !      ! Output variables
    !      ! -----------------------------------------------------------------------------------
    !      real (8) :: d2PSIiso_dCiso2(6,6)
    !
    !      ! Internal variables
    !      ! -----------------------------------------------------------------------------------
    !      real (8) :: C10, BulkModulus
    !
    !      !************************************************************************************
    !      ! TANGENT MODULUS
    !!************************************************************************************
    !
    !      ! Optional: Retrieve Properties
    !      ! -----------------------------------------------------------------------------------
    !C10         = Properties%C10
    !BulkModulus = Properties%BulkModulus
    !      ! -----------------------------------------------------------------------------------
    !
    !
    !      ! Second Derivative with respect to Isochoric Right-Cauchy Green Strain
    !      !--------------------------------------------------------------------
    !      d2PSIiso_dCiso2 = 0.0d0
    !      !--------------------------------------------------------------------
    !
    !
    !!************************************************************************************
    !
    !  end subroutine
    !==========================================================================================

    !==========================================================================================
    ! Method UpdateStateVariables_"NameOfTheMaterialModel":  Compute the second derivatives of
    ! volumetric Strain Energy function with respect to Mean Dilatation
    !------------------------------------------------------------------------------------------
    ! Modifications:
    ! Date:         Author:
    !==========================================================================================
    !  subroutine SecondDerivativesOfPSI_Jbar_HyperelasticQ1P0 ( this, d2PSIvol_dJbar2   )
    !
    !!************************************************************************************
    !      ! DECLARATIONS OF VARIABLES
    !!************************************************************************************
    !      ! Modules and implicit declarations
    !      ! ---------------------------------------------------------------------------------
    !      use MathRoutines
    !
    !      ! Object
    !      ! -----------------------------------------------------------------------------------
    !      class(ClassElasticViscoPlastic) :: this
    !
    !      ! Output variables
    !      ! -----------------------------------------------------------------------------------
    !      real (8) ::  d2PSIvol_dJbar2
    !
    !      ! Internal variables
    !      ! -----------------------------------------------------------------------------------
    !      real (8) :: C10, BulkModulus
    !
    !      !************************************************************************************
    !      ! TANGENT MODULUS
    !!************************************************************************************
    !
    !      ! Optional: Retrieve Properties
    !      ! -----------------------------------------------------------------------------------
    !C10         = this%Properties%C10
    !BulkModulus = this%Properties%BulkModulus
    !      ! -----------------------------------------------------------------------------------
    !
    !
    !      ! Second Derivative with respect to Mean Dilatation
    !      !--------------------------------------------------------------------
    !d2PSIvol_dJbar2 = ( -this%Properties%BulkModulus*this%Jbar**(-5.0d0/3.0d0) ) * &
    !                        ( this%Jbar**(1.0d0/3.0d0) - 2.0d0  )
    !
    !      !--------------------------------------------------------------------
    !
    !!************************************************************************************
    !
    !  end subroutine
    !==========================================================================================

    !==========================================================================================
    subroutine Ogden(P , eps , We , dWe , d2We )

    type(ElasticViscoPlasticProperties) :: P
    real(8) :: eps(3) , We , dWe(3) , d2We(3)

    integer :: i,j

    We=0.0d0
    do j=1,3
        do i=1,6
            if ( P%Ni(i) .ne. 0 ) then
                We= We + (P%Mi(i) / P%Ni(i)) * ( dexp(P%Ni(i)*eps(j)) - 1.0d0)
            endif
        enddo
    enddo
    dWe=0.0d0
    d2We=0.0d0
    do j=1,3
        do i=1,6
            dWe(j) = dWe(j) + P%Mi(i)* dexp(P%Ni(i)*eps(j))
            d2We(j)  = d2We(j) + P%Mi(i) * P%Ni(i) * dexp(P%Ni(i)*eps(j))
        enddo
    enddo

    end subroutine
    !==========================================================================================

    !==========================================================================================
    subroutine KappaFunction(P , alpha , Kappa, dKappa, EnergyP)

    type(ElasticViscoPlasticProperties) :: P
    real(8) :: alpha, Kappa, dKappa, EnergyP

    select case (P%FlagHardening)
    case (1)
        Kappa   = P%KHiso*alpha
        dKappa  = P%KHiso
        EnergyP = 0.5d0*P%KHiso*(alpha**2.0d0)
    case (2)
        Kappa = P%KHiso*( 1.0d0 - dexp(-P%Knh*alpha) )
        dKappa = P%KHiso*P%Knh*dexp(-P%Knh*alpha)
        EnergyP = (P%KHiso/P%Knh)*(P%Knh*alpha+dexp(-P%Knh*alpha)-P%KHiso)
        case default
        stop "FlagHardening não definido corretamente"
    end select

    end subroutine
    !==========================================================================================
    !==========================================================================================
    subroutine ViscoArrasto(P, alpha, VA )

    type(ElasticViscoPlasticProperties) :: P
    real(8) :: alpha, VA

    VA = P%Kfa + P%Kh*alpha

    end subroutine
    !==========================================================================================

    !==========================================================================================
    subroutine Return_Map(P , etr, vi_old, dWe, delta_t, Kappa, vi_new )
    use MathRoutines
    type(ElasticViscoPlasticProperties) :: P
    real(8) :: alpha_n, kappa, We, fA, A, dAda, dkda, delta_t, delta_alpha, erro, TOL, norma
    real(8) :: etr(3), eps(3), vi(4), dWde(3), d2W(3), q(3), KT(4,4), dWtr(3,3), devdWtr(3,3)
    real(8) :: dWe(3), vi_old(4), vi_trial(4), VFun(4), vi_new(4), DELTA(4)
    real(8) :: eigenvectors(3,3), eigenvalues(3), I(3,3), M(3,3)
    real(8) :: TEMP1(3,3), TEMP2(3)
    real(8) :: work(10)
    integer :: info, cont

    ! Identity
    I = 0.0d0
    I(1,1) = 1.0d0
    I(2,2) = 1.0d0
    I(3,3) = 1.0d0

    dWtr = 0.0d0
    dWtr(1,1) = dWe(1)
    dWtr(2,2) = dWe(2)
    dWtr(3,3) = dWe(3)

    devdWtr = dWtr - (1.0d0/3.0d0)*sum(dWe)*I

    norma = dsqrt(devdWtr(1,1)**2.0d0+devdWtr(2,2)**2.0d0+devdWtr(3,3)**2.0d0)

    M = dsqrt(3.0d0/2.0d0)*devdWtr/norma

    eigenvectors = M

    call dsyev("V", "U", 3, eigenvectors, 3, eigenvalues, work, 10, info)

    TEMP1=eigenvectors
    TEMP2=eigenvalues
    eigenvectors(1:3,1)= TEMP1(1:3,3)
    eigenvectors(1:3,2)= TEMP1(1:3,2)
    eigenvectors(1:3,3)= TEMP1(1:3,1)
    eigenvalues(1:3) = [TEMP2(3), TEMP2(2), TEMP2(1)]

    alpha_n = vi_old(4)

    vi_trial(1:3) = eigenvalues

    delta_alpha = 1e-12

    vi_trial(4) = vi_old(4) + delta_alpha

    vi_new = vi_trial

    eps = etr - delta_alpha*vi_trial(1:3)

    call Function_F(P, eps, vi_trial, alpha_n, delta_t, kappa, VFun )

    erro = 1.0d0

    TOL = 1.0e-6

    !DELTA(1:3) = vi_new(1:3)
    !DELTA(4) = delta_alpha
    cont=1

    do while (erro > TOL)

    DELTA=0.0d0

    call Kt_Newton(P , eps, vi_new, alpha_n, delta_t, kappa, KT )

    ! DELTA = -(KT)^-1 * VFun
    call Solve_Linear_System(KT,DELTA,-VFun)

    if (delta_alpha + DELTA(4) .lt. 0.0d0) then
        write(*,'(12x,a,i3,a,e16.9)') 'N-R local: ',cont ,'  ERRO: ', delta_alpha + DELTA(4)
        !call Error('Newton Local1')
        stop
    endif

    vi_new(1:3) = vi_new(1:3) + DELTA(1:3)

    delta_alpha = delta_alpha + DELTA(4)

    vi_new(4) = alpha_n + delta_alpha

    eps = etr - delta_alpha*vi_new(1:3)

    call Function_F(P, eps, vi_new, alpha_n, delta_t, kappa, VFun )

    erro = norm(VFun)

    !write(*,'(12x,a,i3,a,e16.9)') 'N-R local: ',cont ,'  ERRO: ',erro

    cont = cont + 1

    if ((cont .gt. 20) .or. (delta_alpha .lt. 0.0d0)) then
        write(*,'(12x,a,i3,a,e16.9)') 'N-R local: ',cont ,'  ERRO: ', erro
        stop
        !call Error('Newton Local2')
    endif

    enddo

    end subroutine
    !==========================================================================================

    !==========================================================================================
    subroutine Function_F(P, eps, vi, alpha_n, delta_t, kappa, F )

    type(ElasticViscoPlasticProperties) :: P
    real(8) :: F(4), vi(4), dWde(3), d2We(3), eps(3)
    real(8) :: alpha_n, alpha_n1, kappa, fA, delta_t, delta_alpha, We, dKappa, A
    real(8) :: Wp

    alpha_n1 = vi(4)
    delta_alpha = alpha_n1 - alpha_n

    call Ogden(P, eps, We, dWde, d2We )
    call ViscoArrasto(P , alpha_n1, fA )
    call KappaFunction(P , alpha_n1, Kappa, dKappa, Wp)

    A = kappa + P%SigmaY0 + fA * ((delta_alpha/delta_t) / P%Kc) ** P%Keta

    F(1) = -2.0d0*dWde(1) +   dWde(2)   +   dWde(3)   + 2.0d0*A*vi(1)
    F(2) =    dWde(1) - 2.0d0*dWde(2)   +   dWde(3)   + 2.0d0*A*vi(2)
    F(3) =    dWde(1) +   dWde(2)   - 2.0d0*dWde(3)   + 2.0d0*A*vi(3)
    F(4) = -dWde(1)*vi(1) -   dWde(2)*vi(2)  -  dWde(3)*vi(3) + A

    end subroutine
    !==========================================================================================

    !==========================================================================================
    subroutine Kt_Newton(P , eps, vi, alpha_n, delta_t, kappa, KT )

    type(ElasticViscoPlasticProperties) :: P
    real(8) :: alpha_n, kappa, We, fA, A, dAda, dkda, delta_t
    real(8) :: eps(3), vi(4), dWde(3), d2W(3), q(3), KT(4,4), dedq(3), deda(3)
    real(8) :: alpha_n1, delta_alpha
    real(8) :: Wp

    alpha_n1 = vi(4)

    call Ogden(P, eps, We, dWde, d2W )
    call ViscoArrasto(P , alpha_n1, fA )
    call KappaFunction(P , alpha_n1, kappa, dkda, Wp)

    delta_alpha = vi(4) - alpha_n

    A = kappa + P%SigmaY0 + fA * ((delta_alpha/delta_t) / P%Kc) ** P%Keta

    !dAda = dkda + P%Kh*((delta_alpha/delta_t)/P%Kc)**P%Keta + fA * P%Keta* (((1/delta_t)/P%Kc) ** P%Keta)*delta_t ** (P%Keta-1)
    dAda = dkda + P%Kh*((delta_alpha/delta_t)/P%Kc)**P%Keta + fA * P%Keta* (((1/delta_t)/P%Kc) ** P%Keta)*delta_alpha** (P%Keta-1)

    q(1)=vi(1)
    q(2)=vi(2)
    q(3)=vi(3)

    dedq(1)=-delta_alpha
    dedq(2)=-delta_alpha
    dedq(3)=-delta_alpha
    deda(1)=-q(1)
    deda(2)=-q(2)
    deda(3)=-q(3)

    KT(1,1)=-2.0d0*d2W(1)*dedq(1)+2.0d0*A
    KT(1,2)=d2W(2)*dedq(2)
    KT(1,3)=d2W(3)*dedq(3)
    KT(1,4)=-2.0d0*d2W(1)*deda(1)+d2W(2)*deda(2)+d2W(3)*deda(3)+2.0d0*dAda*q(1)

    KT(2,1)=d2W(1)*dedq(1)
    KT(2,2)=-2.0d0*d2W(2)*dedq(2)+2.0d0*A
    KT(2,3)=d2W(3)*dedq(3);
    KT(2,4)=d2W(1)*deda(1)-2.0d0*d2W(2)*deda(2)+d2W(3)*deda(3)+2.0d0*dAda*q(2)

    KT(3,1)=d2W(1)*dedq(1)
    KT(3,2)=d2W(2)*dedq(2)
    KT(3,3)=-2.0d0*d2W(3)*dedq(3)+2.0d0*A
    KT(3,4)=d2W(1)*deda(1)+d2W(2)*deda(2)-2.0d0*d2W(3)*deda(3)+2.0d0*dAda*q(3)

    KT(4,1)=-(d2W(1)*dedq(1)*q(1)+ dWde(1))
    KT(4,2)=-(d2W(2)*dedq(2)*q(2)+ dWde(2))
    KT(4,3)=-(d2W(3)*dedq(3)*q(3)+ dWde(3))
    KT(4,4)=-(d2W(1)*deda(1)*q(1)+d2W(2)*deda(2)*q(2)+d2W(3)*deda(3)*q(3))+dAda

    end subroutine

    !==========================================================================================
    ! Method UpdateStateVariables_"NameOfTheMaterialModel"_3D: Routine that
    ! contains the algorithm employed to update the state variables.
    !------------------------------------------------------------------------------------------
    ! Modifications:
    ! Date:         Author:
    !==========================================================================================
    subroutine UpdateStressAndStateVariables_ElasticViscoPlastic_3D(this)

    !************************************************************************************
    ! DECLARATIONS OF VARIABLES
    !************************************************************************************
    ! Modules and implicit declarations
    ! ---------------------------------------------------------------------------------
    use MathRoutines

    ! Object
    ! ---------------------------------------------------------------------------------
    class(ClassElasticViscoPlastic_3D) :: this

    ! Internal variables
    ! -----------------------------------------------------------------------------------
    real(8) :: F_new(3,3), F_old(3,3), Fp_new(3,3), Fp_old(3,3),  Fp_old_inv(3,3)

    real(8) :: C_new(3,3), Fiso_new(3,3), Ciso_new(3,3) ,Ciso_new_tr(3,3)

    real(8) :: S_new(3,3), dWdCtr(3,3), dWdC(3,3), dev_dWdC(3,3), M(3,3)

    real(8) :: dWe(3) , d2We(3), dev_dWe(3), vi_new(4), vi_old(4), eps(3), etr(3)

    real(8) :: J, Time_new, Time_old, We, Wp, T_eq_tr, Kappa, dKappa, SigmaY0, BulkModulus

    real(8) :: f_tr, dUdJ, alpha_n, k, delta_t, delta_alpha

    real(8) :: eigenvectors(3,3), eigenvalues(3), I(3,3), eigvect_Ciso(3,3),  expM(3,3)
    real(8) :: TEMP1(3,3), TEMP2(3)
    real(8) :: work(10)
    integer :: info
    real(8) :: Knd, Km, KR, Kg, KS, KN
    real(8) :: Yn1, Dn1, Dn, dpn1, dpn, dhn1, dhn, wn1, wn, Ddh, Ddp

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
    F_new = this%F

    !F_new(1,1:3) = [1.08591409d0,	0.00000000d0,	0.00000000d0]
    !F_new(2,1:3) = [0.00166667d0,	0.99955341d0,	0.00166667d0]
    !F_new(3,1:3) = [0.00000000d0,   0.00000000d0,	1.00000000d0]
    !    this%F=F_new


    !F_new(1,1:3) = [1.17182818d0,	       0.0d0,	0.0d0]
    !F_new(2,1:3) = [0.01298092d0,	0.95770525d0,	0.01323470d0]
    !F_new(3,1:3) = [0.00184840d0,  -0.00223651d0,	0.96733484d0]
    !

    !F_new(1,1:3) = [1.17182818d0,	       0.0d0,	       0.0d0]
    !F_new(2,1:3) = [       0.0d0,	0.93438811d0,	       0.0d0]
    !F_new(3,1:3) = [       0.0d0,          0.0d0,	0.93438811d0]
    !

    !F_new(1,1:3) = [1.68731273d0, 0.0d0, 0.0d0]
    !F_new(2,1:3) = [0.0d0, 0.81200717d0, 0.0d0]
    !F_new(3,1:3) = [0.0d0, 0.0d0, 0.81200717d0]

    !vi_new = this%vi_new
    vi_old = this%vi_old
    Dn = 1.0d0 - this%vidam_old(1)
    dpn = this%vidam_old(2)
    dhn = this%vidam_old(3)

    !vi_old = [1.0d0,  -0.5d0,  -0.5d0,   0.04288011d0]
    Time_old = this%Time_old
    Fp_old = this%Fp_old

    !Fp_old(1,1:3) = [1.04381274d0, 0.0d0, 0.0d0]
    !Fp_old(2,1:3) = [0.0d0, 0.97878814d0, 0.0d0]
    !Fp_old(3,1:3) = [0.0d0, 0.0d0, 0.97878814d0]

    Time_new = this%Time

    delta_t = Time_new - Time_old

    SigmaY0 = this%Properties%SigmaY0
    BulkModulus = this%Properties%BulkModulus

    Knd = this%Properties%Knd
    Km  = this%Properties%Km
    KR  = this%Properties%KR
    Kg  = this%Properties%Kg

    KS =  this%Properties%KS
    KN =  this%Properties%KN

    ! -----------------------------------------------------------------------------------

    ! Identity
    I = 0.0d0
    I(1,1) = 1.0d0
    I(2,2) = 1.0d0
    I(3,3) = 1.0d0

    ! -----------------------------------------------------------------------------------
    J = det(F_new)

    C_new = matmul(transpose(F_new),F_new)

    Fiso_new = (J**(-1.0d0/3.0d0))*F_new

    Ciso_new = matmul(transpose(Fiso_new),Fiso_new)

    Fp_old_inv = inverse(Fp_old)

    Ciso_new_tr = matmul( transpose(Fp_old_inv), matmul(Ciso_new,Fp_old_inv) )

    eigenvectors = Ciso_new_tr

    ! V compute eigenvalues and eigenvectors. N eigenvalues only
    ! U upper triangle of A
    ! 3 The order of the matrix
    ! eigenvectors

    call dsyev("V", "U", 3, eigenvectors, 3, eigenvalues, work, 10, info)
    TEMP1=eigenvectors
    TEMP2=eigenvalues
    eigenvectors(1:3,1)= TEMP1(1:3,3)
    eigenvectors(1:3,2)= TEMP1(1:3,2)
    eigenvectors(1:3,3)= TEMP1(1:3,1)
    eigenvalues(1:3) = [TEMP2(3), TEMP2(2), TEMP2(1)]

    do k=1,3
        this%Ea(:,:,k) = Tensor_Product(eigenvectors(:,k),eigenvectors(:,k))
    enddo

    etr = 0.5d0*dlog(eigenvalues)
    ! -----------------------------------------------------------------------------------

    call Ogden(this%Properties , etr , We , dWe , d2We )

    dev_dWe = dWe - (1.0d0/3.0d0)*sum(dWe)

    T_eq_tr = dsqrt(3.0d0/2.0d0)*norm(dev_dWe)

    call KappaFunction(this%Properties , vi_old(4) , Kappa, dKappa, Wp)

    f_tr = T_eq_tr - (SigmaY0 + Kappa)

    ! -----------------------------------------------------------------------------------
    if (f_tr .le. 0.0d0 ) then

    this%flag_ELAST = 1

    Yn1 = We + 0.5d0*BulkModulus*(dlog(J))**2.0d0 + Wp

    ! Dano Hidrolitico (ocorre tanto no passo elastico quanto no
    ! plastico
    Ddh =( ((1.0d0-Dn)**Knd) * ((Yn1+Kg)**Km)/kR )*delta_t

    if (this%Properties%FlagHidrDam .eq. 0) then
        Ddh=0.0d0
    endif

    dhn1=dhn+Ddh
    dpn1=dpn
    ! Atualizacao das variaveis caso elastico
    Dn1=Dn+Ddh
    wn1=(1.0d0-Dn1)

    dWdCtr = 0.0d0
    do k = 1,3
        dWdCtr = dWdCtr + (0.5d0*dWe(k)/eigenvalues(k)) * Tensor_Product(eigenvectors(:,k),eigenvectors(:,k))
    enddo

    dWdC = matmul( Fp_old_inv, matmul( dWdCtr, transpose(Fp_old_inv) ) )

    dev_dWdC = dWdC - (1.0d0/3.0d0)*Tensor_Inner_Product(dWdC,C_new)*inverse(C_new)

    dUdJ = BulkModulus*dlog(J)/J

    S_new = wn1 * (2.0d0*(J**(-2.0d0/3.0d0))*dev_dWdC + J*dUdJ*inverse(C_new))

    ! Modified Cauchy Stress - Calculated in 3D Tensorial Format and converted to Voigt
    ! notation.
    ! -----------------------------------------------------------------------------------
    S_new = matmul(matmul(F_new,S_new),transpose(F_new))/J

    this%Stress = Convert_to_Voigt(S_new)

    ! -----------------------------------------------------------------------------------
    !this%Fp_new = Fp_old
    this%Fp_new=Fp_old
    this%vi_new=vi_old
    this%vidam_new = [wn1, dpn1, dhn1, Yn1]
    this%dWdCiso = dWdC
    this%DEV_dWdCiso = DEV_dWdC
    !this%Time_old=Time_old

    else

    this%flag_ELAST = 0

    call Return_Map(this%Properties , etr, vi_old, dWe, delta_t, Kappa, vi_new )

    delta_alpha = vi_new(4) - vi_old(4)

    eps = etr - delta_alpha * vi_new(1:3)

    M = 0.0d0
    expM = 0.0d0
    do k=1,3
        M = M + vi_new(k)*Tensor_Product(eigenvectors(:,k),eigenvectors(:,k))
        expM = expM + dexp(delta_alpha*vi_new(k))*Tensor_Product(eigenvectors(:,k),eigenvectors(:,k))
    enddo

    Fp_new=matmul( expM, Fp_old)

    call Ogden(this%Properties , eps , We , dWe , d2We)

    call KappaFunction(this%Properties , vi_new(4) , Kappa, dKappa, Wp) ! 20150928

    Yn1 = We + 0.5d0*BulkModulus*(dlog(J))**2.0d0 + Wp ! 20150928

    Ddh =( ((1.0d0-Dn)**Knd) * ((Yn1+Kg)**Km)/kR )*delta_t  ! 20150928

    if (this%Properties%FlagHidrDam .eq. 0) then
        Ddh=0.0d0  ! 20150928
    endif

    dhn1=dhn+Ddh ! 20150928

    Ddp=(delta_alpha)*(Yn1**KS)/KN ! 20150928

    if (this%Properties%FlagPlasDam .eq. 0) then
        Ddp=0.0d0 ! 20150928
    endif

    dpn1=dpn+Ddp ! 20150928

    Dn1=Dn+(Ddp+Ddh) ! 20150928

    ! Atualizacao das variaveis caso elastico

    if (vi_new(4) .lt. this%Properties%THRESHOLD) then ! 20150928
        wn1=1
    else
        wn1=(1.0d0-Dn1) ! 20150928
    endif

    dWdCtr = 0.0d0
    do k = 1,3
        dWdCtr = dWdCtr + (0.5d0*dWe(k)/eigenvalues(k)) * Tensor_Product(eigenvectors(:,k),eigenvectors(:,k))
    enddo

    dWdC = matmul( Fp_old_inv, matmul( dWdCtr, transpose(Fp_old_inv) ) )

    dev_dWdC = dWdC - (1.0d0/3.0d0)*Tensor_Inner_Product(dWdC,C_new)*inverse(C_new)

    dUdJ = BulkModulus*dlog(J)/J

    S_new = wn1 * (2.0d0*(J**(-2.0d0/3.0d0))*dev_dWdC + J*dUdJ*inverse(C_new)) ! 20150928

    ! Modified Cauchy Stress - Calculated in 3D Tensorial Format and converted to Voigt
    ! notation.
    ! -----------------------------------------------------------------------------------
    S_new = matmul(matmul(F_new,S_new),transpose(F_new))/J

    this%Stress = Convert_to_Voigt(S_new)
    ! -----------------------------------------------------------------------------------
    this%Fp_new=Fp_new
    this%vi_new=vi_new
    this%vidam_new = [wn1, dpn1, dhn1, Yn1] ! 20150928
    this%dWdCiso = dWdC
    this%DEV_dWdCiso = DEV_dWdC
    !this%Time_old=Time_new

    endif

    this%etrial = etr

    !************************************************************************************

    end subroutine
    !==========================================================================================

    subroutine TanModDF_ElasticViscoPlastic_3D(this, C_new, Ciso_new, J, S_new)

!************************************************************************************
    ! DECLARATIONS OF VARIABLES
    !************************************************************************************
    ! Modules and implicit declarations
    ! ---------------------------------------------------------------------------------
    use MathRoutines

    ! Object
    ! ---------------------------------------------------------------------------------
    class(ClassElasticViscoPlastic_3D) :: this

    ! Internal variables
    ! -----------------------------------------------------------------------------------
    real(8) :: F_new(3,3), F_old(3,3), Fp_new(3,3), Fp_old(3,3),  Fp_old_inv(3,3)

    real(8) :: C_new(3,3), Fiso_new(3,3), Ciso_new(3,3) ,Ciso_new_tr(3,3)

    real(8) :: S_new(3,3), dWdCtr(3,3), dWdC(3,3), dev_dWdC(3,3), M(3,3)

    real(8) :: dWe(3) , d2We(3), dev_dWe(3), vi_new(4), vi_old(4), eps(3), etr(3)

    real(8) :: J, Time_new, Time_old, We, Wp, T_eq_tr, Kappa, dKappa, SigmaY0, BulkModulus

    real(8) :: f_tr, dUdJ, alpha_n, k, delta_t, delta_alpha

    real(8) :: eigenvectors(3,3), eigenvalues(3), I(3,3), eigvect_Ciso(3,3),  expM(3,3)
    real(8) :: TEMP1(3,3), TEMP2(3)
    real(8) :: work(10)
    integer :: info
    real(8) :: Knd, Km, KR, Kg, KS, KN
    real(8) :: Yn1, Dn1, Dn, dpn1, dpn, dhn1, dhn, wn1, wn, Ddh, Ddp

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
    F_new = this%F

    !F_new(1,1:3) = [1.08591409d0,	0.00000000d0,	0.00000000d0]
    !F_new(2,1:3) = [0.00166667d0,	0.99955341d0,	0.00166667d0]
    !F_new(3,1:3) = [0.00000000d0,   0.00000000d0,	1.00000000d0]
    !    this%F=F_new


    !F_new(1,1:3) = [1.17182818d0,	       0.0d0,	0.0d0]
    !F_new(2,1:3) = [0.01298092d0,	0.95770525d0,	0.01323470d0]
    !F_new(3,1:3) = [0.00184840d0,  -0.00223651d0,	0.96733484d0]
    !

    !F_new(1,1:3) = [1.17182818d0,	       0.0d0,	       0.0d0]
    !F_new(2,1:3) = [       0.0d0,	0.93438811d0,	       0.0d0]
    !F_new(3,1:3) = [       0.0d0,          0.0d0,	0.93438811d0]
    !

    !F_new(1,1:3) = [1.68731273d0, 0.0d0, 0.0d0]
    !F_new(2,1:3) = [0.0d0, 0.81200717d0, 0.0d0]
    !F_new(3,1:3) = [0.0d0, 0.0d0, 0.81200717d0]

    !vi_new = this%vi_new
    vi_old = this%vi_old
    Dn = 1.0d0 - this%vidam_old(1)
    dpn = this%vidam_old(2)
    dhn = this%vidam_old(3)

    !vi_old = [1.0d0,  -0.5d0,  -0.5d0,   0.04288011d0]
    Time_old = this%Time_old
    Fp_old = this%Fp_old

    !Fp_old(1,1:3) = [1.04381274d0, 0.0d0, 0.0d0]
    !Fp_old(2,1:3) = [0.0d0, 0.97878814d0, 0.0d0]
    !Fp_old(3,1:3) = [0.0d0, 0.0d0, 0.97878814d0]

    Time_new = this%Time

    delta_t = Time_new - Time_old

    SigmaY0 = this%Properties%SigmaY0
    BulkModulus = this%Properties%BulkModulus

    Knd = this%Properties%Knd
    Km  = this%Properties%Km
    KR  = this%Properties%KR
    Kg  = this%Properties%Kg

    KS =  this%Properties%KS
    KN =  this%Properties%KN

    ! -----------------------------------------------------------------------------------

    ! Identity
    I = 0.0d0
    I(1,1) = 1.0d0
    I(2,2) = 1.0d0
    I(3,3) = 1.0d0

    ! -----------------------------------------------------------------------------------
    !J = det(F_new)
    !
    !C_new = matmul(transpose(F_new),F_new)
    !
    !Fiso_new = (J**(-1.0d0/3.0d0))*F_new
    !
    !Ciso_new = matmul(transpose(Fiso_new),Fiso_new)

    Fp_old_inv = inverse(Fp_old)

    Ciso_new_tr = matmul( transpose(Fp_old_inv), matmul(Ciso_new,Fp_old_inv) )

    eigenvectors = Ciso_new_tr

    ! V compute eigenvalues and eigenvectors. N eigenvalues only
    ! U upper triangle of A
    ! 3 The order of the matrix
    ! eigenvectors

    call dsyev("V", "U", 3, eigenvectors, 3, eigenvalues, work, 10, info)
    TEMP1=eigenvectors
    TEMP2=eigenvalues
    eigenvectors(1:3,1)= TEMP1(1:3,3)
    eigenvectors(1:3,2)= TEMP1(1:3,2)
    eigenvectors(1:3,3)= TEMP1(1:3,1)
    eigenvalues(1:3) = [TEMP2(3), TEMP2(2), TEMP2(1)]

    do k=1,3
        this%Ea(:,:,k) = Tensor_Product(eigenvectors(:,k),eigenvectors(:,k))
    enddo

    etr = 0.5d0*dlog(eigenvalues)
    ! -----------------------------------------------------------------------------------

    call Ogden(this%Properties , etr , We , dWe , d2We )

    dev_dWe = dWe - (1.0d0/3.0d0)*sum(dWe)

    T_eq_tr = dsqrt(3.0d0/2.0d0)*norm(dev_dWe)

    call KappaFunction(this%Properties , vi_old(4) , Kappa, dKappa, Wp)

    f_tr = T_eq_tr - (SigmaY0 + Kappa)

    ! -----------------------------------------------------------------------------------
    if (f_tr .le. 0.0d0 ) then

    Yn1 = We + 0.5d0*BulkModulus*(dlog(J))**2.0d0 + Wp

    ! Dano Hidrolitico (ocorre tanto no passo elastico quanto no
    ! plastico
    Ddh =( ((1.0d0-Dn)**Knd) * ((Yn1+Kg)**Km)/kR )*delta_t

    if (this%Properties%FlagHidrDam .eq. 0) then
        Ddh=0.0d0
    endif

    dhn1=dhn+Ddh
    dpn1=dpn
    ! Atualizacao das variaveis caso elastico
    Dn1=Dn+Ddh
    wn1=(1.0d0-Dn1)

    dWdCtr = 0.0d0
    do k = 1,3
        dWdCtr = dWdCtr + (0.5d0*dWe(k)/eigenvalues(k)) * Tensor_Product(eigenvectors(:,k),eigenvectors(:,k))
    enddo

    dWdC = matmul( Fp_old_inv, matmul( dWdCtr, transpose(Fp_old_inv) ) )

    dev_dWdC = dWdC - (1.0d0/3.0d0)*Tensor_Inner_Product(dWdC,C_new)*inverse(C_new)

    dUdJ = BulkModulus*dlog(J)/J

    S_new = wn1 * (2.0d0*(J**(-2.0d0/3.0d0))*dev_dWdC + J*dUdJ*inverse(C_new))

    ! Modified Cauchy Stress - Calculated in 3D Tensorial Format and converted to Voigt
    ! notation.
    ! -----------------------------------------------------------------------------------
    !S_new = matmul(matmul(F_new,S_new),transpose(F_new))/J

    ! -----------------------------------------------------------------------------------
    !this%Fp_new = Fp_old

    !this%Time_old=Time_old
    else

    call Return_Map(this%Properties , etr, vi_old, dWe, delta_t, Kappa, vi_new )

    delta_alpha = vi_new(4) - vi_old(4)

    eps = etr - delta_alpha * vi_new(1:3)

    M = 0.0d0
    expM = 0.0d0
    do k=1,3
        M = M + vi_new(k)*Tensor_Product(eigenvectors(:,k),eigenvectors(:,k))
        expM = expM + dexp(delta_alpha*vi_new(k))*Tensor_Product(eigenvectors(:,k),eigenvectors(:,k))
    enddo

    Fp_new=matmul( expM, Fp_old)

    call Ogden(this%Properties , eps , We , dWe , d2We)

    call KappaFunction(this%Properties , vi_new(4) , Kappa, dKappa, Wp) ! 20150928

    Yn1 = We + 0.5d0*BulkModulus*(dlog(J))**2.0d0 + Wp ! 20150928

    Ddh =( ((1.0d0-Dn)**Knd) * ((Yn1+Kg)**Km)/kR )*delta_t  ! 20150928

    if (this%Properties%FlagHidrDam .eq. 0) then
        Ddh=0.0d0  ! 20150928
    endif

    dhn1=dhn+Ddh ! 20150928

    Ddp=(delta_alpha)*(Yn1**KS)/KN ! 20150928

    if (this%Properties%FlagPlasDam .eq. 0) then
        Ddp=0.0d0 ! 20150928
    endif

    dpn1=dpn+Ddp ! 20150928

    Dn1=Dn+(Ddp+Ddh) ! 20150928

    ! Atualizacao das variaveis caso elastico

    if (vi_new(4) .lt. this%Properties%THRESHOLD) then ! 20150928
        wn1=1
    else
        wn1=(1.0d0-Dn1) ! 20150928
    endif

    dWdCtr = 0.0d0
    do k = 1,3
        dWdCtr = dWdCtr + (0.5d0*dWe(k)/eigenvalues(k)) * Tensor_Product(eigenvectors(:,k),eigenvectors(:,k))
    enddo

    dWdC = matmul( Fp_old_inv, matmul( dWdCtr, transpose(Fp_old_inv) ) )

    dev_dWdC = dWdC - (1.0d0/3.0d0)*Tensor_Inner_Product(dWdC,C_new)*inverse(C_new)

    dUdJ = BulkModulus*dlog(J)/J

    S_new = wn1 * (2.0d0*(J**(-2.0d0/3.0d0))*dev_dWdC + J*dUdJ*inverse(C_new)) ! 20150928

        ! Modified Cauchy Stress - Calculated in 3D Tensorial Format and converted to Voigt
        ! notation.
        ! -----------------------------------------------------------------------------------
        !S_new = matmul(matmul(F_new,S_new),transpose(F_new))/J

        ! -----------------------------------------------------------------------------------


    endif

    !************************************************************************************

    end subroutine

    !!************************************************************************************
    !!  MODULO TANGENTE
    !!************************************************************************************


    function dX2dX (A) result(T)

    real(8) :: Id(3,3),  A(3,3)
    real(8), dimension(6,6) :: T
    real(8), dimension(3,3,3,3) :: C4
    integer :: i, j, k, l

    C4=0.0d0
    T=0.0d0

    Id = 0.0d0
    Id(1,1) = 1.0d0
    Id(2,2) = 1.0d0
    Id(3,3) = 1.0d0

    do i=1,3
        do j=1,3
            do k=1,3
                do l=1,3

                C4(i,j,k,l) = C4(i,j,k,l) &
                + 0.5d0*( Id(i,k)*A(l,j)+ Id(i,l)*A(k,j)+Id(j,l)*A(i,k)+Id(k,j)*A(i,l))

                enddo
            enddo
        enddo
    enddo

    T(1,1:6)=[ C4(1,1,1,1),C4(1,1,2,2),C4(1,1,3,3),C4(1,1,1,2),C4(1,1,2,3),C4(1,1,1,3)]
    T(2,1:6)=[ C4(2,2,1,1),C4(2,2,2,2),C4(2,2,3,3),C4(2,2,1,2),C4(2,2,2,3),C4(2,2,1,3)]
    T(3,1:6)=[ C4(3,3,1,1),C4(3,3,2,2),C4(3,3,3,3),C4(3,3,1,2),C4(3,3,2,3),C4(3,3,1,3)]
    T(4,1:6)=[ C4(1,2,1,1),C4(1,2,2,2),C4(1,2,3,3),C4(1,2,1,2),C4(1,2,2,3),C4(1,2,1,3)]
    T(5,1:6)=[ C4(2,3,1,1),C4(2,3,2,2),C4(2,3,3,3),C4(2,3,1,2),C4(2,3,2,3),C4(2,3,1,3)]
    T(6,1:6)=[ C4(1,3,1,1),C4(1,3,2,2),C4(1,3,3,3),C4(1,3,1,2),C4(1,3,2,3),C4(1,3,1,3)]

    end function

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    function OperadW2dCtr( W,fpn ) result (T)

    implicit none
    real(8),dimension(:,:)                 :: W
    real(8),dimension(3,3)                 :: F, Ft, fpn
    real(8),dimension(size(W,1),size(W,2)) :: T, A, AW

    Ft = fpn

    A(1,1) = Ft(1,1) ** 2.0d0
    A(1,2) = Ft(2,1) ** 2.0d0
    A(1,3) = Ft(3,1) ** 2.0d0
    A(1,4) = 2.0d0 * Ft(1,1) * Ft(2,1)
    A(1,5) = 2.0d0 * Ft(2,1) * Ft(3,1)
    A(1,6) = 2.0d0 * Ft(1,1) * Ft(3,1)
    A(2,1) = Ft(1,2) ** 2.0d0
    A(2,2) = Ft(2,2) ** 2.0d0
    A(2,3) = Ft(3,2) ** 2.0d0
    A(2,4) = 2.0d0 * Ft(1,2) * Ft(2,2)
    A(2,5) = 2.0d0 * Ft(2,2) * Ft(3,2)
    A(2,6) = 2.0d0 * Ft(1,2) * Ft(3,2)
    A(3,1) = Ft(1,3) ** 2.0d0
    A(3,2) = Ft(2,3) ** 2.0d0
    A(3,3) = Ft(3,3) ** 2.0d0
    A(3,4) = 2.0d0 * Ft(1,3) * Ft(2,3)
    A(3,5) = 2.0d0 * Ft(2,3) * Ft(3,3)
    A(3,6) = 2.0d0 * Ft(1,3) * Ft(3,3)
    A(4,1) = Ft(1,1) * Ft(1,2)
    A(4,2) = Ft(2,1) * Ft(2,2)
    A(4,3) = Ft(3,1) * Ft(3,2)
    A(4,4) = Ft(1,1) * Ft(2,2) + Ft(2,1) * Ft(1,2)
    A(4,5) = Ft(2,1) * Ft(3,2) + Ft(3,1) * Ft(2,2)
    A(4,6) = Ft(3,1) * Ft(1,2) + Ft(1,1) * Ft(3,2)
    A(5,1) = Ft(1,2) * Ft(1,3)
    A(5,2) = Ft(2,2) * Ft(2,3)
    A(5,3) = Ft(3,2) * Ft(3,3)
    A(5,4) = Ft(1,2) * Ft(2,3) + Ft(2,2) * Ft(1,3)
    A(5,5) = Ft(2,2) * Ft(3,3) + Ft(3,2) * Ft(2,3)
    A(5,6) = Ft(3,2) * Ft(1,3) + Ft(1,2) * Ft(3,3)
    A(6,1) = Ft(1,1) * Ft(1,3)
    A(6,2) = Ft(2,1) * Ft(2,3)
    A(6,3) = Ft(3,1) * Ft(3,3)
    A(6,4) = Ft(1,3) * Ft(2,1) + Ft(2,3) * Ft(1,1)
    A(6,5) = Ft(2,3) * Ft(3,1) + Ft(3,3) * Ft(2,1)
    A(6,6) = Ft(3,3) * Ft(1,1) + Ft(1,3) * Ft(3,1)

    T = matmul(A, matmul(W,transpose(A)) )

    end function

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine SystemDerivatives (this, eps, d2W, X)

    use MathRoutines

    ! Object
    ! -----------------------------------------------------------------------------------
    class(ClassElasticViscoPlastic_3D) :: this


    ! Internal variables
    ! -----------------------------------------------------------------------------------
    real(8) :: X(4,3), KT(4,4), eps(3), F(4), q(3), d2W(3)
    real(8) :: alpha_n, delta_t, kappa
    integer :: j

    q = this%vi_new(1:3)
    alpha_n = this%vi_old(4)
    delta_t = this%Time - this%Time_old
    kappa =0.0d0
    X = 0.0d0
    F = 0.0d0
    KT = 0.0d0


    do j=1,3
        select case (j)
        case (1)
            F(1)=-2*d2W(j)
            F(2)=d2W(j)
            F(3)=d2W(j)
            F(4)=-d2W(j)*q(j)
        case (2)
            F(1)=d2W(j)
            F(2)=-2*d2W(j)
            F(3)=d2W(j)
            F(4)=-d2W(j)*q(j)
        case (3)
            F(1)=d2W(j)
            F(2)=d2W(j)
            F(3)=-2*d2W(j)
            F(4)=-d2W(j)*q(j)
        end select


        call Kt_Newton(this%Properties, eps, this%vi_new, alpha_n, delta_t, kappa, KT )

        ! DELTA = -(KT)^-1 * VFun
        call Solve_Linear_System(KT, X(:,j), -F)

    enddo


    end subroutine

    !!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !
    subroutine GetTangentModulus_ElasticViscoPlastic_3D(this, D)

    !************************************************************************************
    ! DECLARATIONS OF VARIABLES
    !************************************************************************************
    ! Modules and implicit declarations
    ! ---------------------------------------------------------------------------------
    use MathRoutines

    ! Object
    ! -----------------------------------------------------------------------------------
    class(ClassElasticViscoPlastic_3D) :: this

    ! Output variables
    ! -----------------------------------------------------------------------------------
    real(8) , dimension(:,:) ,intent(inout) :: D

    ! Internal variables
    ! -----------------------------------------------------------------------------------
    real(8) :: MTX1(6,6), MTX2(6,6), MTX3(6,6), MTX4(6,6), MTX5(6,6), Dbar(6,6), MdC(6,6)
    real(8) :: MTX21(6,6), MTX22(6,6), MTX23(6,6), MTX24(6,6), dEjdC(6,6), PROJT(6,6), dC2dC(6,6)
    real(8) :: CioCi(6,6) , CixCi(6,6), MxCi(6,6), CixM(6,6), IxM(6,6), MxI(6,6), MxM(6,6), IxI(6,6)
    real(8), dimension(6,6) :: CxC, CxI, IxC
    real(8) :: AUX(6,6), derivs(4,3), depsdetr(3,3), dWe(3), d2We(3)
    real(8) :: F_new(3,3), C_new(3,3), Ciso(3,3), Cinv(3,3), dWdCiso(3,3), DEV_dWdCiso(3,3), I(3,3), Fpn(3,3)
    real(8) :: Ea (3,3,3), Ei(3,3), Ej(3,3), Cpr(3,3), Ctrinv(3,3)
    real(8) :: vC(6), vCinv(6), vdWdCiso(6), vDEV_dWdCiso(6), vdJdC(6), vEi(6), vEj(6), vI(6), vCtrinv(6), vCpr(6)
    real(8) :: eps(3), etr(3), ctr(3), vi_new(4), la(3)
    real(8) :: J, BULK, dUdJ, d2UdJ2, delta_alpha, We, TOL, I1, I3, Da
    real(8) :: s1, s2, s3, s4 ,s5, s6, dwde1, dwde3, umnumero, NORMATAN
    integer :: cont, flag_ELAST, k, l, p1, p2, p3

    TOL=1.0e-4

    Dbar= 0.0d0
    MdC = 0.0d0

    I = 0.0d0
    I(1,1) = 1.0d0
    I(2,2) = 1.0d0
    I(3,3) = 1.0d0

    vI = Convert_to_Voigt(I)

    vi_new = this%vi_new
    BULK = this%Properties%BulkModulus
    dWdCiso = this%dWdCiso
    DEV_dWdCiso = this%DEV_dWdCiso
    Ea = this%Ea
    etr = this%etrial
    ctr = dexp(2.0d0*etr)

    delta_alpha = this%vi_new(4) - this%vi_old(4)

    !************************************************************************************

    !************************************************************************************
    ! TANGENT MODULUS
    !************************************************************************************

    ! Optional: Retrieve Variables
    ! -----------------------------------------------------------------------------------
    F_new = this%F

    Fpn = this%Fp_old

    C_new = matmul(transpose(F_new),F_new)

    J = dsqrt(det(C_new))

    Ciso =  (J**(-2.0d0/3.0d0))*C_new

    Cpr = matmul( transpose(inverse(Fpn)), matmul(Ciso, inverse(Fpn)) )

    ! -----------------------------------------------------------------------------------
    ! PRIMEIRO TERMO ISOCORICO
    ! -----------------------------------------------------------------------------------
    vC = Convert_to_Voigt(C_new)
    Cinv = (inverse(C_new))
    vCinv = Convert_to_Voigt(Cinv)
    vDEV_dWdCiso = Convert_to_Voigt(DEV_dWdCiso)
    MTX1 = -(4.0d0/3.0d0) * J**(-2.0d0/3.0d0)*Ball_Voigt(vDEV_dWdCiso,vCinv)
    ! -----------------------------------------------------------------------------------

    ! ***********************************************************************************
    ! SEGUNDO TERMO ISOCORICO
    ! ***********************************************************************************
    !call  Deriv_dWdctr(MTX2)
    MTX2=0.0d0

    eps = etr - delta_alpha*vi_new(1:3)

    call Ogden(this%Properties, eps, We, dWe, d2We)

    flag_ELAST= this%flag_ELAST

    if (flag_ELAST .eq. 0) then
        call SystemDerivatives (this, eps, d2We, derivs)
    else
        derivs = 0.0d0
    endif

    do k=1,3
        do l=1,3
            depsdetr(k,l)=(I(k,l)-derivs(4,l)*vi_new(k)-delta_alpha*derivs(k,l))
        enddo
    enddo

    do k=1,3
        do l=1,3
            AUX(k,l) = d2We(k)*( 1.0d0 / (4.0d0*(ctr(k)*ctr(l))))* depsdetr(k,l) &
            - dWe(k)*I(k,l)*(1.0d0/(2.0d0*(ctr(k)**2.0d0)))
        enddo
    enddo

    if ((dabs(ctr(1)-ctr(2)) .gt. TOL) &
    .and. (dabs(ctr(1)-ctr(3))  .gt. TOL) &
    .and. (dabs(ctr(2)-ctr(3))  .gt. TOL)) then

    do k=1,3
        do l=1,3
            Ei=Ea(:,:,k)
            Ej=Ea(:,:,l)
            vEi=Convert_to_Voigt(Ei)
            vEj=Convert_to_Voigt(Ej)
            MdC = MdC + AUX(k,l)*Ball_Voigt(vEi,vEj)
        enddo
    enddo
    !-----------------------------------------------------------------
    ! BEGIN - EIGENPROJECTION DERIVATIVES
    !-----------------------------------------------------------------
    AUX=0.0d0

    la = ctr
    I1 = Cpr(1,1) + Cpr(2,2) + Cpr(3,3)
    I3 = det(Cpr)

    Ctrinv = (inverse(Cpr))
    vCtrinv = Convert_to_Voigt(Ctrinv)

    do k=1,3
        vEj = Convert_to_Voigt(Ea(:,:,k))

        Da = 2.0d0*la(k)**2.0d0 - I1*la(k) + I3*(la(k)**-1.0d0)

        CioCi = Square_Voigt(vCtrinv,vCtrinv)
        CixCi = Ball_Voigt(vCtrinv,vCtrinv)
        MxCi  = Ball_Voigt(vEj ,vCtrinv)
        CixM  = Ball_Voigt(vCtrinv,vEj)
        IxM   = Ball_Voigt(vI,vEj)
        MxI   = Ball_Voigt(vEj,vI);
        MxM   = Ball_Voigt(vEj,vEj)
        IxI   = Ball_Voigt(vI,vI)

        dEjdC = &
        (Da**-1.0d0)*la(k)*( IsymV() - (I3*la(k)**-1.0d0)*CioCi )+ &
        (Da**-1.0d0)*la(k)*( (I3*la(k)**-1.0d0)*CixCi - IxI )+ &
        (Da**-1.0d0)*la(k)*( -(I3*la(k)**-2.0d0)*(CixM + MxCi) + ( IxM  + MxI ) ) + &
        (Da**-1.0d0)*la(k)*( -2.0d0*(1.0d0-(I3*la(k)**-3.0d0))*(MxM))

        AUX = AUX + (1.0d0/(2.0d0*ctr(k)))*dWe(k)*dEjdC
    enddo
    !-----------------------------------------------------------------
    ! END - EIGENPROJECTION DERIVATIVES
    !-----------------------------------------------------------------
    MdC = MdC + AUX
    !MTX21 = OperadW2dCtr( MdC ,fpninv)

    else
        !    % -------------------------------------------------------------------------
        !    % PARA TODOS OS AUTOVALORES IGUAIS
        if ((dabs(ctr(1)-ctr(2)) .lt. TOL) &
        .and. (dabs(ctr(1)-ctr(3))  .lt. TOL) &
        .and. (dabs(ctr(2)-ctr(3))  .lt. TOL) ) then

        IxI  = Ball_Voigt(vI,vI)
        MdC = (AUX(1,1)-AUX(1,2))*IsymV()+(AUX(1,2)*IxI)

        else
            !        % -------------------------------------------------------------------------
            !        % PARA DOIS AUTOVALORES IGUAIS
            if (dabs(ctr(1)-ctr(2)) .lt. TOL) then
                p1=1
                p2=2
                p3=3
            endif
            if  (dabs(ctr(1)-ctr(3)) .lt. TOL) then
                p1=1
                p2=3
                p3=2
            endif
            if  (dabs(ctr(2)-ctr(3)) .lt. TOL) then
                p1=2
                p2=3
                p3=1
            endif
            la = ctr

            dwde1=dWe(p1)/(2.0d0*la(p1))
            dwde3=dWe(p3)/(2.0d0*la(p3))

            s1 = (dwde3-dwde1)/((la(p3)-la(p1))**2.0d0) + (1.0d0/(la(p3)-la(p1)))* (AUX(p1,p2) - AUX(p1,p1))

            s2 = 2.0d0*la(p1)*(dwde3-dwde1)/((la(p3)-la(p1))**2.0d0) &
            + ((la(p3)+la(p1))/(la(p3)-la(p1))) * (AUX(p1,p2) - AUX(p1,p1))

            s3 = 2.0d0*(dwde3-dwde1)/((la(p3)-la(p1))**3.0d0) &
            + (1.0d0/((la(p3)-la(p1))**2.0d0))*(AUX(p3,p1) + AUX(p1,p3) - AUX(p3,p3) - AUX(p1,p1))

            s4 = 2.0d0*la(p1)* (dwde3-dwde1)/((la(p3)-la(p1))**3) &
            + (1.0d0/(la(p3)-la(p1)))* (AUX(p3,p1) - AUX(p1,p2)) &
            + (la(p1)/((la(p3)-la(p1))**2.0d0))*(AUX(p3,p1) + AUX(p1,p3) - AUX(p3,p3) - AUX(p1,p1))

            s5 = 2.0d0*la(p1)* (dwde3-dwde1)/((la(p3)-la(p1))**3) &
            + (1.0d0/(la(p3)-la(p1)))* (AUX(p1,p3) - AUX(p1,p2)) &
            + (la(p1)/((la(p3)-la(p1))**2.0d0))*(AUX(p3,p1) + AUX(p1,p3) - AUX(p3,p3) - AUX(p1,p1));

            s6 = 2.0d0*(la(p1)**2.0d0)* (dwde3-dwde1)/((la(p3)-la(p1))**3.0d0) &
            + (la(p3)*la(p1)/((la(p3)-la(p1))**2.0d0))*(AUX(p3,p1) + AUX(p1,p3)) &
            - ((la(p1)*la(p1))/((la(p3)-la(p1))**2.0d0))*(AUX(p3,p3) + AUX(p1,p1)) &
            - ((la(p3)+la(p1))/((la(p3)-la(p1)))) * AUX(p1,p2)


            !Jc=dsqrt(det(C))
            !Ctrial = transpose(fpn^-1) * (Jc^(-2/3)*C) * (fpn)^-1;

            dC2dC = dX2dX(Cpr)

            vCpr = Convert_to_Voigt(Cpr)
            CxC  = Ball_Voigt(vCpr,vCpr)
            CxI  = Ball_Voigt(vCpr,vI)
            IxC  = Ball_Voigt(vI,vCpr)
            IxI  = Ball_Voigt(vI,vI)

            MdC = s1*dC2dC-s2*IsymV()-s3*CxC+s4*CxI+s5*IxC-s6*IxI
            !
            !        % -------------------------------------------------------------------------
        endif
    endif
    MTX21 = OperadW2dCtr(MdC, inverse(Fpn))


    ! ***********************************************************************************
    ! ***********************************************************************************

    ! -----------------------------------------------------------------------------------
    ! TERCEIRO TERMO ISOCORICO
    ! -----------------------------------------------------------------------------------
    MTX22 = Tensor_4_Double_Contraction_Voigt(Ball_Voigt(vCinv, vC), MTX21)
    ! -----------------------------------------------------------------------------------

    ! -----------------------------------------------------------------------------------
    ! QUARTO TERMO ISOCORICO
    ! -----------------------------------------------------------------------------------
    vdWdCiso = Convert_to_Voigt(dWdCiso)
    MTX23 = Ball_Voigt(vCinv, vdWdCiso)
    ! -----------------------------------------------------------------------------------

    ! -----------------------------------------------------------------------------------
    ! QUINTO TERMO ISOCORICO
    ! -----------------------------------------------------------------------------------
    MTX24 = Inner_Product_Voigt(vdWdCiso, vC) * -1.0d0*Square_Voigt(VCinv, VCinv)
    ! -----------------------------------------------------------------------------------

    AUX=0.0d0

    AUX = 4.0d0*(J**(-4.0d0/3.0d0)*MTX21 &
    -1.0d0/3.0d0*(J**(-4.0d0/3.0d0)*MTX22 + J**(-2.0d0/3.0d0)*MTX23 + J**(-2.0d0/3.0d0)*MTX24))

    PROJT = IsymV() - (1.0d0/3.0d0)* Ball_Voigt(vC, vCinv)

    MTX2 = Tensor_4_Double_Contraction_Voigt( AUX, PROJT)

    ! -----------------------------------------------------------------------------------
    ! PRIMEIRO TERMO VOLUMETRICO
    ! -----------------------------------------------------------------------------------
    d2UdJ2 = BULK * (J**-2.0d0) * (1.0d0-dlog(J))
    vdJdC = 0.5d0 * J * vCinv
    MTX3 = 2.0d0*Ball_Voigt(J*vCinv, d2UdJ2*vdJdC)
    ! -----------------------------------------------------------------------------------

    ! -----------------------------------------------------------------------------------
    ! SEGUNDO TERMO VOLUMETRICO
    ! -----------------------------------------------------------------------------------
    dUdJ = BULK*(J**-1.0d0)*(dlog(J));
    MTX4 =2.0d0 * Ball_Voigt(vCinv , dUdJ*vdJdC )
    ! -----------------------------------------------------------------------------------

    !  -----------------------------------------------------------------------------------
    ! TERCEIRO TERMO VOLUMETRICO
    ! -----------------------------------------------------------------------------------
    MTX5 = -1.0d0 * Square_Voigt(vCinv, vCinv)
    MTX5 = 2.0d0*J*dUdJ*MTX5
    ! -----------------------------------------------------------------------------------

    Dbar = MTX1+MTX2+MTX3+MTX4+MTX5

    umnumero = Tensor_Inner_Product(Dbar,Dbar)
    NORMATAN=dsqrt(umnumero)

    open(10, file='aaaa.txt', access='append', status='unknown')
       write (10,*) NORMATAN
    close(10)


    !      !! Push-Forward:
    !      !! Computation of the modified spatial tangent modulus
    !      !! -----------------------------------------------------------------------------------
    D = Push_Forward_Voigt(Dbar,F_new)
    !      ! -----------------------------------------------------------------------------------
    D=D

    end subroutine      !FIM DO MODULO TANGENTE



    !!!************************************************************************************
    !!!  MODULO TANGENTE POR INDIFERENCA INFINITA
    !!!************************************************************************************
    !   subroutine GetTangentModulus_ElasticViscoPlastic_3D(this,D)
    !
    !
    !   !************************************************************************************
    !   ! DECLARATIONS OF VARIABLES
    !   !************************************************************************************
    !   ! Modules and implicit declarations
    !   ! ---------------------------------------------------------------------------------
    !   use MathRoutines
    !
    !   ! Object
    !   ! -----------------------------------------------------------------------------------
    !   class(ClassElasticViscoPlastic_3D) :: this
    !
    !   ! Output variables
    !   ! -----------------------------------------------------------------------------------
    !   real(8) , dimension(:,:) ,intent(inout) :: D
    !
    !   ! Internal variables
    !   ! -----------------------------------------------------------------------------------
    !   real(8) :: Dbar(6,6), F_new(3,3), C_new(3,3), C_f(3,3), C_b(3,3), Ciso_f(3,3), Ciso_b(3,3)
    !   real(8) :: S_f(3,3), S_b(3,3)
    !   real(8) :: vector_C(6), vector_C_f(6), vector_C_b(6), vector_Sf(6), vector_Sb(6), AUX(6)
    !   real(8) :: PERTUB, Jf, Jb
    !   integer :: cont, i, j
    !
    !   PERTUB = 1.0d-4
    !   AUX=0.0d0
    !   Dbar=0.0d0
    !
    !   !************************************************************************************
    !
    !   !************************************************************************************
    !   ! TANGENT MODULUS
    !   !************************************************************************************
    !
    !   ! Optional: Retrieve Variables
    !   ! -----------------------------------------------------------------------------------
    !   F_new = this%F
    !
    !   C_new = matmul(transpose(F_new),F_new)
    !
    !   do cont=1,6
    !
    !   vector_C = [C_new(1,1), C_new(2,2), C_new(3,3), C_new(1,2), C_new(2,3), C_new(1,3)]
    !
    !   vector_C_f = vector_C
    !   vector_C_b = vector_C
    !
    !   vector_C_f(cont) = vector_C_f(cont) + PERTUB
    !   vector_C_b(cont) = vector_C_b(cont) - PERTUB
    !
    !   C_f(1,:) =  [ vector_C_f(1), vector_C_f(4), vector_C_f(6)]
    !   C_f(2,:) =  [ vector_C_f(4), vector_C_f(2), vector_C_f(5)]
    !   C_f(3,:) =  [ vector_C_f(6), vector_C_f(5), vector_C_f(3)]
    !
    !   C_b(1,:) =  [ vector_C_b(1), vector_C_b(4), vector_C_b(6)]
    !   C_b(2,:) =  [ vector_C_b(4), vector_C_b(2), vector_C_b(5)]
    !   C_b(3,:) =  [ vector_C_b(6), vector_C_b(5), vector_C_b(3)]
    !
    !   Jf = dsqrt(det(C_f))
    !   Jb = dsqrt(det(C_b))
    !
    !   Ciso_f =  (Jf**(-2.0d0/3.0d0))*C_f
    !   Ciso_b =  (Jb**(-2.0d0/3.0d0))*C_b
    !
    !   call TanModDF_ElasticViscoPlastic_3D(this, C_f, Ciso_f, Jf, S_f)
    !   call TanModDF_ElasticViscoPlastic_3D(this, C_b, Ciso_b, Jb, S_b)
    !
    !   vector_Sf = [S_f(1,1), S_f(2,2), S_f(3,3), S_f(1,2), S_f(2,3), S_f(1,3)]
    !   vector_Sb = [S_b(1,1), S_b(2,2), S_b(3,3), S_b(1,2), S_b(2,3), S_b(1,3)]
    !
    !   AUX= 0.5d0*(vector_Sf - vector_Sb)/PERTUB
    !
    !   Dbar(1:6,cont) = AUX
    !
    !   enddo
    !
    !   do i=1,6
    !       do j=1,6
    !           if (j .gt. 3) then
    !               Dbar(i,j) = 0.5d0*Dbar(i,j)
    !           end if
    !       enddo
    !   enddo
    !
    !
    !   Dbar=2.0d0*Dbar
    !
    !   !      !! Push-Forward:
    !   !      !! Computation of the modified spatial tangent modulus
    !   !      !! -----------------------------------------------------------------------------------
    !   D = Push_Forward_Voigt(Dbar,F_new)
    !   !      ! -----------------------------------------------------------------------------------
    !   D=D
    !
    !   end subroutine      !FIM DO MODULO TANGENTE POR INDIFERENCA INFINITA




    !==========================================================================================
    !==========================================================================================
    ! Method SaveConvergedState_"NameOfTheMaterialModel": Routine that save de converged state.
    !------------------------------------------------------------------------------------------
    ! Modifications:
    ! Date:         Author:
    !==========================================================================================
    subroutine SaveConvergedState_ElasticViscoPlastic(this)
    class(ClassElasticViscoPlastic) :: this
    this%Fp_old=this%Fp_new
    this%vi_old=this%vi_new
    this%Time_old=this%Time
    this%vidam_old=this%vidam_new
    end subroutine
    !==========================================================================================


    ! TODO (Thiago#1#02/13/15): Ver no GiD a ordem de exportar as tensões (notação de Voigt)
    !==========================================================================================
    ! Method GetTangentModulus_"NameOfTheMaterialModel"_3D: Routine that evaluates the
    ! Tangente Modulus.
    !------------------------------------------------------------------------------------------
    ! Modifications:
    ! Date:         Author:
    !==========================================================================================
!    subroutine GetResult_ElasticViscoPlastic(this, ID , Name , Length , Variable , VariableType  )
!
!    !************************************************************************************
!    ! DECLARATIONS OF VARIABLES
!    !************************************************************************************
!    ! Modules and implicit declarations
!    ! ---------------------------------------------------------------------------------
!    use MathRoutines
!
!    ! Object
!    ! -----------------------------------------------------------------------------------
!    class(ClassElasticViscoPlastic) :: this
!
!    ! Input variables
!    ! -----------------------------------------------------------------------------------
!    integer :: ID
!
!    ! Output variables
!    ! -----------------------------------------------------------------------------------
!    character(len=*)            :: Name
!    integer                     :: Length, VariableType
!    real(8) , dimension(:)      :: Variable
!
!    ! Internal variables
!    ! -----------------------------------------------------------------------------------
!    integer, parameter :: Scalar=1,Vector=2,Tensor=3
!    real (8) :: h , c(6), I(3,3), e(3), eV(6), von_Mises
!    real(8) :: eigenvectors(3,3), eigenvalues(3), RightCauchy(3,3), TEMP2(3)
!    real(8) :: work(10)
!    integer :: info
!    !************************************************************************************
!
!    !___________________   WARNIG! DO NOT CHANGE OR ERASE THIS BLOCK    _________________
!    ! Initializing variable name.
!    Name = ''
!    !____________________________________________________________________________________
!
!
!    ! Template to Export Result to GiD
!    !------------------------------------------------------------------------------------
!
!    !case(0)
!    ! Inform the number of results
!    !Length = 3
!    !case(1)
!    !Name = 'Name of the Variable'
!    !VariableType = 'Type of the Variable (Scalar,Vector,Tensor(in Voigt Notation))'
!    !Length = 'Size of the Variable'
!    !Variable = Result to be informed. Inform a Gauss Point result or compute a new
!    !           variable.
!
!    !------------------------------------------------------------------------------------
!
!    ! Identity
!    I = 0.0d0
!    I(1,1) = 1.0d0
!    I(2,2) = 1.0d0
!    I(3,3) = 1.0d0
!
!    !von_Mises = 0.0d0
!
!    select case (ID)
!
!    case(0)
!
!    Length = 6
!
!    case(1)
!
!    Name='Cauchy Stress'
!    VariableType=Tensor
!    Length=size(this%Stress)
!    Variable(1:Length) = this%Stress
!
!    case (2)
!
!    Name='Log Strain'
!    VariableType = Vector
!    Length=3
!    !-------------------------------------------------------------
!    !Almansi Strain
!    !-------------------------------------------------------------
!    RightCauchy = matmul( transpose(this%F) , this%F )
!    eigenvectors = RightCauchy
!    call dsyev("V", "U", 3, eigenvectors, 3, eigenvalues, work, 10, info)
!    TEMP2=eigenvalues
!    eigenvalues(1:3) = [TEMP2(3), TEMP2(2), TEMP2(1)]
!    e =  dlog(dsqrt(eigenvalues))
!    Variable(1:Length) = e(1:Length)
!    !-------------------------------------------------------------
!    case(3)
!
!    Name='Total Damage'
!    VariableType = Scalar
!    Length=1
!    Variable(1:Length) = 1-this%vidam_new(1)
!
!    case(4)
!
!    Name='Plastic Damage'
!    VariableType = Scalar
!    Length=1
!    Variable(1:Length) = this%vidam_new(2)
!
!    case(5)
!
!    Name='Hydro Damage'
!    VariableType = Scalar
!    Length=1
!    Variable(1:Length) = this%vidam_new(3)
!
!    case(6)
!
!    Name='von Mises'
!    VariableType = Scalar
!    Length=1
!
!    von_Mises=dsqrt(0.5d0*( (this%Stress(1)-this%Stress(2))**2.0d0 &
!    + (this%Stress(2)-this%Stress(3))**2.0d0 + (this%Stress(3)-this%Stress(1))**2.0d0 &
!    + 6.0d0*(this%Stress(4)**2.0d0+this%Stress(5)**2.0d0+this%Stress(6)**2.0d0) ) )
!
!    Variable(1:Length) = von_Mises
!
!
!    case default
!    call Error("Error retrieving result :: GetResult")
!    end select
!
!    !************************************************************************************
!
!    end subroutine
    !==========================================================================================

    end module

    !==========================================================================================
    ! Method GetTangentModulus_"NameOfTheMaterialModel"_3D: Routine that evaluates the
    ! Tangente Modulus.
    !------------------------------------------------------------------------------------------
    ! Modifications:
    ! Date:         Author:
    !==========================================================================================
    !  subroutine GetTangentModulus_ElasticViscoPlastic_3D(this,D)
    !
    !
    !!************************************************************************************
    !      ! DECLARATIONS OF VARIABLES
    !!************************************************************************************
    !      ! Modules and implicit declarations
    !      ! ---------------------------------------------------------------------------------
    !      use MathRoutines
    !
    !      ! Object
    !      ! -----------------------------------------------------------------------------------
    !      class(ClassElasticViscoPlastic_3D) :: this
    !
    !      ! Output variables
    !      ! -----------------------------------------------------------------------------------
    !      real(8) , dimension(:,:) ,intent(inout) :: D
    !
    !      ! Internal variables
    !      ! -----------------------------------------------------------------------------------
    !      real(8) :: J, Jbar, pbar, D2psiDJ2, BulkModulus, C10
    !      real(8) :: F(3,3), C(3,3),Cinv(3,3), Ciso(3,3), Sfric(3,3)
    !
    !
    !      real(8) :: CV(6), CinvV(6), CisoV(6), SfricV(6), devSfricV(6), SisoV(6)
    !      real(8) :: PmV(6,6) , PV(6,6), Diso(6,6), Dp(6,6), Dbar(6,6), Is(6,6)
    !      real(8) :: dPSIiso_dCiso(3,3), dPSIvol_dJbar, d2PSIiso_dCiso2(6,6)
    !
    !!************************************************************************************
    !
    !      !************************************************************************************
    !      ! TANGENT MODULUS
    !!************************************************************************************
    !
    !      ! Optional: Retrieve Variables
    !      ! -----------------------------------------------------------------------------------
    !      BulkModulus = this%Properties%BulkModulus
    !      !C10         = this%Properties%C10
    !      !F           = this%F
    !      !Jbar        = this%Jbar
    !      !! -----------------------------------------------------------------------------------
    !      !
    !      !! Quantities calculated in 3D Tensorial Format
    !      !! -----------------------------------------------------------------------------------
    !      !! Right-Cauchy Green Strain
    !      !C = matmul(transpose(F),F)
    !      !
    !      !! Inverse of Right-Cauchy Green Strain
    !      !Cinv = inverse(C)
    !      !
    !      !! Isochoric part of the Right-Cauchy Green Strain
    !      !Ciso = (J**(-2.0d0/3.0d0))*C
    !      !
    !      !! Jacobian
    !      !J = det(F)
    !      !
    !      !! Computations of the first derivatives of isochoric Strain Energy function with respected to:
    !      !! dPSIiso_dCiso = Isochoric Right-Cauchy Green Strain
    !      !! dPSIvol_dJbar = Mean Dilatation
    !      !call FirstDerivativesOfPSI ( this%Properties, Jbar, Ciso, dPSIiso_dCiso, dPSIvol_dJbar  )
    !      !
    !      !! Second Piola-Kirchhoff Frictional
    !      !Sfric = 2.0d0*dPSIiso_dCiso
    !      !
    !      !! Modified Hydrostatic Pressure
    !      !pbar = dPSIvol_dJbar
    !      !
    !      !! -----------------------------------------------------------------------------------
    !      !
    !      !
    !      !! -----------------------------------------------------------------------------------
    !      !! The subsequent computations are made in Voigt notation
    !      !! -----------------------------------------------------------------------------------
    !      !
    !      !
    !      !! Material tangent modulus in referential configuration
    !      !! -----------------------------------------------------------------------------------
    !      !
    !      !! Right-Cauchy Green Strain
    !      !CV = Convert_to_Voigt(C)
    !      !
    !      !! Inverse of Right-Cauchy Green Strain
    !      !CinvV = Convert_to_Voigt(Cinv)
    !      !
    !      !! Isochoric part of the Right-Cauchy Green Strain
    !      !CisoV = Convert_to_Voigt(Ciso)
    !      !
    !      !! Second Piola-Kirchhoff Frictional
    !      !SfricV = Convert_to_Voigt(Sfric)
    !      !
    !      !! Deviatoric part of the Second Piola-Kirchhoff Frictional
    !      !devSfricV = SfricV - (1.0d0/3.0d0)*Inner_Product_Voigt(SfricV,CV)*CinvV
    !      !
    !      !! Isochoric part of the Second Piola-Kirchhoff
    !      !SisoV = (J**(-2.0d0/3.0d0))*devSfricV
    !      !
    !      !! Projection Operator
    !      !PV = IsymV() - (1.0d0/3.0d0)*Ball_Voigt(CinvV,CV)
    !      !
    !      !! Modified Projection Operator
    !      !PmV = Square_Voigt(CinvV,CinvV) - (1.0d0/3.0d0)*Ball_Voigt(CinvV,CinvV)
    !      !
    !      !! Computations of the first derivatives of isochoric Strain Energy function with respected to:
    !      !! dPSIiso_dCiso = Isochoric Right-Cauchy Green Strain
    !      !! dPSIvol_dJbar = Mean Dilatation
    !      !call SecondDerivativesOfPSI_Ciso ( this%Properties, CisoV, d2PSIiso_dCiso2 )
    !      !
    !      !
    !      !! Isochoric part of the material tangent modulus in referential configuration
    !      !Diso = Tensor_4_Double_Contraction_Voigt( Tensor_4_Double_Contraction_Voigt(PV,d2PSIiso_dCiso2),transpose(Pv)) + &
    !      !        (2.0d0/3.0d0)*(J**(-2.0d0/3.0d0))*Inner_Product_Voigt(SfricV,CV)*PmV - &
    !      !        (2.0d0/3.0d0)*( Ball_Voigt(SisoV,CinvV) + Ball_Voigt(CinvV,SisoV) )
    !      !
    !      !! Pressure component of the material tangent modulus in referential configuration
    !      !Dp  = J*pbar*( Ball_Voigt(CinvV,CinvV) - 2.0d0*Square_Voigt(CinvV,CinvV)  )
    !      !
    !      !! Modified material tangent modulus in referential configuration
    !      !Dbar = Diso + Dp
    !      !! -----------------------------------------------------------------------------------
    !      !
    !      !! Push-Forward:
    !      !! Computation of the modified spatial tangent modulus
    !      !! -----------------------------------------------------------------------------------
    !      !D = Push_Forward_Voigt(Dbar,F)
    !      ! -----------------------------------------------------------------------------------
    !
    !
    !!************************************************************************************
    !
    !  end subroutine
    !==========================================================================================
