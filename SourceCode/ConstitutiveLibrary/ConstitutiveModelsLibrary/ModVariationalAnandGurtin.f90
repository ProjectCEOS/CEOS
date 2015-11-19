module modVariationalAnandGurtin2003

    use ConstitutiveModel
    use MathRoutines

    implicit none

    private

    type VAGProperties

        real(8) :: G , K , mu

        real(8) :: m , nu0

        !Flux Resistance
        real(8) :: s0,scv,zeta,beta,gamma

    end type

    type ClassState
        real(8) :: r = 0.0d0
        real(8) :: Fp(3,3) = 0.0d0
    end type

    type , extends (ClassConstitutiveModel) :: ClassVariationalAnandGurtin2003

        type(VAGProperties),pointer :: Properties => null()

        type(ClassState) :: NewState , OldState

    contains

            procedure :: UpdateStressAndStateVariables => UpdateStressAndStateVariables_VAG
            procedure :: GetTangentModulus => GetTangentModulus_VAG
            procedure :: SaveConvergedState => SaveConvergedState_VAG
            procedure :: ConstitutiveModelConstructor => ConstitutiveModelConstructor_VAG
            procedure :: ReadMaterialParameters => ReadMaterialParameters_VAG
            procedure :: GetResult => GetResult_VAG
            procedure :: SecondDerivativesOfPSI_Jbar => SecondDerivativesOfPSI_Jbar_VAG
            procedure :: CopyProperties => CopyProperties_VAG

    end type

    contains

    subroutine ReadMaterialParameters_VAG(this ,DataFile)
        use Parser
        class(ClassVariationalAnandGurtin2003) :: this
        class(ClassParser)::DataFile

        character(len=100),dimension(10)::ListOfOptions,ListOfValues

        allocate (this%Properties)

        ListOfOptions=["K","G","mu","m","nu0","s0","scv","zeta","beta","gamma"]

        call DataFile%FillListOfOptions(ListOfOptions,ListOfValues)

        call DataFile%ConvertToDouble(ListOfValues(1),this%Properties%K)
        call DataFile%ConvertToDouble(ListOfValues(2),this%Properties%G)
        call DataFile%ConvertToDouble(ListOfValues(3),this%Properties%mu)
        call DataFile%ConvertToDouble(ListOfValues(4),this%Properties%m)
        call DataFile%ConvertToDouble(ListOfValues(5),this%Properties%nu0)
        call DataFile%ConvertToDouble(ListOfValues(6),this%Properties%s0)
        call DataFile%ConvertToDouble(ListOfValues(7),this%Properties%scv)
        call DataFile%ConvertToDouble(ListOfValues(8),this%Properties%zeta)
        call DataFile%ConvertToDouble(ListOfValues(9),this%Properties%beta)
        call DataFile%ConvertToDouble(ListOfValues(10),this%Properties%gamma)

    end subroutine

    subroutine CopyProperties_VAG(this,Reference)
        class(ClassVariationalAnandGurtin2003) :: this
        class(ClassConstitutiveModel) :: Reference
        select type ( Reference )
            class is ( ClassVariationalAnandGurtin2003 )
                this%Properties => Reference%Properties
            class default
                stop "ERROR :: CopyProperties :: ClassVariationalAnandGurtin2003 - input reference not identified"
        end select
    end subroutine

    subroutine ConstitutiveModelConstructor_VAG(this,AnalysisSettings)
        use Analysis
        class(ClassVariationalAnandGurtin2003) :: this
        type(ClassAnalysis) :: AnalysisSettings

        ! TODO (Jan#1#11/18/15): O usuário não precisa se preocupar com a tensão...  ...
!Colocar para alocar a tensão na rotina que chama o construtor. Ele deve apenas inicializar as suas variáveis internas
        allocate( this%Stress( AnalysisSettings%StressSize ) ) ; this%Stress= 0.0d0

        this%OldState%r=0.0d0
        this%OldState%Fp=IdentityMatrix(3)
    end subroutine

    subroutine UpdateStressAndStateVariables_VAG(this)
        class(ClassVariationalAnandGurtin2003) :: this
    end subroutine

    subroutine GetTangentModulus_VAG(this, D)
        class(ClassVariationalAnandGurtin2003) :: this
        real(8) , dimension(:,:) , intent(inout) :: D
    end subroutine

    subroutine SaveConvergedState_VAG(this)
        class(ClassVariationalAnandGurtin2003) :: this
        this%OldState=this%NewState
    end subroutine

    subroutine GetResult_VAG( this, ID , Name , Length , Variable , VariableType )
        class(ClassVariationalAnandGurtin2003) :: this
        integer                         :: ID,Length,VariableType
        character(len=*)                :: Name
        real(8) , dimension(:)          :: Variable

        integer,parameter :: Scalar=1,Vector=2,Tensor=3


        Name=''

        select case (ID)
            case(0)
                Length=3
            case(1)
                !deformacao viscosa acumulada
            case(2)
                !resistencia ao fluxo
            case(3)

        end select
    end subroutine
!--------------------------------------------------------------------------------------------------------------------------------
    function FluxResistance(p,r) result(s)
        type(VAGProperties)::p
        real(8) :: r , s
        s = p%scv + dexp(-p%zeta*r) * ( (p%s0-p%scv)*dcosh(p%beta*r) + p%gamma*dsinh(p%beta*r))
    end function

    subroutine SolveNewStress(model,stress)
        class(ClassVariationalAnandGurtin2003)::model
        real(8), dimension(:) :: stress
        !ou sair em forma de tensor ?

    end subroutine




end module

