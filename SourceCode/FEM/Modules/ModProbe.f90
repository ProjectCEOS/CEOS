module ModProbe

    implicit none


    ! Enumerators
    !----------------------------------------------------------------------------------------------
    !Probe Locations
    type ClassProbeLocations
        integer  :: Node=1 , GaussPoint=2
    end type
    type (ClassProbeLocations), parameter :: ProbeLocations = ClassProbeLocations()

    !Variable Names - Default variables and user defined variables
    type ClassVariableNames
        integer  :: Displacements=1 , Temperature=2, CauchyStress=3, LogarithmicStrain=4, DeformationGradient=5 , UserDefined=6
    end type
    type (ClassVariableNames), parameter :: VariableNames = ClassVariableNames()

    !----------------------------------------------------------------------------------------------
    type , abstract :: ClassProbe

        integer :: Location
        character(len=255) :: FileName='', VariableName='', ProbeType=''
        integer, allocatable , dimension(:) :: Components
        integer :: VariableNameID, ProbeTypeID
        logical :: AllComponents = .false., Active = .true.

    contains
        procedure (Write_Probe_Result) , deferred :: WriteProbeResult
        procedure :: InitializeFile
        procedure :: WriteOnFile_Char
        procedure :: WriteOnFile_Value
        generic :: WriteOnFile => WriteOnFile_Char , WriteOnFile_Value
    end type

    abstract interface
        subroutine Write_Probe_Result(this,FEA)
            use FEMAnalysis
            import
            class(ClassProbe) :: this
            class(ClassFEMAnalysis) :: FEA
        end subroutine
    end interface

    type, extends(ClassProbe) :: ClassNodeProbe
        integer :: Node
    contains
        procedure :: WriteProbeResult => WriteProbeResult_Node
    end type

    type, extends(ClassProbe) :: ClassGaussPointProbe
        integer :: Element, GaussPoint
    contains
        procedure :: WriteProbeResult => WriteProbeResult_GaussPoint
    end type


    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    !Wrapper in order to use different types of probes
    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type ClassProbeWrapper

        class(ClassProbe) , pointer :: Pr => null()

    end type
    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


    contains

    subroutine ParseComponents( ComponentsString , Components )
        use StringLib
        use Parser
        character(len=*) :: ComponentsString
        integer , allocatable , dimension(:) :: Components
        type(ClassParser) :: Comp
        character(len=len(ComponentsString)) , pointer , dimension(:) :: SubStrings
        integer::i

        call Comp%Setup()

        if (allocated(Components)) deallocate(Components)

        call SplitSub(ComponentsString,",", SubStrings)

        if (.not.associated(SubStrings)) then
            return
        endif

        allocate(Components(size(SubStrings)))

        do i=1,size(SubStrings)
            call Comp%ConvertToInteger( SubStrings(i) , Components(i) )
        enddo

        deallocate(SubStrings)
    end subroutine

    function ParseVariableName(Variable) result(enu)
        use Parser
        character(len=*) :: Variable
        integer :: enu
        type(ClassParser)::Comp
        call Comp%Setup
        IF ( Comp%CompareStrings( Variable,'Cauchy Stress') ) then
            enu = VariableNames%CauchyStress
        ELSEIF ( Comp%CompareStrings( Variable,'Deformation Gradient') ) then
            enu = VariableNames%DeformationGradient
        ELSEIF ( Comp%CompareStrings( Variable,'Displacements') ) then
            enu = VariableNames%Displacements
        ELSEIF ( Comp%CompareStrings( Variable,'Logarithmic Strain') ) then
            enu = VariableNames%LogarithmicStrain
        ELSEIF ( Comp%CompareStrings( Variable,'Temperature') ) then
            enu = VariableNames%Temperature
        ELSE
            enu = VariableNames%UserDefined
        ENDIF
    end function





    !==========================================================================================
    subroutine InitializeFile(this)

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassProbe) :: this

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            logical :: FileExists

            !************************************************************************************

            inquire(file=this%FileName,exist=FileExists)

            if ( FileExists ) then
                open (1234, file=this%FileName, status='unknown')
                close(1234, status='delete')
            endif

            ! Writing a header
            open (1234, file=this%FileName, status='unknown')
            write(1234,*) ' Time                    Value'
            close(1234)


    end subroutine
    !==========================================================================================
    !==========================================================================================
    subroutine WriteOnFile_Char(this , string )
            class(ClassProbe) :: this
            character(len=*) :: string
            integer::FileNumber
            FileNumber = 33
            open( FileNumber, file=this%FileName, Access='append', status='unknown')
                write(FileNumber , '(A)') string
            close(FileNumber)
    end subroutine
    !==========================================================================================
    !==========================================================================================
    subroutine WriteOnFile_Value(this , Time , Values )
            class(ClassProbe) :: this
            real(8)::Time
            real(8) , dimension(:) :: Values
            character(len=20) :: CharFormat
            integer::FileNumber , i
            CharFormat=''
            write(CharFormat , '(A,I2,A)') '(' , size(Values) + 1 , '(E23.15,1x))'
            FileNumber = 33
            open( FileNumber, file=this%FileName, Access='append', status='unknown') !Create the string format
                write(FileNumber , CharFormat) Time , (Values(i),i=1,size(Values)) !Export the result
            close(FileNumber)
    end subroutine
    !==========================================================================================

    !==========================================================================================
    subroutine NodeProbeConstructor(Probe, FileName, VariableName, Node, ComponentsString)


            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use Parser
            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassProbe), pointer :: Probe

            ! Input variables
            ! -----------------------------------------------------------------------------------
            type(ClassParser) :: Comp
            character(*) :: FileName, VariableName, ComponentsString
            integer :: Node

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            type(ClassNodeProbe), pointer :: NodeProbe

            !************************************************************************************


            call Comp%Setup()


            allocate(NodeProbe)

            NodeProbe%FileName = FileName
            NodeProbe%VariableName = VariableName
            NodeProbe%Node = Node

            if ( comp%CompareStrings( VariableName,'Displacements') ) then

                NodeProbe%VariableNameID = VariableNames%Displacements

                    if ( Comp%CompareStrings(ComponentsString, 'All') ) then
                        NodeProbe%AllComponents = .true.
                    else
                        NodeProbe%AllComponents = .false.
                        Call ParseComponents( ComponentsString , NodeProbe%Components )

                    endif


            end if


            Probe => NodeProbe

    end subroutine
    !==========================================================================================

    !==========================================================================================
    subroutine WriteProbeResult_Node(this,FEA)


            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use FEMAnalysis
            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassNodeProbe) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            class(ClassFEMAnalysis) :: FEA

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8), dimension(size(this%Components)) :: Uprobe
            integer :: FileNumber, i

            !************************************************************************************


            ! TODO (Thiago#1#11/05/15): varificar se as entradas informadas no arquivo do probe é a prova de orelhas...

            ! teste se probe esta ativo
            if (.not. this%Active) then
                return
            endif


            FileNumber = 33
            open( FileNumber, file=this%FileName, Access='append', status='unknown')



            select case (this%VariableNameID)

                case (VariableNames%Displacements)

                    select case (FEA%AnalysisSettings%ProblemType)

                        case (ProblemTypes%Mechanical)

                            Uprobe = FEA%U( (this%Node - 1)*FEA%AnalysisSettings%NDOFnode + this%Components(:) )

                        case (ProblemTypes%Thermal)
                            !A fazer...

                    end select

                    write(FileNumber,*) FEA%Time, (Uprobe(i),i=1,size(Uprobe))

                case default
                    stop 'Error in ModProbe - WriteProbeResult_Node'

            end select

            close(33)


    end subroutine
    !==========================================================================================


    !==========================================================================================
    subroutine GaussPointProbeConstructor (Probe, ProbeType, Variable, Element, FileName,  GaussPoint, ComponentsString)
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use Parser
            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassProbe), pointer :: Probe

            ! Input variables
            ! -----------------------------------------------------------------------------------
            character(len=*) :: Variable, ProbeType
            character(len=*) :: FileName
            integer          :: Element, GaussPoint
            character(len=*) :: ComponentsString

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            type(ClassParser) :: Comp
            type(ClassGaussPointProbe), pointer :: GaussPointProbe
            !************************************************************************************

            call Comp%Setup()

            allocate(GaussPointProbe)

            GaussPointProbe%FileName       = FileName
            GaussPointProbe%VariableName   = Variable
            GaussPointProbe%Element        = Element
            GaussPointProbe%GaussPoint     = GaussPoint
            GaussPointProbe%VariableNameID = ParseVariableName(Variable)

            if ( Comp%CompareStrings(ComponentsString, 'All') ) then
                GaussPointProbe%AllComponents = .true.
            else
                GaussPointProbe%AllComponents = .false.
                Call ParseComponents( ComponentsString , GaussPointProbe%Components )
            endif


            Probe => GaussPointProbe

    end subroutine
    !==========================================================================================
    !==========================================================================================
    subroutine WriteProbeResult_GaussPoint(this,FEA)
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use FEMAnalysis
            use MathRoutines
            use Parser
            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassGaussPointProbe) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            class(ClassFEMAnalysis) :: FEA

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            type (ClassParser) :: Comp

            real(8), dimension(size(this%Components)) , target :: ProbeVariable
            integer :: FileNumber, i, NumberOfComp, NumberOfGaussPoint, NumberOfElements

            real(8) , dimension(FEA%AnalysisSettings%StrainSize) :: Strain_Voigt
            real(8) , dimension(9) :: F_voigt
            real(8) , dimension(3) :: eigenvalues
            real(8) , dimension(3,3) :: F, b, Log_Strain, eigenvectors, N

            real(8) , dimension(9)            :: UD_Variable
            integer                           :: UD_ID, UD_Length, UD_VariableType
            character(len=255)                :: UD_Name
            logical :: FoundUserVariable
            class(ClassConstitutiveModel) , pointer :: GP

            !************************************************************************************
            ! Teste se probe esta ativo
            if (.not. this%Active) then
                return
            endif

            call Comp%Setup()

            ! Teste de inteligência do usuário
            NumberOfElements = size(FEA%ElementList)
            if ( this%Element .gt.  NumberOfElements ) then
                call this%WriteOnFile('ERROR - Number of Element is greater than Max Number of Elements in Mesh')
                this%Active = .false.
                return
            else
                NumberOfGaussPoint = size(FEA%ElementList(this%Element)%El%GaussPoints)
                if ( this%GaussPoint .gt. NumberOfGaussPoint ) then
                    call this%WriteOnFile('ERROR - Number of Gauss Point is greater than Max Number of Gauss Points in the Element')
                    this%Active = .false.
                    return
                endif
            endif

            GP => FEA%ElementList(this%Element)%El%GaussPoints(this%GaussPoint)

            select case (this%VariableNameID)

                ! Writing Cauchy Stress
                case (VariableNames%CauchyStress)

                    NumberOfComp = size(GP%Stress)
                    if ( any( this%Components .gt. NumberOfComp ) ) then
                        call this%WriteOnFile('ERROR - Stress component is greater than Max Stress Size')
                        this%Active = .false.
                    else
                        if (this%AllComponents) then
                            call this%WriteOnFile(FEA%Time , GP%Stress)
                        else
                            ProbeVariable = GP%Stress(this%Components)
                            call this%WriteOnFile(FEA%Time , ProbeVariable)
                        endif
                    endif

                ! Writing Logarithmic Strain
                case (VariableNames%LogarithmicStrain)

                    ! Deformação tem o mesmo numero do componenetes da tensão
                    NumberOfComp = size(GP%Stress)
                    if ( any( this%Components .gt. NumberOfComp ) ) then
                        call this%WriteOnFile('ERROR - Strain component is greater than Max Strain Size')
                        this%Active = .false.
                    else

                                ! TODO (Jan#1#11/14/15): Isso aqui é precioso demais para ficar só aqui. ...
!Tem que criar um modulo para disponibilizar isso quando quiser. Ex: modMechanical , modContinuumMechanics sei lá
                                ! Logarithmic Strain
                                !---------------------------------------------------------------------------------------
                                F = 0.0d0
                                b = 0.0d0
                                F = GP%F
                                b = matmul(F,transpose(F))

                                eigenvalues  = 0.0d0
                                eigenvectors = 0.0d0
                                call EigenProblemSym3D ( b, eigenvalues, eigenvectors )

                                Log_Strain = 0.0d0
                                do i = 1,size(Log_Strain,1)
                                    N = 0.0d0
                                    N = Tensor_Product(eigenvectors(:,i),eigenvectors(:,i)) !Autoprojeção
                                    N = 0.50d0*( N + transpose(N) ) !simetrizando para garantir :)
                                    Log_Strain = Log_Strain + dlog((eigenvalues(i)**0.50d0))*N
                                enddo

                                Strain_Voigt = Convert_to_Voigt_3D_Sym (Log_Strain)

                                if (this%AllComponents) then
                                    call this%WriteOnFile(FEA%Time , Strain_Voigt)
                                else
                                    ProbeVariable = Strain_Voigt (this%Components)
                                    call this%WriteOnFile(FEA%Time , ProbeVariable)
                                endif



                                !---------------------------------------------------------------------------------------
                            endif


                ! Writing Deformation Gradient
                case (VariableNames%DeformationGradient)

                    NumberOfComp = size(GP%F,1)
                    NumberOfComp = NumberOfComp*NumberOfComp
                    if ( any( this%Components .gt. NumberOfComp ) ) then
                        call this%WriteOnFile('ERROR - Deformation Gradient component is greater than Max Size')
                        this%Active = .false.
                    else
                        F = GP%F
                        F_voigt = Convert_to_Voigt_3D(F)
                        if (this%AllComponents) then
                            call this%WriteOnFile(FEA%Time , F_voigt)
                        else
                            ProbeVariable = F_voigt(this%Components)
                            call this%WriteOnFile(FEA%Time , ProbeVariable)
                        endif

                    endif

                !Writing User Defined Variables
                case (VariableNames%UserDefined)

                    ! User Defined
                    !---------------------------------------------------------------------------------------
                    UD_ID = 0 !Pegar o numero de variaveis implementadas no ponto de gauss
                    call GP%GetResult( UD_ID, UD_Name, UD_Length, UD_Variable, UD_VariableType )

                    ! Loop sobre as numero de variaveis do ponto de gauss
                    !Primeiramente vamos encontrar a variável que o usuário quer
                    FoundUserVariable = .false.
                    LOOP_USER_DEFINED: do UD_ID = 1,UD_Length

                        call GP%GetResult( UD_ID, UD_Name, UD_Length, UD_Variable, UD_VariableType )

                        FoundUserVariable = Comp%CompareStrings( this%VariableName,UD_Name)

                        if (FoundUserVariable) then

                            !a variável foi encontrada. Vamos verificar se as componentes que o usuário quer são consistentes
                            !com as componentes disponíveis
                            if ( any( this%Components .gt. UD_Length) ) then
                                call this%WriteOnFile('ERROR - User defined variable component is greater than Maximum.')
                                this%Active = .false.
                                return
                            endif

                            exit LOOP_USER_DEFINED !Já encontramos a variável, então vamos sair do loop
                        endif

                    enddo LOOP_USER_DEFINED

                    IF (FoundUserVariable) then
                        !Como a variável foi encontrada, vamos exportar ela
                        if (this%AllComponents) then
                            call this%WriteOnFile(FEA%Time , UD_Variable(1:UD_Length) )
                        else
                            ProbeVariable = UD_Variable(this%Components)
                            call this%WriteOnFile(FEA%Time , ProbeVariable)
                        endif

                    else
                        call this%WriteOnFile('ERROR - User defined variable not found.')
                        this%Active = .false.
                        return
                    endif


                case default
                    stop 'Error in ModProbe - WriteProbeResult_GaussPoint - VariableNameID - not identified'

            end select

    end subroutine
    !==========================================================================================


end module



