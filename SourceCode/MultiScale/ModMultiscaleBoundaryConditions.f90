!##################################################################################################
! This module has the attributes and methods to select the parameters of the analysis type chosen.
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
module ModMultiscaleBoundaryConditions

    use LoadHistoryData
    use Nodes
    use Element

    !-----------------------------------------------------------------------------------
    type ClassMultiscaleBCType
        integer :: Taylor=1, Linear=2, Periodic=3
    end type
    type(ClassMultiscaleBCType), parameter :: MultiscaleBCType = ClassMultiscaleBCType()
    !-----------------------------------------------------------------------------------

    type ClassMacroDefGradCurve
        type (ClassLoadHistory), pointer :: LoadHistory => null()
    end type

    type ClassMultiscaleNodalBC
        type(ClassNodes), pointer :: Node
        type (ClassMacroDefGradCurve), dimension (3,3) :: Fmacro
    end type



    type, extends(ClassBoundaryConditions) :: ClassMultiscaleBoundaryConditions

        integer :: TypeOfBC
        type (ClassMultiscaleNodalBC), allocatable, dimension(:) :: NodalMultiscaleDispBC

    end type




    type, extends(ClassMultiscaleBoundaryConditions) :: ClassMultiscaleBoundaryConditionsTaylorAndLinear

        contains
            procedure :: GetBoundaryConditions => GetBoundaryConditionsMultiscaleTaylorAndLinear
            ! A rotina de aplicação de contorno para Taylor e Linear é a mesma de FEM

    end type



    type, extends(ClassMultiscaleBoundaryConditions) :: ClassMultiscaleBoundaryConditionsPeriodic

        contains
            procedure :: ApplyBoundaryConditions => ApplyBoundaryConditionsMultiscalePeriodic
            procedure :: GetBoundaryConditions => GetBoundaryConditionsMultiscalePeriodic

    end type



    contains


    !=================================================================================================
    subroutine GetBoundaryConditionsMultiscaleTaylorAndLinear( this, AnalysisSettings, LC, ST, Fext, DeltaFext, ActiveDOF, U, DeltaUPresc )

        !************************************************************************************
        ! DECLARATIONS OF VARIABLES
        !************************************************************************************
        ! Modules and implicit declarations
        ! -----------------------------------------------------------------------------------
        use Analysis
        use MathRoutines

        implicit none

        ! Input variables
        ! -----------------------------------------------------------------------------------
        class(ClassMultiscaleBoundaryConditions) :: this
        class(ClassAnalysis)                     :: AnalysisSettings
        integer                                  :: LC, ST

        ! Output variables
        ! -----------------------------------------------------------------------------------
        real(8) , dimension(:)               :: Fext , DeltaFext
        real(8) , dimension(:)               :: U, DeltaUPresc
        integer , pointer , dimension(:)     :: ActiveDOF

        ! Internal variables
        ! -----------------------------------------------------------------------------------
        integer                                :: i,j,k
        real(8), allocatable, dimension(:) :: ActiveInitialValue, ActiveFinalValue
        real(8) :: FMacroInitial(3,3), FMacroFinal(3,3), Y(3), UmicroYInitial(3),UmicroYFinal(3)

        !************************************************************************************

        !************************************************************************************
        Fext = 0.0d0
        DeltaFext = 0.0d0

        if (associated(ActiveDOF))          deallocate(ActiveDOF)


        !CONTANDO QUANTAS CONDIÇÕES ATIVAS (número total de graus de liberdade com deslocamento prescrito)
        nActive = size(this%NodalMultiscaleDispBC)*AnalysisSettings%NDOFnode

        Allocate( ActiveDOF(nActive) , ActiveInitialValue(nActive) , ActiveFinalValue(nActive) )



        !CRIAÇÃO DO VETOR E MONTAGEM DAS CONDIÇÕES DOS GRAUS DE LIBERDADE UTILIZADOS
        do k=1,size(this%NodalMultiscaleDispBC)

            ! Montando FMacro no tempo t baseado na curva informada pelo usuário
            do i = 1,3
                do j = 1,3
                FMacroInitial(i,j) = this%NodalMultiscaleDispBC(k)%Fmacro(i,j)%LoadHistory%LoadCase(LC)%Step(ST)%InitVal
                FMacroFinal(i,j)   = this%NodalMultiscaleDispBC(k)%Fmacro(i,j)%LoadHistory%LoadCase(LC)%Step(ST)%FinalVal
                enddo
            enddo

            ! Obter a coordenada do nó onde se será aplicada a condição de contorno prescrita
            Y = 0.0d0
            Y(1:size(this%NodalMultiscaleDispBC(k)%Node%CoordX)) = this%NodalMultiscaleDispBC(k)%Node%CoordX

            ! Calcular os deslocamento microscópico na coordenada Y
            UmicroYInitial = matmul((FMacroInitial - IdentityMatrix(3)),Y)
            UmicroYFinal = matmul((FMacroFinal - IdentityMatrix(3)),Y)

            ! Montando os deslocamentos micro prescritos nos graus de liberdade (analise mecânica)
            do i = 1,AnalysisSettings%NDOFnode
                j = AnalysisSettings%NDOFnode*(k -1 ) + i
                ActiveDOF(j) = AnalysisSettings%NDOFnode*(this%NodalMultiscaleDispBC(k)%Node%ID -1 ) + i
                ActiveInitialValue(j) = UmicroYInitial(i)
                ActiveFinalValue(j)   = UmicroYFinal(i)
            enddo
        enddo


        DeltaUPresc=0.0d0
        do i = 1, size(NodalDispDOF)
            U( ActiveDOF(i) ) = ActiveInitialValue(i)
            DeltaUPresc( ActiveDOF(i) ) =  ActiveFinalValue(i) - ActiveInitialValue(i)
        enddo


        !************************************************************************************

    end subroutine
    !=================================================================================================







end module
