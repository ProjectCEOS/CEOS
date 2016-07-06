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
module BoundaryConditions

    use LoadHistoryData
    use Nodes
    use Element

    type ClassNodalBC
        integer :: dof
        type (ClassLoadHistory), pointer :: LoadHistory => null()
    end type

    type ClassLineBC
        integer :: LocalDOF
        class(ClassElement) , pointer :: Element => null()
        type(ClassElementNodes) , pointer , dimension(:) :: LineNodes => null()
    end type


    type ClassFixedSupport
        integer, pointer, dimension(:) :: dof=>null()
        contains
            procedure :: FixedSupportConstructor
    end type

    type ClassBoundaryNodes
        character(len=255) :: Name=''
        integer, allocatable, dimension(:) :: Nodes
    end type


    type ClassBoundaryConditions

        type (ClassLoadHistory),    pointer,     dimension(:) :: SetOfLoadHistory => null()
        type (ClassLoadHistory)                               :: TimeInformation
        type (ClassFixedSupport)                              :: FixedSupport
        type (ClassNodalBC),        allocatable, dimension(:) :: NodalForceBC , NodalDispBC
        type (ClassBoundaryNodes),  allocatable, dimension(:) :: BoundaryNodes

        contains

            procedure :: GetNumberOfLoadCases
            procedure :: GetNumberOfSteps
            procedure :: GetExternalForces
            procedure :: GetPrescribedDisplacements
            procedure :: GetTimeInformation
            procedure :: ApplyBoundaryConditions
            procedure :: GetBoundaryConditions

    end type

    contains
!=================================================================================================
    subroutine FixedSupportConstructor (this, FSArray, NDOFnode)

        implicit none
        integer :: cont, i, j, NDOFnode
        integer, dimension(:,:) :: FSArray

        class (ClassFixedSupport) :: this

        !contando o numero de graus de liberdade prescritos
        cont = 0
        do i = 1,size(FSArray,1)
            do j = 1,NDOFnode
                cont = cont + FSArray(i,j+1)
            end do
        end do

        allocate( this%dof (cont) )

        cont = 0
        do i = 1,size(FSArray,1)
            do  j = 1,NDOFnode
                if (FSArray(i,j+1) == 1) then
                    cont = cont + 1
                    this % dof (cont) = NDOFnode * (FSArray(i,1)-1) + j
                end if
            end do
        end do

    end subroutine
!=================================================================================================




!=================================================================================================
    function GetNumberOfLoadCases(this) result(nLC)
        class(ClassBoundaryConditions) :: this
        integer::nLC
        nLC = this%TimeInformation%nLoadCases
    end function
!=================================================================================================

!=================================================================================================
    function GetNumberOfSteps(this,LoadCase) result(nST)
        class(ClassBoundaryConditions) :: this
        integer::LoadCase,nST
        nST = this%TimeInformation%LoadCase(LoadCase)%nSteps
    end function
!=================================================================================================


!=================================================================================================
    subroutine GetTimeInformation(this,LoadCase,ST,InitialTime,DeltaTime)

        class(ClassBoundaryConditions) :: this
        real(8)::InitialTime,FinalTime,DeltaTime
        integer :: LoadCase , ST

        InitialTime = this%TimeInformation%LoadCase(LoadCase)%Step(ST)%InitTime
        FinalTime   = this%TimeInformation%LoadCase(LoadCase)%Step(ST)%FinalTime
        DeltaTime   = FinalTime - InitialTime

    end subroutine
!=================================================================================================

!=================================================================================================
    subroutine ApplyBoundaryConditions(this, Kg , R , Presc_Disp_DOF , Ubar , U )

        !************************************************************************************
        ! DECLARATIONS OF VARIABLES
        !************************************************************************************
        ! Modules and implicit declarations
        ! -----------------------------------------------------------------------------------
        use GlobalSparseMatrix
        implicit none

        ! Input variables
        ! -----------------------------------------------------------------------------------
        class(ClassBoundaryConditions)  :: this
        integer , dimension(:) , intent(in) :: Presc_Disp_DOF

        ! Input/Output variables
        ! -----------------------------------------------------------------------------------
        real(8) , dimension(:) , intent(inout) :: R , Ubar , U
        type(ClassGlobalSparseMatrix) :: Kg

        ! Internal variables
        ! -----------------------------------------------------------------------------------
        integer :: i , n , dof
        real(8) :: penaliza
        real(8) , allocatable, dimension(:) ::  Udirichlet, Rmod

        !************************************************************************************

        !************************************************************************************
        ! APPLYING BOUNDARY CONDITIONS
        !************************************************************************************

        allocate( Udirichlet(size(U)), Rmod(size(U)) )
        Udirichlet = 0.0d0
        Rmod = 0.0d0



        ! Applying prescribed boundary conditions
        if ( size(Presc_Disp_DOF) .ne. 0 ) then

            ! Loop over the prescribed degrees of freedom
             do n=1,size(Presc_Disp_DOF)
                dof=Presc_Disp_DOF(n)
                ! Assembly the Dirichlet displacement BC
                Udirichlet(dof) = ( Ubar(dof) - U(dof) )
            enddo

            ! Multiplicação esparça - Vetor Força para montagem da condição de contorno de rearranjo
            call mkl_dcsrgemv('N', size(U), Kg%Val, Kg%RowMap, Kg%Col, Udirichlet, Rmod)

            !Resíduo Modificado
            R = R - Rmod

            ! Loop over the prescribed degrees of freedom
             do n=1,size(Presc_Disp_DOF)

                dof=Presc_Disp_DOF(n)
                ! Applying the BC in the stiffness matrix - Rearranjo!
                do i=1,size(Kg%Row)
                    if ((Kg%Row(i)==dof).or.(Kg%Col(i)==dof)) then
                        Kg%Val(i)=0.0d0
                    endif
                    if ((Kg%Row(i)==dof).and.(Kg%Col(i)==dof)) then
                        Kg%Val(i)=1.0d0
                    endif
                enddo

                R(dof) = Udirichlet(dof)

            enddo

        end if


        ! Applying homogeneous boundary conditions (fixed supports)
        if ( size(this%FixedSupport%dof) .ne. 0 ) then

            ! Loop over the prescribed degrees of freedom
             do n=1,size(this%FixedSupport%dof)
                dof=this%FixedSupport%dof(n)

                ! Applying the BC in the stiffness matrix
                do i=1,size(Kg%Row)
                    if ((Kg%Row(i)==dof).or.(Kg%Col(i)==dof)) then
                        Kg%Val(i)=0.0d0
                    endif
                    if ((Kg%Row(i)==dof).and.(Kg%Col(i)==dof)) then
                        Kg%Val(i)=1.0d0
                    endif
                enddo

                ! Applying the BC in the residual vector
                R(dof)=0.0d0

            enddo

        end if

        !************************************************************************************

    end subroutine
!=================================================================================================

!=================================================================================================
    subroutine GetBoundaryConditions( this, AnalysisSettings, LC, ST, Fext, DeltaFext, NodalDispDOF, U, DeltaUPresc )

        !************************************************************************************
        ! DECLARATIONS OF VARIABLES
        !************************************************************************************
        ! Modules and implicit declarations
        ! -----------------------------------------------------------------------------------
        use Analysis

        implicit none

        ! Input variables
        ! -----------------------------------------------------------------------------------
        class(ClassBoundaryConditions) :: this
        class(ClassAnalysis)           :: AnalysisSettings
        integer                        :: LC, ST

        ! Output variables
        ! -----------------------------------------------------------------------------------
        real(8) , dimension(:)               :: Fext , DeltaFext
        real(8) , dimension(:)               :: U, DeltaUPresc
        integer , pointer , dimension(:)     :: NodalDispDOF

        !************************************************************************************

        !************************************************************************************
    
        call this%GetExternalForces(LC, ST, Fext, DeltaFext)

        call this%GetPrescribedDisplacements(LC , ST, NodalDispDOF, U, DeltaUPresc)

        !************************************************************************************

    end subroutine
!=================================================================================================

!=================================================================================================
    subroutine GetExternalForces( this, LC, ST, Fext, DeltaFext )

        !************************************************************************************
        ! DECLARATIONS OF VARIABLES
        !************************************************************************************
        ! Modules and implicit declarations
        ! -----------------------------------------------------------------------------------


        implicit none

        ! Input variables
        ! -----------------------------------------------------------------------------------
        class(ClassBoundaryConditions) :: this
        integer                       , intent(in) :: LC, ST

        ! Output variables
        ! -----------------------------------------------------------------------------------
        real(8) , dimension(:) , intent(out)  :: Fext , DeltaFext

        ! Internal variables
        ! -----------------------------------------------------------------------------------
        real(8) , allocatable, dimension(:) :: InitialValue , FinalValue

        !************************************************************************************

        !************************************************************************************
        ! ASSEMBLING THE EXTERNAL FORCE AND ITS INCREMENT
        !************************************************************************************

        Fext=0.0d0
        allocate( InitialValue(size(Fext)) , FinalValue(size(Fext)) )
        InitialValue=0.0d0 ; FinalValue=0.0d0

        call AssembleNodalExternalForce( this%NodalForceBC , LC, ST , InitialValue , FinalValue )
        !call AssembleLineExternalForce( BC % LineForceBC , LC , InitialValue , FinalValue )

        Fext = InitialValue

        DeltaFext = FinalValue - InitialValue

        !************************************************************************************

    end subroutine
!=================================================================================================

!=================================================================================================
    subroutine GetPrescribedDisplacements ( this , LC , ST, NodalDispDOF, U, DeltaUPresc )

        !************************************************************************************
        ! DECLARATIONS OF VARIABLES
        !************************************************************************************
        ! Modules and implicit declarations
        ! -----------------------------------------------------------------------------------


        implicit none

        ! Input variables
        ! -----------------------------------------------------------------------------------
        class(ClassBoundaryConditions)  :: this
        integer                        :: LC, ST

        ! Output variables
        ! -----------------------------------------------------------------------------------
        real(8) , dimension(:)              :: U, DeltaUPresc
        integer , pointer , dimension(:)    :: NodalDispDOF

        ! Internal variables
        ! -----------------------------------------------------------------------------------
        real(8) , pointer, dimension(:) :: InitialValue , FinalValue
        integer                         :: i

        !************************************************************************************

        !************************************************************************************
        ! ASSEMBLING THE PRESCRIBED DISPLACEMENT AND ITS INCREMENT
        !************************************************************************************

        ! Prescribed displacement
        call GetActiveDOFNodal( this%NodalDispBC  , LC, ST , NodalDispDOF  , InitialValue,  &
                                FinalValue )

        DeltaUPresc=0.0d0
        do i = 1, size(NodalDispDOF)
            U( NodalDispDOF(i) ) = InitialValue(i)
            DeltaUPresc( NodalDispDOF(i) ) =  FinalValue(i) - InitialValue(i)
        enddo

        !************************************************************************************

    end subroutine
!=================================================================================================










! Rotinas que não são métodos de alguma classe.


!=================================================================================================
    subroutine CreateNodalBoundaryCondition( SwitchDOF , TableDOF , SetOfLoadHistory , NodalBC )

        implicit none
        integer,dimension(:,:)::SwitchDOF
        character*100,dimension(:,:)::TableDOF
        !type(ClassTable),dimension(:)::LoadCaseTables
        type (ClassLoadHistory)   ,dimension(:), pointer    :: SetOfLoadHistory
        type(ClassNodalBC),dimension(:),allocatable::NodalBC

        integer::nBC,i , j , k , NDOFnode

        !contar quantos graus de liberdade sao efetivamente prescritos
        nBC=0
        do i=1,size(SwitchDOF,1)
            nBC = nBC + sum( SwitchDOF(i,2:) )
        enddo

        allocate(NodalBC(nBC))

        NDOFnode = size(SwitchDOF,2) - 1

        k=0
        do i=1,size(SwitchDOF,1)
            do j=1,NDOFnode

                !se o grau de liberdade for prescrito vamos pegar a sua tabela
                if (SwitchDOF(i,1+j)==1) then
                    k=k+1
                    NodalBC(k)%dof = NDOFnode * ( SwitchDOF(i,1) - 1 ) + j !calcula o grau de liberdade prescrito
                    !call RetrieveTable( LoadCaseTables , TableDOF(i,j) , NodalBC(k)%LoadCase )
                    call RetrieveLoadHistory( SetOfLoadHistory , TableDOF(i,j) , NodalBC(k)%LoadHistory )
                endif

            enddo
        enddo


    end subroutine
!=================================================================================================

!=================================================================================================
    subroutine GetActiveDOFNodal( NodalBC , LC, ST , ActiveDOF , ActiveInitialValue, ActiveFinalValue )

        implicit none
        integer :: LC, ST
        integer       , pointer , dimension(:) :: ActiveDOF
        real(8)       , pointer , dimension(:) :: ActiveInitialValue, ActiveFinalValue
        type(ClassNodalBC) ,      dimension(:) :: NodalBC

        integer :: nActive , i , j , k

        if (associated(ActiveDOF))          deallocate(ActiveDOF)
        if (associated(ActiveInitialValue)) deallocate(ActiveInitialValue)
        if (associated(ActiveFinalValue))   deallocate(ActiveFinalValue)
        !=================================================================================================

        !CONTANDO QUANTAS CONDIÇÕES ATIVAS
        nActive=0
        do i=1,size(NodalBC)
            !if ( NodalBC(i)%LoadCase(LC)%active ) then
             if (NodalBC(i)%LoadHistory%LoadCase(LC)%Step(ST)%active) then
                nActive=nActive+1
            endif
        enddo

        Allocate( ActiveDOF(nActive) , ActiveInitialValue(nActive) , ActiveFinalValue(nActive) )

        !CRIAÇÃO DO VETOR E MONTAGEM DAS CONDIÇÕES DOS GRAUS DE LIBERDADE UTILIZADOS
        k=0
        do i=1,size(NodalBC)
            !if ( NodalBC(i)%LoadCase(LC)%active ) then
             if ( NodalBC(i)%LoadHistory%LoadCase(LC)%Step(ST)%active ) then
                k=k+1
                ActiveDOF(k)          = NodalBC(i)%dof
                ActiveInitialValue(k) = NodalBC(i)%LoadHistory%LoadCase(LC)%Step(ST)%InitVal
                ActiveFinalValue(k)   = NodalBC(i)%LoadHistory%LoadCase(LC)%Step(ST)%FinalVal

            endif
        enddo

    end subroutine
!=================================================================================================

!=================================================================================================
    subroutine AssembleNodalExternalForce( NodalForceBC , LC, ST, InitialValue, FinalValue )

        implicit none
        integer                             :: LC, ST
        real(8), dimension(:)               :: InitialValue, FinalValue
        type(ClassNodalBC), dimension(:)    :: NodalForceBC

        integer :: i

        do i=1,size(NodalForceBC)

            !if ( NodalForceBC(i)%LoadCase(LC)%active ) then
            if ( NodalForceBC(i)%LoadHistory%LoadCase(LC)%Step(ST)%active) then
                InitialValue( NodalForceBC(i)%dof ) = InitialValue( NodalForceBC(i)%dof ) + NodalForceBC(i)%LoadHistory%LoadCase(LC)%Step(ST)%InitVal
                FinalValue  ( NodalForceBC(i)%dof ) = FinalValue( NodalForceBC(i)%dof )   + NodalForceBC(i)%LoadHistory%LoadCase(LC)%Step(ST)%FinalVal
            endif

        enddo

    end subroutine
!=================================================================================================

!================================================================================================
    ! subroutine AssembleLineExternalForce( LineForceBC , LC , nDOFnode , InitialValue, FinalValue )
    !    implicit none
    !    integer :: LC  , nDOFnode
    !    real(8)       , dimension(:) :: InitialValue, FinalValue
    !    type(ClassLineBC) ,      dimension(:) :: LineForceBC
    !
    !    real(8) , dimension(:) , allocatable :: InitialNodalForce , FinalNodalForce
    !
    !    integer :: i,n,DOF,node
    !
    !    do i=1,size(LineForceBC)
    !        if ( LineForceBC(i)%LoadCase(LC)%active ) then
    !
    !            allocate( InitialNodalForce( size(LineForceBC(i)%LineNodes) ) , FinalNodalForce( size(LineForceBC(i)%LineNodes) ) )
    !
    !            call LineForceBC(i)%Element%IntegrateLine( LineForceBC(i)%LineNodes , LineForceBC(i)%LoadCase(LC)%InitVal , InitialNodalForce )
    !            call LineForceBC(i)%Element%IntegrateLine( LineForceBC(i)%LineNodes , LineForceBC(i)%LoadCase(LC)%FinalVal , FinalNodalForce )
    !
    !            do n=1,size(LineForceBC(i)%LineNodes)
    !                node = LineForceBC(i)%LineNodes(n)%Node%ID
    !                DOF = nDOFnode * (node-1) + LineForceBC(i)%LocalDOF
    !                InitialValue(DOF) = InitialValue(DOF) + InitialNodalForce(n)
    !                FinalValue(DOF) = FinalValue(DOF) + FinalNodalForce(n)
    !            enddo
    !
    !            deallocate(InitialNodalForce,FinalNodalForce)
    !
    !
    !        endif
    !    enddo
    !
    !end subroutine
!=================================================================================================




end module
