module modFEMSystemOfEquations
    
    use ModNonLinearSystemOfEquations
    use Analysis
    use BoundaryConditions
    use ElementLibrary
    use GlobalSparseMatrix

    implicit none

    type , extends(ClassNonLinearSystemOfEquations) :: ClassFEMSystemOfEquations

        real(8),dimension(:),allocatable                       :: Fint , Fext , UBar
        real (8)                                               :: Time
        integer                      , dimension(:) , pointer  :: DispDOF

        type (ClassElementsWrapper)  , dimension(:) , pointer  :: ElementList
        type (ClassNodes)            , dimension(:) , pointer  :: GlobalNodesList
        type (ClassAnalysis)                                   :: AnalysisSettings
        type (ClassBoundaryConditions)              , pointer  :: BC
        type (ClassGlobalSparseMatrix)              , pointer  :: Kg


    contains

        procedure :: EvaluateSystem => EvaluateR
        procedure :: EvaluateGradientSparse => EvaluateKt
        procedure :: RotinaExtra => FEMRotinaExtra

    end type

    contains
!--------------------------------------------------------------------------------------------------
    subroutine EvaluateR(this,X,R)
        use Interfaces
        class(ClassFEMSystemOfEquations) :: this
        real(8),dimension(:) :: X,R

        ! Update stress and internal variables
        call SolveConstitutiveModel( this%ElementList , this%AnalysisSettings , this%Time, X)

                ! Used for Cut Back Strategy
                !if (AnalysisSettings%ErrorNumber < 0 ) then
                    !call this%Status%SetError()
                  !  return
                !endif

                ! Internal Force
        call InternalForce(this%ElementList , this%AnalysisSettings , this%Fint)

                ! Used for Cut Back Strategy
               ! if (AnalysisSettings%ErrorNumber < 0 ) then
               !     return
               ! endif

            ! Residual
            R = this%Fint - this%Fext

    end subroutine

!--------------------------------------------------------------------------------------------------

    subroutine EvaluateKt(this,X,R,G)

        use Interfaces
        class(ClassFEMSystemOfEquations)        :: this
        class (ClassGlobalSparseMatrix), pointer :: G
        real(8),dimension(:) :: X , R

        call TangentStiffnessMatrix(this%AnalysisSettings , this%ElementList , this%Kg )
        ! As CC de deslocamento prescrito estão sendo aplicadas no sistema Kx=R e não em Kx=-R!!!
        R = -R
        call this%BC%ApplyBoundaryConditions(  this%Kg , R , this%DispDOF, this%Ubar , X   )
        R = -R

        G => this%Kg

    end subroutine

!--------------------------------------------------------------------------------------------------

    subroutine FEMRotinaExtra(this,X)
        use Interfaces
        class(ClassFEMSystemOfEquations) :: this
        real(8),dimension(:)::X

        if (this%AnalysisSettings%NLAnalysis == .true.) then
            call UpdateMeshCoordinates(this%GlobalNodesList,this%AnalysisSettings,X)
        endif

    end subroutine





end module

