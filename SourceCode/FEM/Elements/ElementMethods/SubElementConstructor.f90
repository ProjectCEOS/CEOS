!##################################################################################################
! This routine constructs the element.
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
subroutine ElementConstructor( this, ElementNodes, ElementType, GlobalNodesList, Material, &
                               AnalysisSettings )

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

	! Object
	! -----------------------------------------------------------------------------------
	class(ClassElement) , pointer :: this

	! Input variables
	! -----------------------------------------------------------------------------------
	type(ClassAnalysis)                       , intent(in) :: AnalysisSettings
	type(ClassNodes) , dimension(:) , pointer , intent(in) :: GlobalNodesList

	integer				 , intent(in) :: ElementType
	integer,dimension(:) , intent(in) :: ElementNodes
	class(ClassConstitutiveModelWrapper)  , pointer :: Material

	! Internal variables
	! -----------------------------------------------------------------------------------
	integer :: i, nNodes, nGP, gp

	!************************************************************************************


	!************************************************************************************
	! CONSTRUCT THE ELEMENT
	!************************************************************************************

	call AllocateNewElement( this , ElementType )

	nNodes = this%GetNumberOfNodes()

	allocate( this%ElementNodes(nNodes) )

	do i=1,nNodes
		this%ElementNodes(i)%Node => GlobalNodesList( ElementNodes(i) )
	enddo

	call this%AllocateGaussPoints(nGP)

    if ( ( nGP <= 0 ) .or. (nGP > 1000) ) then
        call Error ("Error: Number of the Gauss Points <=0 or >1000")
    endif

    ! Allocate the constitutive model for all element's Gauss point
    ! -----------------------------------------------------------------------------------
	call AllocateConstitutiveModel( Material%ModelEnumerator , AnalysisSettings , nGP ,  this%GaussPoints )

    ! Copy material properties from reference material (read in the settings file) to
    ! Gauss points
    ! -----------------------------------------------------------------------------------
	do gp = 1,nGP
        call this%GaussPoints(gp)%CopyProperties(Material%Mat(1))
    enddo

    ! Construct the Constitutive Model
    ! -----------------------------------------------------------------------------------
    do gp=1,nGP

        allocate( this%GaussPoints(gp)%Stress( AnalysisSettings%StressSize ) )

        this%GaussPoints(gp)%Stress = 0.0d0

        call this%GaussPoints(gp)%ConstitutiveModelConstructor(AnalysisSettings)

    enddo

	!************************************************************************************



end subroutine
