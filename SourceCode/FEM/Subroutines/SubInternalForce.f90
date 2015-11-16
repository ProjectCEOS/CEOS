!##################################################################################################
! This routine assembles the nodal contributions of the global internal force.
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
subroutine InternalForce( ElementList, AnalysisSettings, Fint )

    !************************************************************************************
    ! DECLARATIONS OF VARIABLES
    !************************************************************************************
    ! Modules and implicit declarations
    ! -----------------------------------------------------------------------------------
    use Analysis
    use ElementLibrary

    implicit none

    ! Input variables
    ! -----------------------------------------------------------------------------------
    type(ClassElementsWrapper) , dimension(:) , intent(in) :: ElementList
    type(ClassAnalysis)                       , intent(inout) :: AnalysisSettings

    ! Output variables
    ! -----------------------------------------------------------------------------------
    real(8) , dimension(:) , intent(out) :: Fint

    ! Internal variables
    ! -----------------------------------------------------------------------------------
    integer :: e , nDOFel
    integer , pointer , dimension(:) :: GM
    real(8) , pointer , dimension(:) :: Fe

    !************************************************************************************

    !************************************************************************************
    ! ASSEMBLING THE INTERNAL FORCE
    !************************************************************************************

    Fint=0.0d0

     do  e = 1, size( ElementList )

        call ElementList(e)%El%GetElementNumberDOF(AnalysisSettings , nDOFel)

        Fe => Fe_Memory( 1:nDOFel )
        GM => GM_Memory( 1:nDOFel )

        call ElementList(e)%El%GetGlobalMapping(AnalysisSettings,GM)

        call ElementList(e)%El%ElementInternalForce(AnalysisSettings,Fe)

        Fint(GM) = Fint(GM) + Fe

    enddo

    !************************************************************************************

end subroutine

