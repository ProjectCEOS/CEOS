!##################################################################################################
! This routine solves the constitutive equations.
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
subroutine SolveConstitutiveModel( ElementList , AnalysisSettings, Time, U)

    !************************************************************************************
    ! DECLARATIONS OF VARIABLES
    !************************************************************************************
    ! Modules and implicit declarations
    ! -----------------------------------------------------------------------------------
    use ElementLibrary
    use Analysis
    use ConstitutiveModel


    implicit none

    ! Input variables
    ! -----------------------------------------------------------------------------------
    type(ClassElementsWrapper) , dimension(:)  :: ElementList
    type(ClassAnalysis)                        :: AnalysisSettings
    real(8)                    , dimension(:)  :: U
    real(8)                                    :: Time

    ! Internal variables
    ! -----------------------------------------------------------------------------------
    real(8) :: F(3,3)
    real(8) :: Volume, VolumeX, T
    integer :: e , gp , nDOFel
    integer , pointer , dimension(:)   :: GM
    real(8) , pointer , dimension(:,:) :: NaturalCoord
    real(8) , pointer , dimension(:)   :: Weight

    !************************************************************************************

    !************************************************************************************
    ! SOLVING THE GLOBAL CONSTITUTIVE MODEL
    !************************************************************************************


    ! Loop over the elements
    do e = 1 , size(ElementList)

        call ElementList(e)%El%GetElementNumberDOF(AnalysisSettings , nDOFel)
        GM => GM_Memory( 1:nDOFel )

        call ElementList(e)%El%GetGaussPoints(NaturalCoord,Weight)

        call ElementList(e)%El%GetGlobalMapping(AnalysisSettings,GM)

        call ElementList(e)%El%ElementVolume(AnalysisSettings,Volume,VolumeX)

        ! Armazendo o volume de cada elemento
        ElementList(e)%El%Volume  = Volume
        ElementList(e)%El%VolumeX = VolumeX

        ! Loop over the Gauss Points
        do gp = 1 , size(ElementList(e)%El%GaussPoints)

            call ElementList(e)%El%DeformationGradient( NaturalCoord(gp,:) , U(GM) , &
                                                        AnalysisSettings , F )

            ElementList(e)%El%GaussPoints(gp)%F = F

            ElementList(e)%El%GaussPoints(gp)%Jbar = Volume/VolumeX

            ElementList(e)%El%GaussPoints(gp)%Time = Time

            call ElementList(e)%El%GaussPoints(gp)%UpdateStressAndStateVariables

        enddo

    enddo




end subroutine


