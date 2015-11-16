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
subroutine CentroidCorrection( ElementList , AnalysisSettings, GlobalNodesList )

! TODO (Thiago#1#04/02/15): colocar a correção do centroide na leitura da malha
    !************************************************************************************
    ! DECLARATIONS OF VARIABLES
    !************************************************************************************
    ! Modules and implicit declarations
    ! -----------------------------------------------------------------------------------
    use Analysis
    use ElementLibrary
    use Nodes

    implicit none

    ! Input variables
    ! -----------------------------------------------------------------------------------
    type (ClassElementsWrapper) , dimension(:)  :: ElementList
    type (ClassAnalysis)                        :: AnalysisSettings
    type (ClassNodes) , dimension(:)            :: GlobalNodesList

    ! Internal variables
    ! -----------------------------------------------------------------------------------
    integer							    :: NDOFel , gp
    real(8)							    :: detJ, TotalVol
    real(8) , pointer , dimension(:)    :: Weight , Cauchy
    real(8) , pointer , dimension(:,:)  :: NaturalCoord
    real(8) , pointer , dimension(:,:)  :: B , G
    real(8)                             :: FactorAxi, Volume, VolumeX
    real(8), allocatable, dimension(:)  :: Centroid, Y
    !************************************************************************************

    !************************************************************************************
    ! CENTROID CORRECTION
    !************************************************************************************

    allocate ( Centroid(AnalysisSettings%AnalysisDimension), Y(AnalysisSettings%AnalysisDimension) )



    TotalVol = 0.0d0
    !Loop over Elements
    do e = 1,size(ElementList)

        call ElementList(e)%El%ElementVolume(AnalysisSettings, Volume, VolumeX)

        TotalVol = TotalVol + VolumeX

    enddo



    Centroid = 0.0d0
    Y = 0.0d0

    !Loop over Elements
    do e = 1,size(ElementList)

        ! Number of degrees of freedom
        call ElementList(e)%El%GetElementNumberDOF(AnalysisSettings,NDOFel)

        ! Allocating matrix B
        B => B_Memory(  1:AnalysisSettings%BrowSize , 1:NDOFel )

        ! Allocating matrix G
        G => G_Memory(  1:AnalysisSettings%GrowSize , 1:NDOFel )

        ! Retrieving gauss points parameters for numerical integration
        call ElementList(e)%El%GetGaussPoints(NaturalCoord,Weight)

        !Loop over gauss points
        do gp = 1, size(NaturalCoord,dim=1)

            !Get matrix B and the Jacobian determinant
            call ElementList(e)%El%Matrix_B_and_G(AnalysisSettings, NaturalCoord(gp,:) , B, G, detJ , FactorAxi)

            do i = 1,AnalysisSettings%AnalysisDimension
                call ElementList(e)%El%ElementInterpolation( [( ElementList(e)%El%ElementNodes(n)%Node%Coord(i),n=1,nNodes )], &
                                                             NaturalCoord(gp,:), Y(i) )
            enddo

            !Homogenized Stress
             Centroid = Centroid + Y*Weight(gp)*detJ*FactorAxi/TotalVol

        enddo

    enddo


    !Translate the centroid to origin
    do i = 1,size(GlobalNodesList)

        GlobalNodesList(i)%CoordX = GlobalNodesList(i)%CoordX - Centroid
        GlobalNodesList(i)%Coord  = GlobalNodesList(i)%Coord  - Centroid

    enddo


    !************************************************************************************


end subroutine


