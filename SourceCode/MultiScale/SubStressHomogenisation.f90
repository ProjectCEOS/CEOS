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
subroutine StressHomogenisation( ElementList , AnalysisSettings, HomogenizedStress )

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
    type (ClassElementsWrapper) , dimension(:)  :: ElementList
    type(ClassAnalysis)                         :: AnalysisSettings

    ! Input/Output variables
    ! -----------------------------------------------------------------------------------
    real(8) , dimension(:) :: HomogenizedStress

    ! Internal variables
    ! -----------------------------------------------------------------------------------
    integer							    :: NDOFel , gp
    real(8)							    :: detJ, TotalVol
    real(8) , pointer , dimension(:)    :: Weight , Cauchy
    real(8) , pointer , dimension(:,:)  :: NaturalCoord
    real(8) , pointer , dimension(:,:)  :: B , G
    real(8)                             :: FactorAxi
    !************************************************************************************

    !************************************************************************************
    ! STRESS HOMOGENISATION
    !************************************************************************************
    HomogenizedStress = 0.0d0

    TotalVol = 0.0d0
    !Loop over Elements
    do e = 1,size(ElementList)

        TotalVol = TotalVol + ElementList(e)%El%Volume

    enddo

    !Loop over Elements
    do e = 1,size(ElementList)

        ! Number of degrees of freedom
        call ElementList(e)%El%GetElementNumberDOF(AnalysisSettings,NDOFel)

        ! Allocating matrix B
        B => B_Memory(  1:AnalysisSettings%BrowSize , 1:NDOFel )

        ! Allocating matrix G
        G => G_Memory(  1:AnalysisSettings%GrowSize , 1:NDOFel )

        ! Allocating memory for the Cauchy Stress (Plain States, Axisymmetric or 3D)
        Cauchy => Stress_Memory( 1:AnalysisSettings%StressSize )

        ! Retrieving gauss points parameters for numerical integration
        call ElementList(e)%El%GetGaussPoints(NaturalCoord,Weight)

        !Loop over gauss points
        do gp = 1, size(NaturalCoord,dim=1)

            !Get Cauchy Stress
            Cauchy => ElementList(e)%El%GaussPoints(gp)%Stress

            !Get matrix B and the Jacobian determinant
            call ElementList(e)%El%Matrix_B_and_G(AnalysisSettings, NaturalCoord(gp,:) , B, G, detJ , FactorAxi)

            !Homogenized Stress
             HomogenizedStress = HomogenizedStress + (Cauchy*Weight(gp)*detJ*FactorAxi)/TotalVol

        enddo

    enddo


    !************************************************************************************


end subroutine


