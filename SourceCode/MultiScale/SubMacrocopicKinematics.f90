!##################################################################################################
! This routine
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
subroutine MacroscopicKinematics( BC, LC, ST, Fmacro, DeltaFmacro, Umacro, DeltaUmacro )

    !************************************************************************************
    ! DECLARATIONS OF VARIABLES
    !************************************************************************************
    ! Modules and implicit declarations
    ! -----------------------------------------------------------------------------------
    use BoundaryConditions

    implicit none

    ! Input variables
    ! -----------------------------------------------------------------------------------
    type(ClassBoundaryConditions) :: BC
    integer                       :: LC, ST

    ! Output variables
    ! -----------------------------------------------------------------------------------
    real (8) :: Fmacro(3,3), Umacro(3), DeltaFmacro(3,3), DeltaUmacro(3)

    ! Internal variables
    ! -----------------------------------------------------------------------------------
    integer  :: i, j

    !************************************************************************************

    !************************************************************************************
    ! ASSEMBLING THE MACROSCOPIC DISPLACEMENT AND DEFORMATION GRADIENT
    !************************************************************************************

    ! Macroscopic Displacement
    do i = 1,3

        Umacro(i) = BC%HistoryOfMacroDisp(i)%LoadCase(LC)%Step(ST)%InitVal

        DeltaUmacro(i) = BC%HistoryOfMacroDisp(i)%LoadCase(LC)%Step(ST)%FinalVal - Umacro(i)

    enddo



    ! Macroscopic Deformation Gradient
    do i = 1,3
        do j=1,3

            Fmacro(i,j) = BC%HistoryOfMacroDefGrad(i,j)%LoadCase(LC)%Step(ST)%InitVal

            DeltaFmacro(i,j) = BC%HistoryOfMacroDefGrad(i,j)%LoadCase(LC)%Step(ST)%FinalVal - Fmacro(i,j)

        enddo
    enddo


    !************************************************************************************

end subroutine

