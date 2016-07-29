
!##################################################################################################
!
! Module that contains the routines to interface UMAT and CEOS
!
!##################################################################################################
Module ModInterface_UMAT_CEOS


    contains


        !==========================================================================================
        Subroutine TransformStressAndModulusFromCEOStoUMAT( CauchyStress, TangentModulus )

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use MathRoutines
            implicit none



            ! Input/Output variables
            ! -----------------------------------------------------------------------------------
            real(8), dimension(:)   :: CauchyStress
            real(8), dimension(:,:) :: TangentModulus

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: Id(3,3), C(6,6)

            !************************************************************************************

            !************************************************************************************
            ! Compute Stress and Tangent Modulus used by Abaqus-UMAT
            !************************************************************************************
            ! Identity
            Id = 0.0d0
            Id(1,1) = 1.0d0
            Id(2,2) = 1.0d0
            Id(3,3) = 1.0d0


            ! Computing UMAT Tangent Modulus.
            ! -----------------------------------------------------------------------------------
            ! See Eq.(51) of:
            !   - Nguyen, N., Waas, A. M., "Nonlinear, finite deformation, finite element analysis", 2016,
            !     DOI 10.1007/s00033-016-0623-5.
            !
            ! D(i,j,k,l) = Cx(i,j,k,l) + 0.50d0*( S(i,k)*Id(j,l) + S(j,l)*Id(i,k) + S(i,l)*Id(j,k) + S(j,k)*Id(i,l) )
            !
            !   - D  = Tangent Modulus used by Abaqus/UMAT
            !   - Cx = Spatial Tangent Modulus
            !   - S  = Cauchy Stress
            !   - Id = Identity
            ! -----------------------------------------------------------------------------------
              C = 0.0d0
              C(1,1) = 0.20D1 * CauchyStress(1)
              C(1,4) = 0.10D1 * CauchyStress(4)
              C(1,6) = 0.10D1 * CauchyStress(6)
              C(2,2) = 0.20D1 * CauchyStress(2)
              C(2,4) = 0.10D1 * CauchyStress(4)
              C(2,5) = 0.10D1 * CauchyStress(5)
              C(3,3) = 0.20D1 * CauchyStress(3)
              C(3,5) = 0.10D1 * CauchyStress(5)
              C(3,6) = 0.10D1 * CauchyStress(6)
              C(4,1) = 0.10D1 * CauchyStress(4)
              C(4,2) = 0.10D1 * CauchyStress(4)
              C(4,4) = 0.50D0 * CauchyStress(1) + 0.50D0 * CauchyStress(2)
              C(4,5) = 0.50D0 * CauchyStress(6)
              C(4,6) = 0.50D0 * CauchyStress(5)
              C(5,2) = 0.10D1 * CauchyStress(5)
              C(5,3) = 0.10D1 * CauchyStress(5)
              C(5,4) = 0.50D0 * CauchyStress(6)
              C(5,5) = 0.50D0 * CauchyStress(2) + 0.50D0 * CauchyStress(3)
              C(5,6) = 0.50D0 * CauchyStress(4)
              C(6,1) = 0.10D1 * CauchyStress(6)
              C(6,3) = 0.10D1 * CauchyStress(6)
              C(6,4) = 0.50D0 * CauchyStress(5)
              C(6,5) = 0.50D0 * CauchyStress(4)
              C(6,6) = 0.50D0 * CauchyStress(1) + 0.50D0 * CauchyStress(3)

            ! Abaqus/UMAT Tangent Modulus
            TangentModulus = TangentModulus + C
            ! -----------------------------------------------------------------------------------

            ! Correcting Stress and Modulus:
            !   - CEOS Stress Voigt Notation - [ 11 22 33 12 23 13 ]
            !   - UMAT Stress Voigt Notation - [ 11 22 33 12 13 23 ]
            ! -----------------------------------------------------------------------------------
            CauchyStress   = ConvertT2VoigtSymToABAQUS( CauchyStress )
            TangentModulus = ConvertT4VoigtSymToABAQUS( TangentModulus )
            ! -----------------------------------------------------------------------------------

            !************************************************************************************

        end subroutine
        !==========================================================================================




end module
!##################################################################################################

