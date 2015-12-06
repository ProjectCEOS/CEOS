!##################################################################################################
!                                   VOIGT NOTATION MODULE
!
! Tensorial Operations in Euclidean Space 3D using Voigt Notation
!--------------------------------------------------------------------------------------------------
!
! Remark: Operations used in this module were implemented following
!         the mapping shown in Appendix E of Jog(2007).
!
! References:
!
!   [1] JOG, C. Foundations and Applications of Mechnics Volume I: Continuum Mechanics.
!       2 edition. ed. [S.l.]: Alpha Science Intl Ltd, 2007. I. 510 p.
!--------------------------------------------------------------------------------------------------
!
! Variables:
!--------------------------------------------------------------------------------------------------
! s                       = Scalars
! A                       = Second Order Tensors
! C                       = Forth Order Tensors
! A_voigt,B_voigt         = Second Order Tensors in Non-Symmetric Voigt Notation
! A_voigt_sym,B_voigt_sym = Second Order Tensors in Symmetric Voigt Notation
! C_voigt,D_voigt         = Forth Order Tensors in Non-Symmetric Voigt Notation
! C_voigt_sym,D_voigt_sym = Forth Order Tensors in Symmetric Voigt Notation
!--------------------------------------------------------------------------------------------------
!
! Functions:
!--------------------------------------------------------------------------------------------------
! "OperationName"Voigt    - Operations using non-symmetric mapping
! "OperationName"VoigtSym - Operations using symmetric mapping
!--------------------------------------------------------------------------------------------------
!
! Mappings:
!
!    A           = VoigtToTensor2(A_voigt)
!    A           = VoigtSymToTensor2(A_voigt_sym)
!    A_voigt     = Tensor2ToVoigt(A)
!    A_voigt_sym = Tensor2ToVoigtSym(A)
!    C_voigt     = Tensor4ToVoigt(C)
!    C_voigt_sym = Tensor4ToVoigtSym(C)
!    C_voigt_sym = IdentityT4SymVoigtSym()
!    C_voigt_sym = MaterialToSpatialModulusVoigtSym(D_voigt_sym,A)
!
!
! Operations:
!
!    C_voigt_sym = BallVoigtSym(A_voigt_sym,B_voigt_sym)
!    C_voigt_sym = SquareVoigtSym(A_voigt_sym,B_voigt_sym)
!    s           = DoubleContractionT2T2VoigtSym(A_voigt_sym,B_voigt_sym)
!    A_voigt_sym = DoubleContractionT4T2VoigtSym(C_voigt_sym,B_voigt_sym)
!    C_voigt_sym = DoubleContractionT4T4VoigtSym(C_voigt_sym,D_voigt_sym)
!
!--------------------------------------------------------------------------------------------------
!
!##################################################################################################
module ModVoigtNotation

    use ModTensorAlgebra


    contains

        !==========================================================================================
        function Tensor2ToVoigt (A) result( T )

            implicit none
            real(8),dimension(:,:)       :: A
            real(8),dimension(9)         :: T

                T(1) = A(1,1)
                T(2) = A(2,1)
                T(3) = A(3,1)
                T(4) = A(1,2)
                T(5) = A(2,2)
                T(6) = A(3,2)
                T(7) = A(1,3)
                T(8) = A(2,3)
                T(9) = A(3,3)

        end function
        !==========================================================================================

        !==========================================================================================
        function VoigtToTensor2 (T) result( A)

            implicit none
            real(8),dimension(:)         :: T
            real(8),dimension(3,3)       :: A

                A(1,1) = T(1)
                A(2,1) = T(2)
                A(3,1) = T(3)
                A(1,2) = T(4)
                A(2,2) = T(5)
                A(3,2) = T(6)
                A(1,3) = T(7)
                A(2,3) = T(8)
                A(3,3) = T(9)

        end function
        !==========================================================================================

        !==========================================================================================
        function Tensor2ToVoigtSym (A) result( T )

            implicit none
            real(8),dimension(:,:)       :: A
            real(8),dimension(6)         :: T

                T(1) = A(1,1)
                T(2) = A(2,2)
                T(3) = A(3,3)
                T(4) = A(1,2)
                T(5) = A(2,3)
                T(6) = A(1,3)

        end function
        !==========================================================================================

        !==========================================================================================
        function VoigtSymToTensor2 (T) result( A )

            implicit none
            real(8),dimension(:)         :: T
            real(8),dimension(3,3)       :: A

                A(1,1) = T(1)
                A(2,2) = T(2)
                A(3,3) = T(3)
                A(1,2) = T(4)
                A(2,1) = T(4)
                A(2,3) = T(5)
                A(3,2) = T(5)
                A(1,3) = T(6)
                A(3,1) = T(6)

        end function
        !==========================================================================================

        !==========================================================================================
        function Tensor4ToVoigt(C) result(D)

            implicit none
            real(8),dimension(3,3,3,3) :: C
            real(8),dimension(9,9)     :: D

              D(1,1) = C(1,1,1,1)
              D(1,2) = C(1,1,2,1)
              D(1,3) = C(1,1,3,1)
              D(1,4) = C(1,1,1,2)
              D(1,5) = C(1,1,2,2)
              D(1,6) = C(1,1,3,2)
              D(1,7) = C(1,1,1,3)
              D(1,8) = C(1,1,2,3)
              D(1,9) = C(1,1,3,3)
              D(2,1) = C(2,1,1,1)
              D(2,2) = C(2,1,2,1)
              D(2,3) = C(2,1,3,1)
              D(2,4) = C(2,1,1,2)
              D(2,5) = C(2,1,2,2)
              D(2,6) = C(2,1,3,2)
              D(2,7) = C(2,1,1,3)
              D(2,8) = C(2,1,2,3)
              D(2,9) = C(2,1,3,3)
              D(3,1) = C(3,1,1,1)
              D(3,2) = C(3,1,2,1)
              D(3,3) = C(3,1,3,1)
              D(3,4) = C(3,1,1,2)
              D(3,5) = C(3,1,2,2)
              D(3,6) = C(3,1,3,2)
              D(3,7) = C(3,1,1,3)
              D(3,8) = C(3,1,2,3)
              D(3,9) = C(3,1,3,3)
              D(4,1) = C(1,2,1,1)
              D(4,2) = C(1,2,2,1)
              D(4,3) = C(1,2,3,1)
              D(4,4) = C(1,2,1,2)
              D(4,5) = C(1,2,2,2)
              D(4,6) = C(1,2,3,2)
              D(4,7) = C(1,2,1,3)
              D(4,8) = C(1,2,2,3)
              D(4,9) = C(1,2,3,3)
              D(5,1) = C(2,2,1,1)
              D(5,2) = C(2,2,2,1)
              D(5,3) = C(2,2,3,1)
              D(5,4) = C(2,2,1,2)
              D(5,5) = C(2,2,2,2)
              D(5,6) = C(2,2,3,2)
              D(5,7) = C(2,2,1,3)
              D(5,8) = C(2,2,2,3)
              D(5,9) = C(2,2,3,3)
              D(6,1) = C(3,2,1,1)
              D(6,2) = C(3,2,2,1)
              D(6,3) = C(3,2,3,1)
              D(6,4) = C(3,2,1,2)
              D(6,5) = C(3,2,2,2)
              D(6,6) = C(3,2,3,2)
              D(6,7) = C(3,2,1,3)
              D(6,8) = C(3,2,2,3)
              D(6,9) = C(3,2,3,3)
              D(7,1) = C(1,3,1,1)
              D(7,2) = C(1,3,2,1)
              D(7,3) = C(1,3,3,1)
              D(7,4) = C(1,3,1,2)
              D(7,5) = C(1,3,2,2)
              D(7,6) = C(1,3,3,2)
              D(7,7) = C(1,3,1,3)
              D(7,8) = C(1,3,2,3)
              D(7,9) = C(1,3,3,3)
              D(8,1) = C(2,3,1,1)
              D(8,2) = C(2,3,2,1)
              D(8,3) = C(2,3,3,1)
              D(8,4) = C(2,3,1,2)
              D(8,5) = C(2,3,2,2)
              D(8,6) = C(2,3,3,2)
              D(8,7) = C(2,3,1,3)
              D(8,8) = C(2,3,2,3)
              D(8,9) = C(2,3,3,3)
              D(9,1) = C(3,3,1,1)
              D(9,2) = C(3,3,2,1)
              D(9,3) = C(3,3,3,1)
              D(9,4) = C(3,3,1,2)
              D(9,5) = C(3,3,2,2)
              D(9,6) = C(3,3,3,2)
              D(9,7) = C(3,3,1,3)
              D(9,8) = C(3,3,2,3)
              D(9,9) = C(3,3,3,3)

        end function
        !==========================================================================================

        !==========================================================================================
        function Tensor4ToVoigtSym(C) result(D)

            implicit none
            real(8),dimension(3,3,3,3) :: C
            real(8),dimension(6,6)     :: D

              D(1,1) = C(1,1,1,1)
              D(1,2) = C(1,1,2,2)
              D(1,3) = C(1,1,3,3)
              D(1,4) = C(1,1,1,2)
              D(1,5) = C(1,1,2,3)
              D(1,6) = C(1,1,1,3)
              D(2,1) = C(2,2,1,1)
              D(2,2) = C(2,2,2,2)
              D(2,3) = C(2,2,3,3)
              D(2,4) = C(2,2,1,2)
              D(2,5) = C(2,2,2,3)
              D(2,6) = C(2,2,1,3)
              D(3,1) = C(3,3,1,1)
              D(3,2) = C(3,3,2,2)
              D(3,3) = C(3,3,3,3)
              D(3,4) = C(3,3,1,2)
              D(3,5) = C(3,3,2,3)
              D(3,6) = C(3,3,1,3)
              D(4,1) = C(1,2,1,1)
              D(4,2) = C(1,2,2,2)
              D(4,3) = C(1,2,3,3)
              D(4,4) = C(1,2,1,2)
              D(4,5) = C(1,2,2,3)
              D(4,6) = C(1,2,1,3)
              D(5,1) = C(2,3,1,1)
              D(5,2) = C(2,3,2,2)
              D(5,3) = C(2,3,3,3)
              D(5,4) = C(2,3,1,2)
              D(5,5) = C(2,3,2,3)
              D(5,6) = C(2,3,1,3)
              D(6,1) = C(3,1,1,1)
              D(6,2) = C(3,1,2,2)
              D(6,3) = C(3,1,3,3)
              D(6,4) = C(3,1,1,2)
              D(6,5) = C(3,1,2,3)
              D(6,6) = C(3,1,1,3)

        end function
        !==========================================================================================

        !==========================================================================================
        function BallVoigtSym(Av,Bv) result(D)

            implicit none
            real(8),dimension(6)   :: Av,Bv
            real(8),dimension(6,6) :: D

              D(1,1) = Av(1) * Bv(1)
              D(1,2) = Av(1) * Bv(2)
              D(1,3) = Av(1) * Bv(3)
              D(1,4) = Av(1) * Bv(4)
              D(1,5) = Av(1) * Bv(5)
              D(1,6) = Av(1) * Bv(6)
              D(2,1) = Av(2) * Bv(1)
              D(2,2) = Av(2) * Bv(2)
              D(2,3) = Av(2) * Bv(3)
              D(2,4) = Av(2) * Bv(4)
              D(2,5) = Av(2) * Bv(5)
              D(2,6) = Av(2) * Bv(6)
              D(3,1) = Av(3) * Bv(1)
              D(3,2) = Av(3) * Bv(2)
              D(3,3) = Av(3) * Bv(3)
              D(3,4) = Av(3) * Bv(4)
              D(3,5) = Av(3) * Bv(5)
              D(3,6) = Av(3) * Bv(6)
              D(4,1) = Av(4) * Bv(1)
              D(4,2) = Av(4) * Bv(2)
              D(4,3) = Av(4) * Bv(3)
              D(4,4) = Av(4) * Bv(4)
              D(4,5) = Av(4) * Bv(5)
              D(4,6) = Av(4) * Bv(6)
              D(5,1) = Av(5) * Bv(1)
              D(5,2) = Av(5) * Bv(2)
              D(5,3) = Av(5) * Bv(3)
              D(5,4) = Av(5) * Bv(4)
              D(5,5) = Av(5) * Bv(5)
              D(5,6) = Av(5) * Bv(6)
              D(6,1) = Av(6) * Bv(1)
              D(6,2) = Av(6) * Bv(2)
              D(6,3) = Av(6) * Bv(3)
              D(6,4) = Av(6) * Bv(4)
              D(6,5) = Av(6) * Bv(5)
              D(6,6) = Av(6) * Bv(6)


        end function
        !==========================================================================================

        !==========================================================================================
        function SquareVoigtSym(Av,Bv) result(D)

            implicit none
            real(8),dimension(6)   :: Av,Bv
            real(8),dimension(6,6) :: D

              D(1,1) = Av(1) * Bv(1)
              D(1,2) = Av(4) * Bv(4)
              D(1,3) = Av(6) * Bv(6)
              D(1,4) = Av(1) * Bv(4) / 0.2D1 + Av(4) * Bv(1) / 0.2D1
              D(1,5) = Av(4) * Bv(6) / 0.2D1 + Av(6) * Bv(4) / 0.2D1
              D(1,6) = Av(1) * Bv(6) / 0.2D1 + Av(6) * Bv(1) / 0.2D1
              D(2,1) = Av(4) * Bv(4)
              D(2,2) = Av(2) * Bv(2)
              D(2,3) = Av(5) * Bv(5)
              D(2,4) = Av(4) * Bv(2) / 0.2D1 + Av(2) * Bv(4) / 0.2D1
              D(2,5) = Av(2) * Bv(5) / 0.2D1 + Av(5) * Bv(2) / 0.2D1
              D(2,6) = Av(4) * Bv(5) / 0.2D1 + Av(5) * Bv(4) / 0.2D1
              D(3,1) = Av(6) * Bv(6)
              D(3,2) = Av(5) * Bv(5)
              D(3,3) = Av(3) * Bv(3)
              D(3,4) = Av(6) * Bv(5) / 0.2D1 + Av(5) * Bv(6) / 0.2D1
              D(3,5) = Av(5) * Bv(3) / 0.2D1 + Av(3) * Bv(5) / 0.2D1
              D(3,6) = Av(6) * Bv(3) / 0.2D1 + Av(3) * Bv(6) / 0.2D1
              D(4,1) = Av(1) * Bv(4) / 0.2D1 + Av(4) * Bv(1) / 0.2D1
              D(4,2) = Av(4) * Bv(2) / 0.2D1 + Av(2) * Bv(4) / 0.2D1
              D(4,3) = Av(6) * Bv(5) / 0.2D1 + Av(5) * Bv(6) / 0.2D1
              D(4,4) = Av(1) * Bv(2) / 0.4D1 + Av(4) * Bv(4) / 0.2D1 + Av(2) * Bv(1) / 0.4D1
              D(4,5) = Av(4) * Bv(5) / 0.4D1 + Av(2) * Bv(6) / 0.4D1 + Av(6) * Bv(2) / 0.4D1 + Av(5) * Bv(4) / 0.4D1
              D(4,6) = Av(1) * Bv(5) / 0.4D1 + Av(4) * Bv(6) / 0.4D1 + Av(6) * Bv(4) / 0.4D1 + Av(5) * Bv(1) / 0.4D1
              D(5,1) = Av(4) * Bv(6) / 0.2D1 + Av(6) * Bv(4) / 0.2D1
              D(5,2) = Av(2) * Bv(5) / 0.2D1 + Av(5) * Bv(2) / 0.2D1
              D(5,3) = Av(5) * Bv(3) / 0.2D1 + Av(3) * Bv(5) / 0.2D1
              D(5,4) = Av(4) * Bv(5) / 0.4D1 + Av(2) * Bv(6) / 0.4D1 + Av(6) * Bv(2) / 0.4D1 + Av(5) * Bv(4) / 0.4D1
              D(5,5) = Av(2) * Bv(3) / 0.4D1 + Av(5) * Bv(5) / 0.2D1 + Av(3) * Bv(2) / 0.4D1
              D(5,6) = Av(4) * Bv(3) / 0.4D1 + Av(5) * Bv(6) / 0.4D1 + Av(6) * Bv(5) / 0.4D1 + Av(3) * Bv(4) / 0.4D1
              D(6,1) = Av(1) * Bv(6) / 0.2D1 + Av(6) * Bv(1) / 0.2D1
              D(6,2) = Av(4) * Bv(5) / 0.2D1 + Av(5) * Bv(4) / 0.2D1
              D(6,3) = Av(6) * Bv(3) / 0.2D1 + Av(3) * Bv(6) / 0.2D1
              D(6,4) = Av(1) * Bv(5) / 0.4D1 + Av(4) * Bv(6) / 0.4D1 + Av(6) * Bv(4) / 0.4D1 + Av(5) * Bv(1) / 0.4D1
              D(6,5) = Av(4) * Bv(3) / 0.4D1 + Av(5) * Bv(6) / 0.4D1 + Av(6) * Bv(5) / 0.4D1 + Av(3) * Bv(4) / 0.4D1
              D(6,6) = Av(1) * Bv(3) / 0.4D1 + Av(6) * Bv(6) / 0.2D1 + Av(3) * Bv(1) / 0.4D1


        end function
        !==========================================================================================

        !==========================================================================================
        function DoubleContractionT2T2VoigtSym(Av,Bv) result(s)

            implicit none
            real(8),dimension(6)   :: Av,Bv
            real(8)                :: s

            s = Av(1)*Bv(1) + Av(2)*Bv(2) + Av(3)*Bv(3) + &
                2.0d0*Av(4)*Bv(4) + 2.0d0*Av(5)*Bv(5) + 2.0d0*Av(6)*Bv(6)

        end function
        !==========================================================================================

        !==========================================================================================
        function DoubleContractionT4T2VoigtSym(Cv,Av) result(Bv)

            implicit none
            real(8),dimension(6,6) :: Cv
            real(8),dimension(6)   :: Av,Bv

              Bv(1) = Cv(1,1) * Av(1) + Cv(1,2) * Av(2) + Cv(1,3) * Av(3) + &
                        2 * Cv(1,4) * Av(4) + 2 * Cv(1,5) * Av(5) + 2 * Cv(1,6) * Av(6)
              Bv(2) = Cv(2,1) * Av(1) + Cv(2,2) * Av(2) + Cv(2,3) * Av(3) + &
                        2 * Cv(2,4) * Av(4) + 2 * Cv(2,5) * Av(5) + 2 * Cv(2,6) * Av(6)
              Bv(3) = Cv(3,1) * Av(1) + Cv(3,2) * Av(2) + Cv(3,3) * Av(3) + &
                        2 * Cv(3,4) * Av(4) + 2 * Cv(3,5) * Av(5) + 2 * Cv(3,6) * Av(6)
              Bv(4) = Cv(4,1) * Av(1) + Cv(4,2) * Av(2) + Cv(4,3) * Av(3) + &
                        2 * Cv(4,4) * Av(4) + 2 * Cv(4,5) * Av(5) + 2 * Cv(4,6) * Av(6)
              Bv(5) = Cv(5,1) * Av(1) + Cv(5,2) * Av(2) + Cv(5,3) * Av(3) + &
                        2 * Cv(5,4) * Av(4) + 2 * Cv(5,5) * Av(5) + 2 * Cv(5,6) * Av(6)
              Bv(6) = Cv(6,1) * Av(1) + Cv(6,2) * Av(2) + Cv(6,3) * Av(3) + &
                        2 * Cv(6,4) * Av(4) + 2 * Cv(6,5) * Av(5) + 2 * Cv(6,6) * Av(6)

        end function
        !==========================================================================================

        !==========================================================================================
        function DoubleContractionT4T4VoigtSym(Cv,Dv) result(Ev)

            implicit none
            real(8),dimension(6,6) :: Cv,Dv,Ev

                Ev = Dv

                Ev(4:6,:) = 2.0d0*Ev(4:6,:)

                Ev = matmul(Cv,Ev)

        end function
        !==========================================================================================

        !==========================================================================================
        function IdentityT4SymVoigtSym() result(T)

            real(8),dimension(6,6) :: T

            T = 0.0d0
            T(1,1) = 1.0d0
            T(2,2) = 1.0d0
            T(3,3) = 1.0d0
            T(4,4) = 0.50d0
            T(5,5) = 0.50d0
            T(6,6) = 0.50d0

        end function
        !==========================================================================================



        !==========================================================================================
        function MaterialToSpatialModulusVoigtSym(Dmaterial,F) result(Dspatial)

            implicit none
            real(8) :: J
            real(8),dimension(:,:) :: Dmaterial
            real(8),dimension(3,3) :: F, Ft
            real(8),dimension(6,6) :: Dspatial, A, ADmaterial

              J = DeterminantT2(F)

              Ft = transpose(F)

              A(1,1) = Ft(1,1) ** 2.0d0
              A(1,2) = Ft(2,1) ** 2.0d0
              A(1,3) = Ft(3,1) ** 2.0d0
              A(1,4) = 2.0d0 * Ft(1,1) * Ft(2,1)
              A(1,5) = 2.0d0 * Ft(2,1) * Ft(3,1)
              A(1,6) = 2.0d0 * Ft(1,1) * Ft(3,1)
              A(2,1) = Ft(1,2) ** 2.0d0
              A(2,2) = Ft(2,2) ** 2.0d0
              A(2,3) = Ft(3,2) ** 2.0d0
              A(2,4) = 2.0d0 * Ft(1,2) * Ft(2,2)
              A(2,5) = 2.0d0 * Ft(2,2) * Ft(3,2)
              A(2,6) = 2.0d0 * Ft(1,2) * Ft(3,2)
              A(3,1) = Ft(1,3) ** 2.0d0
              A(3,2) = Ft(2,3) ** 2.0d0
              A(3,3) = Ft(3,3) ** 2.0d0
              A(3,4) = 2.0d0 * Ft(1,3) * Ft(2,3)
              A(3,5) = 2.0d0 * Ft(2,3) * Ft(3,3)
              A(3,6) = 2.0d0 * Ft(1,3) * Ft(3,3)
              A(4,1) = Ft(1,1) * Ft(1,2)
              A(4,2) = Ft(2,1) * Ft(2,2)
              A(4,3) = Ft(3,1) * Ft(3,2)
              A(4,4) = Ft(1,1) * Ft(2,2) + Ft(2,1) * Ft(1,2)
              A(4,5) = Ft(2,1) * Ft(3,2) + Ft(3,1) * Ft(2,2)
              A(4,6) = Ft(3,1) * Ft(1,2) + Ft(1,1) * Ft(3,2)
              A(5,1) = Ft(1,2) * Ft(1,3)
              A(5,2) = Ft(2,2) * Ft(2,3)
              A(5,3) = Ft(3,2) * Ft(3,3)
              A(5,4) = Ft(1,2) * Ft(2,3) + Ft(2,2) * Ft(1,3)
              A(5,5) = Ft(2,2) * Ft(3,3) + Ft(3,2) * Ft(2,3)
              A(5,6) = Ft(3,2) * Ft(1,3) + Ft(1,2) * Ft(3,3)
              A(6,1) = Ft(1,1) * Ft(1,3)
              A(6,2) = Ft(2,1) * Ft(2,3)
              A(6,3) = Ft(3,1) * Ft(3,3)
              A(6,4) = Ft(1,3) * Ft(2,1) + Ft(2,3) * Ft(1,1)
              A(6,5) = Ft(2,3) * Ft(3,1) + Ft(3,3) * Ft(2,1)
              A(6,6) = Ft(3,3) * Ft(1,1) + Ft(1,3) * Ft(3,1)

              !Dspatial = matmul(A, matmul(Dmaterial,transpose(A)) )/J

              call dsymm('R', 'U', 6, 6, 1.0d0, Dmaterial, 6, A, 6, 0.0d0, ADmaterial, 6)

              call dgemm('N', 'T', 6, 6, 6, 1.0d0/J, ADmaterial, 6, A,  6, 0.0d0, Dspatial, 6)


        end function
        !==========================================================================================
! TODO (Thiago#1#12/06/15): Colocar os push-pull entre os modulos tangente no módulo de mecânica do contínuo.





end module
