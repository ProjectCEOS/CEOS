module MathRoutines
    real(8),parameter,private::zero=0.0d0
    private::Error

    real(8),parameter :: Pi = 4.0d0*atan(1.0d0)

    ! TODO (Jan#1#10/29/15): Criar um módulo só para notação de VOIGT e anexar com o mathroutines

    contains
!######################################################################################################################################################################################
        subroutine Error(MSG)
            character(len=*)::MSG
            write(*,*) 'myMath Error::'//MSG
            pause
            stop
        end subroutine
!######################################################################################################################################################################################
        function det(M) result(d)
            implicit none
            real(8)::d
            real(8),dimension(:,:)::M

            select case (size(M,dim=1))
                case (2)
                    d = M(1,1)*M(2,2) - M(1,2)*M(2,1)
                case (3)
                    d = M(1,1)*M(2,2)*M(3,3) + M(1,2)*M(2,3)*M(3,1) + M(1,3)*M(2,1)*M(3,2) - M(1,3)*M(2,2)*M(3,1) - M(1,2)*M(2,1)*M(3,3) - M(1,1)*M(2,3)*M(3,2)
                case (1)
                    d=M(1,1)
                case default
                    call Error('det::Dimension not implemented')
            end select
        end function
!######################################################################################################################################################################################
        function inverse(A) result(B)
            implicit none
            real(8),dimension(:,:)::A
            real(8),dimension( size(A,dim=1) , size(A,dim=2) )::B

            real(8):: detA

            detA = det(A)
            if (detA==zero) call error('inv::Det==0')

            select case (size(A,dim=1))
            case (2)
                B(1,1)= A(2,2) ; B(1,2)=-A(1,2)
                B(2,1)=-A(2,1) ; B(2,2)= A(1,1)
                B=B/detA

            case (3)
                B(1,1) = A(2,2)*A(3,3) - A(2,3)*A(3,2) ; B(1,2) = A(1,3)*A(3,2) - A(1,2)*A(3,3) ; B(1,3) = A(1,2)*A(2,3) - A(1,3)*A(2,2)
                B(2,1) = A(2,3)*A(3,1) - A(2,1)*A(3,3) ; B(2,2) = A(1,1)*A(3,3) - A(1,3)*A(3,1) ; B(2,3) = A(1,3)*A(2,1) - A(1,1)*A(2,3)
                B(3,1) = A(2,1)*A(3,2) - A(2,2)*A(3,1) ; B(3,2) = A(1,2)*A(3,1) - A(1,1)*A(3,2) ; B(3,3) = A(1,1)*A(2,2) - A(1,2)*A(2,1)
    			B = B/detA

            case (1)
                B = 1.0d0/A(1,1)

            case default
                call error('inv::Dimension not implemented')
            end select
        end function
!######################################################################################################################################################################################
    function norm(X) result(n)
        real(8),dimension(:)::X
        real(8)::n
        n=dsqrt(dot_product(X,X))
    end function
!######################################################################################################################################################################################
    function Tensor_Product(a,b) result(T)

        real(8),dimension(:)::a,b
        real(8),dimension(size(a),size(b))::T

        do i=1,size(a)
            do j=1,size(b)
                T(i,j)=a(i)*b(j)
            enddo
        enddo

    end function
!######################################################################################################################################################################################


!######################################################################################################################################################################################
    function Tensor_Product_Ball (a,b) result(T)

        real(8),dimension(:,:) :: a, b
        real(8),dimension(size(a,1),size(a,1),size(a,1),size(a,1)) :: T

                do i=1,size(a,1)
                    do j=1,size(a,1)
                        do k=1,size(a,1)
                            do l=1,size(a,1)

                               T(i,j,k,l) = A(i,j)*B(k,l)

                            enddo
                        enddo
                    enddo
                enddo

    end function
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    function Tensor_Product_Square (a,b) result(T)

        real(8),dimension(:,:) :: a, b
        real(8),dimension(size(a,1),size(a,1),size(a,1),size(a,1)) :: T

                do i=1,size(a,1)
                    do j=1,size(a,1)
                        do k=1,size(a,1)
                            do l=1,size(a,1)

                               T(i,j,k,l) = A(i,k)*B(j,l)

                            enddo
                        enddo
                    enddo
                enddo

    end function
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    function Tensor_Inner_Product(a,b) result(T)

        real(8),dimension(:,:) :: a, b
        real(8) :: T

        T = sum(sum(a*b,2))

    end function
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    function Tensor_4_Double_Contraction (a,b) result(T)

        real(8),dimension(:,:,:,:) :: a, b
        real(8),dimension(size(a,1),size(a,1),size(a,1),size(a,1)) :: T
		integer :: i,j,k,l,p,q

                T = 0.0d0
                do i=1,size(a,1)
                    do j=1,size(a,1)
                        do k=1,size(a,1)
                            do l=1,size(a,1)

                                do p=1,size(a,1)
                                    do q=1,size(a,1)

                                        T(i,j,k,l) = T(i,j,k,l) + A(i,j,p,q)*B(p,q,k,l)

                                    enddo
                                enddo

                            enddo
                        enddo
                    enddo
                enddo

    end function
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    function Isym() result(T)

        real(8),dimension(3,3,3,3) :: T
        real(8),dimension(3,3) ::Id

        Id = 0.0d0
        Id(1,1) = 1.0d0
        Id(2,2) = 1.0d0
        Id(3,3) = 1.0d0

                do i=1,3
                    do j=1,3
                        do k=1,3
                            do l=1,3

                               T(i,j,k,l) = (1.0d0/2.0d0)*( Id(i,k)*Id(j,l) + Id(i,l)*Id(j,k)  )

                            enddo
                        enddo
                    enddo
                enddo

    end function
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    function IsymV() result(T)

        real(8),dimension(6,6) :: T

        T = 0.0d0
        T(1,1) = 1.0d0
        T(2,2) = 1.0d0
        T(3,3) = 1.0d0
        T(4,4) = 0.50d0
        T(5,5) = 0.50d0
        T(6,6) = 0.50d0


    end function
!######################################################################################################################################################################################


!######################################################################################################################################################################################
    function Push_Forward_Tensor_4 (W,F) result(T)

        real(8) :: detF, aux
        real(8),dimension(:,:,:,:) :: W
        real(8),dimension(3,3)     :: F
        real(8),dimension(size(W,1),size(W,1),size(W,1),size(W,1)) :: T
		integer :: i,j,k,l,a,b,c,d

            ! Tensor C espacial
                detF = det(F)

                do i=1,3
                    do j=1,3
                        do k=1,3
                            do l=1,3

                                aux=0.0d0
                                do a=1,3
                                    do b=1,3
                                        do c=1,3
                                            do d=1,3

                                               aux = aux + F(i,a)*F(j,b)*F(k,c)*F(l,d)*W(a,b,c,d)/detF

                                            enddo
                                        enddo
                                    enddo
                                enddo

                                T(i,j,k,l) = aux

                            enddo
                        enddo
                    enddo
                enddo

    end function
!######################################################################################################################################################################################



!######################################################################################################################################################################################
    function Push_Forward_Voigt (W,F) result(T)

        implicit none
        real(8) :: J
        real(8),dimension(:,:)                 :: W
        real(8),dimension(3,3)                 :: F, Ft
        real(8),dimension(size(W,1),size(W,2)) :: T, A, AW


    J = det(F)

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

      !T = matmul(A, matmul(W,transpose(A)) )/J

      call dsymm('R', 'U', 6, 6, 1.0d0, W, 6, A, 6, 0.0d0, AW, 6)

      call dgemm('N', 'T', 6, 6, 6, 1.0d0/J, AW, 6, A,  6, 0.0d0, T, 6)


    end function
!######################################################################################################################################################################################


!######################################################################################################################################################################################
    function Tensor_4_Double_Contraction_Voigt (A,B) result(T)

        real(8),dimension(6,6) :: A, B, T

        T = B

        T(4:6,:) = 2.0d0*T(4:6,:)

        T = matmul(A,T)

    end function
!######################################################################################################################################################################################


!######################################################################################################################################################################################
    function Ball_Voigt (A,B) result(T)

        implicit none
        real(8),dimension(6)   :: A,B
        real(8),dimension(6,6) :: T

      T(1,1) = A(1) * B(1)
      T(1,2) = A(1) * B(2)
      T(1,3) = A(1) * B(3)
      T(1,4) = A(1) * B(4)
      T(1,5) = A(1) * B(5)
      T(1,6) = A(1) * B(6)
      T(2,1) = A(2) * B(1)
      T(2,2) = A(2) * B(2)
      T(2,3) = A(2) * B(3)
      T(2,4) = A(2) * B(4)
      T(2,5) = A(2) * B(5)
      T(2,6) = A(2) * B(6)
      T(3,1) = A(3) * B(1)
      T(3,2) = A(3) * B(2)
      T(3,3) = A(3) * B(3)
      T(3,4) = A(3) * B(4)
      T(3,5) = A(3) * B(5)
      T(3,6) = A(3) * B(6)
      T(4,1) = A(4) * B(1)
      T(4,2) = A(4) * B(2)
      T(4,3) = A(4) * B(3)
      T(4,4) = A(4) * B(4)
      T(4,5) = A(4) * B(5)
      T(4,6) = A(4) * B(6)
      T(5,1) = A(5) * B(1)
      T(5,2) = A(5) * B(2)
      T(5,3) = A(5) * B(3)
      T(5,4) = A(5) * B(4)
      T(5,5) = A(5) * B(5)
      T(5,6) = A(5) * B(6)
      T(6,1) = A(6) * B(1)
      T(6,2) = A(6) * B(2)
      T(6,3) = A(6) * B(3)
      T(6,4) = A(6) * B(4)
      T(6,5) = A(6) * B(5)
      T(6,6) = A(6) * B(6)



    end function
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    function Square_Voigt (A,B) result(T)

        implicit none
        real(8),dimension(6)   :: A,B
        real(8),dimension(6,6) :: T

      T(1,1) = A(1) * B(1)
      T(1,2) = A(4) * B(4)
      T(1,3) = A(6) * B(6)
      T(1,4) = A(1) * B(4) / 0.2D1 + A(4) * B(1) / 0.2D1
      T(1,5) = A(4) * B(6) / 0.2D1 + A(6) * B(4) / 0.2D1
      T(1,6) = A(1) * B(6) / 0.2D1 + A(6) * B(1) / 0.2D1
      T(2,1) = A(4) * B(4)
      T(2,2) = A(2) * B(2)
      T(2,3) = A(5) * B(5)
      T(2,4) = A(4) * B(2) / 0.2D1 + A(2) * B(4) / 0.2D1
      T(2,5) = A(2) * B(5) / 0.2D1 + A(5) * B(2) / 0.2D1
      T(2,6) = A(4) * B(5) / 0.2D1 + A(5) * B(4) / 0.2D1
      T(3,1) = A(6) * B(6)
      T(3,2) = A(5) * B(5)
      T(3,3) = A(3) * B(3)
      T(3,4) = A(6) * B(5) / 0.2D1 + A(5) * B(6) / 0.2D1
      T(3,5) = A(5) * B(3) / 0.2D1 + A(3) * B(5) / 0.2D1
      T(3,6) = A(6) * B(3) / 0.2D1 + A(3) * B(6) / 0.2D1
      T(4,1) = A(1) * B(4) / 0.2D1 + A(4) * B(1) / 0.2D1
      T(4,2) = A(4) * B(2) / 0.2D1 + A(2) * B(4) / 0.2D1
      T(4,3) = A(6) * B(5) / 0.2D1 + A(5) * B(6) / 0.2D1
      T(4,4) = A(1) * B(2) / 0.4D1 + A(4) * B(4) / 0.2D1 + A(2) * B(1) /0.4D1
      T(4,5) = A(4) * B(5) / 0.4D1 + A(6) * B(2) / 0.4D1 + A(2) * B(6) /0.4D1 + A(5) * B(4) / 0.4D1
      T(4,6) = A(1) * B(5) / 0.4D1 + A(6) * B(4) / 0.4D1 + A(4) * B(6) / 0.4D1 + A(5) * B(1) / 0.4D1
      T(5,1) = A(4) * B(6) / 0.2D1 + A(6) * B(4) / 0.2D1
      T(5,2) = A(2) * B(5) / 0.2D1 + A(5) * B(2) / 0.2D1
      T(5,3) = A(5) * B(3) / 0.2D1 + A(3) * B(5) / 0.2D1
      T(5,4) = A(4) * B(5) / 0.4D1 + A(6) * B(2) / 0.4D1 + A(2) * B(6) / 0.4D1 + A(5) * B(4) / 0.4D1
      T(5,5) = A(2) * B(3) / 0.4D1 + A(5) * B(5) / 0.2D1 + A(3) * B(2) / 0.4D1
      T(5,6) = A(4) * B(3) / 0.4D1 + A(6) * B(5) / 0.4D1 + A(5) * B(6) / 0.4D1 + A(3) * B(4) / 0.4D1
      T(6,1) = A(1) * B(6) / 0.2D1 + A(6) * B(1) / 0.2D1
      T(6,2) = A(4) * B(5) / 0.2D1 + A(5) * B(4) / 0.2D1
      T(6,3) = A(6) * B(3) / 0.2D1 + A(3) * B(6) / 0.2D1
      T(6,4) = A(1) * B(5) / 0.4D1 + A(6) * B(4) / 0.4D1 + A(4) * B(6) / 0.4D1 + A(5) * B(1) / 0.4D1
      T(6,5) = A(4) * B(3) / 0.4D1 + A(6) * B(5) / 0.4D1 + A(5) * B(6) / 0.4D1 + A(3) * B(4) / 0.4D1
      T(6,6) = A(1) * B(3) / 0.4D1 + A(6) * B(6) / 0.2D1 + A(3) * B(1) / 0.4D1


    end function
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    function Inner_Product_Voigt (A,B) result(T)

        implicit none
        real(8),dimension(6)   :: A,B
        real(8) :: T

        T = A(1)*B(1) + A(2)*B(2) + A(3)*B(3) + 2.0d0*A(4)*B(4) + 2.0d0*A(5)*B(5) + 2.0d0*A(6)*B(6)


    end function
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    function Convert_to_Voigt (A) result(T)

        implicit none
        real(8),dimension(3,3)   :: A
        real(8),dimension(6)     :: T

        T(1) = A(1,1)
        T(2) = A(2,2)
        T(3) = A(3,3)
        T(4) = A(1,2)
        T(5) = A(2,3)
        T(6) = A(1,3)


    end function
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    function Convert_to_Voigt_3D_Sym (A) result(T)

        implicit none
        real(8),dimension(3,3)   :: A
        real(8),dimension(6)     :: T

        T(1) = A(1,1)
        T(2) = A(2,2)
        T(3) = A(3,3)
        T(4) = A(1,2)
        T(5) = A(2,3)
        T(6) = A(1,3)


    end function
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    function Convert_to_Voigt_3D (A) result(T)

        implicit none
        real(8),dimension(3,3)   :: A
        real(8),dimension(9)     :: T

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
!######################################################################################################################################################################################


!######################################################################################################################################################################################
    subroutine LinearInterpolation ( x, A, y, flag )

        implicit none
        real(8),dimension(:,:)   :: A
        real(8)                  :: x, y
        integer                  :: flag, i


        flag = 0

        do i = 1,size(A,1)-1

            if ( (x .ge. A(i,1)) .and. (x .le. A(i+1,1))  ) then

                y = (x - A(i,1))*(A(i+1,2) - A(i,2))/(A(i+1,1) - A(i,1)) +  A(i,2)

                flag = 1

                return

            endif

        enddo


    end subroutine
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    subroutine EigenProblemSym3D ( M, eigenvalues, eigenvectors )

        implicit none

        real(8),dimension(:,:)   :: M

        character(len=1)         :: jobz, uplo
        integer                  :: n, lda, lwork, info
        real(8),dimension(9)     :: work
        real(8),dimension(3)     :: w, eigenvalues
        real(8),dimension(3,3)   :: eigenvectors, A

        ! INPUT work: is a workspace array, its dimension max(1, lwork)
        ! INPUT A: A(lda,*) is an array containing either upper or lower triangular part of the symmetric matrix A, as specified by uplo.
        ! OUTPUT A: On exit, if jobz = 'V', then if info = 0, array a contains the orthonormal eigenvectors of the matrix A.
        ! OUTPUT w: If info = 0, contains the eigenvalues of the matrix A in ascending order.
        ! OUTPUT info:If info = 0, the execution is successful.

        jobz = 'V'      !If jobz = 'V', then eigenvalues and eigenvectors are computed
        uplo = 'U'      !If uplo = 'U', a stores the upper triangular part of A
        n = size(A,1)   !The order of the matrix A (n ≥ 0).
        lda = n         !The leading dimension of the array a.
        lwork = 9       !Constraint: lwork ≥ max(1, 3n-1).

        A = M

        call dsyev(jobz, uplo, n, A, lda, w, work, lwork, info)

        eigenvalues = w
        eigenvectors = A

    end subroutine
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    subroutine EigenProblemSym2D ( M, eigenvalues, eigenvectors )

        implicit none

        real(8),dimension(:,:)   :: M

        character(len=1)         :: jobz, uplo
        integer                  :: n, lda, lwork, info
        real(8),dimension(9)     :: work
        real(8),dimension(2)     :: w, eigenvalues
        real(8),dimension(2,2)   :: eigenvectors, A

        ! INPUT work: is a workspace array, its dimension max(1, lwork)
        ! INPUT A: A(lda,*) is an array containing either upper or lower triangular part of the symmetric matrix A, as specified by uplo.
        ! OUTPUT A: On exit, if jobz = 'V', then if info = 0, array a contains the orthonormal eigenvectors of the matrix A.
        ! OUTPUT w: If info = 0, contains the eigenvalues of the matrix A in ascending order.
        ! OUTPUT info:If info = 0, the execution is successful.

        jobz = 'V'      !If jobz = 'V', then eigenvalues and eigenvectors are computed
        uplo = 'U'      !If uplo = 'U', a stores the upper triangular part of A
        n = size(A,1)   !The order of the matrix A (n ≥ 0).
        lda = n         !The leading dimension of the array a.
        lwork = 9       !Constraint: lwork ≥ max(1, 3n-1).

        A = M

        call dsyev(jobz, uplo, n, A, lda, w, work, lwork, info)

        eigenvalues = w
        eigenvectors = A

    end subroutine
!######################################################################################################################################################################################


!######################################################################################################################################################################################
    subroutine MatrixMatrixMultiply_Trans ( A, B, C, alpha, beta )

        implicit none

        real(8),dimension(:,:)   :: A, B, C
        real(8)                  :: alpha, beta
        integer                  :: m, n, k, lda, ldb, ldc
        character(len=1)         :: transA, transB

        ! The routine compute a scalar-matrix-matrix product and add the result to a
        ! scalar-matrix product, with general matrices. The operation is defined as:
        ! C := alpha*op(A)*op(B) + beta*C,
        ! where:
        ! op(X) is one of op(X) = X or op(X) = X^T

        ! transA or transB: Specifies the form of op(X) used in the matrix multiplication:
        ! if trans = 'N' or 'n', then op(X) = X;
        ! if trans = 'T' or 't', then op(X) = X^T;

        transA = 'T'
        transB = 'N'

        m = size(C,1)
        n = size(C,2)
        k = size(B,1)

        lda = k
        ldb = k
        ldc = m

        call dgemm(transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc)

    end subroutine
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    subroutine MatrixMatrixMultiply_Sym ( A, B, C, alpha, beta )

        implicit none

        real(8),dimension(:,:)   :: A, B, C
        real(8)                  :: alpha, beta
        integer                  :: m, n, lda, ldb, ldc
        character(len=1)         :: side, uplo

        ! The routines compute a scalar-matrix-matrix product with one symmetric matrix
        ! and add the result to a scalar-matrix product. The operation is defined as:
        ! C := alpha*A*B + beta*C  ; A=Sym and upper triangular

        side = 'L'
        uplo = 'U'

        m = size(C,1)
        n = size(C,2)

        lda = m
        ldb = m
        ldc = m

        call dsymm(side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc)

    end subroutine
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    subroutine MatrixVectorMultiply ( trans, A, x, y, alpha, beta )

        implicit none

        real(8),dimension(:,:)   :: A
        real(8),dimension(:)     :: x,y
        real(8)                  :: alpha, beta
        integer                  :: m, n, lda, incx, incy
        character(len=1)         :: trans

        ! The routines perform a matrix-vector operation defined as:
        ! y := alpha*op(A)*x + beta*y
        ! where:
        ! op(X) is one of op(X) = X or op(X) = X^T

        ! transA or transB: Specifies the form of op(X) used in the matrix multiplication:
        ! if trans = 'N' or 'n', then op(X) = X;
        ! if trans = 'T' or 't', then op(X) = X^T;

        m = size(A,1)
        n = size(A,2)

        lda = m
        incx = 1
        incy = 1

        call dgemv(trans, m, n, alpha, A, lda, x, incx, beta, y, incy)

    end subroutine
!######################################################################################################################################################################################

!######################################################################################################################################################################################
    subroutine SolveLinearSystemFull(A_input,b_input,x_output)

        implicit none

        real(8), dimension(:,:)                              :: A_input
        real(8), dimension(:)                                :: b_input, x_output

        real(8), dimension(size(A_input,1),size(A_input,2))  :: A
        real(8), dimension(size(b_input),1)                  :: b
        integer, dimension(size(A,1))                        :: ipiv
        integer                                              :: m, n, lda, ldb, info, nrhs
        character(len=1)                                     :: trans


        !Intel® Math Kernel Library
        !LAPACK routines: Linear Equations
        !dgetrf: Computes the LU factorization of a general matrix.
        !dgetrs: Solves a system of linear equations with an LU-factored square matrix.

        A = A_input
        b(:,1) = b_input

        trans = 'N' ! The system has the form A*X = B.
        m = size(A,1)
        n = size(A,2)
        lda = n
        ldb = n
        nrhs = 1

        ipiv = 0

        call dgetrf( m, n, A, lda, ipiv, info )
        call dgetrs( trans, n, nrhs, A, lda, ipiv, b, ldb, info )

        x_output = b(:,1)

    end subroutine
!######################################################################################################################################################################################


! TODO (Thiago#1#11/03/15): Preparar solver não linear com matriz cheia para usar no ponto de gauss ...
!



end module
