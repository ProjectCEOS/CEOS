!##################################################################################################
! This module has the attributes and methods for the material model.
!--------------------------------------------------------------------------------------------------
! Date: 2014/02
!
! Authors:  Thiago Andre Carniel
!------------------------------------------------------------------------------------------------
! Modifications:
! Date:         Author:
!##################################################################################################
module ModViscoelasticMatrix

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! DECLARATIONS OF VARIABLES
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Modules and implicit declarations
    ! --------------------------------------------------------------------------------------------
    use ConstitutiveModel
    use ModContinuumMechanics

    implicit none


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel": Attributes and methods of the constitutive model
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type ViscoelasticMatrixProperties

        ! Variables of material parameters
        !----------------------------------------------------------------------------------------------
        real(8) :: K_inf, Mu_inf, Lambda_inf, K_e, Mu_e, Ni_v

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel": Attributes and methods of the constitutive model
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , extends(ClassConstitutiveModel) :: ClassViscoelasticMatrix

		! Class Attributes : Usually the state variables (instant and internal variables)
		!----------------------------------------------------------------------------------------
        type (ViscoelasticMatrixProperties), pointer :: Properties => null()

        ! Variables
        real(8) :: Time_old
        real(8) :: gama_new, gama_old
        real(8) , allocatable , dimension(:) :: dvm_new, dvm_old, Fvm_new, Fvm_old

        contains

            ! Class Methods
            !----------------------------------------------------------------------------------
             procedure :: ConstitutiveModelConstructor => ConstitutiveModelConstructor_ViscoelasticMatrix
             procedure :: ConstitutiveModelDestructor  => ConstitutiveModelDestructor_ViscoelasticMatrix
             procedure :: ReadMaterialParameters       => ReadMaterialParameters_ViscoelasticMatrix
             procedure :: GetResult                    => GetResult_ViscoelasticMatrix
             procedure :: SwitchConvergedState         => SwitchConvergedState_ViscoelasticMatrix
             procedure :: CopyProperties               => CopyProperties_ViscoelasticMatrix

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel"_PlaneStrain: Attributes and methods of the constitutive model
    ! in Three-Dimensional analysis.
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , extends(ClassViscoelasticMatrix) :: ClassViscoelasticMatrix_3D

         contains
            ! Class Methods
            !----------------------------------------------------------------------------------
             procedure :: UpdateStressAndStateVariables  =>  UpdateStressAndStateVariables_ViscoelasticMatrix_3D
             !procedure :: GetTangentModulus              =>  GetTangentModulus_ViscoelasticMatrix_3D

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


    contains

        !==========================================================================================
        ! Method ConstitutiveModelConstructor_"NameOfTheMaterialModel": Routine that constructs the
        ! Constitutive Model
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine ConstitutiveModelConstructor_ViscoelasticMatrix(this,AnalysisSettings)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use Analysis
            use ModVoigtNotation

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassViscoelasticMatrix) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            type(ClassAnalysis) :: AnalysisSettings

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: I(9)
		    !************************************************************************************

 		    !************************************************************************************
            ! ALLOCATE THE STATE VARIABLES
		    !************************************************************************************

            ! Identity
            I = 0.0d0
            I(1) = 1.0d0
            I(5) = 1.0d0
            I(9) = 1.0d0

            allocate( this%dvm_new(9) )
            allocate( this%dvm_old(9) )
            allocate( this%Fvm_new(9) )
            allocate( this%Fvm_old(9) )

            this%dvm_new = 0.0d0
            this%dvm_old = 0.0d0
            this%Fvm_new = I
            this%Fvm_old = I
            this%gama_new = 0.0d0
            this%gama_old = 0.0d0

		    !************************************************************************************

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method ConstitutiveModelDestructor_"NameOfTheMaterialModel": Routine that constructs the
        ! Constitutive Model
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine ConstitutiveModelDestructor_ViscoelasticMatrix(this)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use Analysis

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassViscoelasticMatrix) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------

		    !************************************************************************************

 		    !************************************************************************************
            ! DEALLOCATE THE STATE VARIABLES
		    !************************************************************************************

            if (allocated(this%dvm_new)) deallocate(this%dvm_new)
            if (allocated(this%dvm_old)) deallocate(this%dvm_old)
            if (allocated(this%Fvm_new)) deallocate(this%Fvm_new)
            if (allocated(this%Fvm_old)) deallocate(this%Fvm_old)
            this%gama_new = 0.0d0
            this%gama_old = 0.0d0

		    !************************************************************************************

        end subroutine
        !==========================================================================================



        !==========================================================================================
        ! Method ReadMaterialParameters_"NameOfTheMaterialModel": Routine that reads the material
        ! parameters
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine ReadMaterialParameters_ViscoelasticMatrix(this,DataFile)
            use Parser

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! ---------------------------------------------------------------------------------
            class(ClassViscoelasticMatrix) :: this

            ! Input variables
            ! ---------------------------------------------------------------------------------
            !integer , intent(in) :: FileNum
            type(ClassParser)::DataFile

		    !************************************************************************************
		    character(len=100),dimension(6)::ListOfOptions,ListOfValues
		    logical,dimension(6)::FoundOption
		    integer::i

            !************************************************************************************
            ! READ THE MATERIAL PARAMETERS
		    !************************************************************************************
            allocate (this%Properties)

            ListOfOptions=[ "K_inf", "Mu_inf", "Lambda_inf", "K_e", "Mu_e", "Ni_v" ]

            call DataFile%FillListOfOptions(ListOfOptions,ListOfValues,FoundOption)
            call DataFile%CheckError

            do i=1,size(FoundOption)
                if (.not.FoundOption(i)) then
                    write(*,*) "ReadMaterialParameters_ViscoelasticMatrix :: Option not found ["//trim(ListOfOptions(i))//"]"
                    stop
                endif
            enddo

            this%Properties%K_inf       = ListOfValues(1)
            this%Properties%Mu_inf      = ListOfValues(2)
            this%Properties%Lambda_inf  = ListOfValues(3)
            this%Properties%K_e         = ListOfValues(4)
            this%Properties%Mu_e        = ListOfValues(5)
            this%Properties%Ni_v        = ListOfValues(6)


		    !************************************************************************************

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method CopyProperties_"NameOfTheMaterialModel": Routine that reads the material
        ! parameters
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine CopyProperties_ViscoelasticMatrix(this,Reference)

             class(ClassViscoelasticMatrix) :: this
             class(ClassConstitutiveModel) :: Reference

             select type ( Reference )

                 class is ( ClassViscoelasticMatrix )
                    this%Properties => Reference%Properties
                 class default
                     stop "erro na subroutine CopyProperties_ViscoelasticMatrix"

            end select

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method UpdateStateVariables_"NameOfTheMaterialModel"_3D: Routine that
        ! contains the algorithm employed to update the state variables in the Three-Dimensional
        ! analysis.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine UpdateStressAndStateVariables_ViscoelasticMatrix_3D(this,Status)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! ---------------------------------------------------------------------------------
            use MathRoutines
            use ModVoigtNotation

            class(ClassViscoelasticMatrix_3D) :: this
            type (ClassStatus) :: Status


            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: K_inf, Mu_inf, Lambda_inf, K_e, Mu_e, Ni_v

            real(8) :: dt, gama_new, gama_old, Q

            real(8) :: J_new, TrC_new

            real(8) :: F_new(9), C_new(9)

            real(8) :: dvm_new(9), dvm_old(9), Fvm_new(9), Fvm_old(9)

            real(8) :: Sm_new(9), Sinfm_new(9), Sem_new(9)

            real(8) :: I(9), Aux_T2(9), Snh(9), Cauchy(3,3)

		    !************************************************************************************

            !************************************************************************************
            ! ALGORITHM THAT UPDATES STATE VARIABLES
		    !************************************************************************************

            ! Optional: Retrieve Variables
            ! -----------------------------------------------------------------------------------
            K_inf       = this%Properties%K_inf
            Mu_inf      = this%Properties%Mu_inf
            Lambda_inf  = this%Properties%Lambda_inf
            K_e         = this%Properties%K_e
            Mu_e        = this%Properties%Mu_e
            Ni_v        = this%Properties%Ni_v

            F_new  = Tensor2ToVoigt(this%F)

            dvm_old  = this%dvm_old
            Fvm_old  = this%Fvm_old
            gama_old = this%gama_old             
            ! -----------------------------------------------------------------------------------

            ! Identity
            I = 0.0d0
            I(1) = 1.0d0
            I(5) = 1.0d0
            I(9) = 1.0d0

            ! Increment of Time
            dt = this%Time - this%Time_old


            ! VARIATIONAL UPDATE - Local Newton-Raphson Procedure
            ! -----------------------------------------------------------------------------------

            ! MATRIX
            ! -----------------------------------------------------------------------------------
            call Local_Newton_Raphson_MATRIX( F_new, Fvm_new, Fvm_old, dvm_new, dvm_old, gama_new, gama_old, &
                                              K_e, Mu_e, Ni_v, dt, Sem_new, Status )

            ! Save Updated Internal Variable
            this%dvm_new  = dvm_new
            this%Fvm_new  = Fvm_new
            this%gama_new = gama_new
            ! -----------------------------------------------------------------------------------


            ! UPDATE STRESSES
            ! -----------------------------------------------------------------------------------

            ! Stress Inf - Second Piola
            ! -----------------------------------------------------------------------------------
            ! Jacobian
            J_new = DeterminantT2Voigt(F_new)

            !Right-Cauchy Green Strain
            C_new = SingleContractionT2T2Voigt(TransposeT2Voigt(F_new),F_new)

            !Trace of the Right-Cauchy Green Strain
            TrC_new = C_new(1) + C_new(5) + C_new(9)

            ! Fung with Qe=Neo-Hookean
            !------------------------
            Q = (Mu_inf/2.0d0)*( TrC_new - 3.0d0 ) - Mu_inf*dlog(J_new) + &
                 (Lambda_inf/2.0d0)*( dlog(J_new)**2.0d0 )

            Snh = Mu_inf*(I - InverseT2Voigt(C_new)) + Lambda_inf*dlog(J_new)*InverseT2Voigt(C_new)

            ! Second Piola Inf
            Sinfm_new = K_inf*dexp(Q)*Snh
            ! -----------------------------------------------------------------------------------

            ! Stress Elastic - Second Piola - Computed in Local_Newton_Raphson_MATRIX
            ! -----------------------------------------------------------------------------------


            ! Total Stress - Second Piola
            ! -----------------------------------------------------------------------------------
            Aux_T2 = SingleContractionT2T2Voigt(InverseT2Voigt(Fvm_new),Sem_new)
            Aux_T2 = SingleContractionT2T2Voigt(Aux_T2,TransposeT2Voigt(InverseT2Voigt(Fvm_new)))

            Sm_new = Sinfm_new + Aux_T2
            ! -----------------------------------------------------------------------------------


            ! TOTAL STRESS - Cauchy
            ! -----------------------------------------------------------------------------------
            Cauchy = StressTransformation(this%F,VoigtToTensor2(Sm_new),StressMeasures%SecondPiola,StressMeasures%Cauchy )

            this%Stress = Tensor2ToVoigtSym(Cauchy)
            ! -----------------------------------------------------------------------------------


		    !************************************************************************************

        end subroutine
        !==========================================================================================



        !==========================================================================================
        subroutine  Local_Newton_Raphson_MATRIX( F_new, Fvm_new, Fvm_old, dvm_new, dvm_old, gama_new, gama_old, &
                                                 K_e, Mu_e, Ni_v, dt, Sem_new, Status )

            use ModVoigtNotation

            ! -----------------------------------------------------------------------------------
            ! Input/Output variables
            real(8), dimension(:) :: F_new, Fvm_new, Fvm_old, dvm_new, dvm_old
            real(8)               :: gama_new, gama_old
            real(8)               :: K_e, Mu_e, Ni_v, dt

            type(ClassStatus) :: Status

            ! Internal Variables
            integer :: j, MaxIter, it
            real(8) :: Tol, Norm_Rm
            real(8) :: X(7), deltaX(7), Rm(7), Km(7,7)

            real(8) :: Jvm_new, Jem_new, TrCem_new, Qe

            real(8) :: I(9), Fem_new(9), Cem_new(9), CemInv_new(9), Sem_new(9), Mem_new(9), Snhe(9)

            real(8) :: Fvm_new_FvmInv_old(9), DJvm_Ddvm(9), DPinc_Ddvm(9), DL_Ddvm(9), DL_Dgama

            real(8) :: Aux1_T2(9), Aux2_T2(9), Aux1_T2Sym(6)

            real(8) :: DPhivm_Ddvm(9), D2Phivm_Ddvm2(9,9), Dem(9,9)

            real(8) :: B(9,9), DCem_Ddvm(9,9), DSem_Ddvm(9,9), DMem_Ddvm(9,9), D2Hem_Ddvm2(9,9)

            real(8) :: D2Jvm_Ddvm2(9,9), D2Pinc_Ddvm2(9,9), D2L_Ddvm2(9,9)
            ! -----------------------------------------------------------------------------------


            ! NR Parameters
            ! -----------------------------------------------------------------------------------
            MaxIter = 10
            Tol     = 1.0d-5

            ! Identity - Voigt Notation
            ! -----------------------------------------------------------------------------------
            I = 0.0d0
            I(1) = 1.0d0
            I(5) = 1.0d0
            I(9) = 1.0d0

            ! NR Procedure
            ! -----------------------------------------------------------------------------------

            ! Guess
            dvm_new  = 0.0d0
            gama_new = gama_old

            X(1:6) = Tensor2VoigtToTensor2VoigtSym(dvm_new) 
            X(7)   = gama_new  

            ! NR Loop
            LOCAL_NR:  do it = 1 , MaxIter

                ! *******************************************************************************
                ! VARIABLES
                ! *******************************************************************************

                ! Viscous Variables
                Fvm_new = SingleContractionT2T2Voigt( InverseT2Voigt((I - dt*dvm_new)) , Fvm_old)

                Jvm_new = DeterminantT2Voigt(Fvm_new)

                ! Elastic Variables - Maxwell Branch
                Fem_new = SingleContractionT2T2Voigt( F_new, InverseT2Voigt(Fvm_new) )

                Jem_new = DeterminantT2Voigt(Fem_new)

                Cem_new = SingleContractionT2T2Voigt(TransposeT2Voigt(Fem_new),Fem_new)

                CemInv_new = InverseT2Voigt(Cem_new)

                TrCem_new = Cem_new(1) + Cem_new(5) + Cem_new(9)

                ! Fung with Qe=Neo-Hookean
                !------------------------
                Qe = (Mu_e/2.0d0)*( TrCem_new - 3.0d0 ) - Mu_e*dlog(Jem_new)
                Snhe = Mu_e*( I - CemInv_new )

                ! Elastic Second Piola
                Sem_new = K_e*dexp(Qe)*Snhe

                ! Elastic Modulus
                Dem = 2.0d0*Mu_e*RightSymmetrizationT4Voigt( SquareVoigt(CemInv_new,CemInv_new) )

                Dem = dexp(Qe)*( Dem + 2.0d0*BallVoigt(Snhe,Snhe) )
                !------------------------

                ! Elastic Mandel
                Mem_new = SingleContractionT2T2Voigt(Cem_new,Sem_new)


                ! Viscous Potential Derivatives
                !------------------------------
                DPhivm_Ddvm   = Ni_v*dvm_new

                D2Phivm_Ddvm2 = Ni_v*IdentityT4SymVoigt()
                !------------------------------


                ! *******************************************************************************
                ! RESIDUAL
                ! *******************************************************************************

                !Fvm_new*(Fvm_old)^-1
                !--------------------
                Fvm_new_FvmInv_old = SingleContractionT2T2Voigt( Fvm_new, InverseT2Voigt(Fvm_old) )

                ! Derivative of the Lagrangian Related to the Viscous Rate of Deformation
                !------------------------------
                ! Derivative of the Elastic Helmholtz
                DPinc_Ddvm = - dt*( SingleContractionT2T2Voigt(TransposeT2Voigt(Fvm_new_FvmInv_old), Mem_new) )
                DPinc_Ddvm = 0.50d0*( DPinc_Ddvm + TransposeT2Voigt(DPinc_Ddvm) )

                ! Derivative of Incremental Potential
                DPinc_Ddvm = DPinc_Ddvm + dt*DPhivm_Ddvm

                ! Derivative of the Viscous Jacobian
                DJvm_Ddvm = dt*Jvm_new*Fvm_new_FvmInv_old

                ! Derivative of the Lagrangian
                DL_Ddvm = DPinc_Ddvm + gama_new*DJvm_Ddvm
                !------------------------------

                ! Derivative of the Lagrangian Related to the Lagrangian Multiplier
                !------------------------------
                DL_Dgama = Jvm_new - 1.0d0
                !------------------------------

                ! Residual - Symmetric Voigt
                !------------------------------
                Rm(1:6) = Tensor2VoigtToTensor2VoigtSym(DL_Ddvm)
                Rm(7) = DL_Dgama
                ! *******************************************************************************


                ! *******************************************************************************
                ! STOPPING CRITERION
                ! *******************************************************************************
                Norm_Rm = norm2(Rm)
                if (Norm_Rm .lt. Tol) then
                    return !exit
                endif


                ! *******************************************************************************
                ! JACOBIAN MATRIX
                ! *******************************************************************************

                ! Part 1 - D2L_Ddvm2
                !----------------------------------------------
                B = SquareVoigt(SingleContractionT2T2Voigt(Cem_new,Fvm_new_FvmInv_old),I)

                ! Derivative of the elastic Right Cauchy-Green
                DCem_Ddvm = -2.0d0*dt*RigthLeftSymmetrizationT4Voigt(B)

                ! Derivative of the elastic Second Piola
                DSem_Ddvm = -dt*RightSymmetrizationT4Voigt(DoubleContractionT4T4Voigt(Dem,B))

                ! Derivative of the elastic Mandel
                DMem_Ddvm = DoubleContractionT4T4Voigt(SquareVoigt(I,Sem_new),DCem_Ddvm) + &
                            DoubleContractionT4T4Voigt(SquareVoigt(Cem_new,I),DSem_Ddvm)

                ! Second Derivative of the elastic Helmholtz
                Aux1_T2 = TransposeT2Voigt(InverseT2Voigt(Fvm_old))
                Aux1_T2 = SingleContractionT2T2Voigt(Aux1_T2,Fvm_new_FvmInv_old)

                Aux2_T2 = SingleContractionT2T2Voigt(Fvm_new,Mem_new)
                Aux2_T2 = TransposeT2Voigt(Aux2_T2)

                D2Hem_Ddvm2 = (dt**2.0d0)*SquareVoigt(Aux1_T2,Aux2_T2) + &
                              dt*DoubleContractionT4T4Voigt(SquareVoigt(TransposeT2Voigt(Fvm_new_FvmInv_old),I),DMem_Ddvm)

                D2Hem_Ddvm2 = -RigthLeftSymmetrizationT4Voigt(D2Hem_Ddvm2)

                ! Second Derivative of the Incremental Potential
                D2Pinc_Ddvm2 = D2Hem_Ddvm2 + dt*D2Phivm_Ddvm2

                ! Second Derivativa of the Viscous Jacobian
                D2Jvm_Ddvm2 = BallVoigt(Fvm_new_FvmInv_old,Fvm_new_FvmInv_old) + &
                              SquareVoigt(Fvm_new_FvmInv_old,Fvm_new_FvmInv_old)

                D2Jvm_Ddvm2 = (dt**2.0d0)*Jvm_new*RigthLeftSymmetrizationT4Voigt(D2Jvm_Ddvm2)

                ! Second Derivative of the Lagrangian
                D2L_Ddvm2 = D2Pinc_Ddvm2 + gama_new*D2Jvm_Ddvm2

                !----------------------------------------------

                ! Part 2 and 3 - D2L_DdvmDgama = D2L_DgamaDdvm = DJvm_Ddvm
                !----------------------------------------------

                ! Part 4 - D2L_DgamaDgama = 0
                !----------------------------------------------

                ! Jacobian Matrix - Assembly
                !----------------------------------------------
                Km = 0.0d0

                Km(1:6,1:6) = Tensor4VoigtToTensor4VoigtSym(D2L_Ddvm2)

                Aux1_T2Sym  = Tensor2VoigtToTensor2VoigtSym(DJvm_Ddvm)
                do j = 1,6
                    Km(j,7)   = Aux1_T2Sym(j)
                    Km(7,j)   = Aux1_T2Sym(j)
                end do


                ! *******************************************************************************
                ! SOLVE NR INCREMENT
                ! *******************************************************************************
                call SolveLinearSystemLU(Km, -Rm, deltaX)

                ! *******************************************************************************
                ! UPDATE
                ! *******************************************************************************
                X = X + deltaX

                dvm_new  = Tensor2VoigtSymToTensor2Voigt(X(1:6))
                gama_new = X(7)


            enddo LOCAL_NR

            Status%Error = .true.
            Status%ErrorDescription = 'Max Iteration in Local Newton-Raphson - Viscoelastic Matrix Model'




        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method GetTangentModulus_"NameOfTheMaterialModel"_3D: Routine that evaluates the
        ! Tangente Modulus in Plane Strain analysis.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine GetTangentModulus_ViscoelasticMatrix_3D(this,D)


		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! -----------------------------------------------------------------------------------
             use MathRoutines

            class(ClassViscoelasticMatrix_3D) :: this

            ! Input/Output variables
            ! -----------------------------------------------------------------------------------
            real(8) , dimension(:,:) , intent(inout) :: D

            ! Internal variables
            ! -----------------------------------------------------------------------------------

             ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: vf, Mu, Lambda, cinf1, cinf2, ce1, ce2, nv, dt
            real(8) :: I4_new, I4e_new, J_new, D_Psif_DI4
            real(8) :: dvf_new, lf_new, dvf_old, lvf_old, lvf_new, lef_new

            real(8) :: d2PhiInf_dI42, ddvf_dlf, drf_ddvf, drf_dlf
            real(8) :: dPhie_dI4e, d2Phie_dI4e2, dPhiv_ddv, d2Phiv_ddv2
            real(8) :: Sef_new, Mef_new, Cef_new, Cinff_new, Cf_new

            real(8) :: mX_new(3), M_new(3,3)
            real(8) :: F_new(3,3), C_new(3,3), I(3,3)

            real(8) :: Ivoigt(6), Dm_voigt(6,6), Df_voigt(6,6), M_new_voigt(6)

		    !************************************************************************************

            !************************************************************************************
            ! TANGENT MODULUS
		    !************************************************************************************

             ! Optional: Retrieve Variables
            ! -----------------------------------------------------------------------------------
            !vf      = this%Properties%FiberVolumeFraction
            !Mu      = this%Properties%Mu_Matrix
            !Lambda  = this%Properties%Lambda_Matrix
            !cinf1   = this%Properties%Cinf1_Fiber
            !cinf2   = this%Properties%Cinf2_Fiber
            !ce1     = this%Properties%Ce1_Fiber
            !ce2     = this%Properties%Ce2_Fiber
            !nv      = this%Properties%Ni_Fiber
            !
            !F_new  = this%F
            !mX_new = this%AdditionalVariables%mX
            !
            !dvf_old = this%dvf_old
            !dvf_new = this%dvf_new
            !lvf_old = this%lvf_old
            !lvf_new = this%lvf_new

            ! -----------------------------------------------------------------------------------

            ! Identity
            I = 0.0d0
            I(1,1) = 1.0d0
            I(2,2) = 1.0d0
            I(3,3) = 1.0d0

            Ivoigt = Convert_to_Voigt_3D_Sym(I)

            ! Increment of Time
            dt = this%Time - this%Time_old

            ! Kinematic Variables
            ! -----------------------------------------------------------------------------------

            ! Jacobian
            J_new = det(F_new)

            !Right-Cauchy Green Strain
            C_new = matmul(transpose(F_new),F_new)

            !Material Structural Tensor
            M_new = Tensor_Product(mX_new,mX_new)

            M_new_voigt = Convert_to_Voigt_3D_Sym(M_new)

            !Fourth Invariant
            I4_new = Tensor_Inner_Product(C_new,M_new)

            !Total Stretch
            lf_new = (I4_new)**(0.50d0)

            ! -----------------------------------------------------------------------------------


            ! MATRIX CONTRIBUTION - Compressible Neo-Hookean (Bonet and Wood, 2008)
            ! -----------------------------------------------------------------------------------

            ! Spatial Tangent Modulus - In Voigt Notation
            Dm_voigt = (Lambda/J_new)*Ball_Voigt(Ivoigt,Ivoigt) + (2.0d0/J_new)*(Mu - Lambda*dlog(J_new))*IsymV()
            ! -----------------------------------------------------------------------------------


            ! FIBER CONTRIBUTION
            ! -----------------------------------------------------------------------------------
            if ( lf_new .gt. 1.0d0) then

                ! Equilibrium Modulus - Scalar
                ! -------------------------------------------------------------------------------
                ! POWER LAW - Balzani (2006)
                d2PhiInf_dI42  = cinf1*cinf2*(cinf2-1.0d0)*((I4_new-1.0d0)**(cinf2-2.0d0))

                ! Inf. Scalar Modulus
                Cinff_new = 4.0d0*d2PhiInf_dI42


                ! Non-equilibrium Modulus - Scalar
                ! -------------------------------------------------------------------------------
                ! Stretches
                lef_new = lf_new/lvf_new

                I4e_new = lef_new**2.0d0

                ! Elastic Model - POWER LAW - Balzani (2006)
                dPhie_dI4e   = ce1*ce2*((I4e_new-1.0d0)**(ce2-1.0d0))
                d2Phie_dI4e2 = ce1*ce2*(ce2-1.0d0)*((I4e_new-1.0d0)**(ce2-2.0d0))

                ! Viscous Model - QUADRATIC
                dPhiv_ddv   = nv*dvf_new
                d2Phiv_ddv2 = nv

                ! Stresses
                Sef_new = 2.0d0*dPhie_dI4e

                Mef_new = (lef_new**2.0d0)*Sef_new

                ! Elastic Modulus
                Cef_new = 4.0d0*d2Phie_dI4e2

                if (lef_new .lt. 1.0d0) then
                    Sef_new = 0.0d0
                    Mef_new = 0.0d0
                    Cef_new = 0.0d0
                endif


                ! Computation of the Derivative - Ddvf_Dlf_new
                ! -------------------------------------------------------------------------------
                drf_ddvf = ( (dt*lvf_new/lvf_old)**2.0d0 )*(Mef_new + (lef_new**4.0d0)*Cef_new) + &
                            dt*d2Phiv_ddv2

                drf_dlf = -(dt*lvf_new/(lf_new*lvf_old))*(2.0d0*Mef_new + (lef_new**4.0d0)*Cef_new)

                ddvf_dlf = -drf_dlf/drf_ddvf


                ! Scalar Tangent Modulus
                ! -------------------------------------------------------------------------------
                Cf_new = Cinff_new + Cef_new/(lvf_new**4.0d0) - &
                ( dt/(lf_new*lvf_new*lvf_old) )*( 2.0d0*Sef_new + (lef_new**2.0d0)*Cef_new )*ddvf_dlf


                ! Material Tangent Modulus - In Voigt Notation
                ! -------------------------------------------------------------------------------
                Df_voigt = Cf_new*Ball_Voigt(M_new_voigt,M_new_voigt)

                ! Spatial Tangent Modulus - In Voigt Notation
                ! -------------------------------------------------------------------------------
                Df_voigt = Push_Forward_Voigt(Df_voigt,F_new)


            else

                Df_voigt = 0.0d0

            endif
            ! -----------------------------------------------------------------------------------


            ! TOTAL TANGENT MODULUS
            ! -----------------------------------------------------------------------------------
            D = (1.0d0-vf)*Dm_voigt + vf*Df_voigt

		    !************************************************************************************

        end subroutine
        !==========================================================================================




        !==========================================================================================
        subroutine SwitchConvergedState_ViscoelasticMatrix(this)

            class(ClassViscoelasticMatrix) :: this

            this%Time_old = this%Time

            this%dvm_old  = this%dvm_new

            this%Fvm_old = this%Fvm_new
            
            this%gama_old = this%gama_new


        end subroutine
        !==========================================================================================

        !==========================================================================================
        subroutine GetResult_ViscoelasticMatrix(this, ID , Name , Length , Variable , VariableType  )

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! ---------------------------------------------------------------------------------
            use MathRoutines

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassViscoelasticMatrix) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            integer :: ID

            ! Output variables
            ! -----------------------------------------------------------------------------------
            character(len=*)            :: Name
            integer                     :: Length, VariableType
            real(8) , dimension(:)      :: Variable

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            integer, parameter :: Scalar=1,Vector=2,Tensor=3
            real (8) :: FiberStretch, C(3,3), mX(3), m(3), A(3,3)
		    !************************************************************************************

		    !___________________   WARNIG! DO NOT CHANGE OR ERASE THIS BLOCK    _________________
		    ! Initializing variable name.
		    Name = ''
		    !____________________________________________________________________________________

            select case (ID)

                case(0)

                    Length=4

                case (1)



                case (2)



                case (3)

 

                case (4)



                case default

                    call Error("Error retrieving result :: GetResult_ViscoelasticMatrix")

            end select

        end subroutine
        !==========================================================================================



    end module

