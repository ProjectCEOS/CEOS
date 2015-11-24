!##################################################################################################
! This module has the attributes and methods for the J2 (von Mises) Plasticity material model.
!--------------------------------------------------------------------------------------------------
! Date: 2014/02
!
! Authors:  Jan-Michel Farias
!           Thiago Andre Carniel
!           Paulo Bastos de Castro
!!------------------------------------------------------------------------------------------------
! Remarks:

!##################################################################################################
module J2Plasticity

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! DECLARATIONS OF VARIABLES
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Modules and implicit declarations
    ! --------------------------------------------------------------------------------------------
    use ConstitutiveModel

    implicit none

    ! Variables of material parameters
    !----------------------------------------------------------------------------------------------
    real(8) , private :: Poisson , YoungModulus , HardeningModulus , YieldStress

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel": Attributes and methods of the constitutive model
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , extends(ClassConstitutiveModel) :: ClassJ2Plasticity

		! Class Attributes : Usually the state variables (instant and internal variables)
		!----------------------------------------------------------------------------------------

        ! Instant N+1
		!----------------------------------------------------------------------------------------
        ! State Variables
        real(8)                              :: AccumulatedPlasticStrain
        real(8) , allocatable , dimension(:) :: PlasticStrain

        ! Variables used to evaluate the tangent modulus
        real(8)                              :: DeltaGamma , q_t
        real(8) , allocatable , dimension(:) :: S_dev_t
        logical                              :: PlasticState

        !Instant N
		!----------------------------------------------------------------------------------------
        ! State Variables
        real(8)                              :: OldAccumulatedPlasticStrain
        real(8) , allocatable , dimension(:) :: OldPlasticStrain

        contains

            ! Class Methods
            !----------------------------------------------------------------------------------
             procedure :: ConstitutiveModelConstructor => ConstitutiveModelConstructor_J2Plasticity
             procedure :: ReadMaterialParameters       => ReadMaterialParameters_J2Plasticity
             procedure :: GetResult                    => GetResult_J2Plasticity
             procedure :: SwitchConvergedState           => SwitchConvergedState_J2

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel"_PlaneStrain: Attributes and methods of the constitutive model
    ! in Plane Strain analysis.
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , extends(ClassJ2Plasticity) :: ClassJ2Plasticity_PlaneStrain

         contains

            ! Class Methods
            !----------------------------------------------------------------------------------
             procedure :: UpdateStressAndStateVariables  =>  UpdateStressAndStateVariables_J2Plasticity_PlaneStrain
             procedure :: GetTangentModulus              =>  GetTangentModulus_J2Plasticity_PlaneStrain

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel"_PlaneStrain: Attributes and methods of the constitutive model
    ! in Three-Dimensional analysis.
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , extends(ClassJ2Plasticity) :: ClassJ2Plasticity_3D

         contains
            ! Class Methods
            !----------------------------------------------------------------------------------
             procedure :: UpdateStressAndStateVariables  =>  UpdateStressAndStateVariables_J2Plasticity_3D
             procedure :: GetTangentModulus              =>  GetTangentModulus_J2Plasticity_3D

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
        subroutine ConstitutiveModelConstructor_J2Plasticity(this,AnalysisSettings)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use Analysis


            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassJ2Plasticity) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            type(ClassAnalysis) :: AnalysisSettings

		    !************************************************************************************

 		    !************************************************************************************
            ! ALLOCATE THE STATE VARIABLES
		    !************************************************************************************

            allocate( this%Stress( AnalysisSettings%StressSize ) ) ; this%Stress= 0.0d0

            allocate( this%S_dev_t( AnalysisSettings%StressSize ) ) ; this%S_dev_t=0.0d0

            allocate( this%PlasticStrain   ( AnalysisSettings%StrainSize ) ) ; this%PlasticStrain= 0.0d0
            allocate( this%OldPlasticStrain( AnalysisSettings%StrainSize ) ) ; this%OldPlasticStrain= 0.0d0

            this%AccumulatedPlasticStrain=0.0d0
            this%OldAccumulatedPlasticStrain=0.0d0
            this%DeltaGamma=0.0d0
            this%q_t=0.0d0


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
        subroutine ReadMaterialParameters_J2Plasticity(this,DataFile)
            use Parser
		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! ---------------------------------------------------------------------------------
            class(ClassJ2Plasticity) :: this

            ! Input variables
            ! ---------------------------------------------------------------------------------
            !integer , intent(in) :: FileNum
              type(ClassParser)::DataFile

		    !************************************************************************************
		    character(len=100),dimension(4)::ListOfOptions,ListOfValues
		    logical,dimension(4)::FoundOption
		    integer::i

            !************************************************************************************
            ! READ THE MATERIAL PARAMETERS
		    !************************************************************************************
            ListOfOptions=["YoungModulus","Poisson","YieldStress","HardeningModulus"]
            call DataFile%FillListOfOptions(ListOfOptions,ListOfValues,FoundOption)
            call DataFile%CheckError
            do i=1,size(FoundOption)
                if (.not.FoundOption(i)) then
                    write(*,*) "ReadMaterialParameters_J2Plasticity :: Option not found ["//trim(ListOfOptions(i))//"]"
                    stop
                endif
            enddo
            call DataFile%ConvertToDouble(ListOfValues(1),YoungModulus)
            if (DataFile%Error) then
                write(*,*) "Could Not read YoungModulus. Found: "//trim(ListOfValues(1))
                stop
            endif
            call DataFile%ConvertToDouble(ListOfValues(2),Poisson)
            if (DataFile%Error) then
                write(*,*) "Could Not read Poisson. Found: "//trim(ListOfValues(2))
                stop
            endif
            call DataFile%ConvertToDouble(ListOfValues(3),YieldStress)
            if (DataFile%Error) then
                write(*,*) "Could Not read YieldStress. Found: "//trim(ListOfValues(3))
                stop
            endif
            call DataFile%ConvertToDouble(ListOfValues(4),HardeningModulus)
            if (DataFile%Error) then
                write(*,*) "Could Not read HardeningModulus. Found: "//trim(ListOfValues(4))
                stop
            endif


		    !************************************************************************************

            !************************************************************************************
            ! READ THE MATERIAL PARAMETERS
		    !************************************************************************************

            !Read(FileNum,*) YoungModulus, Poisson , YieldStress , HardeningModulus

            !************************************************************************************

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method UpdateStateVariables_"NameOfTheMaterialModel"_PlaneStrain: Routine that contains
        ! the algorithm employed to update the state variables in the Plane Strain analysis.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine UpdateStressAndStateVariables_J2Plasticity_PlaneStrain(this)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! ---------------------------------------------------------------------------------
            class(ClassJ2Plasticity_PlaneStrain) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            real(8) :: F(3,3)

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: K,G,E_e_vol_t,p_t,p,E_p_acum_t,phi_t
            real(8) :: E_e(4) , E_e_t(4) , E_e_dev_t(4) ,  S_dev(4)


		    !************************************************************************************
            !nao mexer guardando informação pra usar depois
            F = this%F
            !************************************************************************************

            !************************************************************************************
            ! ALGORITHM THAT UPDATES STRESS AND STATE VARIABLES IN PLANE STRAIN ANALYSIS
		    !************************************************************************************

!            Strain(1) = F(1,1) - 1.0d0
!            Strain(2) = F(2,2) - 1.0d0
!            Strain(3) = F(1,2) + F(2,1)
!
!            E_e=0.0d0 ; E_e_t=0.0d0 ; E_e_dev_t=0.0d0 ;  this%S_dev_t=0.0d0 ; S_dev=0.0d0
!
!            K = YoungModulus / (3.0d0 * (1.0d0-2.0d0*Poisson) )
!            G = YoungModulus / (2.0d0*(1.0d0+Poisson))
!
!            !def elastica trial
!            E_e_t(1:3) = this%Strain - this%OldPlasticStrain
!
!            E_e_vol_t = E_e_t(1) + E_e_t(2)
!            E_e_dev_t = E_e_t - E_e_vol_t*[1.0d0,1.0d0,0.0d0,1.0d0]/3.0d0
!
!            E_p_acum_t = this%OldAccumulatedPlasticStrain
!
!            p_t = K * E_e_vol_t
!
!            this%S_dev_t = G*E_e_dev_t*[2.0d0,2.0d0,1.0d0,2.0d0]
!
!            this%q_t = dsqrt( (3.0d0/2.0d0) * (this%S_dev_t(1)*this%S_dev_t(1) + this%S_dev_t(2)*this%S_dev_t(2) + &
!                       this%S_dev_t(4)*this%S_dev_t(4) +2.0d0*this%S_dev_t(3)*this%S_dev_t(3) ) )
!
!            phi_t = this%q_t - (YieldStress + HardeningModulus*E_p_acum_t)
!
!            if (phi_t<=0.0d0) then
!                this%PlasticState=.false.
!                !incremento elastico
!                this%AccumulatedPlasticStrain = E_p_acum_t
!                this%PlasticStrain = this%OldPlasticStrain
!
!                this%Stress = this%S_dev_t + p_t *[1.0d0,1.0d0,0.0d0,1.0d0]
!                this%DeltaGamma=0.0d0
!
!            else
!
!                this%PlasticState=.true.
!
!                this%DeltaGamma = phi_t / (3.0d0*G+HardeningModulus)
!
!                p = p_t
!                S_dev = (1.0d0 - this%DeltaGamma*3.0d0*G /  this%q_t ) * this%S_dev_t
!
!                this%Stress = S_dev + p *[1.0d0,1.0d0,0.0d0,1.0d0]
!                E_e = S_dev/(2.0d0*G) + E_e_vol_t*[1.0d0,1.0d0,0.0d0,1.0d0]/3.0d0
!
!                this%AccumulatedPlasticStrain = this%OldAccumulatedPlasticStrain + this%DeltaGamma
!                this%PlasticStrain = this%Strain - E_e(1:3)
!
!            endif

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method UpdateStateVariables_"NameOfTheMaterialModel"_ThreeDimensional: Routine that
        ! contains the algorithm employed to update the state variables in the Three-Dimensional
        ! analysis.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine UpdateStressAndStateVariables_J2Plasticity_3D(this)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! ---------------------------------------------------------------------------------
            class(ClassJ2Plasticity_3D) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            real(8) :: F(3,3)


            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: K,G,E_e_vol_t,p_t,p,E_p_acum_t,phi_t
            real(8) :: E_e(6) , E_e_t(6) , E_e_dev_t(6) ,  S_dev(6)


		    !************************************************************************************

             !************************************************************************************
            !nao mexer guardando informação pra usar depois
             F = this%F
            !************************************************************************************

            !************************************************************************************
            ! ALGORITHM THAT UPDATES STATE VARIABLES IN PLANE STRAIN ANALYSIS
		    !************************************************************************************

!            this%Strain(1) = F(1,1) - 1.0d0
!            this%Strain(2) = F(2,2) - 1.0d0
!            this%Strain(3) = F(3,3) - 1.0d0
!            this%Strain(4) = F(2,3) + F(3,2)
!            this%Strain(5) = F(1,3) + F(3,1)
!            this%Strain(6) = F(1,2) + F(2,1)
!
!            E_e=0.0d0 ; E_e_t=0.0d0 ; E_e_dev_t=0.0d0 ;  this%S_dev_t=0.0d0 ; S_dev=0.0d0
!
!            K = YoungModulus / (3.0d0 * (1.0d0-2.0d0*Poisson) )
!            G = YoungModulus / (2.0d0*(1.0d0+Poisson))
!
!            !def elastica trial
!            E_e_t = this%Strain - this%OldPlasticStrain
!
!            E_e_vol_t = E_e_t(1) + E_e_t(2) + E_e_t(3)
!
!            E_e_dev_t = E_e_t - E_e_vol_t*[1.0d0,1.0d0,1.0d0,0.0d0,0.0d0,0.0d0]/3.0d0
!
!            E_p_acum_t = this%OldAccumulatedPlasticStrain
!
!            p_t = K * E_e_vol_t
!
!            this%S_dev_t = G*E_e_dev_t*[2.0d0,2.0d0,2.0d0,1.0d0,1.0d0,1.0d0]
!
!            this%q_t = dsqrt( (3.0d0/2.0d0) * (this%S_dev_t(1)*this%S_dev_t(1) + this%S_dev_t(2)*this%S_dev_t(2) + &
!                                               this%S_dev_t(3)*this%S_dev_t(3) +2.0d0*this%S_dev_t(4)*this%S_dev_t(4)+ &
!                                               2.0d0*this%S_dev_t(5)*this%S_dev_t(5)+2.0d0*this%S_dev_t(6)*this%S_dev_t(6) ) )
!
!            phi_t = this%q_t - (YieldStress + HardeningModulus*E_p_acum_t)
!
!            if (phi_t<=0.0d0) then
!                this%PlasticState=.false.
!                !incremento elastico
!                this%AccumulatedPlasticStrain = E_p_acum_t
!                this%PlasticStrain = this%OldPlasticStrain
!
!                this%Stress = this%S_dev_t + p_t *[1.0d0,1.0d0,1.0d0,0.0d0,0.0d0,0.0d0]
!
!                this%DeltaGamma=0.0d0
!
!            else
!
!                this%PlasticState=.true.
!
!                this%DeltaGamma = phi_t / (3.0d0*G+HardeningModulus)
!
!                p = p_t
!                S_dev = (1.0d0 - this%DeltaGamma*3.0d0*G /  this%q_t ) * this%S_dev_t
!
!                this%Stress = S_dev + p*[1.0d0,1.0d0,1.0d0,0.0d0,0.0d0,0.0d0]
!                E_e = S_dev/(2.0d0*G) + E_e_vol_t*[1.0d0,1.0d0,1.0d0,0.0d0,0.0d0,0.0d0]/3.0d0
!
!                this%AccumulatedPlasticStrain = this%OldAccumulatedPlasticStrain + this%DeltaGamma
!                this%PlasticStrain = this%Strain - E_e
!
!            endif

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method GetTangentModulus_"NameOfTheMaterialModel"_PlaneStrain: Routine that evaluates the
        ! Tangente Modulus in Plane Strain analysis.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine GetTangentModulus_J2Plasticity_PlaneStrain(this,D)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassJ2Plasticity_PlaneStrain) :: this

            ! Input/Output variables
            ! -----------------------------------------------------------------------------------
            real(8) , dimension(:,:) , intent(inout) :: D

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: cte , Is(3,3) , I(3,1) , Id(3,3) , N(3,1) , K , G
            real(8) , parameter :: R0=0.0d0 , R1=1.0d0 , R2=2.0d0

		    !************************************************************************************

            !************************************************************************************
            ! TANGENT MODULUS
		    !************************************************************************************

            if (this%PlasticState==.false.) then

                cte =  YoungModulus/ ((R1+Poisson)* (R1-R2*Poisson))

                D(1,:) = [ R1-Poisson , Poisson    , R0              ]
                D(2,:) = [ Poisson    , R1-Poisson , R0              ]
                D(3,:) = [ R0         , R0         , R1/R2 - poisson ]

                D = cte*D

            else

                K = YoungModulus / (3.0d0 * (1.0d0-2.0d0*Poisson) )
                G = YoungModulus / (2.0d0*(1.0d0+Poisson))

                Is=0.0d0
                Is(1,1)=1.0d0 ; Is(2,2)=1.0d0 ; Is(3,3)=0.5d0
                I(:,1) = [1.0d0 , 1.0d0 , 0.0d0 ]
                Id = Is - matmul( I , transpose(I) ) / 3.0d0

                N(:,1) = this%S_dev_t(1:3) / dsqrt( this%S_dev_t(1)*this%S_dev_t(1) + this%S_dev_t(2)*this%S_dev_t(2) + &
                         this%S_dev_t(4)*this%S_dev_t(4) +2.0d0*this%S_dev_t(3)*this%S_dev_t(3)  )
                !  N(:,1) = this%S_dev_t(1:3) / dsqrt( this%S_dev_t(1)*this%S_dev_t(1) + this%S_dev_t(2)*this%S_dev_t(2) + &
                !        2.0d0*this%S_dev_t(3)*this%S_dev_t(3)  )

                D = 2.0d0*G*(1.0d0-this%DeltaGamma*3.0d0*G/this%q_t)*Id +                                              &
                    6.0d0*G*G*( this%DeltaGamma/this%q_t - 1.0d0/(3.0d0*G+HardeningModulus) )*matmul(N,transpose(N)) + &
                    K*matmul(I,transpose(I))
            endif

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method GetTangentModulus_"NameOfTheMaterialModel"_PlaneStrain: Routine that evaluates the
        ! Tangente Modulus in Plane Strain analysis.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine GetTangentModulus_J2Plasticity_3D(this,D)


		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassJ2Plasticity_3D) :: this

            ! Input/Output variables
            ! -----------------------------------------------------------------------------------
            real(8) , dimension(:,:) , intent(inout) :: D

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: cte , Is(6,6) , I(6,1) , Id(6,6) , N(6,1) , K , G
            real(8) , parameter :: R0=0.0d0 , R1=1.0d0 , R2=2.0d0

		    !************************************************************************************


            !************************************************************************************
            ! TANGENT MODULUS
		    !************************************************************************************
            if (this%PlasticState==.false.) then

                cte =  YoungModulus/ ((R1+Poisson)* (R1-R2*Poisson))

                D(1,:) = [ R1-Poisson , Poisson   , Poisson    , R0             , R0            , R0            ]
                D(2,:) = [ Poisson    , R1-Poisson, Poisson    , R0             , R0            , R0            ]
                D(3,:) = [ Poisson    , Poisson   , R1-Poisson , R0             , R0            , R0            ]
                D(4,:) = [ R0         , R0        , R0         , R1/R2-Poisson  , R0            , R0            ]
                D(5,:) = [ R0         , R0        , R0         , R0             , R1/R2-Poisson , R0            ]
                D(6,:) = [ R0         , R0        , R0         , R0             , R0            , R1/R2-Poisson ]

                D = cte*D

		     else

                K = YoungModulus / (3.0d0 * (1.0d0-2.0d0*Poisson) )
                G = YoungModulus / (2.0d0*(1.0d0+Poisson))

                Is=0.0d0
                Is(1,1)=R1 ; Is(2,2)=R1 ; Is(3,3)=R1 ; Is(4,4)=0.5d0; Is(5,5)=0.5d0; Is(6,6)=0.5d0
                I(:,1) = [R1,R1,R1,R0,R0,R0]
                Id = Is - matmul( I , transpose(I) ) / 3.0d0

                N(:,1) = this%S_dev_t / dsqrt( this%S_dev_t(1)*this%S_dev_t(1) + this%S_dev_t(2)*this%S_dev_t(2) + &
                                               this%S_dev_t(3)*this%S_dev_t(3) +2.0d0*this%S_dev_t(4)*this%S_dev_t(4)+ &
                                               2.0d0*this%S_dev_t(5)*this%S_dev_t(5)+2.0d0*this%S_dev_t(6)*this%S_dev_t(6))

                D = 2.0d0*G*(1.0d0-this%DeltaGamma*3.0d0*G/this%q_t)*Id +                                              &
                    6.0d0*G*G*( this%DeltaGamma/this%q_t - 1.0d0/(3.0d0*G+HardeningModulus) )*matmul(N,transpose(N)) + &
                    K*matmul(I,transpose(I))

            endif

        end subroutine
        !==========================================================================================

        subroutine SwitchConvergedState_J2(this)

            class(ClassJ2Plasticity) :: this
            this%OldPlasticStrain = this%PlasticStrain
            this%OldAccumulatedPlasticStrain = this%AccumulatedPlasticStrain

        end subroutine

        !==========================================================================================
        subroutine GetResult_J2Plasticity(this, ID , Name , Length , Variable , VariableType  )
            implicit none
            class(ClassJ2Plasticity) :: this
            integer                   :: ID,Length,VariableType
            character(len=*)          :: Name
            real(8) , dimension(:)    :: Variable

            integer,parameter :: Scalar=1,Vector=2,Tensor=3
            real (8) :: h , c(4)

            Name=''

            select case (ID)
                case(0)
                    Length=3

                case(1)
                    Name='Stress'
                    VariableType=Tensor
                    Length=size(this%Stress)
                    Variable(1:Length) = this%Stress

                case (2)
                    !Name='Strain'
                    !VariableType = Tensor
                    !Length=size(this%Strain)
                    !Variable(1:Length) =this%Strain

                case (3)
                    Name='von Mises Stress'
                    VariableType = Scalar
                    Length=1
                    !associate(c => this%Stress)
                    c = this%Stress
                    h=( c(1) + c(2) + c(4))/3.0d0
                    !Variable(1:Length)  = dsqrt( (3.0d0/2.0d0) * ((c(1)-h)**2.0d0 + (c(2)-h)**2.0d0 + (c(4)-h)**2.0d0 +2.0d0*c(3)*c(3) ) )

                    !h=( c(1) + c(2) + c(3))/3.0d0
                    Variable(1:Length)  = this%Q_t ! dsqrt( (3.0d0/2.0d0) * ((c(1)-h)**2.0d0 + (c(2)-h)**2.0d0 + (c(3)-h)**2.0d0 +2.0d0*c(4)*c(4) +2.0d0*c(5)*c(5) +2.0d0*c(6)*c(6) ) )

                    !end associate

                case default
                    call Error("Error retrieving result :: GetResult_J2Plasticity")
            end select
        end subroutine




end module


