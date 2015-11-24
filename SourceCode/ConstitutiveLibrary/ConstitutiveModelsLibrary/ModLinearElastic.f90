!##################################################################################################
! This module has the attributes and methods for the Linear Elastic material model.
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
module LinearElastic

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! DECLARATIONS OF VARIABLES
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Modules and implicit declarations
    ! --------------------------------------------------------------------------------------------
    use ConstitutiveModel
    use ModStatus

    implicit none



 	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel": Attributes and methods of the constitutive model
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type LinearElasticProperties

        ! Variables of material parameters
        !----------------------------------------------------------------------------------------------
        real(8) :: Poisson , YoungModulus, Lambda, Mu

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel": Attributes and methods of the constitutive model
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , extends(ClassConstitutiveModel) :: ClassLinearElastic

		! Class Attributes : Usually the state variables (instant and internal variables)
		!----------------------------------------------------------------------------------------
         type (LinearElasticProperties), pointer :: Properties => null()

        contains

            ! Class Methods
            !----------------------------------------------------------------------------------
             procedure :: ConstitutiveModelConstructor => ConstitutiveModelConstructor_LinearElastic
             procedure :: ReadMaterialParameters       => ReadMaterialParameters_LinearElastic
             procedure :: GetResult                    => GetResult_LinearElastic
             procedure :: SwitchConvergedState           => SwitchConvergedState_LinearElastic
             procedure :: CopyProperties               => CopyProperties_LinearElastic

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel"_PlaneStrain: Attributes and methods of the constitutive model
    ! in Plane Strain analysis.
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    !type , extends(ClassLinearElastic) :: ClassLinearElastic_PlaneStrain
    !
    !     contains
    !
    !        ! Class Methods
    !        !----------------------------------------------------------------------------------
    !
    !         procedure :: UpdateStressAndStateVariables  =>  UpdateStressAndStateVariables_LinearElastic_PlaneStrain
    !         procedure :: GetTangentModulus              =>  GetTangentModulus_LinearElastic_PlaneStrain
    !
    !
    !end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel"_PlaneStrain: Attributes and methods of the constitutive model
    ! in Plane Strain analysis.
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    !type , extends(ClassLinearElastic) :: ClassLinearElastic_Axisymmetric
    !
    !     contains
    !
    !        ! Class Methods
    !        !----------------------------------------------------------------------------------
    !
    !         procedure :: UpdateStressAndStateVariables  =>  UpdateStressAndStateVariables_LinearElastic_Axisymmetric
    !         procedure :: GetTangentModulus              =>  GetTangentModulus_LinearElastic_Axisymmetric
    !
    !
    !end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel"_PlaneStrain: Attributes and methods of the constitutive model
    ! in Three-Dimensional analysis.
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , extends(ClassLinearElastic) :: ClassLinearElastic_ThreeDimensional

         contains
            ! Class Methods
            !----------------------------------------------------------------------------------
             procedure :: UpdateStressAndStateVariables  =>  UpdateStressAndStateVariables_LinearElastic_ThreeDimensional
             procedure :: GetTangentModulus              =>  GetTangentModulus_LinearElastic_ThreeDimensional

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
        subroutine ConstitutiveModelConstructor_LinearElastic(this,AnalysisSettings)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use Analysis

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassLinearElastic) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            type(ClassAnalysis) :: AnalysisSettings

		    !************************************************************************************

 		    !************************************************************************************
            ! ALLOCATE THE STATE VARIABLES
		    !************************************************************************************

            allocate( this%Stress( AnalysisSettings%StressSize ) ) ; this%Stress= 0.0d0

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
        subroutine ReadMaterialParameters_LinearElastic(this,DataFile)
            use Parser

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! ---------------------------------------------------------------------------------
            class(ClassLinearElastic) :: this

            ! Input variables
            ! ---------------------------------------------------------------------------------
            !integer , intent(in) :: FileNum
            type(ClassParser)::DataFile

		    !************************************************************************************
		    character(len=100),dimension(2)::ListOfOptions,ListOfValues
		    logical,dimension(2)::FoundOption
		    integer::i

            !************************************************************************************
            ! READ THE MATERIAL PARAMETERS
		    !************************************************************************************
            allocate (this%Properties)

            ListOfOptions=["YoungModulus","Poisson"]

            call DataFile%FillListOfOptions(ListOfOptions,ListOfValues,FoundOption)
            call DataFile%CheckError

            do i=1,size(FoundOption)
                if (.not.FoundOption(i)) then
                    write(*,*) "ReadMaterialParameters_LinearElastic :: Option not found ["//trim(ListOfOptions(i))//"]"
                    stop
                endif
            enddo

            call DataFile%ConvertToDouble(ListOfValues(1),this%Properties%YoungModulus)

            if (DataFile%Error) then
                write(*,*) "Could Not read YoungModulus. Found: "//trim(ListOfValues(1))
                stop
            endif

            call DataFile%ConvertToDouble(ListOfValues(2),this%Properties%Poisson)

            if (DataFile%Error) then
                write(*,*) "Could Not read Poisson. Found: "//trim(ListOfValues(2))
                stop
            endif

            this%Properties%Lambda = this%Properties%Poisson*this%Properties%YoungModulus /( (1.0d0 + this%Properties%Poisson)*(1.0d0-2.0d0*this%Properties%Poisson) )
            this%Properties%Mu = this%Properties%YoungModulus / (2.0d0*(1.0d0+this%Properties%Poisson))
            !************************************************************************************
            ! READ THE MATERIAL PARAMETERS
		    !************************************************************************************

            !Read(FileNum,*) YoungModulus, Poisson

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
        subroutine CopyProperties_LinearElastic(this,Reference)

             class(ClassLinearElastic) :: this
             class(ClassConstitutiveModel) :: Reference

             select type ( Reference )

                 class is ( ClassLinearElastic )
                    this%Properties => Reference%Properties
                 class default
                     stop "erro na subroutine CopyProperties_LinearElastic"

            end select

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method UpdateStateVariables_"NameOfTheMaterialModel"_PlaneStrain: Routine that contains
        ! the algorithm employed to update the state variables in the Plane Strain analysis.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
!        subroutine UpdateStressAndStateVariables_LinearElastic_PlaneStrain(this,Status)
!
!		    !************************************************************************************
!            ! DECLARATIONS OF VARIABLES
!		    !************************************************************************************
!            ! Object
!            ! ---------------------------------------------------------------------------------
!            class(ClassLinearElastic_PlaneStrain) :: this
!
!            ! Input variables
!            ! -----------------------------------------------------------------------------------
!            real(8) :: F(3,3)
!
!            ! Internal variables
!            ! -----------------------------------------------------------------------------------
!            real(8) :: D(3,3)
!
!		    !************************************************************************************
!      !      !nao mexer guardando informa��o pra usar depois
!      !      F = this%F
!      !      !************************************************************************************
!      !
!      !      !************************************************************************************
!      !      ! ALGORITHM THAT UPDATES STATE VARIABLES IN PLANE STRAIN ANALYSIS
!		    !!************************************************************************************
!      !
!      !      this%Strain(1) = F(1,1) - 1.0d0
!      !      this%Strain(2) = F(2,2) - 1.0d0
!      !      this%Strain(3) = F(1,2) + F(2,1)
!      !
!      !      call GetTangentModulus_LinearElastic_PlaneStrain(this,D)
!      !
!      !      this%Stress(1:3) = matmul( D , this%Strain )
!      !
!      !      this%Stress(4) = Poisson * ( this%Stress(1) + this%Stress(2) )
!
!
!
!		    !************************************************************************************
!
!        end subroutine
!        !==========================================================================================
!
!        !==========================================================================================
!        ! Method GetTangentModulus_"NameOfTheMaterialModel"_PlaneStrain: Routine that evaluates the
!        ! Tangente Modulus in Plane Strain analysis.
!        !------------------------------------------------------------------------------------------
!        ! Modifications:
!        ! Date:         Author:
!        !==========================================================================================
!        subroutine GetTangentModulus_LinearElastic_PlaneStrain(this,D)
!
!		    !************************************************************************************
!            ! DECLARATIONS OF VARIABLES
!		    !************************************************************************************
!            ! Object
!            ! -----------------------------------------------------------------------------------
!            class(ClassLinearElastic_PlaneStrain) :: this
!
!            ! Input/Output variables
!            ! -----------------------------------------------------------------------------------
!            real(8) , dimension(:,:) , intent(inout) :: D
!
!            ! Internal variables
!            ! -----------------------------------------------------------------------------------
!            real(8) :: cte
!            real(8) , parameter :: R0=0.0d0 , R1=1.0d0 , R2=2.0d0
!
!		    !************************************************************************************
!
!            !************************************************************************************
!            ! TANGENT MODULUS
!		    !************************************************************************************
!
!            !cte =  YoungModulus/ ((R1+Poisson)* (R1-R2*Poisson))
!            !
!            !D(1,:) = [ R1-Poisson , Poisson    , R0              ]
!            !D(2,:) = [ Poisson    , R1-Poisson , R0              ]
!            !D(3,:) = [ R0         , R0         , R1/R2 - poisson ]
!            !
!            !D = cte*D
!
!		    !************************************************************************************
!
!        end subroutine
!        !==========================================================================================
!
!
!        !==========================================================================================
!        ! Method UpdateStateVariables_"NameOfTheMaterialModel"_Axisymmetric: Routine that
!        ! contains the algorithm employed to update the state variables in the Three-Dimensional
!        ! analysis.
!        !------------------------------------------------------------------------------------------
!        ! Modifications:
!        ! Date:         Author:
!        !==========================================================================================
!        subroutine UpdateStressAndStateVariables_LinearElastic_Axisymmetric(this)
!
!		    !************************************************************************************
!            ! DECLARATIONS OF VARIABLES
!		    !************************************************************************************
!            ! Object
!            ! ---------------------------------------------------------------------------------
!            use MathRoutines
!
!            class(ClassLinearElastic_Axisymmetric) :: this
!
!
!            ! Internal variables
!            ! -----------------------------------------------------------------------------------
!            real(8) :: D(4,4), F(3,3)
!
!		    !************************************************************************************
!
!            !************************************************************************************
!            ! ALGORITHM THAT UPDATES STATE VARIABLES IN PLANE STRAIN ANALYSIS
!		    !************************************************************************************
!
!!            ! Engineering Strain
!!            F = this%F
!!
!!            this%Strain(1) = F(1,1) - 1.0d0
!!            this%Strain(2) = F(2,2) - 1.0d0
!!            this%Strain(3) = F(3,3) - 1.0d0
!!            this%Strain(4) = F(1,2) + F(2,1)
!!
!!            call GetTangentModulus_LinearElastic_Axisymmetric(this,D)
!!
!!            ! Engineering Stress
!!            this%Stress = matmul( D , this%Strain )
!
!
!		    !************************************************************************************
!
!        end subroutine
!        !==========================================================================================
!
!        !==========================================================================================
!        ! Method GetTangentModulus_"NameOfTheMaterialModel"_Axisymmetric: Routine that evaluates the
!        ! Tangente Modulus in Plane Strain analysis.
!        !------------------------------------------------------------------------------------------
!        ! Modifications:
!        ! Date:         Author:
!        !==========================================================================================
!        subroutine GetTangentModulus_LinearElastic_Axisymmetric(this,D)
!
!
!		    !************************************************************************************
!            ! DECLARATIONS OF VARIABLES
!		    !************************************************************************************
!            ! Object
!            ! -----------------------------------------------------------------------------------
!             use MathRoutines
!
!            class(ClassLinearElastic_Axisymmetric) :: this
!
!            ! Input/Output variables
!            ! -----------------------------------------------------------------------------------
!            real(8) , dimension(:,:) , intent(inout) :: D
!
!            ! Internal variables
!            ! -----------------------------------------------------------------------------------
!            real(8) :: cte, c1, c2, YoungModulus, Poisson
!
!		    !************************************************************************************
!
!            !************************************************************************************
!            ! TANGENT MODULUS
!		    !************************************************************************************
!            YoungModulus = this%Properties%YoungModulus
!            Poisson = this%Properties%Poisson
!
!
!            cte =  YoungModulus*(1.0d0-Poisson) / ( (1.0d0+Poisson)*(1.0d0-2.0d0*Poisson) )
!
!            c1 = Poisson/(1.0d0-Poisson)
!
!            c2 = (1.0d0-2.0d0*Poisson)/( 2.0d0*(1.0d0-Poisson) )
!
!            D(1,:) = [ 1.0d0    ,   c1      ,   c1      ,   0.0d0  ]
!            D(2,:) = [ c1       ,   1.0d0   ,   c1      ,   0.0d0  ]
!            D(3,:) = [ c1       ,   c1      ,   1.0d0   ,   0.0d0  ]
!            D(4,:) = [ 0.0d0    ,   0.0d0   ,   0.0d0   ,   c2     ]
!
!            D = cte*D
!
!
!
!        end subroutine


        !==========================================================================================
        ! Method UpdateStateVariables_"NameOfTheMaterialModel"_ThreeDimensional: Routine that
        ! contains the algorithm employed to update the state variables in the Three-Dimensional
        ! analysis.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine UpdateStressAndStateVariables_LinearElastic_ThreeDimensional(this,Status)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! ---------------------------------------------------------------------------------
            use MathRoutines

            class(ClassLinearElastic_ThreeDimensional) :: this
            type(ClassStatus) :: Status

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: D(6,6), F(3,3)

		    !************************************************************************************

            !************************************************************************************
            ! ALGORITHM THAT UPDATES STATE VARIABLES IN PLANE STRAIN ANALYSIS
		    !************************************************************************************

!            !Engineering Strain
!            F = this%F
!
!            this%Strain(1) = F(1,1) - 1.0d0
!            this%Strain(2) = F(2,2) - 1.0d0
!            this%Strain(3) = F(3,3) - 1.0d0
!            this%Strain(4) = F(1,2) + F(2,1)
!            this%Strain(5) = F(2,3) + F(3,2)
!            this%Strain(6) = F(1,3) + F(3,1)
!
!
!            call GetTangentModulus_LinearElastic_ThreeDimensional(this,D)
!
!            ! Engineering Stress
!            this%Stress = matmul( D , this%Strain )

		    !************************************************************************************

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method GetTangentModulus_"NameOfTheMaterialModel"_PlaneStrain: Routine that evaluates the
        ! Tangente Modulus in Plane Strain analysis.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine GetTangentModulus_LinearElastic_ThreeDimensional(this,D)


		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! -----------------------------------------------------------------------------------
             use MathRoutines

            class(ClassLinearElastic_ThreeDimensional) :: this

            ! Input/Output variables
            ! -----------------------------------------------------------------------------------
            real(8) , dimension(:,:) , intent(inout) :: D

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: cte, YoungModulus, Poisson
            real(8) , parameter :: R0=0.0d0 , R1=1.0d0 , R2=2.0d0

		    !************************************************************************************

            !************************************************************************************
            ! TANGENT MODULUS
		    !************************************************************************************
            YoungModulus = this%Properties%YoungModulus
            Poisson = this%Properties%Poisson

                cte =  YoungModulus/ ((R1+Poisson)* (R1-R2*Poisson))

                D(1,:) = [ R1-Poisson , Poisson   , Poisson    , R0             , R0            , R0            ]
                D(2,:) = [ Poisson    , R1-Poisson, Poisson    , R0             , R0            , R0            ]
                D(3,:) = [ Poisson    , Poisson   , R1-Poisson , R0             , R0            , R0            ]
                D(4,:) = [ R0         , R0        , R0         , R1/R2-Poisson  , R0            , R0            ]
                D(5,:) = [ R0         , R0        , R0         , R0             , R1/R2-Poisson , R0            ]
                D(6,:) = [ R0         , R0        , R0         , R0             , R0            , R1/R2-Poisson ]

                D = cte*D

		    !************************************************************************************

        end subroutine

        !==========================================================================================


        !==========================================================================================
        subroutine SwitchConvergedState_LinearElastic(this)
            class(ClassLinearElastic) :: this
        end subroutine
        !==========================================================================================

        !==========================================================================================
        subroutine GetResult_LinearElastic(this, ID , Name , Length , Variable , VariableType  )

            implicit none
            class(ClassLinearElastic) :: this
            integer                   :: ID,Length,VariableType
            character(len=*)          :: Name
            real(8) , dimension(:)    :: Variable

            integer,parameter :: Scalar=1,Vector=2,Tensor=3
            real (8) :: h , c(6)

            Name=''

            select case (ID)
                case(0)
                    Length=1
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
                    !Name='von Mises Stress'
                    !VariableType = Scalar
                    !Length=1
                    !associate(c => this%Stress)
                    !c = this%Stress
                    !h=( c(1) + c(2) + c(4))/3.0d0
                    !Variable(1:Length)  = dsqrt( (3.0d0/2.0d0) * ((c(1)-h)**2.0d0 + (c(2)-h)**2.0d0 + (c(4)-h)**2.0d0 +2.0d0*c(3)*c(3) ) )

                    !h=( c(1) + c(2) + c(3))/3.0d0
                    !Variable(1:Length)  = dsqrt( (3.0d0/2.0d0) * ((c(1)-h)**2.0d0 + (c(2)-h)**2.0d0 + (c(3)-h)**2.0d0 +2.0d0*c(4)*c(4) +2.0d0*c(5)*c(5) +2.0d0*c(6)*c(6) ) )

                    !end associate
                case default
                    call Error("Error retrieving result :: GetResult_LinearElastic")
            end select
        end subroutine
        !==========================================================================================



    end module

