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
module StVenantKirchhoff

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! DECLARATIONS OF VARIABLES
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Modules and implicit declarations
    ! --------------------------------------------------------------------------------------------
    use ConstitutiveModel
    implicit none


 	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel": Attributes and methods of the constitutive model
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type StVenantKirchhoffProperties

        ! Variables of material parameters
        !----------------------------------------------------------------------------------------------
        real(8) :: Poisson , YoungModulus, Lambda, Mu

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel": Attributes and methods of the constitutive model
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , extends(ClassConstitutiveModel) :: ClassStVenantKirchhoff

		! Class Attributes : Usually the state variables (instant and internal variables)
		!----------------------------------------------------------------------------------------
         type (StVenantKirchhoffProperties), pointer :: Properties => null()

        contains

            ! Class Methods
            !----------------------------------------------------------------------------------
             procedure :: ConstitutiveModelConstructor => ConstitutiveModelConstructor_StVenantKirchhoff
             procedure :: ReadMaterialParameters       => ReadMaterialParameters_StVenantKirchhoff
             procedure :: GetResult                    => GetResult_StVenantKirchhoff
             procedure :: SaveConvergedState           => SaveConvergedState_StVenantKirchhoff
             procedure :: CopyProperties               => CopyProperties_StVenantKirchhoff

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel"_PlaneStrain: Attributes and methods of the constitutive model
    ! in Plane Strain analysis.
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , extends(ClassStVenantKirchhoff) :: ClassStVenantKirchhoff_Axisymmetric

         contains

            ! Class Methods
            !----------------------------------------------------------------------------------

             procedure :: UpdateStressAndStateVariables  =>  UpdateStressAndStateVariables_StVenantKirchhoff_Axisymmetric
             procedure :: GetTangentModulus              =>  GetTangentModulus_StVenantKirchhoff_Axisymmetric


    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! Class"NameOfTheMaterialModel"_PlaneStrain: Attributes and methods of the constitutive model
    ! in Three-Dimensional analysis.
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , extends(ClassStVenantKirchhoff) :: ClassStVenantKirchhoff_3D

         contains
            ! Class Methods
            !----------------------------------------------------------------------------------
             procedure :: UpdateStressAndStateVariables  =>  UpdateStressAndStateVariables_StVenantKirchhoff_3D
             procedure :: GetTangentModulus              =>  GetTangentModulus_StVenantKirchhoff_3D

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
        subroutine ConstitutiveModelConstructor_StVenantKirchhoff(this,AnalysisSettings)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use Analysis

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassStVenantKirchhoff) :: this

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
        subroutine ReadMaterialParameters_StVenantKirchhoff(this,DataFile)
            use Parser

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! ---------------------------------------------------------------------------------
            class(ClassStVenantKirchhoff) :: this

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
        subroutine CopyProperties_StVenantKirchhoff(this,Reference)

             class(ClassStVenantKirchhoff) :: this
             class(ClassConstitutiveModel) :: Reference

             select type ( Reference )

                 class is ( ClassStVenantKirchhoff )
                    this%Properties => Reference%Properties
                 class default
                     stop "erro na subroutine CopyProperties_StVenantKirchhoff"

            end select

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method UpdateStateVariables_"NameOfTheMaterialModel"_Axisymmetric: Routine that
        ! contains the algorithm employed to update the state variables in the Three-Dimensional
        ! analysis.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine UpdateStressAndStateVariables_StVenantKirchhoff_Axisymmetric(this)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! ---------------------------------------------------------------------------------
            use MathRoutines

            class(ClassStVenantKirchhoff_Axisymmetric) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            real(8) :: E(3,3), I(3,3), S(3,3)

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: D(6,6), GradU(3,3), F(3,3)
            real(8) :: J, trE, Lambda, Mu

		    !************************************************************************************

            !************************************************************************************
            ! ALGORITHM THAT UPDATES STATE VARIABLES IN PLANE STRAIN ANALYSIS
		    !************************************************************************************
            Lambda = this%Properties%Lambda
            Mu = this%Properties%Mu

            I = 0.0d0
            I(1,1) = 1.0d0
            I(2,2) = 1.0d0
            I(3,3) = 1.0d0


            ! Green-Lagrange Strain
            E = (1.0d0/2.0d0)*( matmul(transpose(this%F),this%F) - I )

            ! Piola 2
            trE = E(1,1) + E(2,2) + E(3,3)

            S = Lambda*trE*I + 2.0d0*Mu*E

            !Cauchy
            J = det(this%F)

            S = matmul(matmul(this%F,S),transpose(this%F))/J

             this%Stress(1)=S(1,1)
             this%Stress(2)=S(2,2)
             this%Stress(3)=S(3,3)
             this%Stress(4)=S(1,2)

		    !************************************************************************************

        end subroutine
        !==========================================================================================

        !==========================================================================================
        ! Method GetTangentModulus_"NameOfTheMaterialModel"_Axisymmetric: Routine that evaluates the
        ! Tangente Modulus in Plane Strain analysis.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine GetTangentModulus_StVenantKirchhoff_Axisymmetric(this,D)


		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! -----------------------------------------------------------------------------------
             use MathRoutines

            class(ClassStVenantKirchhoff_Axisymmetric) :: this

            ! Input/Output variables
            ! -----------------------------------------------------------------------------------
            real(8) , dimension(:,:) , intent(inout) :: D

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: cte
            real(8) , parameter :: R0=0.0d0 , R1=1.0d0 , R2=2.0d0

            integer :: i,j,k,l
            real(8) :: aux, detF, c1, c2, YoungModulus, Poisson
            real(8) :: b(3,3)

            class (StVenantKirchhoffProperties), pointer :: p


		    !************************************************************************************

            !************************************************************************************
            ! TANGENT MODULUS
		    !************************************************************************************

            !Montagem da matriz D espacial
            b = matmul(this%F,Transpose(this%F))

            detF = det(this%F)


            p => this%Properties


            D(1,1:4) = [ Cs(b,p,detF,1,1,1,1) , Cs(b,p,detF,1,1,2,2)  , Cs(b,p,detF,1,1,3,3)  ,  Cs(b,p,detF,1,1,1,2)  ]
            D(2,2:4) = [                        Cs(b,p,detF,2,2,2,2)  , Cs(b,p,detF,2,2,3,3)  ,  Cs(b,p,detF,2,2,1,2)  ]
            D(3,3:4) = [                                                Cs(b,p,detF,3,3,3,3)  ,  Cs(b,p,detF,3,3,1,2)  ]
            D(4,4)   =                                                                           Cs(b,p,detF,1,2,1,2)

		    !************************************************************************************

        end subroutine


        !==========================================================================================
        ! Method UpdateStateVariables_"NameOfTheMaterialModel"_ThreeDimensional: Routine that
        ! contains the algorithm employed to update the state variables in the Three-Dimensional
        ! analysis.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine UpdateStressAndStateVariables_StVenantKirchhoff_3D(this)

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! ---------------------------------------------------------------------------------
            use MathRoutines

            class(ClassStVenantKirchhoff_3D) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            real(8) :: E(3,3), I(3,3), S(3,3)

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: D(6,6)
            real(8) :: J, trE, Lambda, Mu

		    !************************************************************************************

            !************************************************************************************
            ! ALGORITHM THAT UPDATES STATE VARIABLES IN PLANE STRAIN ANALYSIS
		    !************************************************************************************
            Lambda = this%Properties%Lambda
            Mu = this%Properties%Mu


            !Green-Lagrange Strain
            I = 0.0d0
            I(1,1) = 1.0d0
            I(2,2) = 1.0d0
            I(3,3) = 1.0d0

            E = (1.0d0/2.0d0)*( matmul(transpose(this%F),this%F) - I )

            ! Second Piola-Kirchhoff Stress
            trE = E(1,1) + E(2,2) + E(3,3)

            S = Lambda*trE*I + 2.0d0*Mu*E

            !Cauchy
            J = det(this%F)

            S = matmul(matmul(this%F,S),transpose(this%F))/J

             this%Stress(1)=S(1,1)
             this%Stress(2)=S(2,2)
             this%Stress(3)=S(3,3)
             this%Stress(4)=S(1,2)
             this%Stress(5)=S(2,3)
             this%Stress(6)=S(1,3)


		    !************************************************************************************

        end subroutine
        !==========================================================================================


        !==========================================================================================
        ! Method GetTangentModulus_"NameOfTheMaterialModel"_3D: Routine that evaluates the
        ! Tangente Modulus in Plane Strain analysis.
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine GetTangentModulus_StVenantKirchhoff_3D(this,D)


		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Object
            ! -----------------------------------------------------------------------------------
             use MathRoutines

            class(ClassStVenantKirchhoff_3D) :: this

            ! Input/Output variables
            ! -----------------------------------------------------------------------------------
            real(8) , dimension(:,:) , intent(inout) :: D

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: cte, YoungModulus, Poisson
            real(8) , parameter :: R0=0.0d0 , R1=1.0d0 , R2=2.0d0

            integer :: i,j,k,l
            real(8) :: aux, detF
            real(8) :: b(3,3)
            !real(8) :: Cmat(3,3,3,3), Cs(3,3,3,3)

            class (StVenantKirchhoffProperties), pointer :: p

		    !************************************************************************************

            !************************************************************************************
            ! TANGENT MODULUS
		    !************************************************************************************

            !Montagem da matriz D espacial
            b = matmul(this%F,Transpose(this%F))

            detF = det(this%F)

            p => this%Properties

            ! Upper Triangular!!!
            D(1,1:6) = [ Cs(b,p,detF,1,1,1,1) , Cs(b,p,detF,1,1,2,2)  , Cs(b,p,detF,1,1,3,3)  , Cs(b,p,detF,1,1,1,2) , Cs(b,p,detF,1,1,2,3) , Cs(b,p,detF,1,1,1,3)  ]
            D(2,2:6) = [                        Cs(b,p,detF,2,2,2,2)  , Cs(b,p,detF,2,2,3,3)  , Cs(b,p,detF,2,2,1,2) , Cs(b,p,detF,2,2,2,3) , Cs(b,p,detF,2,2,1,3)  ]
            D(3,3:6) = [                                                Cs(b,p,detF,3,3,3,3)  , Cs(b,p,detF,3,3,1,2) , Cs(b,p,detF,3,3,2,3) , Cs(b,p,detF,3,3,1,3)  ]
            D(4,4:6) = [                                                                        Cs(b,p,detF,1,2,1,2) , Cs(b,p,detF,1,2,2,3) , Cs(b,p,detF,1,2,1,3)  ]
            D(5,5:6) = [                                                                                               Cs(b,p,detF,2,3,2,3) , Cs(b,p,detF,2,3,1,3)  ]
            D(6,6)   =                                                                                                                        Cs(b,p,detF,1,3,1,3)

		    !************************************************************************************

        end subroutine

        function Cs(b,p,detF,i,j,k,l) result(x)

            class (StVenantKirchhoffProperties) :: p
            integer :: i,j,k,l
            real(8) :: b(3,3), detF, x

            x = (p%Lambda/detF)*b(i,j)*b(k,l) + (p%Mu/detF)*( b(i,k)*b(j,l) + b(i,l)*b(j,k) )

        end function
        !==========================================================================================

        !==========================================================================================
        subroutine SaveConvergedState_StVenantKirchhoff(this)
            class(ClassStVenantKirchhoff) :: this
        end subroutine
        !==========================================================================================



        !==========================================================================================
        subroutine GetResult_StVenantKirchhoff( this, ID , Name , Length , Variable , VariableType )

            use MathRoutines
            implicit none

            class(ClassStVenantKirchhoff)   :: this
            integer                         :: ID,Length,VariableType
            character(len=*)                :: Name
            real(8) , dimension(:)          :: Variable

            integer,parameter :: Scalar=1,Vector=2,Tensor=3
            real (8) :: h , c(6)
            real (8) :: I(3,3), e(3,3), eV(6)

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
                    Name='AlmansiStrain'
                    VariableType = Tensor
                    Length=size(this%Stress)
                    !-------------------------------------------------------------
                    !Almansi Strain
                    !-------------------------------------------------------------

                    ! Identity
                    I = 0.0d0
                    I(1,1) = 1.0d0
                    I(2,2) = 1.0d0
                    I(3,3) = 1.0d0

                    e = inverse( matmul(this%F, transpose(this%F)) )
                    e = 0.50d0*( I - e )
                    eV = Convert_to_Voigt(e)
                    Variable(1:Length) = eV(1:Length)
                    !-------------------------------------------------------------

                case (3)
                    Name='vonMisesCauchyStress'
                    VariableType = Scalar
                    Length=1
                    !-------------------------------------------------------------
                    ! von Mises Cauchy Stress
                    !-------------------------------------------------------------
                    associate(c => this%Stress)
                        
                    c = this%Stress
                    !h=( c(1) + c(2) + c(4))/3.0d0
                    !Variable(1:Length)  = dsqrt( (3.0d0/2.0d0) * ((c(1)-h)**2.0d0 + (c(2)-h)**2.0d0 + (c(4)-h)**2.0d0 +2.0d0*c(3)*c(3) ) )

                    h=( c(1) + c(2) + c(3))/3.0d0
                    Variable(1:Length)  = dsqrt( (3.0d0/2.0d0) * ((c(1)-h)**2.0d0 + (c(2)-h)**2.0d0 + (c(3)-h)**2.0d0 +2.0d0*c(4)*c(4) +2.0d0*c(5)*c(5) +2.0d0*c(6)*c(6) ) )

                    end associate
                    !-------------------------------------------------------------

                case default
                    call Error("Error retrieving result :: GetResult_StVenantKirchhoff")
            end select
        end subroutine
        !==========================================================================================



    end module

