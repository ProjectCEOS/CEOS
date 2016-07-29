!##################################################################################################
! INCLUDE AUXILIAR MODULES
!##################################################################################################
include 'SubError.f90'
include 'ModStatus.f90'
include 'ModIO.f90'
include 'ModCharacter.f90'
include 'ModParser.f90'
include 'ModAnalysis.f90'
include 'ModMathRoutines.f90'
include 'ModContinuumMechanics.f90'
include 'ModConstitutiveModel.f90'
include 'ModInterface_UMAT_CEOS.f90'
!##################################################################################################




! ===================================================================================
! INCLUDE THE MODULE OF YOUR MATERIAL MODEL
! -----------------------------------------------------------------------------------
include 'ModNeoHookeanIsochoric.f90'
! ===================================================================================




!##################################################################################################
!
! Abaqus UMAT - User subroutine to define a material's mechanical behavior.
!
!##################################################################################################
SUBROUTINE UMAT (STRESS , STATEV , DDSDDE , SSE    , SPD    , SCD,     &
                 RPL    , DDSDDT , DRPLDE , DRPLDT , STRAN  , DSTRAN , &
                 TIME   , DTIME  , TEMP   , DTEMP  , PREDEF , DPRED  , &
                 CMNAME , NDI    , NSHR   , NTENS  , NSTATV , PROPS  , &
                 NPROPS , COORDS , DROT   , PNEWDT , CELENT , DFGRD0 , &
                 DFGRD1 , NOEL   , NPT    , LAYER  , KSPT   , JSTEP  , &
                 KINC )


    !************************************************************************************
    ! DECLARATIONS OF VARIABLES
    !************************************************************************************

    ! ===================================================================================
    ! USE THE MODULE OF YOUR MATERIAL MODEL
    ! -----------------------------------------------------------------------------------
    use NeoHookeanIsochoric
    ! ===================================================================================
    
    
    use ModInterface_UMAT_CEOS
    implicit real*8 (a-h,o-z)

    ! Object
    ! -----------------------------------------------------------------------------------
    type(ClassStatus) :: Status
    
    
    ! ===================================================================================
    ! CREATE YOUR MATERIAL MODEL
    ! -----------------------------------------------------------------------------------
    type(ClassNeoHookeanIsochoric_3D) :: Model
    ! ===================================================================================


    ! Input/Output variables
    ! -----------------------------------------------------------------------------------
      CHARACTER*80  CMNAME
      DIMENSION     STATEV(NSTATV), DDSDDE(NTENS,NTENS), DDSDDT(NTENS),                 &
                    DRPLDE(NTENS), STRAN(NTENS),   DSTRAN(NTENS),       TIME(2),        &
                    PREDEF(1),     DPRED(1),       COORDS(3),           DROT(3,3),      &
                    DFGRD0(3,3),   DFGRD1(3,3),    JSTEP(4) ,           PROPS(NPROPS)

      real(8), dimension(NTENS), target :: STRESS
    !************************************************************************************

    ! Variables used by CEOS
    ! -----------------------------------------------------------------------------------
    ! DFGRD1 - Deformation gradient tensor at the end of the increment.
    ! STRESS - Cauchy stress in Voigt symmetric notation.
    ! DDSDDE - Tangent modulus C in Voigt symmetric notation. Considering T=C*D, where
    !          T is the Jaumann rate of the Kirchhoff stress and D is the Rate of deformation.
    ! STATEV - Array containing the state variables (internal variables) at the beginning of
    !          the increment. The dimension of STATEV is set by Depvar variable during the
    !          definition of the User Material into Abaqus/CAE.
    ! TIME  -  TIME(2) Value of total time at the beginning of the current increment.
    ! DTIME -  Time increment.
    ! PROPS -  User-specified array of material constants associated with this user material.
    ! -----------------------------------------------------------------------------------



    !************************************************************************************
    ! INTERFACE UMAT-CEOS
    !************************************************************************************

    ! Loading variables from UMAT to CEOS constitutive model structure:
    ! -----------------------------------------------------------------------------------
    !   - Properties                    -   PROPS
    !   - Current time (t_n+1)          -   TIME(2)+DTIME
    !   - Deformation gradient (F_n+1)  -   DFGRD1
    !   - Internal variables (iv_n)     -   STATEV
    ! -----------------------------------------------------------------------------------
    call Model % LoadPropertiesFromVector( PROPS )
    call Model % LoadInternalVariablesFromVector( STATEV )
    Model % Time =  TIME(2)+DTIME
    Model % F    =  DFGRD1

    model%Stress => STRESS

    ! Solve constitutive model by CEOS routines - Update Cauchy stress and state variables
    ! -----------------------------------------------------------------------------------
    call Model%UpdateStressAndStateVariables(Status)
    
    ! Cutback Control
    if (Status%Error) then
        PNEWDT = 0.50d0
        return
    endif

    ! Compute spatial tangent modulus - CEOS
    ! -----------------------------------------------------------------------------------
    call Model%GetTangentModulus(DDSDDE)


    ! Transform Cauchy stress and spatial tangent modulus from CEOS to UMAT:
    !   - Transform spatial tangent modulus to tangent modulus used by Abaqus
    !   - Transform Cauchy stress and modulus in correspondig symmetric Voigt mapping used by UMAT
    ! -----------------------------------------------------------------------------------
    call TransformStressAndModulusFromCEOStoUMAT(STRESS, DDSDDE)


    ! Export Cauchy stress and internal variables from CEOS to UMAT:
    !   - transform it to UMAT tangent modulus
    ! -----------------------------------------------------------------------------------
    call Model%ExportInternalVariablesToVector(STATEV)

    ! Deallocate Properties and Destructor
    ! -----------------------------------------------------------------------------------
    if (associated(Model%Properties)) deallocate(Model%Properties)
    
    call Model%ConstitutiveModelDestructor

    !************************************************************************************



RETURN
END
!##################################################################################################

