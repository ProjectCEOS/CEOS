!##################################################################################################
! This routine calculates the global tangent stiffness matrix.
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
subroutine TangentStiffnessMatrix( AnalysisSettings , ElementList , Kg )

    !************************************************************************************
    ! DECLARATIONS OF VARIABLES
    !************************************************************************************
    ! Modules and implicit declarations
    ! -----------------------------------------------------------------------------------
    use Analysis
    use ElementLibrary
    use Interfaces
    use GlobalSparseMatrix
    use Timer


    implicit none


    ! Input variables
    ! -----------------------------------------------------------------------------------
    type(ClassAnalysis)                       , intent(inout) :: AnalysisSettings
    type(ClassElementsWrapper) , dimension(:) , intent(in) :: ElementList
    type(ClassGlobalSparseMatrix)             , intent(in) :: Kg
    type(ClassTimer)                                       :: Tempo

    ! Internal variables
    ! -----------------------------------------------------------------------------------
    integer :: e , nDOFel
    integer , pointer , dimension(:)   :: GM
    real(8) , pointer , dimension(:,:) :: Ke
    real(8) :: val
    !************************************************************************************

    !************************************************************************************
    ! GLOBAL TANGENT STIFFNESS MATRIX
    !************************************************************************************

    
    Kg%Val = 0.0d0

    
 
    do  e = 1, size( ElementList )

 
        
        call ElementList(e)%El%GetElementNumberDOF( AnalysisSettings , nDOFel )

        Ke => Ke_Memory( 1:nDOFel , 1:nDOFel )
        GM => GM_Memory( 1:nDOFel )

        call ElementList(e)%El%GetGlobalMapping( AnalysisSettings, GM )

             
        call ElementList(e)%El%ElementStiffnessMatrix( Ke, AnalysisSettings )

 !call tempo%Start 
  
        call AssembleGlobalMatrix( GM, Ke, Kg )

  !call tempo%Stop

    enddo
    


    !************************************************************************************


    
end subroutine
