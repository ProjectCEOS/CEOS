!##################################################################################################
!                                               CEOS
!
! - Plane Strain, Axisymmetric and 3D Analysis.
! - Nonlinear Geometric Analysis (Current Lagrangian Formulation).
! - Nonlinear Constitutive Material Module.
! - Parallel Direct Sparse Solver - PARDISO
! - Full Newton-Raphson Procedure
! - GiD Interface (Pre and Post Processing)
!
!--------------------------------------------------------------------------------------------------
! Date: 2014/02
!
! Authors:  Jan-Michel Farias
!           Thiago Andre Carniel
!           Paulo Bastos de Castro
!!------------------------------------------------------------------------------------------------
! Remarks:
!##################################################################################################
program MAIN


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! DECLARATIONS OF VARIABLES
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! Modules and implicit declarations
	! ---------------------------------------------------------------------------------------------
    use FEMAnalysis
    use ModProbe
    use ModPostProcessors
    use ModExportResultFile
    use modTools
    use Timer
    use Parser

    implicit none

    ! Objects
	! ---------------------------------------------------------------------------------------------
    type (ClassFEMAnalysis) :: FEMAnalysis_1
    type (ClassProbeWrapper), pointer, dimension(:) :: ProbeList
    class(ClassPostProcessor), pointer :: PostProcessor


    ! Internal variables
	! ---------------------------------------------------------------------------------------------

    character(len=100), allocatable, dimension(:) :: Args
    type(ClassTimer)                              :: AnalysisTime
    type(ClassParser)                             :: Comp

    character(len=255)                            :: SettingsFileName
    character(len=255)                            :: PostProcessingFileName
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

! TODO (Thiago#2#11/17/15): Trocar todos o nome dos módulos para Mod'NOME'


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	!                                       MAIN PROGRAM
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


    !**********************************************************************************************
    ! Reading Arguments
    !**********************************************************************************************
    call GetArgs(Args,.false.)

    call Comp%Setup()

    if ( size(Args) == 2 ) then

    elseif ( size(Args) == 4 ) then

        if( Comp%CompareStrings('/Solve',Args(1)) ) then
            SettingsFileName = trim(Args(2))
        else
            stop 'ERROR: Expected /Solve in .bat file'
        endif

        if( Comp%CompareStrings('/PostProcessing',Args(3)) ) then
            PostProcessingFileName = trim(Args(4))
        else
            stop 'ERROR: Expected /PostProcessing in .bat file'
        endif

    else
        stop 'ERROR: Arguments not consistent in .bat file.'
    endif

    !**********************************************************************************************



    !**********************************************************************************************
    ! SOLVING A FINITE ELEMENT ANALYSIS
    !**********************************************************************************************
    call AnalysisTime%Start

    ! Reading FEM input files
    ! ---------------------------------------------------------------------------------------------
	call FEMAnalysis_1%ReadInputData( SettingsFileName )

    ! Allocating memory for the sparse matrix (pre-assembling)
	! ---------------------------------------------------------------------------------------------
	call FEMAnalysis_1%AllocateGlobalSparseStiffnessMatrix

    ! Solve FEM Analysis
	! ---------------------------------------------------------------------------------------------
    call FEMAnalysis_1%Solve

    call AnalysisTime%Stop
    write(*,*) ''
    write(*,*) ''
    write(*,*) 'Finite Element Analysis: CPU Time =', AnalysisTime%GetElapsedTime() , '[s]'
    write(*,*) ''
    write(*,*) ''
    !**********************************************************************************************






    !**********************************************************************************************
    ! POSTPROCESSING THE FINITE ELEMENT ANALYSIS RESULTS
    !**********************************************************************************************
! TODO (Thiago#2#11/09/15): Puxar via argumento o nome dos arquivos dos probes de dos pos processadores.
! TODO (Thiago#1#11/09/15): Criar um arquivo único com os probes e pos processadores

    call AnalysisTime%Start
    write(*,*) '---------------------------------------------------------'
    write(*,*) 'POST PROCESSING'
    write(*,*) '---------------------------------------------------------'
    write(*,*) ''

    ! Reading Probes Input File
	! ---------------------------------------------------------------------------------------------
    call ReadPostProcessingInputFile(PostProcessingFileName,ProbeList,PostProcessor)
    write(*,*) ''

    ! Reading Post Processors Input File
	! ---------------------------------------------------------------------------------------------
    !call ReadPostProcessorsInputFile(PostProcessorsFileName,PostProcessorsList)

    ! Post Processing Results
	! ---------------------------------------------------------------------------------------------
    call PostProcessingResults(ProbeList,PostProcessor,FEMAnalysis_1)

    call AnalysisTime%Stop
    write(*,*) ''
    write(*,*) ''
    write(*,*) 'CPU Time =', AnalysisTime%GetElapsedTime() , '[s]'
    write(*,*) '---------------------------------------------------------'
    write(*,*) ''
    write(*,*) ''
    !**********************************************************************************************




	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


! TODO (Thiago#1#11/03/15): Padronizar gerenciamento de erros.







end program MAIN
!##################################################################################################

