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

    character(len=255)                            :: SettingsFileName , PostProcessingFileName
    Logical                                       :: TaskSolve , TaskPostProcess
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

! TODO (Thiago#2#11/17/15): Trocar todos o nome dos módulos para Mod'NOME'


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	!                                       MAIN PROGRAM
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	write(*,*) '---------------------------------------------------------'
    write(*,*) '                         CEOS'
    write(*,*) '---------------------------------------------------------'

    !**********************************************************************************************
    ! Reading Arguments
    !**********************************************************************************************
    call ArgumentHandler(TaskSolve , TaskPostProcess ,SettingsFileName , PostProcessingFileName)
    !**********************************************************************************************
    write(*,*) ''
    write(*,*) 'Settings File Name: '//trim(SettingsFileName)
    write(*,*) ''
    if (TaskSolve) then
        write(*,*) 'Problem will be solved'
    else
        write(*,*) 'Problem will *NOT* be solved'
    endif
    write(*,*) ''
    if (TaskPostProcess) then
        write(*,*) 'Problem will be postprocessed'
        write(*,*) 'PostProcessing File Name: '//trim(PostProcessingFileName)
    else
        write(*,*) 'Problem will *NOT* be postprocessed'
    endif
    write(*,*) ''


    ! Reading FEM settings file
    ! ---------------------------------------------------------------------------------------------
	call FEMAnalysis_1%ReadInputData( SettingsFileName )

	if (TaskSolve) then
        !**********************************************************************************************
        ! SOLVING A FINITE ELEMENT ANALYSIS
        !**********************************************************************************************

        write(*,*) '---------------------------------------------------------'
        write(*,*) 'SOLVING'
        write(*,*) '---------------------------------------------------------'

        ! Solve FEM Analysis
        ! ---------------------------------------------------------------------------------------------
        call AnalysisTime%Start

        ! Allocating memory for the sparse matrix (pre-assembling)
        ! ---------------------------------------------------------------------------------------------
        call FEMAnalysis_1%AllocateGlobalSparseStiffnessMatrix

        call FEMAnalysis_1%Solve

        call AnalysisTime%Stop
        write(*,*) ''
        write(*,*) ''
        write(*,*) 'Finite Element Analysis: CPU Time =', AnalysisTime%GetElapsedTime() , '[s]'
        write(*,*) ''
        write(*,*) ''
        !**********************************************************************************************
    endif

    if (TaskPostProcess) then
    !**********************************************************************************************
    ! POSTPROCESSING THE FINITE ELEMENT ANALYSIS RESULTS
    !**********************************************************************************************

        call AnalysisTime%Start
        write(*,*) '---------------------------------------------------------'
        write(*,*) 'POST PROCESSING'
        write(*,*) '---------------------------------------------------------'
        write(*,*) ''

        ! Reading Probes Input File
        ! ---------------------------------------------------------------------------------------------
        call ReadPostProcessingInputFile(PostProcessingFileName,ProbeList,PostProcessor)
        write(*,*) ''

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
    endif


    ! TODO (Thiago#1#11/03/15): Padronizar gerenciamento de erros.


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end program MAIN
!##################################################################################################


! TODO (Jan#2#11/17/15): ver com o thiago onde colocar isto aqui

subroutine ArgumentHandler(TaskSolve , TaskPostProcess ,SettingsFileName , PostProcessingFileName)
    use modTools
    use Parser
    implicit none
    character(len=255)                            :: SettingsFileName , PostProcessingFileName
    Logical                                       :: TaskSolve , TaskPostProcess

    type(ClassParser) :: Comp
    integer :: i
    character(len=255) , allocatable , dimension(:) :: Commands , aux
    character(len=255)::CommandLine
    logical :: FoundSettingsFileName

    TaskSolve=.false.
    TaskPostProcess = .false.
    FoundSettingsFileName = .false.

    call comp%setup

    call get_command(CommandLine)

    call SplitString(CommandLine,Commands , "/")
    if (size(commands)<=1) then
        stop "ERROR :: CommandLine not consistent"
    endif

    do i=2,size(Commands)

        if (Comp%CompareStrings(Commands(i),"solve")) then
            TaskSolve=.true.
            cycle
        elseif (Comp%CompareStrings(Commands(i),"help")) then
            call Help
        elseif (Comp%CompareStrings(Commands(i),"?")) then
            call Help
        endif

        call SplitString( Commands(i) , aux , " ")

        if (size(aux).ne.2) then
            write(*,*) 'WARNING :: Invalid Argument: '//trim(commands(i))
            cycle
        endif

        if (comp%CompareStrings(aux(1),"settings")) then
            SettingsFileName = trim( aux(2) )
            FoundSettingsFileName = .true.
            cycle
        elseif (comp%CompareStrings(aux(1),"Post Process")) then
            TaskPostProcess = .true.
            PostProcessingFileName = trim( aux(2) )
        else
            write(*,*) "WARNING :: Option not identified: ["//trim(aux(1))//"]. Ignoring..."
            cycle
        endif
    enddo

    if (.not.FoundSettingsFileName) then
        stop "ERROR :: Settings file missing"
    elseif (.not.(TaskPostProcess.or.TaskSolve)) then
        stop "ERROR :: Please inform a task."
    endif

end subroutine

subroutine Help()
    write(*,*) '---------------------------------------------------------'
    write(*,*) ' CEOS - Command Line Help'
    write(*,*) '---------------------------------------------------------'
    write(*,*) 'CEOS /Settings {FILENAME} /Solve /PostProcess {FILENAME}'
    write(*,*) ''
    write(*,*) '  /Settings {FILENAME}      Specifies the settings file.'
    write(*,*) '                            {FILENAME} must be a single'
    write(*,*) '                            word file'
    write(*,*) ''
    write(*,*) '  /Solve                    Solves the problem specified '
    write(*,*) '                            by the settings file '
    write(*,*) ''
    write(*,*) '  /PostProcess {FILENAME}   Specifies the postprocessing'
    write(*,*) '                            file and activates the '
    write(*,*) '                            postprocessing mode. '
    write(*,*) '                            {FILENAME} must be a '
    write(*,*) '                            single word file'
    write(*,*) ''
    write(*,*) '---------------------------------------------------------'
    stop
end subroutine































