module ModExportResultFile

    use FEMAnalysis
    use ModProbe
    use ModPostProcessors
    use ModGid
    use ModHyperView


    contains

    !==========================================================================================
    ! Subroutine Description:
    !==========================================================================================
    subroutine  ReadPostProcessingInputFile(FileName,ProbeList,PostProcessor)


        !************************************************************************************
        ! DECLARATIONS OF VARIABLES
        !************************************************************************************
        ! Modules and implicit declarations
        ! -----------------------------------------------------------------------------------
        use Parser
        implicit none


        ! Input variables
        ! -----------------------------------------------------------------------------------
        character (len=*) :: FileName
        type (ClassProbeWrapper), pointer, dimension(:) :: ProbeList
        class(ClassPostProcessor), pointer :: PostProcessor

        ! Internal variables
        ! -----------------------------------------------------------------------------------
        type (ClassParser) :: File
        character(len=255) :: OptionName, OptionValue, String
        character(len=255) :: ProbeLocation, ProbeFileName, ProbeVariableName, ProbeComponentsString
        logical :: ProbeAllComponents

        character(len=255), allocatable, dimension(:) :: PostProcessorResults
        character(len=255)                            :: PostProcessorFileName=''

        integer :: NumberOfProbes, ProbeNode, ProbeElement, ProbeGaussPoint, i

        !************************************************************************************

        call File%Setup (FileName,FileNumber=30)

        write(*,*) 'Reading Post Processing File: ',trim(FileName)


        ! Leitura do Pos Processador
        !------------------------------------------------------------------------------------
        call File%GetNextString(String)

        if ( .not. File%CompareStrings(String,'POST PROCESSOR') ) then
            call File%RaiseError('Expecting Word POST PROCESSOR in '//trim(FileName))
        end if


        call File%GetNextOption(OptionName,OptionValue)

        if (File%CompareStrings(OptionName,'Post Processor')) then

            ! GiD 7
            !========================================================================================
            if ( File%CompareStrings(OptionValue,'GiD 7') ) then

                call File%GetNextOption(OptionName,OptionValue)
                if ( .not. File%CompareStrings(OptionName,'Results') ) then
                    call File%RaiseError('Expecting Word Results in '//trim(FileName))
                end if

                call Split( OptionValue , PostProcessorResults , ",")

                call File%GetNextOption(OptionName,OptionValue)
                if ( .not. File%CompareStrings(OptionName,'File Name') ) then
                    call File%RaiseError('Expecting Word File Name in '//trim(FileName))
                end if
                PostProcessorFileName = OptionValue

                ! Constuindo o Pos Processador GiD 7
                !------------------------------------------------------------------------------------
                call Constructor_GiD( PostProcessor, PostProcessorResults, PostProcessorFileName )

                call File%GetNextString(String)

                if ( .not. File%CompareStrings(String,'END POST PROCESSOR') ) then
                    call File%RaiseError('Expecting Word END POST PROCESSOR in '//trim(FileName))
                end if

            ! HyperView 12
            !========================================================================================
            elseif ( File%CompareStrings(OptionValue,'HyperView 12') ) then

                call File%GetNextOption(OptionName,OptionValue)
                if ( .not. File%CompareStrings(OptionName,'Results') ) then
                    call File%RaiseError('Expecting Word Results in '//trim(FileName))
                end if

                call Split( OptionValue , PostProcessorResults , ",")

                call File%GetNextOption(OptionName,OptionValue)
                if ( .not. File%CompareStrings(OptionName,'File Name') ) then
                    call File%RaiseError('Expecting Word File Name in '//trim(FileName))
                end if
                PostProcessorFileName = OptionValue

                ! Constuindo o Pos Processador HiperView 12
                !------------------------------------------------------------------------------------
                call Constructor_HyperView( PostProcessor, PostProcessorResults, PostProcessorFileName )

                call File%GetNextString(String)

                if ( .not. File%CompareStrings(String,'END POST PROCESSOR') ) then
                    call File%RaiseError('Expecting Word END POST PROCESSOR in '//trim(FileName))
                end if

            ! Nenhum Pós Processodor
            !========================================================================================
            elseif ( File%CompareStrings(OptionValue,'None') ) then

                call File%GetNextString(String)
                do while (.not. File%CompareStrings(String,'END POST PROCESSOR'))
                    call File%GetNextString(String)
                enddo

            endif
            !========================================================================================

        else
            call File%RaiseError('Expecting Post Processor Name in '//trim(FileName))

        end if




        !Começo da leitura do arquivo de Probes
        call File%GetNextOption(OptionName,OptionValue)


        if (File%CompareStrings(OptionName,'Number of Probes')) then

            NumberOfProbes = OptionValue
        else
            call File%RaiseError('Expecting Number of Probes in '//trim(FileName))
        end if

        allocate (ProbeList(NumberOfProbes))


        do i = 1,NumberOfProbes

            call File%GetNextString(String)

            if (.not. File%CompareStrings(String,'Probe')) then
                call File%RaiseError('Expecting Word PROBE in '//trim(FileName))
            end if

            ProbeLocation = ''
            ProbeFileName = ''
            ProbeVariableName = ''
            ProbeComponentsString = ''
            NumberOfProbes = 0
            ProbeNode = 0
            ProbeElement = 0
            ProbeGaussPoint = 0

            PROBE_BLOCK_LOOP: do while (.true.)

                call File%GetNextString(String)

                if (File%CompareStrings(String,'End Probe')) then
                    exit PROBE_BLOCK_LOOP
                end if
                OptionValue = ''
                call File%GetCurrentOption(OptionName,OptionValue)

                if (File%CompareStrings(OptionName,'Location')) then
                    ProbeLocation = OptionValue

                elseif (File%CompareStrings(OptionName,'File Name')) then
                    ProbeFileName = OptionValue

                elseif (File%CompareStrings(OptionName,'Variable Name')) then
                    ProbeVariableName = OptionValue

                elseif (File%CompareStrings(OptionName,'Node')) then
                    ProbeNode = OptionValue

                elseif (File%CompareStrings(OptionName,'Components')) then
                    ProbeComponentsString = OptionValue

                elseif (File%CompareStrings(OptionName,'Element')) then
                    ProbeElement = OptionValue

                elseif (File%CompareStrings(OptionName,'Gauss Point')) then
                    ProbeGaussPoint = OptionValue

                else
                    call File%RaiseError('Expression not identified in '//trim(FileName))
                endif

            enddo PROBE_BLOCK_LOOP



        ! Construtor dos Probes
        ! Probes de Nós
        if (  File%CompareStrings(ProbeLocation, 'Node' ) ) then

            call NodeProbeConstructor(ProbeList(i)%Pr, ProbeFileName, ProbeVariableName, ProbeNode, ProbeComponentsString)

        ! Probes de Pontos de Gauss
        elseif (  File%CompareStrings(ProbeLocation, 'Gauss Point' ) ) then

            call GaussPointProbeConstructor(ProbeList(i)%Pr, ProbeVariableName, ProbeElement, ProbeFileName, ProbeGaussPoint, ProbeComponentsString)

        ! Probes de Micro Estrutura
        elseif (  File%CompareStrings(ProbeLocation, 'Micro Structure' ) ) then

            call MicroStructureProbeConstructor(ProbeList(i)%Pr, ProbeVariableName, ProbeFileName, ProbeComponentsString)

        endif

    end do

        !************************************************************************************

    end subroutine
    !==========================================================================================



    !==========================================================================================
    ! Subroutine Description:
    !==========================================================================================
    subroutine  PostProcessingResults(ProbeList,PostProcessor,FEA)

        !************************************************************************************
        ! DECLARATIONS OF VARIABLES
        !************************************************************************************
        ! Modules and implicit declarations
        ! -----------------------------------------------------------------------------------
        use Parser
        use Interfaces
        use ModStatus
        use ModIO
        implicit none


        ! Input variables
        ! -----------------------------------------------------------------------------------
        type (ClassProbeWrapper), pointer, dimension(:) :: ProbeList
        class (ClassPostProcessor), pointer             :: PostProcessor
        class (ClassFEMAnalysis)                        :: FEA

        ! Internal variables
        ! -----------------------------------------------------------------------------------
        type(ClassParser)                         :: ResultFile
        type(ClassStatus)                         :: Status
        integer :: TotalNDOF, LoadCase, Step, CutBack, SubStep, el, gp, i, FileNumber
        real(8) :: Time
        real(8) , allocatable, target, dimension(:) :: U
        character(len=255) :: OptionName, OptionValue, String, FileName
        integer :: Flag_EndStep, NumberOfIterations,IterationFile


        !************************************************************************************

        write(*,*) 'Post Processing Results...'

        ! Analisar se existem os arquivos dos probes pedidos. Caso existam, são deletados
        do i = 1, size(ProbeList)
            call ProbeList(i)%Pr%InitializeFile
        enddo

        if (associated(PostProcessor)) then
            call PostProcessor%InitializePostProcessorFile(FEA)
        endif

        IterationFile = FreeFile()
        open(IterationFile,File='NumberOfIterationsToConverge.dat',status='unknown')
        write(IterationFile,*)'  Time                    Number Of Iterations To Converge'

        FileName='FEMAnalysis.result'
        FileNumber = 222
        call ResultFile%Setup(FileName,FileNumber)

        call ResultFile%GetNextOption(OptionName,OptionValue)

        TotalNDOF = OptionValue

        allocate( U(TotalNDOF) )


        call FEA%AdditionalMaterialModelRoutine()

        ! Restart Constitutive Model
        ! -----------------------------------------------------------------------------------
        do el = 1,size(FEA%ElementList)
            do gp = 1,size(FEA%ElementList(el)%El%GaussPoints)
                call FEA%ElementList(el)%El%GaussPoints(gp)%ConstitutiveModelDestructor()
                call FEA%ElementList(el)%El%GaussPoints(gp)%ConstitutiveModelConstructor(FEA%AnalysisSettings)
            enddo
        enddo

        ! Restart Mesh Coordinates
        ! -----------------------------------------------------------------------------------
        do i = 1,size(FEA%GlobalNodesList)
            FEA%GlobalNodesList(i)%Coord = FEA%GlobalNodesList(i)%CoordX
        enddo


        LOOP_TIME :do while (.true.)


            call ResultFile%GetNextOption(OptionName,OptionValue)

            if (EOF(ResultFile)) exit LOOP_TIME

            Time = OptionValue

            call ResultFile%GetNextOption(OptionName,OptionValue)

            LoadCase = OptionValue

            call ResultFile%GetNextOption(OptionName,OptionValue)

            Step = OptionValue

            call ResultFile%GetNextOption(OptionName,OptionValue)

            CutBack = OptionValue

            call ResultFile%GetNextOption(OptionName,OptionValue)

            Substep = OptionValue

            call ResultFile%GetNextOption(OptionName,OptionValue)

            Flag_EndStep = OptionValue

            call ResultFile%GetNextOption(OptionName,OptionValue)

            NumberOfIterations = OptionValue

            do i = 1, TotalNDOF
                call ResultFile%GetNextString(String)
                U(i) = String
            enddo

            FEA%LoadCase = LoadCase
            FEA%Time = Time
            FEA%U => U
            ! Update stress and internal variables
            call SolveConstitutiveModel( FEA%ElementList , FEA%AnalysisSettings, Time, U, Status)

            ! Escrevendo os pontos pedidos. Excluindo soluções dos Cut Backs.
            if (Flag_EndStep .eq. 1) then
                do i = 1, size(ProbeList)
                    call ProbeList(i)%Pr%WriteProbeResult(FEA)
                enddo
                if (associated(PostProcessor)) then
                    call PostProcessor%WritePostProcessorResult(FEA)
                endif
                write(IterationFile,*)Time,NumberOfIterations
            endif

            ! SAVING THE CONVERGED STATE
            ! ----------------------------------------------------------------------------------
            do el=1,size(FEA%ElementList)
                do gp=1,size(FEA%ElementList(el)%el%GaussPoints)
                    call FEA%ElementList(el)%el%GaussPoints(gp)%SwitchConvergedState()
                enddo
            enddo

            ! Update Coordinates
            if (FEA%AnalysisSettings%NLAnalysis == .true.) then
                call UpdateMeshCoordinates(FEA%GlobalNodesList,FEA%AnalysisSettings,U)
            endif



        enddo LOOP_TIME

        call ResultFile%CloseFile

        close(IterationFile)

    end subroutine






end module
