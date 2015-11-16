module ModExportResultFile

    use FEMAnalysis
    use ModProbe


    contains

    !==========================================================================================
    ! Subroutine Description:
    !==========================================================================================
    subroutine  ReadProbesInputFile(FileName,ProbeList)


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

        ! Internal variables
        ! -----------------------------------------------------------------------------------
        type (ClassParser) :: ProbeFile
        character(len=255) :: OptionName, OptionValue, String
        character(len=255) :: ProbeLocation, ProbeFileName, ProbeVariableName, ProbeComponentsString
        character(len=255) :: ProbeType
        logical :: ProbeAllComponents

        integer :: NumberOfProbes, ProbeNode, ProbeElement, ProbeGaussPoint, i

        !************************************************************************************

        call ProbeFile%Setup (FileName,FileNumber=30)

        write(*,*) 'Reading Probe File: ',trim(FileName)

        !Começo da leitura do arquivo dat
        call ProbeFile%GetNextOption(OptionName,OptionValue)


        if (ProbeFile%CompareStrings(OptionName,'Number of Probes')) then
            call ProbeFile%ConvertToInteger(OptionValue,NumberOfProbes)
        else
            call ProbeFile%RaiseError('Expecting Number of Probes in '//trim(FileName))
        end if

        allocate (ProbeList(NumberOfProbes))


        do i = 1,NumberOfProbes

            call ProbeFile%GetNextString(String)

            if (.not. ProbeFile%CompareStrings(String,'Probe')) then
                call ProbeFile%RaiseError('Expecting Word PROBE in '//trim(FileName))
            end if

            ProbeType = ''
            ProbeLocation = ''
            ProbeFileName = ''
            ProbeVariableName = ''
            ProbeComponentsString = ''
            NumberOfProbes = 0
            ProbeNode = 0
            ProbeElement = 0
            ProbeGaussPoint = 0

            PROBE_BLOCK_LOOP: do while (.true.)

                call ProbeFile%GetNextString(String)

                if (ProbeFile%CompareStrings(String,'End Probe')) then
                    exit PROBE_BLOCK_LOOP
                end if
                OptionValue = ''
                call ProbeFile%GetCurrentOption(OptionName,OptionValue)

                if (ProbeFile%CompareStrings(OptionName,'Type')) then
                    ProbeType = OptionValue
                elseif (ProbeFile%CompareStrings(OptionName,'Location')) then
                    ProbeLocation = OptionValue
                elseif (ProbeFile%CompareStrings(OptionName,'File Name')) then
                    ProbeFileName = OptionValue
                elseif (ProbeFile%CompareStrings(OptionName,'Variable Name')) then
                    ProbeVariableName = OptionValue
                elseif (ProbeFile%CompareStrings(OptionName,'Node')) then
                    call ProbeFile%ConvertToInteger(OptionValue,ProbeNode)
                elseif (ProbeFile%CompareStrings(OptionName,'Components')) then
                    ProbeComponentsString = OptionValue
                elseif (ProbeFile%CompareStrings(OptionName,'Element')) then
                    call ProbeFile%ConvertToInteger(OptionValue,ProbeElement)
                elseif (ProbeFile%CompareStrings(OptionName,'Gauss Point')) then
                    call ProbeFile%ConvertToInteger(OptionValue,ProbeGaussPoint)
                else
                    call ProbeFile%RaiseError('Expression not identified in '//trim(FileName))
                endif

            enddo PROBE_BLOCK_LOOP



        ! Construtor dos Probes
        ! Probes de Nós
        if (  ProbeFile%CompareStrings(ProbeLocation, 'Node' ) ) then

            call NodeProbeConstructor(ProbeList(i)%Pr, ProbeFileName, ProbeVariableName, ProbeNode, ProbeComponentsString)

        ! Probes de Pontos de Gauss
        elseif (  ProbeFile%CompareStrings(ProbeLocation, 'Gauss Point' ) ) then

            call GaussPointProbeConstructor(ProbeList(i)%Pr, ProbeType, ProbeVariableName, ProbeElement, ProbeFileName, ProbeGaussPoint, ProbeComponentsString)

        endif

    end do

        !************************************************************************************

    end subroutine
    !==========================================================================================



    !==========================================================================================
    ! Subroutine Description:
    !==========================================================================================
    subroutine  PostProcessingResults(ProbeList,FEA)

        !************************************************************************************
        ! DECLARATIONS OF VARIABLES
        !************************************************************************************
        ! Modules and implicit declarations
        ! -----------------------------------------------------------------------------------
        use Parser
        use Interfaces
        implicit none


        ! Input variables
        ! -----------------------------------------------------------------------------------
        type (ClassProbeWrapper), pointer, dimension(:) :: ProbeList
        type (ClassFEMAnalysis)                         :: FEA

        ! Internal variables
        ! -----------------------------------------------------------------------------------
        type(ClassParser) :: ResultFile
        integer :: TotalNDOF, LoadCase, Step, CutBack, SubStep, el, gp, i, FileNumber
        real(8) :: Time
        real(8) , allocatable, target, dimension(:) :: U
        character(len=255) :: OptionName, OptionValue, String, FileName


        !************************************************************************************

        write(*,*) 'Post Processing Results...'

        ! Analisar se existem os arquivos dos probes pedidos. Caso exitam, são deletados
        do i = 1, size(ProbeList)
            call ProbeList(i)%Pr%InitializeFile
        enddo



        FileName='FEMAnalysis.result'
        FileNumber = 222
        call ResultFile%Setup(FileName,FileNumber)

        call ResultFile%GetNextOption(OptionName,OptionValue)
        call ResultFile%ConvertToInteger(OptionValue,TotalNDOF)

        allocate( U(TotalNDOF) )


        ! Restart Constitutive Model
        ! -----------------------------------------------------------------------------------
        do el = 1,size(FEA%ElementList)
            do gp = 1,size(FEA%ElementList(el)%El%GaussPoints)
                call FEA%ElementList(el)%El%GaussPoints(gp)%ConstitutiveModelConstructor(FEA%AnalysisSettings)
            enddo
        enddo

        ! Restart Mesh Coordinates
        ! -----------------------------------------------------------------------------------
        do i = 1,size(FEA%GlobalNodesList)
            FEA%GlobalNodesList(i)%Coord = FEA%GlobalNodesList(i)%CoordX
        enddo


        LOOP_TIME :do while (.not.EOF(ResultFile))


            call ResultFile%GetNextOption(OptionName,OptionValue)
!            if (OptionValue == '1.00000000000000') then
!                write(*,*)''
!            endif
            ! Evita a o processamento de uma linha em branco (última linha do arquivo)
            if (ResultFile%isEmptyString(OptionValue)) then
                exit LOOP_TIME
            endif
            call ResultFile%ConvertToDouble(OptionValue,Time)

            call ResultFile%GetNextOption(OptionName,OptionValue)
            call ResultFile%ConvertToInteger(OptionValue,LoadCase)

            call ResultFile%GetNextOption(OptionName,OptionValue)
            call ResultFile%ConvertToInteger(OptionValue,Step)

            call ResultFile%GetNextOption(OptionName,OptionValue)
            call ResultFile%ConvertToInteger(OptionValue,CutBack)

            call ResultFile%GetNextOption(OptionName,OptionValue)
            call ResultFile%ConvertToInteger(OptionValue,SubStep)

            do i = 1, TotalNDOF
                call ResultFile%GetNextString(String)
                call ResultFile%ConvertToDouble(String,U(i))
            enddo

            FEA%Time = Time
            FEA%U => U
            ! Update stress and internal variables
            call SolveConstitutiveModel( FEA%ElementList , FEA%AnalysisSettings, Time, U)

            ! Escrevendo os pontos pedidos. Excluindo soluções dos Cut Backs.
            if (CutBack .eq. 0) then
                do i = 1, size(ProbeList)
                    call ProbeList(i)%Pr%WriteProbeResult(FEA)
                enddo
            endif

            ! SAVING THE CONVERGED STATE
            ! ----------------------------------------------------------------------------------
            do el=1,size(FEA%ElementList)
                do gp=1,size(FEA%ElementList(el)%el%GaussPoints)
                    call FEA%ElementList(el)%el%GaussPoints(gp)%SaveConvergedState()
                enddo
            enddo

            ! Update Coordinates
            if (FEA%AnalysisSettings%NLAnalysis == .true.) then
                call UpdateMeshCoordinates(FEA%GlobalNodesList,FEA%AnalysisSettings,U)
            endif



        enddo LOOP_TIME

        call ResultFile%CloseFile



    end subroutine






end module
