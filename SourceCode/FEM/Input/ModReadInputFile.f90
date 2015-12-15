module modReadInputFile

    use Analysis
    use Nodes
    use ElementLibrary
    use BoundaryConditions

    use ConstitutiveModelLibrary
    use NonLinearSolverLibrary
    use LinearSolverLibrary
    use Parser
    use ConstitutiveModel

    type ClassPreprocessors
        integer :: Gid7 = 1
        integer :: Gid12 = 2
        integer :: HyperMesh = 3
    end type

    type (ClassPreprocessors) , parameter :: PreProcessors = ClassPreprocessors()

    integer,parameter :: iAnalysisSettings=1, iLinearSolver=2, iNonLinearSolver=3, iMaterial=4, iMeshAndBC=5, nblocks=5
    logical,dimension(nblocks)::BlockFound=.false.
    character(len=100),dimension(nblocks)::BlockName

contains

    !=======================================================================================================================
    subroutine ReadInputFile( FileName, AnalysisSettings , GlobalNodesList , ElementList , BC , NLSolver )
        implicit none

        type (ClassAnalysis)                                     :: AnalysisSettings
        type (ClassNodes) , pointer , dimension(:)               :: GlobalNodesList
        type (ClassElementsWrapper) , pointer , dimension(:)     :: ElementList
        type (ClassBoundaryConditions)                           :: BC
        class(ClassNonlinearSolver) , pointer                    :: NLSolver
        character(len=*) :: FileName


        integer :: ModelID , i
        character(len=255) :: string , endstring, DataFileName
        Type(ClassParser) :: DataFile
        type(ClassConstitutiveModelWrapper) , pointer , dimension(:) :: MaterialList
        class(ClassLinearSolver) , pointer :: LinearSolver


        BlockName(1)="Analysis Settings"
        BlockName(2)="Linear Solver"
        BlockName(3)="NonLinear Solver"
        BlockName(4)="Material"
        BlockName(5)="Mesh and Boundary Conditions"



        BlockFound=.false.

        DataFileName = FileName

        write(*,*) 'Opening Input File:',trim(DataFileName)

        ! TODO (Jan#1#11/07/15): Melhorar informações para o usuário.  ...
    !Escrever na tela quais arquivos estão sendo abertos,
    !isso ajuda a evitar erros
        call DataFile%Setup(FileName=trim(DataFileName),FileNumber=12)
        write(*,*) 'Reading Input File:',trim(DataFileName)

        !Começo da leitura do arquivo dat
        call DataFile%GetNextString(string)

            do while (.not.EOF(DataFile))

                if (DataFile%Error) then
                    call DataFile%ShowError
                    stop
                endif


    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                select case (GetBlockID(DataFile,string))
    !---------------------------------------------------------------------------------------------------------------------------------------------------------


    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                case (iAnalysisSettings)
                    call ReadAnalysisSettings(DataFile,AnalysisSettings)

    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                case (iLinearSolver)
                    call ReadLinearSolver(DataFile,LinearSolver)

    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                case (iNonLinearSolver)
                    if (.not.BlockFound(iLinearSolver)) call DataFile%RaiseError("Linear Solver must be specified before the NonLinear solver")
                    call ReadNonLinearSolver(DataFile,LinearSolver,NLSolver)

    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                case (iMaterial)
                    if (.not.BlockFound(iAnalysisSettings)) call Datafile%RaiseError("Analysis Settings must be specified before the material.")
                    call ReadMaterialModel(AnalysisSettings,MaterialList,DataFile)

    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                case (iMeshAndBC)
                    if (.not.all(BlockFound([iAnalysisSettings,iMaterial]))) call DataFile%RaiseError("Analysis Settings, Multiscale Settings and Material must be specified before mesh.")
                    call ReadMeshAndBC(DataFile,GlobalNodesList,ElementList,AnalysisSettings,MaterialList,BC)

    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                case default
                    call DataFile%RaiseError("Erro no select.")
                end select
    !---------------------------------------------------------------------------------------------------------------------------------------------------------

    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                call DataFile%GetNextString(string)
    !---------------------------------------------------------------------------------------------------------------------------------------------------------

            end do
            !termina leitura de dados



            !check if all entries were found
            if (.not.all(BlockFound)) then
                write(*,*) '######### ERROR #########'
                do i=1,nblocks
                    if (.not.BlockFound(i)) write(*,*) 'Block ['//trim(BlockName(i))//'] was not detected!'
                enddo
                stop
            endif

            call DataFile%CloseFile

            write(*,*) 'Input File Closed'
            write(*,*) ''

        end subroutine
    !=======================================================================================================================

    !=======================================================================================================================
    function GetBlockID(DataFile,string) result(BlockID)
        implicit none
        type(ClassParser)::DataFile
        character(len=*)::string
        integer::BlockID

        integer::i

        do i=1,nblocks
            if (DataFile%CompareStrings(string,BlockName(i) )) then
                BlockID=i
                return
            endif
        enddo

        call DataFile%RaiseError("Block was not identified.")
    end function
    !=======================================================================================================================

    !=======================================================================================================================
    subroutine ReadAnalysisSettings(DataFile,AnalysisSettings)

        implicit none

        type(ClassParser)::DataFile
        type (ClassAnalysis) :: AnalysisSettings
        character(len=255)::string

        character(len=100),dimension(6)::ListOfOptions,ListOfValues
        logical,dimension(6)::FoundOption
        integer :: i


        ListOfOptions=["Problem Type","Analysis Type","Nonlinear Analysis","Hypothesis of Analysis","Element Technology","Maximum Cut Backs"]


        call DataFile%FillListOfOptions(ListOfOptions,ListOfValues,FoundOption)

        call DataFile%CheckError

        do i=1,size(FoundOption)
            if (.not.FoundOption(i)) then
                write(*,*) "Analysis Settings :: Option not found ["//trim(ListOfOptions(i))//"]"
                stop
            endif
        enddo


        ! Option Problem Type
        if (DataFile%CompareStrings(ListOfValues(1),"Mechanical")) then
            AnalysisSettings%ProblemType=ProblemTypes%Mechanical
        elseif (DataFile%CompareStrings(ListOfValues(1),"Thermal")) then
            AnalysisSettings%ProblemType=ProblemTypes%Thermal
        else
            call Error( "Problem Type not identified" )
        endif

        ! Option Analysis Type
        if (DataFile%CompareStrings(ListOfValues(2),"Quasi Static")) then
            AnalysisSettings%AnalysisType = AnalysisTypes%Quasi_Static
        elseif (DataFile%CompareStrings(ListOfValues(2),"Transient")) then
            AnalysisSettings%AnalysisType = AnalysisTypes%Transient
        else
            call Error( "Analysis Type not identified" )
        endif

        ! Option Nonlinear Analysis
        if (DataFile%CompareStrings(ListOfValues(3),"True")) then
            AnalysisSettings%NLAnalysis=.true.
        elseif (DataFile%CompareStrings(ListOfValues(3),"False")) then
            AnalysisSettings%NLAnalysis=.false.
        else
            call Error( "Nonlinear Analysis not identified" )
        endif

        ! Option Hypothesis of Analysis
        if     (DataFile%CompareStrings(ListOfValues(4),"Plane Strain")) then
            call AnalysisSettings%ClassAnalysisConstructor(HypothesisOfAnalysis%PlaneStrain)

        elseif (DataFile%CompareStrings(ListOfValues(4),"Plane Stress")) then
            call AnalysisSettings%ClassAnalysisConstructor(HypothesisOfAnalysis%PlaneStress)

        elseif (DataFile%CompareStrings(ListOfValues(4),"Axisymmetric")) then
            call AnalysisSettings%ClassAnalysisConstructor(HypothesisOfAnalysis%Axisymmetric)

        elseif (DataFile%CompareStrings(ListOfValues(4),"3D")) then
            call AnalysisSettings%ClassAnalysisConstructor(HypothesisOfAnalysis%ThreeDimensional)

        else
            call Error( "Analysis Type not identified" )
        endif

        ! Option Element Technology
        if (DataFile%CompareStrings(ListOfValues(5),"Full Integration")) then
            AnalysisSettings%ElementTech=ElementTechnologies%Full_Integration
        elseif (DataFile%CompareStrings(ListOfValues(5),"Mean Dilatation")) then
            AnalysisSettings%ElementTech=ElementTechnologies%Mean_Dilatation
        else
            call Error( "Element Technology not identified" )
        endif


        AnalysisSettings%MaxCutBack = ListOfValues(6)


        BlockFound(iAnalysisSettings)=.true.
        call DataFile%GetNextString(string)
        if (.not.DataFile%CompareStrings(string,'end'//trim(BlockName(iAnalysisSettings)))) then
            call DataFile%RaiseError("End of block was expected. BlockName="//trim(BlockName(iAnalysisSettings)))
        endif



    end subroutine
    !=======================================================================================================================

    !=======================================================================================================================
    subroutine ReadLinearSolver(DataFile,LinearSolver)
        implicit none
        type(ClassParser)::DataFile
        class(ClassLinearSolver),pointer :: LinearSolver


        integer::LinearSolverID
        character(len=100)::Stype,string

        call DataFile%GetNextString(Stype)
        call datafile%CheckError
        call LinearSolverIdentifier( SType , LinearSolverID )
        call AllocateNewLinearSolver( LinearSolver , LinearSOlverID )
        call LinearSolver%ReadSolverParameters(DataFile)

        BlockFound(iLinearSolver)=.true.
        call DataFile%GetNextString(string)
        if (.not.DataFile%CompareStrings(string,'end'//trim(BlockName(iLinearSolver)))) then
            call DataFile%RaiseError("End of block was expected. BlockName="//trim(BlockName(iLinearSolver)))
        endif

    end subroutine
    !=======================================================================================================================

    !=======================================================================================================================
    subroutine ReadNonLinearSolver(DataFile,LinearSolver,NLSolver)
        implicit none

        type(ClassParser)::DataFile
        class(ClassLinearSolver) , pointer :: LinearSolver
        class(ClassNonlinearSolver) , pointer  :: NLSolver

        character(len=100)::SType,string
        integer::SolverID

        call DataFile%GetNextString(Stype)
        call DataFile%CheckError

        call SolverIdentifier( SType , SolverID )
        call AllocateNewNonLinearSolver( NLSolver , SolverID )
        call NLSolver%ReadSolverParameters(DataFile)
        NLSolver % LinearSolver => LinearSolver


        BlockFound(iNonLinearSolver)=.true.
        call DataFile%GetNextString(string)
        if (.not.DataFile%CompareStrings(string,'end'//trim(BlockName(iNonLinearSolver)))) then
            call DataFile%RaiseError("End of block was expected. BlockName="//trim(BlockName(iNonLinearSolver)))
        endif

    end subroutine
    !=======================================================================================================================

    !=======================================================================================================================
    subroutine ReadMaterialModel(AnalysisSettings,MaterialList,DataFile)
        implicit none

        type (ClassAnalysis) :: AnalysisSettings
        type(ClassConstitutiveModelWrapper) , pointer , dimension(:) :: MaterialList
        type(ClassParser)::DataFile

        integer :: NumberOfMaterials , i , MaterialID, ModelEnumerator
        character(len=100):: ModelName, string , OptionName , OptionValue


        call DataFile%GetNextOption(OptionName , OptionValue)
        call DataFile%CheckError

        if (.not.DataFile%CompareStrings(OptionName,"Number of Materials")) then
            call DataFile%RaiseError("Expected Number of Materials")
        endif


        NumberOfMaterials = OptionValue
        call DataFile%CheckError

        allocate( MaterialList(NumberOfMaterials) )

        do i=1,NumberOfMaterials

            call DataFile%GetNextOption(OptionName , OptionValue)
            call DataFile%CheckError

            if (.not.DataFile%CompareStrings(OptionName,"Material ID")) then
                call DataFile%RaiseError("Expected Material ID")
            endif


            MaterialID = OptionValue
            call DataFile%CheckError

            MaterialList(i)%MaterialID = MaterialID

            call DataFile%GetNextString(ModelName)
            call DataFile%CheckError

            call ConstitutiveModelIdentifier( ModelName, AnalysisSettings, ModelEnumerator )

            MaterialList(i)%ModelEnumerator= ModelEnumerator

            call AllocateConstitutiveModel( ModelEnumerator , AnalysisSettings , 1 , MaterialList(i)%Mat )

            call MaterialList(i)%Mat(1)% ReadMaterialParameters(DataFile)

        enddo

        BlockFound(iMaterial)=.true.
        call DataFile%GetNextString(string)
        if (.not.DataFile%CompareStrings(string,'end'//trim(BlockName(iMaterial)))) then
            call DataFile%RaiseError("End of block was expected. BlockName="//trim(BlockName(iMaterial)))
        endif

    end subroutine
    !=======================================================================================================================

    !=======================================================================================================================
    subroutine ReadMeshAndBC(DataFile,GlobalNodesList,ElementList,AnalysisSettings,MaterialList,BC)

        implicit none

        type (ClassParser)                                            :: DataFile
        type (ClassAnalysis)                                          :: AnalysisSettings
        type (ClassNodes) , pointer , dimension(:)                    :: GlobalNodesList
        type (ClassElementsWrapper) , pointer , dimension(:)          :: ElementList
        type (ClassBoundaryConditions)                                :: BC
        type (ClassConstitutiveModelWrapper) , pointer , dimension(:) :: MaterialList

        character(len=100)                                            :: OptionName, OptionValue,string
        type (ClassParser)                                            :: DataMeshBC

        character(len=100),dimension(3) :: ListOfOptions,ListOfValues
        character(len=100)              :: TimeFileName
        logical,dimension(3)            :: FoundOption
        integer                         :: i,j,PreProcessorID


        ListOfOptions=["Mesh File","Time Discretization File","Preprocessor"]

        call DataFile%FillListOfOptions(ListOfOptions,ListOfValues,FoundOption)

        call DataFile%CheckError

        IF (.NOT.FoundOption(1)) then
            write(*,*) "MESH AND BOUNDARY CONDITIONS :: Mesh File was not found"
            stop
        ELSEIF (.NOT.FoundOption(2)) then
            write(*,*) "MESH AND BOUNDARY CONDITIONS :: Time Discretization File was not found"
            stop
        ELSEIF (.NOT.FoundOption(3)) then
            call DataFile%RaiseError("MESH AND BOUNDARY CONDITIONS :: Preprocessor was not found")
        ENDIF

        IF (DataFile%CompareStrings(ListOfValues(3),"Gid7")) then
            PreProcessorID = PreProcessors%Gid7
        ELSEIF (DataFile%CompareStrings(ListOfValues(3),"Gid12")) then
            PreProcessorID = PreProcessors%Gid12
        ELSEIF (DataFile%CompareStrings(ListOfValues(3),"HyperMesh")) then
            PreProcessorID = PreProcessors%HyperMesh
        ELSE
            call datafile%RaiseError("Preprocessor not identified")
        ENDIF


        call DataMeshBC%Setup(FileName=ListOfValues(1),FileNumber=43)



        !Leitura da malha e CC do Ansys gerada pelo HyperMesh
        !--------------------------------------------------------------------------
        !call ReadMeshHyperMesh(DataMeshBC,GlobalNodesList,ElementList,AnalysisSettings,MaterialList, PreProcessorID)




        !--------------------------------------------------------------------------




        !Leitura da malha e CC gerada pelo GiD
        !--------------------------------------------------------------------------
        call ReadMesh(DataMeshBC,GlobalNodesList,ElementList,AnalysisSettings,MaterialList, PreProcessorID)

        TimeFileName = ListOfValues(2)
        call ReadBoundaryConditions(TimeFileName,DataMeshBC,BC,AnalysisSettings)

        call DataMeshBC%CloseFile

       call DataFile%GetNextString(string)

        IF (.not.DataFile%CompareStrings(string,'end'//trim(BlockName(iMeshAndBC)))) then
            call DataFile%RaiseError("End of block was expected. BlockName="//trim(BlockName(iMeshAndBC)))
        endif

        BlockFound(iMeshAndBC)=.true.
        !--------------------------------------------------------------------------


    end subroutine
    !=======================================================================================================================

    !=======================================================================================================================
    subroutine ReadMeshHyperMesh(DataFile, GlobalNodesList, ElementList, AnalysisSettings, MaterialList, PreProcessorID)

        use Interfaces
        implicit none

        type (ClassParser)                                    :: DataFile
        type (ClassAnalysis)                                  :: AnalysisSettings
        type (ClassNodes) , pointer , dimension(:)            :: GlobalNodesList
        type (ClassElementsWrapper) , pointer , dimension(:)  :: ElementList
        type (ClassConstitutiveModelWrapper) , pointer , dimension(:) :: MaterialList
        Integer                                               :: PreProcessorID


        character(len=255)::string , OptionName,OptionValue , line
        logical :: FoundMaterial , IsQuadratic
        real(8) , allocatable , dimension(:) :: Coords
        integer , allocatable ,dimension(:) :: MatID
        integer::i,e,id,j
        integer::nnodes,nelem,ndime,ElemType, ElemConec(MaxElementNodes),GeoType,ENnodes , NumberOfMaterialIDs
        class(ClassConstitutiveModelWrapper)  , pointer :: Material

        call DataFile%GetNextString(string)

        do i=1,3
            call DataFile%GetNextOption(OptionName,OptionValue)
            if (DataFile%Error) then
                call DataFile%ShowError
                write(*,*) "Expected Nnodes,Nelem or Ndime"
                stop
            end if
            if (DataFile%CompareStrings(OptionName,"nnodes")) then

                nnodes = OptionValue
                call DataFile%CheckError
                if (nnodes<=0) call DataFile%RaiseError("Nnodes must be positive")
            elseif (DataFile%CompareStrings(OptionName,"ndime")) then

                ndime = OptionValue
                call DataFile%CheckError
                if (ndime<=0) call DataFile%RaiseError("Ndime must be positive")
            elseif (DataFile%CompareStrings(OptionName,"nelem")) then

                nelem = OptionValue
                call DataFile%CheckError
                if (nelem<=0) call DataFile%RaiseError("Nelem must be positive")
            else
                call DataFile%RaiseError("Expected Nnodes,Nelem or Ndime")
            endif
        enddo

        allocate( GlobalNodesList( Nnodes ) , ElementList(Nelem) , Coords(Ndime) )

        call DataFile%GetNextString(string)
        call DataFile%CheckError
        if (.not.DataFile%CompareStrings(string,"Coordinates")) call DataFile%RaiseError("Coordinates were not found")


        do i=1,nnodes
            call DataFile%GetNextString()
            call DataFile%CheckError
            call DataFile%GetOriginalLine(line)
            Read(line,*) id, ( Coords(j) , j=1,Ndime )
            call GlobalNodesList(i)%NodeConstructor( Coords , i , AnalysisSettings % NDOFnode )
        enddo

        call DataFile%GetNextString(string)
        IF (.not.DataFile%CompareStrings(string,'end coordinates')) call DataFile%RaiseError("Expected: End Coordinates")


        call DataFile%GetNextString(string)
        call DataFile%CheckError

        if (.not.DataFile%CompareStrings(string,"MATERIAL ID")) call DataFile%RaiseError("MATERIAL ID was not found")

        call DataFile%GetNextString(string) ; call DataFile%CheckError

        NumberOfMaterialIDs = string
        call DataFile%CheckError

        if (NumberOfMaterialIDs .ne. nelem ) then
            call DataFile%RaiseError("Elements without material were found")
        endif

        allocate( MatID(nelem))

        do e = 1,nelem
            call DataFile%GetNextString()
            call DataFile%CheckError
            call DataFile%GetOriginalLine(line)
            read(line,*) id , MatID(id)
        enddo

        call DataFile%GetNextString(string)
        IF (.not.DataFile%CompareStrings(string,'end material id')) call DataFile%RaiseError("Expected: End Material ID")


        call DataFile%GetNextString(string)
        call DataFile%CheckError

        if (.not.DataFile%CompareStrings(string,"CONNECTIVITY")) call DataFile%RaiseError("CONNECTIVITY was not found")
        do e=1,Nelem
                call DataFile%GetNextString()
                call DataFile%CheckError
                call DataFile%GetOriginalLine(line)
                Read(line,*) id , GeoType , ENnodes , (ElemConec(j),j=1,ENnodes)

                FoundMaterial=.false.
                do i=1,size(MaterialList)
                    if (MaterialList(i)%MaterialID == MatID(id)) then
                        Material => MaterialList(i)
                        FoundMaterial = .true.
                    endif
                enddo
                if (.not.FoundMaterial) then
                    call DataFile%RaiseError("Element's Material was not found")
                endif

                !---------------------------------------------------------------------------------------------------------------
                selectcase (PreProcessorID)
                !===============================================================================================================
                    case (PreProcessors%Gid7)

                        IF (GeoType==1) then
                            IsQuadratic=.true.
                        elseif(GeoType==0) then
                            IsQuadratic=.false.
                        else
                            stop "Preprocessor GiD 7 :: ReadMesh :: IsQuadratic must be 1 or 0"
                        endif
                        call ElementIdentifier_IsQuadratic(IsQuadratic, Ndime ,ENnodes,AnalysisSettings%ElementTech,ElemType)
                !===============================================================================================================
                    case (PreProcessors%Gid12)
                        call ElementIdentifier( GeoType , ENnodes , AnalysisSettings%ElementTech, ElemType )
                !===============================================================================================================
                    case default
                        stop "ReadMesh :: Preprocessor not available"
                end select
                !---------------------------------------------------------------------------------------------------------------

                call ElementConstructor( ElementList(id)%el , ElemConec(1:ENnodes) ,ElemType , GlobalNodesList, Material, AnalysisSettings)

        enddo

        call DataFile%GetNextString(string)
        IF (.not.DataFile%CompareStrings(string,'end connectivity')) call DataFile%RaiseError("Expected: End connectivity")


        call DataFile%GetNextString(string)
        IF (.not.DataFile%CompareStrings(string,'end mesh') ) then
            call DataFile%RaiseError("End of block was expected. BlockName=MESH")
        endif

    end subroutine
    !=======================================================================================================================


! TODO (Thiago#2#): Criar rotinas separadas para a leitura da malha de cada pre processador.
    !=======================================================================================================================
    subroutine ReadMesh(DataFile, GlobalNodesList, ElementList, AnalysisSettings, MaterialList, PreProcessorID)

        use Interfaces
        implicit none

        type (ClassParser)                                    :: DataFile
        type (ClassAnalysis)                                  :: AnalysisSettings
        type (ClassNodes) , pointer , dimension(:)            :: GlobalNodesList
        type (ClassElementsWrapper) , pointer , dimension(:)  :: ElementList
        type (ClassConstitutiveModelWrapper) , pointer , dimension(:) :: MaterialList
        Integer                                               :: PreProcessorID


        character(len=255)::string , OptionName,OptionValue , line
        logical :: FoundMaterial , IsQuadratic
        real(8) , allocatable , dimension(:) :: Coords
        integer , allocatable ,dimension(:) :: MatID
        integer::i,e,id,j
        integer::nnodes,nelem,ndime,ElemType, ElemConec(MaxElementNodes),GeoType,ENnodes , NumberOfMaterialIDs
        class(ClassConstitutiveModelWrapper)  , pointer :: Material

        call DataFile%GetNextString(string)

        do i=1,3
            call DataFile%GetNextOption(OptionName,OptionValue)
            if (DataFile%Error) then
                call DataFile%ShowError
                write(*,*) "Expected Nnodes,Nelem or Ndime"
                stop
            end if
            if (DataFile%CompareStrings(OptionName,"nnodes")) then

                nnodes = OptionValue
                call DataFile%CheckError
                if (nnodes<=0) call DataFile%RaiseError("Nnodes must be positive")
            elseif (DataFile%CompareStrings(OptionName,"ndime")) then

                ndime = OptionValue
                call DataFile%CheckError
                if (ndime<=0) call DataFile%RaiseError("Ndime must be positive")
            elseif (DataFile%CompareStrings(OptionName,"nelem")) then

                nelem = OptionValue
                call DataFile%CheckError
                if (nelem<=0) call DataFile%RaiseError("Nelem must be positive")
            else
                call DataFile%RaiseError("Expected Nnodes,Nelem or Ndime")
            endif
        enddo

        allocate( GlobalNodesList( Nnodes ) , ElementList(Nelem) , Coords(Ndime) )

        call DataFile%GetNextString(string)
        call DataFile%CheckError
        if (.not.DataFile%CompareStrings(string,"Coordinates")) call DataFile%RaiseError("Coordinates were not found")


        do i=1,nnodes
            call DataFile%GetNextString()
            call DataFile%CheckError
            call DataFile%GetOriginalLine(line)
            Read(line,*) id, ( Coords(j) , j=1,Ndime )
            call GlobalNodesList(i)%NodeConstructor( Coords , i , AnalysisSettings % NDOFnode )
        enddo

        call DataFile%GetNextString(string)
        IF (.not.DataFile%CompareStrings(string,'end coordinates')) call DataFile%RaiseError("Expected: End Coordinates")


        call DataFile%GetNextString(string)
        call DataFile%CheckError

        if (.not.DataFile%CompareStrings(string,"MATERIAL ID")) call DataFile%RaiseError("MATERIAL ID was not found")

        call DataFile%GetNextString(string) ; call DataFile%CheckError

        NumberOfMaterialIDs = string
        call DataFile%CheckError

        if (NumberOfMaterialIDs .ne. nelem ) then
            call DataFile%RaiseError("Elements without material were found")
        endif

        allocate( MatID(nelem))

        do e = 1,nelem
            call DataFile%GetNextString()
            call DataFile%CheckError
            call DataFile%GetOriginalLine(line)
            read(line,*) id , MatID(id)
        enddo

        call DataFile%GetNextString(string)
        IF (.not.DataFile%CompareStrings(string,'end material id')) call DataFile%RaiseError("Expected: End Material ID")


        call DataFile%GetNextString(string)
        call DataFile%CheckError

        if (.not.DataFile%CompareStrings(string,"CONNECTIVITY")) call DataFile%RaiseError("CONNECTIVITY was not found")
        do e=1,Nelem
                call DataFile%GetNextString()
                call DataFile%CheckError
                call DataFile%GetOriginalLine(line)
                Read(line,*) id , GeoType , ENnodes , (ElemConec(j),j=1,ENnodes)

                FoundMaterial=.false.
                do i=1,size(MaterialList)
                    if (MaterialList(i)%MaterialID == MatID(id)) then
                        Material => MaterialList(i)
                        FoundMaterial = .true.
                    endif
                enddo
                if (.not.FoundMaterial) then
                    call DataFile%RaiseError("Element's Material was not found")
                endif

                !---------------------------------------------------------------------------------------------------------------
                selectcase (PreProcessorID)
                !===============================================================================================================
                    case (PreProcessors%Gid7)

                        IF (GeoType==1) then
                            IsQuadratic=.true.
                        elseif(GeoType==0) then
                            IsQuadratic=.false.
                        else
                            stop "Preprocessor GiD 7 :: ReadMesh :: IsQuadratic must be 1 or 0"
                        endif
                        call ElementIdentifier_IsQuadratic(IsQuadratic, Ndime ,ENnodes,AnalysisSettings%ElementTech,ElemType)
                !===============================================================================================================
                    case (PreProcessors%Gid12)
                        call ElementIdentifier( GeoType , ENnodes , AnalysisSettings%ElementTech, ElemType )
                !===============================================================================================================
                    case default
                        stop "ReadMesh :: Preprocessor not available"
                end select
                !---------------------------------------------------------------------------------------------------------------

                call ElementConstructor( ElementList(id)%el , ElemConec(1:ENnodes) ,ElemType , GlobalNodesList, Material, AnalysisSettings)

        enddo

        call DataFile%GetNextString(string)
        IF (.not.DataFile%CompareStrings(string,'end connectivity')) call DataFile%RaiseError("Expected: End connectivity")


        call DataFile%GetNextString(string)
        IF (.not.DataFile%CompareStrings(string,'end mesh') ) then
            call DataFile%RaiseError("End of block was expected. BlockName=MESH")
        endif

    end subroutine
    !=======================================================================================================================

    !=======================================================================================================================
    subroutine ReadBoundaryConditions(TimeFileName,DataFile,BC,AnalysisSettings)
        implicit none

        character(len=100)                  :: TimeFileName
        type (ClassParser)                  :: DataFile
        type (ClassBoundaryConditions)      :: BC
        type (ClassAnalysis)                :: AnalysisSettings




        character(len=255)::string, FileName
        integer:: i,j
        integer::nFS,nNF,nND,NDOFnode
        integer , allocatable , dimension(:,:) :: FSArray, NFArray, NDArray
        character*100 , allocatable , dimension(:) :: TablesList
        character*100 , allocatable , dimension(:,:) :: NFTable, NDTable


        NDOFnode = AnalysisSettings % NDOFnode

        !Ler a palavra "BOUNDARY CONDITIONS" no arquivo da malha e BC
        call DataFile%GetNextString(string)

        !Começa a leitura do bloco BOUNDARY CONDITIONS.
        call DataFile%GetNextString(string)


        LoopBC: do while (.true.)

            call DataFile%CheckError

            if (EOF(DataFile)) call DataFile%RaiseError("ReadBoundaryConditions:: End of File reached.")

    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                if (DataFile%CompareStrings(string,"fixed supports")) then

                    call DataFile%GetNextString(string) ; call DataFile%CheckError

                    nFS = string

                    if (DataFile%Error) then
                        call DataFile%ShowError
                        write(*,*) "ReadBoundaryConditions::Expected: Number of Fixed supports"
                        stop
                    endif

                    allocate ( FSArray(nFS,NDOFnode+1) )
                    do i = 1,nFS
                        call DataFile%GetNextString() ; call DataFile%CheckError ; call DataFile%GetOriginalLine(string)
                        Read(string,*) ( FSArray(i,j), j=1,NDOFnode+1 )
                    end do

                    call BC%FixedSupport%FixedSupportConstructor (FSArray, NDOFnode)

    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                elseif (DataFile%CompareStrings(string,"Nodal Load")) then

                    call DataFile%GetNextString(string) ; call DataFile%CheckError

                    nNF = string

                    if (DataFile%Error) then
                        call DataFile%ShowError
                        write(*,*) "ReadBoundaryConditions::Expected: Number of Nodal Loads"
                        stop
                    endif

                    allocate ( NFArray(nNF,NDOFnode+1) )
                    allocate ( NFTable(nNF,NDOFnode) )

                    do i = 1,nNF
                        call DataFile%GetNextString() ; call DataFile%CheckError; call DataFile%GetOriginalLine(string)
                        Read(string,*) NFArray(i,1), ( NFArray(i,j+1),NFTable(i,j), j=1,NDOFnode )
                    end do

    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                elseif (DataFile%CompareStrings(string,"nodal displacement")) then

                    call DataFile%GetNextString(string) ; call DataFile%CheckError

                    nND=string

                    if (DataFile%Error) then
                        call DataFile%ShowError
                        write(*,*) "ReadBoundaryConditions::Expected: Number of Nodal Displacements"
                        stop
                    endif

                    allocate ( NDArray(nND,NDOFnode+1) )
                    allocate ( NDTable(nND,NDOFnode) )

                    do i = 1,nND
                        call DataFile%GetNextString() ; call DataFile%CheckError; call DataFile%GetOriginalLine(string)
                        Read(string,*) NDArray(i,1), ( NDArray(i,j+1),NDTable(i,j), j=1,NDOFnode )
                    end do

    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                elseif (DataFile%CompareStrings(string,"line load")) then
                    call DataFile%RaiseError("Line Load not implemented.")

    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                elseif (DataFile%CompareStrings(string,"surface load")) then
                    call DataFile%RaiseError("Surface Load not implemented.")

    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                elseif (DataFile%CompareStrings(string,"load cases")) then
                    !ignorar

    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                elseif (DataFile%CompareStrings(string,"END BOUNDARY CONDITIONS")) then
                    exit LoopBC

    !---------------------------------------------------------------------------------------------------------------------------------------------------------
                else
                    call DataFile%RaiseError("ReadBoundaryConditions::Condition not detected.")
                endif

                call DataFile%GetNextString(string)
            end do LoopBC


        !************************************************************************************
        ! READING LOAD CASE TABLES : "INPUT_FILE.TAB"
        !************************************************************************************

        ! Checks multiple references to the same table
        call AnalyzeLoadHistoryTables ( NFArray, NFTable, NDArray, NDTable, TablesList )

        ! Reading only the tables used in the analysis.
        allocate(BC%SetOfLoadHistory( size(TablesList) ))


        do i=1,size(TablesList)
            call BC%SetOfLoadHistory(i)%ReadTimeDiscretization(TimeFileName)
            call BC%SetOfLoadHistory(i)%ReadValueDiscretization(TablesList(i))
        enddo

        ! Setting the prescribed nodes with its respective tables.
        ! Nodal Forces
        call CreateNodalBoundaryCondition( NFArray,NFTable,BC%SetOfLoadHistory,BC%NodalForceBC )
        ! Nodal Displacements
        call CreateNodalBoundaryCondition( NDArray,NDTable,BC%SetOfLoadHistory,BC%NodalDispBC )

    end subroutine
    !=======================================================================================================================



end module
