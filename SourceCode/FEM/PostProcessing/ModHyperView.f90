module ModHyperView

    use ModPostProcessors
    use ModContinuumMechanics


    implicit none

    !************************************************************************************
    type, extends(ClassPostProcessor) :: ClassHyperView

        character(len=10) :: GaussPointName=''
        integer :: FileNumber

        contains

            procedure ::  InitializePostProcessorFile =>  InitializePostProcessorFile_HyperView
            procedure ::  WritePostProcessorResult    =>  WritePostProcessorResult_HyperView
            procedure ::  ExportOnGaussPointsHV
            procedure ::  ExportOnNodesHV
    end type
    !************************************************************************************


    contains


    !************************************************************************************
    subroutine InitializePostProcessorFile_HyperView(this, FEA)

            use FEMAnalysis

            implicit none

            class(ClassHyperView)   :: this
            class(ClassFEMAnalysis) :: FEA


            integer::FileNumber , ElementType
            character(len=10) :: GaussPointName
            class(ClassElement), pointer :: Element

            FileNumber = 1

            this%FileNumber = FileNumber

            open (FileNumber,file=this%FileName,status='unknown')

            write(FileNumber,'(a)') adjustl('ALTAIR ASCII FILE')


     end subroutine
    !************************************************************************************



    !************************************************************************************
    subroutine WritePostProcessorResult_HyperView(this, FEA)

            use FEMAnalysis
            use Parser
            use ModCharacter


            implicit none

            class (ClassHyperView)                  :: this
            class( ClassFEMAnalysis )               :: FEA
            real(8), allocatable, dimension(:,:)    :: NodalValues
            real(8), allocatable, dimension(:,:,:)  :: GaussPointlValues
            real(8) :: Tensor(3,3), Tensor_voigt(6)
            integer :: i, j, v, e, gp, nelem, ngp, nnodes, n
            class(ClassConstitutiveModel) , pointer :: GaussPoint

            type (ClassParser)                      :: Comp

            real(8) , dimension(9)            :: UD_Variable
            integer                           :: UD_ID, UD_Length, UD_VariableType, VariableType, VariableLength
            character(len=255)                :: UD_Name
            logical :: FoundUserVariable
            character(len=255)                :: LoadCaseChar
            integer, allocatable, dimension(:,:)   :: Conec


        call Comp%Setup()

        do v = 1,size(this%VariableNames)


            select case (this%VariableNameID(v))


                case (VariableNames%Displacements)

                    allocate ( NodalValues(size(FEA%GlobalNodesList),FEA%AnalysisSettings%NDOFnode) )

                    do i = 1 , size(NodalValues,1)
                        do j = 1, size(NodalValues,2)
                            NodalValues(i,j) = FEA%U( FEA%AnalysisSettings%NDOFnode*(i -1) + j )
                        enddo
                    enddo

                    LoadCaseChar = FEA%LoadCase
                    LoadCaseChar = RemoveSpaces(LoadCaseChar)
                    call this%ExportOnNodesHV ( trim(this%VariableNames(v)) ,LoadCaseChar , FEA%Time , NodalValues , 2)

                    deallocate(NodalValues)


                case (VariableNames%CauchyStress)
! TODO (Thiago#2#): O HyperView lê os resultados nos pontos de gauss segundo a conectividade dos nós. Implementado somente para elementos com a mesma quantidade de nós e pontos de gauss.


                    nelem = size( FEA%ElementList )
                    ngp = size(FEA%ElementList(1)%el%GaussPoints)
                    nnodes = size(FEA%ElementList(1)%El%ElementNodes)
                    allocate( GaussPointlValues( nelem , ngp , 6 ) )
                    allocate( Conec(nelem , nnodes ) )

                    do e=1,nelem
                        do gp=1,ngp
                            GaussPoint => FEA%ElementList(e)%El%GaussPoints(gp)
                            GaussPointlValues(e,gp,1:FEA%AnalysisSettings%StressSize) = GaussPoint%Stress
                        enddo
                        do n = 1,nnodes
                            Conec(e,n) = FEA%ElementList(e)%El%ElementNodes(n)%Node%ID
                        enddo
                    enddo

                    LoadCaseChar = FEA%LoadCase
                    LoadCaseChar = RemoveSpaces(LoadCaseChar)
                    call this%ExportOnGaussPointsHV( this%VariableNames(v), LoadCaseChar, FEA%Time , Conec, &
                                                     GaussPointlValues(:,:,1:FEA%AnalysisSettings%StressSize)  , 3 )

                    deallocate(GaussPointlValues, Conec)



                case (VariableNames%LogarithmicStrain)

                    nelem = size( FEA%ElementList )
                    ngp = size(FEA%ElementList(1)%el%GaussPoints)
                    nnodes = size(FEA%ElementList(1)%El%ElementNodes)
                    allocate( GaussPointlValues( nelem , ngp , 6 ) )
                    allocate( Conec(nelem , nnodes ) )

                    do e=1,nelem
                        do gp=1,ngp
                            GaussPoint => FEA%ElementList(e)%El%GaussPoints(gp)
                            Tensor_voigt = 0.0d0
                            Tensor = 0.0d0
                            Tensor = StrainMeasure(GaussPoint%F,StrainMeasures%Logarithmic)
                            Tensor_voigt = Convert_to_Voigt_3D_Sym( Tensor )
                            GaussPointlValues(e,gp,1:FEA%AnalysisSettings%StrainSize) =  Tensor_voigt
                        enddo
                        do n = 1,nnodes
                            Conec(e,n) = FEA%ElementList(e)%El%ElementNodes(n)%Node%ID
                        enddo
                    enddo


                    LoadCaseChar = FEA%LoadCase
                    LoadCaseChar = RemoveSpaces(LoadCaseChar)
                    call this%ExportOnGaussPointsHV( this%VariableNames(v), LoadCaseChar, FEA%Time, Conec, &
                                                     GaussPointlValues(:,:,1:FEA%AnalysisSettings%StressSize)  , 3 )

                    deallocate(GaussPointlValues, Conec)



                 case (VariableNames%UserDefined)

                    ! TODO (Jan#1#11/18/15): Colocar para exportar todos os dados do usuário também

                    nelem = size( FEA%ElementList )
                    ngp = size(FEA%ElementList(1)%el%GaussPoints)
                    nnodes = size(FEA%ElementList(1)%El%ElementNodes)
                    allocate( GaussPointlValues( nelem , ngp , 6 ) )
                    allocate( Conec(nelem , nnodes ) )
                    GaussPointlValues = 0.0d0
                    Conec = 0

       
                    !8888888888888888888888888888888888888888888888888888888888888888888
                    do e=1,nelem
                            
                        do gp=1,ngp
                                
                                GaussPoint => FEA%ElementList(e)%El%GaussPoints(gp)
                                
                                UD_ID = 0 !Pegar o numero de variaveis implementadas no ponto de Gauss                         
                                call GaussPoint%GetResult( UD_ID, UD_Name, UD_Length, UD_Variable, UD_VariableType )                               
                              
                                !Loop sobre as numero de variaveis do ponto de gauss
                                FoundUserVariable = .false.
                                LOOP_USER_DEFINED: do UD_ID = 1,UD_Length
                                
                                    !Acessando o Ponto de Gauss para obter o nome das variáveis
                                    call GaussPoint%GetResult( UD_ID, UD_Name, UD_Length, UD_Variable, UD_VariableType ) 
                                    
                                    !Encontrar a variável que o usuário quer
                                    FoundUserVariable = Comp%CompareStrings( this%VariableNames(v),UD_Name)
                                
                                    if (FoundUserVariable) then  
                                        ! Armazenando a variável
                                        GaussPointlValues(e,gp,1:UD_Length) = UD_Variable(1:UD_Length)
                                        ! Armazendo o tipo e tamanho de variável para a rotina de exportação
                                        VariableType = UD_VariableType
                                        VariableLength = UD_Length
                                    endif
                                
                            enddo  LOOP_USER_DEFINED
                                
                            
                        enddo 
 
                        do n = 1,nnodes
                            Conec(e,n) = FEA%ElementList(e)%El%ElementNodes(n)%Node%ID
                        enddo
                        
                    enddo
                    
                    
                        
                    LoadCaseChar = FEA%LoadCase
                    LoadCaseChar = RemoveSpaces(LoadCaseChar)
                    call this%ExportOnGaussPointsHV( this%VariableNames(v), LoadCaseChar, FEA%Time, Conec, &
                                                     GaussPointlValues(:,:,1:VariableLength), VariableType )
                            
                        
                    deallocate(GaussPointlValues, Conec)
                        
                    !888888888888888888888888888888888888888888888888888888888888888888888888888
                    
                    ! só funciona se todos os materiais tiverem as mesmas variaveis nos pontos de gauss
                    !GaussPoint => FEA%ElementList(1)%El%GaussPoints(1)

                    !UD_ID = 0 !Pegar o numero de variaveis implementadas no ponto de gauss
                    !call GaussPoint%GetResult( UD_ID, UD_Name, UD_Length, UD_Variable, UD_VariableType )

                   ! Loop sobre as numero de variaveis do ponto de gauss
                    !Primeiramente vamos encontrar a variável que o usuário quer
                    !FoundUserVariable = .false.
                    !LOOP_USER_DEFINED: do UD_ID = 1,UD_Length
                    !
                    !     
                    !    
                    !    GaussPoint => FEA%ElementList(1)%El%GaussPoints(1)
                    !    
                    !    call GaussPoint%GetResult( UD_ID, UD_Name, UD_Length, UD_Variable, UD_VariableType )
                    !    
                    !    FoundUserVariable = Comp%CompareStrings( this%VariableNames(v),UD_Name)
                    !    
                    !    if (FoundUserVariable) then
                    !    
                    !        do e=1,nelem
                    !            do gp=1,ngp
                    !                GaussPoint => FEA%ElementList(e)%El%GaussPoints(gp)
                    !                call GaussPoint%GetResult( UD_ID, UD_Name, UD_Length, UD_Variable, UD_VariableType )
                    !                GaussPointlValues(e,gp,1:UD_Length) = UD_Variable(1:UD_Length)
                    !            enddo
                    !            do n = 1,nnodes
                    !                Conec(e,n) = FEA%ElementList(e)%El%ElementNodes(n)%Node%ID
                    !            enddo
                    !        enddo
                    !    
                    !        LoadCaseChar = FEA%LoadCase
                    !        LoadCaseChar = RemoveSpaces(LoadCaseChar)
                    !        call this%ExportOnGaussPointsHV( this%VariableNames(v), LoadCaseChar, FEA%Time, Conec, &
                    !                                         GaussPointlValues(:,:,1:UD_Length), UD_VariableType )
                    !    
                    !        deallocate(GaussPointlValues, Conec)
                    !    
                    !    endif
                    !    
                    !    
                    !
                    !enddo LOOP_USER_DEFINED



            end select

        enddo



    end subroutine
    !************************************************************************************



    !************************************************************************************
    subroutine Constructor_HyperView( PostProcessor, PostProcessorResults, PostProcessorFileName )

        implicit none

        class(ClassPostProcessor), pointer   :: PostProcessor
        type(ClassHyperView), pointer        :: PostProcessorHyperView

        character(len=255), allocatable, dimension(:) :: PostProcessorResults
        character(len=255)                            :: PostProcessorFileName
        integer :: i

        allocate(PostProcessorHyperView)

        PostProcessorHyperView%FileName = PostProcessorFileName

        allocate(PostProcessorHyperView%VariableNames(size(PostProcessorResults)))
        allocate(PostProcessorHyperView%VariableNameID(size(PostProcessorResults)))

        PostProcessorHyperView%VariableNames = PostProcessorResults

        do i = 1,size(PostProcessorResults)

            PostProcessorHyperView%VariableNameID(i) = ParseVariableName(PostProcessorResults(i))

        enddo


        PostProcessor => PostProcessorHyperView


    end subroutine
    !************************************************************************************




        ! Export Results on Nodes - HyperView
        !----------------------------------------------------------------------------------------
        subroutine ExportOnNodesHV( this , Name , LoadCaseChar, Time , Variable , VariableType)

            implicit none

            class(ClassHyperView) :: this
            integer :: FileNumber , VariableType
            Real(8) :: Time
            character(len=*) :: Name, LoadCaseChar
            real(8),dimension(:,:) :: Variable

            character(30) :: DataType , ComponentName,Form,Complement
            integer::e,gp,i,j,nComponents



             integer,parameter :: Scalar=1 , Vector=2 , Tensor=3

             FileNumber = this%FileNumber

            nComponents = size(Variable,2)

            if     ((VariableType==Scalar).and.(nComponents==1)) then
                DataType = '(s)'
                Form = '1X,E16.9'
                Complement = ''

            elseif ((VariableType==Vector).and.(nComponents==2)) then
                DataType = '(v)'
                Form = '2(1X,E16.9)'
                Complement = '   0.0'

            elseif ((VariableType==Vector).and.(nComponents==3)) then
                DataType = '(v)'
                Form = '3(1X,E16.9)'
                Complement = ''

            elseif ((VariableType==Tensor).and.(nComponents==3)) then
                DataType = '(2t)'
                Form = '3(1X,E16.9)'
                Complement = ''

            elseif ((VariableType==Tensor).and.(nComponents==4)) then
                DataType = '(t)'
                Form = '4(1X,E16.9)'
                Complement='   0.0   0.0'

            elseif ((VariableType==Tensor).and.(nComponents==6)) then
                DataType = '(t)'
                Form = '6(1X,E16.9)'
                Complement=''

            else

                stop "Error on ExportOnNodesHV"

            endif


            write(FileNumber,'(a)') adjustl( '$TITLE = Transient analysis' )
            write(FileNumber,'(a)') adjustl( '$SUBCASE = '//trim(LoadCaseChar)//'  Load Case '//trim(LoadCaseChar) )
            write(FileNumber,'(a)') adjustl( '$BINDING = NODE' )
            write(FileNumber,'(a)') adjustl( '$COLUMN_INFO = ENTITY_ID' )
            write(FileNumber,'(a)') adjustl( '$RESULT_TYPE = '//trim(Name)//trim(DataType) )
            write(FileNumber,'(a,1X,E16.9,a)') adjustl('$TIME  = '),Time,' sec'


            do i=1,size(Variable,1)
                write(FileNumber,'(I0,'//trim(Form)//',A)') i ,  (Variable(i,j) , j=1,nComponents), trim(Complement)
            enddo


        end subroutine


        ! Export Results on GaussPoints - HyperView
        !----------------------------------------------------------------------------------------
        subroutine ExportOnGaussPointsHV( this ,  Name , LoadCaseChar, Time, Conec, Variable, VariableType )

            implicit none
            class(ClassHyperView) :: this
            integer::FileNumber ,VariableType
            Real(8)::Time
            character(*)::Name, LoadCaseChar
            real(8),dimension(:,:,:)::Variable
            integer, dimension(:,:) :: Conec

            character(100) :: DataType , ComponentName,Form , Complement
            integer::e,gp,i,j,nComponents

            integer,parameter :: Scalar=1 , Vector=2 , Tensor=3

            FileNumber = this%FileNumber

            nComponents = size(Variable,3)

            if     ((VariableType==Scalar).and.(nComponents==1)) then
                DataType = '(s)'
                Form = '1X,E16.9'
                Complement=''

            elseif ((VariableType==Vector).and.(nComponents==2)) then
                DataType = '(v)'
                Form = '2(1X,E16.9)'
                Complement='  0.0 '

            elseif ((VariableType==Vector).and.(nComponents==3)) then
                DataType = '(v)'
                Form = '3(1X,E16.9)'
                Complement=''

            elseif ((VariableType==Tensor).and.(nComponents==3)) then
                DataType = '(2t)'
                Form = '3(1X,E16.9)'
                Complement=' '

            elseif ((VariableType==Tensor).and.(nComponents==4)) then
                DataType = '(t)'
                Form = '4(1X,E16.9)'
                Complement='  0.0   0.0 '

            elseif ((VariableType==Tensor).and.(nComponents==6)) then
                DataType = '(t)'
                Form = '6(1X,E16.9)'
                Complement = ' '

            else
                stop "Error on ExportOnGaussPoints"

            endif


            write(FileNumber,'(a)') adjustl( '$TITLE = Transient analysis' )
            write(FileNumber,'(a)') adjustl( '$SUBCASE = '//trim(LoadCaseChar)//'  Load Case '//trim(LoadCaseChar) )
            write(FileNumber,'(a)') adjustl( '$BINDING = ELEMENT' )
            write(FileNumber,'(a)') adjustl( '$COLUMN_INFO = ENTITY_ID GRID_ID' )
            write(FileNumber,'(a)') adjustl( '$RESULT_TYPE = '//trim(Name)//trim(DataType) )
            write(FileNumber,'(a)') adjustl( '$SYS_ID = 1' )
            write(FileNumber,'(a,1X,E16.9,a)') adjustl('$TIME  = '),Time,' sec'
            ! NOTE (Thiago#1#): SYS_ID = 1 exporta os resultados no sistema global da análise.

! TODO (Thiago#1#): HyperView - Resultados nos pontos de gauss coerentes com ordenação dos elementos em ordem crescente.
            do e=1,size(Variable,1)
                do gp=1,size(Variable,2)
                    write(FileNumber,'(I0,(1X,I0),'//trim(Form)//',A)') e , Conec(e,gp),  (Variable(e,gp,j) , j=1,nComponents) , trim(Complement)
                enddo
            enddo

        end subroutine


end module



