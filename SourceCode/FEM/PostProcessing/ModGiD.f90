module ModGid

    use ModPostProcessors


    implicit none

    !************************************************************************************
    type, extends(ClassPostProcessor) :: ClassGiD

        character(len=10) :: GaussPointName=''
        integer :: FileNumber

        contains

            procedure ::  InitializePostProcessorFile =>  InitializePostProcessorFile_GiD
            procedure ::  WritePostProcessorResult    =>  WritePostProcessorResult_GiD
            procedure :: ExportOnGaussPoints
            procedure :: ExportOnNodes
            !procedure :: Close => CloseGidFile
    end type
    !************************************************************************************


    contains


    !************************************************************************************
    subroutine InitializePostProcessorFile_GiD(this, FEA)

            use FEMAnalysis

            implicit none

            class(ClassGiD)           :: this
            class( ClassFEMAnalysis ) :: FEA



            integer::FileNumber , ElementType
            character(len=10) :: GaussPointName
            class(ClassElement), pointer :: Element

            FileNumber = 1

            this%FileNumber = FileNumber

            !arquivo de resultados do GiD -----------------------------------------
            !this%ResultFile  = this%DatFile(1:len(trim(this%DatFile))-4) // '.post.res'

            open (FileNumber,file=this%FileName,status='unknown')

            write(FileNumber,*) 'GiD Post Results File 1.0'

            Element => FEA%ElementList(1)%El

            select type ( Element )

                type is (ClassElementTri3)
                    write(FileNumber,*) 'GaussPoints "Tri3" ElemType Triangle'
                    write(FileNumber,*) 'Number Of Gauss Points: 1'
                    write(FileNumber,*) 'Natural Coordinates: internal'
                    write(FileNumber,*) 'end gausspoints'
                    GaussPointName = "Tri3"

                type is (ClassElementQuad4)
                    write(FileNumber,*) 'GaussPoints "Quad4" ElemType Quadrilateral'
                    write(FileNumber,*) 'Number Of Gauss Points: 4'
                    write(FileNumber,*) 'Natural Coordinates: internal'
                    write(FileNumber,*) 'end gausspoints'
                    GaussPointName="Quad4"

                type is (ClassElementHexa8)
                    write(FileNumber,*) 'GaussPoints "Hexa8" ElemType Hexahedra'
                    write(FileNumber,*) 'Number Of Gauss Points: 8'
                    write(FileNumber,*) 'Natural Coordinates: internal'
                    write(FileNumber,*) 'end gausspoints'
                    GaussPointName="Hexa8"

                type is (ClassElementTetra4)
                    write(FileNumber,*) 'GaussPoints "Tetra4" ElemType Tetrahedra'
                    write(FileNumber,*) 'Number Of Gauss Points: 1'
                    write(FileNumber,*) 'Natural Coordinates: internal'
                    write(FileNumber,*) 'end gausspoints'
                    GaussPointName="Tetra4"

            class default
                write(*,*) "WriteResultFileHeader :: Element not identified"
                stop

            end select

            this%GaussPointName = GaussPointName

     end subroutine
    !************************************************************************************



    !************************************************************************************
    subroutine WritePostProcessorResult_GiD(this, FEA)

            use FEMAnalysis

            implicit none

            class (ClassGiD)          :: this
            class( ClassFEMAnalysis ) :: FEA
            real(8), allocatable, dimension(:,:) :: NodalValues
            real(8), allocatable, dimension(:,:,:) :: GaussPointlValues
            integer :: i, j, v, e, gp, nelem, ngp

        do v = 1,size(this%VariableNames)

            select case (this%VariableNameID(v))



                case (VariableNames%Displacements)

                    allocate ( NodalValues(size(FEA%GlobalNodesList),FEA%AnalysisSettings%NDOFnode) )
                    do i = 1 , size(NodalValues,1)
                        do j = 1, size(NodalValues,2)
                            NodalValues(i,j) = FEA%U( FEA%AnalysisSettings%NDOFnode*(i -1) + j )
                        enddo
                    enddo

                    call this%ExportOnNodes ( trim(this%VariableNames(v)) , FEA%Time , NodalValues , 2)
                    deallocate(NodalValues)




                case (VariableNames%CauchyStress)
                ! TODO (Thiago#1#11/17/15): GiD - não exporta resultados com resultados de malha mista

                    nelem = size( FEA%ElementList )
                    ngp = size(FEA%ElementList(1)%el%GaussPoints)
                    allocate( GaussPointlValues( nelem , ngp , 6 ) )

                    do e=1,nelem
                        do gp=1,ngp
                            GaussPointlValues(e,gp,1:FEA%AnalysisSettings%StressSize) = FEA%ElementList(e)%El%GaussPoints(gp)%Stress
                        enddo
                    enddo

                    call this%ExportOnGaussPoints( this%VariableNames(v) , FEA%Time , GaussPointlValues(:,:,1:FEA%AnalysisSettings%StressSize)  , 3 )


            end select

        enddo



    end subroutine
    !************************************************************************************



    !************************************************************************************
    subroutine Constructor_GiD( PostProcessor, PostProcessorResults, PostProcessorFileName )

        implicit none

        class(ClassPostProcessor), pointer   :: PostProcessor
        type(ClassGiD), pointer              :: PostProcessorGiD7

        character(len=255), allocatable, dimension(:) :: PostProcessorResults
        character(len=255)                            :: PostProcessorFileName
        integer :: i

        allocate(PostProcessorGiD7)

        PostProcessorGiD7%FileName = PostProcessorFileName

        allocate(PostProcessorGiD7%VariableNames(size(PostProcessorResults)))
        allocate(PostProcessorGiD7%VariableNameID(size(PostProcessorResults)))

        PostProcessorGiD7%VariableNames = PostProcessorResults

        do i = 1,size(PostProcessorResults)

            PostProcessorGiD7%VariableNameID(i) = ParseVariableName(PostProcessorResults(i))

        enddo


        PostProcessor => PostProcessorGiD7


    end subroutine
    !************************************************************************************




        ! Export Results on Nodes - GiD
        !----------------------------------------------------------------------------------------
        subroutine ExportOnNodes( this , Name , Time , Variable , VariableType)

            implicit none

            class(ClassGiD) :: this
            integer :: FileNumber , VariableType
            Real(8) :: Time
            character(len=*) :: Name
            real(8),dimension(:,:) :: Variable

            character(30) :: DataType , ComponentName,Form,Complement
            integer::e,gp,i,j,nComponents


             integer,parameter :: Scalar=1 , Vector=2 , Tensor=3

             FileNumber = this%FileNumber

            nComponents = size(Variable,2)

            if     ((VariableType==Scalar).and.(nComponents==1)) then
                DataType='Scalar'
                ComponentName='"Scalar"'
                Form = '1X,E16.9'
                Complement=''

            elseif ((VariableType==Vector).and.(nComponents==2)) then
                DataType='Vector'
                ComponentName='"x","y"'
                Form = '2(1X,E16.9)'
                Complement=''

            elseif ((VariableType==Vector).and.(nComponents==3)) then
                DataType='Vector'
                ComponentName='"x", "y", "z"'
                Form = '3(1X,E16.9)'
                Complement=''

            elseif ((VariableType==Tensor).and.(nComponents==3)) then
                DataType='PlainDeformationMatrix'
                ComponentName='"11","22","12","33"'
                Form = '3(1X,E16.9)'
                Complement=' 0.0'

            elseif ((VariableType==Tensor).and.(nComponents==4)) then
                DataType='PlainDeformationMatrix'
                ComponentName='"11","22","12","33"'
                Form = '4(1X,E16.9)'
                Complement=''

            elseif ((VariableType==Tensor).and.(nComponents==6)) then
                DataType='Matrix'
                ComponentName='"11","22","33","12","23","13"'
                Form = '6(1X,E16.9)'
                Complement=''

            else

                stop "Error on ExportOnNodes"

            endif

            write(FileNumber,'(a,E16.9,a)') 'Result "'//trim(Name)//'" "Analysis" ',Time, ' '//trim(DataType)//' OnNodes'
            Write(FileNumber,*) 'ComponentNames '//trim(ComponentName)
            write(FileNumber,*) 'Values'
            do i=1,size(Variable,1)
                Write(FileNumber,'(I8,'//trim(Form)//',A)') i ,  (Variable(i,j) , j=1,nComponents) , trim(Complement)
            enddo

            write(FileNumber,*) 'End Values'

        end subroutine


        ! Export Results on GaussPoints - GiD
        !----------------------------------------------------------------------------------------
        subroutine ExportOnGaussPoints( this ,  Name , Time , Variable , VariableType )

            implicit none
            class(ClassGiD) :: this
            integer::FileNumber ,VariableType
            Real(8)::Time
            character(*)::Name
            real(8),dimension(:,:,:)::Variable

            character(100) :: DataType , ComponentName,Form , Complement
            integer::e,gp,i,j,nComponents

            integer,parameter :: Scalar=1 , Vector=2 , Tensor=3

            FileNumber = this%FileNumber

            nComponents = size(Variable,3)

            if     ((VariableType==Scalar).and.(nComponents==1)) then
                DataType='Scalar'
                ComponentName='"'//trim(Name)//'"'
                Form = '1X,E16.9'
                Complement=''

            elseif ((VariableType==Vector).and.(nComponents==2)) then
                DataType='Vector'
                ComponentName='"x","y"'
                Form = '2(1X,E16.9)'
                Complement=''

            elseif ((VariableType==Vector).and.(nComponents==3)) then
                DataType='Vector'
                ComponentName='"x", "y", "z"'
                Form = '3(1X,E16.9)'
                Complement=''

            elseif ((VariableType==Tensor).and.(nComponents==3)) then
                DataType='PlainDeformationMatrix'
                ComponentName='"11","22","12","33"'
                Form = '3(1X,E16.9)'
                Complement=' 0.0'

            elseif ((VariableType==Tensor).and.(nComponents==4)) then
                DataType='PlainDeformationMatrix'
                ComponentName='"11","22","12","33"'
                Form = '4(1X,E16.9)'
                Complement=''

            elseif ((VariableType==Tensor).and.(nComponents==6)) then
                DataType='Matrix'
                ComponentName='"11","22","33","12","23","13"'
                Form = '6(1X,E16.9)'
                Complement=''

            else
                stop "Error on ExportOnGaussPoints"

            endif

            write(FileNumber,'(a,E16.9,a)') 'Result "'//trim(Name)//'" "Analysis" ',Time, ' '//trim(DataType)//' OnGaussPoints "'//trim(this%GaussPointName)//'"'
            Write(FileNumber,*) 'ComponentNames '//trim(ComponentName)
            write(FileNumber,*) 'Values'
            do e=1,size(Variable,1)
                Write(FileNumber,'(I8,'//trim(Form)//',A)') e ,  (Variable(e,1,j) , j=1,nComponents) , trim(Complement)
                do gp=2,size(Variable,2)
                    Write(FileNumber,'('//trim(Form)//',A)') (Variable(e,gp,j) , j=1,nComponents) , trim(Complement)
                enddo
            enddo

            write(FileNumber,*) 'End Values'

        end subroutine


end module



