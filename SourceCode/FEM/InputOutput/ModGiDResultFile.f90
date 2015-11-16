module GiDResultFile

    type ClassGiDResultFile
        integer :: FileNumber=-1 , ElementType=-1
        character(len=10) :: GaussPointName=''
        character(len=100) :: DatFile='' , ResultFile=''

        contains
            procedure :: WriteResultFileHeader
            procedure :: ExportOnGaussPoints
            procedure :: ExportOnNodes
            procedure :: Close => CloseGidFile
    end type

    contains

        subroutine CloseGidFile(this)

            class(ClassGiDResultFile) :: this

            close (this%FileNumber)

        end subroutine

        subroutine WriteResultFileHeader(this)

            use ElementLibrary
            implicit none
            class(ClassGiDResultFile) :: this
            integer::FileNumber , ElementType
            character(len=10) :: GaussPointName

            FileNumber = 1
            this%FileNumber = 1

            !arquivo de resultados do GiD -----------------------------------------
            this%ResultFile  = this%DatFile(1:len(trim(this%DatFile))-4) // '.post.res'

            open (FileNumber,file=this%ResultFile,status='unknown')

            write(FileNumber,*) 'GiD Post Results File 1.0'

            select case (this%ElementType)

                case (ElementTypes%Tri3)
                    write(FileNumber,*) 'GaussPoints "Tri3" ElemType Triangle'
                    write(FileNumber,*) 'Number Of Gauss Points: 1'
                    write(FileNumber,*) 'Natural Coordinates: internal'
                    write(FileNumber,*) 'end gausspoints'
                    GaussPointName = "Tri3"

                case (ElementTypes%Quad4)
                    write(FileNumber,*) 'GaussPoints "Quad4" ElemType Quadrilateral'
                    write(FileNumber,*) 'Number Of Gauss Points: 4'
                    write(FileNumber,*) 'Natural Coordinates: internal'
                    write(FileNumber,*) 'end gausspoints'
                    GaussPointName="Quad4"

                case (ElementTypes%Hexa8)
                    write(FileNumber,*) 'GaussPoints "Hexa8" ElemType Hexahedra'
                    write(FileNumber,*) 'Number Of Gauss Points: 8'
                    write(FileNumber,*) 'Natural Coordinates: internal'
                    write(FileNumber,*) 'end gausspoints'
                    GaussPointName="Hexa8"

                case (ElementTypes%Tetra4)
                    write(FileNumber,*) 'GaussPoints "Tetra4" ElemType Tetrahedra'
                    write(FileNumber,*) 'Number Of Gauss Points: 1'
                    write(FileNumber,*) 'Natural Coordinates: internal'
                    write(FileNumber,*) 'end gausspoints'
                    GaussPointName="Tetra4"

            case default
                write(*,*) "WriteResultFileHeader :: Element not identified"
                stop

            end select

            this%GaussPointName = GaussPointName

        end subroutine

        ! Export Results on GaussPoints - GiD
        !----------------------------------------------------------------------------------------
        subroutine ExportOnGaussPoints( this ,  Name , Time , Variable , VariableType )

            implicit none
            class(ClassGiDResultFile) :: this
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
        !----------------------------------------------------------------------------------------

        ! Export Results on Nodes - GiD
        !----------------------------------------------------------------------------------------
        subroutine ExportOnNodes( this , Name , Time , Variable , VariableType)

            implicit none
            class(ClassGiDResultFile) :: this
            integer::FileNumber , VariableType
            Real(8)::Time
            character(*)::Name
            real(8),dimension(:,:)::Variable

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



end module





