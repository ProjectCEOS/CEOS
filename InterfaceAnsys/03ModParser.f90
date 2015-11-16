!##################################################################################################
! PARSER
!--------------------------------------------------------------------------------------------------
! Date: 2015/01
!
! Authors:  Jan-Michel Farias
!           Thiago Andre Carniel
!           Paulo Bastos de Castro
!!------------------------------------------------------------------------------------------------
! Remarks:

!##################################################################################################
module Parser

    implicit none
    private

    public :: GetNextString , GetCurrentString,  GetNextOption , GetCurrentOption
    public :: ShowError , RaiseError , ResetError , AdvanceTo , Setup , FormatString
    public :: CompareStrings , EOF , Warning , CheckError , ConvertToDouble , ConvertToInteger
    public :: GetOriginalLine , CloseFile , EndParser , FillListOfOptions


    character(len=1) , parameter  ,private :: space=' '


    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! ClassParser:
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , public :: ClassParser

        private

        ! Class Attributes
		!----------------------------------------------------------------------------------------
        integer          :: FileNumber = -1 , CurrentLineNumber = 0
        character(len=1) :: Comment = '!' , OptionDelim='='
        logical :: EndOfFile = .false.
        character(len=255):: CurrentLine='' , Line='' , FileName=''
        integer , pointer , dimension(:) :: NullCharacters=>null()

        logical , public :: Error=.false.
        integer :: ErrorID = 0
        character(len=255),public:: ErrorDesc=''


        contains

            ! Class Methods
            !----------------------------------------------------------------------------------
            ! PRIVATE
            procedure :: IncrementLine
            procedure :: ReadNextLine
            procedure :: RemoveComments
            procedure :: RemoveNullCharacters
            procedure :: isNullCharacter
            procedure :: isEmptyString
            procedure :: SetError
            !----------------------------------------------------------------------------------
            !PUBLIC
            procedure :: GetCurrentOption
            procedure :: GetNextOption
            procedure :: GetNextString
            procedure :: GetCurrentString
            procedure :: RaiseError
            procedure :: ResetError
            procedure :: ShowError
            procedure :: AdvanceTo
            procedure :: Setup
            procedure :: FormatString
            procedure :: CompareStrings
            procedure :: Warning
            procedure :: CheckError
            procedure :: ConvertToDouble
            procedure :: ConvertToInteger
            procedure :: GetOriginalLine
            procedure :: CloseFile
            procedure :: EndParser
            procedure :: FillListOfOptions_Complete
            procedure :: FillListOfOptions_Reduced
            generic   :: FillListOfOptions => FillListOfOptions_Complete , FillListOfOptions_Reduced
            procedure :: Reset
    end type
    !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

    Interface FillListOfOptions
        procedure  FillListOfOptions_Complete
        procedure  FillListOfOptions_Reduced
    end interface

    contains
!==========================================================================================
        subroutine Reset(this)
            class(ClassParser)::this
            this%FileNumber = -1
            this%CurrentLineNumber = 0
            this%Comment = '!'
            this% OptionDelim='='
            this% EndOfFile = .false.
            this%CurrentLine='' ; this% Line='' ;this% FileName=''
            if (associated(this%NullCharacters)) deallocate(this%NullCharacters)
             this%NullCharacters=>null()
             call this%ResetError()
        end subroutine

        function FileExists(FileName) result(answer)
            character(len=*)::FileName
            logical::answer
            inquire(file=FileName,exist=answer)
        end function
!==========================================================================================
        function FreeFile(From,To) result(i)
            integer,optional::From,To
            integer::i , iFrom,iTo
            logical::aberto
            if (present(From).and.present(To)) then
                iFrom=min(From,To)
                iTo = max(From,To)
            elseif (present(From).or.present(To)) then
                write(*,*) "FreeFile::Must specify range [from,to]"
                stop
            else
                iFrom=1
                iTo = 1000
            endif

            do i=iFrom,iTo
                inquire(i,opened=aberto)
                if (.not.aberto) return
            enddo

            i=-1 !não encontrou nenhuma possibilidade
            !retorna um número negativo para poder testar no programa principal

        end function
!==========================================================================================
        function UnitUsed(i) result(answer)
            integer::i
            logical::answer
            inquire(i,opened=answer)
        end function
!==========================================================================================
        subroutine Setup(this,FileName, FileNumber ,NullCharacters,CommentSymbol,OptionDelim)

            class(ClassParser)::this
            character(len=*),optional::FileName
            integer , optional :: FileNumber
            integer,dimension(:),optional::NullCharacters
            character(len=1),optional::CommentSymbol,OptionDelim

            integer::status
            logical::FileNamePresent,FileNumberPresent

            call this%Reset()

            if (present(FileName)) then
                FileNamePresent=.true.
                this%FileName = FileName
            else
                FileNamePresent=.false.
            endif
            if (present(FileNumber)) then
                FileNumberPresent=.true.
                this%FileNumber = FileNumber
            else
                FileNumberPresent=.false.
            endif

            call this%ResetError

            if (FileNamePresent.and.FileNumberPresent) then
                if (.not.FileExists(FileName)) goto 996
                if (UnitUsed(FileNumber))      goto 997
                open(this%FileNumber,file=this%FileName,status='old',ERR=999,iostat=status)

            elseif (FileNamePresent) then
                this%FileNumber=FreeFile()
                if (this%FileNumber<0) goto 998
                if (.not.FileExists(FileName)) goto 996
                open(this%FileNumber,file=this%FileName,status='old',ERR=999,iostat=status)

            elseif (FileNumberPresent) then
                if (.not.UnitUsed(FileNumber)) goto 994
            else
                goto 995
            endif

            if (present(CommentSymbol)) then
                this%Comment=CommentSymbol
            else
                this%Comment='!'
            endif

            if (present(OptionDelim)) then
                this%OptionDelim=OptionDelim
            else
                this%OptionDelim='='
            endif

            if (present(NullCharacters)) then
                if (associated(this%nullcharacters)) deallocate(this%nullcharacters)
                allocate(this%NullCharacters(size(NullCharacters)))
                this%NullCharacters = NullCharacters
            else
                allocate(this%NullCharacters(3))
                this%NullCharacters=[0,9,32]
            end if



            return

        !ERROR HANDLER
        994 call this%RaiseError("Setup::FileNumber not opened.")
        995 call this%RaiseError("Setup::Must inform FileName,FileNumber, or both")
        996 call this%RaiseError("Setup::File does not exist.")
        997 call this%RaiseError("Setup::FileNumber already opened.")
        998 call this%RaiseError("Setup:: a Free FileNumber could not be found.")
        999 call this%RaiseError("Setup:: Error opening the file. Check ErrorID.",status)
        !999 call this%SetError("Setup:: Error opening the file. Check ErrorID.",status)
        !    call this%ShowError
        !    stop
        end subroutine
!==========================================================================================
        subroutine Warning(this,msg,ShowLine)
            class(ClassParser)::this
            character(len=*)::msg
            logical,optional::ShowLine
            logical::show
            if (present(ShowLine)) then
                show=showline
            else
                show=.false.
            endif
            write(*,*) '#### WARNING ####'
            write(*,*) trim(msg)
            write(*,*) 'LineNumber:',this%CurrentLineNumber
            if (show) write(*,*) trim(this%CurrentLine)
        end subroutine
!==========================================================================================
        subroutine CloseFile(this)
            class(ClassParser)::this
            close(this%FileNumber)
        end subroutine
        subroutine EndParser(this)
            class(classparser)::this
            if (associated(this%nullcharacters)) deallocate(this%nullcharacters)
        end subroutine
!==========================================================================================
        function EOF(this) result(answer)
            class(ClassParser)::this
            logical::answer
            answer=this%EndOfFile
        end function
!==========================================================================================
        subroutine IncrementLine(this)
            Class(ClassParser) :: this
            this%CurrentLineNumber = this%CurrentLineNumber + 1
        end subroutine
 !==========================================================================================
        subroutine SetError(this,ErrorDesc,ErrorID)
            class(ClassParser)::this
            character(len=*),optional::ErrorDesc
            integer,optional::ErrorID
            this%Error=.true.
            if (present(ErrorDesc)) then
                this%ErrorDesc = ErrorDesc
            else
                this%ErrorDesc=''
            endif
            if (present(ErrorID)) then
                this%ErrorID=ErrorID
            else
                this%ErrorID=-9999
            endif
        end subroutine
 !==========================================================================================
        subroutine ResetError(this)
            class(ClassParser)::this
            this%Error=.false.
            this%ErrorDesc=''
            this%ErrorID=0
        end subroutine
 !==========================================================================================
        subroutine ShowError(this)
            class(ClassParser)::this
            write(*,*) '#### Parser Module ####'
            write(*,*) '#### ShowError     ####'
            write(*,*) 'Error: ' , this%Error
            write(*,*) 'Line Number: ' , this%CurrentLineNumber
            write(*,*) 'Line: ' , trim(this%CurrentLine)
            write(*,*) 'Error ID: ' , this%ErrorID
            write(*,*) 'Desc: ' , trim(this%ErrorDesc)
        end subroutine
 !==========================================================================================
        subroutine GetCurrentOption(this,OptionName,OptionValue)

            use StringLib
            class(ClassParser)::this
            character(len=*)::OptionName,OptionValue
            character(len=len(OptionName)), dimension(:), pointer :: SubStrings

            call this%ResetError
            OptionName='' ; OptionValue=''

            SubStrings => null()
            call SplitSub(this%Line,this%OptionDelim,SubStrings)


            if (size(SubStrings).ne.2) then
                call this%SetError('GetOption:: Number of substrings not equal to 2 when using '//this%OptionDelim//' as a delimiter')
                return
            else
                OptionName=SubStrings(1)
                OptionValue=SubStrings(2)
                deallocate(SubStrings)
            endif


        end subroutine
!==========================================================================================
        subroutine GetNextOption(this,OptionName,OptionValue)
            class(ClassParser)::this
            character(len=*)::OptionName,OptionValue
            call this%ResetError
            call this%GetNextString
            if (this%Error) return
            call this%GetCurrentOption(OptionName,OptionValue)
        end subroutine
 !==========================================================================================
        subroutine ReadNextLine(this)

            class(ClassParser)::this
            integer::status
            character (len=255) :: string

            this%EndOfFile=.false.
            call this%ResetError
            call this%IncrementLine


            read(this%FileNumber,'(A255)',IOSTAT=status,err=998,end=999) string !this%CurrentLine
            this%CurrentLine = string

            return
        !**********************
        !ERROR HANDLER
        998 call this%SetError("ReadNextLine::An Error Occured. Check ErrorID.",status)
            return
        999 this%EndOfFile=.true.
            return

        end subroutine
 !==========================================================================================
        function RemoveComments(this,string) result(newstring)
            class(ClassParser)::this
            character(len=*)::string
            character(len=len(string))::newstring
            integer :: StringLength , i , j
            newstring=string
            call this%ResetError
            StringLength=len(string)
            loop: do i=1,StringLength
                    if (string(i:i)==this%Comment) then
                        do j=i,StringLength
                            newstring(j:j)=space
                        enddo
                        exit loop
                    endif
                enddo loop
        end function
 !==========================================================================================
        function FormatString(this,string) result(newstring)
            use stringlib
            class(ClassParser)::this
            character(len=*)::string
            character(len=len(string))::newstring
            newstring=lcase(string)
            newstring=this%RemoveComments(newstring)
            newstring=this%RemoveNullCharacters(newstring)
        end function
!==========================================================================================
        function CompareStrings(this,A,B) result(answer)
            class(ClassParser)::this
            character(len=*)::A,B
            logical::answer
            answer = (this%FormatString(A)==this%FormatString(B))
        end function
 !==========================================================================================
        function RemoveNullCharacters(this,string) result(newstring)
            class(ClassParser)::this
            character(len=*)::string
            character(len=len(string))::Temp,newstring
            integer::StringLength,i,j,n,iTemp
            StringLength=len(string)
            Temp=''
            iTemp=0
            LoopString: do i=1,StringLength
                    if (this%isNullCharacter(string(i:i))) then
                        cycle LoopString
                    endif

                    iTemp=iTemp+1
                    Temp(itemp:itemp) = string(i:i)
            enddo LoopString
            newstring = ''
            newstring = Temp

        end function
 !==========================================================================================
        function isEmptyString(this,string) result(answer)
            class(ClassParser)::this
            character(len=*)::string
            logical::answer
            INTEGER::i
            answer=.true.
            do i=1,len((string))
                if (.not.this%isNullCharacter(string(i:i))) then
                    answer=.false.
                    return
                endif
            enddo

        end function
!==========================================================================================
        function isNullCharacter(this,c) result(answer)
            class(ClassParser)::this
            character(len=1)::c
            logical::answer
            integer::n
            answer=.false.
            do n = 1,size(this%NullCharacters)
                if (ichar(c)==this%NullCharacters(n)) then
                    answer=.true.
                    return
                endif
            enddo
        end function
!==========================================================================================
        subroutine GetNextString(this,string)
            class(ClassParser)::this
            character(len=*),optional::string

            logical::ValidLine
            character(len=255)::OptionName,OptionValue
            if (present(string)) string=''
            ValidLine=.false.
            call this%ResetError


            do while (.not.ValidLine)

                call this%ReadNextLine

                this%Line = this%FormatString(this%CurrentLine)

                if (this%Error) then
                    call this%ShowError
                    stop
                elseif (EOF(this)) then
                    return
                endif

                if (trim(this%line)=='pause') then
                    pause
                    ValidLine=.false.
                else

                    call this%GetCurrentOption(OptionName,OptionValue)


                    if (this%Error) then
                       !se deu erro é pq não eh nenhum comando interno precisamos verificar o que é
                        call this%ResetError
                        ValidLine = .not. this%isEmptyString(this%Line)

                    else
                        !se não deu erro precisamos verificar qual a opção que aparece
                        if ((trim(OptionName))=='disp') then
                            !encontramos o comando disp... vamos escrever na tela
                            write(*,*) trim(this%CurrentLine(6:))
                            ValidLine=.false.
                        else
                            !se não encontramos este comando... deve ser alguma coisa interna do script...
                            !vamos continuar como se nada tivesse acontecido
                            ValidLine = .not. this%isEmptyString(this%Line)
                        endif
                    endif
                endif
            end do
            if (present(string)) string = this%line
        end subroutine
!==========================================================================================
        subroutine GetOriginalLine(this,string)
            class(ClassParser)::this
            character(len=*)::string
            string=this%CurrentLine
        end subroutine
!==========================================================================================
        subroutine GetCurrentString(this,string)
            class(ClassParser)::this
            logical::ValidLine
            character(len=*)::string
            call this%ResetError
            string=''
            string = this%line
        end subroutine
!==========================================================================================
        subroutine RaiseError(this,msg,id)
            class(ClassParser)::this
            integer,optional::id
            character(len=*)::msg
            call this%SetError(msg,id)
            call this%ShowError
            stop
        end subroutine
!==========================================================================================
        subroutine CheckError(this)
            class(ClassParser)::this
            if (this%Error) then
                call this%ShowError
                stop
            endif
        end subroutine
!==========================================================================================
        subroutine AdvanceTo(this,string)
            class(ClassParser)::this
            character(len=*)::string
            write(*,*) 'advanceto::nao ta pronto'
            stop
        end subroutine
!==========================================================================================
        subroutine  ConvertToDouble(this,string,n)
            class(ClassParser)::this
            character(len=*)::string
            real(8)::n
            call this%ResetError
            read(string,*,err=999) n
            return
            !999 call this%SetError("ConvertToDouble::Could not convert ["//trim(string)//"] to double")
            999 call this%RaiseError("ConvertToDouble::Could not convert ["//trim(string)//"] to double")
        end subroutine
!==========================================================================================
        subroutine ConvertToInteger(this,string,n)
            class(ClassParser)::this
            character(len=*)::string
            integer::n
            call this%ResetError
            read(string,*,err=999) n
            return
            !999 call this%SetError("ConvertToInteger::Could not convert ["//trim(string)//"] to integer")
            999 call this%RaiseError("ConvertToInteger::Could not convert ["//trim(string)//"] to integer")
        end subroutine
!==========================================================================================
    subroutine FillListOfOptions_Complete(this,ListOfOptions,ListOfValues,Found)

        class(ClassParser)::this
        character(len=*),dimension(:)::ListOfOptions,ListOfValues
        logical,dimension(:)::Found

        character(len=255)::string
        character(len=len(ListOfOptions)) :: OptionName
        character(len=len(ListOfValues))  :: OptionValue

        integer::nLines,nOptions,option,i

        if ((Size(ListOfOptions).ne.size(ListOfValues)).or.(size(ListOfOptions).ne.size(Found))) then
            call This%SetError("FillListOfOptions:: ListOfOptions,ListOfValues,and Found array must have the same size")
            return
        endif


        nlines = size(ListOfOptions)
        nOptions = nlines
        Found=.false.
        ListOfValues=''

        do i=1,nLines
            call This%GetNextOption(OptionName,OptionValue)


            if (this%Error) return
            do option=1,nOptions
                if (this%CompareStrings(OptionName,ListOfOptions(option))) then
                    ListOfValues(option)=OptionValue
                    Found(option)=.true.
                endif
            enddo
        enddo

    end subroutine
!==========================================================================================
    subroutine FillListOfOptions_Reduced(this,ListOfOptions,ListOfValues)

        class(ClassParser)::this
        character(len=*),dimension(:)::ListOfOptions,ListOfValues

        logical,allocatable,dimension(:)::Found
        character(len=255)::string
        character(len=len(ListOfOptions)) :: OptionName
        character(len=len(ListOfValues))  :: OptionValue

        integer::nLines,nOptions,option,i

        if ((Size(ListOfOptions).ne.size(ListOfValues)).or.(size(ListOfOptions))) then
            call This%SetError("FillListOfOptions:: ListOfOptions,ListOfValues must have the same size")
            return
        endif

        allocate(Found(size(ListOfOptions)))

        call this%FillListOfOptions_Complete(ListOfOptions,ListOfValues,Found)
        call this%CheckError
        if (.not.all(Found)) then
            do i=1,size(Found)
                if (.not.Found(i)) then
                    write(*,*) "FillListOfOptions :: Option not found ["//trim(ListOfOptions(i))//"]"
                endif

            enddo
            stop
        endif

    end subroutine



end module

