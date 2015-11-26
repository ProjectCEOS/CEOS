module StringLib
    public

    private:: Error

    type myString
        character*255::String=''
        integer::Length=0
    end type

    contains
!############################################################################################################
    subroutine Error(MSG)
        character(len=*)::MSG
        write(*,*) MSG
        pause
        stop
    end subroutine
!############################################################################################################
    function str2dble(string) result(num)
        character(len=*)::string
        real(8)::num
        read(string,*) num
    end function
!############################################################################################################
    function IsEmpty(String) result(answer)
        character(len=*)::String
        logical::answer
        integer::l,k
        character::C*1
        answer=.true.
        do l=1,len(String)
            C=string(l:l)
            k=ichar(string(l:l))
            if ((k.ne.0).and.(k.ne.32)) then
                answer=.false.
                return
            endif
        enddo

        !l=len(trim(String))
        !answer=(l==0)
    end function
!############################################################################################################
    subroutine SplitSub(String,Delim,res)
        implicit none
        integer::i,er
        character(len=*)::String  , Delim
        character(len=*),dimension(:),pointer::res
        type(myString),pointer,dimension(:)::Subs
        logical :: status

        Subs=>Split(String,Delim)

        if (.not.associated(subs)) then
            res=>null()
            return
        endif


        status = ASSOCIATED(res)
        if ( status == .true. ) then
            deallocate(res,STAT=er)
            !write(*,*)'erro',er
            !pause
        endif


        allocate(res(size(subs)))




        do i=1,size(subs)
            res(i)=trim(subs(i)%String)
        enddo
        deallocate(subs)
    end subroutine
!############################################################################################################
    function Split(String,Delim) result(Subs)
        implicit none
        character(len=*)::String
        character(len=*)::Delim
        type(myString),pointer,dimension(:)::Subs

        logical::CanCount
        logical,allocatable,dimension(:)::MASK
        integer::p0,pf,k,Ld,Ls,s
        character(len=len(string))::aux

        Subs=>null()

        Ld=len(Delim)
        Ls=len(string)

        allocate(MASK(ls))
        MASK=.true.

!primeiro cria uma mascara para ver quais caracteres que sao validos
loopMask:do p0=1,Ls
            pf=p0+Ld-1
            if (pf>ls) exit loopMask
            if (String(p0:pf)==Delim) Mask(p0:pf)=.false.
        enddo LoopMask

!depois contar quantas substrings tem
        k=0
        CanCount=.true.
        do p0=1,ls
            if (Mask(p0)) then
                if (CanCount) k=k+1
                CanCount=.false.
            else
                CanCount=.true.
            endif
        enddo

        if (k==0) return

        allocate(Subs(k))

!agora separar as string em arrays
        k=0
        p0=0
loopScan:do while (.true.)
            p0=p0+1
            if (p0>ls) exit loopScan
            IF (MASK(p0)) then
                k=k+1 !entao estamos em uma substring valida
                aux=''
                !vamos comecar a depositar os valores
                s=1
loopDump:       do while (MASK(p0))
                    aux(s:s)=String(p0:p0)
                    s=s+1;p0=p0+1
                    if (p0>ls) exit loopDump
                enddo LoopDump
                subs(k)%String=aux
                subs(k)%Length=s-1
            endif
        enddo loopScan


    end function
!############################################################################################################
    !coloquei o opcional para retornar o lugar que encontrou e para especificar o lugar de comeco da procura
    function HaveSub(String,Find,iStart,iFound) result(answer)
        implicit none
        character(len=*)::String,Find
        integer::ls,lf,p0,pf
        integer,optional::iFound,iStart
        logical::answer
        integer::Start
        answer=.false.
        if (present(iFound)) iFound=-1
        if (present(iStart)) Start = max(iStart,1)
        ls=len(String);lf=len(find)
        if (lf>ls) return
        do p0=Start,ls
            pf=p0+lf-1
            if (pf>ls) return
            if (string(p0:pf)==find) then
                if (present(iFound)) iFound=p0
                answer=.true.
                return
            endif
        enddo
    end function
!############################################################################################################
    function Replace(String,From,To) result(NewString)
        implicit none
        character(len=*)::String,From,to
        character(len=len(string))::NewString

        integer::Ls,Lf,Lt , p0,pf,k
        integer::minL
        type(myString),pointer,dimension(:)::Subs
        logical::StartFrom,EndFrom

        NewString=''
        Ls=len(String);Lf=Len(From);Lt=Len(to)
        if (any([ls,lf,lt]==0)) call error('Replace:: String Length cannot be zero')


        if (Ls<Lf) then     !if the string size is smaller than the string we are searching for, we will never find it, and can leave the function
            NewString=String
            return
        elseif (Ls==Lf) then !if their size are equal, we must check whether the strings are equal
            IF (String==From) then
                !ok.. they are equal .. so we must replace by TO... but TO may be larger than STRING
                minL=min(Ls,Lt)
                NewString(1:minL)=To(1:minL)
                return
            else  !the strings are not equal... so we are not replacing anything
                NewString=String
                return
            endif
        endif

        !the only possibility here is that LF < LT
        !lets split the string into substrings
        subs=>Split(String,Delim=From)
        if (.not.associated(subs)) call error('Replace::Subs not associated')
        !lets check if the string starts with the FROM string
        StartFrom=string(1:lf)==From
        !check if the string ends with the FROM string
        EndFrom = string(ls-lf+1:ls)==From

        !now lets assemble the new string
        p0=0
        If (StartFrom) then
            p0=p0+1
            if (p0>ls) goto 999
            pf=p0+lt-1
            pf=min(pf,ls)
            NewString(p0:pf)=to(1:pf-p0+1)
            p0=pf
        endif
LoopSubs: do k=1,size(subs)

            p0=p0+1
            if (p0>ls) goto 999
            pf=p0+subs(k)%Length-1
            pf=min(pf,ls)
            NewString(p0:pf)=subs(k)%String(1:pf-p0+1)
            p0=pf

            if (k==size(subs)) exit LoopSubs

            p0=p0+1
            if (p0>ls) goto 999
            pf=p0+lt-1
            pf=min(pf,ls)
            NewString(p0:pf)=to(1:pf-p0+1)
            p0=pf
        enddo loopsubs
        IF (EndFrom) then
            p0=p0+1
            if (p0>ls) goto 999
            pf=p0+lt-1
            pf=min(pf,ls)
            NewString(p0:pf)=to(1:pf-p0+1)
            p0=pf
        endif

999 if (associated(subs)) deallocate(subs)
end function

function lcase(string) result(saida)
		character*(*) :: string
		character*(len(string)) :: saida

		saida=string
		do i=1,len(string)
			if ( ( saida(i:i) >= 'A' ) .and. ( saida(i:i) <= 'Z' )) then
				saida(i:i) = CHAR( ICHAR(saida(i:i)) + 32 )
			endif
		enddo
end function

!==========================================================================================
    subroutine SplitString( String , SubStrings, Delimiter )

        !use StringLib

        character(len=*) :: String
        character(len=*) , allocatable , dimension(:) :: SubStrings
        character(len=*) :: Delimiter

        character(len=len(String)) , pointer , dimension(:) :: SubStringsPointer
        integer::i


        if (allocated(SubStrings)) deallocate(SubStrings)

        call SplitSub(String,Delimiter, SubStringsPointer)

        if (.not.associated(SubStringsPointer)) then
            return
        endif

        allocate(SubStrings(size(SubStringsPointer)))

        do i=1,size(SubStrings)
            SubStrings(i) = SubStringsPointer(i)
        enddo

        deallocate(SubStringsPointer)
    end subroutine
!==========================================================================================

end module