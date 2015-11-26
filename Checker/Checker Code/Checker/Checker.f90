program Checker

    use StringLib
    use Parser
    
    implicit none

    integer :: i, status, FileNumber
    character(len=255)::CommandLine, File1, File2
    character(len=255) , allocatable , dimension(:) :: Commands, aux
    type(ClassParser) :: Comp
    logical :: FoundFile1, FoundFile2
    real(8) ::  tol, a1,a2,b1,b2
    
    call comp%setup
    
    call get_command(CommandLine,status=status)

    call SplitString(CommandLine,Commands , "/")
    if (size(Commands)<=1) then
        stop "ERROR in Checker :: CommandLine not consistent"
    endif
 
    
    
    tol = 1.0d-8
    
    do i=2,size(Commands)

        call SplitString( Commands(i) , aux , " ")

        if (size(aux).ne.2) then
            write(*,*) 'ERROR in Checker :: Invalid Argument: '//trim(commands(i))
            stop
        endif

        if (comp%CompareStrings(aux(1),"File1")) then
            File1 = trim( aux(2) )
            FoundFile1 = .true.
            cycle
        elseif (comp%CompareStrings(aux(1),"File2")) then
            File2 = trim( aux(2) )
            FoundFile2 = .true.
        elseif (comp%CompareStrings(aux(1),"Tol")) then
            call comp%converttodouble( trim(aux(2)), tol)
        else
            write(*,*) "ERROR in Checker :: Option not identified: ["//trim(aux(1))//"]."
            stop
        endif
    enddo

    if (.not.FoundFile1) then
        stop "ERROR :: File 1 missing"
    elseif (.not.FoundFile2) then
        stop "ERROR :: File 2 missing"
    endif

    
   open(1, file=File1, status='old')
   open(2, file=File2, status='old')
  
   
    write(*,*) "******************************************************* "   
    write(*,*) " Comparing File: ",trim(File1)," with File: ",trim(File2)   
   
   do while ( .not. EOF(1) )
       
        read(1,*) a1, b1

        read(2,*) a2, b2
        
        if ( dabs(a1-a2) .gt. tol ) then
            write(*,*) " "
            write(*,*) "Absolute difference in first column greater than the tolerance." 
            write(*,*) "Tolerance = ", tol
            write(*,*) "File 1 :: point = ", a1
            write(*,*) "File 2 :: point = ", a2
            write(*,*) "******************************************************* " 
            stop         
        endif
 
        if ( dabs(b1-b2) .gt. tol ) then
            write(*,*) " "
            write(*,*) "Absolute difference in second column greater than the tolerance"
            write(*,*) "Tolerance = ", tol
            write(*,*) "File 1 :: point = ", b1
            write(*,*) "File 2 :: point = ", b2
            write(*,*) "******************************************************* " 

            stop         
        endif
        
   end do

   write(*,*) " "
   write(*,*) "Success!" 
   write(*,*) "******************************************************* " 

end program Checker

