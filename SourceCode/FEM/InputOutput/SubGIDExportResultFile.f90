!##################################################################################################
! This
!--------------------------------------------------------------------------------------------------
! Date: 2014/02
!
! Authors:  Jan-Michel Farias
!           Thiago Andre Carniel
!           Paulo Bastos de Castro
!!------------------------------------------------------------------------------------------------
! Modifications:
! Date:         Author:
!##################################################################################################
subroutine ExportResultFile( Time , u, nDOFnode, TotalnNodes , ElementList , GiDFile)

    use Element
    use GiDResultFile


    implicit none

    type (ClassElementsWrapper) , dimension(:)                     :: ElementList
    type(ClassGiDResultFile)                                       :: GidFile

    integer :: nDOFnode, TotalnNodes
    real(8) , dimension(:) :: u
    real(8) , allocatable, dimension(:,:) :: NodalValues
    Real(8)::Time , Variable(6)
    integer :: i, j , e, gp ,  nelem , ngp , VariableID , NumberOfStateVariables , VariableLength , VariableType
    character(100)   :: VariableName
    real(8),dimension(:,:,:),allocatable::Values


    ! Export Results: Vector on Nodes
    !----------------------------------------

    allocate ( NodalValues(TotalnNodes, nDOFnode) )
    do i = 1 ,TotalnNodes
        do j = 1, nDOFnode
            NodalValues(i,j) = u( nDOFnode*(i -1) + j )
        enddo
    enddo

    VariableName = 'Displacement'
    VariableType = 2
    call GiDFile%ExportOnNodes ( VariableName , Time , NodalValues , VariableType)

    nelem = size( ElementList )
    ngp = size(ElementList(1)%el%GaussPoints)
    allocate( Values( nelem , ngp , 6 ) )

    VariableID=0 !ID para pegar o numero de propriedades
    call ElementList(1)%el%GaussPoints(1)%GetResult( VariableID , VariableName , NumberOfStateVariables , Variable , VariableType )
! TODO (Thiago#2#11/12/15): Melhorar a saida dos resultados para o GID, informando quais variávies deseja-se escrever, independente de quantas variáveis estão implementadas nas rotinas GetResult_"NomeModeloConstitutivo"


    do VariableID=1,NumberOfStateVariables

        do e=1,nelem
            do gp=1,ngp
                call ElementList(e)%el%GaussPoints(gp)%GetResult( VariableID , VariableName , VariableLength , Variable , VariableType )
                Values(e,gp,1:VariableLength)=Variable(1:VariableLength)
            enddo
        enddo

        call GiDFile%ExportOnGaussPoints( VariableName , Time , Values(:,:,1:VariableLength)  , VariableType )

    enddo

end subroutine
