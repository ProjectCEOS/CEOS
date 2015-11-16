subroutine UpdateMeshCoordinates(GlobalNodesList,AnalysisSettings,U)

     use Nodes
     use Analysis
     implicit none
     type (ClassNodes)    , pointer , dimension(:) :: GlobalNodesList
     real(8) , dimension(:)                        :: U
     type(ClassAnalysis)                           :: AnalysisSettings

     integer::node,dof

     select case (AnalysisSettings%ProblemType)
        case (ProblemTypes%Mechanical)

            do node=1,size(GlobalNodesList)
                do dof=1,AnalysisSettings%NDOFnode
                    GlobalNodesList(node)%Coord(dof) = GlobalNodesList(node)%CoordX(dof) + U( AnalysisSettings%NDOFnode * (node-1) + dof )
                enddo
            enddo

        case default
            call Error("UpdateMeshCoordinates")
    end select

end subroutine

