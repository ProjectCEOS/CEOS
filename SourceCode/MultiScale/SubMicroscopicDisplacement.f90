subroutine MicroscopicDisplacement ( AnalysisSettings, GlobalNodesList, U, Umicro )

     use Nodes
     use Analysis

     implicit none

     type(ClassAnalysis)                           :: AnalysisSettings
     type (ClassNodes)    , pointer , dimension(:) :: GlobalNodesList
     real(8) , dimension(:)                        :: U, Umicro

     integer :: node,dof,i, GlobalDOF
     real(8) :: GradU(3,3), Y(3), Utilde(3), UmicroLocal(3)


    GradU  = 0.0d0
    Y      = 0.0d0
    Utilde = 0.0d0


     select case (AnalysisSettings%ProblemType)
        case (ProblemTypes%Mechanical)

            GradU = AnalysisSettings%Fmacro
            GradU(1,1) = AnalysisSettings%Fmacro(1,1) - 1.0d0
            GradU(2,2) = AnalysisSettings%Fmacro(2,2) - 1.0d0
            GradU(3,3) = AnalysisSettings%Fmacro(3,3) - 1.0d0


            do node = 1,size(GlobalNodesList)


                do dof = 1,AnalysisSettings%NDOFnode

                    GlobalDOF = AnalysisSettings%NDOFnode * (node-1) + dof

                    Utilde(dof) = U(GlobalDOF)

                    Y(dof) = GlobalNodesList(node)%CoordX(dof)

                enddo


                UmicroLocal = AnalysisSettings%Umacro + matmul(GradU,Y) + Utilde


                do dof = 1,AnalysisSettings%NDOFnode

                    GlobalDOF = AnalysisSettings%NDOFnode * (node-1) + dof

                    Umicro(GlobalDOF) = UmicroLocal(dof)

                enddo


            enddo



        case default

            call Error("MicroscopicDisplacement")

    end select



end subroutine

