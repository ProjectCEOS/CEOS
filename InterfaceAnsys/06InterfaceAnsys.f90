subroutine InterfaceAnsys(                                        &
                        matId, elemId,kDomIntPt, kLayer, kSectPt, &
                        ldstep,isubst,keycut,                     &
                        nDirect,nShear,ncomp,nStatev,nProp,       &
                        Time,dTime,Temp,dTemp,                    &
                        stress,ustatev,dsdePl,sedEl,sedPl,epseq,  &
                        Strain,dStrain, epsPl, prop, coords,      &
                        var0, defGrad_t, defGrad,                 &
                        tsstif, epsZZ,                            &
                        var1, var2, var3, var4, var5,             &
                        var6, var7, var8)

!################## MODULO DO MODELO #################
    use ElasticViscoPlastic
!#####################################################

    implicit none

    INTEGER    ::          matId, elemId,                             &
                          kDomIntPt, kLayer, kSectPt,                 &
                          ldstep,isubst,keycut,                       &
                          nDirect,nShear,ncomp,nStatev,nProp

    DOUBLE PRECISION ::    Time,    dTime,   Temp,    dTemp,          &
                          sedEl,   sedPl,   epseq,   epsZZ

    DOUBLE PRECISION ::    stress  (ncomp  ), ustatev (nStatev),      &
                          dsdePl  (ncomp,ncomp),                      &
                          Strain  (ncomp  ), dStrain (ncomp  ),       &
                          epsPl   (ncomp  ), prop    (nProp  ),       &
                          coords  (3),                                &
                          defGrad (3,3),     defGrad_t(3,3),          &
                          tsstif  (2)
!######################## DEFINICAO DO MODELO E PROPRIEDADE ###########
    type(ClassElasticViscoPlastic_3D) :: modelo
    type(ElasticViscoPlasticProperties) , target :: Propriedades
!######################################################################
    REAL(8) , DIMENSION( NCOMP ) , TARGET :: STRESSOBJ

    !Como são ponteiros, precisamos definir. No código original
    !eles passam por um contrutor. Nesta interface não existe construtor
    !e por isto eles devem ser inicializados.
    modelo%Properties => Propriedades
    modelo%Stress => STRESSOBJ


    call modelo%LoadDataFromAnsys(Time,dTime,Temp,dTemp,defgrad , defGrad_t )

    call modelo%LoadProperties(prop)
    call modelo%LoadInternalVariablesFromVector(ustatev)

    call modelo%UpdateStressAndStateVariables()
    call modelo%GetTangentModulus(D)

    call modelo%ExportInternalVariablesToVector(ustatev)

end subroutine

