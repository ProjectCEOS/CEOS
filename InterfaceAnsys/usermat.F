*deck,usermat      USERDISTRIB  parallel                                gal
      subroutine usermat(
     &                   matId, elemId,kDomIntPt, kLayer, kSectPt,
     &                   ldstep,isubst,keycut,
     &                   nDirect,nShear,ncomp,nStatev,nProp,
     &                   Time,dTime,Temp,dTemp,
     &                   stress,ustatev,dsdePl,sedEl,sedPl,epseq,
     &                   Strain,dStrain, epsPl, prop, coords,
     &                   var0, defGrad_t, defGrad,
     &                   tsstif, epsZZ,
     &                   var1, var2, var3, var4, var5,
     &                   var6, var7, var8)
c*************************************************************************
c     *** primary function ***
c
c           user defined material constitutive model
c
c      Attention:
c           User must define material constitutive law properly
c           according to the stress state such as 3D, plane strain
c           and axisymmetry, plane stress and 3D/1D beam.
c
c           a 3D material constitutive model can use for
c           plane strain and axisymmetry cases.
c
c           When using shell elements, a plane stress algorithm
c           must be use.
c
c                                             gal July, 1999
c
c       The following demonstrates a USERMAT subroutine for
c       a plasticity model, which is the same as TB, BISO,
c       for different stress states.
c       See "ANSYS user material subroutine USERMAT" for detailed
c       description of how to write a USERMAT routine.
c
c       This routine calls four routines,
c       usermat3d.F, usermatps.F usermatbm.F and usermat1d.F, w.r.t.
c       the corresponding stress states.
c       Each routine can be also a usermat routine for the specific
c       element.
c
c*************************************************************************
c
c     input arguments
c     ===============
c      matId     (int,sc,i)               material #
c      elemId    (int,sc,i)               element #
c      kDomIntPt (int,sc,i)               "k"th domain integration point
c      kLayer    (int,sc,i)               "k"th layer
c      kSectPt   (int,sc,i)               "k"th Section point
c      ldstep    (int,sc,i)               load step number
c      isubst    (int,sc,i)               substep number
c      nDirect   (int,sc,in)              # of direct components
c      nShear    (int,sc,in)              # of shear components
c      ncomp     (int,sc,in)              nDirect + nShear
c      nstatev   (int,sc,l)               Number of state variables
c      nProp     (int,sc,l)               Number of material ocnstants
c
c      Temp      (dp,sc,in)               temperature at beginning of
c                                         time increment
c      dTemp     (dp,sc,in)               temperature increment
c      Time      (dp,sc,in)               time at beginning of increment (t)
c      dTime     (dp,sc,in)               current time increment (dt)
c
c      Strain   (dp,ar(ncomp),i)          Strain at beginning of time increment
c      dStrain  (dp,ar(ncomp),i)          Strain increment
c      prop     (dp,ar(nprop),i)          Material constants defined by TB,USER
c      coords   (dp,ar(3),i)              current coordinates
c      defGrad_t(dp,ar(3,3),i)            Deformation gradient at time t
c      defGrad  (dp,ar(3,3),i)            Deformation gradient at time t+dt
c
c     input output arguments
c     ======================
c      stress   (dp,ar(nTesn),io)         stress
c      ustatev   (dp,ar(nstatev),io)      user state variables
c      sedEl    (dp,sc,io)                elastic work
c      sedPl    (dp,sc,io)                plastic work
c      epseq    (dp,sc,io)                equivalent plastic strain
c      tsstif   (dp,ar(2),io)             transverse shear stiffness
c                                         tsstif(1) - Gxz
c                                         tsstif(2) - Gyz
c                                         tsstif(1) is also used to calculate hourglass
c                                         stiffness, this value must be defined when low
c                                         order element, such as 181, 182, 185 with uniform
c                                         integration is used.
c      var?     (dp,sc,io)                not used, they are reserved arguments
c                                         for further development
c
c     output arguments
c     ================
c      keycut   (int,sc,io)               loading bisect/cut control
c                                         0 - no bisect/cut
c                                         1 - bisect/cut
c                                         (factor will be determined by ANSYS solution control)
c      dsdePl   (dp,ar(ncomp,ncomp),io)   material jacobian matrix
c      epsZZ    (dp,sc,o)                 strain epsZZ for plane stress,
c                                         define it when accounting for thickness change
c                                         in shell and plane stress states
c
c*************************************************************************
c
c      ncomp   6   for 3D  (nshear=3)
c      ncomp   4   for plane strain or axisymmetric (nShear = 1)
c      ncomp   3   for plane stress (nShear = 1)
c      ncomp   3   for 3d beam      (nShear = 2)
c      ncomp   1   for 1D (nShear = 0)
c
c      stresss and strains, plastic strain vectors
c          11, 22, 33, 12, 23, 13    for 3D
c          11, 22, 33, 12            for plane strain or axisymmetry
c          11, 22, 12                for plane stress
c          11, 13, 12                for 3d beam
c          11                        for 1D
c
c      material jacobian matrix
c        3D
c           dsdePl    |  1111   1122   1133   1112   1123   1113 |
c           dsdePl    |  2211   2222   2233   2212   2223   2213 |
c           dsdePl    |  3311   3322   3333   3312   3323   3313 |
c           dsdePl    |  1211   1222   1233   1212   1223   1213 |
c           dsdePl    |  2311   2322   2333   2312   2323   2313 |
c           dsdePl    |  1311   1322   1333   1312   1323   1313 |
c        plane strain or axisymmetric (11, 22, 33, 12)
c           dsdePl    |  1111   1122   1133   1112 |
c           dsdePl    |  2211   2222   2233   2212 |
c           dsdePl    |  3311   3322   3333   3312 |
c           dsdePl    |  1211   1222   1233   1212 |
c        plane stress (11, 22, 12)
c           dsdePl    |  1111   1122   1112 |
c           dsdePl    |  2211   2222   2212 |
c           dsdePl    |  1211   1222   1212 |
c        3d beam (11, 13, 12)
c           dsdePl    |  1111   1113   1112 |
c           dsdePl    |  1311   1313   1312 |
c           dsdePl    |  1211   1213   1212 |
c        1d
c           dsdePl    |  1111 |
c
c*************************************************************************
#include "impcom.inc"
c
      INTEGER
     &                 matId, elemId,
     &                 kDomIntPt, kLayer, kSectPt,
     &                 ldstep,isubst,keycut,
     &                 nDirect,nShear,ncomp,nStatev,nProp
      DOUBLE PRECISION
     &                 Time,    dTime,   Temp,    dTemp,
     &                 sedEl,   sedPl,   epseq,   epsZZ
      DOUBLE PRECISION
     &                 stress  (ncomp  ), ustatev (nStatev),
     &                 dsdePl  (ncomp,ncomp),
     &                 Strain  (ncomp  ), dStrain (ncomp  ),
     &                 epsPl   (ncomp  ), prop    (nProp  ),
     &                 coords  (3),
     &                 defGrad (3,3),     defGrad_t(3,3),
     &                 tsstif  (2)
c
      EXTERNAL         usermat3d, usermatps, usermatbm, usermat1d

      DOUBLE PRECISION var0, var1, var2, var3, var4, var5,
     &                 var6, var7, var8
c
c*************************************************************************
c
      IF(ncomp .GE. 4) THEN
c ***    3d, plane strain and axisymmetric example
         call usermat3d (
     &                   matId, elemId,kDomIntPt, kLayer, kSectPt,
     &                   ldstep,isubst,keycut,
     &                   nDirect,nShear,ncomp,nStatev,nProp,
     &                   Time,dTime,Temp,dTemp,
     &                   stress,ustatev,dsdePl,sedEl,sedPl,epseq,
     &                   Strain,dStrain, epsPl, prop, coords,
     &                   var0, defGrad_t, defGrad,
     &                   tsstif, epsZZ,
     &                   var1, var2, var3, var4, var5,
     &                   var6, var7, var8)

      ELSE IF(nDirect.eq. 2 .and. ncomp .EQ. 3) THEN
c ***    plane stress example
         call usermatps (
     &                   matId, elemId,kDomIntPt, kLayer, kSectPt,
     &                   ldstep,isubst,keycut,
     &                   nDirect,nShear,ncomp,nStatev,nProp,
     &                   Time,dTime,Temp,dTemp,
     &                   stress,ustatev,dsdePl,sedEl,sedPl,epseq,
     &                   Strain,dStrain, epsPl, prop, coords,
     &                   var0, defGrad_t, defGrad,
     &                   tsstif, epsZZ,
     &                   var1, var2, var3, var4, var5,
     &                   var6, var7, var8)

      ELSE IF(ncomp .EQ. 3) THEN
c ***    3d beam example
         call usermatbm (
     &                   matId, elemId,kDomIntPt, kLayer, kSectPt,
     &                   ldstep,isubst,keycut,
     &                   nDirect,nShear,ncomp,nStatev,nProp,
     &                   Time,dTime,Temp,dTemp,
     &                   stress,ustatev,dsdePl,sedEl,sedPl,epseq,
     &                   Strain,dStrain, epsPl, prop, coords,
     &                   var0, defGrad_t, defGrad,
     &                   tsstif, epsZZ,
     &                   var1, var2, var3, var4, var5,
     &                   var6, var7, var8)

      ELSE IF(ncomp .EQ. 1) THEN
c ***    1d beam example
         call usermat1d (
     &                   matId, elemId,kDomIntPt, kLayer, kSectPt,
     &                   ldstep,isubst,keycut,
     &                   nDirect,nShear,ncomp,nStatev,nProp,
     &                   Time,dTime,Temp,dTemp,
     &                   stress,ustatev,dsdePl,sedEl,sedPl,epseq,
     &                   Strain,dStrain, epsPl, prop, coords,
     &                   var0, defGrad_t, defGrad,
     &                   tsstif, epsZZ,
     &                   var1, var2, var3, var4, var5,
     &                   var6, var7, var8)

      END IF
      return
      end
c*deck,usermat3d    USERDISTRIB  parallel                                gal
      subroutine usermat3d(
     &                   matId, elemId,kDomIntPt, kLayer, kSectPt,
     &                   ldstep,isubst,keycut,
     &                   nDirect,nShear,ncomp,nStatev,nProp,
     &                   Time,dTime,Temp,dTemp,
     &                   stress,ustatev,dsdePl,sedEl,sedPl,epseq,
     &                   Strain,dStrain, epsPl, prop, coords,
     &                   var0, defGrad_t, defGrad,
     &                   tsstif, epsZZ,
     &                   var1, var2, var3, var4, var5,
     &                   var6, var7, var8)
           use MathRoutines
           use ElasticViscoPlastic

c*************************************************************************
c     *** primary function ***
c
c           user defined material constitutive model
c
c      Attention:
c           User must define material constitutive law properly
c           according to the stress state such as 3D, plane strain
c           and axisymmetry, plane stress and beam.
c
c           a 3D material constitutive model can use for
c           plane strain and axisymmetry cases.
c
c           When using shell elements, a plane stress algorithm
c           must be use.
c
c                                             gal July, 1999
c
c       The following demonstrates a USERMAT subroutine for
c       a plasticity model of 3D solid elements or plane elements
c       in plane strain or axisymmetric stress state. The plasticity
c       model is the same as TB, BISO.
c       See "ANSYS user material subroutine USERMAT" for detailed
c       description of how to write a USERMAT routine.
c
c*************************************************************************
c
c     input arguments
c     ===============
c      matId     (int,sc,i)               material #
c      elemId    (int,sc,i)               element #
c      kDomIntPt (int,sc,i)               "k"th domain integration point
c      kLayer    (int,sc,i)               "k"th layer
c      kSectPt   (int,sc,i)               "k"th Section point
c      ldstep    (int,sc,i)               load step number
c      isubst    (int,sc,i)               substep number
c      nDirect   (int,sc,in)              # of direct components
c      nShear    (int,sc,in)              # of shear components
c      ncomp     (int,sc,in)              nDirect + nShear
c      nstatev   (int,sc,l)               Number of state variables
c      nProp     (int,sc,l)               Number of material ocnstants
c
c      Temp      (dp,sc,in)               temperature at beginning of
c                                         time increment
c      dTemp     (dp,sc,in)               temperature increment
c      Time      (dp,sc,in)               time at beginning of increment (t)
c      dTime     (dp,sc,in)               current time increment (dt)
c
c      Strain   (dp,ar(ncomp),i)          Strain at beginning of time increment
c      dStrain  (dp,ar(ncomp),i)          Strain increment
c      prop     (dp,ar(nprop),i)          Material constants defined by TB,USER
c      coords   (dp,ar(3),i)              current coordinates
c      defGrad_t(dp,ar(3,3),i)            Deformation gradient at time t
c      defGrad  (dp,ar(3,3),i)            Deformation gradient at time t+dt
c
c     input output arguments
c     ======================
c      stress   (dp,ar(nTesn),io)         stress
c      ustatev   (dp,ar(nstatev),io)      user state variable
c            ustatev(1)                     - equivalent plastic strain
c            ustatev(2) - statev(1+ncomp)   - plastic strain vector
c            ustatev(nStatev)               - von-Mises stress
c      sedEl    (dp,sc,io)                elastic work
c      sedPl    (dp,sc,io)                plastic work
c      epseq    (dp,sc,io)                equivalent plastic strain
c      tsstif   (dp,ar(2),io)             transverse shear stiffness
c                                         tsstif(1) - Gxz
c                                         tsstif(2) - Gyz
c                                         tsstif(1) is also used to calculate hourglass
c                                         stiffness, this value must be defined when low
c                                         order element, such as 181, 182, 185 with uniform
c                                         integration is used.
c      var?     (dp,sc,io)                not used, they are reserved arguments
c                                         for further development
c
c     output arguments
c     ================
c      keycut   (int,sc,io)               loading bisect/cut control
c                                         0 - no bisect/cut
c                                         1 - bisect/cut
c                                         (factor will be determined by ANSYS solution control)
c      dsdePl   (dp,ar(ncomp,ncomp),io)   material jacobian matrix
c      epsZZ    (dp,sc,o)                 strain epsZZ for plane stress,
c                                         define it when accounting for thickness change
c                                         in shell and plane stress states
c
c*************************************************************************
c
c      ncomp   6   for 3D  (nshear=3)
c      ncomp   4   for plane strain or axisymmetric (nShear = 1)
c      ncomp   3   for plane stress (nShear = 1)
c      ncomp   3   for 3d beam      (nShear = 2)
c      ncomp   1   for 1D (nShear = 0)
c
c      stresss and strains, plastic strain vectors
c          11, 22, 33, 12, 23, 13    for 3D
c          11, 22, 33, 12            for plane strain or axisymmetry
c          11, 22, 12                for plane stress
c          11, 13, 12                for 3d beam
c          11                        for 1D
c
c      material jacobian matrix
c        3D
c           dsdePl    |  1111   1122   1133   1112   1123   1113 |
c           dsdePl    |  2211   2222   2233   2212   2223   2213 |
c           dsdePl    |  3311   3322   3333   3312   3323   3313 |
c           dsdePl    |  1211   1222   1233   1212   1223   1213 |
c           dsdePl    |  2311   2322   2333   2312   2323   2313 |
c           dsdePl    |  1311   1322   1333   1312   1323   1313 |
c        plane strain or axisymmetric (11, 22, 33, 12)
c           dsdePl    |  1111   1122   1133   1112 |
c           dsdePl    |  2211   2222   2233   2212 |
c           dsdePl    |  3311   3322   3333   3312 |
c           dsdePl    |  1211   1222   1233   1212 |
c        plane stress (11, 22, 12)
c           dsdePl    |  1111   1122   1112 |
c           dsdePl    |  2211   2222   2212 |
c           dsdePl    |  1211   1222   1212 |
c        3d beam (11, 13, 12)
c           dsdePl    |  1111   1113   1112 |
c           dsdePl    |  1311   1313   1312 |
c           dsdePl    |  1211   1213   1212 |
c        1d
c           dsdePl    |  1111 |
c
c*************************************************************************
#include "impcom.inc"
c


      INTEGER
     &                 matId, elemId,
     &                 kDomIntPt, kLayer, kSectPt,
     &                 ldstep,isubst,keycut,
     &                 nDirect,nShear,ncomp,nStatev,nProp
      DOUBLE PRECISION
     &                 Time,    dTime,   Temp,    dTemp,
     &                 sedEl,   sedPl,   epseq,   epsZZ
      DOUBLE PRECISION
     &                 stress  (ncomp  ), ustatev (nStatev),
     &                 dsdePl  (ncomp,ncomp),
     &                 Strain  (ncomp  ), dStrain (ncomp  ),
     &                 epsPl   (ncomp  ), prop    (nProp  ),
     &                 coords  (3),
     &                 defGrad (3,3),     defGrad_t(3,3),
     &                 tsstif  (2)
c
c***************** User defined part *************************************
c
c --- parameters
c
      INTEGER          mcomp
      DOUBLE PRECISION HALF, THIRD, ONE, TWO, SMALL, ONEHALF,
     &                 ZERO, TWOTHIRD, ONEDM02, ONEDM05, sqTiny
      PARAMETER       (ZERO       = 0.d0,
     &                 HALF       = 0.5d0,
     &                 THIRD      = 1.d0/3.d0,
     &                 ONE        = 1.d0,
     &                 TWO        = 2.d0,
     &                 SMALL      = 1.d-08,
     &                 sqTiny     = 1.d-20,
     &                 ONEDM02    = 1.d-02,
     &                 ONEDM05    = 1.d-05,
     &                 ONEHALF    = 1.5d0,
     &                 TWOTHIRD   = 2.0d0/3.0d0,
     &                 mcomp      = 6
     &                 )
c
c --- local variables
c
c      sigElp   (dp,ar(6  ),l)            trial stress
c      dsdeEl   (dp,ar(6,6),l)            elastic moduli
c      sigDev   (dp,ar(6  ),l)            deviatoric stress tensor
c      dfds     (dp,ar(6  ),l)            derivative of the yield function
c      JM       (dp,ar(6,6),l)            2D matrix for a 4 order tensor
c      pEl      (dp,sc     ,l)            hydrostatic pressure stress
c      qEl      (dp,sc     ,l)            von-mises stress
c      pleq_t   (dp,sc     ,l)            equivalent plastic strain at beginnig of time increment
c      pleq     (dp,sc     ,l)            equivalent plastic strain at end of time increment
c      dpleq    (dp,sc     ,l)            incremental equivalent plastic strain
c      sigy_t   (dp,sc     ,l)            yield stress at beginnig of time increments
c      sigy     (dp,sc     ,l)            yield stress at end of time increment
c      young    (dp,sc     ,l)            Young's modulus
c      posn     (dp,sc     ,l)            Poiss's ratio
c      sigy0    (dp,sc     ,l)            initial yield stress
c      dsigdep  (dp,sc     ,l)            plastic slop
c      twoG     (dp,sc     ,l)            two time of shear moduli
c      threeG   (dp,sc     ,l)            three time of shear moduli
c
c --- temperary variables for solution purpose
c      i, j
c      threeOv2qEl, oneOv3G, qElOv3G, con1, con2, fratio
c
      EXTERNAL         vzero, vmove, get_ElmData
      DOUBLE PRECISION sigElp(mcomp), dsdeEl(mcomp,mcomp), G(mcomp),
     &                 sigDev(mcomp), JM    (mcomp,mcomp), dfds(mcomp),
     &                 sigi  (mcomp), strainEl(mcomp)

      DOUBLE PRECISION var0, var1, var2, var3, var4, var5,
     &                 var6, var7, var8

      DATA G/1.0D0,1.0D0,1.0D0,0.0D0,0.0D0,0.0D0/
c
      INTEGER          i, j
      DOUBLE PRECISION pEl,   qEl,     pleq_t,  sigy_t , sigy,
     &                 dpleq, pleq,
     &                 young, posn,    sigy0,   dsigdep,
     &                 elast1,elast2,
     &                 twoG,  threeG,  oneOv3G, qElOv3G, threeOv2qEl,
     &                 fratio,  con1,    con2, dperr(3)

      DOUBLE PRECISION  detMTXA, MTXA(3,3)

      call InterfaceAnsys (
     &                   matId, elemId,kDomIntPt, kLayer, kSectPt,
     &                   ldstep,isubst,keycut,
     &                   nDirect,nShear,ncomp,nStatev,nProp,
     &                   Time,dTime,Temp,dTemp,
     &                   stress,ustatev,dsdePl,sedEl,sedPl,epseq,
     &                   Strain,dStrain, epsPl, prop, coords,
     &                   var0, defGrad_t, defGrad,
     &                   tsstif, epsZZ,
     &                   var1, var2, var3, var4, var5,
     &                   var6, var7, var8)










c **********************************************************************************

c **********************************************************************************

c*************************************************************************
c
      keycut   = 0
      dsigdep  = ZERO
      pleq_t   = ustatev(1)
      pleq     = pleq_t
c *** get Young's modulus and Poisson's ratio, initial yield stress and others
      young    = prop(1)
      posn     = prop(2)
      sigy0    = prop(3)
c *** plastic strain tensor
      call vmove(ustatev(2), epsPl(1), ncomp)
c *** calculate plastic slope
      dsigdep  = young*prop(4)/(young-prop(4))
      twoG     = young / (ONE+posn)
      threeG   = ONEHALF * twoG
      elast1=young*posn/((1.0D0+posn)*(1.0D0-TWO*posn))
      elast2=HALF*twoG
c *** define tsstif(1) since it is used for calculation of hourglass stiffness
      tsstif(1) = elast2

c *** calculate elastic stiffness matrix (3d)
c
      dsdeEl(1,1)=(elast1+TWO*elast2)*G(1)*G(1)
      dsdeEl(1,2)=elast1*G(1)*G(2)+elast2*TWO*G(4)*G(4)
      dsdeEl(1,3)=elast1*G(1)*G(3)+elast2*TWO*G(5)*G(5)
      dsdeEl(1,4)=elast1*G(1)*G(4)+elast2*TWO*G(1)*G(4)
      dsdeEl(1,5)=elast1*G(1)*G(5)+elast2*TWO*G(1)*G(5)
      dsdeEl(1,6)=elast1*G(1)*G(6)+elast2*TWO*G(4)*G(5)
      dsdeEl(2,2)=(elast1+TWO*elast2)*G(2)*G(2)
      dsdeEl(2,3)=elast1*G(2)*G(3)+elast2*TWO*G(6)*G(6)
      dsdeEl(2,4)=elast1*G(2)*G(4)+elast2*TWO*G(1)*G(4)
      dsdeEl(2,5)=elast1*G(2)*G(5)+elast2*TWO*G(1)*G(5)
      dsdeEl(2,6)=elast1*G(2)*G(6)+elast2*TWO*G(2)*G(6)
      dsdeEl(3,3)=(elast1+TWO*elast2)*G(3)*G(3)
      dsdeEl(3,4)=elast1*G(3)*G(4)+elast2*TWO*G(5)*G(6)
      dsdeEl(3,5)=elast1*G(3)*G(5)+elast2*TWO*G(5)*G(3)
      dsdeEl(3,6)=elast1*G(3)*G(6)+elast2*TWO*G(6)*G(3)
      dsdeEl(4,4)=elast1*G(4)*G(4)+elast2*(G(1)*G(2)+G(4)*G(4))
      dsdeEl(4,5)=elast1*G(4)*G(5)+elast2*(G(1)*G(6)+G(5)*G(4))
      dsdeEl(4,6)=elast1*G(4)*G(6)+elast2*(G(4)*G(6)+G(5)*G(2))
      dsdeEl(5,5)=elast1*G(5)*G(5)+elast2*(G(1)*G(3)+G(5)*G(5))
      dsdeEl(5,6)=elast1*G(5)*G(6)+elast2*(G(4)*G(3)+G(5)*G(6))
      dsdeEl(6,6)=elast1*G(6)*G(6)+elast2*(G(2)*G(3)+G(6)*G(6))
      do i=1,ncomp-1
        do j=i+1,ncomp
          dsdeEl(j,i)=dsdeEl(i,j)
        end do
      end do
c
c
c *** get initial stress
      call vzero(sigi(1),ncomp)
      i = ncomp
      call get_ElmData ('ISIG', elemId,kDomIntPt, i, sigi)
c
c *** calculate the trial stress and
c     copy elastic moduli dsdeEl to material Jacobian matrix
      do i=1,ncomp
         strainEl(i) = Strain(i) + dStrain(i) - epsPl(i)
      end do
      call vzero(sigElp, 6)
      do i=1,ncomp
         do j=1,ncomp
            dsdePl(j,i) = dsdeEl(j,i)
            sigElp(i) = sigElp(i)+dsdeEl(j,i)*strainEl(j)
         end do
         sigElp(i) = sigElp(i) + sigi(i)
      end do

c *** hydrostatic pressure stress
      pEl = -THIRD * (sigElp(1) + sigElp(2) + sigElp(3))
c *** compute the deviatoric stress tensor
      sigDev(1) = sigElp(1) + pEl
      sigDev(2) = sigElp(2) + pEl
      sigDev(3) = sigElp(3) + pEl
      sigDev(4) = sigElp(4)
      sigDev(5) = sigElp(5)
      sigDev(6) = sigElp(6)
c *** compute the determinant
      MTXA(1,1) = 1.0d0
      MTXA(2,2) = 1.0d0
      MTXA(3,3) = 1.0d0
      MTXA(1,2) = 0.0d0
      MTXA(2,1) = 0.0d0
      MTXA(2,3) = 0.0d0
      MTXA(3,2) = 0.0d0
      MTXA(1,3) = 0.0d0
      MTXA(3,1) = 0.0d0

      detMTXA = det(MTXA)

      print*, "Determinante= ", detMTXA


c *** compute von-mises stress
      qEl =
     &  sigDev(1) * sigDev(1)+sigDev(2) * sigDev(2)+
     &  sigDev(3) * sigDev(3)+
     &  TWO*(sigDev(4) * sigDev(4)+ sigDev(5) * sigDev(5)+
     &  sigDev(6) * sigDev(6))
      qEl = sqrt( ONEHALF * qEl)
c *** compute current yield stress
      sigy    = sigy0 + dsigdep * pleq
c
      fratio = qEl / sigy - ONE
c *** check for yielding
      IF (sigy .LE. ZERO.or.fratio .LE. -SMALL) GO TO 500
c
      sigy_t = sigy
      threeOv2qEl = ONEHALF / qEl
c *** compute derivative of the yield function
      DO i=1, ncomp
         dfds(i) = threeOv2qEl * sigDev(i)
      END DO
      oneOv3G  = ONE / threeG
      qElOv3G  = qEl * oneOv3G
c *** initial guess of incremental equivalent plastic strain
      dpleq    = qElOv3G - sigy * oneOv3G
      pleq     = pleq_t + dpleq
      sigy     = sigy0 + dsigdep * pleq
c
c ***  update stresses
      DO i = 1 , ncomp
         stress(i) =  sigElp(i) - TWOTHIRD * (qEl-sigy) * dfds(i)
      END DO
c
c ***  update plastic strains
      DO i = 1 , nDirect
         epsPl(i) = epsPl(i) + dfds(i) * dpleq
      END DO
      DO i = nDirect + 1 , ncomp
         epsPl(i) = epsPl(i) + TWO * dfds(i) * dpleq
      END DO
      epseq  = pleq
c *** Update state variables
      ustatev(1) = pleq
      do i=1,ncomp
         ustatev(i+1) = epsPl(i)
      end do
c *** Update plastic work
      sedPl = sedPl + HALF * (sigy_t+sigy)*dpleq
c
c *** Material Jcobian matrix
c
      IF (qEl.LT.sqTiny) THEN
         con1 = ZERO
      ELSE
         con1 = threeG * dpleq / qEl
      END IF
      con2 = threeG/(threeG+dsigdep) - con1
      con2 = TWOTHIRD * con2
      DO i=1,ncomp
         DO j=1,ncomp
            JM(j,i) = ZERO
         END DO
      END DO
      DO i=1,nDirect
         DO j=1,nDirect
            JM(i,j) = -THIRD
         END DO
         JM(i,i) = JM(i,i) + ONE
      END DO
      DO i=nDirect + 1,ncomp
         JM(i,i) = HALF
      END DO
      DO i=1,ncomp
         DO j=1,ncomp
            dsdePl(i,j) =    dsdeEl(i,j) - twoG
     &           * (  con2 * dfds(i) * dfds(j) + con1 * JM(i,j) )
         END DO
      END DO
c
      goto 600
  500 continue

c *** Update stress in case of elastic/unloading
      do i=1,ncomp
         stress(i) = sigElp(i)
      end do

  600 continue
      sedEl = ZERO
      DO i = 1 , ncomp
         sedEl = sedEl + stress(i)*(Strain(i)+dStrain(i)-epsPl(i))
      END DO
      sedEl    = sedEl * HALF
      ustatev(nStatev) = sigy
c
      return
      end
