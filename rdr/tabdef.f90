      subroutine tabdef
!
!     give values to table for saturation pressure and for d(es)/dt
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!
      real xndegr
      integer nstart,nstop
!
      parameter (xndegr=100.,nstart=1316,nstop=37316)
!
      real estab,destab,estab_water
!
      common/comtab/estab(nstart:nstop),destab(nstart:nstop),            &
                   estab_water(nstart:nstop)
!
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!
      integer jk
      real zdiv,ztemp
!
!-------------------------------------------------------------------
!
! define saturation pressure function to make vectorisation possible
!      Gerard Cats  KNMI    24 September 1992
!
      real esat,desdt,tt,zc2,zc4,zes,zes_water,esat_water
      real c1es,c2es,c2is,c3es,c4es,c4is

!     constants used in saturation vapour pressure calculations

      parameter (               &
          c1es=610.78,          &
          c2es=17.269,          &
          c2is=21.875,          &
          c3es=273.16,          &
          c4es=35.86,           &
          c4is=7.66             &
      )
!
      zc2(tt)=c2is + min(1.,max(0.,(tt-c3es+15)/15.))*( c2es-c2is )
      zc4(tt)=c4is + min(1.,max(0.,(tt-c3es+15)/15.))*( c4es-c4is )
      zes(tt)=zc2(tt)*(tt-c3es)/(tt-zc4(tt))
      zes_water(tt)=c2es*(tt-c3es)/(tt-c4es)
      esat(tt)=c1es*exp(zes(tt))
      esat_water(tt)=c1es*exp(zes_water(tt))
      desdt(tt)=c1es*exp(zes(tt))*zc2(tt)*(c3es-zc4(tt))/(tt-zc4(tt))**2
!
!-----------------------------------------------------------------------
!
      zdiv = 1./xndegr
!
      do jk=nstart,nstop
         ztemp = float(jk)*zdiv
         estab(jk) = esat(ztemp)
         destab(jk) = desdt(ztemp)
         if (jk.ge.10000) then
           estab_water(jk) = esat_water(ztemp)
         else
           estab_water(jk) = esat_water(100.)
         endif
      end do
!
!-----------------------------------------------------------------------
!
      return
      end
