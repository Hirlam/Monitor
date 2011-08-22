      program satbimon
C
C     ML program to calculate statistics from ODB based ascii files.
C     (inspired by KSM cmatat program)
C
      implicit none

      logical
     $     lsynop,lsynopz,lsynopt2m,lsynoptd2m,lsynopu10m,lsynopv10m,
     $     lship,lshipz,lshipt2m,lshiptd2m,lshipu10m,lshipv10m,
     $     lmetar,lmetarz,
     $     lairep,lairept,lairepu,lairepv,
     $     lsatob,lsatobu,lsatobv,
     $     ldribu,ldribuz,ldribut2m,ldributd2m,ldribuu10m,ldribuv10m,
     $     ltemp,ltempt,ltempu,ltempv,ltempq,
     $     lpilot,lpilotu,lpilotv,
     $     lamsua,lamsua_noaa15,lamsua_noaa16,lamsua_noaa17,
     $     lamsua_noaa18,lamsua_noaa19,lamsua_aqua,lamsua_metop,
     $     lgbgps,lgbgpsiwv,lgbgpstdel,lgbgpswdel,lgbgpsps,lpreplo,
     $     lusage, lstatcal

      namelist/odbstat/
     $     lsynop,lsynopz,lsynopt2m,lsynoptd2m,lsynopu10m,lsynopv10m,
     $     lship,lshipz,lshipt2m,lshiptd2m,lshipu10m,lshipv10m,
     $     lmetar,lmetarz,
     $     lairep,lairept,lairepu,lairepv,
     $     lsatob,lsatobu,lsatobv,
     $     ldribu,ldribuz,ldribut2m,ldributd2m,ldribuu10m,ldribuv10m,
     $     ltemp,ltempt,ltempu,ltempv,ltempq,
     $     lpilot,lpilotu,lpilotv,
     $     lamsua,lamsua_noaa15,lamsua_noaa16,lamsua_noaa17,
     $     lamsua_noaa18,lamsua_noaa19,lamsua_aqua,lamsua_metop,
     $     lgbgps,lgbgpsiwv,lgbgpstdel,lgbgpswdel,lgbgpsps,lpreplo,
     $     lusage,lstatcal  
      integer iunit,outunit,plunit
       parameter(outunit=33,plunit=34)
      integer nlevelspres,nlevelsheig,nlevelsatov
      parameter (nlevelspres=15)
      parameter (nlevelsheig=15)
      parameter (nlevelsatov=15)
      real refpres(nlevelspres),refheig(nlevelsheig),
     +     refatov(nlevelsatov)
      data refpres/92500,80000,60000,45000,35000,27500,22500,17500,
     $  12500,8500,6500,4000,2500,1500,0/
      data refatov/14.5, 13.5, 12.5, 11.5, 10.5, 9.5, 8.5, 7.5, 
     $ 6.5, 5.5,4.5, 3.5, 2.5, 1.5, 0.5/
      integer totlim
      parameter(totlim=41)
      real histlim(totlim)
      integer histnumfg(totlim+1),histnuman(totlim+1)
      data histlim/-400,-380,-360,-340,-320,-300,-280,-260,-240,-220,
     $ -200,-180,-160,-140,-120,-100,-80,-60,-40,-20,0,20,40,60,80,
     $ 100,120,140,160,180,200,220,240,260,280,300,320,340,360,380,400/
      integer maxlevels
      parameter (maxlevels=400)
      real rmsfg(maxlevels)
      real biasfg(maxlevels)
      real absbiasfg(maxlevels)
      real rmsan(maxlevels)
      real biasan(maxlevels)
      real absbiasan(maxlevels)
      real biascorr(maxlevels)
      integer count(maxlevels)
      real qccount(4,maxlevels)
      integer i
      logical lexists,singlelvl
c
      integer ngbgps
      parameter (ngbgps=31)

      lsynop=.true.
      lsynopz=.true.
      lsynopt2m=.false.
      lsynoptd2m=.false.
      lsynopu10m=.false.
      lsynopv10m=.false.
      lship=.true.
      lshipz=.true.
      lshipt2m=.false.
      lshiptd2m=.false.
      lshipu10m=.true.
      lshipv10m=.true.
      lmetar=.true.
      lmetarz=.true.
      lairep=.true.
      lairept=.true.
      lairepu=.true.
      lairepv=.true.
      lsatob=.true.
      lsatobu=.true.
      lsatobv=.true.
      ldribu=.true.
      ldribuz=.true.
      ldribut2m=.false.
      ldributd2m=.false.
      ldribuu10m=.false.
      ldribuv10m=.false.
      ltemp=.true.
      ltempt=.true.
      ltempu=.true.
      ltempv=.true.
      ltempq=.true.
      lpilot=.true.
      lpilotu=.true.
      lpilotv=.true.
      lamsua=.true.
      lamsua_noaa15=.true.
      lamsua_noaa16=.true.
      lamsua_noaa17=.true.
      lamsua_noaa18=.true.
      lamsua_noaa19=.true.
      lamsua_aqua=.true. 
      lamsua_metop=.true.
      lgbgps=.false.
      lgbgpsiwv=.false.
      lgbgpstdel=.false.
      lgbgpswdel=.false.
      lgbgpsps=.false.
      lpreplo=.true.
      do i=1,nlevelsheig
         refheig(nlevelsheig+1-i)=(i-1)*20000
      enddo
      iunit=10
      inquire(file='odbstat.dat',exist=lexists) 
      if (lexists) then
         open(iunit,file='odbstat.dat')
         read(iunit,odbstat)
         close(iunit)
      endif
      write(*,odbstat)

c     atovs amsua

        if  (lamsua) then
     
        if (lamsua_noaa15) then
         if (lpreplo) then
            open(plunit,file='amsua_noaa15_hist.dat')
         endif

           singlelvl=.false.
           call calbi_sat(7,206,206,
     $    3,119,3,
     $    singlelvl,nlevelsatov,refatov,lusage,'amsua_noaa15_bt.dat',19,
     $    biasfg,biasan,biascorr,count)

         if (lpreplo) then
            call prep_biplot('*****BIAS-RMS BT*****',21,15,
     $                      biasfg,biasan,biascorr,count,plunit)
         endif

         if (lpreplo) then
            close(plunit)
         endif
       endif

        if (lamsua_noaa16) then
         if (lpreplo) then
            open(plunit,file='amsua_noaa16_hist.dat')
         endif

           singlelvl=.false.
           call calbi_sat(7,207,207,
     $    3,119,3,
     $    singlelvl,nlevelsatov,refatov,lusage,'amsua_noaa16_bt.dat',19,
     $    biasfg,biasan,biascorr,count)

         if (lpreplo) then
            call prep_biplot('*****BIAS-RMS BT*****',21,15,
     $                      biasfg,biasan,biascorr,count,plunit)
         endif

         if (lpreplo) then
            close(plunit)
         endif
        endif

        if (lamsua_noaa17) then
         if (lpreplo) then
            open(plunit,file='amsua_noaa17_hist.dat')
         endif

           singlelvl=.false.
           call calbi_sat(7,208,208,
     $    3,119,3,
     $    singlelvl,nlevelsatov,refatov,lusage,'amsua_noaa17_bt.dat',19,
     $    biasfg,biasan,biascorr,count)

         if (lpreplo) then
            call prep_biplot('*****BIAS-RMS BT*****',21,15,
     $                      biasfg,biasan,biascorr,count,plunit)
         endif

         if (lpreplo) then
            close(plunit)
         endif
       endif

        if (lamsua_noaa18) then
         if (lpreplo) then
            open(plunit,file='amsua_noaa18_hist.dat')
         endif

           singlelvl=.false.
           call calbi_sat(7,209,209,
     $    3,119,3,
     $    singlelvl,nlevelsatov,refatov,lusage,'amsua_noaa18_bt.dat',19,
     $    biasfg,biasan,biascorr,count)

         if (lpreplo) then
            call prep_biplot('*****BIAS-RMS BT*****',21,15,
     $                      biasfg,biasan,biascorr,count,plunit)
         endif

         if (lpreplo) then
            close(plunit)
         endif
        endif

        if (lamsua_noaa19) then
         if (lpreplo) then
            open(plunit,file='amsua_noaa19_hist.dat')
         endif

           singlelvl=.false.
           call calbi_sat(7,223,223,
     $    3,119,3,
     $    singlelvl,nlevelsatov,refatov,lusage,'amsua_noaa19_bt.dat',19,
     $    biasfg,biasan,biascorr,count)

         if (lpreplo) then
            call prep_biplot('*****BIAS-RMS BT*****',21,15,
     $                      biasfg,biasan,biascorr,count,plunit)
         endif

         if (lpreplo) then
            close(plunit)
         endif
        endif


        if (lamsua_aqua) then
         if (lpreplo) then
            open(plunit,file='amsua_aqua_hist.dat')
         endif

             singlelvl=.false.
           call calbi_sat(7,784,784,
     $      3,119,3,
     $      singlelvl,nlevelsatov,refatov,lusage,'amsua_aqua_bt.dat',17,
     $      biasfg,biasan,biascorr,count)

         if (lpreplo) then
            call prep_biplot('*****BIAS-RMS BT*****',21,15,
     $                      biasfg,biasan,biascorr,count,plunit)
         endif

         if (lpreplo) then
            close(plunit)
         endif
        endif

        if (lamsua_metop) then
         if (lpreplo) then
            open(plunit,file='amsua_metop_hist.dat')
         endif

             singlelvl=.false.
           call calbi_sat(7,4,4,
     $      3,119,3,
     $     singlelvl,nlevelsatov,refatov,lusage,'amsua_metop_bt.dat',18,
     $     biasfg,biasan,biascorr,count)

         if (lpreplo) then
            call prep_biplot('*****BIAS-RMS BT*****',21,15,
     $                      biasfg,biasan,biascorr,count,plunit)
         endif

         if (lpreplo) then
            close(plunit)
         endif
        endif



       endif


      end



