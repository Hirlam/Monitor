      program rej_stat
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
     $     lamsub,lamsub_noaa15,lamsub_noaa16,lamsub_noaa17,
     $     lamsub_noaa18,lamsub_noaa19,lamsub_aqua,lamsub_metop,
     $     lgbgps,lgbgpsiwv,lgbgpstdel,lgbgpswdel,lgbgpsps,lpreplo,
     $     lstatcal

      namelist/rejstat/
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
     $     lamsub,lamsub_noaa15,lamsub_noaa16,lamsub_noaa17,
     $     lamsub_noaa18,lamsub_noaa19,lamsub_aqua,lamsub_metop,
     $     lgbgps,lgbgpsiwv,lgbgpstdel,lgbgpswdel,lgbgpsps,lpreplo,
     $     lstatcal  
      integer iunit,outunit,plunit
       parameter(outunit=33,plunit=34)
      integer nlevelspres,nlevelsheig,nlevelsamsua
      integer nlevelsamsub, norejlims
      parameter (nlevelspres=15)
      parameter (nlevelsheig=15)
      parameter (nlevelsamsua=15)
      parameter (nlevelsamsub=5)
      parameter (norejlims=6)

      real refpres(nlevelspres),refheig(nlevelsheig)
      real refamsua(nlevelsamsua),refamsub(nlevelsamsub)
      real rejlims(norejlims)

      data refpres/92500,80000,60000,45000,35000,27500,22500,17500,
     $  12500,8500,6500,4000,2500,1500,0/

      data refamsua/14.5,13.5,12.5,11.5,10.5,9.5,8.5,7.5,6.5,5.5,
     $             4.5,3.5,2.5,1.5,0.5/

      data refamsub/4.5,3.5,2.5,1.5,0.5/

c      data rejlims/-2,-1,-0.5,0,0.5,1.,2./
      data rejlims/-1.5,-0.5,0.,0.5,1.5,200./


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
      lamsub=.true.
      lamsub_noaa15=.true.
      lamsub_noaa16=.true.
      lamsub_noaa17=.true.
      lamsub_noaa18=.true.
      lamsub_noaa19=.true.
      lamsub_aqua=.true. 
      lamsub_metop=.true.
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
      inquire(file='rejstat.dat',exist=lexists)
      if (lexists) then
         open(iunit,file='rejstat.dat')
         read(iunit,rejstat)
         close(iunit)
      endif
      write(*,rejstat)

c     temp 
        if  (ltemp) then

c     temp temperature
        if  (ltempt) then
           singlelvl=.false.
           call calrej(5,0,255,2,1,
     $        singlelvl,nlevelspres,refpres,'temp_t.dat',10)
        endif

        if  (ltempu) then
c     temp wind u-comp
           singlelvl=.false.
           call calrej(5,0,255,3,1,  
     $       singlelvl,nlevelspres,refpres,'temp_u.dat',10)
        endif

        if  (ltempv) then
c     temp wind v-comp
           singlelvl=.false.
           call calrej(5,0,255,4,1,
     $       singlelvl,nlevelspres,refpres,'temp_v.dat',10)
        endif


        if  (ltempq) then
c     temp wind v-comp
           singlelvl=.false.
           call calrej(5,0,255,7,1, 
     $       singlelvl,nlevelspres,refpres,'temp_q.dat',10)
        endif
        endif



c     airep
        if  (lairep) then

c     airep temperature
        if  (lairept) then
           singlelvl=.false.
           call calrej(2,0,255,2,1,
     $       singlelvl,nlevelspres,refpres,'airep_t.dat',11)
        endif

        if  (lairepu) then
           singlelvl=.false.
           call calrej(2,0,255,3,1,
     $          singlelvl,nlevelspres,refpres,'airep_u.dat',11)
        endif

        if  (lairepv) then
           singlelvl=.false.
           call calrej(2,0,255,4,1,
     $       singlelvl,nlevelspres,refpres,'airep_v.dat',11)
        endif
        endif

        if  (lpilot) then

c     pilot u-wind comp
        if  (lpilotu) then
           singlelvl=.false.
           call calrej(6,0,255,3,1,
     $      singlelvl,nlevelspres,refpres,'pilot_t.dat',11)
        endif

        if  (lpilotv) then
           singlelvl=.false.
           call calrej(6,0,255,4,1,
     $      singlelvl,nlevelspres,refpres,'pilot_v.dat',11)
        endif
        endif


        if  (lamsua) then
c     ATOVS amsua brightness temperatures                                             
                                                          
        if (lamsua_noaa15) then    
           singlelvl=.false.
           call calrej_sat(7,206,206,
     $     3,119,3,
     $     singlelvl,nlevelsamsua,refamsua,'amsua_noaa15_bt.dat',19
     $     ,norejlims,rejlims,'amsua_noaa15_bt_area.dat',24)
        endif

        if (lamsua_noaa16) then
           singlelvl=.false.
           call calrej_sat(7,207,207,
     $     3,119,3,
     $     singlelvl,nlevelsamsua,refamsua,'amsua_noaa16_bt.dat',19
     $     ,norejlims,rejlims,'amsua_noaa16_bt_area.dat',24)
        endif

        if (lamsua_noaa17) then
           singlelvl=.false.
           call calrej_sat(7,208,208,
     $     3,119,3,
     $     singlelvl,nlevelsamsua,refamsua,'amsua_noaa17_bt.dat',19
     $     ,norejlims,rejlims,'amsua_noaa17_bt_area.dat',24)
        endif

        if (lamsua_noaa18) then
              singlelvl=.false.
           call calrej_sat(7,209,209,
     $     3,119,3,
     $     singlelvl,nlevelsamsua,refamsua,'amsua_noaa18_bt.dat',19
     $     ,norejlims,rejlims,'amsua_noaa18_bt_area.dat',24)

        endif

        if (lamsua_noaa19) then
              singlelvl=.false.
           call calrej_sat(7,223,223,
     $     3,119,3,
     $     singlelvl,nlevelsamsua,refamsua,'amsua_noaa19_bt.dat',19
     $     ,norejlims,rejlims,'amsua_noaa19_bt_area.dat',24)
        endif

        if (lamsua_aqua) then
              singlelvl=.false.
           call calrej_sat(7,784,784,
     $     3,119,3,
     $     singlelvl,nlevelsamsua,refamsua,'amsua_aqua_bt.dat',17
     $      ,norejlims,rejlims,'amsua_aqua_bt_area.dat',22)

        endif

        if (lamsua_metop) then
              singlelvl=.false.
           call calrej_sat(7,4,4,
     $     3,119,3,
     $     singlelvl,nlevelsamsua,refamsua,'amsua_metop_bt.dat',18
     $     ,norejlims,rejlims,'amsua_metop_bt_area.dat',23)

        endif

        endif

        if  (lamsub) then
c     ATOVS amsub brightness temperatures                                             
                                                          
        if (lamsub_noaa15) then    
           singlelvl=.false.
           call calrej_sat(7,206,206,
     $     4,119,3,
     $     singlelvl,nlevelsamsub,refamsub,'amsub_noaa15_bt.dat',19
     $     ,norejlims,rejlims,'amsub_noaa15_bt_area.dat',24)
        endif

        if (lamsub_noaa16) then
           singlelvl=.false.
           call calrej_sat(7,207,207,
     $     4,119,3,
     $     singlelvl,nlevelsamsub,refamsub,'amsub_noaa16_bt.dat',19
     $     ,norejlims,rejlims,'amsub_noaa16_bt_area.dat',24)
        endif

        if (lamsub_noaa17) then
           singlelvl=.false.
           call calrej_sat(7,208,208,
     $     4,119,3,
     $     singlelvl,nlevelsamsub,refamsub,'amsub_noaa17_bt.dat',19
     $     ,norejlims,rejlims,'amsub_noaa17_bt_area.dat',24)
        endif

        if (lamsub_noaa18) then
              singlelvl=.false.
           call calrej_sat(7,209,209,
     $     4,119,3,
     $     singlelvl,nlevelsamsub,refamsub,'amsub_noaa18_bt.dat',19
     $     ,norejlims,rejlims,'amsub_noaa18_bt_area.dat',24)

        endif

        if (lamsub_noaa19) then
              singlelvl=.false.
           call calrej_sat(7,223,223,
     $     15,119,3,
     $     singlelvl,nlevelsamsub,refamsub,'amsub_noaa19_bt.dat',19
     $     ,norejlims,rejlims,'amsub_noaa19_bt_area.dat',24)
        endif

        if (lamsub_aqua) then
              singlelvl=.false.
           call calrej_sat(7,784,784,
     $     15,119,3,
     $     singlelvl,nlevelsamsub,refamsub,'amsub_aqua_bt.dat',17
     $      ,norejlims,rejlims,'amsua_aqua_bt_area.dat',22)

        endif

        if (lamsub_metop) then
              singlelvl=.false.
           call calrej_sat(7,4,4,
     $     15,119,3,
     $     singlelvl,nlevelsamsub,refamsub,'amsub_metop_bt.dat',18
     $     ,norejlims,rejlims,'amsub_metop_bt_area.dat',23)

        endif

        endif


c     synop

        if  (lsynop) then
c     synop geopotential (surface pressure)
        if  (lsynopz) then
           singlelvl=.false.
           call calrej(1,11,14,1,1,
     $      singlelvl,nlevelspres,refpres,'synop_z.dat',11)
        endif
        endif

c     ship

        if  (lship) then

c     ship geopotential (surface pressure)
        if  (lshipz) then
           singlelvl=.true.
           call calrej(1,21,24,1,1,
     $       singlelvl,nlevelspres,refpres,'ship_z.dat',10)
        endif
        endif

        if  (lmetar) then

c     METAR gepotential (surface pressure)
        if  (lmetarz) then
           singlelvl=.true.
           call calrej(1,139,141,1,1,
     $       singlelvl,nlevelspres,refpres,'metar_z.dat',11)
        endif
        endif

        if  (ldribu) then

c     DRIBU gepotential (surface pressure)
        if  (ldribuz) then
           singlelvl=.true.
           call calrej(4,0,255,1,1,
     $       singlelvl,nlevelspres,refpres,'dribu_z.dat',11)

        endif 
        endif


      end



