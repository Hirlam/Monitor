      subroutine calstat(obstyp,subtypestart,subtypeend,par,vertco,
     $     singlelvl,nlevels,ref,lusage,outfile,lenoutfile,
     $     rmsfg,biasfg,absbiasfg,rmsan,biasan,absbiasan,count)
      implicit none
c
c     Subroutine to calculation statistics from aux. ODB based ascii files.
c     Author: ML 20080515 (based on allstat from KSM)
c
c
c     Output:
c     rmsfg: RMS difference between observed value and first guess
c     biasfg: bias between observed value and first guess
c     absbiasfg: abs(bias) between observed value and first guess
c     rmsan: RMS difference between observed value and analysis
c     biasan: bias between observed value and analysis
c     absbiasan: abs(bias) between observed value and analysis
c     count: no of active obs
c
c     Description of meaning of variables, flags etc can be found on
c     Meteo-France Web-based ODB manual:
c     http://www.cnrm.meteo.fr/gmapdoc/meshtml/DOC_odb     

      integer par,obstyp
      integer nlevels,subtypestart,subtypeend,vertco
      real ref(nlevels)
      logical  lusage,singlelvl
      character*(*) outfile
      integer lenoutfile
      real biasfg(nlevels),absbiasfg(nlevels),rmsfg(nlevels)
      real biasan(nlevels),absbiasan(nlevels),rmsan(nlevels)
      integer count(nlevels),qccount(4,nlevels)

      integer ilenin,ipos,i,j,no
      real lat,lon,alt,obs,vert
      integer nlev

c ODB based ascii-files variables

      CHARACTER*8 CSTAID_ODB 
      CHARACTER*400 CLINE
      INTEGER IUNIT,icount

      integer IOBTYP_ODB,ICODETYPE_ODB,IVARNO_ODB
      integer IVERTCO_TYPE_ODB, ISENSOR_ODB,IDATE_ODB,ITIME_ODB
      integer ISTATUS_ACTHDR_ODB,ISTATUS_BLKHDR_ODB,ISTATUS_PASHDR_ODB
      integer ISTATUS_REJHDR_ODB,ISTATUS_ACTBOD_ODB,ISTATUS_BLKBOD_ODB
      integer ISTATUS_PASBOD_ODB,ISTATUS_REJBOD_ODB
      real RLAT_ODB,RLON_ODB,RPRESS_ODB,RANDEP_ODB,RFGDEP_ODB,ROBS_ODB
      real RFINOE_ODB, BIASCRL_ODB
      parameter (iunit=10)      


c
c     open output file 
c
      if (lusage) then
         open(50,file=outfile(1:lenoutfile))
      endif
c
c     initialize arrays
c
      do i=1,nlevels 
         biasfg(i)=0 
         absbiasfg(i)=0 
         rmsfg(i)=0
         biasan(i)=0 
         absbiasan(i)=0 
         rmsan(i)=0
         count(i)=0
      enddo

      icount = 0

c
c     loop over odb based ascii-file
c
      OPEN(UNIT=IUNIT,FORM='FORMATTED',STATUS='UNKNOWN',ERR=206)
 200  continue
      icount=icount+1
      READ(iunit,*,END=210,ERR=205) IOBTYP_ODB,ICODETYPE_ODB,
     x  CSTAID_ODB,IVARNO_ODB,
     x  RLAT_ODB,RLON_ODB,IVERTCO_TYPE_ODB,RPRESS_ODB,ISENSOR_ODB,
     x  IDATE_ODB,ITIME_ODB,ISTATUS_ACTHDR_ODB,ISTATUS_BLKHDR_ODB,
     x  ISTATUS_PASHDR_ODB,ISTATUS_REJHDR_ODB,ISTATUS_ACTBOD_ODB,
     x  ISTATUS_BLKBOD_ODB,ISTATUS_PASBOD_ODB,ISTATUS_REJBOD_ODB,
     x  RANDEP_ODB,RFGDEP_ODB,ROBS_ODB, RFINOE_ODB, BIASCRL_ODB
             
      vert=RPRESS_ODB
!
!    Accumulate statistics

      if ((IOBTYP_ODB.eq.obstyp).and.(ICODETYPE_ODB.ge.subtypestart)
     x        .and.(ICODETYPE_ODB.le.subtypeend)) then

        if  ((IVARNO_ODB.EQ.par).and.
     x      (IVERTCO_TYPE_ODB.eq.vertco)) then
          if ((ISTATUS_ACTHDR_ODB.EQ.1).and.
     x       (ISTATUS_ACTBOD_ODB.EQ.1)) then

c
c     find the right level
c     
            if (singlelvl) then
              nlev=1
            else
              nlev=1
              do j=1,nlevels
                if (ref(j).lt.vert) exit
              enddo
              nlev=j
            endif

            count(nlev)=count(nlev)+1
            biasfg(nlev)=biasfg(nlev)+RFGDEP_ODB
            absbiasfg(nlev)=absbiasfg(nlev)+abs(RFGDEP_ODB)
            rmsfg(nlev)=rmsfg(nlev)+RFGDEP_ODB*RFGDEP_ODB
            biasan(nlev)=biasan(nlev)+RANDEP_ODB
            absbiasan(nlev)=absbiasan(nlev)+abs(RANDEP_ODB)
            rmsan(nlev)=rmsan(nlev)+RANDEP_ODB*RANDEP_ODB

            if (lusage) then 
 
              write(50,*) RLAT_ODB*57.2957795131,  
     x                    RLON_ODB*57.2957795131, 1,0,0,0
 
            endif 
          endif
        endif
      endif


      GOTO 200
 205  CONTINUE
      STOP 'ERROR WHEN READING ODB BASED STAT FILE'
 206  CONTINUE
      STOP 'ERROR WHEN OPENING ODB BASED STAT FILE'
 210  CONTINUE

c
c     Normalize rms/bias values
c
      if (singlelvl) then
        nlev=1
      else
        nlev=nlevels
      endif
      do i=1,nlev
        if (count(i).gt.0) then
           rmsfg(i)=sqrt(rmsfg(i)/count(i))
           biasfg(i)=biasfg(i)/count(i)
           absbiasfg(i)=absbiasfg(i)/count(i)
           rmsan(i)=sqrt(rmsan(i)/count(i))
           biasan(i)=biasan(i)/count(i)
           absbiasan(i)=absbiasan(i)/count(i)
        endif
      enddo

      if (lusage) then
        close(50)
      endif

      close(iunit)
      end      
