      subroutine calrej_sat(obstyp,subtypestart,subtypeend,
     $     sensid,par,vertco,
     $     singlelvl,nlevels,ref,outfile,lenoutfile,
     $     norejlims,rejlims,outfile2,lenoutfile2)
      implicit none
c
c     Subroutine to calculation rejections from aux. ODB based ascii files.
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

      integer par,obstyp,sensid, satid
      integer nlevels,subtypestart,subtypeend,vertco
      real ref(nlevels)
      logical  singlelvl
      character*(*) outfile
      integer lenoutfile
      character*(*) outfile2
      integer lenoutfile2
      integer norejlims
      real rejlims(norejlims)
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
      integer iflgact,iflgrej,iflgpas,iflgblk
      parameter (iunit=10)    
      integer ifgdep,ifgbc,iandep,ibccor

c
c     open output file 
c
      open(50,file=outfile(1:lenoutfile))
      open(51,file=outfile2(1:lenoutfile2))


c     loop over odb based ascii-file
c
      OPEN(UNIT=IUNIT,FORM='FORMATTED',STATUS='UNKNOWN',ERR=206)
 200    continue 
      icount=icount+1
      READ(iunit,*,END=210,ERR=205) IOBTYP_ODB,ICODETYPE_ODB,
     x  CSTAID_ODB,IVARNO_ODB,
     x  RLAT_ODB,RLON_ODB,IVERTCO_TYPE_ODB,RPRESS_ODB,ISENSOR_ODB,
     x  IDATE_ODB,ITIME_ODB,ISTATUS_ACTHDR_ODB,ISTATUS_BLKHDR_ODB,
     x  ISTATUS_PASHDR_ODB,ISTATUS_REJHDR_ODB,ISTATUS_ACTBOD_ODB,
     x  ISTATUS_BLKBOD_ODB,ISTATUS_PASBOD_ODB,ISTATUS_REJBOD_ODB,
     x  RANDEP_ODB,RFGDEP_ODB,ROBS_ODB,BIASCRL_ODB
             
      vert=RPRESS_ODB

      if (IOBTYP_ODB.eq.obstyp) then
          read(CSTAID_ODB(1:8),'(I8)') satid
      endif

  
      if ((IOBTYP_ODB.eq.obstyp).and.(satid.ge.subtypestart)
     $        .and.(satid.le.subtypeend).and.
     $    (ISENSOR_ODB.eq.sensid)) then

        if((IVARNO_ODB.EQ.par).and.(IVERTCO_TYPE_ODB.eq.vertco))then

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
            nlev=(nlevels-j)+1
          endif

          iflgact=0
          iflgrej=0
          iflgpas=0
          iflgblk=0
      
          if ((ISTATUS_ACTHDR_ODB.EQ.1).and.
     x       (ISTATUS_ACTBOD_ODB.EQ.1)) iflgact=1
          if ((ISTATUS_REJHDR_ODB.EQ.1).or.
     x       (ISTATUS_REJBOD_ODB.EQ.1)) iflgrej=1
          if ((ISTATUS_PASHDR_ODB.EQ.1).or.
     x       (ISTATUS_PASBOD_ODB.EQ.1)) iflgpas=1
          if ((ISTATUS_BLKHDR_ODB.EQ.1).or.
     x       (ISTATUS_BLKBOD_ODB.EQ.1)) iflgblk=1

          write(50,*) RLAT_ODB*57.2957795131, 
     x                RLON_ODB*57.2957795131,
     x        iflgact,iflgrej, iflgpas,iflgblk
 
        endif
c
c     find the right channel
c     
         nlev=1
         do j=1,nlevels
           if (ref(j).lt.vert) exit
         enddo
         nlev=(nlevels-j)+1

c
c     find the departures
c  
                     
         do j=1,norejlims
           if (rejlims(j).gt.RFGDEP_ODB) exit
         enddo
         ifgdep=j

         do j=1,norejlims
           if (rejlims(j).gt.(RFGDEP_ODB+BIASCRL_ODB)) exit
         enddo
         ifgbc=j

         do j=1,norejlims
           if (rejlims(j).gt.RANDEP_ODB) exit
         enddo
         iandep=j

         do j=1,norejlims
           if (rejlims(j).gt.BIASCRL_ODB) exit
         enddo
         ibccor=j

         write(51,'(2F10.3,5I3)') RLAT_ODB*57.2957795131,
     x                            RLON_ODB*57.2957795131,
     x            nlev,ifgdep,ifgbc,iandep,ibccor 



      endif


      GOTO 200
 205  CONTINUE
      STOP 'ERROR WHEN READING ODB BASED STAT FILE'
 206  CONTINUE
      STOP 'ERROR WHEN OPENING ODB BASED STAT FILE'
 210  CONTINUE

      close(50)
      close(51)
      close(iunit)
      end      
