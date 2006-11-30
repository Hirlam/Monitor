SUBROUTINE adddtg(idate,itime,inc,odate,otime)

!
! Interface to eclib time increment routine
!

IMPLICIT NONE

! Input

INTEGER  :: idate      ! Input date YYYYMMDD
INTEGER  :: itime      ! Input time HHMMSS
INTEGER  :: inc        ! Time increment in seconds
INTEGER  :: odate      ! Input date YYYYMMDD
INTEGER  :: otime      ! Input time HHMMSS

! Local

 INTEGER ::  year,  month,  day,  hour,  minute,	&
            nyear, nmonth, nday, nhour,	nminute,	&
            ierr


!-----------------------------------------------

   year   = idate/10000
   month  = (idate-year*10000)/100
   day    = mod(idate,100)
   hour   = itime / 10000
   minute = (itime-hour*10000)/100

   !CALL hourincr(year,month,day,hour,inc/3600,nyear,nmonth,nday,nhour,ierr)
   CALL minincr(year,month,day,hour,minute,inc/60,nyear,nmonth,nday,nhour,nminute,ierr)


   IF (ierr.NE.0) THEN
      WRITE(6,*)'Error returned from minincr ',ierr
      CALL abort
   ENDIF

   odate = nyear*10000 +  nmonth*100 + nday
   otime = nhour*10000 + nminute*100

   !WRITE(6,*)'ADDDTG'
   !WRITE(6,*)idate,itime,inc,odate,otime
   !WRITE(6,*)year,month,day,hour,minute,inc/60,nyear,nmonth,nday,nhour,nminute,ierr
   !WRITE(6,*)


RETURN

END SUBROUTINE adddtg
