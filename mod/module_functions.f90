MODULE functions
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
 CONTAINS
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
REAL FUNCTION tics(xmin,xmax)

 !
 ! Calculate tic interval 
 !
 IMPLICIT NONE

 ! Input

 REAL, INTENT(IN) :: xmin,xmax

 ! Local
 REAL :: magn,magn_diff,fac

 !------------------------------------------

  magn_diff=ABS(xmax-xmin)
  magn=FLOOR(LOG10(magn_diff))
  magn_diff = NINT(magn_diff /10.**magn)

  IF(magn_diff.GT.5.) THEN
     fac = 1.
  ELSEIF(magn_diff.GT.2.) THEN
     fac = 0.5
  ELSE
     fac = 0.2
  ENDIF

  tics = fac*10.**(magn)

END FUNCTION tics
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
INTEGER FUNCTION get_maxmon(sdate,edate)

 IMPLICIT NONE

 INTEGER :: sdate,edate

 INTEGER :: ydiff,mdiff

 ydiff = edate/10000 - sdate/10000
 mdiff = MOD(edate,10000)/100 - MOD(sdate,10000)/100 

 get_maxmon = ydiff * 12 + mdiff + 1

END FUNCTION get_maxmon
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
INTEGER FUNCTION get_maxtim(sdate,edate,int)

 ! Estimate data array length based on startdate, enddate
 ! and interval in hours

 IMPLICIT NONE

 INTEGER :: sdate,edate,int

 INTEGER :: ydiff,mdiff,ddiff

 ydiff = edate/10000 - sdate/10000
 mdiff = MOD(edate,10000)/100 - MOD(sdate,10000)/100 
 ddiff = MOD(edate,100) - MOD(sdate,100)

 
 get_maxtim = ydiff * 12 * 31 * 24 / int +  &
              mdiff      * 31 * 24 / int +  &
             (ddiff + 2)      * 24 / int 

             !WRITE(6,*)sdate,edate,ydiff,mdiff,ddiff,get_maxtim

END FUNCTION get_maxtim
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
INTEGER FUNCTION monincr(yyyymm,inc)

IMPLICIT NONE

 INTEGER :: yyyymm,inc

 INTEGER :: y1,m1

 integer :: yyyymmdd2,t2
 
 y1=yyyymm/100
 m1=MOD(yyyymm,100)

 call adddtg( yyyymm*100+25 , 0,inc*30*86400, yyyymmdd2, t2)
 monincr= yyyymmdd2/100

END FUNCTION monincr
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
INTEGER FUNCTION mondiff(sdate,edate)

IMPLICIT NONE

 INTEGER :: sdate,edate

 INTEGER :: ydiff,mdiff

 ydiff   = edate/100      - sdate/100
 mdiff   = MOD(edate,100) - MOD(sdate,100)
 mondiff = ydiff * 12 + mdiff 

END FUNCTION mondiff
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
INTEGER FUNCTION missing_index(date,ndates,step)

IMPLICIT NONE

! Input
INTEGER :: ndates,step
INTEGER :: date(ndates)

! Local
INTEGER :: y1,m1,d1,y2,m2,d2,diff,i,date1

INTEGER :: difdtg

 missing_index = 0

 y2 = date(1)/10000
 m2 = MOD(date(1),10000)/100
 d2 = MOD(date(1),100)

 DO i=2,ndates
 
    y1 = y2
    m1 = m2
    d1 = d2
    date1 = (y1*100 + m1)*100 + d1 
    y2 = date(i)/10000
    m2 = MOD(date(i),10000)/100
    d2 = MOD(date(i),100)
    
!EC    CALL daydiff(y2,m2,d2,y1,m1,d1,diff,ierr)
    diff = difdtg(date1,0,date(i),0)/86400.

    IF (diff.GT.step) THEN
       missing_index = i - 1
       EXIT
    ENDIF
 
 ENDDO

END FUNCTION missing_index
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
INTEGER FUNCTION check_this_file(lunin)

IMPLICIT NONE

INTEGER :: lunin,idum,i
REAL    :: rdum

READ(lunin,*)
READ(lunin,*)
READ(lunin,*)

DO i=1,37
   READ(lunin,*)idum,rdum
ENDDO

READ(lunin,*)idum,rdum

REWIND(lunin)

IF (ABS(rdum-6.).GT.1.e-6) THEN
 check_this_file = 1
ELSE
 check_this_file = 0
ENDIF

RETURN

END FUNCTION check_this_file
END MODULE functions
