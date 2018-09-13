MODULE functions
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
 CONTAINS
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
REAL FUNCTION PMSLCOM(PS,FIS,TS,NLON,NLAT)
!
      IMPLICIT NONE
!
      INTEGER NLON,NLAT
      REAL PS(NLON*NLAT),FIS(NLON*NLAT),    &
     &     TS(NLON*NLAT),                   &
     &     PSL(NLON*NLAT)
!
!-----------------------------------------------------------------------
!
!
      REAL TLAPSE,TSTAR,ALFA,TZERO,ARG,RAIR,GRAVIT
      INTEGER KHOR,NHOR
!
!-----------------------------------------------------------------------
!
!*   1.0   INITALIZE LOCAL CONSTANTS
!
      RAIR   =  287.04
      GRAVIT =  9.80665
      TLAPSE = 0.0065
      NHOR   = NLON*NLAT
!
!-----------------------------------------------------------------------
!
!*   2.0   START LOOP OVER ALL GRID-POINTS
!
      DO 2000 KHOR=1,NHOR
!
!*       2.1 SURFACE TEMPERATURE
!*           AND TEMP.REDUCTION FACTOR
!
      TSTAR = TS(KHOR)
!
      IF( TSTAR .LT. 255.0 ) THEN
!
!*       2.2  PREVENT TOO HIGH PRESSURES
!*            UNDER COLD TERRAIN
!
         TSTAR = 0.5*( TSTAR + 255. )
         ALFA = TLAPSE*RAIR/GRAVIT
!
      ELSE
!
!*       2.3  TRIAL EXTRA-POLATION OF SEA-LEVEL
!             TEMPERATURE
!
         TZERO = TSTAR + TLAPSE*FIS(KHOR)/GRAVIT
!
         IF( TZERO .GT. 290.5 ) THEN
!
!*       2.4 PREVENT TOO LOW PRESSURES UNDER
!            HOT TERRAIN
!
            IF( TSTAR.LE.290.5 ) THEN
               ALFA = RAIR*(290.5-TSTAR)/FIS(KHOR)
            ELSE
               ALFA = 0.
               TSTAR = 0.5*(290.5+TSTAR)
            ENDIF
!
         ELSE
!
!*       2.5 NORMAL TEMP.REDUCTION FACTOR
!
            ALFA = TLAPSE*RAIR/GRAVIT
!
         ENDIF
!
      ENDIF
!
!*        2.6 COMPUTE SEA-LEVEL PRESSURE
!*            END LOOP OVER GRIDPOINTS
!
      ARG = FIS(KHOR)/RAIR/TSTAR
      PSL(KHOR) = PS(KHOR)*                                             &
     &                 EXP( ARG*( 1. - 0.5*ALFA*ARG +         &
     &                            1./3.*(ALFA*ARG)**2 ) )
!
 2000 CONTINUE

      PMSLCOM=PSL(1)
!
!-----------------------------------------------------------------------
!
END FUNCTION PMSLCOM
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

  IF ( ABS(xmax-xmin) < TINY(xmin)) THEN
   tics = 1.0
   RETURN
  ENDIF

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
INTEGER FUNCTION get_maxtim(sdate,edate,fcint)

 ! Estimate data array length based on startdate, enddate
 ! and interval in hours

 IMPLICIT NONE

 INTEGER :: sdate,edate,fcint

 INTEGER :: ydiff,mdiff,ddiff

 ydiff = edate/10000 - sdate/10000
 mdiff = MOD(edate,10000)/100 - MOD(sdate,10000)/100 
 ddiff = MOD(edate,100) - MOD(sdate,100)

 
 get_maxtim = ydiff * 12 * 31 * 24 / fcint +  &
              mdiff      * 31 * 24 / fcint +  &
             (ddiff + 2)      * 24 / fcint 

 !WRITE(6,*)sdate,edate,fcint,ydiff,mdiff,ddiff,get_maxtim

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
    
    diff = difdtg(date1,0,date(i),0)/86400

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
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
LOGICAL FUNCTION do_you_like_me(check_blacklist,istnr,inpar)

 !
 ! Ulf Andrae, SMHI, 2018
 !
 ! Function for blacklisting of observations per station and type
 ! Expected format is
 ! stationid1
 ! PAR1 PAR2 .. PARN
 ! stationid2
 ! PAR1
 ! ...
 ! Use ALL to blacklist all parameters for a station
 !
 ! If memory becomes a problem for the lookup table we have to rewrite
 ! the search method
 !

 IMPLICIT NONE

 INTEGER, PARAMETER :: maxstnr=100000
 INTEGER, PARAMETER :: maxparbl=10
 INTEGER, PARAMETER :: iunit=22

 LOGICAL, INTENT(IN)          :: check_blacklist
 INTEGER, INTENT(IN)          :: istnr
 CHARACTER(LEN=*), INTENT(IN) :: inpar

 INTEGER :: ierr,stnr,ind,is,i

 CHARACTER(LEN=100) cpar
 CHARACTER(LEN=10), SAVE :: parlist(maxstnr,maxparbl)  = '#'

 LOGICAL, SAVE :: init_table = .TRUE.

 IF ( init_table ) THEN

  ierr = 99
  IF ( check_blacklist ) &
  OPEN(iunit,FILE='black.list',iostat=ierr)

  IF ( ierr /= 0 ) THEN
   init_table = .FALSE.
   RETURN
  ENDIF

  LOOP : DO
   READ(iunit,*,iostat=ierr)stnr
   IF ( ierr /= 0 ) EXIT LOOP
   IF ( stnr > maxstnr ) CALL abort
   WRITE(6,*)'Station:',stnr
   READ(iunit,'(100A)',iostat=ierr)cpar
   IF ( ierr /= 0 ) EXIT LOOP
   WRITE(6,*)' blacklist:',TRIM(cpar)

   is=1
   ind = INDEX(TRIM(cpar(is:)),' ')
   i=0
   DO WHILE ( ind /= 0 )
    i=i+1
    parlist(stnr,i) = cpar(is:is+ind-2)
    is = ind + is 
    ind = INDEX(TRIM(cpar(is:)),' ')
   ENDDO
   i=i+1
   parlist(stnr,i) = TRIM(cpar(is:))
  ENDDO LOOP

 init_table = .FALSE.

 ENDIF

 do_you_like_me = ( .NOT. & 
  & ( ANY(parlist(istnr,:) == TRIM(inpar) ) .OR. &
  &   parlist(istnr,1) == 'ALL' ) )

END FUNCTION do_you_like_me

END MODULE functions
