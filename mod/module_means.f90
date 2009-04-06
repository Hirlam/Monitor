MODULE means
 
 IMPLICIT NONE

 REAL, ALLOCATABLE :: mwork(:,:)

 CONTAINS

 !-----------------------------------------
 !-----------------------------------------
 !-----------------------------------------

 SUBROUTINE carefull_sumup(data,date,time,    &
               ntim,ndim,win,dlen,	          &
               data_min,data_max,data_ave,    &
               startdate,starttime,	          &
               sumup_tolerance,obint,         &
               err_ind,window_pos,do_average)
 !
 ! Apply win lenght averages on time series
 ! Check data piecevise and use the average only
 ! if we have data for sumup_tolerance % of the expected period
 !
 ! Ulf Andrae, ECMWF, 2003
 !

  IMPLICIT NONE

  ! Input/output
  REAL,    INTENT(INOUT) :: data(ndim)
  INTEGER, INTENT(INOUT) :: date(ndim),time(ndim)
  INTEGER, INTENT(IN )   :: ntim,ndim,win
  INTEGER, INTENT(OUT)   :: dlen
  REAL,    INTENT(OUT)   :: data_min,data_max,data_ave
  INTEGER, INTENT(IN )   :: startdate,starttime
  REAL,    INTENT(IN)    :: sumup_tolerance
  INTEGER, INTENT(IN)    :: obint
  REAL,    INTENT(IN)    :: err_ind
  INTEGER, INTENT(IN)    :: window_pos
  LOGICAL, INTENT(IN)    :: do_average

  ! Local
  INTEGER :: i,ii,j,jj,k,				&
             y1,m1,d1,h1,				&
             y2,m2,d2,h2,				&
             cy,cm,cd,ch,				&
             ierr,				&
             mindate,mintime,			&
             middate,midtime,			&
             maxdate,maxtime,dhour

  REAL    :: r_win,r_jj,wrk,rlen,tmp(ndim)
  INTEGER  :: ldate(ndim),ltime(ndim)

  INTEGER :: difdtg

  LOGICAL :: mask(ndim)
  
 !-------------------------------------------
   dhour = obint

   r_win = 1. / FLOAT(win / dhour )

   !
   ! Always start at the closest timeserie_win position to make sure we always make
   ! e.g. daily means over 00 - 23
   !
   
   k    = 0
   mask = .TRUE.
   ii   = MINVAL(MINLOC(MOD(time(1:ntim),win)))

   mindate = startdate
   mintime = starttime * 10000

   SUM_LOOP : DO 

      ! Find start and endpoint of window
      CALL adddtg(mindate,mintime,3600*(win-1),maxdate,maxtime)

      y1 = mindate/10000
      m1 = MOD(mindate/100,100)
      d1 = MOD(mindate,100)
      h1 = mintime/10000

      y2 = maxdate/10000
      m2 = MOD(maxdate/100,100)
      d2 = MOD(maxdate,100)
      h2 = maxtime/10000

      jj  = 0
      wrk = 0.

      !
      ! Accumulate data within this interval
      !

      FIND_TIMES : DO i=ii,ntim

         cy = date(i)/10000
         cm = MOD(date(i)/100,100)
         cd = MOD(date(i),100)
         ch = time(i)

!EC         CALL hourdiff(cy,cm,cd,ch,y1,m1,d1,h1,dhour,ierr)
         CALL hourdiff2(cy,cm,cd,ch,y1,m1,d1,h1,dhour,ierr)
         IF ( dhour < 0 ) CYCLE FIND_TIMES

!EC         CALL hourdiff(cy,cm,cd,ch,y2,m2,d2,h2,dhour,ierr)
         CALL hourdiff2(cy,cm,cd,ch,y2,m2,d2,h2,dhour,ierr) 
         IF ( dhour > 0 ) EXIT FIND_TIMES

         ! YES we found a suitable time
         IF(ABS(data(i)-err_ind).GT. 1.e-6) THEN
            wrk = wrk + data(i)
            jj  = jj  + 1
         ENDIF

         IF ( dhour == 0 ) EXIT FIND_TIMES

      ENDDO FIND_TIMES

      r_jj = FLOAT(jj)  * r_win
      IF(r_jj < sumup_tolerance .OR. jj == 0 ) THEN
        wrk        = err_ind
        jj         = 1
        mask(k+1)  = .FALSE.
      ENDIF
   
      k = k + 1
   
      r_jj = 1./FLOAT(jj)

      IF ( jj == 1 ) THEN
         middate = mindate
         midtime = mintime
      ELSE
         SELECT CASE( window_pos )
         CASE (-1)
            middate = mindate
            midtime = mintime
         CASE ( 0)
            CALL adddtg(mindate,mintime,3600*win/2,middate,midtime)
         CASE ( 1)
            CALL adddtg(mindate,mintime,3600*win,middate,midtime)
         CASE DEFAULT
            WRITE(6,*)'No such window_pos option', window_pos
            CALL abort
         END SELECT
      ENDIF

      tmp(k) = wrk
      IF ( do_average ) THEN
        tmp(k) = tmp(k) * r_jj
      ENDIF

      ldate(k) = middate
      ltime(k) = midtime / 10000

      IF( i > ntim ) EXIT

      CALL adddtg(maxdate,maxtime,3600,mindate,mintime)
      ii = i
      
   ENDDO SUM_LOOP

   dlen     = k
   data_min = 0.
   data_max = 1.
   data_ave = err_ind

   IF(dlen.EQ.0) RETURN

   IF(.NOT.ANY(mask(1:dlen))) RETURN

   rlen     = 1./COUNT(mask(1:dlen))

   data_min = MINVAL(tmp(1:dlen),mask(1:dlen))
   data_max = MAXVAL(tmp(1:dlen),mask(1:dlen))
   data_ave = SUM(tmp(1:dlen),mask(1:dlen)) * rlen

   ! Copy back

   data = tmp
   date = ldate
   time = ltime

   RETURN

 END SUBROUTINE carefull_sumup
 !-----------------------------------------
 !-----------------------------------------
 !-----------------------------------------
 SUBROUTINE sumup(data,ntim,npar,win,		&
                  dsta,dsto,dste,dlen)

  IMPLICIT NONE

  ! Input/output
  INTEGER, INTENT(IN ) :: ntim,npar,win
  INTEGER, INTENT(OUT) :: dsta,dsto,dste,dlen
  REAL, INTENT(INOUT) :: data(ntim,npar)

  ! Local
  INTEGER :: i,j,k
  REAL    :: r_win

 !---------------------------

   ALLOCATE(mwork(ntim,npar))

   r_win = 1./float(win)

   DO j=1,npar
      k = 0
      DO i=1,ntim,win
        IF (i+win-1.GT.ntim) EXIT 
         k = k + 1
         mwork(k,j) = SUM(data(i:i+win-1,j))
      ENDDO
   ENDDO
   dsta = 1
   dsto = k
   dste = win
   dlen = k

   data(dsta:dsto,:) = mwork(dsta:dsto,:) * r_win

   DEALLOCATE(mwork)

   RETURN

 END SUBROUTINE sumup
 !-----------------------------------------
 !-----------------------------------------
 !-----------------------------------------
 SUBROUTINE running_mean(data,ntim,npar,win,	&
                         dsta,dsto,dste,dlen)

  IMPLICIT NONE

  INTEGER :: i,j,ntim,npar,win,l_win,	&
             dsta,dsto,dste,dlen

  REAL :: data(ntim,npar),r_win

  ! ---------------------------------------

   ALLOCATE(mwork(ntim,npar))

   l_win = 2*win + 1
   r_win = 1./FLOAT(l_win)

   DO j=1,npar
   DO i=1+win,ntim-win
     mwork(i,j) = SUM(data(i-win:i+win,j))
   ENDDO
   ENDDO

   dsta = 1    + win
   dsto = ntim - win
   dlen = ntim - win*2
   dste = 1

   data(dsta:dsto,:) = mwork(dsta:dsto,:) * r_win

   DEALLOCATE(mwork)

  RETURN

 END SUBROUTINE running_mean
 !-----------------------------------------
 !-----------------------------------------
 !-----------------------------------------
 SUBROUTINE yearcycle(data,date,time,		&
                      ntim,npar,			&
                      dsta,dsto,dste,dlen)

  IMPLICIT NONE

  INTEGER :: i,j,ntim,npar,				&
             dsta,dsto,dste,dlen,		&
             yy,mm,dd,yday,ierr,		&
             date(ntim),time(ntim)

  INTEGER :: idate2yd,minday,maxday
  INTEGER :: idat2c, yday1

  REAL :: data(ntim,npar),nday(366)

  ! ---------------------------------------

   ALLOCATE(mwork(366,npar))

   nday  = 0.
   mwork = 0.

   minday = 400
   maxday = 0

   DO i=1,ntim
      yy =     date(i) / 10000
      mm = MOD(date(i) / 100,100)
      dd = MOD(date(i)      ,100)

!EC      yday=idate2yd(yy,mm,dd,ierr)
      yday1 = idat2c((100*yy+01)*100+01)
      yday  = idat2c(date(i)) - yday1 + 1

      mwork(yday,:) = mwork(yday,:) + data(i,:)
      nday(yday)    = nday(yday)    + 1.

      minday = MIN(minday,yday)
      maxday = MAX(maxday,yday)

   ENDDO

   dsta = 1
   dsto = maxday - minday + 1
   dste = 1
   dlen = dsto

   DO i=dsta,dsto

      CALL GREGOR (i+minday-1 + yday1 -1,yy,mm,dd)
!EC      CALL YD2DATE  (i+minday-1,yy,mm,dd,ierr)
      date(i)=yy*10000 + mm*100 + dd
      time(i)=00
   ENDDO

   WHERE(nday.LT.1.)
    nday = 1.
   ENDWHERE

   DO i=1,npar
      data(dsta:dsto,i) = ( mwork(minday:maxday,i) / nday(minday:maxday)) 
   ENDDO

   DEALLOCATE(mwork)

  RETURN

 END SUBROUTINE yearcycle
 !-----------------------------------------
 !-----------------------------------------
 !-----------------------------------------
END MODULE means
