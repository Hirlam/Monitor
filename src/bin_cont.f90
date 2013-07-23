SUBROUTINE bin_cont(xval,yval,nobs,  &
                    bin_min_yx,      &
                    bin_max_yx,      &
                    nlevels,level,   &
                    levcheck,        &
                    nbin,sbin)

 USE types, ONLY : scatter_bin

 IMPLICIT NONE

 !
 ! Inspired by a routine found at ECMWF ...
 ! Thanks to Lars Isaksen, Christina Köpken et.al.
 !

 ! Input
 INTEGER,INTENT(IN) :: nlevels,nobs,nbin

 REAL,    INTENT(IN) :: xval(nobs),yval(nobs),  &
                        bin_min_yx(2),          &
                        bin_max_yx(2)

 REAL, INTENT(INOUT) :: level(nlevels)

 LOGICAL, INTENT(OUT) :: levcheck(nlevels)

 TYPE(scatter_bin) :: sbin

 ! Local
  
  INTEGER :: nbinx,nbiny,               &
             ibin_x,ibin_y,             &
             i,l,ii,numrej
  REAL                   ::             &
    bin_inc_yx(2)=(/1,1/),              & ! inc for x and y axis
    fac,maxobs,                         &
    sumy,sumx,sumyy,sumxx,sumxy,sumxy2, &
    xhelp,yhelp,xpt,xptd,rv,sid,        &
    XMEAN, YMEAN, STDEVX, STDEVY, BIAS, RMSE, STDEVD, CORR

  REAL, POINTER :: biny(:),binx(:),array(:,:)

!---------------------------------------------------------------------

  level = 1.
  ! Set bin increments
  bin_inc_yx(1) = (bin_max_yx(1) -bin_min_yx(1)) / FLOAT(nbin)
  bin_inc_yx(2) = (bin_max_yx(2) -bin_min_yx(2)) / FLOAT(nbin)

  nbiny=INT( (bin_max_yx(1)-bin_min_yx(1))/REAL(bin_inc_yx(1)) )
  nbinx=INT( (bin_max_yx(2)-bin_min_yx(2))/REAL(bin_inc_yx(2)) )

  nbiny=MAX(2,nbiny)
  nbinx=MAX(2,nbinx)

  ! Set pointer ( convinience )
  biny  => sbin%biny
  binx  => sbin%binx
  array => sbin%array

  array = 0.
  biny(0) = bin_min_yx(1)
  binx(0) = bin_min_yx(2)
  DO i=1,nbiny
    biny(i) = biny(i - 1) + bin_inc_yx(1)
  ENDDO
  DO i=1,nbinx
    binx(i) = binx(i - 1) + bin_inc_yx(2)
  ENDDO

!-------------------------------------------------------------------------------
! Bin the data
!-------------------------------------------------------------------------------
  sumy   = 0.
  sumx   = 0.
  sumxx  = 0.
  sumyy  = 0.
  sumxy  = 0.
  sumxy2 = 0.

  numrej = 0

  DO i=1,nobs

    IF ( xval(i) < bin_min_yx(2) .OR. &
      xval(i) > bin_max_yx(2) .OR. &
      yval(i) < bin_min_yx(1) .OR. &
      yval(i) > bin_max_yx(1)      ) THEN
      numrej = numrej + 1 
      CYCLE
    ENDIF

    ibin_x = int((xval(i) - binx(0))/bin_inc_yx(2)) + 1
    ibin_x = MAX(1,min(nbinx,ibin_x))
    ibin_y = int((yval(i) - biny(0))/bin_inc_yx(1)) + 1
    ibin_y = MAX(1,min(nbiny,ibin_y))
   
    array(ibin_x,ibin_y) = array(ibin_x,ibin_y) + 1.

    sumy   = sumy   + yval(i)
    sumyy  = sumyy  + yval(i)*yval(i)
    sumx   = sumx   + xval(i)
    sumxx  = sumxx  + xval(i)*xval(i)
    sumxy  = sumxy  + xval(i)*yval(i)
    sumxy2 = sumxy2 + (xval(i)-yval(i))*(xval(i)-yval(i))

  ENDDO

  ! Set obs count scale
  level(1)=1.
  level(2)=2.
  level(3)=5.
  ii = 3

  maxobs=MAXVAL(array)

  DO i=4,nlevels,4
    fac = 10.**((i)/4)
    level(i)=1*fac
    ii = i 
    IF(level(ii).GT.maxobs) EXIT
    ii = i+1
    level(i+1)=2.5*fac
    IF(level(ii).GT.maxobs) EXIT
    ii = i+2
    level(ii)=5*fac
    IF(level(ii).GT.maxobs) EXIT
    ii = i+3
    level(ii)=7.5*fac
    IF(level(ii).GT.maxobs) EXIT
  ENDDO

  XMEAN  = 0.
  YMEAN  = 0.
  STDEVX = 0.
  STDEVY = 0.
  BIAS   = 0.
  RMSE   = 0.
  STDEVD = 0.
  CORR   = 0.

  IF (nobs.GT.1) THEN

    XPT  = REAL(NOBS)
    XPTD = 1./XPT

    YMEAN = SUMY*XPTD
    XMEAN = SUMX*XPTD
    BIAS = YMEAN-XMEAN
    RMSE = SQRT(SUMXY2*XPTD)

    XHELP = XPT*SUMXX-SUMX*SUMX
    YHELP = XPT*SUMYY-SUMY*SUMY

    STDEVX = ABS(XHELP/(XPT*(XPT-1.)))
    STDEVX = SQRT(STDEVX)
    STDEVY = ABS(YHELP/(XPT*(XPT-1.)))
    STDEVY = SQRT(STDEVY)

    IF (XMEAN.NE.0.) SID    = STDEVD/XMEAN
    IF (XHELP.GT.0.) RV     = 1. - XPT*SUMXY2/XHELP
    IF (STDEVX.NE.0. .AND. STDEVY.NE.0.) &
        CORR   = (SUMXY-XPT*XMEAN*YMEAN)/(STDEVX*STDEVY*(XPT-1.))

!   WRITE(lunout,*)'------------------------------------------------'
!   WRITE(lunout,'(5X,A9,X,A9)')'Mean   ','Stdv   '
!   WRITE(lunout,'(X,A3,X,2(f8.3,X))')'MOD',YMEAN,STDEVY
!   WRITE(lunout,'(X,A3,X,2(f8.3,X))')'OBS',XMEAN,STDEVX
!   WRITE(lunout,*)'Bias, Rmse, Corr:' , BIAS, RMSE,  CORR
!   WRITE(lunout,*)'------------------------------------------------'

  ENDIF

  levcheck(:) = .FALSE.
  DO l=2,nlevels

   IF ( level(l) <= level(l-1)) EXIT

   levcheck(l) = &
   ANY(array(:,:) >= level(l-1) .AND.  &
       array(:,:) <  level(l  )       )

  ENDDO
 
  ! Clear memory
  NULLIFY(biny,binx,array)

  RETURN

END SUBROUTINE bin_cont
