SUBROUTINE calc_corr(nobs,xval,yval,            &
                     xmean,ymean,stdevx,stdevy, &
                     bias,rmse,stdv,corr)

 IMPLICIT NONE

 INTEGER, INTENT(IN ) :: nobs
 REAL,    INTENT(IN ) :: xval(nobs),yval(nobs)
 REAL,    INTENT(OUT) :: xmean,ymean,          &
                         stdevx,stdevy,        &
                         bias,rmse,stdv,corr

 ! Local

 INTEGER :: i
 REAL    :: sumy,sumx,sumxx,sumyy,             &
            sumxy,sumxy2,stdevd,               &
            xpt,xptd,xhelp,yhelp,sid,rv
!-------------------------------------------------------------------------------
! Bin the data
!-------------------------------------------------------------------------------
    sumy   = 0.
    sumx   = 0.
    sumxx  = 0.
    sumyy  = 0.
    sumxy  = 0.
    sumxy2 = 0.

    DO i=1,nobs

       sumy   = sumy   + yval(i)
       sumyy  = sumyy  + yval(i)*yval(i)
       sumx   = sumx   + xval(i)
       sumxx  = sumxx  + xval(i)*xval(i)
       sumxy  = sumxy  + xval(i)*yval(i)
       sumxy2 = sumxy2 + (xval(i)-yval(i))*(xval(i)-yval(i))

    ENDDO

  XMEAN  = 0.
  YMEAN  = 0.
  STDEVX = 0.
  STDEVY = 0.
  BIAS   = 0.
  RMSE   = 0.
  STDEVD = 0.
  CORR   = 0.

  XPT  = REAL(nobs)
  XPTD = 1./XPT

  YMEAN = SUMY*XPTD
  XMEAN = SUMX*XPTD
  BIAS = YMEAN-XMEAN
  STDV = SQRT(SUMXY2*XPTD-BIAS**2)
  RMSE = SQRT(SUMXY2*XPTD)

    XHELP = XPT*SUMXX-SUMX*SUMX
    YHELP = XPT*SUMYY-SUMY*SUMY

    STDEVX = ABS(XHELP/(XPT*(XPT-1.)))
    STDEVX = SQRT(STDEVX)
    STDEVY = ABS(YHELP/(XPT*(XPT-1.)))
    STDEVY = SQRT(STDEVY)

    IF (XMEAN /= 0.) SID    = STDEVD/XMEAN
    IF (XHELP >  0.) RV     = 1. - XPT*SUMXY2/XHELP
    IF (STDEVX /= 0. .AND. STDEVY /= 0.) &
        CORR   = (SUMXY-XPT*XMEAN*YMEAN)/(STDEVX*STDEVY*(XPT-1.))

 RETURN
END SUBROUTINE 
