SUBROUTINE bin_cont(lunout,xval,yval,nobs,          &
                    minx,maxx,miny,maxy,            &
                    nlevels,fname,                  &
                    heading1,heading2,              &
                    heading3,heading4,              &
                    axist1,axist2)

 IMPLICIT NONE

 !
 ! Inspired by a routine found at ECMWF ...
 ! Thanks to Lars Isaksen, Christina Köpken et.al.
 !

 ! Input
 INTEGER,INTENT(IN) :: lunout,nlevels,nobs

 REAL,    INTENT(IN) :: xval(nobs),yval(nobs),	&
                        minx,miny,maxx,maxy
 CHARACTER(LEN=*)    :: fname,                  &
                        heading1,heading2,      &
                        heading3,heading4,      &
                        axist1,axist2

 ! Local
  
  INTEGER :: nbinx,nbiny,		&
             ibin_x,ibin_y,		&
             i,j,l,ii,numrej,sunit
  REAL                   :: 		&
    bin_min_yx(2)=(/0.,0./)     ,	& ! min for x and y axis
    bin_max_yx(2)=(/100.,100./) ,	& ! max for x and y axis
    bin_inc_yx(2)=(/1,1/),    		& ! inc for x and y axis
    level(nlevels),			&
    fac,magn,maxobs,		&
    sumy,sumx,sumyy,sumxx,sumxy,sumxy2,	&
    xhelp,yhelp,xpt,xptd,rv,sid,        &
    XMEAN, YMEAN, STDEVX, STDEVY, BIAS, RMSE, STDEVD, CORR

  REAL, ALLOCATABLE :: biny(:),binx(:),array(:,:)

  CHARACTER(LEN=100) :: sname = ''
  CHARACTER(LEN=  2) :: cnum  = ''

  LOGICAL :: print_data

!---------------------------------------------------------------------

  level = 1.
  ! Set bin tic and limits
  bin_max_yx(1) = maxy
  bin_min_yx(1) = miny
  bin_max_yx(2) = maxx
  bin_min_yx(2) = minx
  bin_inc_yx(1) = (bin_max_yx(1) -bin_min_yx(1)) / 100.
  bin_inc_yx(2) = (bin_max_yx(2) -bin_min_yx(2)) / 100.

  nbiny=INT( (bin_max_yx(1)-bin_min_yx(1))/REAL(bin_inc_yx(1)) )
  nbinx=INT( (bin_max_yx(2)-bin_min_yx(2))/REAL(bin_inc_yx(2)) )

  nbiny=MAX(2,nbiny)
  nbinx=MAX(2,nbinx)

  ALLOCATE(biny(0:nbiny))
  ALLOCATE(binx(0:nbinx))
  ALLOCATE(array(nbinx,nbiny))   ; array = 0.

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

       IF ( xval(i) < minx .OR. xval(i) > maxx .OR.     &
            yval(i) < miny .OR. yval(i) > maxy      ) THEN
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
     IF(level(i).GT.maxobs) EXIT
     level(i+1)=2.5*fac
     ii = i+1
     IF(level(i+1).GT.maxobs) EXIT
     level(i+2)=5*fac
     ii = i+2
     IF(level(i+2).GT.maxobs) EXIT
     level(i+3)=7.5*fac
     ii = i+3
     IF(level(i+3).GT.maxobs) EXIT
  ENDDO

  XMEAN  = 0.
  YMEAN  = 0.
  STDEVX = 0.
  STDEVY = 0.
  BIAS   = 0.
  RMSE   = 0.
  STDEVD = 0.
  CORR   = 0.

  !IF (nobs.LT.1) RETURN
  XPT  = REAL(nobs)
  XPTD = 1./XPT

  YMEAN = SUMY*XPTD
  XMEAN = SUMX*XPTD
  BIAS = YMEAN-XMEAN
  RMSE = SQRT(SUMXY2*XPTD)

  IF (nobs.GT.1) THEN

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

  ! Print out the different points
  WRITE(lunout,'(2A)')'#HEADING_1 ',TRIM(heading1)
  WRITE(lunout,'(2A)')'#HEADING_2 ',TRIM(heading2)
  WRITE(lunout,'(2A)')'#HEADING_3 ',TRIM(heading3)
  WRITE(lunout,'(2A)')'#HEADING_4 ',TRIM(heading4)

  WRITE(lunout,'(2A)')'#YLABEL ',TRIM(axist1)
  WRITE(lunout,'(2A)')'#XLABEL ',TRIM(axist2)
  WRITE(lunout,*)'#XMIN ',minx
  WRITE(lunout,*)'#XMAX ',maxx
  WRITE(lunout,*)'#YMIN ',miny
  WRITE(lunout,*)'#YMAX ',maxy


  DO l=2,nlevels

   IF ( level(l) <= level(l-1)) EXIT
   WRITE(cnum,'(I2.2)')l
   sname = TRIM(fname)//'_'//cnum

   WRITE(lunout,'(A,X,A,I10)')'#SLEVEL ',cnum,NINT(level(l))

   OPEN(UNIT=37,FILE=sname)
   print_data=.FALSE.
   DO j=1,nbiny
     DO i=1,nbinx
      IF (array(i,j) >= level(l-1) .AND.  &
          array(i,j) <  level(l  )      ) THEN
          WRITE(37,*)binx(i-1),biny(j-1)
          print_data=.TRUE.
      ENDIF
     ENDDO
   ENDDO
   IF(.NOT.print_data) WRITE(37,*) "-999.   -999."
   CLOSE(37)
  ENDDO

  WRITE(lunout,'(A)')'#END'

END SUBROUTINE bin_cont
