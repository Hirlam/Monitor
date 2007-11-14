SUBROUTINE bin_scat(xval,yval,nobs,          &
                    minx,maxx,miny,maxy,     &
                    nlevels,l_corr,ctitle,   &
                    obsname,yname,xname,     &
                    wtext3,wtext4)

USE functions

IMPLICIT NONE

 ! Parameters
 !INTEGER, PARAMETER :: nlevels      = 23
 INTEGER,INTENT(IN) :: nlevels
 LOGICAL, PARAMETER :: l_level_list = .TRUE.

 ! Input
 INTEGER, INTENT(IN) :: nobs
 REAL,    INTENT(IN) :: xval(nobs),yval(nobs),	&
                        minx,miny,maxx,maxy
 LOGICAL, INTENT(IN) :: l_corr
 CHARACTER(LEN=*), INTENT(IN) :: ctitle,obsname,xname,yname,wtext3,wtext4

 ! Local
  
  INTEGER :: nbinx,nbiny,		&
             ibin_x,ibin_y,		&
             i,ii,numrej
  REAL                   :: 		&
    bin_min_yx(2)=(/0.,0./)     ,	& ! min for x and y axis
    bin_max_yx(2)=(/100.,100./) ,	& ! max for x and y axis
    bin_inc_yx(2)=(/1,1/),    		& ! inc for x and y axis
    bin_tic_yx(2)=(/100,100/),		& ! tick frequency for x and y axis
    level(nlevels),			&
    fac,magn,magn_diff,maxobs,		&
    sumy,sumx,sumyy,sumxx,sumxy,sumxy2,	&
    XMEAN, YMEAN, STDEVX, STDEVY, BIAS, RMSE, STDEVD, CORR

  REAL, ALLOCATABLE :: biny(:),binx(:),array(:,:)


!---------------------------------------------------------------------

  level = 1.
  ! Set bin tic and limits
  bin_tic_yx(1) = tics(miny,maxy)
  bin_max_yx(1) = maxy
  bin_min_yx(1) = miny
  bin_tic_yx(2) = tics(minx,maxx)
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
  
  CALL PNEW('SUPER_PAGE')

  CALL mag_scatter(array, &
                   nbiny,biny,bin_inc_yx(1),bin_tic_yx(1),&
                   bin_min_yx(1),bin_max_yx(1)           ,&
                   nbinx,binx,bin_inc_yx(2),bin_tic_yx(2),&
                   bin_min_yx(2),bin_max_yx(2)           ,&
                   l_corr,l_level_list,ii,level(1:ii)    ,&
                   sumy,sumx,sumyy,sumxx,sumxy,sumxy2    ,&
                   numrej,nobs                           ,&
                   XMEAN, YMEAN, STDEVX, STDEVY, BIAS    ,&
                   RMSE, STDEVD,CORR                     ,&
                   ctitle,obsname,yname,xname,wtext3,wtext4)


  ! Clear memory

  DEALLOCATE(biny)
  DEALLOCATE(binx)
  DEALLOCATE(array)

  RETURN
END SUBROUTINE bin_scat

!-------------------------------------------------------------------------------
SUBROUTINE mag_scatter(array_plot,nbiny,biny,inc,inc_axis,bin_min,bin_max,     &
                       nbinx,binx,incx,inc_axisx,bin_minx,bin_maxx,            &
                       l_corr,l_level_list,nlevels,level,                      &
                       sumy,sumx,sumyy,sumxx,sumxy,sumxy2,                     &         
                       numrej,nobs,                                            &
                       XMEAN, YMEAN, STDEVX, STDEVY, BIAS, RMSE, STDEVD, CORR, &
                       ctitle,obsname,yname,xname,wtext3,wtext4)

!-------------------------------------------------------------------------------
!
! Description:
! plots a scatter plot
!
!   Namelist parameters used:
!
! Method:
!
! Author : Antje Dethof
!
! History:
! Version    cdate       Name
! ---------- ---------- ----
! Initial    20010420   Antje Dethof
!===============================================================================
!
! Declarations:
!-------------------------------------------------------------------------------
! Modules used:
!-------------------------------------------------------------------------------
!===============================================================================

 USE DATA, ONLY : lunstat

IMPLICIT NONE

  INTEGER, INTENT(IN) :: &
    nbinx,nbiny

  INTEGER, INTENT(INOUT) :: &
    nlevels,nobs,numrej

  REAL, INTENT(IN) :: &
    biny(0:nbiny),inc,bin_min,bin_max ,&
    binx(0:nbinx),incx,bin_minx,bin_maxx 

  REAL, INTENT(INOUT) :: &
    inc_axis  ,&
    inc_axisx  ,&
    level(nlevels) ,&
    array_plot(nbinx,nbiny),	&
                       sumy,sumx,sumyy,sumxx,sumxy,sumxy2
  
  LOGICAL, INTENT(INOUT) :: &
    l_level_list,l_corr
  
  INTEGER ::  icontlev,jx,jy,isymbol

  REAL :: zxx(nbinx),zyy(nbinx),zmx
  REAL :: zbnx,zbn,zmn,zbny,zymxmn,zymxsd,zxmn,zymn,zxsd,zysd,zxypc
  REAL :: zmax,znum,zlvl1
  REAL :: XHELP,XPT,XPTD, YHELP, &
    XMEAN, YMEAN, STDEVX, STDEVY, BIAS, RMSE, STDEVD, CORR, rv, sid
  LOGICAL :: mask(nbinx,nbiny)

  CHARACTER (LEN=80) :: cline
  CHARACTER (LEN=*) :: ctitle,obsname,yname,xname,wtext3,wtext4

!-------------------------------------------------------------------------------
! Begin Subroutine mag_scatter
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Section 0: Set up super_page layout
!-------------------------------------------------------------------------------
  CALL defsp(4,'H',1,1)

!-------------------------------------------------------------------------------
! Section 1: First page = scatter diagram on left
!-------------------------------------------------------------------------------
  CALL PSETC ('PAGE_ID_LINE', 'ON')
  call psetr('PAGE_X_LENGTH',20.0)
  call psetr('PAGE_Y_LENGTH',21.0)
  call psetr('PAGE_X_POSITION',0.0)
  call psetr('PAGE_Y_POSITION',0.0)
  CALL PSETC ('PAGE_FRAME' ,'off')
  CALL PSETC ('PAGE_FRAME_COLOUR' ,'BLACK')

  call psetr('SUBPAGE_X_LENGTH',15.0)
  call psetr('SUBPAGE_Y_LENGTH',14.0)
  call psetr('SUBPAGE_X_POSITION',3.0)
  call psetr('SUBPAGE_Y_POSITION',3.0)
  CALL PSETC ('SUBPAGE_FRAME' ,'on')
  CALL PSETC ('SUBPAGE_FRAME_COLOUR' ,'BLACK')
  CALL psetc('TEXT_QUALITY','HIGH')
 call psetc('SUBPAGE_MAP_PROJECTION','NONE')

 CALL psetc('TEXT_QUALITY','HIGH')
 !CALL psetc('PAGE_ID_LINE_USER_TEXT','Copyright ECMWF')
 CALL psetc('PAGE_ID_LINE_SYSTEM_PLOT','ON')
 CALL psetc('PAGE_ID_LINE_ERRORS_PLOT','OFF')
 CALL psetc('PAGE_ID_LINE_DATE_PLOT','ON')
 CALL psetc('PAGE_ID_LINE_LOGO_PLOT','OFF')
 CALL psetc('PAGE_ID_LINE_QUALITY','HIGH')

!-------------------------------------------------------------------------------
! Section 1.1: Plot scatter diagram
!-------------------------------------------------------------------------------
  CALL pltcnt(array_plot,nbinx,nbiny,l_level_list,nlevels,level)

!-------------------------------------------------------------------------------
! Section 1.2: Plot axis
!-------------------------------------------------------------------------------
  CALL susqpl( bin_minx,bin_maxx, inc_axisx, xname &
             , bin_min, bin_max,  inc_axis,  yname )

!-------------------------------------------------------------------------------
! Section 1.3: Plot title 
!-------------------------------------------------------------------------------
       ! Set title text
         CALL PRESET ('TEXT_JUSTIFICATION')
         CALL PRESET ('TEXT_MODE')
         CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 5.0)
         CALL PSETC('TEXT_LINE_1',ctitle)
         CALL PSETC('TEXT_LINE_2',obsname)

         IF (wtext3 /= ' ')  THEN
            CALL PSETC('TEXT_LINE_3',wtext3)
            CALL PSETI('TEXT_LINE_COUNT',3)
            IF (wtext4 /= ' ')  THEN
               CALL PSETC('TEXT_LINE_4',wtext4)
               CALL PSETI('TEXT_LINE_COUNT',4)
            ENDIF
         ELSE
            CALL PSETI('TEXT_LINE_COUNT',2)
         ENDIF
         
         CALL PTEXT
!-------------------------------------------------------------------------------
! Section 1.4: Plot 0 or 45 degree line (depending on y-scale min/max values)
!-------------------------------------------------------------------------------
  zxx(1) = bin_minx
  zyy(1) = 0.
  zxx(2) = bin_maxx
  zyy(2) = 0.
  IF((ABS(bin_min - bin_minx) <1.e-6 ).AND.   &
     (ABS(bin_max - bin_maxx) <1.e-6 )) THEN
     zyy(1) = bin_min
     zyy(2) = bin_max
  ENDIF
  CALL curvef( zxx, zyy, 2, 2, 1 )

!-------------------------------------------------------------------------------
! Section 1.5: Plot symbols for mean Y at every X
!-------------------------------------------------------------------------------
     IF (.FALSE.) THEN
     zmx = real(nbinx)
     DO jx=1,nbinx
       zbnx= binx(0) + real(jx-.5)*incx
       zyy(jx) = 0.
       zxx(jx) = zbnx
       zbn = 0.
       zmn = 0.
       DO jy=1,nbiny
         zbn = zbn + array_plot(jx,jy)
         zbny= biny(0) + real(jy-.5)*inc
         zmn = zmn + array_plot(jx,jy)*zbny
       ENDDO
       IF( zbn.gt.0.5 )zyy(jx) = zmn/zbn
     ENDDO

     isymbol = 15
     CALL pltsym( zxx, zyy, nbinx , isymbol, "black                           " )
     ENDIF

  zmax = maxval(array_plot)

  XMEAN  = 0.
  YMEAN  = 0.
  STDEVX = 0.
  STDEVY = 0.
  BIAS   = 0.
  RMSE   = 0.
  STDEVD = 0.
  CORR   = 0.

  IF (nobs.LT.1) RETURN
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

  ENDIF
  IF ( .FALSE. ) THEN
  IF ( index(ctitle,'Difference') <= 0 ) THEN
     WRITE(lunstat,*)'------------------------------------------------'
     WRITE(lunstat,*)TRIM(ctitle(17:)),' ',TRIM(obsname),' ',TRIM(yname)
     WRITE(lunstat,'(5X,A9,X,A9)')'Mean   ','Stdv   '
     WRITE(lunstat,'(X,A3,X,2(f8.3,X))')'MOD',YMEAN,STDEVY
     WRITE(lunstat,'(X,A3,X,2(f8.3,X))')'OBS',XMEAN,STDEVX
     WRITE(lunstat,*)'Bias, Rmse, Corr:' , BIAS, RMSE,  CORR
     WRITE(lunstat,*)'------------------------------------------------'
  ENDIF
  ENDIF
!-------------------------------------------------------------------------------
! Section 2.2: Plot statistics
!-------------------------------------------------------------------------------
! Section 2.2a: Set up new page and textbox
!-------------------------------------------------------------------------------
  CALL PNEW('PAGE')
  CALL PSETR ('PAGE_X_GAP' ,0.)
  CALL PSETR ('PAGE_Y_GAP' ,0.)

  CALL PSETR ('PAGE_X_POSITION' ,19.)
  CALL PSETR ('PAGE_Y_POSITION' ,3.)
  CALL PSETR ('PAGE_X_LENGTH' ,9.)
  CALL PSETR ('PAGE_Y_LENGTH' ,17.)

  CALL PSETC ('PAGE_FRAME' ,'off')
  CALL PSETC ('PAGE_FRAME_COLOUR' ,'BLACK')
  CALL PSETC ('PAGE_ID_LINE', 'off')

  CALL PSETR ('SUBPAGE_X_POSITION' ,0.)
  CALL PSETR ('SUBPAGE_Y_POSITION' ,0.)
  CALL PSETR ('SUBPAGE_X_LENGTH' ,9.)
  CALL PSETR ('SUBPAGE_Y_LENGTH' ,17.)
  CALL PSETC ('SUBPAGE_FRAME' ,'off')
  CALL PSETC ('SUBPAGE_FRAME_COLOUR' ,'BLACK')

  CALL PSETC ('TEXT_COLOUR', 'BLACK')
  CALL PSETC ('TEXT_MODE', 'POSITIONAL')
  CALL PSETC ('TEXT_JUSTIFICATION', 'LEFT')
  CALL PSETC ('TEXT_BORDER', 'OFF')

!-------------------------------------------------------------------------------
! Section 2.2b: Write text
!-------------------------------------------------------------------------------

! First box
!------------
  CALL PSETR ('TEXT_BOX_Y_POSITION', 8.5)
  CALL PSETR ('TEXT_BOX_X_POSITION', 1.0)
  CALL PSETR ('TEXT_BOX_X_LENGTH', 8.)
  CALL PSETR ('TEXT_BOX_Y_LENGTH', 5.0)

  CALL PSETI ('TEXT_LINE_COUNT', 1)

  IF ( numrej == 0 ) THEN
     WRITE(cline,'(''Obs = '',I7)')nobs
  ELSE
     WRITE(cline,'(''Obs (outside/total)= '',I7,''/'',I7)')numrej,nobs
  ENDIF

  CALL PSETC ('TEXT_LINE_1', TRIM(cline))
  CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 5.0)
  CALL PSETC ('TEXT_QUALITY', 'HIGH')
  CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_1', 1.5)
  CALL ptext

     CALL PSETC('TEXT_BORDER', 'OFF')
     CALL PSETR ('TEXT_BOX_Y_POSITION', 5.)
     CALL PSETR ('TEXT_BOX_X_POSITION', 1.)
     CALL PSETR ('TEXT_BOX_X_LENGTH', 8.)
     CALL PSETR ('TEXT_BOX_Y_LENGTH', 3.0)
     CALL PSETI ('TEXT_LINE_COUNT', 5)
     WRITE(cline,'(''y mean = '',F6.1,''     y stdev = '',F6.1)') YMEAN, STDEVY
     CALL PSETC ('TEXT_LINE_1', TRIM(cline))
     WRITE(cline,'(''x mean = '',F6.1,''     x stdev = '',F6.1)') XMEAN, STDEVX
     CALL PSETC ('TEXT_LINE_2', TRIM(cline))
     IF(l_corr) THEN
        WRITE(cline,'(''BIAS (y-x) = '',F6.2)') BIAS
        CALL PSETC ('TEXT_LINE_3', TRIM(cline))
        WRITE(cline,'(''RMS = '',F6.2)') RMSE
        CALL PSETC ('TEXT_LINE_4', TRIM(cline))
        WRITE(cline,'(''corr. coef. = '',F6.3)') CORR
        CALL PSETC ('TEXT_LINE_5', TRIM(cline))
     ELSE
      CALL PRESET('TEXT_LINE_5')
      CALL PRESET('TEXT_LINE_4')
      CALL PRESET('TEXT_LINE_3')
      CALL PSETI ('TEXT_LINE_COUNT', 2)
     ENDIF

     CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 5.0)
     CALL PSETC ('TEXT_QUALITY', 'HIGH')
     CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_1', 1.0)
     CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_2', 1.0)
     CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_3', 1.0)
     CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_4', 1.0)
     CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_5', 1.0)
     CALL PTEXT

RETURN
!-------------------------------------------------------------------------------
END SUBROUTINE mag_scatter

SUBROUTINE defsp(kfmt,cdori,knxp,knyp)
!-------------------------------------------------------------------------------

!     This subroutine sets magics parameters in order to define
!     a super page according to the selected arguments

!     arguments
!     ---------
!     kfmt: format  3 for a3, 4 for a4
!     cdori: orientation 'v' for vertical h' 'for horizontal
!     knxp: nuber of required subpages in x direction
!     knyp: nuber of required subpages in y direction

!     Author: Alex Rubli (ECMWF)
!     Recoded by Lars Isaksen (ECMWF)
!----------------------------------------------------------------------

      implicit none

      integer kfmt,knxp,knyp
      character cdori*1

      real :: zsppl(3:4) = (/42.0,29.7/)
      real :: zspps(3:4) = (/29.7,21.0/)
      real zspxl,zspyl,zsxl,zsyl,zpxl,zpyl

!     Reset page parameters


      call preset('SUPER_PAGE_X_LENGTH')
      call preset('SUPER_PAGE_Y_LENGTH')
      call preset('PAGE_X_LENGTH')
      call preset('PAGE_Y_LENGTH')
      call preset('SUBPAGE_X_LENGTH')
      call preset('SUBPAGE_Y_LENGTH')
      call preset('PAGE_X_POSITION')
      call preset('PAGE_Y_POSITION')
      call preset('SUBPAGE_X_POSITION')
      call preset('SUBPAGE_Y_POSITION')

      CALL PSETC('PAGE_ID_LINE','OFF')
      CALL PSETC('PAGE_FRAME','OFF')
      CALL PSETC('PAGE_FRAME_COLOUR','BLACK')


!     Set parameters for vertical or horizontal page format

      if(cdori.eq.'V')then
        call psetr('SUPER_PAGE_Y_LENGTH',zsppl(kfmt))
        call psetr('SUPER_PAGE_X_LENGTH',zspps(kfmt))
      else
        call psetr('SUPER_PAGE_X_LENGTH',zsppl(kfmt))
        call psetr('SUPER_PAGE_Y_LENGTH',zspps(kfmt))
      endif
      call penqr('SUPER_PAGE_X_LENGTH',zspxl)
      call penqr('SUPER_PAGE_Y_LENGTH',zspyl)



RETURN
!-------------------------------------------------------------------------------
END SUBROUTINE defsp


SUBROUTINE susqpl(pxn,pxx,pxt,cdhtitlx,pyn,pyx,pyt,cdhtitly)
!-------------------------------------------------------------------------------

      implicit none

      real pxn,pxx,pxt,pyn,pyx,pyt
      character (LEN=*),INTENT(IN):: cdhtitlx,cdhtitly

!     Define super page and page layout

!     Horizontal axis

      call psetc ('AXIS_ORIENTATION','HORIZONTAL')
      call psetc ('AXIS_POSITION','BOTTOM')
      call psetc ('AXIS_TITLE_QUALITY','MEDIUM')
      call psetc ('AXIS_TICK_LABEL','ON')
      call psetc ('AXIS_TICK','ON')
      call psetr ('AXIS_MIN_VALUE',pxn)
      call psetr ('AXIS_MAX_VALUE',pxx)
      call psetr ('AXIS_TICK_INTERVAL', pxt)
      CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',1)
      call psetc ('AXIS_TITLE','ON')
      call psetc ('AXIS_TITLE_TEXT',cdhtitlx)
      call psetc ('AXIS_GRID','ON')
      call pseti ('AXIS_GRID_THICKNESS',1)
      call psetc ('AXIS_GRID_LINE_STYLE','DASH')
      call paxis

!     Vertical axis

      call psetc ('AXIS_ORIENTATION','VERTICAL')
      call psetr ('AXIS_MIN_VALUE',pyn)
      call psetr ('AXIS_MAX_VALUE',pyx)
      call psetr ('AXIS_TICK_INTERVAL',pyt)
      CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',1)
      call psetc ('AXIS_TITLE_TEXT',cdhtitly)
      call psetc ('AXIS_POSITION','LEFT')
      call pseti ('AXIS_GRID_THICKNESS',1)
      call psetc ('AXIS_GRID_LINE_STYLE','DASH')
      call paxis

!     Right side of box

      call psetc ('AXIS_POSITION','RIGHT')
      call psetc ('AXIS_TITLE','OFF')
      call psetc ('AXIS_TICK_LABEL','OFF')
      call paxis

!     Top of box

      call psetc ('AXIS_ORIENTATION','HORIZONTAL')
      call psetc ('AXIS_POSITION','TOP')
      call psetr ('AXIS_MIN_VALUE',pxn)
      call psetr ('AXIS_MAX_VALUE',pxx)
      call psetr ('AXIS_TICK_INTERVAL', pxt)
      call paxis

RETURN
!-------------------------------------------------------------------------------
END SUBROUTINE susqpl


SUBROUTINE curvef(px,py,kpt,kthick,kline)
!-------------------------------------------------------------------------------

!*    Drawing a curve using magics calls


!     Coded by L. ISAKSEN  ECMWF  04.04.99

!     ARGUMENTS:
!     -----------
!                 PX     : ARRAY FOR X COORDINATES
!                 PY     : ARRAY FOR Y COORDINATES
!                 KPT    : NUMBER OF POINTS ON X AND Y ARRAYS
!                 KTHICK : THICKNESS OF CURVE
!                 KLINE  : LINE STYLE
!                              0 : NO LINE IS DRAWN
!                              1 : SOLID
!                              2 : DASH
!                              3 : DOT

!     ***************  NOTE  ****************************

!        THE CALL TO THIS SUBROUTINE HAS TO BE PRECEEDED BY A CALL
!        TO ANY ACTION ROUTINE IN THE SAME SUBPAGE

      implicit none

      integer kpt,kthick,kline
      real px(kpt),py(kpt)

      character (len=5), dimension(3) :: cstyle = (/ 'SOLID','DASH ', 'DOT  ' /)
      
      if (kline.ge.1.and.kline.le.3) then
        call psetc('GRAPH_TYPE','CURVE')
        call pset1r('GRAPH_CURVE_X_VALUES',px,kpt)
        call pset1r('GRAPH_CURVE_Y_VALUES',py,kpt)
        call pseti('GRAPH_LINE_THICKNESS',kthick)
        call psetc('GRAPH_LINE_STYLE',cstyle(kline))
        call psetc('GRAPH_LINE_COLOUR','BLACK')
        call pgraph
      else
        write(6,*) 'curvef: kline out of range ',kline
      endif
RETURN
!-------------------------------------------------------------------------------
END SUBROUTINE curvef


SUBROUTINE pltsym(px,py,kdim,ksymb,cdhcol)
!-------------------------------------------------------------------------------

!     Makesa scatter plot at positions px(i),py(i) with symbol ksymb
!     values px(i) and py(i) scale between pxn,pxx, resp. pyn,pyx
!     (n: minimum and x: maximum)
!     use susqpl (set up square plot) to draw the axis

!     Ad Stoffelen 9/11/'91
!     Ad Stoffelen 9/03/'92 Colour
!     Lars Isaksen 4/04/'99 Recoded

      implicit none

      integer kdim,ksymb
      real px(kdim),py(kdim)
      character cdhcol*32

      integer imar(kdim),ji

      do ji=1,kdim
        imar(ji)=ksymb
      enddo

      call psetc ('SYMBOL_POSITION_MODE','GRAPH')
      call pset1r('SYMBOL_INPUT_X_POSITION',px,kdim)
      call pset1r('SYMBOL_INPUT_Y_POSITION',py,kdim)
      call psetc ('SYMBOL_TABLE_MODE','OFF')

      call psetc ('SYMBOL_TYPE','MARKER')
      call psetc ('SYMBOL_COLOUR',cdhcol)
      call pset1i('SYMBOL_INPUT_MARKER_LIST',imar,kdim)
      call psetr ('SYMBOL_HEIGHT',0.1)

      call psymb

RETURN
!-------------------------------------------------------------------------------
END SUBROUTINE pltsym

SUBROUTINE pltcnt(pfld,kx,ky,l_level_list,nlevels,level)
!-------------------------------------------------------------------------------

!     Contours field PFLD at levels level; 

!     Ad Stoffelen  ECMWF  02-09-1991
!     Lars Isaksen  ECMWF  02-04-1999  Recoded
!     Antje Dethof  ECMWF  23-04-2001  Recoded

IMPLICIT NONE

  INTEGER, INTENT(IN):: &
    kx,ky,nlevels

  REAL, INTENT(IN):: &
    pfld(kx,ky),level(nlevels)

  LOGICAL, INTENT(IN):: &
    l_level_list

  REAL zxx,zyy

  IF (.not. l_level_list) THEN
    PRINT*,'Option l_level_list=.false. not realised yet.'
    PRINT*,'STOP'
    STOP
  ENDIF

!     Define projection

      call psetc('SUBPAGE_MAP_PROJECTION','CYLINDRICAL')
      call psetc('INPUT_FIELD_SUBPAGE_MAPPING','LOWER_LEFT')

      call psetr('SUBPAGE_LOWER_LEFT_LONGITUDE',0.)
      call psetr('SUBPAGE_LOWER_LEFT_LATITUDE',0.)
      zxx=float(kx)
      call psetr('SUBPAGE_UPPER_RIGHT_LONGITUDE',90.)
      zyy=float(ky)
      call psetr('SUBPAGE_UPPER_RIGHT_LATITUDE',90.)

!     Plot field

      call pset2r ('INPUT_FIELD',pfld,kx,ky)
      call psetc ('INPUT_FIELD_ORGANIZATION','FITTED')

!     Plot contours

      call psetc ('CONTOUR_SHADE','ON')
      call psetc ('CONTOUR_HILO','OFF')
      CALL PSETC('CONTOUR','OFF')
      call psetc ('CONTOUR_LABEL','OFF')
      CALL PSETC('CONTOUR_SHADE_TECHNIQUE','CELL_SHADING')

      call psetc ('CONTOUR_LEVEL_SELECTION_TYPE','LEVEL_LIST')
      call pset1r('CONTOUR_LEVEL_LIST',level,nlevels)

      CALL PSETC('CONTOUR_SHADE_MIN_LEVEL_COLOUR','BLUE')
      CALL PSETC('CONTOUR_SHADE_MAX_LEVEL_COLOUR','RED')
      CALL PSETC('CONTOUR_SHADE_COLOUR_DIRECTION','CLOCKWISE')

      call psetc ('LEGEND','ON')
      CALL PSETI('LEGEND_COLUMN_COUNT',10)
      CALL PSETR('LEGEND_TEXT_MAXIMUM_HEIGHT',0.5)
      CALL PSETR('LEGEND_BOX_Y_LENGTH',4.5)
      CALL PSETC('LEGEND_DISPLAY_TYPE','CONTINUOUS')
      CALL PSETC('LEGEND_TITLE','OFF')
      call pcont

RETURN
!-------------------------------------------------------------------------------
END SUBROUTINE pltcnt
