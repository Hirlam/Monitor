MODULE mymagics


 USE data, ONLY : err_ind,maxexp

 IMPLICIT NONE

 SAVE

 INTEGER :: clen = 0

 LOGICAL :: plotfcver     = .FALSE.
 LOGICAL :: plotseason    = .FALSE.
 LOGICAL :: plotltemp     = .FALSE.
 REAL    :: plottimdiff = 1.

 CHARACTER(LEN=20), ALLOCATABLE :: cdate(:)
 CHARACTER(LEN=20) :: ytitle = ''

 CHARACTER(LEN=20), DIMENSION(maxexp) :: linecolor=(/'RED        ',       &
                                                'BLUE       ',            &
                                                'BLACK      ',            &
                                                'CYAN       ',            &
                                                'KELLY_GREEN',            &
                                                'MAGENTA    ',            &
                                                'ORAGNE     ',            &
                                                'BROWN      ',            &
                                                'VIOLET     ',            &
                                                'NAVY       '/)

 CHARACTER(LEN=3) :: seasonal_name1(4)=(/'DJF','MAM','JJA','SON'/),       &
                     seasonal_name2(12)=(/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/)


#ifdef MAGICS

 CONTAINS

 !-----------------------------------------------------
 !-----------------------------------------------------
 !-----------------------------------------------------


 !-----------------------------------------------------
 !-----------------------------------------------------
 !-----------------------------------------------------

 SUBROUTINE plot_data(data,n,lincol,legtxt,flag)

  IMPLICIT NONE
 
  INTEGER :: n,flag
  REAL    :: data(n)
  CHARACTER(LEN=*) :: legtxt,lincol

    IF (flag.EQ.1) THEN
       CALL PSETC('GRAPH_LINE','ON')
       CALL PSETC('GRAPH_SYMBOL','OFF')
    ELSE
       CALL PSETC('GRAPH_LINE','OFF')
       CALL PSETC('GRAPH_SYMBOL','ON')
    ENDIF

    CALL PSETC  ('GRAPH_SYMBOL_COLOUR',lincol)
    CALL PSETC  ('GRAPH_LINE_COLOUR',  lincol)
    CALL PSETI  ('GRAPH_LINE_THICKNESS',  5)
    CALL PSETC  ('LEGEND_USER_TEXT' ,legtxt)
    CALL PSET1R ('GRAPH_CURVE_Y_VALUES',data,n)

    CALL PGRAPH

 END SUBROUTINE plot_data
 !-----------------------------------------------------
 !-----------------------------------------------------
 !-----------------------------------------------------
 SUBROUTINE plot_y_data(data,n,lincol,legtxt,flag)

  IMPLICIT NONE
 
  INTEGER :: n,flag
  REAL    :: data(n)
  CHARACTER(LEN=*) :: legtxt,lincol

    IF (flag.EQ.1) THEN
       CALL PSETC('GRAPH_LINE','ON')
       CALL PSETC('GRAPH_SYMBOL','OFF')
    ELSE
       CALL PSETC('GRAPH_LINE','OFF')
       CALL PSETC('GRAPH_SYMBOL','ON')
    ENDIF

    CALL PSETC  ('GRAPH_SYMBOL_COLOUR',lincol)
    CALL PSETC  ('GRAPH_LINE_COLOUR',  lincol)
    CALL PSETI  ('GRAPH_LINE_THICKNESS',  5)
    CALL PSETC  ('LEGEND_USER_TEXT' ,legtxt)
    CALL PSET1R ('GRAPH_CURVE_X_VALUES',data,n)

    CALL PGRAPH

 END SUBROUTINE plot_y_data
 !-----------------------------------------------------
 !-----------------------------------------------------
 !-----------------------------------------------------
SUBROUTINE set_cdate(date,time,n,s)

 IMPLICIT NONE

 ! Input
 INTEGER :: n,s,date(n),time(n)

 ! Local
 INTEGER i,k,kk,i_start

 IF(ALLOCATED(cdate)) DEALLOCATE(cdate)
 ALLOCATE(cdate(n))

     k = 0
     DO i=1,n,s
         !kk = i+s-1
          kk = i+s/2

         IF(kk.GT.n) EXIT

         k  = k + 1
 
         WRITE(cdate(k)(1:20),'(I4.4,3(A1,I2.2),A6)')           &
               date(kk)/10000,'-',            &
               MOD(date(kk),10000)/100,'-',   &
               MOD(date(kk),100),' ',     &
                   time(kk),':00:00'
     ENDDO 
 
  clen=k
  CALL PRESET ('GRAPH_CURVE_DATE_X_VALUES')
  CALL PSET1C ('GRAPH_CURVE_DATE_X_VALUES',cdate(1:clen),clen)

  RETURN
 END SUBROUTINE set_cdate
 !-----------------------------------------------------
 !-----------------------------------------------------
 !-----------------------------------------------------
 SUBROUTINE new_multipage(npages)

  IMPLICIT NONE

  REAL, PARAMETER :: scalex = 0.95
  REAL, PARAMETER :: scaley = 1.0

  INTEGER :: npages
  REAL :: xl,yl,xf,yf,xg,yg

  SELECT CASE(npages)

  CASE(1,2,3)

  xf = npages
  yf = 1.

  CASE(4)

  xf = 2.
  yf = 2.

  CASE(5,6)

  xf = 3.
  yf = 2.

  CASE(7,8,9)

  xf = 3.
  yf = 3.

  END SELECT
  yg = 0.1
  xg = 0.1

  
  xl = (29.7 - xg*(xf -1.)) / xf * scalex
  yl = (21.0 - yg*(yf -1.)) / yf * scaley

 WRITE(6,*)'XL,YL',xl,yl

 CALL PSETR  ('PAGE_Y_LENGTH',yl)
 CALL PSETR  ('PAGE_X_LENGTH',xl)
 CALL PSETR  ('PAGE_X_GAP',xg)
 CALL PSETR  ('PAGE_Y_GAP',yg)
 CALL PSETR  ('SUBPAGE_Y_LENGTH',.8*yl)
 CALL PSETR  ('SUBPAGE_X_LENGTH',0.70*xl)
 CALL PSETR  ('SUBPAGE_Y_POSITION',0.2*yl)
 CALL PSETR  ('SUBPAGE_X_POSITION',0.2*xl)
 !CALL PSETC  ('PAGE_FRAME','OFF')
 CALL PSETC  ('PAGE_ID_LINE','OFF')
  
  RETURN

 END SUBROUTINE new_multipage
 !-----------------------------------------------------
 !-----------------------------------------------------
 !-----------------------------------------------------
 SUBROUTINE new_page(mindate,mintime,	&
                     maxdate,maxtime,	&
                     minx,maxx,miny,maxy)

 IMPLICIT NONE

 ! Input
 INTEGER mindate,mintime,maxdate,maxtime
 REAL :: miny,maxy,minx,maxx

 ! Local
 INTEGER :: i,j,				&
            SY,SM,SD,EY,EM,ED,	&
            diff,ierr

 CHARACTER(LEN=20) :: wdate = ''

 REAL :: err_ind_l

 integer :: difdtg
 !-----------------------------------------------------

 ! Plotting part

 CALL PNEW('SUPER_PAGE')
 IF (plotltemp) THEN
 CALL PSETC('SUBPAGE_MAP_PROJECTION',         'NONE')
 CALL PSETR  ('SUPER_PAGE_X_LENGTH',           21.0)
 CALL PSETR  ('SUPER_PAGE_Y_LENGTH',           29.7)
 CALL PSETR  ('PAGE_X_LENGTH',                 21.0)
 CALL PSETR  ('PAGE_Y_LENGTH',                 29.7)
 CALL PSETR  ('SUBPAGE_X_POSITION',             2.5)
 CALL PSETR  ('SUBPAGE_Y_POSITION',             2.0)
 CALL PSETR  ('SUBPAGE_X_LENGTH',              18.0)
 CALL PSETR  ('SUBPAGE_Y_LENGTH',              23.0)
 ELSE
 CALL PSETR  ('SUBPAGE_X_POSITION',             3.5)
 CALL PSETR  ('SUBPAGE_Y_POSITION',             3.0)
 CALL PSETR  ('SUBPAGE_X_LENGTH',              23.0)
 CALL PSETR  ('SUBPAGE_Y_LENGTH',              18.0)
 ENDIF

 CALL PSETC('LEGEND','ON')
 CALL PSETC('LEGEND_TEXT_COLOUR','BLACK')
 CALL PSETC('LEGEND_TEXT_QUALITY','HIGH')
 CALL PSETR('LEGEND_TEXT_MAXIMUM_HEIGHT',0.25)

 ! ID line


 CALL psetc('TEXT_COLOUR','BLACK')
 CALL psetc('TEXT_QUALITY','HIGH')
 !CALL psetc('PAGE_ID_LINE_USER_TEXT','Copyright ECMWF')
 CALL psetc('PAGE_ID_LINE_SYSTEM_PLOT','ON')
 CALL psetc('PAGE_ID_LINE_ERRORS_PLOT','OFF')
 CALL psetc('PAGE_ID_LINE_LOGO_PLOT','OFF')
 CALL psetc('PAGE_ID_LINE_DATE_PLOT','ON')
 CALL psetc('PAGE_ID_LINE_QUALITY','HIGH')

 ! Set axis
 CALL psetc ('AXIS_GRID','ON')
 CALL PSETR ('AXIS_TITLE_HEIGHT',0.35)
 CALL PSETC ('AXIS_TITLE_QUALITY','HIGH')

 ! Set vertical axis
 CALL PSETC ('AXIS_ORIENTATION','VERTICAL') 
 CALL preset('AXIS_POSITION')
 CALL preset('AXIS_TYPE')
 CALL preset('AXIS_TICK_INTERVAL')
 CALL preset('AXIS_TITLE')
 CALL PSETC ('AXIS_TYPE','REGULAR') 
 CALL PSETR ('AXIS_MIN_VALUE',miny) 
 CALL PSETR ('AXIS_MAX_VALUE',maxy)
 IF (plotltemp ) THEN
    IF ( miny < maxy ) THEN 
       CALL PSETC ('AXIS_TYPE','REGULAR') 
       CALL PSETC ('AXIS_TITLE_TEXT','Height (m)')
    ELSE
       CALL PSETC ('AXIS_TYPE','LOGARITHMIC') 
       CALL PSETC ('AXIS_TITLE_TEXT','Pressure (hPa)')
    ENDIF    
 ELSE
    CALL PSETC ('AXIS_TYPE','REGULAR') 
    CALL PSETC ('AXIS_TITLE_TEXT',ytitle)
 ENDIF

 IF(mindate.eq.0) THEN

    IF (ABS(maxx-minx).LT.1.e-12) THEN
       CALL PSETC ('AXIS_TITLE_TEXT','MOD')
       CALL PAXIS
       CALL PSETC ('AXIS_ORIENTATION','HORIZONTAL') 
       CALL PSETC ('AXIS_TITLE_TEXT','OBS')
    ELSE
       CALL PAXIS
       CALL PSETC ('AXIS_ORIENTATION','HORIZONTAL') 
       CALL preset('AXIS_TYPE')
       CALL preset('AXIS_TITLE')

       IF (plotltemp ) THEN
          CALL PRESET('AXIS_TICK_INTERVAL')
       ELSE
          CALL PSETR('AXIS_TICK_INTERVAL',plottimdiff)
       ENDIF
   
       CALL PSETC ('AXIS_TYPE','REGULAR') 
       CALL PSETR ('AXIS_MIN_VALUE',minx) 
       CALL PSETR ('AXIS_MAX_VALUE',maxx)
   
       IF (plotltemp) THEN
          CALL PSETC ('AXIS_TITLE_TEXT',ytitle)
       ELSEIF (plotseason) THEN
          CALL PSETC ('AXIS_TITLE_TEXT','Day of year')
       ELSEIF (plotfcver) THEN
          CALL PSETC ('AXIS_TITLE_TEXT','Forecast length (hours)')
       ELSE
          CALL PSETC ('AXIS_TITLE_TEXT','Time of day')
       ENDIF

    ENDIF

 ELSE
    CALL PAXIS
    ! Set time axis
    CALL preset('AXIS_DATE_MIN_VALUE')
    CALL preset('AXIS_DATE_MAX_VALUE')
    sy = mindate/10000
    sm = MOD(mindate,10000)/100
    sd = MOD(mindate,100  )
    ey = maxdate/10000
    em = MOD(maxdate,10000)/100
    ed = MOD(maxdate,100  )

    CALL preset('AXIS_TYPE')
    CALL PSETC ('AXIS_ORIENTATION','HORIZONTAL') 
    CALL psetc ('AXIS_TYPE','DATE')
    CALL preset('AXIS_TITLE')
    CALL PRESET ('AXIS_TITLE_TEXT')
    WRITE(wdate(1:20),'(I4.4,3(A1,I2.2),A6)')           &
               sy,'-',sm,'-', sd,' ',mintime,':00:00'
    CALL psetc ('AXIS_DATE_MIN_VALUE',wdate)
    WRITE(wdate(1:20),'(I4.4,3(A1,I2.2),A6)')           &
               ey,'-',em,'-', ed,' ',maxtime,':00:00'
    CALL psetc ('AXIS_DATE_MAX_VALUE',wdate)
    CALL preset('AXIS_TICK_INTERVAL')

    diff = difdtg((sy*100+sm)*100+sd,0,(ey*100+em)*100+ed,0)/86400

    CALL PRESET  ('AXIS_HOURS_LABEL')
    CALL PRESET  ('AXIS_MONTHS_LABEL')
    IF     (diff.GT.5*365) THEN
       CALL PSETC  ('AXIS_DATE_TYPE',         'MONTHS')
       CALL PSETR  ('AXIS_TICK_INTERVAL',          24.)
    ELSEIF (diff.GE.2*365 ) THEN
       CALL PSETC  ('AXIS_DATE_TYPE',         'MONTHS')
       CALL PSETR  ('AXIS_TICK_INTERVAL',           3.)
    ELSEIF (diff.GE.366 ) THEN
       CALL PSETC  ('AXIS_MONTHS_LABEL',          'ON')
       CALL PSETC  ('AXIS_DATE_TYPE',         'MONTHS')
       CALL PSETR  ('AXIS_TICK_INTERVAL',           1.)
    ELSEIF (diff.GT.3*31) THEN
       CALL PSETC  ('AXIS_MONTHS_LABEL',          'ON')
       CALL PSETC  ('AXIS_DATE_TYPE',         'MONTHS')
       CALL PSETR  ('AXIS_TICK_INTERVAL',           1.)
    ELSEIF (diff.GT.31) THEN
       CALL PSETC  ('AXIS_DATE_TYPE',           'DAYS')
       CALL PSETR  ('AXIS_TICK_INTERVAL',           7.)
    ELSEIF (diff.GT.7) THEN
       CALL PSETC  ('AXIS_DATE_TYPE',           'DAYS')
       CALL PSETR  ('AXIS_TICK_INTERVAL',           1.)
       CALL PSETC  ('AXIS_HOURS_LABEL',           'OFF')
    ELSEIF (diff.GT.2) THEN
       CALL PSETC  ('AXIS_DATE_TYPE',           'HOURS')
       CALL PSETR  ('AXIS_TICK_INTERVAL',           3.)
       CALL PSETC  ('AXIS_HOURS_LABEL',           'ON')
    ELSE
       CALL PSETC  ('AXIS_DATE_TYPE',           'HOURS')
       CALL PSETR  ('AXIS_TICK_INTERVAL',           1.)
       CALL PSETC  ('AXIS_HOURS_LABEL',           'ON')
    ENDIF
   
 ENDIF

 CALL PAXIS

 CALL PSETC ('GRAPH_TYPE','CURVE') 
 err_ind_l = err_ind + 1
 CALL PSETR ('GRAPH_Y_SUPPRESS_BELOW',err_ind_l)
!CALL PSETC ('GRAPH_MISSING_DATA_MODE','DROP')

 ! Text
 CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT',.5)

 RETURN

 END SUBROUTINE new_page
 !-----------------------------------------------------
 !-----------------------------------------------------
 !-----------------------------------------------------
 SUBROUTINE new_subpage(mindate,mintime,	&
                        maxdate,maxtime,	&
                        minx,maxx,miny,maxy)

 IMPLICIT NONE

 ! Input
 INTEGER mindate,mintime,maxdate,maxtime
 REAL :: miny,maxy,minx,maxx

 ! Local
 INTEGER :: i,j,			&
            SY,SM,SD,EY,EM,ED

 !-----------------------------------------------------


 ! Plotting part

 CALL PNEW('PAGE')

 CALL PSETC('LEGEND','OFF')
 CALL PSETC('LEGEND_TEXT_COLOUR','BLACK')
 CALL PSETC('LEGEND_TEXT_QUALITY','HIGH')
 CALL PSETR('LEGEND_TEXT_MAXIMUM_HEIGHT',0.15)


 ! Set axis
 CALL psetc ('AXIS_GRID','ON')
 CALL PSETR ('AXIS_TITLE_HEIGHT',0.15)
 CALL PSETR ('AXIS_TICK_LABEL_HEIGHT',0.15)
 CALL PSETC ('AXIS_TITLE_QUALITY','HIGH')

 ! Set vertical axis
 CALL PSETC ('AXIS_ORIENTATION','VERTICAL') 
 CALL PSETC ('AXIS_TYPE','REGULAR') 
 CALL PSETR ('AXIS_MIN_VALUE',miny) 
 CALL PSETR ('AXIS_MAX_VALUE',maxy)
 CALL preset('AXIS_TICK_INTERVAL')
 CALL preset('AXIS_TITLE')
 CALL PSETC ('AXIS_TITLE_TEXT',ytitle)
 CALL PAXIS


 IF(mindate.eq.0) THEN

    CALL preset('AXIS_TYPE')
    CALL PSETC ('AXIS_ORIENTATION','HORIZONTAL') 
    CALL preset('AXIS_TITLE')
    CALL PSETC ('AXIS_TITLE_TEXT','Time of day')
    CALL PSETR ('AXIS_MIN_VALUE',minx) 
    CALL PSETR ('AXIS_MAX_VALUE',maxx)

 ELSE


    ! Set time axis
    CALL preset('AXIS_TYPE')
    CALL PSETC ('AXIS_ORIENTATION','HORIZONTAL') 
    CALL psetc ('AXIS_TYPE','DATE')
    CALL preset('AXIS_TITLE')
    CALL PSETC ('AXIS_TITLE_TEXT','Date')
    CALL psetc ('AXIS_DATE_MIN_VALUE',cdate(1))
    CALL psetc ('AXIS_DATE_MAX_VALUE',cdate(clen))
    CALL preset('AXIS_TICK_INTERVAL')
   
    ! Set time interval
    sy = mindate/10000
    sm = MOD(mindate,10000)/100
    sd = MOD(mindate,100  )
    ey = maxdate/10000
    em = MOD(maxdate,10000)/100
    ed = MOD(maxdate,100  )
   
    IF     ((EY-SY).GT.5) THEN
       CALL PSETC  ('AXIS_DATE_TYPE',         'MONTHS')
       CALL PSETR  ('AXIS_TICK_INTERVAL',          24.)
    ELSEIF ((EY-SY).GE.2 ) THEN
       CALL PSETC  ('AXIS_DATE_TYPE',         'MONTHS')
       CALL PSETR  ('AXIS_TICK_INTERVAL',           3.)
    ELSEIF ((EY-SY).GE.1 ) THEN
       CALL PSETC  ('AXIS_DATE_TYPE',         'MONTHS')
       CALL PSETR  ('AXIS_TICK_INTERVAL',           1.)
    ELSEIF ((EY-SY).LT.1.AND.(EM-SM).GT.3) THEN
       CALL PSETC  ('AXIS_DATE_TYPE',         'MONTHS')
       CALL PSETR  ('AXIS_TICK_INTERVAL',           1.)
    ELSEIF ((EY-SY).LT.1.AND.(EM-SM).GT.1) THEN
       CALL PSETC  ('AXIS_DATE_TYPE',           'DAYS')
       CALL PSETR  ('AXIS_TICK_INTERVAL',           7.)
    ELSE
       CALL PSETC  ('AXIS_DATE_TYPE',           'DAYS')
       CALL PSETR  ('AXIS_TICK_INTERVAL',           1.)
    ENDIF
   
 ENDIF

 CALL paxis

 CALL PSETC ('GRAPH_TYPE','CURVE') 
 CALL PSETR ('GRAPH_Y_SUPPRESS_BELOW',-998.)

 ! Text
 CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT',.25)

 RETURN

 END SUBROUTINE new_subpage
 !-----------------------------------------------------
 !-----------------------------------------------------
 !-----------------------------------------------------

# endif

END MODULE mymagics
