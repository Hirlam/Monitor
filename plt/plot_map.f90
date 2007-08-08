SUBROUTINE plot_map(stnr,yymm,yymm2,ptype,mtype)

 !
 ! Plot maps of station statistics
 !
 ! Ulf Andrae, SMHI, 2005
 !

 ! Modules
 USE data
 USE mymagics
 USE timing

 IMPLICIT NONE

 ! Input

 INTEGER, INTENT(IN) :: stnr,yymm,yymm2,ptype,mtype

 ! Local

 INTEGER, PARAMETER :: maxint = 6

 INTEGER :: i,j,k,kk,l,ll,m,            &
            hour(ntimver),              &
            maxn,timing_id,             &
            numstn,                     &
            min_stnr,max_stnr,mid(1),   &
            nexp_plot

 INTEGER, ALLOCATABLE :: stn(:),mcount(:)

 REAL :: lmax,lint,   &
         interval(maxint+1) =(/-6.,-4.,-2.,0.,2.,4.,6./),       &
         min_val,max_val,                                     &
         symbol_size(maxint)

 REAL, ALLOCATABLE :: lat(:),lon(:),dat(:,:),mlat(:),mlon(:),mdat(:)

 LOGICAL :: found_hour    = .FALSE.
 LOGICAL :: user_interval = .FALSE.
 LOGICAL :: mask(maxstn)

 CHARACTER(LEN=100) :: text     = ' ',wtext = ' '
 CHARACTER(LEN=10 ) :: chour    = ' '
 CHARACTER(LEN=50 ) :: cobsname = ' '
 CHARACTER(LEN=30)  :: fname    = ' '
 CHARACTER(LEN=30)  :: wname    = ' '
 CHARACTER(LEN=30)  :: mtext    = ' '
 CHARACTER(LEN=50)  :: cunit    = ' '

 CHARACTER(LEN=3) :: prefix = ' '

!-----------------------------------------------------------

 ! Init timing
 timing_id = 0
 IF (ltiming) CALL acc_timing(timing_id,'plot_map')


 !
 ! Set specific settings depending on ptype
 ! 0 : bias
 ! 1 : rmse
 ! 2 : value
 !

 SELECT CASE(ptype)
 CASE(0)
    prefix = 'm_b'
    IF (lfcver) prefix = 'M_b'
    symbol_size = (/.4,.3,.2,.2,.3,.4/)
    mtext = 'Bias'
    nexp_plot = nexp
 CASE(1)
    prefix = 'm_r'
    IF (lfcver) prefix = 'M_r'
    symbol_size = (/.2,.25,.3,.35,.4,.45/)
    mtext = 'Rmse'
    nexp_plot = nexp
 CASE(2)
    prefix = 'm_o'
    IF (lfcver) prefix = 'M_o'
    symbol_size = (/0.40,0.375,0.35,0.3,0.275,0.25/)
    mtext = ' '
    nexp_plot = nexp + 1
 CASE DEFAULT
    WRITE (6,*)'No such ptype in plot_map',ptype
    CALL abort
 END SELECT 


 IF (yymm < 999999 ) THEN
    CALL make_fname(prefix,yymm,stnr,nrun,fname,output_type)
 ELSE
    CALL make_fname(prefix,0   ,stnr,nrun,fname,output_type)
 ENDIF

 !
 ! Set plotting hours
 ! If map_hours not given (-1) plot all
 !

 IF (lfcver) THEN
    hour(1:ntimver)=fclen(1:ntimver)
 ELSE
    DO i=1,ntimver
       hour(i)=(i-1)*timdiff + time_shift
    ENDDO
 ENDIF

 IF ( map_hours(1) == -1 ) THEN
     map_hours(1:ntimver) = hour
    nmap_hours            = ntimver
 ENDIF


 !
 ! MAGICS settings
 !

 CALL open_output(fname)

 CALL PSETR ('SUPER_PAGE_X_LENGTH',29.7)
 CALL PSETR ('SUPER_PAGE_Y_LENGTH',21.0)

 SELECT CASE(mtype)
 CASE(0)
    ! Symbols
    CALL PSETC ('SYMBOL_TYPE','MARKER') 
    CALL PSET1I ('SYMBOL_MARKER_TABLE',(/15,15,15,15,15,15/),maxint) 
    CALL PSET1R ('SYMBOL_HEIGHT_TABLE',symbol_size,maxint) 
    CALL PSET1R ('SYMBOL_MAX_TABLE',interval(2:maxint+1),maxint)
    CALL PSET1R ('SYMBOL_MIN_TABLE',interval(1:maxint  ),maxint)
    CALL PSET1C ('SYMBOL_COLOUR_TABLE',&
    (/'MARINE','BLUE  ','CYAN  ','YELLOW','ORANGE','RED   '/),maxint)

    ! Legend
    CALL PSETC ('SYMBOL_TABLE_MODE','ON') 
    CALL PSETC ('LEGEND','ON') 
    CALL PSETC ('LEGEND_ENTRY','ON') 
    CALL PSETC ('LEGEND_ENTRY_PLOT_DIRECTION','COLUMN')
    CALL PSETR('LEGEND_BOX_X_POSITION',20.0)
    CALL PSETR('LEGEND_BOX_Y_POSITION',2.0)
    CALL PSETR('LEGEND_BOX_X_LENGTH',9.5)
    CALL PSETR('LEGEND_BOX_Y_LENGTH',10.0)
    CALL PSETR('LEGEND_ENTRY_MINIMUM_HEIGHT',0.5)
    CALL PSETR('LEGEND_ENTRY_MAXIMUM_HEIGHT',2.0)
    CALL PSETR('LEGEND_ENTRY_MAXIMUM_WIDTH',6.5)
    CALL PSETR('LEGEND_ENTRY_MINIMUM_WIDTH',1.5)
    CALL PSETC('LEGEND_BOX_MODE','POSITIONAL')
    CALL PSETC('LEGEND_BORDER','OFF')
    CALL PSETC('LEGEND_TITLE','ON')
    CALL PSETC('LEGEND_TEXT_COLOUR','BLACK')
    CALL PSETC('LEGEND_TEXT_QUALITY','HIGH')

 CASE(1)
    ! Numbers
    CALL PSETC ('SYMBOL_TYPE','NUMBER') 
    CALL PSETC ('SYMBOL_TABLE_MODE','OFF')

 CASE DEFAULT
    CALL ABORT
 END SELECT

 ! Map
 CALL PSETC ('MAP_COASTLINE_COLOUR','BLACK') 
 CALL PSETC ('MAP_COASTLINE_RESOLUTION','HIGH') 
 CALL PSETC ('MAP_GRID_COLOUR','BLACK') 
 CALL PSETC ('SUBPAGE_MAP_PROJECTION','POLAR_STEREOGRAPHIC')
 CALL PSETC ('SUBPAGE_MAP_AREA_DEFINITION','CENTRE')
 CALL PSETR ('SUBPAGE_MAP_VERTICAL_LONGITUDE',0.)
 CALL PSETR ('SUBPAGE_MAP_CENTRE_LATITUDE' ,MAP_CENTRE_LATITUDE )
 CALL PSETR ('SUBPAGE_MAP_CENTRE_LONGITUDE',MAP_CENTRE_LONGITUDE)
 CALL PSETR ('SUBPAGE_MAP_SCALE',map_scale)

 ! Id line
 CALL psetc('PAGE_ID_LINE_SYSTEM_PLOT','ON')
 CALL psetc('PAGE_ID_LINE_ERRORS_PLOT','OFF')
 CALL psetc('PAGE_ID_LINE_DATE_PLOT','ON')
 CALL psetc('PAGE_ID_LINE_QUALITY','HIGH')
 CALL psetc('PAGE_ID_LINE_LOGO_PLOT','OFF')

 ! Text
 CALL psetc('TEXT_COLOUR','BLACK')
 CALL psetc('TEXT_QUALITY','HIGH')

 !
 ! Allocate 
 !

 ALLOCATE(lat(maxstn),lon(maxstn),stn(maxstn),dat(nexp_plot,maxstn))

 dat = 0.
 lat = 0.
 lon = 0.
 stn = 0

 !
 ! Loop over all parameters and times
 !

 PAR_LOOP : DO j=1,nparver

    FC_LOOP  : DO k=1,ntimver

       !
       ! Plot only the requested hours
       !

       found_hour = .FALSE.
       DO kk=1,nmap_hours
         found_hour = (map_hours(kk) == hour(k))
         IF ( found_hour ) EXIT
       ENDDO

       IF ( .NOT. found_hour ) CYCLE

       !
       ! Copy data and estimate max/min values 
       !
       maxn = 0
       lmax = 0.
       DO i=1,nexp 

         ll = 0

         DO l=1,maxstn

            IF ( stat(l)%s(i,j,k)%n == 0 ) CYCLE

            ll = ll + 1

            SELECT CASE (ptype)
            CASE(0)
                dat(i,ll) = stat(l)%s(i,j,k)%bias / FLOAT(stat(l)%s(i,j,k)%n)
            CASE(1)
                dat(i,ll) = stat(l)%s(i,j,k)%bias / FLOAT(stat(l)%s(i,j,k)%n)
                dat(i,ll) = sqrt (stat(l)%s(i,j,k)%rmse / FLOAT(stat(l)%s(i,j,k)%n))
            CASE(2)

               IF ( ldiff ) THEN
                  dat(i,ll) = ( stat(l)%s(i,j,k)%bias +     &
                                stat(l)%s(i,j,k)%obs    )   &
                        / FLOAT(stat(l)%s(i,j,k)%n)
               ELSE
                  dat(i,ll) =   stat(l)%s(i,j,k)%bias +     &
                                stat(l)%s(i,j,k)%obs   
               ENDIF

               IF ( i == 1 ) THEN
                 IF ( ldiff ) THEN
                  dat(nexp+1,ll) = stat(l)%s(i,j,k)%obs/FLOAT(stat(l)%s(i,j,k)%n)
                 ELSE
                  dat(nexp+1,ll) = stat(l)%s(i,j,k)%obs
                 ENDIF
               ENDIF

            CASE DEFAULT
               WRITE (6,*)'No such ptype in plot_map',ptype
               CALL abort
            END SELECT

            IF ( i == 1 ) THEN
               lat(ll) = stat(l)%lat
               lon(ll) = stat(l)%lon
               stn(ll) = stat(l)%stnr
            ENDIF

            maxn  = MAX(maxn,stat(l)%s(i,j,k)%n)
            lmax  = MAX(lmax,ABS(dat(i,ll)))

         ENDDO

         IF ( i == 1 ) numstn = ll

       ENDDO 

!      IF ( maxn == 0 ) CYCLE FC_LOOP


       IF ( ALLOCATED(mlat))  DEALLOCATE(mlat,mlon,mdat,mcount)
       ALLOCATE(mlat(numstn),mlon(numstn),mdat(numstn),mcount(numstn))

       !
       ! Set the intervals
       !

       SELECT CASE(ptype)
       CASE(0)
          user_interval = ( ABS(map_bias_interval(j,1) - map_bias_interval(j,maxint+1) ) > 1.e-6 )
       CASE(1)
          user_interval = ( ABS(map_rmse_interval(j,1) - map_rmse_interval(j,maxint+1) ) > 1.e-6 )
       CASE(2)
          user_interval = ( ABS( map_obs_interval(j,1) -  map_obs_interval(j,maxint+1) ) > 1.e-6 )
       END SELECT


       IF ( .NOT. user_interval ) THEN

          lmax = lmax*1.001
          SELECT CASE(ptype)
          CASE(0)
             lint = lmax / (maxint/2)
             DO m=-maxint/2,maxint/2
                interval(m+maxint/2+1) = m*lint
             ENDDO
          CASE(1,2)
             lint = lmax / maxint
             DO m=0,maxint
                interval(m+1) = m*lint
             ENDDO
          CASE DEFAULT
          END SELECT

       ELSE

          SELECT CASE(ptype)
          CASE(0)
             interval = map_bias_interval(j,:)
          CASE(1)
             interval = map_rmse_interval(j,:)
          CASE(2)
             interval = map_obs_interval(j,:)
          END SELECT

       ENDIF

       !
       ! Loop over all experiments
       !

       EXP_LOOP : DO i=1,nexp_plot 

         CALL PNEW('SUPER_PAGE')

         CALL PCOAST

         cunit = ' '
         CALL yunit(obstype(j),cunit)
         cunit = ' '//TRIM(mtext)//' Interval ('//TRIM(cunit)//') '
         CALL PSETC('LEGEND_TITLE_TEXT',cunit)

         CALL pname(obstype(j),cobsname)

         IF (yymm == 0 ) THEN
         ELSEIF(yymm < 13) THEN
     
            SELECT CASE(period_freq) 
            CASE(1)
             WRITE(wtext,'(A8,A8)')'Period: ',seasonal_name2(yymm)
            CASE(3)
             WRITE(wtext,'(A8,A8)')'Period: ',seasonal_name1(yymm)
            END SELECT 
     
         ELSEIF(yymm < 999999 ) THEN
            WRITE(wtext,'(A8,I8)')'Period: ',yymm
         ELSE
            WRITE(wtext,'(A8,I8,A1,I8)')'Period: ',yymm,'-',yymm2
         ENDIF

         text =  TRIM(expname(i))//'  '//TRIM(cobsname)
         IF (lfcver) THEN
            WRITE(chour,'(I3.2,X,A1)')hour(k),'H'
         ELSE
            WRITE(chour,'(I3.2,X,A3)')hour(k),'UTC'
         ENDIF
         text = TRIM(text)//' at '//chour   //'  '//TRIM(wtext)

         CALL PSETC ('TEXT_MODE', 'TITLE')
         CALL PSETC('TEXT_LINE_1',TRIM(text))
         CALL PSETI('TEXT_LINE_COUNT',1)

         mlat = lat(1:numstn)
         mlon = lon(1:numstn)
         mdat = dat(i,1:numstn)

         CALL PSET1R ('SYMBOL_INPUT_Y_POSITION' ,mlat,numstn)
         CALL PSET1R ('SYMBOL_INPUT_X_POSITION' ,mlon,numstn)
         CALL PSET1R ('SYMBOL_INPUT_NUMBER_LIST',mdat,numstn)

         SELECT CASE(mtype)
         CASE(0)
            CALL PSET1R ('SYMBOL_MAX_TABLE',interval(2:maxint+1),maxint)
            CALL PSET1R ('SYMBOL_MIN_TABLE',interval(1:maxint  ),maxint)
         CASE(1)
            CALL PSETC ('SYMBOL_COLOUR','MARINE')
            CALL PSETC ('SYMBOL_FORMAT','(F6.2)')
            CALL PSETR ('SYMBOL_HEIGHT',0.3)
            IF (ptype == 0 ) CALL PSETC ('SYMBOL_TABLE_MODE','ON')
            CALL PSET1R ('SYMBOL_MAX_TABLE',(/-1e-12,1.e-12,1.e24/),3)
            CALL PSET1R ('SYMBOL_MIN_TABLE',(/-1.e24,-1.e-12,1.e-12/),3)
            CALL PSET1C ('SYMBOL_COLOUR_TABLE',(/'MARINE','GREEN ','RED   '/),3)
         END SELECT

         CALL PSYMB 
         CALL PTEXT

         ! Info box
         CALL PSETC ('TEXT_MODE', 'POSITIONAL')
         CALL PSETC ('TEXT_JUSTIFICATION', 'LEFT')
         CALL PSETC ('TEXT_BORDER', 'OFF')
       
         CALL PSETR ('TEXT_BOX_Y_POSITION', 14.0)
         CALL PSETR ('TEXT_BOX_X_POSITION', 18.0)
         CALL PSETR ('TEXT_BOX_X_LENGTH', 8.)
         CALL PSETR ('TEXT_BOX_Y_LENGTH', 5.0)

         CALL PSETI ('TEXT_LINE_COUNT', 4)
         IF ( mtype == 0 .AND. user_interval ) THEN
            mcount = 0
            WHERE ( mdat > interval(1) .AND. mdat < interval (7) ) mcount = 1
            wtext = ' '
            WRITE(wtext,'(''Stations (plotted/total) = '',I7,I8)') &
            SUM(mcount),numstn
            CALL PSETC ('TEXT_LINE_1', TRIM(wtext))
         ELSE
            wtext = ' '
            WRITE(wtext,'(''Number of stations = '',I8)') numstn
            CALL PSETC ('TEXT_LINE_1', TRIM(wtext))
         ENDIF

         IF ( maxn > 0 ) THEN
            mid      = MINLOC(mdat)
            min_val  = mdat(mid(1))
            min_stnr = stn(mid(1))
            mid      = MAXLOC(mdat)
            max_val  = mdat(mid(1))
            max_stnr = stn(mid(1))
         ELSE
            mid      = 0
            min_val  = 0
            min_stnr = 0
            mid      = 0
            max_val  = 0
            max_stnr = 0
         ENDIF

         wname='(A,I8,X,EN13.3e2)'
         wtext = ' '
         WRITE(wtext,wname)' Lowest value:',min_stnr, min_val
         CALL PSETC('TEXT_LINE_2',wtext)
         wtext = ' '
         WRITE(wtext,wname)'Highest value:',max_stnr, max_val
         CALL PSETC('TEXT_LINE_3',wtext)

         IF (.NOT. lfcver .AND. show_fc_length ) THEN
            CALL PSETI ('TEXT_LINE_COUNT', 4)
            IF (nfclengths > 10 ) THEN
               wname='(A,2I3.2,A5,I2.2)'
               WRITE(wtext,wname)'Forecast lengths used:',   &
               fclen(1:2),' ... ',fclen(nfclengths)
            ELSE
               wname='(A,XX(1X,I2.2))'
               WRITE(wname(4:5),'(I2.2)')nfclengths
               WRITE(wtext,wname)'Forecast lengths used:',fclen(1:nfclengths)
            ENDIF
            CALL PSETC('TEXT_LINE_4',wtext)
         ENDIF

         CALL PTEXT

       ENDDO EXP_LOOP
    ENDDO FC_LOOP  
 ENDDO PAR_LOOP 

 CALL PCLOSE

 IF ( ALLOCATED(mlat))  DEALLOCATE(mlat,mlon,mdat,mcount)
 DEALLOCATE(lat,lon,stn,dat)

 IF (ltiming) CALL acc_timing(timing_id,'plot_map')

 RETURN

END SUBROUTINE plot_map

