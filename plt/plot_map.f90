SUBROUTINE plot_map(stnr,yymm,yymm2,ptype,mtype,per_ind)

 !
 ! Plot maps of station statistics
 !
 ! Ulf Andrae, SMHI, 2005
 !

 ! Modules
 USE mymagics
 USE timing
 USE data,     ONLY : maxstn,ltiming,lfcver,&
                      nexp,tag,output_type, &
                      output_mode,len_lab,  &
                      stat,period_freq,     &
                      obstype,expname,      &
                      show_fc_length,ldiff, &
                      ntimver,map_scale,    &
                      maxfclenval,nparver,  &
                      used_hours,used_fclen,&
                      timdiff,time_shift,   &
                      use_fclen,show_times, &
                      map_bias_interval,    &
                      map_rmse_interval,    &
                      map_obs_interval,     &
                      map_projection,       &
                      map_area_definition,  &
                      map_vertical_longitude,      &
                      map_centre_latitude,         &
                      map_centre_longitude,        &
                      map_lower_left_latitude,     &
                      map_lower_left_longitude,    &
                      map_upper_right_latitude,    &
                      map_upper_right_longitude


 IMPLICIT NONE

 ! Input

 INTEGER, INTENT(IN) :: stnr,yymm,yymm2,ptype,mtype,per_ind

 ! Local

 INTEGER, PARAMETER :: maxint = 6

 INTEGER :: i,j,k,kk,l,ll,m,            &
            hour(ntimver),              &
            maxn,timing_id,             &
            numstn,period,              &
            min_stnr,max_stnr,mid(1),   &
            nexp_plot,ntimver_out

 INTEGER, ALLOCATABLE :: stn(:),mcount(:)

 REAL :: lmax,lint,   &
         interval(maxint+1) =(/-6.,-4.,-2.,0.,2.,4.,6./),       &
         min_val,max_val,                                       &
         symbol_size(maxint),                                   &
         rnum,bias,rmse,obs

 REAL, ALLOCATABLE :: lat(:),lon(:),dat(:,:),mlat(:),mlon(:),mdat(:)

 LOGICAL :: found_hour    = .FALSE.
 LOGICAL :: user_interval = .FALSE.
 LOGICAL :: mask(maxstn)

 CHARACTER(LEN=100) :: text     = ' ',wtext = ' '
 CHARACTER(LEN=100) :: my_tag   = ' '
 CHARACTER(LEN=10 ) :: chour    = ' '
 CHARACTER(LEN=50 ) :: cobsname = ' '
 CHARACTER(LEN=100) :: fname    = ' '
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
    period = yymm
 ELSE
    period = 0
 ENDIF

 IF ( ALL(show_times == -1) ) THEN
    ntimver_out = 1
 ELSE
    ntimver_out = ntimver
 ENDIF

 !
 ! Set plotting hours
 ! If map_hours not given (-1) plot all
 !

 IF (lfcver) THEN
    hour(1:ntimver)=use_fclen(1:ntimver)
 ELSE
    DO i=1,ntimver
       hour(i)=(i-1)*timdiff + time_shift
    ENDDO
 ENDIF

 IF ( output_mode == 1 ) THEN
    CALL make_fname(prefix,period,stnr,      &
                    tag,prefix,'0',          &
                    output_mode,output_type, &
                    fname)
    CALL open_output(fname)

    CALL PSETR ('SUPER_PAGE_X_LENGTH',29.7)
    CALL PSETR ('SUPER_PAGE_Y_LENGTH',21.0)
 ENDIF

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

    FC_LOOP  : DO kk=1,ntimver_out

       !
       ! Plot only the requested hours
       !
       IF ( ntimver_out /= 1 .AND. .NOT. ANY( show_times == hour(kk) )) CYCLE

       !
       ! Copy data and estimate max/min values 
       !

       maxn = 0
       lmax = 0.
       DO i=1,nexp 

         ll = 0

         DO l=1,maxstn

            IF ( ntimver_out == 1 ) THEN
             rnum = 0.
             bias = 0.
             rmse = 0.
              obs = 0.
             DO k = 1,ntimver
               rnum = rnum + FLOAT(stat(l)%s(i,j,k)%n)
               bias = bias +       stat(l)%s(i,j,k)%bias
               rmse = rmse +       stat(l)%s(i,j,k)%rmse
                obs = obs  +       stat(l)%s(i,j,k)%obs
             ENDDO
            ELSE
               rnum = FLOAT(stat(l)%s(i,j,kk)%n)
               bias =       stat(l)%s(i,j,kk)%bias
               rmse =       stat(l)%s(i,j,kk)%rmse
                obs =       stat(l)%s(i,j,kk)%obs
            ENDIF

            IF ( NINT(rnum) == 0 ) CYCLE

            ll = ll + 1

            SELECT CASE (ptype)
            CASE(0)
                dat(i,ll) = bias / MAX(rnum,1.)
            CASE(1)
                dat(i,ll) = SQRT (rmse / MAX(rnum,1.))
            CASE(2)

               IF ( ldiff ) THEN
                  dat(i,ll) = ( bias + obs ) / MAX(rnum,1.)
               ELSE
                  dat(i,ll) = ( bias + obs )
               ENDIF

               IF ( i == 1 ) THEN
                 IF ( ldiff ) THEN
                  dat(nexp+1,ll) = obs / rnum
                 ELSE
                  dat(nexp+1,ll) = obs
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

            maxn  = MAX(maxn,NINT(rnum))
            lmax  = MAX(lmax,ABS(dat(i,ll)))

         ENDDO

         IF ( i == 1 ) numstn = ll

       ENDDO 

       IF ( ALLOCATED(mlat))  DEALLOCATE(mlat,mlon,mdat,mcount)
       ALLOCATE(mlat(numstn),mlon(numstn),mdat(numstn),mcount(numstn))

       !
       ! Set the intervals
       !

       SELECT CASE(ptype)
       CASE(0)
          user_interval = ( ABS(map_bias_interval(1,j) - map_bias_interval(maxint+1,j) ) > 1.e-6 )
       CASE(1)
          user_interval = ( ABS(map_rmse_interval(1,j) - map_rmse_interval(maxint+1,j) ) > 1.e-6 )
       CASE(2)
          user_interval = ( ABS( map_obs_interval(1,j) -  map_obs_interval(maxint+1,j) ) > 1.e-6 )
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
             interval = map_bias_interval(:,j)
          CASE(1)
             interval = map_rmse_interval(:,j)
          CASE(2)
             interval = map_obs_interval(:,j)
          END SELECT

       ENDIF

       !
       ! Loop over all experiments
       !

       EXP_LOOP : DO i=1,nexp_plot 

         IF ( output_mode == 2 ) THEN
            IF ( ntimver_out == 1 ) THEN
               my_tag = TRIM(tag)//'_ALL'
            ELSE
               chour = ' '
               WRITE(chour,'(I2.2)')hour(kk)
               my_tag = TRIM(tag)//'_'//TRIM(chour)
            ENDIF
            my_tag = TRIM(my_tag)//'_'//TRIM(expname(i))
            CALL make_fname(prefix,period,stnr,     &
                 my_tag,obstype(j)(1:2),            &
                 obstype(j)(3:len_lab),             &
                 output_mode,output_type,           &
                 fname)
            CALL open_output(fname)
            CALL PSETR ('SUPER_PAGE_X_LENGTH',29.7)
            CALL PSETR ('SUPER_PAGE_Y_LENGTH',21.0)
         ENDIF

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
    CALL PSETC ('LEGEND_BOX_BLANKING','ON')

 CASE(1)
    ! Numbers
    CALL PSETC ('SYMBOL_TYPE','NUMBER') 
    CALL PSETC ('SYMBOL_TABLE_MODE','OFF')

 CASE DEFAULT
    WRITE(6,*)'No such option mtype',mtype
    CALL ABORT
 END SELECT
 ! Map
 CALL PSETC ('MAP_COASTLINE_COLOUR','BLACK') 
 CALL PSETC ('MAP_COASTLINE_RESOLUTION','HIGH') 
 CALL PSETC ('MAP_GRID_COLOUR','BLACK') 
 CALL PSETC ('SUBPAGE_MAP_PROJECTION',     map_projection     )
 CALL PSETC ('SUBPAGE_MAP_AREA_DEFINITION',map_area_definition)

 CALL PSETR ('SUBPAGE_MAP_VERTICAL_LONGITUDE',map_vertical_longitude   )
 CALL PSETR ('SUBPAGE_MAP_CENTRE_LATITUDE'  ,map_centre_latitude       )
 CALL PSETR ('SUBPAGE_MAP_CENTRE_LONGITUDE' ,map_centre_longitude      )
 CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE'  ,map_lower_left_latitude   )
 CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE' ,map_lower_left_longitude  )
 CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE' ,map_upper_right_latitude  )
 CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',map_upper_right_longitude )

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

         IF ( ntimver_out /= 1 ) THEN
            IF (lfcver) THEN
               WRITE(chour,'(I3.2,X,A1)')hour(kk),'H'
            ELSE
               WRITE(chour,'(I3.2,X,A3)')hour(kk),'UTC'
            ENDIF
            text = TRIM(text)//' at '//TRIM(chour)
         ENDIF
         text = TRIM(text)//'  '//TRIM(wtext)

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
         CALL PSETC ('TEXT_BOX_BLANKING','ON')
       
         CALL PSETR ('TEXT_BOX_Y_POSITION', 15.0)
         CALL PSETR ('TEXT_BOX_X_POSITION', 17.5)
         CALL PSETR ('TEXT_BOX_X_LENGTH',   10.0)
         CALL PSETR ('TEXT_BOX_Y_LENGTH',    3.0)

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

         IF ( ntimver_out == 1 ) THEN
            IF ( show_fc_length ) THEN
               CALL fclen_header(.TRUE.,maxfclenval,        &
                                 used_hours(j,per_ind,:),   &
                                 used_fclen(j,per_ind,:),   &
                                 wtext)
               CALL PSETI ('TEXT_LINE_COUNT', 4)
               CALL PSETC('TEXT_LINE_4',wtext)
            ENDIF
         ELSE
            CALL fclen_header(.NOT.lfcver,maxfclenval,   &
                              used_hours(j,per_ind,:),   &
                              used_fclen(j,per_ind,:),   &
                              wtext)
            CALL PSETI ('TEXT_LINE_COUNT', 4)
            CALL PSETC('TEXT_LINE_4',wtext)
         ENDIF

         CALL PTEXT
         IF ( output_mode == 2 ) CALL PCLOSE

       ENDDO EXP_LOOP
    ENDDO FC_LOOP  
 ENDDO PAR_LOOP 

 IF ( output_mode == 1 ) CALL PCLOSE

 IF ( ALLOCATED(mlat))  DEALLOCATE(mlat,mlon,mdat,mcount)
 DEALLOCATE(lat,lon,stn,dat)

 IF (ltiming) CALL acc_timing(timing_id,'plot_map')

 RETURN

END SUBROUTINE plot_map

