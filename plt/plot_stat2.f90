SUBROUTINE plot_stat2(lunout,nexp,nparver,ntimver,   &
                      s,stnr,yymm,yymm2,par_active,  &
                      uh,uf)

 USE types, ONLY : statistics
 USE mymagics
 USE functions
 USE timing
 USE data, ONLY : obstype,expname,station_name,                 &
                  csi,use_fclen,lfcver,                         &
                  maxfclenval,len_lab,output_mode,              &
                  nfclengths,nuse_fclen,tag,                    &
                  timdiff,time_shift,show_fc_length,ltiming,    &
                  show_bias,show_rmse,show_stdv,show_obs,       &
                  copied_obs,copied_mod,period_freq,period_type,&
                  output_type

 IMPLICIT NONE

 INTEGER, INTENT(IN) ::           &
 lunout,nexp,nparver,ntimver,     &
 stnr,yymm,yymm2,                 &
 par_active(nparver)

 TYPE (statistics), INTENT(IN) :: s(nexp,nparver,ntimver)

 LOGICAL, INTENT(IN) :: uh(nparver,0:23),uf(nparver,0:maxfclenval)

! Local

 INTEGER :: i,j,k,timing_id,ntimver_l, &
            period

 REAL, ALLOCATABLE ::        &
            bias(:,:),       &
             obs(:,:),       &
            rmse(:,:),       &
            stdv(:,:),       &
            rnum(:,:),       &
            hour(:)
 REAL :: miny,maxy,diff,           &
         rcount_max

 LOGICAL :: legend_done    = .FALSE.
 LOGICAL :: no_data_at_all = .FALSE.

 CHARACTER(LEN=100) :: wtext=' '
 CHARACTER(LEN=100) :: wtext1=' '
 CHARACTER(LEN=100) :: fname=' '
 CHARACTER(LEN= 30) :: wname=' '
 CHARACTER(LEN=  1) :: prefix = ' '
 CHARACTER(LEN=  6) :: ob_short = '      '

!------------------------------------------
 ! Init timing counter
 timing_id = 0
 IF (ltiming) CALL acc_timing(timing_id,'plot_stat2')

 IF ( show_obs ) THEN
    show_rmse = .FALSE. 
    show_bias = .FALSE. 
    show_stdv = .FALSE. 
 ENDIF

 ! Set output filename

 prefix = 'v'
 IF (lfcver) prefix = 'V'
 IF (yymm < 999999 ) THEN
    period = yymm
 ELSE
    period = 0
 ENDIF


 ! Set number of hours

 IF (lfcver) THEN
   ntimver_l = nuse_fclen
 ELSE 
   ntimver_l = ntimver + 1
 ENDIF

 ALLOCATE(bias(nexp,ntimver_l),         &
          rmse(nexp,ntimver_l),         &
          stdv(nexp,ntimver_l),         &
          rnum(nexp,ntimver_l),         &
           obs(nexp,ntimver_l),         &
          hour(ntimver_l))

 IF (lfcver) THEN
    hour(1:ntimver_l)=use_fclen(1:ntimver_l)
 ELSE
    DO i=1,ntimver
       hour(i)=(i-1)*timdiff + time_shift
    ENDDO
    hour(ntimver_l) = 24
 ENDIF


 ! Plotting

 IF ( output_mode == 1 ) THEN
    CALL make_fname(prefix,period,stnr,tag,      &
                    prefix,'0',             &
                    output_mode,output_type,&
                    fname)
    CALL open_output(fname)
 ENDIF


 DO j=1,nparver

    IF ( output_mode == 2 ) THEN
       CALL make_fname(prefix,period,stnr,tag,          &
                       obstype(j)(1:2),            &
                       obstype(j)(3:len_lab),      &
                       output_mode,output_type,    &
                       fname)
       CALL open_output(fname)
    ENDIF

    CALL PSET1R ('GRAPH_CURVE_X_VALUES',hour(1:ntimver_l),ntimver_l)

    no_data_at_all = (MAXVAL(s(:,j,:)%n) == 0)


    IF ( no_data_at_all ) THEN
       miny = 0.
       maxy = 1.
       rnum = 0.
        obs = 0.
       bias = 0.
       rmse = 0.
       stdv = 0.
    ELSE

    DO i=1,nexp
    DO k=1,ntimver
       rnum(i,k) = MAX(1.,float(s(i,j,k)%n))
        obs(i,k) =      s(i,j,k)%obs /rnum(i,k)
       bias(i,k) =      s(i,j,k)%bias/rnum(i,k)
       rmse(i,k) = sqrt(s(i,j,k)%rmse/rnum(i,k))
       stdv(i,k) = sqrt(ABS(s(i,j,k)%rmse/rnum(i,k) - (s(i,j,k)%bias/rnum(i,k))**2))
    ENDDO
    ENDDO

    IF (.NOT.lfcver) THEN
       rnum(:,ntimver_l) = rnum(:,1)
        obs(:,ntimver_l) =  obs(:,1)
       bias(:,ntimver_l) = bias(:,1)
       rmse(:,ntimver_l) = rmse(:,1)
       stdv(:,ntimver_l) = stdv(:,1)
    ENDIF

    IF ( show_obs) bias = bias + obs
    
    miny = MINVAL(bias)
    maxy = MAXVAL(bias)

    IF (show_obs ) THEN
       miny = MIN(miny,MINVAL(obs))
       maxy = MAX(maxy,MAXVAL(obs))
    ELSE
       miny = MIN(miny,MINVAL(rmse))
       maxy = MAX(maxy,MAXVAL(rmse))
    ENDIF

    diff = tics(miny,maxy)
    miny = miny - 0.5*diff
    maxy = maxy + 0.5*diff

    ENDIF ! no_data_at_all

    ob_short=obstype(j)
    ob_short(3:6)='    '   
    CALL yunit(ob_short,ytitle)
    plotfcver   = lfcver
    plottimdiff = FLOAT(timdiff)
    CALL new_page(0,0,0,0,hour(1),hour(ntimver_l),miny,maxy)

    ! Set title text

    CALL PSETI('TEXT_LINE_COUNT',4)

    ! Line 1
    IF(ALLOCATED(station_name)) THEN
       wtext='Station: '//TRIM(station_name(csi))
    ELSE
       WRITE(wtext(1:8),'(I8)')stnr
       wtext='Station: '//TRIM(wtext(1:8))
    ENDIF

    IF (stnr == 0) THEN
       wtext='     stations'
       WRITE(wtext(1:4),'(I4)')par_active(j)
       IF ( TRIM(tag) /= '#' ) wtext = TRIM(wtext)//' Area:'//TRIM(tag)
    ENDIF
    
    CALL PSETC('TEXT_LINE_1',wtext)

    ! Line 2
    IF (yymm == 0 ) THEN
    ELSEIF(yymm < 13) THEN

       SELECT CASE(period_freq) 
       CASE(1)
        WRITE(wtext,'(A8,A8)')'Period: ',seasonal_name2(yymm)
       CASE(3)
        WRITE(wtext,'(A8,A8)')'Period: ',seasonal_name1(yymm)
       END SELECT 

    ELSEIF(yymm < 9999 .OR. (period_type == 2 .AND. period_freq == 1)) THEN
       WRITE(wtext,'(A8,I8)')'Period: ',yymm
    ELSEIF(yymm < 999999 ) THEN
       WRITE(wtext,'(A8,I6,A2,I6)')'Period: ',        &
       yymm,' -',monincr(yymm,period_freq-1)
    ELSE
       WRITE(wtext,'(A8,I8,A1,I8)')'Period: ',        &
       yymm,'-',yymm2
    ENDIF
    CALL PSETC('TEXT_LINE_2',wtext)

    ! Line 3
    IF ( show_fc_length ) THEN
       CALL pname(obstype(j),wtext)
       CALL fclen_header(( .NOT. lfcver .OR. ( nuse_fclen /= nfclengths )), &
                         maxfclenval,uh(j,:),uf(j,:),wtext1)
       wtext = TRIM(wtext)//'   '//TRIM(wtext1)
       CALL PSETC('TEXT_LINE_3',wtext)

       ! Line 3/4
       wtext =''
       IF (show_rmse) wtext = TRIM(wtext)//'Solid RMS; '
       IF (show_stdv) wtext = TRIM(wtext)//' Dotted STDV; '
       IF (show_bias) wtext = TRIM(wtext)//' Dashed BIAS;'
       wtext = TRIM(wtext)//' Dashed grey is number of cases'
       CALL PSETC('TEXT_LINE_4',wtext)
    ELSE
       wtext =''
       IF (show_rmse) wtext = TRIM(wtext)//'Solid RMS; '
       IF (show_stdv) wtext = TRIM(wtext)//' Dotted STDV; '
       IF (show_bias) wtext = TRIM(wtext)//' Dashed BIAS;'
       wtext = TRIM(wtext)//' Dashed grey is number of cases'
       CALL PSETC('TEXT_LINE_3',wtext)
       CALL PSETI('TEXT_LINE_COUNT',3)
    ENDIF

    CALL ptext
    

    ! Do the plotting
    legend_done = .FALSE.
    CALL PSETC  ('LEGEND','ON')

    IF ( show_obs ) THEN
       IF ( legend_done ) CALL PSETC  ('LEGEND','OFF')
       CALL PSETC  ('GRAPH_LINE_STYLE','SOLID')
       IF (.NOT. copied_mod ) &
       CALL plot_data( obs(1,:),ntimver_l,linecolor(nexp+1),'OBS',1)
       IF (.NOT. copied_obs ) THEN
          DO i=1,nexp
             CALL plot_data(bias(i,:),ntimver_l,linecolor(i),expname(i),1)
          ENDDO
       ENDIF
       legend_done = .TRUE.
    ENDIF

    IF ( show_rmse ) THEN
       IF ( legend_done ) CALL PSETC  ('LEGEND','OFF')
       DO i=1,nexp
          CALL PSETC  ('GRAPH_LINE_STYLE','SOLID')
          CALL plot_data( rmse(i,:),ntimver_l,linecolor(i),expname(i),1)
       ENDDO
       legend_done = .TRUE.
    ENDIF

    IF ( show_bias ) THEN
       IF ( legend_done ) CALL PSETC  ('LEGEND','OFF')
       DO i=1,nexp
          CALL PSETC  ('GRAPH_LINE_STYLE','DASH')
          CALL plot_data(bias(i,:),ntimver_l,linecolor(i),expname(i),1)
       ENDDO
       legend_done = .TRUE.
    ENDIF

    IF ( show_stdv ) THEN
       IF ( legend_done ) CALL PSETC  ('LEGEND','OFF')
       DO i=1,nexp
       CALL PSETC  ('GRAPH_LINE_STYLE','DOT')
          CALL plot_data(stdv(i,:),ntimver_l,linecolor(i),expname(i),1)
       ENDDO
       legend_done = .TRUE.
    ENDIF

    ! Plot number of observations

    DO i=1,1
    rcount_max = MAX(1.,MAXVAL(rnum(i,:)))
    rcount_max = rcount_max + NINT(rcount_max)/10
    CALL PSETC ('AXIS_ORIENTATION','VERTICAL')
    CALL preset('AXIS_TICK_INTERVAL')
    CALL PSETC ('AXIS_TITLE','ON')
    CALL PSETC ('AXIS_GRID','OFF')
    CALL PSETC ('AXIS_TITLE_TEXT','Number of cases')
    CALL PSETC ('AXIS_TYPE','REGULAR')
    CALL PSETC ('AXIS_POSITION','RIGHT')
    CALL PSETR ('AXIS_MIN_VALUE',0)
    CALL PSETR ('AXIS_MAX_VALUE',rcount_max)
    CALL PAXIS

    CALL PSETC  ('GRAPH_LINE_STYLE','DASH')
    CALL PSETI  ('GRAPH_LINE_THICKNESS',  2)
    CALL plot_data(rnum(i,:),ntimver_l,'GREY','Number of cases',1) 
    ENDDO

    IF ( output_mode == 2 ) CALL pclose

 ENDDO

 IF ( output_mode == 1 ) CALL pclose

 IF (ltiming) CALL acc_timing(timing_id,'plot_stat2')

 RETURN

END SUBROUTINE plot_stat2
