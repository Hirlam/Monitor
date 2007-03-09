SUBROUTINE plot_stat2(lunout,nexp,nparver,ntimver,s,stnr,yymm,yymm2,par_active)

 USE types, ONLY : statistics
 USE mymagics
 USE functions
 USE timing
 USE data, ONLY : obstype,expname,station_name,                 &
                  csi,fclen,lfcver,nrun,                        &
                  nfclengths,                     &
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

! Local

 INTEGER :: i,j,k,timing_id,ntimver_l
 REAL, ALLOCATABLE ::        &
            bias(:,:),       &
             obs(:,:),       &
            rmse(:,:),       &
            stdv(:,:),       &
            rnum(:,:),       &
            hour(:)
 REAL :: miny,maxy,diff,           &
         rcount_max

 LOGICAL :: legend_is_on = .FALSE.

 CHARACTER(LEN=100) :: wtext=' '
 CHARACTER(LEN=100) :: wtext1=' '
 CHARACTER(LEN= 30) :: fname=' '
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
    CALL make_fname(prefix,yymm,stnr,nrun,fname,output_type)
 ELSE
    CALL make_fname(prefix,0,stnr,nrun,fname,output_type)
 ENDIF

 ! Set number of hours

 IF (lfcver) THEN
   ntimver_l = ntimver
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
    hour(1:ntimver_l)=fclen(1:ntimver_l)
 ELSE
    DO i=1,ntimver
       hour(i)=(i-1)*timdiff + time_shift
    ENDDO
    hour(ntimver_l) = 24
 ENDIF


 ! Plotting

 CALL open_output(fname)
 !CALL popen
 !CALL PSETC ('PS_DEVICE','ps_a4')
 !CALL PSETC ('PS_FILE_NAME',fname)

 CALL PSET1R ('GRAPH_CURVE_X_VALUES',hour(1:ntimver_l),ntimver_l)

 DO j=1,nparver

    IF (MAXVAL(s(:,j,:)%n) == 0) CYCLE

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
       WRITE(wtext,'(A20,A)')'Statistics for ',trim(station_name(csi))
    ELSE
       WRITE(wtext,'(A25,I8)')'Statistics for station ',stnr
    ENDIF
    IF (stnr.EQ.0) THEN
       wtext='Statistics for      stations'
       WRITE(wtext(16:19),'(I4)')par_active(j)
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
       IF (.NOT. lfcver) THEN
          IF (nfclengths > 10 ) THEN
             wname='(A,2I3.2,A5,I2.2)'
             WRITE(wtext1,wname)'Forecast lengths used:',   &
             fclen(1:2),' ... ',fclen(nfclengths)
          ELSE
             wname='(A,XX(1X,I2.2))'
             WRITE(wname(4:5),'(I2.2)')nfclengths
             WRITE(wtext1,wname)'Forecast lengths used:',fclen(1:nfclengths)
          ENDIF
          wtext = TRIM(wtext)//'   '//TRIM(wtext1)
       ENDIF
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
    legend_is_on = .FALSE.

    IF ( show_obs ) THEN
    CALL PSETC  ('LEGEND','ON')
    CALL PSETC  ('GRAPH_LINE_STYLE','SOLID')
    IF (.NOT. copied_mod ) CALL plot_data( obs(1,:),ntimver_l,linecolor(nexp+1),'OBS',1)
    IF (.NOT. copied_obs ) THEN
    DO i=1,nexp
       CALL plot_data(bias(i,:),ntimver_l,linecolor(i),expname(i),1)
    ENDDO
    legend_is_on = .TRUE.
    ENDIF
    ENDIF

    IF ( show_rmse ) THEN
    CALL PSETC  ('LEGEND','ON')
    DO i=1,nexp
       CALL PSETC  ('GRAPH_LINE_STYLE','SOLID')
       CALL plot_data( rmse(i,:),ntimver_l,linecolor(i),expname(i),1)
    ENDDO
    legend_is_on = .TRUE.
    ENDIF

    IF ( show_bias ) THEN

       IF ( legend_is_on ) THEN
          CALL PSETC  ('LEGEND','OFF')
       ELSE
          CALL PSETC  ('LEGEND','ON')
       ENDIF
       legend_is_on = .TRUE.

    DO i=1,nexp
       CALL PSETC  ('GRAPH_LINE_STYLE','DASH')
       CALL plot_data(bias(i,:),ntimver_l,linecolor(i),expname(i),1)
    ENDDO
    ENDIF

    IF ( show_stdv ) THEN
       IF ( legend_is_on ) THEN
          CALL PSETC  ('LEGEND','OFF')
       ELSE
          CALL PSETC  ('LEGEND','ON')
       ENDIF
       legend_is_on = .TRUE.

       DO i=1,nexp
       CALL PSETC  ('GRAPH_LINE_STYLE','DOT')
          CALL plot_data(stdv(i,:),ntimver_l,linecolor(i),expname(i),1)
       ENDDO

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


 ENDDO

 CALL pclose

 IF (ltiming) CALL acc_timing(timing_id,'plot_stat2')

 RETURN

END SUBROUTINE plot_stat2
