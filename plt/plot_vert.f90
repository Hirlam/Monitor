SUBROUTINE plot_vert(lunout,nexp,nlev,nparver,ntimver,     &
                     s,stnr,yymm,yymm2,par_active,uh,uf)

 !
 ! Plot vertical profile of RMS,STD and BIAS 
 ! Vertical coordinate is assumed to be pressure
 !
 ! Ulf Andrae, SMHI, 2005
 !

 USE types, ONLY : statistics
 USE mymagics
 USE functions
 USE timing
 USE data, ONLY : obstype,expname,station_name,                 &
                  csi,lfcver,maxfclenval,                       &
                  lev_lst,ltemp,nfclengths,                     &
                  show_fc_length,ltiming,tag,                   &
                  show_bias,show_rmse,show_stdv,show_obs,       &
                  len_lab,period_freq,period_type,              &
                  output_type,output_mode,                      &
                  show_times,use_fclen,timdiff,time_shift,      &
                  z_is_pressure,len_lab


 IMPLICIT NONE

 INTEGER,           INTENT(IN) :: lunout,nexp,nlev,nparver,     &
                                  ntimver,stnr,yymm,yymm2,      &
                                  par_active(nparver)
 TYPE (statistics), INTENT(IN) :: s(nexp,nparver,ntimver)

 LOGICAL,           INTENT(IN) :: uh(nparver,0:23),uf(nparver,0:maxfclenval)

! Local

 INTEGER :: i,j,jj,j_ind,k,kk,timing_id,    &
            ntimver_out,hour(ntimver),      &
            period,jl(1)

 REAL    :: bias(nexp,nlev),       &
            rmse(nexp,nlev),       &
            stdv(nexp,nlev),       &
            rnum(nexp,nlev),       &
             obs(nexp,nlev),       &
            miny,maxy,diff,        &
            lev(nlev),             &
            rcount_max

 LOGICAL :: legend_done = .FALSE.

 CHARACTER(LEN=100      ) :: wtext =' ',wtext1=' ',my_tag
 CHARACTER(LEN=100      ) :: fname =' ',wname =' '
 CHARACTER(LEN=len_lab  ) :: ob_short=''
 CHARACTER(LEN=7  ) :: cnum_case = '       '
 CHARACTER(LEN=1  ) :: prefix    = ' '
 CHARACTER(LEN=10 ) :: chour    = ' '

 LOGICAL, ALLOCATABLE :: ldum(:)

!------------------------------------------
 ! Init timing counter
 timing_id = 0
 IF (ltiming) CALL acc_timing(timing_id,'plot_vert')

 IF ( show_obs ) THEN
    show_rmse = .FALSE.
    show_bias = .FALSE.
    show_stdv = .FALSE.
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

 !
 ! Set output filename
 !

 prefix = 'l'
 IF (lfcver) prefix = 'L'
 IF (yymm < 999999 ) THEN
    period = yymm
 ELSE
    period = 0
 ENDIF

 ! Set vertical levels
 lev = lev_lst(1:nlev)

 ! Plotting

 IF ( output_mode == 1 ) THEN
    CALL make_fname(prefix,period,stnr,tag,  &
                    prefix,'0',              &
                    output_mode,output_type, &
                    fname)
    CALL open_output(fname)
 ENDIF



 DO j=nlev,nparver,nlev
  DO kk=1,ntimver_out

    IF ( ntimver_out /= 1 .AND. .NOT. ANY( show_times == hour(kk) )) CYCLE

    rnum = 0.
    bias = 0.
    rmse = 0.
    obs  = 0.

    DO i = 1,nexp
    DO jj= 1,nlev
       j_ind = (j/nlev-1)*nlev + jj
       IF ( ntimver_out == 1 ) THEN
        DO k = 1,ntimver
          rnum(i,jj) = rnum(i,jj) + float(s(i,j_ind,k)%n)
          bias(i,jj) = bias(i,jj) +       s(i,j_ind,k)%bias
          rmse(i,jj) = rmse(i,jj) +       s(i,j_ind,k)%rmse
           obs(i,jj) =  obs(i,jj) +       s(i,j_ind,k)%obs
        ENDDO
       ELSE
          rnum(i,jj) = rnum(i,jj) + float(s(i,j_ind,kk)%n)
          bias(i,jj) = bias(i,jj) +       s(i,j_ind,kk)%bias
          rmse(i,jj) = rmse(i,jj) +       s(i,j_ind,kk)%rmse
           obs(i,jj) =  obs(i,jj) +       s(i,j_ind,kk)%obs
       ENDIF
    ENDDO
    ENDDO

    rnum = MAX(1.,rnum)

    stdv = SQRT(ABS(rmse/rnum - (bias/rnum)**2))
    IF ( show_obs ) THEN
       bias = (  bias + obs ) / rnum
       obs  = obs / rnum
    ELSE
       bias = bias / rnum
    ENDIF
    rmse = SQRT(rmse/rnum)

    miny = MINVAL(bias)
    maxy = MAXVAL(bias)

    IF (show_obs ) THEN
       miny = MIN(miny,MINVAL(obs))
       maxy = MAX(maxy,MAXVAL(obs))
    ELSE
       IF(show_bias) THEN
          miny = MIN(miny,MINVAL(bias))
          maxy = MAX(maxy,MAXVAL(bias))
       ENDIF
       IF(show_rmse) THEN
          miny = MIN(miny,MINVAL(rmse))
          maxy = MAX(maxy,MAXVAL(rmse))
       ENDIF
       IF(show_stdv) THEN
          miny = MIN(miny,MINVAL(stdv))
          maxy = MAX(maxy,MAXVAL(stdv))
       ENDIF
    ENDIF

    diff = tics(miny,maxy)
    miny = miny - 0.5*diff
    maxy = maxy + 0.5*diff

    ob_short = obstype(j)
    ob_short(3:6) = '    '
    CALL yunit(ob_short,ytitle)

    plotfcver     = lfcver
    plotltemp     = ltemp
    z_is_pressure = ( lev_lst(1) > lev_lst(nlev) )

    IF ( output_mode == 2 ) THEN

       IF ( ntimver_out == 1 ) THEN
          my_tag = TRIM(tag)//'_ALL'
       ELSE
          chour = ' '
          WRITE(chour,'(I2.2)')hour(kk)
          my_tag = TRIM(tag)//'_'//TRIM(chour)
       ENDIF
       CALL make_fname(prefix,period,stnr,my_tag,    &
                       obstype(j)(1:2),'0',          &
                       output_mode,output_type,      &
                       fname)

       CALL open_output(fname)

    ENDIF

    CALL PSET1R ('GRAPH_CURVE_Y_VALUES',lev,nlev)
    CALL new_page(0,0,0,0,miny,maxy,lev_lst(1),lev_lst(nlev))

    !
    ! Set title text
    !

    CALL PSETI('TEXT_LINE_COUNT',3)

    ! Line 1
    IF(ALLOCATED(station_name)) THEN
       wtext='Station: '//trim(station_name(csi))
    ELSE
       WRITE(wtext(1:8),'(I8)')stnr
       wtext='Station: '//trim(wtext(1:8))
    ENDIF

    IF (stnr == 0) THEN
       wtext='Statistics for      stations'
       j_ind = (j/nlev-1)*nlev + 1
       jj = MAXVAL(par_active(j_ind:j_ind+nlev-1))
       WRITE(wtext(1:4),'(I4)')jj
       wtext=TRIM(wtext(1:4))//' stations'
       IF ( TRIM(tag) /= '#' ) wtext=TRIM(wtext)//' Area: '//TRIM(tag)
    ENDIF

    CALL PSETC('TEXT_LINE_1',wtext)

    ! Line 2
    ob_short = obstype(j)
    ob_short(3:6) = '    '
    CALL pname(ob_short,wtext)
     
    ! Line 2
    IF ( yymm == 0 ) THEN
    ELSEIF(yymm < 13) THEN

       SELECT CASE(period_freq) 
       CASE(1)
        WRITE(wtext1,'(A8,A8)')'Period: ',seasonal_name2(yymm)
       CASE(3)
        WRITE(wtext1,'(A8,A8)')'Period: ',seasonal_name1(yymm)
       END SELECT 

       ELSEIF(yymm < 9999 .OR. ( period_type == 2 .AND. period_freq == 1)) THEN
       WRITE(wtext1,'(A8,I6)')'Period: ',yymm
    ELSEIF(yymm < 999999 ) THEN
       WRITE(wtext1,'(A8,I6,A2,I6)')'Period: ',        &
       yymm,' -',monincr(yymm,period_freq-1)
    ELSE
       WRITE(wtext1,'(A8,I8,A1,I8)')'Period: ',        &
       yymm,'-',yymm2
    ENDIF
    wtext = TRIM(wtext)//'  '//TRIM(wtext1)
    CALL PSETC('TEXT_LINE_2',wtext)

    ! Line 3
    wtext = ' '
    IF ( show_rmse ) wtext = TRIM(wtext)//' Soild RMS;'
    IF ( show_stdv ) wtext = TRIM(wtext)//' Dotted STDV;'
    IF ( show_bias ) wtext = TRIM(wtext)//' Dashed BIAS;'
    wtext = TRIM(wtext)//' Dashed grey is number of cases'
    CALL PSETC('TEXT_LINE_3',wtext)

    ! Line 4
    ! First find corret index for fclenth usage
    j_ind = (j/nlev-1)*nlev + 1
    jl    = MAXLOC(par_active(j_ind:j_ind+nlev-1)) + j_ind - 1
    IF ( ntimver_out == 1 ) THEN
       IF ( show_fc_length ) THEN
          CALL PSETI('TEXT_LINE_COUNT',4)
          CALL fclen_header(.TRUE.,maxfclenval,uh(jl,:),uf(jl,:),wtext)
          CALL PSETC('TEXT_LINE_4',wtext)
       ENDIF
    ELSE
       IF (lfcver) THEN
          ALLOCATE(ldum(0:hour(kk)))
          ldum           = .FALSE.
          ldum(hour(kk)) = .TRUE.
          CALL fclen_header(.TRUE.,hour(kk),uh(jl,:),ldum,wtext)
          DEALLOCATE(ldum)
       ELSE
          WRITE(chour,'(I3.2,X,A3)')hour(kk),'UTC'
          CALL fclen_header(.TRUE.,maxfclenval,uh(jl,:),uf(jl,:),wtext)
          wtext = 'Statistics at '//chour   //'  '//TRIM(wtext)
       ENDIF
       CALL PSETI('TEXT_LINE_COUNT',4)
       CALL PSETC('TEXT_LINE_4',wtext)
    ENDIF


    CALL ptext

    ! Do the plotting

    legend_done = .FALSE.
    CALL PSETC  ('LEGEND','ON')

    IF ( show_obs ) THEN
       IF ( legend_done ) CALL PSETC  ('LEGEND','OFF')
       CALL plot_y_data(  obs(1,1:nlev),nlev,linecolor(nexp+i),'OBS',1)
       DO i=1,nexp
          CALL PSETC  ('GRAPH_LINE_STYLE','SOLID')
          CALL plot_y_data( bias(i,1:nlev),nlev,linecolor(i),expname(i),1)
       ENDDO
       legend_done = .TRUE.
    ENDIF

    IF ( show_rmse ) THEN
       IF ( legend_done ) CALL PSETC  ('LEGEND','OFF')
       DO i=1,nexp
          CALL PSETC  ('GRAPH_LINE_STYLE','SOLID')
          CALL plot_y_data( rmse(i,1:nlev),nlev,linecolor(i),expname(i),1)
       ENDDO
       legend_done = .TRUE.
    ENDIF

    IF ( show_bias ) THEN
       IF ( legend_done ) CALL PSETC  ('LEGEND','OFF')
       DO i=1,nexp
          CALL PSETC  ('GRAPH_LINE_STYLE','DASH')
          CALL plot_y_data(bias(i,1:nlev),nlev,linecolor(i),expname(i),1)
       ENDDO
       legend_done = .TRUE.
    ENDIF

    IF ( show_stdv ) THEN
       IF ( legend_done ) CALL PSETC  ('LEGEND','OFF')
       DO i=1,nexp
          CALL PSETC  ('GRAPH_LINE_STYLE','DOT')
          CALL plot_y_data(stdv(i,1:nlev),nlev,linecolor(i),expname(i),1)
       ENDDO
       legend_done = .TRUE.
    ENDIF

    ! Plot observations

    rcount_max = MAX(1.,MAXVAL(rnum(1,:)))
    CALL PSETC ('LEGEND','OFF')
    CALL PSETC ('AXIS_ORIENTATION','HORIZONTAL')
    CALL PRESET('AXIS_TICK_INTERVAL')
    CALL PSETC ('AXIS_TITLE','ON')
    CALL PSETC ('AXIS_GRID','OFF')
    CALL PSETC ('AXIS_TITLE_TEXT','Number of cases')
    CALL PSETC ('AXIS_TITLE_TEXT',' ')
    CALL PSETC ('AXIS_TYPE','REGULAR')
    CALL PSETC ('AXIS_POSITION','TOP')
    CALL PSETR ('AXIS_MIN_VALUE',0)
    CALL PSETR ('AXIS_MAX_VALUE',rcount_max*1.05)
    CALL PAXIS

    CALL PSETC ('GRAPH_LINE_STYLE','DASH')
    CALL plot_y_data(rnum(1,1:nlev),nlev,'GREY','OBS',1)

    IF ( output_mode == 2 ) CALL pclose

 ENDDO
 ENDDO

 IF ( output_mode == 1 ) CALL pclose

 z_is_pressure = .FALSE.
 plotltemp     = .FALSE.

 IF (ltiming) CALL acc_timing(timing_id,'plot_vert')

 RETURN

END SUBROUTINE plot_vert
