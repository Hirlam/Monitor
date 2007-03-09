SUBROUTINE plot_p_stat(lunout,ntim,npar,nr,nrun,     &
                       time_stat,par_active,period1,period2)

 USE types
 USE data, ONLY : ldiff

 IMPLICIT NONE

 INTEGER :: lunout,ntim,npar,nr,nrun,par_active(npar),period1,period2
 TYPE(stat_obs) :: time_stat(ntim)

 CALL plot_p_stat_diff(lunout,ntim,npar,nr,nrun,     &
                       time_stat,.false.,par_active,period1,period2)
 IF (ldiff ) CALL plot_p_stat_diff(lunout,ntim,npar,nr,nrun,   &
             time_stat,.true.,par_active,period1,period2)

 RETURN
END SUBROUTINE plot_p_stat

SUBROUTINE plot_p_stat_diff(lunout,ntim,npar,nr,nrun,   &
                            time_stat,ldiff,par_active,period1,period2)
 ! External modules

 USE types
 USE timing
 USE mymagics
 USE means
 USE data, ONLY : obstype,expname,err_ind,nexp,		 &
                  station_name,csi,	                 &
                  ltiming,                               &
                  show_fc_length,nfclengths,fclen,       &
                  timeserie_wind,sumup_tolerance,obint,  &
                  time_stat_fclen,                       &
                  copied_obs,copied_mod,                 &
                  show_rmse,show_stdv,show_bias,         &
                  ltemp,lev_lst,window_pos,output_type

 USE functions

 IMPLICIT NONE

 ! INPUT

 INTEGER :: lunout,ntim,npar,nr,nrun,par_active(npar),    &
            period1,period2
 TYPE(stat_obs) :: time_stat(ntim)
 LOGICAL :: ldiff

 ! local

 INTEGER :: i,ii,j,k,kk,l,              &
            timing_id,                  &
            ntim_use,dlen,mid(1),       &
            istart,iend,maxtim
 !          date(ntim),time(ntim),	&
 !          ndate(ntim),ntime(ntim),	&

 REAL :: miny,maxy,diff,       &
         rcount_max,           &
         rnum_min(0:nexp),rnum_max(0:nexp),rnum_ave(0:nexp), &
         data_min(0:nexp),data_max(0:nexp),data_ave(0:nexp), &
         rmse_min(0:nexp),rmse_max(0:nexp),rmse_ave(0:nexp), &
         stdv_min(0:nexp),stdv_max(0:nexp),stdv_ave(0:nexp)

 !       obs(ntim),            &
 !       rnum(ntim,nexp),      &
 !       bias(ntim,nexp),      &
 !       rmse(ntim,nexp),      &
 !       stdv(ntim,nexp),      &

 ! Allocatable

 REAL,    ALLOCATABLE :: obs(:),rnum(:,:),bias(:,:),rmse(:,:),stdv(:,:)
 INTEGER, ALLOCATABLE :: ndate(:),ntime(:),date(:),time(:)


 CHARACTER(LEN=3  ) :: cdum='   '
 CHARACTER(LEN=6  ) :: ob_short='      '
 CHARACTER(LEN=2  ) :: prefix=' '
 CHARACTER(LEN=30 ) :: fname=' '
 CHARACTER(LEN=120) :: wtext,wname


!-----------------------------------------------------
 ! Init timing counter
 timing_id = 0

 IF (ltiming) CALL acc_timing(timing_id,'plot_p_stat')

 ! Find start and endpoint

 IF ( period1 /= 0 ) THEN
    istart = 1
    iend   = ntim
    DO i=1,ntim
       IF ((time_stat(i)%date/100 - period1) == 0 ) THEN
               istart   = i 
               EXIT
       ENDIF
    ENDDO

    DO i=1,ntim
       IF ((time_stat(i)%date/100 - period2) == 0 ) THEN
          iend   = i
          EXIT
       ENDIF
    ENDDO
 ELSE
    istart = 1
    iend   = ntim
 ENDIF

 ! Create filename
 prefix ='ps'
 IF ( ldiff ) prefix='PS'
 CALL make_fname(prefix,period1,nr,nrun,fname,output_type)

 ! Write to timeserie file

 z_is_pressure = ( ltemp .AND. ( lev_lst(1) > lev_lst(2) ))

 CALL open_output(fname)

 ytitle = ' '

 IF ( SUM(timeserie_wind) /= 0 ) THEN
    IF ( MINVAL(timeserie_wind(1:npar)) == 0 ) THEN
    maxtim = get_maxtim(time_stat(istart)%date,time_stat(iend)%date,obint)
    ELSE
    maxtim = get_maxtim(time_stat(istart)%date,time_stat(iend)%date,MAX(obint,MINVAL(timeserie_wind(1:npar))))
    ENDIF
 ELSE
    maxtim = get_maxtim(time_stat(istart)%date,time_stat(iend)%date,obint)
 ENDIF
 maxtim = MAX(maxtim,ntim)

 ALLOCATE(ndate(maxtim),        &
          ntime(maxtim),        &
           date(maxtim),        &
           time(maxtim),        &
            obs(maxtim),        &
           bias(maxtim,nexp),   &
           rmse(maxtim,nexp),   &
           stdv(maxtim,nexp),   &
           rnum(maxtim,nexp))

 NPAR_LOOP : DO j=1,npar

    rnum = 0.
    bias = 0.
    rmse = 0.
    stdv = 0.
    obs  = 0.

    rnum_min = 0.
    rnum_max = 0.
    rnum_ave = 0.

    stdv_min = 0.
    stdv_max = 0.
    stdv_ave = 0.

    rmse_min = 0.
    rmse_max = 0.
    rmse_ave = 0.

    data_min = 0.
    data_max = 0.
    data_ave = 0.

    ! Copy

    DO k=1,nexp

       ii = 0
       DO i=istart,iend
       IF (time_stat(i)%n(j) /=0 ) THEN
          ii = ii + 1

          date(ii)   = time_stat(i)%date
          time(ii)   = time_stat(i)%time

          rnum(ii,k) = MAX(1.,float(time_stat(i)%n(j)))

          IF ( ldiff ) THEN
             bias(ii,k) =          time_stat(i)%bias(k,j)/rnum(ii,k)
             rmse(ii,k) = SQRT(    time_stat(i)%rmse(k,j)/rnum(ii,k) )
             stdv(ii,k) = SQRT(ABS(time_stat(i)%rmse(k,j)/rnum(ii,k) - &
                                  (time_stat(i)%bias(k,j)/rnum(ii,k))**2))

          ELSE
             bias(ii,k) = ( time_stat(i)%bias(k,j) +              &
                            time_stat(i)%obs(j)      ) / rnum(ii,k)
             rmse(ii,k) = bias(ii,k)  
             stdv(ii,k) = bias(ii,k)  

             IF ( k == 1 ) THEN
                obs(ii) = time_stat(i)%obs(j) / rnum(ii,k)
             ENDIF

          ENDIF

      ENDIF
      ENDDO

    ENDDO


    DO k=1,nexp

      ndate = date
      ntime = time
      dlen  = ii

      IF (timeserie_wind(j) /= 0 .AND. dlen /= 0 ) THEN

         IF ( k == 1 ) THEN

           CALL carefull_sumup(           &
           obs,ndate,ntime,               &
           ii,maxtim,timeserie_wind(j),dlen, &
           data_min(0),data_max(0),       &
           data_ave(0),ndate(1),00,       &
           sumup_tolerance,obint,         &
           err_ind,window_pos)

           ndate = date
           ntime = time


         ENDIF

           CALL carefull_sumup(           &
           rnum(:,k),ndate(:),ntime(:),   &
           ii,maxtim,timeserie_wind(j),dlen, &
           rnum_min(k),rnum_max(k),       &
           rnum_ave(k),ndate(1),00,       &
           sumup_tolerance,obint,         &
           err_ind,window_pos)

           ndate = date
           ntime = time

           CALL carefull_sumup(           &
           bias(:,k),ndate(:),ntime(:),   &
           ii,maxtim,timeserie_wind(j),dlen, &
           data_min(k),data_max(k),       &
           data_ave(k),ndate(1),00,       &
           sumup_tolerance,obint,         &
           err_ind,window_pos)

           IF ( ldiff ) THEN

              ndate = date
              ntime = time

              CALL carefull_sumup(           &
              rmse(:,k),ndate(:),ntime(:),   &
              ii,maxtim,timeserie_wind(j),dlen, &
              rmse_min(k),rmse_max(k),       &
              rmse_ave(k),ndate(1),00,       &
              sumup_tolerance,obint,         &
              err_ind,window_pos)

              ndate = date
              ntime = time

              CALL carefull_sumup(           &
              stdv(:,k),ndate(:),ntime(:),   &
              ii,maxtim,timeserie_wind(j),dlen, &
              stdv_min(k),stdv_max(k),       &
              stdv_ave(k),ndate(1),00,       &
              sumup_tolerance,obint,         &
              err_ind,window_pos)

           ENDIF

      ENDIF

    ENDDO

    ntim_use = MAX(dlen,1)


    ob_short = obstype(j)
    ob_short(3:6) = '   '
    CALL yunit(ob_short,ytitle)

    IF ( timeserie_wind(j) == 0 ) THEN
       miny = MINVAL(bias(1:ntim_use,:))
       maxy = MAXVAL(bias(1:ntim_use,:))
        
       IF ( ldiff ) THEN
          miny = MIN(miny,MINVAL(rmse(1:ntim_use,:)))
          maxy = MAX(maxy,MAXVAL(rmse(1:ntim_use,:)))
       ELSE
          miny = MIN(miny,MINVAL(obs(1:ntim_use)))
          maxy = MAX(maxy,MAXVAL(obs(1:ntim_use)))
       ENDIF
       
    ELSE 

       miny = MINVAL(data_min)
       maxy = MAXVAL(data_max)
       IF ( ldiff ) THEN
          miny = MIN(miny,MINVAL(rmse_min))
          maxy = MAX(maxy,MAXVAL(rmse_max))
       ENDIF

    ENDIF

    diff = tics(miny,maxy)
    miny = miny - 0.5*diff
    maxy = maxy + 0.5*diff

    IF (dlen == 0 ) THEN
       miny = 0.
       maxy = 0.
    ENDIF

    CALL new_page(ndate(1),ntime(1),				&
                  ndate(ntim_use),ntime(ntim_use),		&
                  0.,0.,miny,maxy)

    !
    ! Set title text
    !

    CALL PSETI('TEXT_LINE_COUNT',4)

    ! Line 1
    IF(ALLOCATED(station_name).AND. nr > 0 ) THEN
       WRITE(wtext,'(A20,A)')'Statistics for ',trim(station_name(csi))
    ELSE
       WRITE(wtext,'(A25,I8)')'Statistics for station ',nr
    ENDIF
    IF (nr.EQ.0) THEN
       wname=''
       WRITE(wname(1:5),'(I5)')par_active(j)
       wtext='Statistics for '//TRIM(wname)//' stations'
    ENDIF
    CALL PSETC('TEXT_LINE_1',wtext)

    ! Line 2
    CALL pname(obstype(j),wtext)
    CALL PSETC('TEXT_LINE_2',wtext)
    ! Line 3
    IF ( show_fc_length ) THEN

       IF (ANY(time_stat_fclen /= -1)) THEN
          mid = MINLOC(time_stat_fclen)
          wname='(A,XX(1X,I2.2))'
          WRITE(wname(4:5),'(I2.2)')mid-1
          WRITE(wtext,wname)'Forecast length used:',time_stat_fclen(1:mid(1)-1)
       ELSEIF (nfclengths > 10 ) THEN
          WRITE(cdum,'(I3)')fclen(nfclengths)
          WRITE(wname,'(I3,X,I3)')fclen(1:2)
          WRITE(wtext,*)'Forecast lengths used:'//TRIM(wname)//' ... '//TRIM(cdum)
       ELSE
          wname='(A,XX(1X,I2.2))'
          WRITE(wname(4:5),'(I2.2)')nfclengths
          WRITE(wtext,wname)'Forecast lengths used:',fclen(1:nfclengths)
       ENDIF

       IF ( timeserie_wind(j) /= 0 ) THEN
          wname = ' '
          WRITE(wname(1:3),'(I3)')timeserie_wind(j)
          wtext = TRIM(wtext)//'  Window:'//TRIM(wname)//'h'
       ENDIF

       CALL PSETC('TEXT_LINE_3',wtext)

       ! Line 3/4
       IF (ldiff) THEN
          wtext = 'Solid RMS; Dashed BIAS; Dashed grey is number of cases'
       ELSE
          wtext = ''
       ENDIF
       CALL PSETC('TEXT_LINE_4',wtext)
    ELSE
       IF (ldiff) THEN
          wtext = 'Solid RMS; Dashed BIAS; Dashed grey is number of cases'
       ELSE
          wtext = ''
       ENDIF
       CALL PSETI('TEXT_LINE_COUNT',3)
    ENDIF

    CALL PTEXT

    IF ( ldiff ) THEN

      IF ( show_rmse ) THEN
        CALL PSETC  ('LEGEND','ON')
        CALL PSETC  ('GRAPH_LINE_STYLE','SOLID')
        DO k=1,nexp
           CALL set_cdate(ndate(1:ntim_use),ntime(1:ntim_use),ntim_use,1)
           CALL plot_data(rmse(1:ntim_use,k),ntim_use,linecolor(k),expname(k),1)
        ENDDO
      ENDIF

      IF ( show_stdv ) THEN
        CALL PSETC  ('LEGEND','OFF')
        CALL PSETC  ('GRAPH_LINE_STYLE','DOT')
        DO k=1,nexp
           CALL set_cdate(ndate(1:ntim_use),ntime(1:ntim_use),ntim_use,1)
           CALL plot_data(stdv(1:ntim_use,k),ntim_use,linecolor(k),expname(k),1)
        ENDDO
      ENDIF

      IF ( show_bias ) THEN
        CALL PSETC  ('LEGEND','OFF')
        CALL PSETC  ('GRAPH_LINE_STYLE','DASH')
        DO k=1,nexp
           CALL set_cdate(ndate(1:ntim_use),ntime(1:ntim_use),ntim_use,1)
           CALL plot_data(bias(1:ntim_use,k),ntim_use,linecolor(k),expname(k),1)
        ENDDO
      ENDIF

    ELSE

        IF ( obstype(j)(1:2) == 'DD' ) THEN

           WHERE(obs(1:ntim_use) > 360. ) 
            obs(1:ntim_use) =  obs(1:ntim_use) - 360.
           ELSEWHERE(obs(1:ntim_use) < 0. ) 
            obs(1:ntim_use) =  obs(1:ntim_use) + 360.
           END WHERE

           WHERE(bias(1:ntim_use,:) > 360. ) 
            bias(1:ntim_use,:) =  bias(1:ntim_use,:) - 360.
           ELSEWHERE(bias(1:ntim_use,:) < 0. ) 
            bias(1:ntim_use,:) =  bias(1:ntim_use,:) + 360.
           END WHERE

        ENDIF

        CALL PSETC  ('LEGEND','ON')
        CALL PSETC  ('GRAPH_LINE_STYLE','SOLID')
        CALL set_cdate(ndate(1:ntim_use),ntime(1:ntim_use),ntim_use,1)
        IF (.NOT. copied_mod ) CALL plot_data(obs(1:ntim_use),ntim_use,linecolor(nexp+1),'OBS',1)
        IF ( .NOT. copied_obs) THEN
        DO k=1,nexp
           CALL set_cdate(ndate(1:ntim_use),ntime(1:ntim_use),ntim_use,1)
           CALL plot_data(bias(1:ntim_use,k),ntim_use,linecolor(k),expname(k),1)
        ENDDO
        ENDIF

!       DO k=1,ntim_use
!          WRITE(6,'(I4,I10,I3,3f9.2)')k,ndate(k),ntime(k),obs(k),bias(k,:)
!       ENDDO

!       IF ( .FALSE. ) THEN
!          CALL PSETC  ('LEGEND','OFF')
!          CALL PSETC  ('GRAPH_LINE_STYLE','DASH')
!          CALL kalman_filter(ntim_use,nexp,obs(1:ntim_use),bias(1:ntim_use,:))
!          DO k=1,nexp
!             CALL plot_data(bias(1:ntim_use,k),ntim_use,linecolor(k),expname(k),1)
!          ENDDO
!       ENDIF

    ENDIF


    ! Plot number of observations



    rcount_max = MAX(1.,MAXVAL(rnum(1:ntim_use,1)))

    IF ( ABS(rcount_max - 1. ) > 1.e-06 ) THEN

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
       CALL set_cdate(ndate(1:ntim_use),ntime(1:ntim_use),ntim_use,1)
       CALL plot_data(rnum(1:ntim_use,1),ntim_use,'GREY','Number of cases',1) 

    ENDIF

 ENDDO NPAR_LOOP

 CALL pclose
 
 DEALLOCATE(ndate,ntime,date,time,obs,bias,rmse,stdv,rnum)

 IF (ltiming) CALL acc_timing(timing_id,'plot_p_stat')

RETURN
END 
