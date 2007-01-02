SUBROUTINE print_p_stat(lunout,ntim,npar,nr,nrun,     &
                       time_stat,par_active,period1,period2)

 USE types
 USE data, ONLY : ldiff

 IMPLICIT NONE

 INTEGER :: lunout,ntim,npar,nr,nrun,par_active(npar),period1,period2
 TYPE(stat_obs) :: time_stat(ntim)

 CALL print_p_stat_diff(lunout,ntim,npar,nr,nrun,     &
                       time_stat,.false.,par_active,period1,period2)
 IF (ldiff ) CALL print_p_stat_diff(lunout,ntim,npar,nr,nrun,   &
             time_stat,.true.,par_active,period1,period2)

 RETURN
END SUBROUTINE print_p_stat

SUBROUTINE print_p_stat_diff(lunout,ntim,npar,nr,nrun,   &
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
                  time_stat_fclen,dd_ind,                &
                  copied_obs,copied_mod,                 &
                  show_rmse,show_stdv,show_bias,         &
                  ltemp,lev_lst,window_pos

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

 REAL :: miny,maxy,diff,       &
         rcount_max,           &
         rnum_min(0:nexp),rnum_max(0:nexp),rnum_ave(0:nexp), &
         data_min(0:nexp),data_max(0:nexp),data_ave(0:nexp), &
         rmse_min(0:nexp),rmse_max(0:nexp),rmse_ave(0:nexp), &
         stdv_min(0:nexp),stdv_max(0:nexp),stdv_ave(0:nexp)

 ! Allocatable

 REAL,    ALLOCATABLE :: obs(:),rnum(:,:),bias(:,:),rmse(:,:),stdv(:,:)
 INTEGER, ALLOCATABLE :: ndate(:),ntime(:),date(:),time(:)


 CHARACTER(LEN= 30) :: cform=''
 CHARACTER(LEN=3  ) :: cdum='   '
 CHARACTER(LEN=2  ) :: prefix=' '
 CHARACTER(LEN=100) :: fname=' '
 CHARACTER(LEN=120) :: wtext,wname


!-----------------------------------------------------
 ! Init timing counter
 timing_id = 0

 IF (ltiming) CALL acc_timing(timing_id,'print_p_stat')

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
 CALL make_fname(prefix,period1,nr,nrun,fname,0)

 OPEN(lunout,NAME=fname)

 ! Write to timeserie file

 IF ( SUM(timeserie_wind) /= 0 ) THEN
    maxtim = get_maxtim(time_stat(istart)%date,time_stat(iend)%date,MINVAL(timeserie_wind))
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

      IF (timeserie_wind(j) /= 0) THEN

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



    IF ( j == dd_ind ) THEN

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

    cform = '(I10,I3.2,NN(x,en13.3e2))'
    WRITE(cform(11:12),'(I2.2)')(nexp+1)
    DO i=1,ntim_use
       IF ( (ABS(obs(i) - err_ind ) < 1.e-6) &
            .AND. ( i == 1 .OR. i == ntim_use ) ) CYCLE
       WRITE(lunout,cform)ndate(i),ntime(i),obs(i),bias(i,:)
    ENDDO

    CLOSE(lunout)

 ENDDO NPAR_LOOP

 DEALLOCATE(ndate,ntime,date,time,obs,bias,rmse,stdv,rnum)

 IF (ltiming) CALL acc_timing(timing_id,'print_p_stat')

RETURN
END 
