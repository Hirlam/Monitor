SUBROUTINE print_p_stat(lunout,ntim,npar,stnr,        &
                        time_stat,par_active,         &
                        period1,period2,uh,uf)

 USE types
 USE data, ONLY : ldiff,maxfclenval

 IMPLICIT NONE

 INTEGER :: lunout,ntim,npar,stnr,par_active(npar),period1,period2
 TYPE(stat_obs) :: time_stat(ntim)

 LOGICAL :: uh(npar,0:23),uf(npar,0:maxfclenval)

 CALL print_p_stat_diff(lunout,ntim,npar,stnr,       &
                       time_stat,.false.,par_active, &
                       period1,period2,uh,uf)

 IF (ldiff )                                         &
 CALL print_p_stat_diff(lunout,ntim,npar,stnr,       &
                       time_stat,.true.,par_active,  &
                       period1,period2,uh,uf)

 RETURN
END SUBROUTINE print_p_stat

SUBROUTINE print_p_stat_diff(lunout,ntim,npar,stnr,     &
                            time_stat,ldiff,par_active, &
                            period1,period2,uh,uf)
 ! External modules

 USE types
 USE timing
 USE mymagics
 USE means
 USE data, ONLY : obstype,expname,err_ind,nexp,          &
                  station_name,csi,                      &
                  ltiming,tag,maxfclenval,               &
                  show_fc_length,nuse_fclen,use_fclen,   &
                  timeserie_wind,sumup_tolerance,obint,  &
                  copied_obs,copied_mod,                 &
                  show_rmse,show_stdv,show_bias,show_obs,&
                  ltemp,lev_lst,window_pos,output_type,  &
                  z_is_pressure,output_mode,len_lab,     &
                  accu_int

 USE functions

 IMPLICIT NONE

 ! INPUT

 INTEGER :: lunout,ntim,npar,stnr,par_active(npar),       &
            period1,period2
 TYPE(stat_obs) :: time_stat(ntim)
 LOGICAL :: ldiff,uh(npar,0:23),uf(npar,0:maxfclenval)

 ! local

 INTEGER :: i,ii,j,k,                   &
            timing_id,                  &
            ntim_use,dlen,              &
            istart,iend,maxtim,npp

 REAL :: rnum_min(0:nexp),rnum_max(0:nexp),rnum_ave(0:nexp), &
         data_min(0:nexp),data_max(0:nexp),data_ave(0:nexp), &
         rmse_min(0:nexp),rmse_max(0:nexp),rmse_ave(0:nexp), &
         stdv_min(0:nexp),stdv_max(0:nexp),stdv_ave(0:nexp)

 TYPE print_pointer 
    REAL, POINTER :: v(:)
 END TYPE

 TYPE(print_pointer), ALLOCATABLE :: pdat(:)

 ! Allocatable

 REAL,    ALLOCATABLE, TARGET :: obs(:),rnum(:,:),bias(:,:),rmse(:,:),stdv(:,:)
 INTEGER, ALLOCATABLE :: ndate(:),ntime(:),date(:),time(:)


 CHARACTER(LEN=30 ) :: cform='   '
 CHARACTER(LEN=6  ) :: ob_short='      '
 CHARACTER(LEN=2  ) :: prefix=' '
 CHARACTER(LEN=100) :: fname=' '
 CHARACTER(LEN=120) :: wtext,wname,wtext1


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

 z_is_pressure = ( ltemp .AND. ( lev_lst(1) > lev_lst(2) ))

 ytitle = ' '

 IF ( SUM(timeserie_wind) /= 0 ) THEN
    IF ( MINVAL(timeserie_wind(1:npar)) == 0 ) THEN
    maxtim = get_maxtim(time_stat(istart)%date,time_stat(iend)%date,obint)
    ELSE
    maxtim = get_maxtim(time_stat(istart)%date,time_stat(iend)%date,    &
                        MAX(obint,MINVAL(timeserie_wind(1:npar))))
    ENDIF
 ELSE
    maxtim = get_maxtim(time_stat(istart)%date,time_stat(iend)%date,obint)
 ENDIF
 maxtim = MAX(maxtim,ntim)

    npp = 0
    IF ( ldiff ) THEN
       IF (show_rmse) THEN
          npp = nexp
       ENDIF
       IF (show_stdv) THEN
          npp = npp + nexp
       ENDIF
       IF (show_bias) THEN
          npp = npp + nexp
       ENDIF
    ELSE
       npp =  nexp
    ENDIF

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

    IF ( output_mode == 2 ) THEN
       CALL make_fname(prefix,period1,stnr,tag,     &
                       obstype(j)(1:2),           &
                       obstype(j)(3:len_lab),     &
                       output_mode,output_type,   &
                       fname)
       CALL open_output(fname)
    ENDIF

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
           err_ind,window_pos,.true.)

           ndate = date
           ntime = time

         ENDIF

           CALL carefull_sumup(           &
           rnum(:,k),ndate(:),ntime(:),   &
           ii,maxtim,timeserie_wind(j),dlen, &
           rnum_min(k),rnum_max(k),       &
           rnum_ave(k),ndate(1),00,       &
           sumup_tolerance,obint,         &
           err_ind,window_pos,.false.)

           ndate = date
           ntime = time

           CALL carefull_sumup(           &
           bias(:,k),ndate(:),ntime(:),   &
           ii,maxtim,timeserie_wind(j),dlen, &
           data_min(k),data_max(k),       &
           data_ave(k),ndate(1),00,       &
           sumup_tolerance,obint,         &
           err_ind,window_pos,.true.)

           IF ( ldiff ) THEN

              ndate = date
              ntime = time

              CALL carefull_sumup(           &
              rmse(:,k),ndate(:),ntime(:),   &
              ii,maxtim,timeserie_wind(j),dlen, &
              rmse_min(k),rmse_max(k),       &
              rmse_ave(k),ndate(1),00,       &
              sumup_tolerance,obint,         &
              err_ind,window_pos,.true.)

              ndate = date
              ntime = time

              CALL carefull_sumup(           &
              stdv(:,k),ndate(:),ntime(:),   &
              ii,maxtim,timeserie_wind(j),dlen, &
              stdv_min(k),stdv_max(k),       &
              stdv_ave(k),ndate(1),00,       &
              sumup_tolerance,obint,         &
              err_ind,window_pos,.true.)

           ENDIF

      ENDIF

    ENDDO

    ntim_use = MAX(dlen,1)

    IF ( .NOT. ldiff .AND. obstype(j)(1:2) == 'DD' ) THEN

        WHERE(obs(1:ntim_use) > 360. ) 
         obs(1:ntim_use) =  obs(1:ntim_use) - 360.
        ELSEWHERE( (obs(1:ntim_use) < 0.) .AND. (obs(1:ntim_use) > err_ind) ) 
         obs(1:ntim_use) =  obs(1:ntim_use) + 360.
        END WHERE

        WHERE(bias(1:ntim_use,:) > 360. ) 
         bias(1:ntim_use,:) =  bias(1:ntim_use,:) - 360.
        ELSEWHERE( (bias(1:ntim_use,:) < 0.) .AND. (bias(1:ntim_use,:) > err_ind) ) 
         bias(1:ntim_use,:) =  bias(1:ntim_use,:) + 360.
        END WHERE

    ENDIF

    ! Create headers
 
    ! Line 1
    IF(ALLOCATED(station_name).AND. stnr > 0 ) THEN
       wtext='Station: '//trim(station_name(csi))
    ELSE
       WRITE(wtext1(1:8),'(I8)')stnr
       wtext='Station: '//trim(wtext1(1:8))
    ENDIF
    IF (stnr == 0) THEN
       wname=''
       WRITE(wname(1:5),'(I5)')par_active(j)
       wtext=TRIM(wname)//' stations'
       IF ( TRIM(tag) /= '#' ) wtext='Area: '//TRIM(tag)//'  '//TRIM(wtext)
    ENDIF
    WRITE(lunout,'(A,X,A)')'#HEADING_1',TRIM(wtext)

    ! Line 2
    CALL pname(obstype(j),wtext)
    WRITE(lunout,'(A,X,A)')'#HEADING_2',TRIM(wtext)

    ! Line 3
    IF ( show_fc_length ) THEN

       CALL fclen_header(.true.,maxfclenval,uh(j,:),uf(j,:),accu_int(j),wtext)

       IF ( timeserie_wind(j) /= 0 ) THEN
          wname = ' '
          WRITE(wname(1:3),'(I3)')timeserie_wind(j)
          wtext = TRIM(wtext)//'  Window:'//TRIM(wname)//'h'
       ENDIF

       WRITE(lunout,'(A,X,A)')'#HEADING_3',TRIM(wtext)

    ENDIF

    ! Experiments and parameters and norms
    WRITE(lunout,'(A,X,A)')'#PAR',TRIM(obstype(j))

    npp = 0
    IF ( ldiff ) THEN
       IF (show_rmse) npp = nexp
       IF (show_stdv) npp = npp + nexp
       IF (show_bias) npp = npp + nexp
    ELSE
       npp = 1 + nexp
    ENDIF
    ! Add one column for number of cases 
    npp = npp + 1

    IF ( ldiff ) THEN
       WRITE(lunout,'(A,X,I2)')'#NEXP',nexp
    ELSE
       WRITE(lunout,'(A,X,I2)')'#NEXP',nexp+1
       WRITE(lunout,'(A,I2.2X,A)')'#EXP_',0,'OBS'
    ENDIF
    DO i=1,nexp
       WRITE(lunout,'(A,I2.2X,A)')'#EXP_',i,expname(i)
    ENDDO

    ob_short = obstype(j)
    ob_short(3:6) = '   '
    CALL yunit(ob_short,ytitle)
    WRITE(lunout,'(A,X,A)')'#YLABEL',TRIM(ytitle)
    WRITE(lunout,'(A,X,A)')'#XLABEL','Date'

    ! Time to write the parameters
 
    ALLOCATE(pdat(npp))
  
    k = 0
    IF (ldiff) THEN
     IF ( show_rmse ) THEN
      DO i=1,nexp
        k=k+1
        pdat(k)%v => rmse(1:ntim_use,i)
        WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+2,'RMSE',TRIM(expname(i))
      ENDDO
     ENDIF 
     IF ( show_stdv ) THEN
      DO i=1,nexp
        k=k+1
        pdat(k)%v => stdv(1:ntim_use,i)
        WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+2,'STDV',TRIM(expname(i))
      ENDDO
     ENDIF 
     IF ( show_bias ) THEN
      DO i=1,nexp
        k=k+1
        pdat(k)%v => bias(1:ntim_use,i)
        WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+2,'BIAS',TRIM(expname(i))
      ENDDO
     ENDIF 
    ELSE 
      DO i=1,nexp
        k=k+1
        pdat(k)%v => bias(1:ntim_use,i)
        WRITE(lunout,'(A,I2.2,X,A)')'#COLUMN_',k+2,TRIM(expname(i))
      ENDDO
      k=k+1
      pdat(k)%v => obs(1:ntim_use)
      WRITE(lunout,'(A,I2.2,X,A)')'#COLUMN_',k+2,'OBS'
    ENDIF
    k=k+1
    pdat(k)%v => rnum(1:ntim_use,1)
    WRITE(lunout,'(A,I2.2,X,A)')'#COLUMN_',k+2,'CASES'

    ! End of headings
    WRITE(lunout,'(A,X,en15.5e2)')'#MISSING',err_ind
    WRITE(lunout,'(A)')'#END'


    cform = '(I10,I3.2,NN(x,en15.5e2))'
    WRITE(cform(11:12),'(I2.2)')npp

    DO i=1,ntim_use
        WRITE(lunout,cform)ndate(i),ntime(i),(pdat(k)%v(i),k=1,npp)
    ENDDO

    CLOSE(lunout)

    DO i=1,SIZE(pdat)
       NULLIFY(pdat(i)%v)
    ENDDO
    DEALLOCATE(pdat)

 ENDDO NPAR_LOOP

 DEALLOCATE(ndate,ntime,date,time,obs,bias,rmse,stdv,rnum)

 RETURN

END  SUBROUTINE print_p_stat_diff
