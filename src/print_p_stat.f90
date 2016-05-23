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
 USE means
 USE data, ONLY : varprop,expname,err_ind,nexp,          &
                  station_name,csi,                      &
                  ltiming,tag,maxfclenval,               &
                  show_fc_length,nuse_fclen,use_fclen,   &
                  timeserie_wind,sumup_tolerance,obint,  &
                  show_rmse,show_stdv,show_bias,show_obs,&
                  show_var,show_skw,show_mabe,           &
                  ltemp,lev_lst,window_pos,output_type,  &
                  output_mode,len_lab,cini_hours

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
            istart,iend,maxtim,npp,     &
            case_i,                     &
            i1,i2,min_diff_avail,ut

 REAL minnum,maxnum,ticnum,maxnum_t

 REAL :: rnum_min(0:nexp),rnum_max(0:nexp),rnum_ave(0:nexp), &
         data_min(0:nexp),data_max(0:nexp),data_ave(0:nexp), &
         rmse_min(0:nexp),rmse_max(0:nexp),rmse_ave(0:nexp), &
         stdv_min(0:nexp),stdv_max(0:nexp),stdv_ave(0:nexp)

 TYPE print_pointer 
    REAL, POINTER :: v(:)
 END TYPE

 TYPE(print_pointer), ALLOCATABLE :: pdat(:)

 ! Allocatable

 REAL,    ALLOCATABLE, TARGET ::  obs(:),   &
                                 snum(:,:), &
                                 rnum(:,:), &
                                 mabe(:,:), &
                                 bias(:,:), &
                                 rmse(:,:), &
                                 stdv(:,:)

 INTEGER, ALLOCATABLE :: ndate(:),ntime(:),date(:),time(:)
 REAL,    ALLOCATABLE, TARGET :: stdvi(:,:),skw(:,:),stdvo(:),skwo(:)
 REAL    :: zobs,zfc,zslask2
 CHARACTER(LEN=30 ) :: cform='   '
 CHARACTER(LEN=20 ) :: ytitle=''
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

 ytitle = ' '

 IF ( SUM(timeserie_wind) /= 0 ) THEN
    IF ( MINVAL(timeserie_wind(1:npar)) == 0 ) THEN
    maxtim = get_maxtim(time_stat(istart)%date,time_stat(iend)%date,obint)
    ELSE
    maxtim = get_maxtim(time_stat(istart)%date,time_stat(iend)%date,    &
                        MIN(obint,MINVAL(timeserie_wind(1:npar))))
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
           mabe(maxtim,nexp),   &
           bias(maxtim,nexp),   &
           rmse(maxtim,nexp),   &
           stdv(maxtim,nexp),   &
           snum(maxtim,nexp),   &
           rnum(maxtim,nexp))

 IF ( show_var .OR. show_skw )  &
 ALLOCATE(                      &
          stdvo(maxtim),        &
           skwo(maxtim),        &
          stdvi(maxtim,nexp),   &
            skw(maxtim,nexp))

 NPAR_LOOP : DO j=1,npar

    IF ( output_mode == 2 ) THEN
       wtext = TRIM(tag)//TRIM(cini_hours)
       CALL make_fname(prefix,period1,stnr,wtext, &
                       varprop(j)%id,             &
                       varprop(j)%lev,            &
                       output_mode,output_type,   &
                       fname)
       CALL open_output(fname)
    ENDIF

    ! Calculate shortest available time difference
    i1 = -1 
    i2 = -1
    min_diff_avail = 24
    DO i=0,23
     IF ( uh(j,i) ) THEN
       IF ( i1 == -1 ) THEN
         i1 = i
       ELSEIF ( i2 == -1 ) THEN
         i2 = i
         min_diff_avail = MIN(min_diff_avail,i2-i1)
       ELSE
         i1 = i2
         i2 = i
         min_diff_avail = MIN(min_diff_avail,i2-i1)
       ENDIF
     ENDIF
    ENDDO
   
    i1 = -1 
    i2 = -1
    DO i=0,maxfclenval
     IF ( uf(j,i) ) THEN
       IF ( i1 == -1 ) THEN
         i1 = i
       ELSEIF ( i2 == -1 ) THEN
         i2 = i
         min_diff_avail = MIN(min_diff_avail,i2-i1)
       ELSE
         i1 = i2
         i2 = i
         min_diff_avail = MIN(min_diff_avail,i2-i1)
       ENDIF
     ENDIF
    ENDDO

    IF ( timeserie_wind(j) == 0 ) THEN
      ut = 0
    ELSE
      ut = MAX(min_diff_avail,timeserie_wind(j))
    ENDIF
 
    IF ( ut /= timeserie_wind(j)) THEN
      WRITE(6,*)'Changed timeserie window to:',ut,' for ',TRIM(varprop(j)%text)
    ENDIF

    rnum = 0.
    snum = 0.
    mabe = 0.
    bias = 0.
    rmse = 0.
    stdv = 0.
    obs  = 0.

    IF ( show_var .OR. show_skw ) THEN
       stdvi= 0.
       stdvo= 0.
       skw  = 0.
       skwo = 0.
    ENDIF

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

          rnum(ii,k) = MAX(1.,FLOAT(time_stat(i)%n(j)))
          snum(ii,k) = FLOAT(time_stat(i)%n(j))

          IF ( ldiff ) THEN
             IF ( show_mabe ) &
             mabe(ii,k) =          time_stat(i)%mabe(k,j)/rnum(ii,k)
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

             IF ( show_var .OR. show_skw ) THEN

                !
                ! Model std and skewness:
                !

                zobs = time_stat(i)%obs(j) / rnum(ii,k) ! Mean obs value
                zfc  = bias(ii,k) + zobs                ! Mean forecast value

                stdvi(ii,k) =SQRT(ABS(time_stat(i)%s2(k,j)/rnum(ii,k) - &
                                      zfc**2))

                zslask2 = time_stat(i)%s3(k,j)/rnum(ii,k) &
                     -  3*time_stat(i)%s2(k,j)/rnum(ii,k)*zfc &
                     +  2*zfc**3

                IF(zslask2 < 0.)then 
                   skw(ii,k) = - (-zslask2)**0.333333
                ELSE
                   skw(ii,k) = zslask2**0.333333
                ENDIF

                !
                ! Obs std and skewness:
                !

                stdvo(ii) =SQRT(ABS(time_stat(i)%obs2(j)/rnum(ii,k) - &
                                      zobs**2))

                zslask2 = time_stat(i)%obs3(j)/rnum(ii,k) &
                     -  3*time_stat(i)%obs2(j)/rnum(ii,k)*zobs &
                     +  2*zobs**3

                IF(zslask2 < 0)then
                   skwo(ii) = - (-zslask2)**0.333333
                ELSE
                   skwo(ii) = zslask2**0.333333
                ENDIF 

             ENDIF

          ENDIF

      ENDIF
      ENDDO

    ENDDO


    DO k=1,nexp

      ndate = date
      ntime = time
      dlen  = ii

      IF (ut /= 0 .AND. dlen /= 0 ) THEN

         IF ( k == 1 ) THEN

           CALL carefull_sumup(           &
           obs,ndate,ntime,               &
           ii,maxtim,ut,dlen, &
           data_min(0),data_max(0),       &
           data_ave(0),ndate(1),00,       &
           sumup_tolerance,obint,         &
           err_ind,window_pos,.true.)

           ndate = date
           ntime = time

           CALL carefull_sumup(           &
           snum(:,k),ndate(:),ntime(:),   &
           ii,maxtim,ut,dlen, &
           rnum_min(k),rnum_max(k),       &
           rnum_ave(k),ndate(1),00,       &
           sumup_tolerance,obint,         &
           err_ind,window_pos,.false.)

           ndate = date
           ntime = time

           CALL carefull_sumup(           &
           rnum(:,k),ndate(:),ntime(:),   &
           ii,maxtim,ut,dlen, &
           rnum_min(k),rnum_max(k),       &
           rnum_ave(k),ndate(1),00,       &
           sumup_tolerance,obint,         &
           err_ind,window_pos,.false.)

           ndate = date
           ntime = time

         ENDIF

           CALL carefull_sumup(           &
           bias(:,k),ndate(:),ntime(:),   &
           ii,maxtim,ut,dlen, &
           data_min(k),data_max(k),       &
           data_ave(k),ndate(1),00,       &
           sumup_tolerance,obint,         &
           err_ind,window_pos,.true.)

           IF ( ldiff ) THEN

              ndate = date
              ntime = time

              CALL carefull_sumup(           &
              rmse(:,k),ndate(:),ntime(:),   &
              ii,maxtim,ut,dlen, &
              rmse_min(k),rmse_max(k),       &
              rmse_ave(k),ndate(1),00,       &
              sumup_tolerance,obint,         &
              err_ind,window_pos,.true.)

              ndate = date
              ntime = time

              CALL carefull_sumup(           &
              stdv(:,k),ndate(:),ntime(:),   &
              ii,maxtim,ut,dlen, &
              stdv_min(k),stdv_max(k),       &
              stdv_ave(k),ndate(1),00,       &
              sumup_tolerance,obint,         &
              err_ind,window_pos,.true.)

              ndate = date
              ntime = time

              CALL carefull_sumup(           &
              mabe(:,k),ndate(:),ntime(:),   &
              ii,maxtim,ut,dlen, &
              data_min(k),data_max(k),       &
              data_ave(k),ndate(1),00,       &
              sumup_tolerance,obint,         &
              err_ind,window_pos,.true.)


           ENDIF

      ENDIF

    ENDDO

    ntim_use = MAX(dlen,1)

    IF ( .NOT. ldiff .AND. varprop(j)%id == 'DD' ) THEN

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
    WRITE(lunout,'(A,X,A)')'#HEADING_1',TRIM(varprop(j)%text)
 
    ! Line 2
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
       IF ( TRIM(tag) /= '#' ) wtext='Selection: '//TRIM(tag)//'  '//TRIM(wtext)
    ENDIF
    WRITE(lunout,'(A,X,A)')'#HEADING_2',TRIM(wtext)


    ! Line 3
    IF ( show_fc_length ) THEN

       CALL fclen_header(.true.,maxfclenval,uh(j,:),uf(j,:),varprop(j)%acc,wtext)

       WRITE(lunout,'(A,X,A)')'#HEADING_3',TRIM(wtext)

       IF ( ut /= 0 ) THEN
          wname = ' '
          WRITE(wname(1:3),'(I3)')ut
          WRITE(lunout,'(A,X,A)')'#HEADING_4',' Averaging window:'//TRIM(wname)//'h'
       ENDIF

    ENDIF

    ! Experiments and parameters and norms
    WRITE(lunout,'(A,X,A)')'#PAR',TRIM(varprop(j)%id)

    npp = 0
    IF ( ldiff ) THEN
       IF (show_rmse) npp = nexp
       IF (show_stdv) npp = npp + nexp
       IF (show_bias) npp = npp + nexp
       IF (show_mabe) npp = npp + nexp
    ELSE
       npp = 1 + nexp
       IF ( show_var ) npp = 1 + nexp + npp
       IF ( show_skw ) npp = 1 + nexp + npp
    ENDIF
    ! Add one column for number of cases 
    npp = npp + 1

    IF ( ldiff ) THEN
       WRITE(lunout,'(A,X,I2)')'#NEXP',nexp
    ELSE
       WRITE(lunout,'(A,X,I2)')'#NEXP',nexp+1
       WRITE(lunout,'(A,I2.2,X,A)')'#EXP_',0,'OBS'
    ENDIF
    DO i=1,nexp
       WRITE(lunout,'(A,I2.2,X,A)')'#EXP_',i,expname(i)
    ENDDO

    WRITE(lunout,'(A,X,A)')'#YLABEL',TRIM(varprop(j)%unit)
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
     IF ( show_mabe ) THEN
      DO i=1,nexp
        k=k+1
        pdat(k)%v => mabe(1:ntim_use,i)
        WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+2,'MAE',TRIM(expname(i))
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
      IF ( show_var ) THEN
         DO i=1,nexp
           k=k+1
           pdat(k)%v => stdvi(1:ntim_use,i)
           WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+2,'STDVI',TRIM(expname(i))
         ENDDO
         k=k+1
         pdat(k)%v => stdvo(1:ntim_use)
         WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+2,'STDVO','OBS'
      ENDIF
    ENDIF
    k=k+1
    pdat(k)%v => snum(1:ntim_use,1)
    case_i = k
    WRITE(lunout,'(A,I2.2,X,A)')'#COLUMN_',k+2,'CASES'

    minnum = MINVAL(rnum(1:ntim_use,1))
    maxnum_t = MAXVAL(rnum(1:ntim_use,1))
    minnum = FLOOR(LOG10(MAX(minnum,1.)))
    maxnum = FLOOR(LOG10(MAX(maxnum_t,1.)))
    minnum = 10.**(minnum)
    IF ( minnum < 10. ) minnum = 0.
    maxnum = 10.**(maxnum)
    maxnum = CEILING(maxnum_t/maxnum)*maxnum
    ticnum = tics(minnum,maxnum)

    WRITE(lunout,'(A,X,en15.5e2)')'#MINNUM',minnum
    WRITE(lunout,'(A,X,en15.5e2)')'#TICNUM',ticnum
    WRITE(lunout,'(A,X,en15.5e2)')'#MAXNUM',maxnum

    ! End of headings
    WRITE(lunout,'(A,X,en15.5e2)')'#MISSING',err_ind
    WRITE(lunout,'(A)')'#END'


    cform = '(I10,I3.2,NN(x,en15.5e2))'
    WRITE(cform(11:12),'(I2.2)')npp

    DO i=1,ntim_use
       IF ( pdat(case_i)%v(i) < 1. ) THEN
         DO k=1,npp
           pdat(k)%v(i) = err_ind
         ENDDO
       ENDIF
       WRITE(lunout,cform)ndate(i),ntime(i),(pdat(k)%v(i),k=1,npp)
    ENDDO

    CLOSE(lunout)

    DO i=1,SIZE(pdat)
       NULLIFY(pdat(i)%v)
    ENDDO
    DEALLOCATE(pdat)

 ENDDO NPAR_LOOP

 DEALLOCATE(ndate,ntime,date,time,obs,bias,rmse,stdv,rnum,snum)

 IF ( show_var .OR. show_skw ) DEALLOCATE(stdvi,stdvo,skw,skwo)

 RETURN

END  SUBROUTINE print_p_stat_diff
