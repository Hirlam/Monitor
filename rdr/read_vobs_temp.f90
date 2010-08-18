SUBROUTINE read_vobs_temp

 USE data
 USE functions
 USE timing
 USE constants

 IMPLICIT NONE


 INTEGER :: i,ii,k,kk,kkk,kk_lev,       &
            ierr = 0,                   &
            cdate = 999999,             &
            ctime = 999999,             &
            wdate = 999999,             &
            wtime = 999999,             &
            istnr = 0,                  &
            stat_i,                     &
            num_temp,num_stat,          &
            num_temp_lev,               &
            my_temp_lev,                &
            stations(100000),           &
            max_found_stat,             &
            wrk(mparver),               &
            version_flag
 
 
 REAL :: lat,lon,hgt,val(8)

 LOGICAL :: use_stnlist

 CHARACTER(LEN=200) :: fname =' '
 CHARACTER(LEN= 10) :: ndate =' '

!----------------------------------------------------------

 stations       = 0
 max_found_stat = 0
 use_stnlist = ( MAXVAL(stnlist) > 0 ) 

 CALL allocate_obs

 ! Copy time

 cdate = sdate
 ctime = stime * 10000

 wrk = 0
 WHERE ( lev_lst > 0 ) wrk = 1
 my_temp_lev = SUM(wrk)

 

 !
 ! Loop over all times
 !

 i = 0

 TIME_LOOP : DO

 i = i + 1

 IF (print_read>1) WRITE(6,*)'TIME:',cdate,ctime/10000
 WRITE(ndate,'(I8.8,I2.2)')cdate,ctime/10000
 fname = TRIM(obspath)//'vobs'//ndate


 !
 ! Read obs data
 !

       OPEN(lunin,file=fname,status='old',iostat=ierr)

       IF (ierr.NE.0) THEN
  
          IF( print_read > 0 ) WRITE(6,*)'Could not open:',TRIM(fname)

          wdate = cdate
          wtime = ctime
          CALL adddtg(wdate,wtime,obint*3600,cdate,ctime)
          IF(cdate >  edate_obs) EXIT TIME_LOOP
          IF(cdate >= edate_obs .AND. ctime/10000 > etime_obs) EXIT TIME_LOOP

          i = i - 1
          CYCLE TIME_LOOP

       ENDIF

       IF ( print_read > 0 ) WRITE(6,*)'READ ',TRIM(fname)

       version_flag = 0

       READ(lunin,'(1x,3I6)',IOSTAT=ierr)num_stat,num_temp,version_flag
       IF ( ierr /= 0 ) THEN
         WRITE(6,*)'Error reading first line of vobs file'
         CALL abort
       ENDIF
       READ(lunin,*)num_temp_lev

       IF (num_temp == 0 ) THEN
          IF(print_read>1) WRITE(6,*)'SKIP',cdate,ctime
          i = i - 1
          CLOSE(lunin)
          wdate = cdate
          wtime = ctime
          CALL adddtg(wdate,wtime,obint*3600,cdate,ctime)
          IF(cdate > edate_obs) EXIT TIME_LOOP
          CYCLE TIME_LOOP
       ENDIF

       DO k=1,num_stat
          READ(lunin,*)
       ENDDO

       READ_STATION_OBS : DO k=1,num_temp

          SELECT CASE(version_flag)
          CASE(0)
             READ(lunin,*,iostat=ierr)istnr,lat,lon
             hgt = -99.
          CASE(1:2)
             READ(lunin,*,iostat=ierr)istnr,lat,lon,hgt
          CASE DEFAULT
             WRITE(6,*)'Cannot handle this vobs-file version',version_flag
             CALL abort
          END SELECT 

          IF(print_read>1) WRITE(6,*)istnr,lat,lon,hgt,ierr

          IF (ierr /= 0) THEN
             WRITE(6,*)'Problem in ',TRIM(fname)
             WRITE(6,*)'Error in reading the header of the TEMP observation ',istnr,lat,lon,hgt,ierr
             CALL abort
          ENDIF

          IF (istnr == 0) CYCLE READ_STATION_OBS
          IF (( ABS(lat+99.) < 1.e-4 ) ) CYCLE READ_STATION_OBS
          IF (( ABS(lon+99.) < 1.e-4 ) ) CYCLE READ_STATION_OBS

          !
          ! Find station index
          !

          IF(stations(istnr) == 0) THEN

             stat_i = 0
             IF ( use_stnlist ) THEN
                DO ii=1,maxstn
                   IF (istnr == stnlist(ii) ) stat_i = ii
                ENDDO
                IF (print_read>1) WRITE(6,*)'STATION ',istnr,stat_i
                IF ( stat_i == 0 ) THEN
                    DO kk=1,num_temp_lev
                       READ(lunin,*)
                    ENDDO
                    CYCLE READ_STATION_OBS
                ENDIF
             ENDIF

             IF (stat_i == 0 ) THEN
                max_found_stat  = max_found_stat + 1
                stnlist(max_found_stat) = istnr
             ELSE
                max_found_stat  = stat_i
             ENDIF

             IF (max_found_stat > maxstn) THEN
                WRITE(6,*)'Increase maxstn',max_found_stat
                CALL abort
             ENDIF

             stations(istnr) = max_found_stat
             obs(max_found_stat)%active = .TRUE.
             obs(max_found_stat)%stnr   = istnr
             obs(max_found_stat)%lat    = lat
             obs(max_found_stat)%lon    = lon

          ENDIF

          stat_i = stations(istnr)

          i = obs(stat_i)%ntim + 1 
         
          ALLOCATE(obs(stat_i)%o(i)%date)
          ALLOCATE(obs(stat_i)%o(i)%time)
          ALLOCATE(obs(stat_i)%o(i)%val(nparver))
   
          obs(stat_i)%ntim      = i
          obs(stat_i)%o(i)%date = cdate
          obs(stat_i)%o(i)%time = ctime/10000
          obs(stat_i)%o(i)%val  = err_ind

          READ_LEV_OBS : DO kk=1,num_temp_lev

          val = -99.
          SELECT CASE(version_flag)
          CASE(0)
             1001 format(1x,f5.0,f6.0,f6.1,f6.1,f5.0,f5.0,en13.3e2)
             READ(lunin,1001,iostat=ierr)val(1:7)
          CASE(1:2)
             READ(lunin,*,iostat=ierr)val
          END SELECT 

          IF (print_read>1) WRITE(6,*)'READ',kk,val

          IF (ierr /= 0 ) CYCLE READ_LEV_OBS

          kk_lev = 0
          DO kkk=1,my_temp_lev
             IF (ABS(val(1) -lev_lst(kkk)) < 1.e-6 ) kk_lev = kkk
          ENDDO

          IF ( kk_lev == 0 ) CYCLE READ_LEV_OBS

          IF (print_read>1) WRITE(6,*)'ADD:',kk_lev,val

          IF ((fi_ind /= 0 ) .AND. qca(val(2),-99.)) &
          obs(stat_i)%o(i)%val(kk_lev+ my_temp_lev*(fi_ind-1)) = val(2)
          IF ((tt_ind /= 0 ) .AND. qca(val(3),-99.)) &
          obs(stat_i)%o(i)%val(kk_lev+ my_temp_lev*(tt_ind-1)) = val(3) - tzero
          IF ((rh_ind /= 0 ) .AND. qca(val(4),-99.)) &
          obs(stat_i)%o(i)%val(kk_lev+ my_temp_lev*(rh_ind-1)) = val(4)
          IF ((dd_ind /= 0 ) .AND. qca(val(5),-99.)) &
          obs(stat_i)%o(i)%val(kk_lev+ my_temp_lev*(dd_ind-1)) = val(5)
          IF ((ff_ind /= 0 ) .AND. qca(val(6),-99.)) &
          obs(stat_i)%o(i)%val(kk_lev+ my_temp_lev*(ff_ind-1)) = val(6)
          IF ((qq_ind /= 0 ) .AND. qca(val(7),-99.)) &
          obs(stat_i)%o(i)%val(kk_lev+ my_temp_lev*(qq_ind-1)) = val(7) * 1.e3
          IF ((td_ind /= 0 ) .AND. qca(val(8),-99.)) &
          obs(stat_i)%o(i)%val(kk_lev+ my_temp_lev*(td_ind-1)) = val(8)

       ENDDO READ_LEV_OBS

       IF(print_read>1) WRITE(6,*)'DONE',istnr

       ENDDO READ_STATION_OBS

       CLOSE(lunin)

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,obint*3600,cdate,ctime)
    IF(cdate >  edate_obs) EXIT TIME_LOOP
    IF(cdate >= edate_obs .AND. ctime/10000 > etime_obs) EXIT TIME_LOOP

 ENDDO TIME_LOOP

 if (fi_ind /= 0 ) &
 lev_typ((fi_ind-1)* my_temp_lev + 1:fi_ind* my_temp_lev) = fi_ind
 if (tt_ind /= 0 ) &
 lev_typ((tt_ind-1)* my_temp_lev + 1:tt_ind* my_temp_lev) = tt_ind
 if (rh_ind /= 0 ) &
 lev_typ((rh_ind-1)* my_temp_lev + 1:rh_ind* my_temp_lev) = rh_ind
 if (dd_ind /= 0 ) &
 lev_typ((dd_ind-1)* my_temp_lev + 1:dd_ind* my_temp_lev) = dd_ind
 if (ff_ind /= 0 ) &
 lev_typ((ff_ind-1)* my_temp_lev + 1:ff_ind* my_temp_lev) = ff_ind
 if (qq_ind /= 0 ) &
 lev_typ((qq_ind-1)* my_temp_lev + 1:qq_ind* my_temp_lev) = qq_ind
 if (td_ind /= 0 ) &
 lev_typ((td_ind-1)* my_temp_lev + 1:td_ind* my_temp_lev) = td_ind

 WRITE(6,*) 'FOUND TIMES OBS',MAXVAL(obs(:)%ntim)

 DO i=1,maxstn
    obs(i)%active = ( obs(i)%ntim > 0 )
 ENDDO
 RETURN

END
