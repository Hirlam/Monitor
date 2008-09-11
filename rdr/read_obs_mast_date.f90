SUBROUTINE read_obs_mast_date

 !
 ! Read flax and mast data 
 ! and organize for evalutaion
 !
 ! Ulf Andrae, SMHI, 2008
 !
 
 USE data
 USE mast_data

 IMPLICIT NONE

 
 INTEGER :: i,ii,ierr,stat_i,        &
            cdate,ctime,             &
            wdate,wtime,             &
            newdate,newtime,         &
            stations(100),           &
            max_found_stat,istnr
         
 REAL    :: val1(4),num(4),val(4),   &
            mast(7),mast1(7),mnum(7)

 LOGICAL :: use_stnlist
         
 CHARACTER(LEN=  8) :: ccdate
 CHARACTER(LEN= 12) :: ctstamp
 CHARACTER(LEN=100) :: fname=''

 !---------------------------------------------------------------------

 stations       = 0
 use_stnlist    = ( MAXVAL(stnlist) > 0 )
 max_found_stat = 0

 !
 ! Allocate obs arrays if it is not already done
 !

 CALL allocate_obs

 IF(print_read > 0 ) WRITE(6,*)'OBS is ALLOCATED'

 STATION_LOOP : DO istnr=1,max_flux_station

    ! Check if this station should be used

    stat_i = 0
    IF ( use_stnlist ) THEN
       DO ii=1,maxstn
          IF (istnr == stnlist(ii) ) stat_i = ii
       ENDDO
       IF ( stat_i == 0 ) CYCLE STATION_LOOP
    ENDIF

    IF (stat_i == 0 ) THEN
       max_found_stat  = max_found_stat + 1
       stnlist(max_found_stat) = istnr
    ELSE
       max_found_stat  = stat_i
    ENDIF

    stations(istnr) = max_found_stat
    obs(max_found_stat)%active = .TRUE.
    obs(max_found_stat)%stnr   = istnr

    IF (max_found_stat > maxstn) THEN
       WRITE(6,*)'Increase maxstn',max_found_stat
       CALL abort
    ENDIF

    WRITE(6,*)'Working with station',stat_i,istnr,stations(istnr)

    stat_i = stations(istnr)

    ! The station is used, fill the data arrays

    cdate = sdate
    ctime = stime
    i = 0

    FILL_LOOP : DO

       IF ( cdate > edate ) EXIT FILL_LOOP

       i = i + 1

       ALLOCATE(obs(stat_i)%o(i)%date)
       ALLOCATE(obs(stat_i)%o(i)%time)
       ALLOCATE(obs(stat_i)%o(i)%val(nparver))

       obs(stat_i)%o(i)%date  = cdate
       obs(stat_i)%o(i)%time  = ctime
       obs(stat_i)%o(i)%val   = err_ind

       wdate = cdate
       wtime = ctime*10000
       CALL adddtg(wdate,wtime,3600*obint,cdate,ctime)
       ctime = ctime/10000
       
    ENDDO FILL_LOOP 

    obs(stat_i)%ntim = i

    !
    ! First treat the flux data
    !

    cdate = sdate
    ctime = stime

    IF ( print_read > 0 ) WRITE(6,*)'Read flux data'

    FLUX_DATA : DO

       WRITE(ccdate,'(I8.8)')cdate

       fname = TRIM(obspath)//'Meas_'//          &
               TRIM(stname(istnr))//'_Flux_'//   &
               TRIM(ccdate)//'.txt'

       OPEN(lunin,FILE=fname,STATUS='old',IOSTAT=ierr)

       IF ( ierr /= 0 ) THEN
          WRITE(6,'(2A)')'Could not open ',TRIM(fname)
          wdate = cdate
          wtime = ctime*10000
          CALL adddtg(wdate,00,3600*24,cdate,ctime)
          IF ( cdate > edate ) EXIT FLUX_DATA
          CYCLE FLUX_DATA
       ENDIF

       WRITE(6,'(2A)')'Open ',TRIM(fname)

       IF ( istnr == 2 ) THEN
          READ(lunin,*,IOSTAT=ierr)
          IF ( ierr /= 0 ) THEN
             WRITE(6,*)'Could read first line '
             CALL ABORT
          ENDIF
       ENDIF

       FLUX_READ_LOOP : DO
    
          num = 0.
          val = 0.
          FLUX_SUB_LOOP : DO

            READ(lunin,*,IOSTAT=ierr)ctstamp,val1
            IF ( print_read > 1 ) WRITE(6,*)ctstamp,val1
            IF ( ierr /= 0 ) EXIT FLUX_READ_LOOP
            READ(ctstamp,'(I8.8,I4.4)')newdate,newtime

            WHERE(val1 > -99998. ) 
               val = val + val1
               num = num + 1.
            END WHERE

            IF ( MOD(newtime,100) == 0 ) EXIT FLUX_SUB_LOOP

          ENDDO FLUX_SUB_LOOP 

          IF(print_read > 1 ) WRITE(6,*)'Sum is ',num

          WHERE( num > 0. ) 
             val  = val / num
          ELSEWHERE
             val  = err_ind
          END WHERE

          !
          ! Store data
          !

          IF ( print_read > 1 ) WRITE(6,*)'NEWDATE,NEWTIME',newdate,newtime

          ii = 0
          DO i=1,obs(stat_i)%ntim
             IF ( newdate     == obs(stat_i)%o(i)%date .AND. &
                  newtime/100 == obs(stat_i)%o(i)%time       )  THEN
                  ii = i
                  EXIT
             ENDIF
          ENDDO
          IF ( ii == 0 ) CYCLE FLUX_READ_LOOP
 
          IF(print_read > 1 ) WRITE(6,*)'Added ',newdate,newtime/100,i
   
          IF (wt_ind /= 0 .AND. (ABS(val(1)-err_ind)>1.e-6)) obs(stat_i)%o(ii)%val(wt_ind) = val(1)
          IF (wq_ind /= 0 .AND. (ABS(val(2)-err_ind)>1.e-6)) obs(stat_i)%o(ii)%val(wq_ind) = val(2)
          IF (uw_ind /= 0 .AND. (ABS(val(3)-err_ind)>1.e-6)) obs(stat_i)%o(ii)%val(uw_ind) = val(3)

       ENDDO FLUX_READ_LOOP

       CLOSE(lunin)

       wdate = cdate
       wtime = ctime*10000
       CALL adddtg(wdate,00,3600*24,cdate,ctime)
       IF ( cdate > edate ) EXIT FLUX_DATA

    ENDDO FLUX_DATA

    cdate = sdate
    ctime = stime

    IF ( print_read > 1 ) WRITE(6,*)'Read mast data'

    MAST_LOOP : DO

       !
       ! Second treat the mast data
       !

       WRITE(ccdate,'(I8.8)')cdate
       fname = TRIM(obspath)//'Meas_'//          &
               TRIM(stname(istnr))//'_Mast_'//   &
               TRIM(ccdate)//'.txt'

       OPEN(lunin,FILE=fname,STATUS='old',IOSTAT=ierr)
       IF ( ierr /= 0 ) THEN
          WRITE(6,*)'Could not open ',TRIM(fname)
          wdate = cdate
          wtime = ctime*10000
          CALL adddtg(wdate,00,3600*24,cdate,ctime)
          IF ( cdate > edate ) EXIT MAST_LOOP
          CYCLE MAST_LOOP
       ENDIF
   
       WRITE(6,'(2A)')'Open ',TRIM(fname)

       IF ( istnr == 2 ) THEN
          READ(lunin,*,IOSTAT=ierr)
          IF ( ierr /= 0 ) THEN
             WRITE(6,*)'Could read first line '
             CALL ABORT
          ENDIF
       ENDIF

       MAST_READ_LOOP : DO
    
          mnum = 0.
          mast = 0.

          MAST_SUB_LOOP : DO

             READ(lunin,*,IOSTAT=ierr)ctstamp,mast1
             IF( print_read > 1 ) WRITE(6,*)'READ:',ctstamp,mast1
             IF ( ierr /= 0 ) EXIT MAST_READ_LOOP
             READ(ctstamp,'(I8.8,I4.4)')newdate,newtime

             IF ( newdate < sdate )CYCLE MAST_READ_LOOP

             WHERE(mast1 > -99998. ) 
               mast = mast + mast1
               mnum = mnum + 1.
             END WHERE

             IF ( MOD(newtime,100) == 0 ) EXIT MAST_SUB_LOOP

          ENDDO MAST_SUB_LOOP 

          IF(print_read > 1 ) WRITE(6,*)'Sum is ',mnum

          WHERE( mnum > 0. ) 
             mast  = mast / mnum
          ELSEWHERE
             mast  = err_ind
          END WHERE

          !
          ! Store data
          !

          ii = 0
          DO i=1,obs(stat_i)%ntim
             IF ( newdate     == obs(stat_i)%o(i)%date .AND. &
                  newtime/100 == obs(stat_i)%o(i)%time       )  THEN
                  ii = i
                  EXIT
             ENDIF
          ENDDO

          IF ( ii == 0 ) CYCLE MAST_READ_LOOP
 
          IF(print_read > 1 ) WRITE(6,*)'Added ',newdate,newtime/100,i
   
          IF (tt_ind /= 0 .AND. (ABS(mast(1)-err_ind)>1.e-6)) obs(stat_i)%o(ii)%val(tt_ind) = mast(1)
          IF (tz_ind /= 0 .AND. (ABS(mast(3)-err_ind)>1.e-6)) obs(stat_i)%o(ii)%val(tz_ind) = mast(3)/dz(istnr)
          IF (rh_ind /= 0 .AND. (ABS(mast(4)-err_ind)>1.e-6)) obs(stat_i)%o(ii)%val(rh_ind) = mast(4)
          IF (ff_ind /= 0 .AND. (ABS(mast(5)-err_ind)>1.e-6)) obs(stat_i)%o(ii)%val(ff_ind) = mast(5)
          IF (gr_ind /= 0 .AND. (ABS(mast(6)-err_ind)>1.e-6)    &
          qcl(mast(6),gr_ind) .AND. qcu(mast(6),gr_ind)     ) obs(stat_i)%o(ii)%val(gr_ind) = mast(6)
          IF (lu_ind /= 0 .AND. (ABS(mast(7)-err_ind)>1.e-6)) obs(stat_i)%o(ii)%val(lu_ind) = mast(7)

       ENDDO MAST_READ_LOOP

       CLOSE(lunin)
    
       wdate = cdate
       wtime = ctime*10000
       CALL adddtg(wdate,00,3600*24,cdate,ctime)
       IF ( cdate > edate ) EXIT MAST_LOOP

    ENDDO MAST_LOOP

 ENDDO STATION_LOOP

 ! Set station names
 ALLOCATE(station_name(maxstn))

 DO i=1,maxstn
   station_name(i)=stname(obs(i)%stnr)
 ENDDO

END SUBROUTINE read_obs_mast_date
