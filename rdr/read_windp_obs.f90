SUBROUTINE read_windp_obs

 USE data
 USE windp

 IMPLICIT NONE


 INTEGER :: i,j,ierr,                           &
            yy,mm,dd,hh,                        &
            wdate,wtime,odate,otime
 REAL ::val_scale
 INTEGER  ::val(2)

 CHARACTER(LEN=99) :: cname =''
 CHARACTER(LEN= 7) :: cstn  = ''
 CHARACTER(LEN=11) :: cdate = ''

!----------------------------------------------------------

 CALL init_windp

 CALL allocate_obs

 obs%stnr = stnlist

 !
 ! Loop over all times
 !

 STATION_LOOP : DO i=1,maxstn

    IF ( obs(i)%stnr == 0 ) CYCLE STATION_LOOP

    windp_index = 0
    DO j=1,maxwindp
       IF (obs(i)%stnr == windp_stations(j)%nr ) THEN
          WRITE(cstn ,'(I7)')windp_stations(j)%id
          windp_index = j
       ENDIF
    ENDDO

    IF ( windp_index == 0 ) CYCLE STATION_LOOP

    j = 0
    obs(i)%active = .TRUE.

    !
    ! Read obs data
    !

    cname = TRIM(obspath)//cstn//'.dat'
    OPEN(lunin,file=cname,status='old',iostat=ierr)

    IF (ierr.NE.0) THEN
  
       IF(print_read > 0)WRITE(6,*)'Could not open:',TRIM(cname)
       CYCLE STATION_LOOP

    ENDIF

    IF (print_read > 0 )WRITE(6,*)'READ ',TRIM(cname)

    TIME_LOOP : DO 

       READ(lunin,*,iostat=ierr)yy,mm,dd,hh,val
       IF(print_read > 1)WRITE(6,*)yy,mm,dd,hh,val


       IF ( ierr /= 0   ) EXIT TIME_LOOP
       !IF ( val(2) /= 1 ) CYCLE


       wdate = yy * 10000 + mm * 100 + dd
       wtime = hh * 10000

       IF ( hh == 24 ) THEN
          wtime = 23 * 10000
          CALL adddtg(wdate,wtime,3600,odate,otime) 
          wdate = odate
          wtime = otime
       ENDIF

       ! Make UTC of SNT
       CALL adddtg(wdate,wtime,-3600,odate,otime) 
       wdate = odate
       wtime = otime

       IF ( wdate < sdate ) CYCLE TIME_LOOP
       IF ( wdate > edate ) EXIT  TIME_LOOP
       IF ( MOD(wtime/10000,obint) /= 0 ) CYCLE TIME_LOOP

       !
       ! Station is found, add time
       !
       j = j + 1

       ALLOCATE(obs(i)%o(j)%date)
       ALLOCATE(obs(i)%o(j)%time)
       ALLOCATE(obs(i)%o(j)%val(nparver))

       obs(i)%ntim      = j
       obs(i)%o(j)%val  = err_ind
       obs(i)%o(j)%date = wdate
       obs(i)%o(j)%time = wtime/10000

       SELECT CASE(windp_stations(windp_index)%id)
       CASE(3298924)
          val_scale = 8.
       CASE(3757143)
          val_scale = 4.
       CASE(3395007)
          val_scale = 2.
       CASE DEFAULT
          val_scale = 1.
       END SELECT

       IF (ff_ind /= 0) obs(i)%o(j)%val(ff_ind)  = 5.
       IF (dd_ind /= 0) obs(i)%o(j)%val(dd_ind)  = 0.
       IF (tz_ind /= 0) obs(i)%o(j)%val(tz_ind)  = 0.
       IF (uz_ind /= 0) obs(i)%o(j)%val(uz_ind)  = 0.
       IF (tu_ind /= 0) obs(i)%o(j)%val(tu_ind)  = 0.

       SELECT CASE(special_flag) 

          CASE (1)
             IF (wp_ind /= 0) obs(i)%o(j)%val(wp_ind)  = SQRT(FLOAT(val(1)) / val_scale)
          CASE DEFAULT
             IF (wp_ind /= 0) obs(i)%o(j)%val(wp_ind)  = FLOAT(val(1)) / val_scale
       END SELECT

       IF(print_read > 1)WRITE(6,*) obs(i)%o(j)%date,obs(i)%o(j)%time,obs(i)%o(j)%val 
       

    ENDDO TIME_LOOP

    CLOSE(lunin)

 ENDDO STATION_LOOP


 DO i=1,maxstn
    obs(i)%active = ( obs(i)%ntim > 0 )
 ENDDO

 IF ( .FALSE.) THEN
 DO i=1,maxstn
    !WRITE(6,*)'TEST',obs(i)%stnr
    DO j=2,obs(i)%ntim-1
       IF ( ABS(obs(i)%o(j)%val(wp_ind)-err_ind ) < 1.e-6 ) CYCLE

       !WRITE(6,*)  obs(i)%o(j  )%date,          &
                   !obs(i)%o(j  )%time,          &
                   !obs(i)%o(j-1)%val(wp_ind),   &
                   !obs(i)%o(j  )%val(wp_ind),   &
                   !obs(i)%o(j+1)%val(wp_ind)

       IF ( (obs(i)%o(j  )%val(wp_ind) < 1.e-6)   .AND.    &
            (obs(i)%o(j-1)%val(wp_ind) > 1.e-6)   .AND.    &
            (obs(i)%o(j+1)%val(wp_ind) > 1.e-6) )          &
             obs(i)%o(j  )%val(wp_ind) = err_ind
    ENDDO
 ENDDO
 ENDIF

 RETURN

END SUBROUTINE read_windp_obs
