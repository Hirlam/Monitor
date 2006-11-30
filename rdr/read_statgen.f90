SUBROUTINE read_statgen

 USE data
 USE functions
 USE timing
 USE constants

 IMPLICIT NONE


 INTEGER :: i,ii,j,k,l,			&
            ierr = 0,			&
            istnr = 0,			&
            stat_i,				&
            stations(100000),	&
            max_found_stat,		&
            timing_id,                  &
            obsnr,wmonr,date
            
 
 
 REAL ::lat,lon,val,psss,pes,prr,prrt

 CHARACTER(LEN=99) :: fname ='/data/proj/hirfou/uandrae/verification/STATGEN/tab.dat'
 CHARACTER(LEN=20) :: cname  = ''
 CHARACTER(LEN=11) :: cdate = ''
 INTEGER, PARAMETER :: pl = 3
 INTEGER :: printlev = 1

 LOGICAL :: use_stnlist

!----------------------------------------------------------

 stations       = 0
 max_found_stat = 0

 use_stnlist =(  MAXVAL(stnlist) > 0 )

 CALL allocate_obs

 !
 ! Loop over all times
 !

 TIME_LOOP : DO

 !
 ! Read obs data
 !

       OPEN(lunin,file=fname,status='old',iostat=ierr)

       IF (ierr.NE.0) THEN
  
          IF(print_read > 1 )WRITE(6,*)'Could not open:',TRIM(fname)
          EXIT TIME_LOOP

       ENDIF

       IF(print_read > 0 ) WRITE(6,*)'READ ',fname

       ! Skip header
       DO i=1,12
          READ(lunin,*)
       ENDDO

       READ_STATION_OBS : DO 

          READ(lunin,*,iostat=ierr)lon,lat,val,obsnr,wmonr,istnr,cdate,psss,pes,prr,prrt

          READ(cdate(1:8),'(I8.8)')date

          IF (ierr  == -1) THEN
             EXIT TIME_LOOP
          ENDIF

          !
          ! If no precipitation is observed then cycle
          !

          IF ( ABS(prr + 99.0) < 1.e-6 ) CYCLE READ_STATION_OBS
          IF ( ABS(prr +  1.0) < 1.e-6 ) prr = 0.0

          !
          ! Find station index
          !

          IF(stations(istnr) == -1 ) CYCLE READ_STATION_OBS

          IF(stations(istnr) == 0) THEN
           
             stat_i = 0
             IF ( use_stnlist ) THEN
                DO ii=1,maxstn
                   IF (istnr == stnlist(ii) ) stat_i = ii
                ENDDO
                IF ( stat_i == 0 ) THEN
                   stations(istnr) = -1
                   CYCLE READ_STATION_OBS
                ENDIF
             ENDIF

             IF (stat_i == 0 ) THEN 
                max_found_stat  = max_found_stat + 1
                stnlist(max_found_stat)= istnr
             ELSE
                max_found_stat  = stat_i
             ENDIF

             stations(istnr) = max_found_stat 
             obs(max_found_stat)%active = .TRUE.
             obs(max_found_stat)%stnr   = istnr
             obs(max_found_stat)%lat    = lat
             obs(max_found_stat)%lon    = lon

             IF (max_found_stat > maxstn) THEN
                WRITE(6,*)'Increase maxstn',max_found_stat
                CALL abort
             ENDIF

             IF (printlev > 1 ) WRITE(6,*)'Added :',istnr,stations(istnr)

          ENDIF

          !
          ! Station is found, add time
          !

          stat_i = stations(istnr)

          i = obs(stat_i)%ntim + 1

          ALLOCATE(obs(stat_i)%o(i)%date)
          ALLOCATE(obs(stat_i)%o(i)%time)
          ALLOCATE(obs(stat_i)%o(i)%val(nparver))

          obs(stat_i)%ntim      = i
          obs(stat_i)%o(i)%date = date
          obs(stat_i)%o(i)%time = 0
          obs(stat_i)%o(i)%val  = err_ind
          IF (printlev > 1 ) WRITE(6,*)'New date :',istnr,date

          if (pd_ind /= 0) obs(stat_i)%o(i)%val(pd_ind)  = prr

       ENDDO READ_STATION_OBS

       CLOSE(lunin)

       EXIT TIME_LOOP

 ENDDO TIME_LOOP

 DO i=1,maxstn
    obs(i)%active = ( obs(i)%ntim > 0 )
 ENDDO

 RETURN

END SUBROUTINE read_statgen
