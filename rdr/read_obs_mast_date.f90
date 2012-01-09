SUBROUTINE read_obs_mast_date

 !
 ! Read flax and mast data 
 ! and organize for evalutaion
 ! Mast iformation is set in the module mast_data
 !
 ! Ulf Andrae, SMHI, 2008
 !
 
 USE data
 USE mast_data

 IMPLICIT NONE

 INTEGER :: i,ii,ierr,stat_i,j,      &
            cdate,ctime,             &
            wdate,wtime,             &
            newdate,newtime,         &
            stations(100),           &
            max_found_stat,istnr,    &
            nread,m,n
         
 REAL,    ALLOCATABLE :: val(:,:),val1(:)
 INTEGER, ALLOCATABLE :: date(:),hhmm(:),num(:)

 LOGICAL :: use_stnlist
         
 CHARACTER(LEN=  8) :: ccdate
 CHARACTER(LEN= 12) :: ctstamp
 CHARACTER(LEN=100) :: fname=''
 CHARACTER(LEN= 10) :: invar(10) = '#'

 !---------------------------------------------------------------------

 invar( 1) = 'WT'
 invar( 2) = 'WQ'
 invar( 3) = 'UW'
 invar( 4) = 'TMAST'
 invar( 5) = 'XX'
 invar( 6) = 'TZ'
 invar( 7) = 'RHMAST'
 invar( 8) = 'FFMAST'
 invar( 9) = 'GR'
 invar(10) = 'LU'

 stations       = 0
 use_stnlist    = ( MAXVAL(stnlist) > 0 )
 max_found_stat = 0

 !
 ! Allocate obs arrays if it is not already done
 !

 CALL allocate_obs

 IF( print_read > 0 ) WRITE(6,*)'OBS is ALLOCATED'

 STATION_LOOP : DO istnr=1,max_flux_station

    !
    ! Check if this station should be used
    !

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

    stat_i = stations(istnr)

    IF ( print_read > 0 ) WRITE(6,*)'Working with station',stat_i,istnr,stations(istnr),stname(istnr)


    !
    ! The station is used, fill the date/time arrays
    !

    cdate = sdate
    ctime = stime
    i = 0

    FILL_LOOP : DO

       IF ( cdate > edate ) EXIT FILL_LOOP

       i = i + 1

       ALLOCATE(obs(stat_i)%o(i)%date)
       ALLOCATE(obs(stat_i)%o(i)%time)
       ALLOCATE(obs(stat_i)%o(i)%val(nparver))

       obs(stat_i)%ntim       = i
       obs(stat_i)%o(i)%date  = cdate
       obs(stat_i)%o(i)%time  = ctime
       obs(stat_i)%o(i)%val   = err_ind

       wdate = cdate
       wtime = ctime*10000
       CALL adddtg(wdate,wtime,3600*obint,cdate,ctime)
       ctime = ctime/10000
       
    ENDDO FILL_LOOP 


    !
    ! First read the flux data
    ! Allocate working arrays
    !
 
    ALLOCATE(date(  obs(stat_i)%ntim*obint*12),   &
             hhmm(  obs(stat_i)%ntim*obint*12),   &
              val(4,obs(stat_i)%ntim*obint*12),   &
              val1(4),num(4))


        i = 0
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

          READ(lunin,*,IOSTAT=ierr)ctstamp,val1
          IF ( print_read > 1 ) WRITE(6,*)ctstamp,val1
          IF ( ierr /= 0 ) EXIT FLUX_READ_LOOP
          READ(ctstamp,'(I8.8,I4.4)')newdate,newtime

          IF ( ANY(val1 > -99998.)) THEN
                i   = i+1
           val(:,i) = val1
            date(i) = newdate 
            hhmm(i) = newtime 
          ENDIF

       ENDDO FLUX_READ_LOOP

       CLOSE(lunin)

       wdate = cdate
       wtime = ctime*10000
       CALL adddtg(wdate,00,3600*24,cdate,ctime)
       IF ( cdate > edate ) EXIT FLUX_DATA

    ENDDO FLUX_DATA


    nread = i

    !
    ! Store data
    !
 
    val1 = 0.
    num  = 0

    FLUX_STORAGE : DO j=1,nread

       IF ( print_read > 1 ) WRITE(6,*)'check:date,hhmm',date(j),hhmm(j)

       WHERE ( val(:,j) > -99998. ) 
         val1 = val1 + val(:,j)
         num  = num  + 1
       ENDWHERE

       IF ( MOD(hhmm(j),100) == 0 ) THEN


          !
          ! New hour is found: store data
          !

          ii = 0
          DO i=1,obs(stat_i)%ntim
             IF ( date(j)     == obs(stat_i)%o(i)%date .AND. &
                  hhmm(j)/100 == obs(stat_i)%o(i)%time       )  THEN
                  ii = i
                  EXIT
             ENDIF
          ENDDO

          IF ( ii /= 0 ) THEN

             IF(print_read > 1 ) WRITE(6,*)'Added ',ii,date(j),hhmm(j),num

             WHERE ( num == 0 )
               val1 = err_ind
             ELSEWHERE
               val1 = val1 / FLOAT(num)
             ENDWHERE
            
             PARVER_LOOP : DO m=1,nparver
               INVAR_LOOP : DO n=1,3
                 IF ( varprop(m)%id == invar(n) ) THEN

                   ! Check for missing data flag
                   IF ( .NOT. qca(val1(n),err_ind) ) CYCLE PARVER_LOOP

                   ! Check for missing data / gross error
                   IF ( qclr(val1(n),varprop(m)%llim) .AND. &
                        qcur(val1(n),varprop(m)%ulim) )     &
                   obs(stat_i)%o(i)%val(m) = val1(n)

                 ENDIF

               ENDDO INVAR_LOOP
             ENDDO PARVER_LOOP
 
          ENDIF 

          val1 = 0.
          num  = 0

       ENDIF 


    ENDDO FLUX_STORAGE


    !
    ! Mast data
    !

    DEALLOCATE(val,val1,num)
    ALLOCATE(val(7,obs(stat_i)%ntim*obint*12),val1(4:10),num(7))


    IF ( print_read > 0 ) WRITE(6,*)'Read mast data'

        i = 0
    cdate = sdate
    ctime = stime

    MAST : DO

       WRITE(ccdate,'(I8.8)')cdate

       fname = TRIM(obspath)//'Meas_'//          &
               TRIM(stname(istnr))//'_Mast_'//   &
               TRIM(ccdate)//'.txt'

       OPEN(lunin,FILE=fname,STATUS='old',IOSTAT=ierr)

       IF ( ierr /= 0 ) THEN
          WRITE(6,'(2A)')'Could not open ',TRIM(fname)
          wdate = cdate
          wtime = ctime*10000
          CALL adddtg(wdate,00,3600*24,cdate,ctime)
          IF ( cdate > edate ) EXIT MAST
          CYCLE MAST
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

          READ(lunin,*,IOSTAT=ierr)ctstamp,val1
          IF ( print_read > 1 ) WRITE(6,*)ctstamp,val1
          IF ( ierr /= 0 ) EXIT MAST_READ_LOOP
          READ(ctstamp,'(I8.8,I4.4)')newdate,newtime

          IF ( ANY(val1 > -99998.)) THEN
                i   = i+1
           val(:,i) = val1
            date(i) = newdate 
            hhmm(i) = newtime 
          ENDIF

       ENDDO MAST_READ_LOOP

       CLOSE(lunin)

       wdate = cdate
       wtime = ctime*10000
       CALL adddtg(wdate,00,3600*24,cdate,ctime)
       IF ( cdate > edate ) EXIT MAST

    ENDDO MAST


    nread = i

    !
    ! Store data
    !
 
    val1= 0.
    num = 0

    MAST_STORAGE : DO j=1,nread

       IF ( print_read > 1 ) WRITE(6,*)'date,hhmm',date(j),hhmm(j)

       WHERE ( val(:,j) > -99998. ) 
         val1 = val1 + val(:,j)
         num  = num  + 1
       ENDWHERE

       IF ( MOD(hhmm(j),100) == 0 ) THEN

          ii = 0
          DO i=1,obs(stat_i)%ntim
             IF ( date(j)     == obs(stat_i)%o(i)%date .AND. &
                  hhmm(j)/100 == obs(stat_i)%o(i)%time       )  THEN
                  ii = i
                  EXIT
             ENDIF
          ENDDO

          IF ( ii /= 0 ) THEN

             IF(print_read > 1 ) WRITE(6,*)'Added ',date(j),hhmm(j),i

             WHERE ( num == 0 )
               val1 = err_ind
             ELSEWHERE
               val1 = val1 / FLOAT(num)
             ENDWHERE

             PARVER_LOOP2 : DO m=1,nparver
               INVAR_LOOP2 : DO n=4,10
                 IF ( varprop(m)%id == invar(n) ) THEN

                   ! Check for missing data flag
                   IF ( .NOT. qca(val1(n),err_ind) ) CYCLE PARVER_LOOP2

                   ! Special treatment of some variabels
                   SELECT CASE(invar(n))

                   CASE('TZ')
                    val1(n) = val1(n) / dz(istnr)
                   END SELECT

                   ! Check for missing data / gross error
                   IF ( qclr(val1(n),varprop(m)%llim) .AND. &
                        qcur(val1(n),varprop(m)%ulim) )     &
                   obs(stat_i)%o(i)%val(m) = val1(n)

                 ENDIF

               ENDDO INVAR_LOOP2
             ENDDO PARVER_LOOP2

          ENDIF 

          val1 = 0.
          num  = 0

       ENDIF 

    ENDDO MAST_STORAGE

    DEALLOCATE(date,hhmm,val,val1,num)

 ENDDO STATION_LOOP


 ! Set station names
 ALLOCATE(station_name(maxstn))

 DO i=1,maxstn
   IF ( obs(i)%active ) station_name(i)=stname(obs(i)%stnr)
 ENDDO

END SUBROUTINE read_obs_mast_date
