SUBROUTINE read_windp_oper_obs2

 USE data

 IMPLICIT NONE

 INTEGER, PARAMETER :: lunwp = 45
 INTEGER :: i,j,jj,ierr,  &
            wdate,wtime,  &
            cdate,ctime,  &
            wpi

 REAL :: wpo

 CHARACTER(LEN=99) :: cname =''
 CHARACTER(LEN= 3) :: cstn  = ''
 CHARACTER(LEN=10) :: datec = ''

 LOGICAL :: stnlist_found = .FALSE.

 ! Station list 
 INTEGER :: id,type
 REAL    :: lat,lon,hubhgt,hgt
 CHARACTER(LEN=50) :: sname
 ! ------------------------------------------------------------------

 CALL allocate_obs
 stnlist_found =  ( SUM(stnlist) /= 0 ) 

 IF ( stnlist_found ) THEN
    obs%stnr = stnlist
 ENDIF

 !
 ! Read windp_station.dat
 !

 OPEN(UNIT=lunwp,FILE='windp_stations.dat',IOSTAT=ierr)

 READ_WINDP : IF ( ierr == 0 ) THEN
    WRITE(6,*)'Found windp_stations.dat'
    ierr = 0

    !
    ! Read header
    !
    HEADER_LOOP : DO
      READ(lunwp,*)ierr
      IF ( ierr == 1 ) EXIT HEADER_LOOP
    ENDDO HEADER_LOOP

    !
    ! Read body
    !

    READ(lunwp,*)jj
    WRITE(6,*)'Number of stations in list = ',jj
    WRITE(6,*)

    IF ( jj > maxstn .AND. .NOT. stnlist_found ) THEN
       WRITE(6,*)'Increase maxstn',jj,maxstn
       CALL abort
    ENDIF

    READ_STATION : DO i=1,jj
       sname = ''
       READ(lunwp,'(I10,X,I3,x,4(f7.3,x),A50)',IOSTAT=ierr)     &
       id,type,lat,lon,hubhgt,hgt,sname

       WRITE(6,'(I10,X,I3,x,4(f7.3,x),A50)')     &
       id,type,lat,lon,hubhgt,hgt,sname

       !
       ! Set station parameters
       !

       IF ( stnlist_found ) THEN
          DO j=1,maxstn
             IF ( id == obs(j)%stnr ) THEN
                obs(j)%lat = lat
                obs(j)%lon = lon
                obs(j)%hgt = hgt
                !station_name(j) = TRIM(sname)
                CYCLE READ_STATION
             ENDIF
          ENDDO
       ELSE
          obs(i)%stnr     = id
          obs(i)%lat      = lat
          obs(i)%lon      = lon
          obs(i)%hgt      = hgt
          stnlist(i)      = id
          !station_name(i) = TRIM(sname)
       ENDIF

    ENDDO READ_STATION

    CLOSE(lunwp)

 ELSEIF ( SUM(stnlist) == 0 ) THEN
    WRITE(6,*)'Please give station numbers or windp_stations.dat'
    CALL abort
 ENDIF READ_WINDP

  !
  ! Set lat and lon
  !

 !
 ! Loop over all times
 !

 STATION_LOOP : DO i=1,maxstn
 
  IF (obs(i)%stnr == 0 ) THEN
     obs(i)%active = .FALSE.
     CYCLE STATION_LOOP
  ENDIF
  obs(i)%active = .TRUE.

  j = 0

  cdate = sdate
  ctime = stime*10000

 TIME_LOOP : DO 

    !
    ! Read obs data
    !

    WRITE(datec,'(I8.8,I2.2)')cdate,ctime/10000
    WRITE(cstn,'(I3.3)')obs(i)%stnr

    cname = TRIM(obspath)//cstn//'/'//datec//'.dat'

    OPEN(lunin,file=cname,status='old',iostat=ierr)

    IF (ierr /= 0) THEN
       IF(print_read > 0)WRITE(6,*)'Could not open:',TRIM(cname)

       wdate = cdate
       wtime = ctime
       CALL adddtg(wdate,wtime,obint*3600,cdate,ctime)

       IF(cdate >  edate_obs) EXIT TIME_LOOP
       IF(cdate >= edate_obs .AND. ctime/10000 > etime_obs) EXIT TIME_LOOP

       CYCLE TIME_LOOP
    ENDIF

    IF (print_read > 0 )WRITE(6,*)'READ ',TRIM(cname)


    READ(lunin,*,iostat=ierr)wpo

    j  =  j + 1

    ALLOCATE(obs(i)%o(j)%date)
    ALLOCATE(obs(i)%o(j)%time)
    ALLOCATE(obs(i)%o(j)%val(nparver))

    obs(i)%o(j)%val = err_ind
    obs(i)%ntim      = j
    obs(i)%o(j)%val  = err_ind
    obs(i)%o(j)%date = cdate
    obs(i)%o(j)%time = ctime/10000
    IF ( use_pos ) obs(i)%pos(cdate * 100 + ctime/10000) = j

    IF (wp_ind /= 0) obs(i)%o(j)%val(wp_ind) = wpo


    CLOSE(lunin)

    !
    ! Step time
    !

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,obint*3600,cdate,ctime)

    IF(cdate >  edate_obs) EXIT TIME_LOOP
    IF(cdate >= edate_obs .AND. ctime/10000 > etime_obs) EXIT TIME_LOOP

  ENDDO TIME_LOOP

 ENDDO STATION_LOOP

 DO i=1,maxstn
    obs(i)%active = ( obs(i)%ntim > 0 )
 ENDDO

 RETURN

END SUBROUTINE read_windp_oper_obs2
