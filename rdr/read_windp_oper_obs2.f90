SUBROUTINE read_windp_oper_obs2

 USE data
 USE cpt

 IMPLICIT NONE

 INTEGER, PARAMETER :: lunwp = 45
 INTEGER :: i,ii,j,jj,ierr,  &
            wdate,wtime,     &
            cdate,ctime,     &
            idum,flag

 REAL :: wpo,rdum

 CHARACTER(LEN=99) :: cname =''
 CHARACTER(LEN= 6) :: cstn  = ''
 CHARACTER(LEN= 8) :: datec = ''
 CHARACTER(LEN=10) :: cdum = ''

 LOGICAL :: stnlist_found = .FALSE.

 ! Station list 
 INTEGER :: id,id_lp,x,y,hh
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

 OPEN(UNIT=lunwp,FILE='windp_stations.dat',STATUS='OLD',IOSTAT=ierr)

 READ_WINDP : IF ( ierr == 0 ) THEN

    WRITE(6,*)'Found windp_stations.dat'
    ierr = 0

    READ(lunwp,*)jj
    WRITE(6,*)'Number of stations in list = ',jj
    WRITE(6,*)

    IF ( jj > maxstn .AND. .NOT. stnlist_found ) THEN
       WRITE(6,*)'Increase maxstn',jj,maxstn
       CALL abort
    ENDIF

    READ_STATION : DO i=1,jj
       sname = ''
       READ(lunwp,'(2(I7,1X),I3,1X,A10,1X,F6.0,2(X,I8),1X,F5.0,1X,A40)',IOSTAT=ierr) &
       id,id_lp,idum,cdum,rdum,x,y,hubhgt,sname

       CALL rikgre(x,y,lat,lon)

       WRITE(6,'(2I10,X,A10,x,3(f7.3,x),A50)')                  &
       id,id_lp,cdum,lat,lon,hubhgt,sname

       !
       ! Set station parameters
       !

       IF ( stnlist_found ) THEN
          DO j=1,maxstn
             IF ( id == obs(j)%stnr ) THEN
                obs(j)%lat = lat
                obs(j)%lon = lon
                obs(j)%hgt = hgt
                CYCLE READ_STATION
             ENDIF
          ENDDO
       ELSE
          obs(i)%stnr     = id
          obs(i)%lat      = lat
          obs(i)%lon      = lon
          obs(i)%hgt      = hgt
          stnlist(i)      = id
       ENDIF

    ENDDO READ_STATION

    CLOSE(lunwp)

 ELSEIF ( SUM(stnlist) == 0 ) THEN
    WRITE(6,*)'Please give station numbers or windp_stations.dat'
    CALL abort
 ENDIF READ_WINDP

 !
 ! Loop over all times
 !

 cdate = sdate
 ctime = stime

 TIME_LOOP : DO 

    !
    ! Read obs data
    !

    WRITE(datec,'(I8.8)')cdate
    cname = TRIM(obspath)//'windp_'//TRIM(datec)//'.dat'

    OPEN(lunin,file=cname,status='old',iostat=ierr)

    IF (ierr /= 0) THEN
       IF(print_read > 0)WRITE(6,'(2A)')'Could not open:',TRIM(cname)

       wdate = cdate
       wtime = ctime
       CALL adddtg(wdate,wtime,24*3600,cdate,ctime)

       IF(cdate >  edate_obs) EXIT TIME_LOOP
       IF(cdate >= edate_obs .AND. ctime/10000 > etime_obs) EXIT TIME_LOOP

       CYCLE TIME_LOOP
    ENDIF

    IF (print_read > 0 )WRITE(6,'(2A)')'READ ',TRIM(cname)

    i=1
    READ_LOOP : DO 

      READ(lunin,*,IOSTAT=ierr)id,hh,wpo,flag
      IF ( ierr /= 0 ) EXIT

      IF ( id /= obs(i)%stnr ) THEN
         ii = 0
         DO i=1,maxstn
            IF ( id == obs(i)%stnr ) THEN
              ii = i
              EXIT
            ENDIF
         ENDDO
         IF ( ii == 0 ) CYCLE READ_LOOP
         i = ii
      ENDIF

!     IF ( flag < 0 ) THEN
!        IF(print_read > 0 ) &
!        WRITE(6,*)'Skip observation',cdate,hh,id,wpo,flag
!        CYCLE READ_LOOP
!     ENDIF

      obs(i)%ntim = obs(i)%ntim + 1
      j = obs(i)%ntim

      ALLOCATE(obs(i)%o(j)%date)
      ALLOCATE(obs(i)%o(j)%time)
      ALLOCATE(obs(i)%o(j)%val(nparver))

      obs(i)%o(j)%val  = err_ind
      obs(i)%o(j)%date = cdate
      obs(i)%o(j)%time = hh

      IF ( use_pos ) obs(i)%pos(cdate * 100 + hh) = j

      IF (wp_ind /= 0) obs(i)%o(j)%val(wp_ind) = wpo
      IF (ff_ind /= 0) obs(i)%o(j)%val(ff_ind) = wpo
      IF (tt_ind /= 0) obs(i)%o(j)%val(tt_ind) = 280.0

   ENDDO READ_LOOP

   CLOSE(lunin)

   !
   ! Step time
   !

   wdate = cdate
   wtime = ctime
   CALL adddtg(wdate,wtime,24*3600,cdate,ctime)

   IF(cdate >  edate_obs) EXIT TIME_LOOP
   IF(cdate >= edate_obs .AND. ctime/10000 > etime_obs) EXIT TIME_LOOP

 ENDDO TIME_LOOP

 DO i=1,maxstn
    obs(i)%active = ( obs(i)%ntim > 0 )
 ENDDO

 RETURN

END SUBROUTINE read_windp_oper_obs2
