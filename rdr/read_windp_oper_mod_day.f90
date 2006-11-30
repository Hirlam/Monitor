SUBROUTINE read_windp_oper_mod_day

 USE data

 IMPLICIT NONE

 INTEGER, PARAMETER :: lunwp = 45
 INTEGER :: i,j,jj,k,kk,l,ierr,                 &
            idum_last,idum,                     &
            wdate,wtime,cdate,ctime,            &
            fc_ind

 REAL :: uu,wpm,wpk,awpm,awpk

 LOGICAL :: time_is_not_allocated = .TRUE.
 LOGICAL :: stnlist_found

 CHARACTER(LEN=99) :: cname = ''
 CHARACTER(LEN= 3) :: cstn  = ''
 CHARACTER(LEN=10) :: datec = ''

 ! Station list 
 INTEGER :: id,type
 REAL    :: lat,lon,hubhgt,hgt
 CHARACTER(LEN=50) :: sname
 ! ------------------------------------------------------------------

 CALL allocate_mod
 stnlist_found =  ( SUM(stnlist) /= 0 ) 

 IF ( stnlist_found ) THEN
    hir%stnr = stnlist
 ENDIF

 !
 ! Read windp_station.dat
 !

 OPEN(UNIT=lunwp,STATUS='OLD',FILE='windp_stations.dat',IOSTAT=ierr)

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
                hir(j)%lat = lat
                hir(j)%lon = lon
                hir(j)%hgt = hgt
                !station_name(j) = TRIM(sname)
                CYCLE READ_STATION
             ENDIF
          ENDDO
       ELSE
          hir(i)%stnr     = id
          hir(i)%lat      = lat
          hir(i)%lon      = lon
          hir(i)%hgt      = hgt
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
 ! Loop over all times
 !

 STATION_LOOP : DO i=1,maxstn
 
  hir(i)%active = .TRUE.

  j = 0

  cdate = sdate
  ctime = stime

  !
  ! Set lat and lon
  !

 TIME_LOOP : DO 

    !
    ! Read model data
    !

    time_is_not_allocated = .TRUE.

    EXP_LOOP : DO k=1,nexp,2

       WRITE(datec,'(I8.8,I2.2)')cdate,ctime/10000
       WRITE(cstn,'(I3.3)')hir(i)%stnr

       cname = TRIM(modpath(k))//TRIM(expname(k))   &
                  //'_'//cstn//'_'//datec//'.fc'

       OPEN(lunin,file=cname,status='old',iostat=ierr)
   
       IF (ierr.NE.0) THEN
          IF(print_read > 0)WRITE(6,*)'Could not open:',TRIM(cname)
          CYCLE EXP_LOOP
       ENDIF

       IF (print_read > 0 )WRITE(6,*)'READ ',TRIM(cname)

       idum_last = 0

       awpm = 0.
       awpk = 0.

       READ_LOOP : DO 
   
          READ(lunin,*,iostat=ierr)idum,uu,wpm,wpk
  
          IF ( ierr /= 0 ) EXIT READ_LOOP
          IF ( idum > MAXVAL(fclen) ) EXIT READ_LOOP

          awpm = wpm*(idum - idum_last ) + awpm
          awpk = wpk*(idum - idum_last ) + awpk

          idum_last = idum

          IF ( MOD(idum,24) /= 0     ) CYCLE
          IF ( time_is_not_allocated ) THEN

             j  =  j + 1

             ALLOCATE(hir(i)%o(j)%date)
             ALLOCATE(hir(i)%o(j)%time)
             ALLOCATE(hir(i)%o(j)%nal(nexp,nfclengths,nparver))
      
             hir(i)%ntim      = j
             hir(i)%o(j)%nal  = err_ind
             hir(i)%o(j)%date = cdate
             hir(i)%o(j)%time = ctime/10000

             time_is_not_allocated = .FALSE.

          ENDIF

          fc_ind = idum / 24
          
          IF (wh_ind /= 0) hir(i)%o(j)%nal(k    ,fc_ind,wh_ind) = awpm
          IF (wh_ind /= 0) hir(i)%o(j)%nal(k+1  ,fc_ind,wh_ind) = awpk

          awpm = 0.
          awpk = 0.

       ENDDO READ_LOOP

       CLOSE(lunin)

    ENDDO EXP_LOOP

    !
    ! Step time
    !

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,fcint*3600,cdate,ctime)
    IF(cdate >  edate) EXIT TIME_LOOP
    IF(cdate >= edate .AND. ctime/10000 > etime) EXIT TIME_LOOP

  ENDDO TIME_LOOP

 ENDDO STATION_LOOP

 ! Set active stations

 DO i=1,maxstn
    hir(i)%active = ( hir(i)%ntim > 0 )
 ENDDO

 RETURN

END SUBROUTINE read_windp_oper_mod_day
