SUBROUTINE read_windp_oper_obs

 USE data

 IMPLICIT NONE

 INTEGER :: i,j,jj,ierr,  &
            wdate,wtime,  &
            cdate,ctime,  &
            wpi

 REAL :: wpo

 CHARACTER(LEN=99) :: cname =''
 CHARACTER(LEN= 3) :: cstn  = ''
 CHARACTER(LEN= 8) :: datec = ''

 ! ------------------------------------------------------------------

 CALL allocate_obs

 obs%stnr = stnlist

 !
 ! Loop over all times
 !

 STATION_LOOP : DO i=1,maxstn
 
  obs(i)%active = .TRUE.

  j = 0

  cdate = sdate
  ctime = stime

  !
  ! Set lat and lon
  !

 TIME_LOOP : DO 

    !
    ! Read obs data
    !

       WRITE(datec,'(I8.8)')cdate
       WRITE(cstn,'(I3.3)')obs(i)%stnr

       cname = TRIM(obspath)//cstn//'_'//datec//'00.dat'

       OPEN(lunin,file=cname,status='old',iostat=ierr)
   
       IF (ierr.NE.0) THEN
          IF(print_read > 0)WRITE(6,*)'Could not open:',TRIM(cname)
          wdate = cdate
          wtime = ctime
          CALL adddtg(wdate,wtime,fcint*3600,cdate,ctime)

          IF(cdate >  edate) EXIT TIME_LOOP
          IF(cdate >= edate .AND. ctime/10000 > etime) EXIT TIME_LOOP

          CYCLE TIME_LOOP
       ENDIF

       IF (print_read > 0 )WRITE(6,*)'READ ',TRIM(cname)

       DAY_TIME_LOOP : DO jj=1,24
   
          READ(lunin,*,iostat=ierr)wdate,wpi
          wpo = FLOAT(wpi)
   
          j  =  j + 1

          ALLOCATE(obs(i)%o(j)%date)
          ALLOCATE(obs(i)%o(j)%time)
          ALLOCATE(obs(i)%o(j)%val(nparver))

          obs(i)%o(j)%val = err_ind
          obs(i)%ntim      = j
          obs(i)%o(j)%val  = err_ind
          obs(i)%o(j)%date = wdate/100
          obs(i)%o(j)%time = MOD(wdate,100)

          SELECT CASE(special_flag)
          CASE (1)
             IF (wp_ind /= 0) obs(i)%o(j)%val(wp_ind) = SQRT(wpo)
          CASE DEFAULT
             IF (wp_ind /= 0) obs(i)%o(j)%val(wp_ind) = wpo
          END SELECT

       ENDDO DAY_TIME_LOOP

       CLOSE(lunin)

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

 DO i=1,maxstn
    obs(i)%active = ( obs(i)%ntim > 0 )
 ENDDO

 RETURN

END SUBROUTINE read_windp_oper_obs
