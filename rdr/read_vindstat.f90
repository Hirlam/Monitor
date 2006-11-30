SUBROUTINE read_vindstat

 USE data

 IMPLICIT NONE

 INTEGER :: i,ii,j,ierr,    &
            wdate,wtime,    &
            cdate,ctime,    &
            istnr,          &
            stations(1000), &
            max_found_stat, &
            ip,tt,ph,wp,wpp

 REAL :: ff

 CHARACTER(LEN=99) :: cname =''
 CHARACTER(LEN= 3) :: cerr  = ''
 CHARACTER(LEN= 8) :: datec = ''

 LOGICAL :: use_stnlist = .FALSE.

 ! ------------------------------------------------------------------

 CALL allocate_obs
 use_stnlist =  ( SUM(stnlist) /= 0 ) 
 stations = 0

 IF ( use_stnlist ) THEN
    obs%stnr = stnlist
 ENDIF

 cdate = sdate
 ctime = stime

 TIME_LOOP : DO 

    !
    ! Read obs data
    !

    WRITE(datec,'(I8.8)')cdate

    cname = TRIM(obspath)//datec//'.dat'
    OPEN(lunin,file=cname,status='old',iostat=ierr)

    IF (ierr /= 0) THEN
       IF(print_read > 0)WRITE(6,*)'Could not open:',TRIM(cname)

       wdate = cdate
       wtime = ctime
       CALL adddtg(wdate,wtime,24*3600,cdate,ctime)

       IF(cdate >  edate) EXIT TIME_LOOP
       IF(cdate >= edate .AND. ctime/10000 > etime) EXIT TIME_LOOP
       CYCLE TIME_LOOP
    ENDIF

    IF (print_read > 0 )WRITE(6,*)'READ ',TRIM(cname)

    READ_LOOP : DO

       READ(lunin,*,IOSTAT=ierr)istnr,cerr,ip,ff,tt,ph,wp,wpp

       IF ( ierr /= 0 ) EXIT READ_LOOP
       IF ( TRIM(cerr) == 'FEL' ) CYCLE READ_LOOP

       !
       ! Find station index
       !
   
       IF(stations(istnr) == 0) THEN
              
          i = 0
          IF ( use_stnlist ) THEN
             DO ii=1,maxstn
                IF (istnr == stnlist(ii) ) i = ii
             ENDDO
             IF ( i == 0 ) CYCLE READ_LOOP
          ENDIF
   
          IF ( i == 0 ) THEN 
             max_found_stat  = max_found_stat + 1
             stnlist(max_found_stat)= istnr
          ELSE
             max_found_stat  = i
          ENDIF
   
          stations(istnr) = max_found_stat 
          obs(max_found_stat)%active = .TRUE.
          obs(max_found_stat)%stnr   = istnr
   
          IF (max_found_stat > maxstn) THEN
             WRITE(6,*)'Increase maxstn',max_found_stat
             CALL abort
          ENDIF
   
       ENDIF
   
       i = stations(istnr)
       j = obs(i)%ntim + 1 

       ALLOCATE(obs(i)%o(j)%date)
       ALLOCATE(obs(i)%o(j)%time)
       ALLOCATE(obs(i)%o(j)%val(nparver))

       obs(i)%ntim      = j
       obs(i)%o(j)%val  = err_ind
       obs(i)%o(j)%date = cdate
       obs(i)%o(j)%time = ctime/10000
       IF ( use_pos ) obs(i)%pos(cdate * 100 + ctime/10000 ) = j

       IF (wp_ind /= 0                ) obs(i)%o(j)%val(wp_ind) = ip
       IF (wh_ind /= 0 .AND. ph >  0  ) obs(i)%o(j)%val(wh_ind) = wp 
       IF (tt_ind /= 0 .AND. tt /= 99 ) obs(i)%o(j)%val(tt_ind) = tt
       IF (ff_ind /= 0                ) obs(i)%o(j)%val(ff_ind) = ff

    ENDDO READ_LOOP

    CLOSE(lunin)

    !
    ! Step time
    !

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,obint*3600,cdate,ctime)

    IF(cdate >  edate) EXIT TIME_LOOP
    IF(cdate >= edate .AND. ctime/10000 > etime) EXIT TIME_LOOP

 ENDDO TIME_LOOP

 DO i=1,maxstn
    obs(i)%active = ( obs(i)%ntim > 0 )
 ENDDO

 RETURN

END SUBROUTINE read_vindstat
