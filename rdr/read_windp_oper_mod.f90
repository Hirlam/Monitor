SUBROUTINE read_windp_oper_mod

 USE data

 IMPLICIT NONE

 INTEGER :: i,j,jj,k,kk,l,ierr,idum,            &
            wdate,wtime,cdate,ctime,            &
            fc_ind

 REAL :: uu,wpm,wpk,temp

 LOGICAL :: time_is_not_allocated = .TRUE.

 CHARACTER(LEN=99) :: cname = ''
 CHARACTER(LEN= 6) :: cstn  = ''
 CHARACTER(LEN= 3) :: cstn3  = ''
 CHARACTER(LEN=10) :: datec = '',myexpname

 ! ------------------------------------------------------------------

 CALL allocate_mod

 hir%stnr = obs%stnr
 hir%lat  = obs%lat
 hir%lon  = obs%lon
 hir%hgt  = obs%hgt

 !
 ! Loop over all times
 !

 STATION_LOOP : DO i=1,maxstn
 
  IF ( hir(i)%stnr == 0 ) CYCLE STATION_LOOP

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

    EXP_LOOP : DO k=1,nexp

       myexpname=TRIM(expname(k))
       IF ( k == 2 ) CYCLE
       IF ( k == 3 ) myexpname ='G05'

       WRITE(datec,'(I8.8,I2.2)')cdate,ctime/10000
       WRITE(cstn,'(I6.6)')hir(i)%stnr

       cname = TRIM(modpath(k))//TRIM(myexpname)   &
                  //'_'//cstn//'_'//datec//'.fc'

       OPEN(lunin,file=cname,status='old',iostat=ierr)
   
       IF (ierr /= 0) THEN

          IF(print_read > 0)WRITE(6,'(2A)')'Could not open:',TRIM(cname)

          IF ( hir(i)%stnr < 999 ) THEN
             WRITE(cstn3,'(I3.3)')hir(i)%stnr
             cname = TRIM(modpath(k))//TRIM(myexpname)   &
                     //'_'//cstn3//'_'//datec//'.fc'
             OPEN(lunin,file=cname,status='old',iostat=ierr)
          ENDIF
       ENDIF

       IF (ierr /= 0) THEN
          IF(print_read > 0)WRITE(6,'(2A)')'Could not open:',TRIM(cname)
          CYCLE EXP_LOOP
       ENDIF

       IF (print_read > 0 )WRITE(6,'(2A)')'READ ',TRIM(cname)

       READ_LOOP : DO 
   
          IF ( tt_ind /= 0 ) THEN
             READ(lunin,*,iostat=ierr)idum,uu,wpm,wpk,temp
          ELSE
             READ(lunin,*,iostat=ierr)idum,uu,wpm,wpk
          ENDIF
  
          IF ( ierr /= 0 ) EXIT READ_LOOP
          IF ( idum > MAXVAL(fclen) ) EXIT READ_LOOP

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

          fc_ind = 0
          DO jj=1,nfclengths
             IF ( idum == fclen(jj) )  fc_ind = jj
          ENDDO
          
          IF ( fc_ind /= 0 ) THEN
             IF ( k == 1 ) THEN
                IF ( ABS(uu-wpm) <1.e-6 ) THEN
                   IF (ff_ind /= 0) hir(i)%o(j)%nal(k  ,fc_ind,ff_ind) = uu
                   IF (ff_ind /= 0) hir(i)%o(j)%nal(k+1,fc_ind,ff_ind) = wpk
                ELSE
                   IF (ff_ind /= 0) hir(i)%o(j)%nal(k:k+1,fc_ind,ff_ind) = uu
                ENDIF
                IF (wp_ind /= 0) hir(i)%o(j)%nal(k    ,fc_ind,wp_ind) = wpm
                IF (wp_ind /= 0) hir(i)%o(j)%nal(k+1  ,fc_ind,wp_ind) = wpk
                IF (tt_ind /= 0) hir(i)%o(j)%nal(k:k+1,fc_ind,tt_ind) = temp
             ELSE
                IF ( ABS(uu-wpm) <1.e-6 ) THEN
                   IF (ff_ind /= 0) hir(i)%o(j)%nal(k,fc_ind,ff_ind) = wpk
                ELSE
                   IF (ff_ind /= 0) hir(i)%o(j)%nal(k,fc_ind,ff_ind) = uu
                ENDIF
             ENDIF
          ENDIF

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

END SUBROUTINE read_windp_oper_mod
