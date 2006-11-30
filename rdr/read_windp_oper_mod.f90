SUBROUTINE read_windp_oper_mod

 USE data

 IMPLICIT NONE

 INTEGER :: i,j,jj,k,kk,l,ierr,idum,            &
            wdate,wtime,cdate,ctime,            &
            fc_ind

 REAL :: uu,wpm,wpk

 LOGICAL :: time_is_not_allocated = .TRUE.

 CHARACTER(LEN=99) :: cname = ''
 CHARACTER(LEN= 3) :: cstn  = ''
 CHARACTER(LEN=10) :: datec = ''

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

       READ_LOOP : DO 
   
          READ(lunin,*,iostat=ierr)idum,uu,wpm,wpk
  
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
             IF (ff_ind /= 0) hir(i)%o(j)%nal(k:k+1,fc_ind,ff_ind) = uu
             IF (wp_ind /= 0) hir(i)%o(j)%nal(k    ,fc_ind,wp_ind) = wpm
             IF (wp_ind /= 0) hir(i)%o(j)%nal(k+1  ,fc_ind,wp_ind) = wpk
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
