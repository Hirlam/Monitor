SUBROUTINE read_any

 !
 ! Read data extracted with gl
 !
 ! Ulf Andrae, SMHI, Dec 2005
 !
 
 USE DATA
 USE CONSTANTS
 USE FUNCTIONS

 IMPLICIT NONE

 INTEGER :: i,j,k,          &
            ierr,           &
            date,fctime,    &
            jstart,         &
            cdate,ctime,    &
            wdate,wtime,    &
            fc_ind
 
 REAL :: val(10)

 CHARACTER(LEN=99) :: wname =' '
 CHARACTER(LEN= 8) ::  cnum =' '

 !------------------------------------------

 !
 ! Allocate model data
 !

 CALL allocate_mod

 hir%stnr = stnlist
 hir%lat  = obs%lat
 hir%lon  = obs%lon
 hir%hgt  = obs%hgt

 !
 ! Allocate all times for all stations since we do not
 ! know if all experiments contains all times
 !

 cdate = sdate
 ctime = stime * 10000

 k = 0
 DO 
    k = k+1
    DO i=1,maxstn

      ALLOCATE(hir(i)%o(k)%date)
      ALLOCATE(hir(i)%o(k)%time)
      ALLOCATE(hir(i)%o(k)%nal(nexp,nfclengths,nparver))

      hir(i)%o(k)%date = cdate
      hir(i)%o(k)%time = ctime / 10000
      hir(i)%o(k)%nal  = err_ind

    ENDDO

    CALL adddtg(cdate,ctime,fcint*3600,wdate,wtime)
    IF ( wdate > edate ) EXIT
    cdate = wdate
    ctime = wtime

 ENDDO

 hir%ntim = k

 !
 ! Loop over all stations and all experiments
 !

 STATION_LOOP : DO i=1,maxstn

       ! Create filename
       WRITE(cnum,'(I8.8)')hir(i)%stnr
       wname = cnum//'.dat'

       OPEN(lunin,file=wname,status='old',iostat=ierr)
   
       IF (ierr /= 0) THEN
          WRITE(6,*)'Could not open ',TRIM(wname)
          CYCLE STATION_LOOP
       ENDIF
   
       IF ( print_read > 0 ) WRITE(6,*)'OPEN:',TRIM(wname)

       jstart = 1
       READ_FILE : DO
   
          ! date is YYYYMMDDHH LLL
          READ(lunin,*,iostat=ierr)date,fctime,val

          IF (ierr /= 0) EXIT READ_FILE

          !
          ! Check if this forecast length is requested
          !

          fc_ind = -1
          DO j=1,nfclengths
             IF ( fctime == fclen(j) ) fc_ind = j
          ENDDO

          IF ( fc_ind == -1 ) CYCLE READ_FILE

          !
          ! Find correct date
          !

          k = -1
          DO j=jstart,hir(i)%ntim
             IF ( hir(i)%o(j)%date <  date/100 ) CYCLE
             IF ( hir(i)%o(j)%date >  date/100 ) EXIT
             IF ( hir(i)%o(j)%date == date/100 .AND.    &
                  hir(i)%o(j)%time == MOD(date,100) ) THEN 
                     k = j
                     EXIT
             ENDIF
          ENDDO

          IF ( k == -1 ) CYCLE READ_FILE

          jstart = k

          !
          ! Put data into array
          !

!         IF ( ff_ind /= 0 ) hir(i)%o(k)%nal(1,fc_ind,ff_ind) = SQRT(val(1)**2+val(2)**2)
!         IF ( ff_ind /= 0 ) hir(i)%o(k)%nal(2,fc_ind,ff_ind) = SQRT(val(3)**2+val(4)**2)
!         IF ( ff_ind /= 0 ) hir(i)%o(k)%nal(3,fc_ind,ff_ind) = SQRT(val(5)**2+val(6)**2)
!         !IF ( ff_ind /= 0 ) hir(i)%o(k)%nal(4,fc_ind,ff_ind) = SQRT(val(7)**2+val(8)**2)
!         IF ( ff_ind /= 0 ) hir(i)%o(k)%nal(4,fc_ind,ff_ind) = SQRT(val(9)**2+val(10)**2)
   
       ENDDO READ_FILE
   
       CLOSE(lunin)

 ENDDO STATION_LOOP

 !
 ! Deactivate stations without data
 !

 DO i=1,maxstn
    hir(i)%active = ( hir(i)%ntim > 0 )
 ENDDO

 RETURN

END SUBROUTINE read_any
