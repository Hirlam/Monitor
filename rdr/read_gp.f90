SUBROUTINE read_gp

 !
 ! Read data extracted with gl
 ! This case only covers precipitation but it can be easily ( well....)
 ! modified to cover any variable
 !
 ! Ulf Andrae, SMHI, Dec 2005
 !
 
 USE DATA
 USE CONSTANTS
 USE FUNCTIONS

 IMPLICIT NONE

 INTEGER :: i,ii,j,k,l,o,ierr, &
            fpar,ind,       &
            date1,date2,    &
            time1,time2,    &
            jstart,istn,    &
            cdate,wdate,    &
            wtime
 
 REAL :: val1(2),val2(2),lat,lon

 CHARACTER(LEN=99) :: wname =' '
 CHARACTER(LEN=3)  :: cnum ='000'
 CHARACTER(LEN=10) :: cexp =' '

 !------------------------------------------

 !
 ! If model array is not allocated
 ! do so and init arrays
 !

 IF (.NOT.hir(1)%obs_is_allocated) THEN

    ! Estimate maxtim if not given by user
    IF (maxtim == 0) maxtim=get_maxtim(sdate,edate,1)
    IF(lprint_read) WRITE(6,*)'MAXTIM', maxtim

    ! Init model array
    DO k = 1,maxstn
       ALLOCATE(hir(k)%o(maxtim))
    ENDDO
  
    hir%ntim       = 0
    hir%nexp       = nexp
    hir%nparver    = nparver
    hir%nfclengths = nfclengths
    hir%active     = .FALSE.
    hir%stnr       = stnlist
    hir%lat = obs%lat
    hir%lon = obs%lon

    hir%obs_is_allocated = .TRUE.

 ENDIF


 !
 ! Allocate all times for all stationes since we do not
 ! know if all experiments contains all times
 !

 cdate = sdate

 k = 0
 DO 
    k = k+1
    DO i=1,maxstn

      ALLOCATE(hir(i)%o(k)%date)
      ALLOCATE(hir(i)%o(k)%time)
      ALLOCATE(hir(i)%o(k)%nal(nexp,nfclengths,nparver))

      hir(i)%o(k)%date = cdate
      hir(i)%o(k)%time = 00
      hir(i)%o(k)%nal  = err_ind

    ENDDO
    CALL adddtg(cdate,0,24*3600,wdate,wtime)
    IF ( wdate > edate ) EXIT
    cdate = wdate

 ENDDO

 hir%ntim = k

 !
 ! Loop over all stations and all experiments
 !

 STATION_LOOP : DO ii=1,maxstn

    EXP_LOOP : DO o=1,nexp

       ! Create filename
       WRITE(cnum,'(I3.3)')ii
       wname = TRIM(expname(o))//'/'//cnum//'.dat'

       OPEN(lunin,file=wname,status='old',iostat=ierr)
   
       IF (ierr.NE.0) THEN
          !WRITE(6,*)'Could not open ',TRIM(wname)
          CYCLE EXP_LOOP
       ENDIF
   
       !WRITE(6,*)'OPEN:',TRIM(wname)
       READ(lunin,*,iostat=ierr)date1,lat,lon

       !
       ! Find correct station
       !

       istn = -1
       DO i=1,maxstn
         IF ( ABS(hir(i)%lat - lat ) < 1.e-6 .AND.    &
              ABS(hir(i)%lon - lon ) < 1.e-6 ) THEN
           istn = i
         ENDIF
       ENDDO

       IF ( istn == -1 ) THEN
          CLOSE(lunin)
          CYCLE EXP_LOOP
       ENDIF

       i = istn

       !WRITE(6,*)'ADDING TO STATION ',i,hir(i)%stnr

       jstart = 1
       READ_FILE : DO
   
          READ(lunin,*,iostat=ierr)date1,time1,val1
          READ(lunin,*,iostat=ierr)date2,time2,val2

          IF (ierr.ne.0) EXIT READ_FILE

          IF ( date1 /= date2 )  CALL abort

          date1 = date1 / 100

          !
          ! Find correct date
          !

          k = -1
          DO j=jstart,hir(i)%ntim
             !WRITE(6,*)'TRY',j,hir(i)%o(j)%date,date1
             IF ( hir(i)%o(j)%date < date1 ) CYCLE
             IF ( hir(i)%o(j)%date > date1 ) EXIT
             IF ( hir(i)%o(j)%date == date1 ) THEN 
                     k = j
                     EXIT
             ENDIF
          ENDDO

          IF ( k == -1 ) CYCLE READ_FILE

          jstart = k

          !
          ! Put data into array
          !

          !WRITE(6,*)'DATA TO ',i,hir(i)%stnr,hir(i)%o(k)%date,date1
               
          IF ( pd_ind /= 0 ) hir(i)%o(k)%nal(o,1,pd_ind) = SUM(val2) - SUM(val1)
   
       ENDDO READ_FILE
   
       CLOSE(lunin)
   
    ENDDO EXP_LOOP

 ENDDO STATION_LOOP

 !
 ! Deactivate stations without data
 !

 DO i=1,maxstn
    hir(i)%active = ( hir(i)%ntim > 0 )
 ENDDO

 RETURN

END SUBROUTINE read_gp
