SUBROUTINE read_soda

 !
 ! Read data from Sodankylä from Markku Kangas
 ! and organize for evalutaion
 !
 ! Ulf Andrae, SMHI, 2007
 !
 
 USE data

 IMPLICIT NONE
 
 INTEGER :: i,ierr,stat_i,           &
            date1,time1,             &
            date2,time2
         
 REAL    :: hflux,hflux1,hflux2,     &
            lflux,lflux1,lflux2
         
 CHARACTER(LEN= 12) :: ctstamp
 CHARACTER(LEN= 20) :: fname1='Fluxes_71.txt'
 CHARACTER(LEN=100) :: fname=''

 !---------------------------------------------------------------------

 IF(print_read > 0 ) WRITE(6,*)'READ SODANKYLÄ'

 !
 ! Allocate obs arrays if it is not already done
 !

 CALL allocate_obs
 IF(print_read > 0 ) WRITE(6,*)'OBS is ALLOCATED'

 i       = 0
 stat_i  = 1
 obs(stat_i)%stnr = 1

 ! 
 ! Loop over all times
 ! 
 fname = TRIM(obspath)//TRIM(fname1)
 OPEN(lunin,FILE=fname,STATUS='old',IOSTAT=ierr)
 IF ( ierr /= 0 ) THEN
    WRITE(6,*)'Could not open ',TRIM(fname)
    CALL ABORT
 ENDIF

 IF ( print_read > 0 ) WRITE(6,*)'Open ',TRIM(fname)

 READ(lunin,*)

 ! 
 ! Create filenames, open and exit/cycle or fail if first or second
 ! file not found. Read first dummy lines
 !    

 READ_LOOP : DO
    
    ! Read 00 and 30 of each hour and take average

    READ(lunin,*,IOSTAT=ierr)ctstamp,hflux1,lflux1
    IF ( ierr /= 0 ) EXIT READ_LOOP
    READ(ctstamp,'(I8.8,I4.4)')date1,time1


    READ(lunin,*,IOSTAT=ierr)ctstamp,hflux2,lflux2
    READ(ctstamp,'(I8.8,I4.4)')date2,time2

    IF ( hflux1 < -99990 .OR. lflux1 < -99990 ) CYCLE READ_LOOP
    IF ( hflux2 < -99990 .OR. lflux2 < -99990 ) CYCLE READ_LOOP

    IF ( date1 /= date2 .AND. time1 /= 2330 ) THEN
       WRITE(6,*)'Error reading ',date1,date2,time1,time2
       CYCLE
    ENDIF

    IF ( date2 > edate )THEN
       EXIT READ_LOOP
    ENDIF
    IF ( date1 < sdate )THEN
       CYCLE READ_LOOP
       WRITE(6,*)'Skip ',date1,date2,time1,time2
    ENDIF

    hflux = 0.5 * ( hflux1 + hflux2 ) 
    lflux = 0.5 * ( lflux1 + lflux2 ) 

    i = i + 1
   
    !
    ! Store data
    ! Fluxes are taken as mean over hour, inst. values as the latest instant
    ! NB !. Ground heat flux are calculated as residual
    !

    ALLOCATE(obs(stat_i)%o(i)%date)
    ALLOCATE(obs(stat_i)%o(i)%time)
    ALLOCATE(obs(stat_i)%o(i)%val(nparver))

    obs(stat_i)%o(i)%date  = date1
    obs(stat_i)%o(i)%time  = time2/100
    obs(stat_i)%o(i)%val   = err_ind

    IF (wq_ind /= 0) obs(stat_i)%o(i)%val(wq_ind)  = lflux
    IF (wt_ind /= 0) obs(stat_i)%o(i)%val(wt_ind)  = hflux

    WRITE(6,*)'ADDED ',i,obs(stat_i)%o(i)%date,obs(stat_i)%o(i)%time

 ENDDO READ_LOOP

 CLOSE(lunin)

 obs(stat_i)%ntim = i
 obs(stat_i)%active = .TRUE.

END SUBROUTINE read_soda
