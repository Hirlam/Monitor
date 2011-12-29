PROGRAM verobs

 !
 ! verobs top control subroutine
 !
 ! Ulf Andrae, SMHI, 2002
 !

 ! Modules

 USE data
 USE timing

 IMPLICIT NONE

 ! Local

 INTEGER :: i,j,ierr,maxstn_save,    &
           timing_id1,timing_id2
 !--------------------

 WRITE(6,*)
 WRITE(6,*)'This is verobs running '
 WRITE(6,*)

 !
 ! Read namelist
 !

 OPEN(10,FILE='fort.10',STATUS='OLD',IOSTAT=ierr)
 IF ( ierr /= 0 ) THEN
    WRITE(6,*)'Could not open namelist file fort.10'
    CALL abort
 ENDIF

 READ(10,namver,IOSTAT=ierr)
 IF ( ierr /=0 ) THEN
   WRITE(6,namver)
   WRITE(6,*)
   WRITE(6,*)'Error reading namelist file fort.10',ierr
   CALL abort
 ENDIF

 !
 ! Cross check options in namelist
 !
 CALL setup_varprop

 nrun = 1
 CALL check_namelist

 timing_id1 = 0
 IF (ltiming) CALL add_timing(timing_id1,'verobs')

 !
 ! Allocate and init main arrays
 !

 maxstn_save = maxstn
 ALLOCATE(obs(maxstn))
 ALLOCATE(hir(maxstn))

 DO i=1,maxstn
    obs(i)%obs_is_allocated = .false.
    hir(i)%obs_is_allocated = .false.
 ENDDO

 timing_id2 = 0
 IF (ltiming) CALL add_timing(timing_id2,'reading')


 !
 ! Call user specific subroutine
 !
 
 CALL my_choices

 CALL read_station_name

 IF (ltiming) CALL add_timing(timing_id2,'reading')

 CALL station_summary

 IF (active_stations == 0 ) THEN
    WRITE(6,*)'No active stations'
    WRITE(6,*)'Exit verobs'
    WRITE(6,*)
    STOP
 ENDIF

 NAMELIST : DO
 
    !
    ! Loop over all namelists found
    !

    WRITE(6,*)'THIS nrun is nr:', nrun 

    IF( release_memory.AND.nrun > 1) THEN
       WRITE(6,*)'Sorry your data is gone'
       STOP
    ENDIF

    IF( nrun > 1 ) maxstn = maxstn_save 

    CALL selection(1)

    IF ( estimate_qc_limit ) CALL quality_control
    IF ( lquality_control  ) CALL quality_control
    IF ( lverify           ) CALL verify

    !
    ! Re-init namelist and read again
    !

    CALL ini_namelist

    READ(10,namver,iostat=ierr)
#ifdef PATHSCALE
    IF (ierr == -4001) EXIT
#else
    IF (ierr == -1) EXIT
#endif
    IF (ierr /=  0) THEN
       WRITE(6,*)'Could not read namver correctly, read nr,ierr:',nrun+1,ierr
       CALL abort
    ENDIF

    maxstn_save = maxstn
    nrun        = nrun + 1
    CALL check_namelist

 ENDDO NAMELIST

 !
 ! Deallocate main arrays
 !

 DO i=1,maxstn

    IF ( obs(i)%obs_is_allocated ) THEN
       DO j=1,obs(i)%ntim
         IF(ASSOCIATED(obs(i)%o(j)%date)) DEALLOCATE(obs(i)%o(j)%date)
         IF(ASSOCIATED(obs(i)%o(j)%time)) DEALLOCATE(obs(i)%o(j)%time)
         IF(ASSOCIATED(obs(i)%o(j)%val )) DEALLOCATE(obs(i)%o(j)%val )
       ENDDO

       DO j=obs(i)%ntim+1,SIZE(obs(i)%o)
         NULLIFY(obs(i)%o(j)%date)
         NULLIFY(obs(i)%o(j)%time)
         NULLIFY(obs(i)%o(j)%val )
       ENDDO

       DEALLOCATE(obs(i)%o)
       obs(i)%obs_is_allocated = .FALSE.

    ENDIF

    IF ( hir(i)%obs_is_allocated ) THEN

       DO j=1,hir(i)%ntim
          DEALLOCATE(hir(i)%o(j)%date)
          DEALLOCATE(hir(i)%o(j)%time)
          DEALLOCATE(hir(i)%o(j)%nal )
       ENDDO

       DO j=1,hir(i)%ntim+1,SIZE(hir(i)%o)
          NULLIFY(hir(i)%o(j)%date)
          NULLIFY(hir(i)%o(j)%time)
          NULLIFY(hir(i)%o(j)%nal )
       ENDDO

       DEALLOCATE(hir(i)%o)
       hir(i)%obs_is_allocated = .FALSE.

    ENDIF

 ENDDO

 DEALLOCATE(obs)
 DEALLOCATE(hir)
 DEALLOCATE(varprop)

 IF ( ALLOCATED(station_name) ) DEALLOCATE(station_name)

 IF (ltiming) CALL add_timing(timing_id1,'verobs')
 IF (ltiming) CALL view_timing
 
END PROGRAM verobs
