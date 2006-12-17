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

 INTEGER :: i,ierr,maxstn_save,    &
           timing_id1,timing_id2
 !--------------------

 WRITE(6,*)
 WRITE(6,*)' This is verobs running '
 WRITE(6,*)

 !
 ! Read namelist
 !

 READ(10,namver,iostat=ierr)

 IF (ierr.NE.0) THEN
   WRITE(6,namver)
   STOP
 ENDIF

 !
 ! Cross check options in namelist
 !

 CALL check_namelist

 timing_id1 = 0
 IF (ltiming) CALL add_timing(timing_id1,'verobs')

 !
 ! Allocate and init main arrays
 !

 ALLOCATE(obs(maxstn))
 ALLOCATE(hir(maxstn))

 DO i=1,maxstn
    obs(i)%obs_is_allocated = .false.
    hir(i)%obs_is_allocated = .false.
 ENDDO

 timing_id2 = 0
 IF (ltiming) CALL add_timing(timing_id2,'reading')

 CALL set_obstype

 !
 ! Call user specific subroutine
 !
 
 CALL my_choices

 !
 ! Estimate maximum size of arrays used
 !

 ntimmax = MAXVAL(hir%ntim)
 !ntimmax_findplot = ntimmax * 24 / fcint * nfclengths

 ntimmax = MAX(ntimmax,MAXVAL(obs%ntim))
 !ntimmax_findplot = MAX(ntimmax,ntimmax_findplot)

 WRITE(6,*)
 WRITE(6,*)'MAXTIM (used,suggested) : ',ntimmax,maxtim
 WRITE(6,*)

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

    nrun = nrun + 1

    WRITE(6,*)'THIS nrun is nr:', nrun 

    IF( release_memory.AND.nrun > 1) THEN
       WRITE(6,*)'Sorry your data is gone'
       STOP
    ENDIF

    IF(nrun.GT.1) maxstn = maxstn_save 

    IF (lverify) THEN

       !
       ! Do a station selection and call the main verificaion
       !

       CALL selection(1)
       IF ( use_pos ) THEN
!         CALL verify_pos
       ELSE
          CALL verify
       ENDIF

    ENDIF

!   IF (lfindplot) THEN
!      CALL selection(2)
!      lprint_summary = .FALSE.
!      CALL station_summary
!      CALL findplot
!   ENDIF

    !
    ! Read namelist again
    !

    READ(10,namver,iostat=ierr)

    IF (ierr.NE.0) EXIT

    maxstn_save = maxstn

 ENDDO NAMELIST

 !
 ! Deallocate main arrays
 !

 DO i=1,maxstn
    obs(i)%obs_is_allocated = .false.
    hir(i)%obs_is_allocated = .false.
 ENDDO

 DEALLOCATE(obs)
 DEALLOCATE(hir)

 IF (ltiming) CALL add_timing(timing_id1,'verobs')
 IF (ltiming) CALL view_timing
 
END PROGRAM verobs
