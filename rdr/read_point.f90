SUBROUTINE read_point
 
 USE DATA
 USE CONSTANTS
 USE FUNCTIONS

 IMPLICIT NONE

 INTEGER :: date,i,j,k,l,nl,fl,o,ierr, &
            ffclengths,ll(50),          &
            fclen_ind(0:50),fli,       &
            fpar,ind,                   &
            cdate,ctime,                &
            wdate,wtime

 
 REAL :: t,u,v,q,ps,swn,lwn,lwd,swd

 CHARACTER(LEN=80) :: mname ='YYYYMMDD_HH/XXX_point_00000.dat'
 CHARACTER(LEN=80) :: wname =' '

 LOGICAL :: do_this_time = .true.

 !------------------------------------------

 !
 ! If model array is not allocated
 ! do so and init arrays
 !

 IF (.NOT.hir(1)%obs_is_allocated) THEN

    ! Estimate maxtim if not given by user
    IF (maxtim.EQ.0) maxtim=get_maxtim(sdate,edate,1)
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

    hir%obs_is_allocated = .TRUE.

 ENDIF

 !
 ! Loop over all stations
 !

 STATION_LOOP : DO i=1,maxstn

 !
 ! Loop over all times
 !

 cdate = sdate
 ctime = stime

 TIME_LOOP : DO

    do_this_time = .true.

    wname = mname
    ind = INDEX(mname,'YYYY')
    WRITE(wname(ind:ind+7),'(I8.8)')cdate
    ind = INDEX(mname,'HH')
    WRITE(wname(ind:ind+1),'(I2.2)')ctime/10000
    ind = INDEX(mname,'00000')
    WRITE(wname(ind:ind+4),'(I5.5)')hir(i)%stnr

    !
    ! Loop over all experiments
    !

    EXP_LOOP : DO o=1,nexp

       ind = INDEX(mname,'XXX')
       WRITE(wname(ind:ind+2),'(A3)')expname(o)
   
       OPEN(lunin,file=wname,status='old',iostat=ierr)
   
       IF (ierr.NE.0) THEN
          WRITE(6,*)'Could not open ',wname
          CYCLE EXP_LOOP
       ENDIF
   
       WRITE(6,*)'OPEN:',wname
   
       !
       ! Read parameter and fclength information from file
       !
   
       READ(lunin,*,iostat=ierr)fpar,ffclengths
   
       IF ( ffclengths /= nfclengths ) THEN
          WRITE(6,*)'WARNING: ffclengths and nfclengths differ', &
          ffclengths,nfclengths
       ENDIF
   
       READ(lunin,*,iostat=ierr)
       READ(lunin,*,iostat=ierr)ll(1:ffclengths)
       fclen_ind = 0
   
       !
       ! Find which fclengths are available in file
       !
   
       DO nl=1,nfclengths
       DO fl=1,ffclengths
          IF (ll(fl) == fclen(nl) ) fclen_ind(fl) = nl
       ENDDO
       ENDDO
   
       READ_FILE : DO
   
          !
          ! Loop over ffclengths in file
          !

          FC_CYCLE : DO nl=1,ffclengths
   
          READ(lunin,*,iostat=ierr)date,fli,t,u,v,q,ps,swn,lwn,lwd,swd
   
          IF (ierr.ne.0) EXIT READ_FILE
          IF (fclen_ind(fli) == 0) CYCLE FC_CYCLE
   
          IF (nl == 1) THEN
   
             !
             ! Allocate new time
             !

             IF (do_this_time) THEN
   
                k = hir(i)%ntim + 1
      
                ALLOCATE(hir(i)%o(k)%date)
                ALLOCATE(hir(i)%o(k)%time)
                ALLOCATE(hir(i)%o(k)%nal(nexp,nfclengths,nparver))
   
                hir(i)%ntim      = k
                hir(i)%o(k)%date = date/100
                hir(i)%o(k)%time = mod(date,100)
                hir(i)%o(k)%nal  = err_ind

                do_this_time = .false.
                IF (lprint_read) WRITE(6,*)'Allocated new date at',k, &
                hir(i)%o(k)%date,hir(i)%o(k)%time

             ENDIF
   
          ENDIF
   
          !
          ! Put data into array
          !
               
          l = fclen_ind(fli)
   
          IF ( tt_ind /= 0 ) hir(i)%o(k)%nal(o,l,tt_ind) = t - tzero
          IF ( dd_ind /= 0 ) hir(i)%o(k)%nal(o,l,dd_ind) = atan2(u,v)*180./pi + 180.
          IF ( ff_ind /= 0 ) hir(i)%o(k)%nal(o,l,ff_ind) = sqrt(u**2 + v**2)
          IF ( gr_ind /= 0 ) hir(i)%o(k)%nal(o,l,gr_ind) = swd
          IF ( lw_ind /= 0 ) hir(i)%o(k)%nal(o,l,lw_ind) = lwd
   
          ENDDO FC_CYCLE
   
       ENDDO READ_FILE
   
       CLOSE(lunin)
   
       !
       ! Step time
       !

    ENDDO EXP_LOOP

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,fcint*3600,cdate,ctime)
    IF (cdate > edate ) EXIT TIME_LOOP
   

 ENDDO TIME_LOOP
 ENDDO STATION_LOOP

 !
 ! Deactivate stations without data
 !

 DO i=1,maxstn
    hir(i)%active = ( hir(i)%ntim > 0 )
 ENDDO

 RETURN

END SUBROUTINE read_point
