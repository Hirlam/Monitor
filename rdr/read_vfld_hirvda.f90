SUBROUTINE read_vfld_hirvda

 !
 ! Read synop part of vfldEXPyyyymmddhhll 
 ! and organize in data array 
 ! for verification and plotting
 ! 
 ! Ulf Andrae, SMHI, 2004
 !

 USE data
 USE functions
 USE timing
 USE constants

 IMPLICIT NONE

 INTEGER :: i,ii,j,k,l,			&
            ierr = 0,			&
            cdate = 999999,		&
            ctime = 999999,		&
            wdate = 999999,		&
            wtime = 999999,		&
            istnr = 0,			&
            stat_i,				&
            num_temp,num_stat,	&
            num_temp_lev,		&
            stations(100000),	&
            max_found_stat,		&
            timing_id,aerr
 
 
 REAL :: lat,lon,val(7),qq

 LOGICAL :: allocated_this_time(maxstn),	&
            found_any_time,use_stnlist

 CHARACTER(LEN=50) :: oname ='../vfld',fname = ' '
 CHARACTER(LEN=8) :: cwrk  ='yymmddhh'
 CHARACTER(LEN=02) :: cfclen  ='  '
 INTEGER,PARAMETER :: pl = 3 ! Path length of filename

!----------------------------------------------------------

 999 format(1x,i5.5,2F8.3,f5.0,f5.0,f6.1,f6.1,f6.1,f7.1,f6.1,en13.3e2)
 998 format(1x,i5.5,2F8.3,f5.0,f5.0,f6.1,f6.1,f6.1,f7.1,f6.1)

 stations       = 0
 max_found_stat = 0
 use_stnlist = ( MAXVAL(stnlist) > 0 ) 

 !
 ! If model array is not allocated
 ! do so and init arrays
 !

 IF (.NOT.hir(1)%obs_is_allocated) THEN

    timing_id = 0
    IF (ltiming) CALL add_timing(timing_id,'allocate_read_vfld')

    ! Estimate maxtim if not given by user
    IF (maxtim.EQ.0) maxtim=get_maxtim(sdate,edate,1)
    IF(print_read > 1) WRITE(6,*)'MAXTIM', maxtim

    ! Init model array
    DO k = 1,maxstn
       ALLOCATE(hir(k)%o(maxtim))
    ENDDO
  
    hir%ntim       = 0
    hir%nexp       = nexp
    hir%nparver    = nparver
    hir%nfclengths = nfclengths
    hir%active     = .FALSE.
    hir%stnr       = 0

    hir%obs_is_allocated = .TRUE.

    IF (ltiming) CALL add_timing(timing_id,'allocate_read_vfld')

 ENDIF

 ! Copy time

 cdate = sdate
 ctime = stime

 !
 ! Loop over all times
 !

 i = 0

 TIME_LOOP : DO

 allocated_this_time = .FALSE.
 found_any_time      = .FALSE.

 !
 ! Read model data
 !

    LL_LOOP  : DO j=1,nfclengths
    EXP_LOOP : DO l=1,nexp

       WRITE(cwrk(1:8),'(I6.6,I2.2)')MOD(cdate,1000000),ctime/10000
       WRITE(cfclen(1:2),'(I2.2)')fclen(j)
       fname = TRIM(oname)//TRIM(expname(l))//cwrk//cfclen

       OPEN(lunin,file=fname,status='old',iostat=ierr)
       IF (ierr.NE.0) THEN
               IF (print_read > 0 ) WRITE(6,*)'Could not open ',fname
           CYCLE EXP_LOOP
       ENDIF
       IF (print_read > 0 ) WRITE(6,*)'READ ',fname

       READ(lunin,*)num_stat,num_temp
       READ(lunin,*)num_temp_lev

       READ_STATION_MOD : DO k=1,num_stat

          val = -99.
          READ(lunin,998,iostat=ierr)istnr,lat,lon,val
          IF (ierr.ne.0) CYCLE READ_STATION_MOD

          !
          ! Find station index
          !

          IF(stations(istnr) == 0) THEN

             stat_i = 0
             IF ( use_stnlist ) THEN
                DO ii=1,maxstn
                   IF (istnr == stnlist(ii) ) stat_i = ii
                ENDDO
                IF ( stat_i == 0 ) CYCLE READ_STATION_MOD
             ENDIF

             IF (stat_i == 0 ) THEN
                max_found_stat  = max_found_stat + 1
                stnlist(max_found_stat) = istnr
             ELSE
                max_found_stat  = stat_i
             ENDIF

             stations(istnr) = max_found_stat
             hir(max_found_stat)%active = .TRUE.
             hir(max_found_stat)%stnr   = istnr
             hir(max_found_stat)%lat    = lat 
             hir(max_found_stat)%lon    = lon 

             IF (max_found_stat > maxstn) THEN
                WRITE(6,*)'Increase maxstn',max_found_stat
                CALL abort
             ENDIF

          IF (print_read > 1 ) WRITE(6,*)'ADDED',istnr,stations(istnr)

          ENDIF

          stat_i = stations(istnr)


          WHERE(abs(val+99.).lt.1.e-6)
             val = err_ind
          END WHERE

          !
          ! Station found! Allocate data array if 
          ! this is a new time
          !
         
          IF (print_read > 1 .AND. hir(stat_i)%ntim > 0 ) WRITE(6,*)'BOUND 1',istnr,UBOUND( hir(stat_i)%o(hir(stat_i)%ntim)%nal )
          IF (print_read > 1 .AND. hir(stat_i)%ntim > 0 ) WRITE(6,*)'TEST',istnr,allocated_this_time(stat_i)
          IF ( .NOT. allocated_this_time(stat_i) ) THEN 

             hir(stat_i)%ntim =  hir(stat_i)%ntim + 1
             i = hir(stat_i)%ntim  

             ALLOCATE(hir(stat_i)%o(i)%date)
             ALLOCATE(hir(stat_i)%o(i)%time)
             ALLOCATE(hir(stat_i)%o(i)%nal(nexp,nfclengths,nparver),stat=aerr)


             IF ( aerr /= 0 ) THEN
                WRITE(6,*)'ERROR IN ALLOCATE',aerr
                CALL ABORT
             ENDIF
   
             hir(stat_i)%o(i)%date = cdate
             hir(stat_i)%o(i)%time = ctime/10000
             hir(stat_i)%o(i)%nal  = err_ind
 
             allocated_this_time(stat_i) = .TRUE.
             found_any_time = .TRUE.

          IF (print_read > 1 ) WRITE(6,*)'ALLOCATED',istnr,stat_i,cdate,ctime/10000
  
          ENDIF

          i = hir(stat_i)%ntim  

          !
          ! Add data
          !
          IF (print_read > 1 ) WRITE(6,*)'ADD',istnr,stat_i,cdate,ctime/10000,fclen(j)
          IF (print_read > 1 ) WRITE(6,*)'ADD',istnr,val
          IF (print_read > 1 ) WRITE(6,*)'BOUND',istnr,UBOUND( hir(stat_i)%o(i)%nal )

          if (nn_ind /= 0 ) hir(stat_i)%o(i)%nal(l,j,nn_ind) = val(1)
          if (dd_ind /= 0 ) hir(stat_i)%o(i)%nal(l,j,dd_ind) = val(2)
          if (ff_ind /= 0 ) hir(stat_i)%o(i)%nal(l,j,ff_ind) = val(3)
          if (tt_ind /= 0 ) hir(stat_i)%o(i)%nal(l,j,tt_ind) = val(4) - tzero
          if (rh_ind /= 0 ) hir(stat_i)%o(i)%nal(l,j,rh_ind) = val(5)
          if (ps_ind /= 0 ) hir(stat_i)%o(i)%nal(l,j,ps_ind) = val(6)
          if (pe_ind /= 0 ) hir(stat_i)%o(i)%nal(l,j,pe_ind) = val(7)
!         if (qq_ind /= 0 ) hir(stat_i)%o(i)%nal(l,j,qq_ind) = val(8) * 1.e3

       ENDDO READ_STATION_MOD

       CLOSE(lunin)

    ENDDO EXP_LOOP
    ENDDO LL_LOOP

    !
    ! Step time
    !

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,fcint*3600,cdate,ctime)
    IF(cdate > edate) EXIT TIME_LOOP
    IF(cdate >= edate .AND. ctime/10000 > etime) EXIT TIME_LOOP

 ENDDO TIME_LOOP

 DO i=1,maxstn
    hir(i)%active = ( hir(i)%ntim > 0 )
 ENDDO

 IF (print_read > 0 ) WRITE(6,*) 'FOUND TIMES MODEL',MAXVAL(hir(:)%ntim)

 RETURN

END SUBROUTINE read_vfld_hirvda
