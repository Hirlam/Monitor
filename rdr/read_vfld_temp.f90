SUBROUTINE read_vfld_temp

 !
 ! Read TEMP part of vfldEXPyyyymmddhhll
 ! and organize in data array
 !
 ! Ulf Andrae, SMHI, 2004
 !

 USE data
 USE functions
 USE timing
 USE constants

 IMPLICIT NONE


 INTEGER :: i,ii,j,k,kk,kkk,l,ll,       &
            kk_lev,                     &
            ierr = 0,aerr=0,            &
            cdate = 999999,		&
            ctime = 999999,		&
            wdate = 999999,		&
            wtime = 999999,		&
            istnr = 0,			&
            stat_i,			&
            num_temp,num_stat,	        &
            num_temp_lev,my_temp_lev,   &
            stations(100000),	        &
            max_found_stat,		&
            timing_id,wrk(mparver),     &
            version_flag
 
 REAL :: lat,lon,hgt,val(8)

 LOGICAL :: allocated_this_time(maxstn),	&
            found_any_time,use_stnlist,lfound

 CHARACTER(LEN=200) :: fname = ' '
 CHARACTER(LEN= 10) :: cwrk  ='yyyymmddhh'
 CHARACTER(LEN= 02) :: cfclen  ='  '

!----------------------------------------------------------

 999 format(1x,i5.5,2F8.3,f5.0,f5.0,f6.1,f6.1,f6.1,f7.1,f6.1,en13.3e2)

 stations       = 0
 max_found_stat = 0
 use_stnlist    = ( MAXVAL(stnlist) > 0 ) 
 hgt            = err_ind

 CALL allocate_mod

 ! Copy time

 cdate = sdate
 ctime = stime*10000

 wrk = 0
 WHERE( lev_lst > 0 ) wrk = 1
 my_temp_lev = SUM(wrk)

 IF (lprint_read) WRITE(6,*)'MY_TEMP_LEV',my_temp_lev

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

    !
    ! Check that all files are available
    ! If not, skip this forecast length
    !

    WRITE(cwrk(1:10),'(I8,I2.2)')cdate,ctime/10000
    WRITE(cfclen(1:2),'(I2.2)')fclen(j)
    SUB_EXP_LOOP : DO ll=1,nexp
       fname = TRIM(modpath(ll))//'vfld'//TRIM(expname(ll))//cwrk//cfclen
       INQUIRE(FILE=fname,EXIST=lfound)
       IF ( .NOT. lfound ) THEN
          IF (print_read > 0 ) &
          WRITE(6,'(3A)')'No model data found for ',TRIM(cwrk),TRIM(cfclen)
          CYCLE LL_LOOP 
       ENDIF
    ENDDO SUB_EXP_LOOP

    EXP_LOOP : DO l=1,nexp

       fname = TRIM(modpath(l))//'vfld'//TRIM(expname(l))//cwrk//cfclen

       OPEN(lunin,file=fname,status='old',iostat=ierr)
       IF (ierr.NE.0 .AND. print_read > 0 ) WRITE(6,*)'COULD NOT READ ',fname
       IF (ierr.NE.0) CYCLE EXP_LOOP
       IF (print_read > 0 ) WRITE(6,*)'READ ',TRIM(fname)

       version_flag = 0

       READ(lunin,'(1x,3I6)',IOSTAT=ierr)num_stat,num_temp,version_flag
       IF ( ierr /= 0 ) THEN
         WRITE(6,*)'Error reading first line of vfld file'
         CALL abort
       ENDIF
       READ(lunin,*)num_temp_lev

       DO k=1,num_stat
          READ(lunin,*)
       ENDDO

       READ_STATION_MOD : DO k=1,num_temp

          SELECT CASE (version_flag)
          CASE(0)
             READ(lunin,*,iostat=ierr)istnr,lat,lon
          CASE(1:2)
             READ(lunin,*,iostat=ierr)istnr,lat,lon,hgt
          CASE DEFAULT
          END SELECT 

          IF (ierr.ne.0) CYCLE READ_STATION_MOD

          !
          ! Find station index
          ! Search the first experiment only
          !

          IF ( l == 1 ) THEN
            IF(stations(istnr) == 0) THEN

             stat_i = 0
             IF ( use_stnlist ) THEN
                DO ii=1,maxstn
                   IF (istnr == stnlist(ii) ) stat_i = ii
                ENDDO
                IF ( stat_i == 0 ) THEN
                   DO kk=1,num_temp_lev
                      READ(lunin,*)
                   ENDDO
                   CYCLE READ_STATION_MOD
                ENDIF
             ENDIF

             IF (stat_i == 0 ) THEN
                max_found_stat  = max_found_stat + 1
                stnlist(max_found_stat) = istnr
                IF (print_read > 1 ) WRITE(6,*)'Added station ',istnr
             ELSE
                max_found_stat  = stat_i
             ENDIF

             IF (max_found_stat > maxstn) THEN
                WRITE(6,*)'Increase maxstn',max_found_stat
                CALL abort
             ENDIF

             stations(istnr) = max_found_stat
             hir(max_found_stat)%active = .TRUE.
             hir(max_found_stat)%stnr   = istnr
             hir(max_found_stat)%lat    = lat
             hir(max_found_stat)%lon    = lon
             hir(max_found_stat)%hgt    = hgt


           ENDIF

          ELSE

           IF(stations(istnr) == 0) THEN
             DO kk=1,num_temp_lev
               READ(lunin,*)
             ENDDO
             CYCLE READ_STATION_MOD
           ENDIF

          ENDIF

          stat_i = stations(istnr)

          !
          ! Station found! Allocate data array if 
          ! this is a new time
          !
         
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

             IF (print_read > 1 ) WRITE(6,*)'ALLOCATED',istnr,stat_i,cdate,ctime/10000
  
          ENDIF

          i = hir(stat_i)%ntim  

          READ_LEV_MOD : DO kk=1,num_temp_lev

          val = -99.

          SELECT CASE(version_flag)
          CASE(0)
             READ(lunin,*,iostat=ierr)val(1:7)
          CASE(1:2)
             READ(lunin,*,iostat=ierr)val(1:8)
          END SELECT 

          IF (ierr /= 0 ) CYCLE READ_LEV_MOD

          kk_lev = 0

          DO kkk=1,my_temp_lev
             IF (ABS(val(1) - lev_lst(kkk)) < 1.e-6) kk_lev = kkk
          ENDDO

          IF (kk_lev == 0 ) CYCLE READ_LEV_MOD

          IF (lprint_read) WRITE(6,*)'KK_LEV',kk_lev,val(1),lev_lst(kk_lev)

          ! Do not use levels below model topography
          IF ( (ABS(hgt    - err_ind ) > 1.e-6) .AND. &
               (ABS(val(2) + 99.     ) > 1.e-6) .AND. &
                    val(2) < hgt                     ) CYCLE READ_LEV_MOD

          IF((fi_ind /= 0 ) .AND. qca(val(2),-99.)) &
          hir(stat_i)%o(i)%nal(l,j,kk_lev+ my_temp_lev*(fi_ind-1)) = val(2)
          IF((tt_ind /= 0 ) .AND. qca(val(3),-99.)) &
          hir(stat_i)%o(i)%nal(l,j,kk_lev+ my_temp_lev*(tt_ind-1)) = val(3) - tzero
          IF((rh_ind /= 0 ) .AND. qca(val(4),-99.)) &
          hir(stat_i)%o(i)%nal(l,j,kk_lev+ my_temp_lev*(rh_ind-1)) = val(4)
          IF((dd_ind /= 0 ) .AND. qca(val(5),-99.)) &
          hir(stat_i)%o(i)%nal(l,j,kk_lev+ my_temp_lev*(dd_ind-1)) = val(5)
          IF((ff_ind /= 0 ) .AND. qca(val(6),-99.)) &
          hir(stat_i)%o(i)%nal(l,j,kk_lev+ my_temp_lev*(ff_ind-1)) = val(6)
          IF((qq_ind /= 0 ) .AND. qca(val(7),-99.)) &
          hir(stat_i)%o(i)%nal(l,j,kk_lev+ my_temp_lev*(qq_ind-1)) = val(7) * 1.e3
          IF((td_ind /= 0 ) .AND. qca(val(8),-99.)) &
          hir(stat_i)%o(i)%nal(l,j,kk_lev+ my_temp_lev*(td_ind-1)) = val(8)


       ENDDO READ_LEV_MOD

       ENDDO READ_STATION_MOD

       CLOSE(lunin)

    ENDDO EXP_LOOP
    ENDDO LL_LOOP

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,fcint*3600,cdate,ctime)
    IF(cdate > edate) EXIT TIME_LOOP
    IF(cdate == edate .AND. ctime/10000 > etime) EXIT TIME_LOOP

 ENDDO TIME_LOOP
 
 if (fi_ind /= 0 ) &
 lev_typ((fi_ind-1)* my_temp_lev + 1:fi_ind* my_temp_lev) = fi_ind
 if (tt_ind /= 0 ) &
 lev_typ((tt_ind-1)* my_temp_lev + 1:tt_ind* my_temp_lev) = tt_ind
 if (rh_ind /= 0 ) &
 lev_typ((rh_ind-1)* my_temp_lev + 1:rh_ind* my_temp_lev) = rh_ind
 if (dd_ind /= 0 ) &
 lev_typ((dd_ind-1)* my_temp_lev + 1:dd_ind* my_temp_lev) = dd_ind
 if (ff_ind /= 0 ) &
 lev_typ((ff_ind-1)* my_temp_lev + 1:ff_ind* my_temp_lev) = ff_ind
 if (qq_ind /= 0 ) &
 lev_typ((qq_ind-1)* my_temp_lev + 1:qq_ind* my_temp_lev) = qq_ind
 if (td_ind /= 0 ) &
 lev_typ((td_ind-1)* my_temp_lev + 1:td_ind* my_temp_lev) = td_ind

 WRITE(6,*) 'FOUND TIMES MODEL',MAXVAL(hir(:)%ntim)

 DO i=1,maxstn
    hir(i)%active = ( hir(i)%ntim > 0 )
 ENDDO

 RETURN

END
