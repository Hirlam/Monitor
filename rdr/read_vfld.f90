SUBROUTINE read_vfld

 !
 ! Read synop part of vfldEXPyyyymmddhhll 
 ! and organize in data array 
 ! for verification and plotting
 ! 
 ! Ulf Andrae, SMHI, 2004
 !

 USE data
 USE functions
 USE constants

 IMPLICIT NONE

 INTEGER :: i,ii,j,k,l,ll,              &
            ierr = 0,                   &
            cdate = 999999,             &
            ctime = 999999,             &
            wdate = 999999,             &
            wtime = 999999,             &
            istnr = 0,                  &
            stat_i,                     &
            num_temp,num_stat,          &
            num_temp_lev,               &
            stations(100000),           &
            max_found_stat,             &
            aerr,version_flag
 
 
 REAL :: lat,lon,hgt,val(15)

 LOGICAL :: allocated_this_time(maxstn),&
            found_any_time,use_stnlist,lfound

 CHARACTER(LEN=200) :: fname = ' '
 CHARACTER(LEN=10 ) :: cwrk  ='yyyymmddhh'
 CHARACTER(LEN=02 ) :: cfclen  ='  '

!----------------------------------------------------------

 stations       = 0
 max_found_stat = 0
 use_stnlist = ( MAXVAL(stnlist) > 0 ) 

 hir%hgt = obs%hgt

 CALL allocate_mod

 ! Copy time

 cdate = sdate
 ctime = stime*10000

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
          WRITE(6,'(3A)')'No model data found for ',TRIM(fname)
          CYCLE LL_LOOP 
       ENDIF
    ENDDO SUB_EXP_LOOP

    EXP_LOOP : DO l=1,nexp

       fname = TRIM(modpath(l))//'vfld'//TRIM(expname(l))//cwrk//cfclen
       OPEN(lunin,file=fname,status='old',iostat=ierr)
       IF (ierr.NE.0) THEN
          IF (print_read > 0 ) WRITE(6,'(2A)')'Could not open ',TRIM(fname)
          CYCLE EXP_LOOP
       ENDIF
       IF (print_read > 0 ) WRITE(6,'(2A)')'READ ',TRIM(fname)

       version_flag = 0

       READ(lunin,'(1x,3I6)',IOSTAT=ierr)num_stat,num_temp,version_flag
       IF ( ierr /= 0 ) THEN
         WRITE(6,*)'Error reading first line of vfld file'
         CALL abort
       ENDIF
       READ(lunin,*)num_temp_lev

       READ_STATION_MOD : DO k=1,num_stat

          val = -99.
          SELECT CASE(version_flag)
 
          CASE(0)
             READ(lunin,*,iostat=ierr)istnr,lat,lon,val(1:8)
          CASE(1)
             READ(lunin,*,iostat=ierr)istnr,lat,lon,hgt,val(1:10)
          CASE(2)
             READ(lunin,*,iostat=ierr)istnr,lat,lon,hgt,val(1:15)
          CASE DEFAULT
             WRITE(6,*)'Cannot handle this vfld-file version',version_flag
             CALL abort
          END SELECT

          IF (ierr.ne.0) CYCLE READ_STATION_MOD

          !
          ! Find station index for the first experiment
          !

          IF ( l == 1 ) THEN
             IF ( stations(istnr) == 0 ) THEN

                stat_i = 0
                IF ( use_stnlist ) THEN
                   DO ii=1,maxstn
                      IF (istnr == stnlist(ii) ) THEN
                          stat_i = ii
                          EXIT
                      ENDIF
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
                hir(max_found_stat)%hgt    = hgt 
   
                IF (max_found_stat > maxstn) THEN
                   WRITE(6,*)'Increase maxstn',max_found_stat
                   CALL abort
                ENDIF

                IF (print_read > 1 ) WRITE(6,*)'ADDED',istnr,stations(istnr)

             ENDIF

          ELSE

             IF(stations(istnr) == 0)  CYCLE READ_STATION_MOD

          ENDIF

          stat_i = stations(istnr)

          !
          ! Station found! Allocate data array if 
          ! this is a new time
          !
         
          IF (print_read > 1 .AND. hir(stat_i)%ntim > 0 ) &
          WRITE(6,*)'BOUND 1',istnr,UBOUND( hir(stat_i)%o(hir(stat_i)%ntim)%nal )
          IF (print_read > 1 .AND. hir(stat_i)%ntim > 0 ) &
          WRITE(6,*)'TEST',istnr,allocated_this_time(stat_i)

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

          ! Cloud cover
          IF (nn_ind /= 0 .AND. qca(val(1),-99.)   .AND. &
                                qcl(val(1),nn_ind) .AND. &
                                qcu(val(1),nn_ind)) hir(stat_i)%o(i)%nal(l,j,nn_ind) = val(1)
          ! Wind direction
          IF (dd_ind /= 0 .AND. qca(val(2),-99.)   .AND. &
                                qcl(val(2),dd_ind) .AND. &
                                qcu(val(2),dd_ind)) hir(stat_i)%o(i)%nal(l,j,dd_ind) = val(2)
          ! Wind speed
          IF (ff_ind /= 0 .AND. qca(val(3),-99.)   .AND. &
                                qcl(val(3),ff_ind) .AND. &
                                qcu(val(3),ff_ind)) hir(stat_i)%o(i)%nal(l,j,ff_ind) = val(3)

          ! Temperature
          IF (tt_ind /= 0 .AND. qca(val(4),-99.)         .AND. &
                                qcl(val(4)-tzero,tt_ind) .AND. &
                                qcu(val(4)-tzero,tt_ind)) hir(stat_i)%o(i)%nal(l,j,tt_ind) = val(4) - tzero

          ! Relative humidity
          IF (rh_ind /= 0 .AND. qca(val(5),-99.)   .AND. &
                                qcl(val(5),rh_ind) .AND. &
                                qcu(val(5),rh_ind)) hir(stat_i)%o(i)%nal(l,j,rh_ind) = val(5)
          ! Pressure
          IF (ps_ind /= 0 .AND. qca(val(6),-99.)   .AND. &
                                qcl(val(6),ps_ind) .AND. &
                                qcu(val(6),ps_ind)) hir(stat_i)%o(i)%nal(l,j,ps_ind) = val(6)
          ! Precipitaion
          IF (pe_ind /= 0 .AND. qca(val(7),-99.)   .AND. &
                                qcl(val(7),pe_ind) .AND. &
                                qcu(val(7),pe_ind)) hir(stat_i)%o(i)%nal(l,j,pe_ind) = val(7)
          ! Specific humidity
          IF (qq_ind /= 0 .AND. qca(val(8),-99.)   .AND. &
                                qcl(val(8),qq_ind) .AND. &
                                qcu(val(8),qq_ind)) hir(stat_i)%o(i)%nal(l,j,qq_ind) = val(8) * 1.e3

          ! Visibility
          IF (vi_ind /= 0 .AND. qca(val(9),-99.)   .AND. &
                                qcl(val(9),vi_ind) .AND. &
                                qcu(val(9),vi_ind)) hir(stat_i)%o(i)%nal(l,j,vi_ind) = val(9)

          ! Dew point temperature
          IF (td_ind /= 0 .AND. qca(val(10),-99.)   .AND. &
                                qcl(val(10),td_ind) .AND. &
                                qcu(val(10),td_ind)) hir(stat_i)%o(i)%nal(l,j,td_ind) = val(10)

          ! Maximum temperature
          IF (tx_ind /= 0 .AND. qca(val(11),-99.)         .AND. &
                                qcl(val(11)-tzero,tx_ind) .AND. &
                                qcu(val(11)-tzero,tx_ind)) hir(stat_i)%o(i)%nal(l,j,tx_ind) = val(11) - tzero

          ! Minimum temperature
          IF (tn_ind /= 0 .AND. qca(val(12),-99.)         .AND. &
                                qcl(val(12)-tzero,tn_ind) .AND. &
                                qcu(val(12)-tzero,tn_ind)) hir(stat_i)%o(i)%nal(l,j,tn_ind) = val(12) - tzero

          ! Wind gust
          IF (gg_ind /= 0 .AND. qca(val(13),-99.)   .AND. &
                                qcl(val(13),gg_ind) .AND. &
                                qcu(val(13),gg_ind)) hir(stat_i)%o(i)%nal(l,j,gg_ind) = val(13)

          ! Wind gust max
          IF (gx_ind /= 0 .AND. qca(val(14),-99.)   .AND. &
                                qcl(val(14),gx_ind) .AND. &
                                qcu(val(14),gx_ind)) hir(stat_i)%o(i)%nal(l,j,gx_ind) = val(14)

          ! Max wind speed
          IF (fx_ind /= 0 .AND. qca(val(15),-99.)   .AND. &
                                qcl(val(15),fx_ind) .AND. &
                                qcu(val(15),fx_ind)) hir(stat_i)%o(i)%nal(l,j,fx_ind) = val(15)

          ! Latitude
          IF (la_ind /= 0 ) hir(stat_i)%o(i)%nal(l,j,la_ind) = hir(stat_i)%lat

          ! Station height
          IF (hg_ind /= 0 ) hir(stat_i)%o(i)%nal(l,j,hg_ind) = hir(stat_i)%hgt

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
    IF(cdate >  edate) EXIT TIME_LOOP
    IF(cdate == edate .AND. ctime/10000 > etime) EXIT TIME_LOOP

 ENDDO TIME_LOOP

 DO i=1,maxstn
    hir(i)%active = ( hir(i)%ntim > 0 )
 ENDDO

 WRITE(6,*) 'FOUND TIMES MODEL',MAXVAL(hir(:)%ntim)

 RETURN

END
