SUBROUTINE read_vfld_precip

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

 INTEGER :: i,ii,j,jj,k,l,		&
            ierr = 0,			&
            odate = 999999,		&
            otime = 999999,		&
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
            timing_id,          &
            ind_pe(nfclengths)

 
 
 REAL :: lat,lon,val(8),qq

 LOGICAL :: allocated_this_time(maxstn),	&
            found_any_time,use_stnlist

 CHARACTER(LEN=100) :: oname ='vfld',fname = ' '
 CHARACTER(LEN=10) :: cwrk  ='yyyymmddhh'
 CHARACTER(LEN=02) :: cfclen  ='  '

 INTEGER,PARAMETER :: pl = 3 ! Path length of filename

 TYPE (station) :: tmp(maxstn)

!----------------------------------------------------------

 999 format(1x,i5.5,2F8.3,f5.0,f5.0,f6.1,f6.1,f6.1,f7.1,f6.1,en13.3e2)

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
    IF(print_read > 0) WRITE(6,*)'MAXTIM', maxtim

    ! Init model array
    DO k = 1,maxstn
       ALLOCATE(hir(k)%o(maxtim))
    ENDDO
  
    hir%ntim       = 0
    hir%nexp       = nexp
    hir%nparver    = nparver
    hir%nfclengths = 1
    hir%active     = .FALSE.
    hir%stnr       = 0

    hir%obs_is_allocated = .TRUE.

    IF (ltiming) CALL add_timing(timing_id,'allocate_read_vfld')

 ENDIF

 ind_pe = 0
 DO i=1,nfclengths
    IF(fclen(i).LT.12) CYCLE
    ind_pe(i)=transfer(minloc(abs(fclen(1:nfclengths)-(fclen(i)-12))),ii)
    IF (fclen(i)-fclen(ind_pe(i)).LT.12 ) ind_pe(i) = 0
 ENDDO

 ! Init tmp array
 DO k = 1,maxstn
    ALLOCATE(tmp(k)%o(1))
    ALLOCATE(tmp(k)%o(1)%nal(nexp,nfclengths,nparver))
    tmp(k)%o(1)%nal = err_ind
 ENDDO
  
 tmp%ntim       = 0
 tmp%nexp       = nexp
 tmp%nparver    = nparver
 tmp%nfclengths = nfclengths
 tmp%active     = .FALSE.
 tmp%stnr       = 0



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

       WRITE(cwrk(1:10),'(I8,I2.2)')cdate,ctime/10000
       WRITE(cfclen(1:2),'(I2.2)')fclen(j)
       fname = TRIM(modpath(l))//TRIM(oname)//TRIM(expname(l))//cwrk//cfclen


       OPEN(lunin,file=fname,status='old',iostat=ierr)
       IF (ierr.NE.0) THEN
           IF ( print_read > 0 ) WRITE(6,*)'Could not open ',fname
           CYCLE EXP_LOOP
       ENDIF
       IF (print_read > 0 ) WRITE(6,*)'READ ',fname

       READ(lunin,*)num_stat,num_temp
       READ(lunin,*)num_temp_lev

       READ_STATION_MOD : DO k=1,num_stat

          READ(lunin,999,iostat=ierr)istnr,lat,lon,val
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

          ENDIF

          stat_i = stations(istnr)

          WHERE(abs(val+99.) < 1.e-6)
             val = err_ind
          END WHERE

          !
          ! Add data
          !

          IF (pe_ind /= 0 ) tmp(stat_i)%o(1)%nal(l,j,pe_ind) = val(7)

       ENDDO READ_STATION_MOD

       CLOSE(lunin)

    ENDDO EXP_LOOP
    ENDDO LL_LOOP

    !
    ! Adjust precip
    !

    DO  j=1,nfclengths

       IF ( fclen(j) > 12 .AND. ind_pe(j).GT.0 .OR. fclen(j) == 12 ) THEN


       CALL adddtg(cdate,ctime,fclen(j)*3600,odate,otime)

       DO jj=1,maxstn

          IF ( .NOT. hir(jj)%active ) CYCLE

          i = hir(jj)%ntim + 1

          ALLOCATE(hir(jj)%o(i)%date)
          ALLOCATE(hir(jj)%o(i)%time)
          ALLOCATE(hir(jj)%o(i)%nal(nexp,1,nparver))

          hir(jj)%ntim      = i
          hir(jj)%o(i)%date = odate
          hir(jj)%o(i)%time = otime/10000
          hir(jj)%o(i)%nal  = err_ind

          IF ( fclen(j) == 12 ) THEN
             WHERE( ABS( tmp(jj)%o(1)%nal(:,j        ,pe_ind)- err_ind ) > 1.e-6 )      &
             hir(jj)%o(i)%nal(:,1        ,pe_ind) =                &
             tmp(jj)%o(1)%nal(:,j        ,pe_ind) 
          ELSE
             WHERE( ABS( tmp(jj)%o(1)%nal(:,j        ,pe_ind)- err_ind ) > 1.e-6 .AND.  &
                    ABS( tmp(jj)%o(1)%nal(:,ind_pe(j),pe_ind)- err_ind ) > 1.e-6 )      &
             hir(jj)%o(i)%nal(:,1        ,pe_ind) =                &
             tmp(jj)%o(1)%nal(:,j        ,pe_ind) -                &
             tmp(jj)%o(1)%nal(:,ind_pe(j),pe_ind) 
          ENDIF
          
          !
          ! Due to lack of precision in GRIB the precipitation difference
          ! between two forecasts may actually be negative, reject !!!!!
          !
          DO l=1,nexp
          IF (    (hir(jj)%o(i)%nal(l,1        ,pe_ind) < 0.) .AND.     &
               ABS(hir(jj)%o(i)%nal(l,1        ,pe_ind) - err_ind) > 1.e-6 ) THEN
                  WRITE(6,*)'ERROR in precipitation'
                  WRITE(6,*)'Reject value ', hir(jj)%o(i)%nal(l,1,pe_ind) 
                  WRITE(6,*)' TIME ',cdate,ctime,fclen(j),fclen(ind_pe(j)),ind_pe(j)
                  WRITE(6,*)'STATION ',hir(jj)%stnr
                  WRITE(6,*)'EXP ',expname(l)
                  WRITE(6,*)'VAL 1',tmp(jj)%o(1)%nal(l,j        ,pe_ind) 
                  WRITE(6,*)'VAL 2',tmp(jj)%o(1)%nal(l,ind_pe(j),pe_ind) 
                  hir(jj)%o(i)%nal(:,1        ,pe_ind) = err_ind
          ENDIF
          ENDDO ! l=1,nexp

       ENDDO 

       ENDIF

    ENDDO 


    !
    ! Step time
    !

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,fcint*3600,cdate,ctime)
    IF(cdate > edate) EXIT TIME_LOOP
    IF(cdate >= edate .AND. ctime/10000 > etime) EXIT TIME_LOOP

 ENDDO TIME_LOOP

 ! Reset fclen

 fclen = 0
 nfclengths = 1

 DO i=1,maxstn
    hir(i)%active = ( hir(i)%ntim > 0 )
 ENDDO

 WRITE(6,*) 'FOUND TIMES MODEL',MAXVAL(hir(:)%ntim)

 RETURN

END
