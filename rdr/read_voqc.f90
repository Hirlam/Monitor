SUBROUTINE read_voqc

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
            timing_id
            
 
 
 REAL :: lat,lon,height,val(7)

 CHARACTER(LEN=50) :: fname ='../voqcyymmddhh'
 INTEGER, PARAMETER :: pl = 3

 LOGICAL :: qcq,use_stnlist

!----------------------------------------------------------

 stations       = 0
 max_found_stat = 0

 use_stnlist =(  MAXVAL(stnlist) > 0 )

 !
 ! If obs array is not allocated
 ! do so and init arrays
 !

 IF (.NOT.obs(1)%obs_is_allocated) THEN

    timing_id = 0
    IF (ltiming) CALL add_timing(timing_id,'allocate_read_voqc')

    ! Estimate maxtim if not given by user
    IF (maxtim.EQ.0) maxtim=get_maxtim(sdate,edate_obs,1)
    IF( print_read > 1 ) WRITE(6,*)'MAXTIM', maxtim

    ! Init obs array
    DO k = 1,maxstn
       ALLOCATE(obs(k)%o(maxtim))
    ENDDO
  
    obs%ntim       = 0
    obs%nexp       = nexp
    obs%nparver    = nparver
    obs%nfclengths = nfclengths
    obs%stnr       = 0

    obs%obs_is_allocated = .TRUE.

    IF (ltiming) CALL add_timing(timing_id,'allocate_read_voqc')

 ENDIF

 ! Copy time

 cdate = sdate
 ctime = stime
 wdate = cdate
 wtime = ctime
 
 !
 ! Loop over all times
 !

 i = 0

 TIME_LOOP : DO

 IF (print_read > 1) WRITE(6,*)'TIME:',cdate,ctime/10000
 WRITE(fname(pl+5:12+pl),'(I6.6,I2.2)')MOD(cdate,1000000),ctime/10000

 i = i + 1

 !
 ! Read obs data
 !

       OPEN(lunin,file=fname,status='old',iostat=ierr)

       IF (ierr.NE.0) THEN
  
          IF(print_read > 1 )WRITE(6,*)'Could not open:',TRIM(fname)

          wdate = cdate
          wtime = ctime
          CALL adddtg(wdate,wtime,3600*obint,cdate,ctime)
          IF(cdate.gt.edate_obs) EXIT TIME_LOOP

          i = i - 1
          CYCLE TIME_LOOP

       ENDIF

       IF (print_read > 0 ) WRITE(6,*)'READ ',fname

       READ(lunin,*)num_stat,num_temp
       READ(lunin,*)num_temp_lev

       READ_STATION_OBS : DO k=1,num_stat

          READ(lunin,*,iostat=ierr)istnr,lat,lon,val
          IF (ierr  /= 0) CYCLE READ_STATION_OBS
          IF (istnr == 0) CYCLE READ_STATION_OBS

          !
          ! Find station index
          !

          IF(stations(istnr) == 0) THEN
           
             stat_i = 0
             IF ( use_stnlist ) THEN
                DO ii=1,maxstn
                   IF (istnr == stnlist(ii) ) stat_i = ii
                ENDDO
                IF ( stat_i == 0 ) CYCLE READ_STATION_OBS
             ENDIF

             IF (stat_i == 0 ) THEN 
                max_found_stat  = max_found_stat + 1
                stnlist(max_found_stat)= istnr
             ELSE
                max_found_stat  = stat_i
             ENDIF

             stations(istnr) = max_found_stat 
             obs(max_found_stat)%active = .TRUE.
             obs(max_found_stat)%stnr   = istnr
             obs(max_found_stat)%lat    = lat
             obs(max_found_stat)%lon    = lon

             IF (max_found_stat > maxstn) THEN
                WRITE(6,*)'Increase maxstn',max_found_stat
                CALL abort
             ENDIF

          ENDIF

          stat_i = stations(istnr)

          !
          ! Make Celcius
          !

          IF (ABS(val(4)-err_ind) > 1.e-6 ) val(4) = val(4) - tzero

          i = obs(stat_i)%ntim + 1

          ALLOCATE(obs(stat_i)%o(i)%date)
          ALLOCATE(obs(stat_i)%o(i)%time)
          ALLOCATE(obs(stat_i)%o(i)%val(nparver))

          obs(stat_i)%ntim      = i
          obs(stat_i)%o(i)%date = cdate
          obs(stat_i)%o(i)%time = ctime/10000
          obs(stat_i)%o(i)%val  = err_ind

          if (nn_ind /= 0 .AND. qcq(val(1))) obs(stat_i)%o(i)%val(nn_ind) = val(1)
          if (dd_ind /= 0 .AND. qcq(val(2))) obs(stat_i)%o(i)%val(dd_ind) = val(2)
          if (ff_ind /= 0 .AND. qcq(val(3))) obs(stat_i)%o(i)%val(ff_ind) = val(3)
          if (tt_ind /= 0 .AND. qcq(val(4))) obs(stat_i)%o(i)%val(tt_ind) = val(4) 
          if (rh_ind /= 0 .AND. qcq(val(5))) obs(stat_i)%o(i)%val(rh_ind) = val(5)
          if (ps_ind /= 0 .AND. qcq(val(6))) obs(stat_i)%o(i)%val(ps_ind) = val(6)
          if (pe_ind /= 0 .AND. qcq(val(7))) obs(stat_i)%o(i)%val(pe_ind) = val(7)
!         if (qq_ind /= 0 .AND. qcq(val(8))) obs(stat_i)%o(i)%val(qq_ind) = val(8) * 1.e3

       ENDDO READ_STATION_OBS

       CLOSE(lunin)

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,3600*obint,cdate,ctime)
    IF(cdate.gt.edate_obs) EXIT TIME_LOOP

 ENDDO TIME_LOOP

 IF (print_read > 0 ) WRITE(6,*) 'FOUND TIMES OBS',obs(1)%ntim

 DO i=1,maxstn
    obs(i)%active = ( obs(i)%ntim > 0 )
 ENDDO

 RETURN

END SUBROUTINE read_voqc
LOGICAL FUNCTION qcq(a)

USE DATA, only : err_ind

IMPLICIT NONE

REAL    :: a

 qcq = (ABS(a+99.) > 1.e-6 )

END FUNCTION qcq
