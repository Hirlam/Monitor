SUBROUTINE read_bak

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
            timing_id,                  &
            idate,itime,ilat,ilon
            
 
 
 REAL :: val(12),rh,qq

 CHARACTER(LEN=50) :: fname ='../yyyymmdd.dat'
 INTEGER, PARAMETER :: pl = 3
 INTEGER :: printlev = 1

 LOGICAL :: qcbak,use_stnlist

 
 !     Look-up tables for water vapour satuartion pressure
 !
 real xndegr
 integer nstart,nstop
 
 parameter (xndegr=100.,nstart=1316,nstop=37316)
                    
 real estab,estab_water,destab
      
 common/comtab/estab(nstart:nstop),destab(nstart:nstop),estab_water(nstart:nstop)

 real esat,desdt,tt,esat_water

 esat(tt)=estab(nint(xndegr*tt))
 esat_water(tt)=estab_water(nint(xndegr*tt))
 desdt(tt)=destab(nint(xndegr*tt))


!----------------------------------------------------------

 stations       = 0
 max_found_stat = 0

 use_stnlist =(  MAXVAL(stnlist) > 0 )

 call tabdef

 WRITE(6,*)'ESAT',esat_water(tzero+10.)
 !
 ! If obs array is not allocated
 ! do so and init arrays
 !

 IF (.NOT.obs(1)%obs_is_allocated) THEN

    timing_id = 0
    IF (ltiming) CALL add_timing(timing_id,'allocate_read_vobs')

    ! Estimate maxtim if not given by user
    IF (maxtim.EQ.0) maxtim=get_maxtim(sdate,edate,1)
    IF(lprint_read) WRITE(6,*)'MAXTIM', maxtim

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

    IF (ltiming) CALL add_timing(timing_id,'allocate_read_vfld')

 ENDIF

 ! Copy time

 cdate = sdate
 ctime = stime

 !
 ! Loop over all times
 !

 TIME_LOOP : DO

 IF (lprint_read) WRITE(6,*)'TIME:',cdate,ctime/10000
 WRITE(fname(pl+1:8+pl),'(I8.8)')cdate

 !
 ! Read obs data
 !

       OPEN(lunin,file=fname,status='old',iostat=ierr)

       IF (ierr.NE.0) THEN
  
          IF(lprint_read)WRITE(6,*)'Could not open:',TRIM(fname)

          wdate = cdate
          wtime = ctime
          CALL adddtg(wdate,wtime,24*3600,cdate,ctime)
          IF(cdate.gt.edate) EXIT TIME_LOOP

          CYCLE TIME_LOOP

       ENDIF

       WRITE(6,*)'READ ',fname

       READ_STATION_OBS : DO 

          READ(lunin,*,iostat=ierr)idate,itime,istnr,ilat,ilon,val
          !IF ( printlev > 1 ) WRITE(6,*)idate,itime,istnr

          IF (ierr  == -1) THEN
             wdate = cdate
             wtime = ctime
             CALL adddtg(wdate,wtime,24*3600,cdate,ctime)
             IF(cdate.gt.edate) EXIT TIME_LOOP
             CYCLE TIME_LOOP

          ENDIF

          IF (ierr  /= 0) CYCLE READ_STATION_OBS
          IF (istnr == 0) CYCLE READ_STATION_OBS

          !
          ! Find station index
          !

          !IF (printlev > 1 ) WRITE(6,*)'TRY STATION',istnr

          IF(stations(istnr) == -1 ) CYCLE READ_STATION_OBS

          IF(stations(istnr) == 0) THEN
           
             stat_i = 0
             IF ( use_stnlist ) THEN
                DO ii=1,maxstn
                   IF (istnr == stnlist(ii) ) stat_i = ii
                ENDDO
                IF ( stat_i == 0 ) THEN
                !   IF (printlev > 1 ) WRITE(6,*)'Skip :',istnr
                   stations(istnr) = -1
                   CYCLE READ_STATION_OBS
                ENDIF
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

             IF (max_found_stat > maxstn) THEN
                WRITE(6,*)'Increase maxstn',max_found_stat
                CALL abort
             ENDIF

             IF (printlev > 1 ) WRITE(6,*)'Added :',istnr,stations(istnr)

          ENDIF

          !
          ! Station is found, add time
          !

          stat_i = stations(istnr)

          WHERE(abs(val+999.).lt.1.e-6)
             val = err_ind
          END WHERE

          i = obs(stat_i)%ntim + 1

          ALLOCATE(obs(stat_i)%o(i)%date)
          ALLOCATE(obs(stat_i)%o(i)%time)
          ALLOCATE(obs(stat_i)%o(i)%val(nparver))

          obs(stat_i)%ntim      = i
          obs(stat_i)%o(i)%date = cdate
          obs(stat_i)%o(i)%time = itime
          obs(stat_i)%o(i)%val  = err_ind

          if (ps_ind /= 0 .AND. qcbak(val(2 ))) obs(stat_i)%o(i)%val(ps_ind)  = val(2)
          if (dd_ind /= 0 .AND. qcbak(val(3 ))) obs(stat_i)%o(i)%val(dd_ind)  = val(3)
          if (ff_ind /= 0 .AND. qcbak(val(4 ))) obs(stat_i)%o(i)%val(ff_ind)  = val(4)
          if (tt_ind /= 0 .AND. qcbak(val(5 ))) obs(stat_i)%o(i)%val(tt_ind)  = val(5) 
          if (pe_ind /= 0 .AND. qcbak(val(7 ))) obs(stat_i)%o(i)%val(pe_ind)  = val(7)
          if (nn_ind /= 0 .AND. qcbak(val(11))) obs(stat_i)%o(i)%val(nn_ind)  = val(11)

          if (rh_ind /= 0 .AND.  qcbak(val(5 )) .AND.  qcbak(val(6 ))) obs(stat_i)%o(i)%val(rh_ind) =      &
             100.  * esat_water(tzero+val(6)) / esat_water(tzero+val(5))
          if (qq_ind /= 0 .AND.  qcbak(val(2 )) .AND.  qcbak(val(6 ))) obs(stat_i)%o(i)%val(qq_ind) =      &
             0.622 * esat_water(tzero+val(6)) / ( val(2)*100.)


!         IF (printlev > 1 ) WRITE(6,*)'DATE :',obs(stat_i)%o(i)%date, &
!                                               obs(stat_i)%o(i)%time
!         IF (printlev > 1 ) WRITE(6,*)'PS :',obs(stat_i)%o(i)%val(ps_ind)
!         IF (printlev > 1 ) WRITE(6,*)'DD :',obs(stat_i)%o(i)%val(dd_ind)
!         IF (printlev > 1 ) WRITE(6,*)'FF :',obs(stat_i)%o(i)%val(ff_ind)
!         IF (printlev > 1 ) WRITE(6,*)'TT :',obs(stat_i)%o(i)%val(tt_ind)
!         IF (printlev > 1 ) WRITE(6,*)'PE :',obs(stat_i)%o(i)%val(pe_ind)
!         IF (printlev > 1 ) WRITE(6,*)'NN :',obs(stat_i)%o(i)%val(nn_ind)

       ENDDO READ_STATION_OBS

       CLOSE(lunin)

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,24*3600,cdate,ctime)
    IF(cdate.gt.edate) EXIT TIME_LOOP

 ENDDO TIME_LOOP

 WRITE(6,*) 'FOUND TIMES OBS',obs(1)%ntim

 DO i=1,maxstn
    obs(i)%active = ( obs(i)%ntim > 0 )
 ENDDO

 RETURN

END SUBROUTINE read_bak
LOGICAL FUNCTION qcbak(a)

USE DATA, only : err_ind

IMPLICIT NONE

REAL    :: a

 qcbak = (ABS(a+999.) > 1.e-6 )

END FUNCTION qcbaK
