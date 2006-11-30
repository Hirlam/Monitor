SUBROUTINE verify

!
! Verify MODEL and observations
!
! Ulf Andrae, SMHI, 2002 - 2006
! 
! In principle three types of statistic arrays are calculated and accumulated
! 
! 1. Statistics dependent on forecast length or time of day, this is used for
!    Normals verification plots and for maps of bias/rms/observed values
!
! 2. Time serie statistics where we keep the time position for each departure.
!    For each time we may have several forecast lengths
!
! 3. A huge array keeping all departures as is, both for each single stations
!    and all stations. It's used for scatterplots and frequency distributions.
!    These arrays may demand a wast amount of memory for high number of stations
!    and long time series.
!
! All types of statistics may be accumulated for the full given period,
! monthvise and to some extent as a function of season. The last only applies
! when running over several years.
!
! By the way, this subroutine needs to be rewritten .....
!
!

 USE data
 USE functions
 USE timing

 IMPLICIT NONE

 !
 ! Local
 !

 INTEGER :: i,j,k,l,m,n,o,oo,ii,aerr,                   &
            jj,jjstart,jjcheck(nfclengths),             &
            yer_ind,mon_ind,tim_ind,nmax,               &
            mindate(maxstn),maxdate(maxstn),            &
            wdate,wtime,                                &
            maxmon,                                     &
            timing_id,                                  &
            timing_id_init,                             &
            timing_id_loop,                             &
            timing_id_plot,                             &
            ind_pe(nfclengths),                         &
            lyear,year_map(12) = 0,                     &
            time_stat_max,this_stat_time,               &
            all_time_stat_max,                          &
            cdate,ctime,                                &
            edate_obs,etime_obs,                        &
            all_time_stat_active,                       &
            gross_pos(2),nexp_ver,                      &
            par_active(nparver),                        &
            time_stat_fclen_diff

 INTEGER, ALLOCATABLE :: months(:),gross_error(:,:)

 REAL              :: diff,diff_prep
 REAL, ALLOCATABLE :: tmpdiff(:)

 LOGICAL :: all_exp_verified = .TRUE.
 LOGICAL :: found_right_time = .FALSE.
 LOGICAL :: remember         = .FALSE.
 LOGICAL :: alert_missing    = .TRUE.
 LOGICAL :: demand_equal     = .TRUE.
 LOGICAL :: do_kalman        = .TRUE.

 INTEGER, ALLOCATABLE :: all_par_active(:,:),       &
                         tim_par_active(:,:),       &
                         mon_tim_par_active(:,:,:), &
                         mon_par_active(:,:,:),     &
                         yer_par_active(:,:,:)

 TYPE (statpar), ALLOCATABLE :: allstat(:,:),yearstat(:,:)

 TYPE (stat_obs), ALLOCATABLE :: time_stat(:),all_time_stat(:)

 TYPE (scatter_type), ALLOCATABLE ::              &
                          scat_data(:),           &
                      all_scat_data(:),           &
                      mon_scat_data(:,:),         &
                  all_mon_scat_data(:,:),         &
                      yer_scat_data(:,:)

 !----------------------------------------------------------


 !
 ! Timing initialization
 !

 timing_id = 0
 IF (ltiming) CALL add_timing(timing_id,'Verify')

 timing_id_init = 0
 IF (ltiming) CALL add_timing(timing_id_init,'Verify_init')

 WRITE(6,*)
 WRITE(6,*)'--VERIFY--'
 WRITE(6,*)

 !
 ! Kalman filter specifics
 !
 nexp_ver= nexp
 IF ( use_kalman ) THEN
   nexp = 2 * nexp
   DO i=nexp_ver+1,nexp
     expname(i) ='k_'//TRIM(expname(i-nexp_ver))
   ENDDO
 ENDIF

 ALLOCATE(tmpdiff(nexp))

 !
 ! Estime minimum time steps
 ! Needed for estimation of array sizes later on
 !

 IF (lfcver ) THEN
    IF ( nfclengths == 1 ) THEN
       timdiff = fcint
    ELSE
       timdiff = fclen(2) - fclen(1)
    ENDIF
 ELSE
    timdiff = 24 / ntimver
 ENDIF


 !
 ! Find which single stations to allocate/plot
 !

 IF (MAXVAL(stnlist_plot) == -1 ) THEN
    leach_station = .FALSE.
 ELSEIF (MAXVAL(stnlist_plot) >= 0 ) THEN
    leach_station = .TRUE.
 ENDIF


 !
 ! Adjust observation enddate to maximum fc length
 !

 CALL adddtg(edate,etime,3600*MAXVAL(fclen+24),edate_obs,etime_obs)


 !
 ! Find PE index locations
 !

 IF (lfcver ) THEN
    ind_pe = 0
    DO i=1,nfclengths
       IF(fclen(i) < 12) CYCLE 
       ind_pe(i)=TRANSFER(MINLOC(ABS(fclen(1:nfclengths)-(fclen(i)-12))),ii)
       if (fclen(i)-fclen(ind_pe(i)) < 12 ) ind_pe(i) = 0
    ENDDO
    IF(lprint_verif) WRITE(6,*)'IND_PE',ind_pe
 ENDIF


 ! Set min and max date
 mindate = 90000101
 maxdate = 0


 !
 ! Allocate arrays for monthvise verification
 !

 IF (monthvise) THEN

    IF(lprint_verif) WRITE(6,*)'ALLOCATE monthly'

    !
    ! Find number of months on monthvise verification
    !

    maxmon = get_maxmon(sdate,edate) 
    i = 0
    IF ( MOD(maxmon,monthvise_freq) /= 0 ) i = 1
    maxmon = maxmon / monthvise_freq + i
   
    ALLOCATE(months(maxmon+1))

    months(1) = sdate/100
    DO i=2,maxmon+1
       months(i) = monincr(months(i-1),monthvise_freq)
    ENDDO

    !
    ! Allocate and init the general monthvise statistics array
    !

    ALLOCATE(allstat(maxstn,maxmon))

    DO j=1,maxmon
    DO i=1,maxstn
       ALLOCATE(allstat(i,j)%s(nexp,nparver,ntimver))
       ALLOCATE(allstat(i,j)%par_active(nparver))
    ENDDO
    ENDDO

    DO j = 1,maxmon
    DO i = 1,maxstn
       allstat(i,j)%s = statistics(0.,0.,0.,0,0,0.)
       allstat(i,j)%par_active = 0
    ENDDO
    ENDDO

    IF (lplot_scat .OR. lplot_freq ) THEN

       ! All station scatter plot array 
       ALLOCATE(mon_scat_data(nparver,maxmon))

       ii = 31*monthvise_freq*24/MIN(timdiff,fcint)
       ii = ii * timdiff * nfclengths / fcint
       ii = MAX(ii,maxtim_scat)
       DO j=1,maxmon
          DO i=1,nparver
             ALLOCATE(mon_scat_data(i,j)%dat(nexp+1,ii),stat=aerr)
             IF ( aerr /= 0 ) THEN
                WRITE(6,*)'ERROR IN ALLOCATE',aerr,ii
                CALL ABORT
             ENDIF
   
          ENDDO
       ENDDO
       mon_scat_data%n = 0

       IF (lallstat) THEN
       ALLOCATE(all_mon_scat_data(nparver,maxmon))
       DO j=1,maxmon
          DO i=1,nparver
             ALLOCATE(all_mon_scat_data(i,j)%dat(nexp+1,active_stations*ii),stat=aerr)
             IF ( aerr /= 0 ) THEN
                WRITE(6,*)'ERROR IN ALLOCATE',aerr,ii
                CALL ABORT
             ENDIF
   
          ENDDO
       ENDDO
       all_mon_scat_data%n = 0
       ENDIF

       ALLOCATE(mon_par_active(maxstn,nparver,maxmon))
       mon_par_active = 0

    ENDIF

 ENDIF 

 !
 ! Allocate arrays for seasonal verification
 !

 IF(lyearvise)THEN

    IF(lprint_verif) WRITE(6,*)'ALLOCATE seasonal'

    SELECT CASE(yearvise_freq)
    CASE(1)
       ! Month by month
       year_map =(/1,2,3,4,5,6,7,8,9,10,11,12/)
       lyear = 12
    CASE(3)
       ! Seasons DJF,MAM,JJA,SON
       year_map =(/1,1,2,2,2,3,3,3,4,4,4,1/)
       lyear = 4
    CASE DEFAULT
       WRITE(6,*)'yearvise_freq should be 1 or 3, changed to 3'
       yearvise_freq = 3
       lyear = 4
       year_map =(/1,1,2,2,2,3,3,3,4,4,4,1/)
    END SELECT

    ALLOCATE(yearstat(maxstn,lyear))

    DO j=1,lyear
    DO i=1,maxstn
       ALLOCATE(yearstat(i,j)%s(nexp,nparver,ntimver))
       ALLOCATE(yearstat(i,j)%par_active(nparver))
    ENDDO
    ENDDO


    DO j=1,lyear
    DO i=1,maxstn
       yearstat(i,j)%s=statistics(0.,0.,0.,0,0,0.)
       yearstat(i,j)%par_active = 0
    ENDDO
    ENDDO

    IF (lplot_scat .OR. lplot_freq ) THEN

       ! All station array
       ALLOCATE(yer_scat_data(nparver,lyear))

       ii = 31*yearvise_freq*24/MIN(timdiff,fcint) * &
            (edate/10000 - sdate/10000 +1)
       ii = ii * timdiff * nfclengths / fcint
       ii = MAX(ii,maxtim_scat)
       DO j=1,lyear
          DO i=1,nparver
             ALLOCATE(yer_scat_data(i,j)%dat(nexp+1,active_stations*ii))
          ENDDO
       ENDDO
       yer_scat_data%n = 0

       ALLOCATE(yer_par_active(maxstn,nparver,lyear))
       yer_par_active = 0

    ENDIF

 ENDIF


 !
 ! Allocate array for timeserie statistics
 !

 IF ( (ltimeserie_stat.OR.ltimeserie_stat_month) .AND.(lallstat .OR. leach_station )) THEN

    IF(lprint_verif) WRITE(6,*)'ALLOCATE timeserie',sdate,edate_obs
    ii = get_maxtim(sdate,edate_obs,MIN(timdiff,fcint))
    IF(lprint_verif) WRITE(6,*)'ALLOCATE timeserie',timdiff,fcint,ii

    ALLOCATE(    time_stat(ii),all_time_stat(ii))

    IF(lprint_verif) WRITE(6,*)'DONE ALLOCATE timeserie',timdiff,fcint

        time_stat_max    = 0
    all_time_stat_max    = 0
    all_time_stat_active = 0

    cdate = sdate
    ctime = stime

    !
    ! Check minimum time_stat_fclen difference
    !

    IF ( ANY( time_stat_fclen /= -1 ))THEN

       time_stat_fclen_diff = -1
       IF ( time_stat_fclen(1)   /= -1 ) time_stat_fclen_diff = time_stat_fclen(1)
       IF ( time_stat_fclen_diff ==  0 ) time_stat_fclen_diff = fcint

       DO i=2,48
          
          IF ( time_stat_fclen(i) /= -1 ) THEN
                  IF ( time_stat_fclen_diff >                         &
                       time_stat_fclen(i) - time_stat_fclen(i-1))     &
                       time_stat_fclen_diff =                         &
                       time_stat_fclen(i) - time_stat_fclen(i-1) 
          ENDIF
       ENDDO
    ELSE
       time_stat_fclen_diff = -1
    ENDIF

    IF(lprint_verif) WRITE(6,*)'time_stat_fclen_diff',time_stat_fclen_diff

    IF ( time_stat_fclen_diff == -1 ) THEN
       CALL adddtg(cdate,ctime*10000,fclen(1)*3600,wdate,wtime)
    ELSE
       CALL adddtg(cdate,ctime*10000,time_stat_fclen_diff*3600,wdate,wtime)
    ENDIF

    wtime = wtime / 10000
    cdate = wdate
    ctime = wtime

    i = 0
    DO 


       i = i + 1

       ALLOCATE(                                 &
                time_stat(i)%obs(nparver),       &            
                time_stat(i)%bias(nexp,nparver), &
                time_stat(i)%rmse(nexp,nparver), &
                time_stat(i)%n(nexp,nparver),    &
                time_stat(i)%r(nexp,nparver),    &
                time_stat(i)%date,               &
                time_stat(i)%time            )

         time_stat(i)%obs  = 0.
         time_stat(i)%bias = 0.
         time_stat(i)%rmse = 0.
         time_stat(i)%n    = 0
         time_stat(i)%r    = 0
         time_stat(i)%date = 0
         time_stat(i)%time = 0

       IF (lallstat) THEN

          ALLOCATE(                            &
          all_time_stat(i)%obs(nparver),       &            
          all_time_stat(i)%bias(nexp,nparver), &            
          all_time_stat(i)%rmse(nexp,nparver), &
          all_time_stat(i)%n(nexp,nparver),    &
          all_time_stat(i)%r(nexp,nparver),    &
          all_time_stat(i)%date,               &
          all_time_stat(i)%time            )

          all_time_stat(i)%date = cdate
          all_time_stat(i)%time = ctime

          all_time_stat(i)%obs  = 0.
          all_time_stat(i)%bias = 0.
          all_time_stat(i)%rmse = 0.
          all_time_stat(i)%n    = 0
          all_time_stat(i)%r    = 0

       ENDIF

       IF ( time_stat_fclen_diff == -1 ) THEN
         CALL adddtg(cdate,ctime*10000,timdiff*3600,wdate,wtime)
       ELSE
         CALL adddtg(cdate,ctime*10000,time_stat_fclen_diff*3600,wdate,wtime)
       ENDIF

       wtime = wtime / 10000
       cdate = wdate
       ctime = wtime

       IF ( cdate > edate_obs ) EXIT

    ENDDO

    all_time_stat_max = i

    IF ( ltimeserie_stat ) THEN
       ALLOCATE(tim_par_active(maxstn,nparver))
       tim_par_active = 0
    ENDIF

    IF ( ltimeserie_stat_month ) THEN
       ALLOCATE(mon_tim_par_active(maxmon,maxstn,nparver))
       mon_tim_par_active = 0
    ENDIF

 ENDIF


 IF (lstat_gen .AND. (lplot_scat .OR. lplot_freq) .AND. (lallstat .OR. leach_station)) THEN


    !
    ! Allocate data for array including all verified points
    ! Used for scatterplots and frequency distribution
    !

    ii = get_maxtim(sdate,edate_obs,MIN(timdiff,fcint))
    ii = ii * timdiff * nfclengths / fcint
    ii = MAX(ii,maxtim_scat)

    IF(lprint_verif) WRITE(6,*)'ALLOCATE scatter',ii

    ! Single station array
    ALLOCATE(scat_data(nparver))
    DO i=1,nparver
       ALLOCATE(scat_data(i)%dat(nexp+1,ii))
    ENDDO
    scat_data%n = 0

    ! All station array
    IF (lallstat) THEN
       ALLOCATE(all_scat_data(nparver))
       DO i=1,nparver
          ALLOCATE(all_scat_data(i)%dat(nexp+1,active_stations*ii))
       ENDDO
       all_scat_data%n = 0
    ENDIF

    ALLOCATE(all_par_active(maxstn,nparver))
    all_par_active = 0

 ENDIF

 !
 ! Allocate and init main statistics arrays
 !

 ALLOCATE(stat(maxstn))
 DO i=1,maxstn
    ALLOCATE(stat(i)%s(nexp,nparver,ntimver))
    ALLOCATE(stat(i)%par_active(nparver))
 ENDDO

 DO i = 1,maxstn
    stat(i)%s = statistics(0.,0.,0.,0,0,0.)
    stat(i)%par_active = 0
 ENDDO

 stat(1:maxstn)%stnr  = hir(1:maxstn)%stnr
 stat(1:maxstn)%lat   = hir(1:maxstn)%lat
 stat(1:maxstn)%lon   = hir(1:maxstn)%lon

 stat%active     = .FALSE.


 !
 ! Gross error tracking
 !

 ALLOCATE(gross_error(maxstn,nparver))
 gross_error = 0

 IF (ltiming) CALL add_timing(timing_id_init,'Verify_init')

 ! --------------------------
 ! End of allocation buisness
 ! --------------------------

 timing_id_loop = 0
 IF (ltiming) CALL add_timing(timing_id_loop,'Verify_loop')


 OPEN(lunstat,file=statname)

 !
 ! Create filename
 !

 WRITE(lunstat,*)'<html><pre>'
 WRITE(lunstat,*)'Verification for ',trim(name)

 IF (lfcver) THEN
    WRITE(lunstat,*)'Verification by forecast length'
 ELSE
    WRITE(lunstat,*)'Verification by time of day'
 ENDIF
 WRITE(lunstat,*)


 !
 ! Loop over all stations
 !

 STATION_CYCLE : DO i=1,maxstn


    csi = i
    !
    ! Check if data are available, active and correct
    !

    IF ( .NOT. hir(i)%obs_is_allocated ) THEN
       WRITE(6,*)'Your model data is not allocated '
       WRITE(6,*)'Set LRELEASE_MEMORY = F'
       CYCLE STATION_CYCLE
    ENDIF

    IF (.NOT.(hir(i)%active.AND.obs(i)%active)) THEN
       CYCLE STATION_CYCLE
       IF (release_memory) DEALLOCATE(obs(i)%o,hir(i)%o)
    ENDIF

    IF (hir(i)%stnr.NE.obs(i)%stnr) THEN
       WRITE(6,*)'Your stations does not agree',hir(i)%stnr,obs(i)%stnr
       CALL abort
    ENDIF

    IF (lprint_verif) WRITE(6,*)'DO STATION',i,hir(i)%stnr


    !
    ! Check if we should plot this station
    !

    IF (MAXVAL(stnlist_plot) == -1 ) THEN
       leach_station = .FALSE.
    ELSEIF (MAXVAL(stnlist_plot) == 0 ) THEN
       leach_station = .TRUE.
    ELSE
       leach_station = .FALSE.
       DO j=1,maxstn
          IF (hir(i)%stnr == stnlist_plot(j)) THEN
             leach_station = .TRUE.
             EXIT
          ENDIF
       ENDDO
    ENDIF


    !
    ! Loop over all model times and forecast hours for this observation
    !

    jjstart = 1
    jjcheck = obs(i)%ntim

    J_CYCLE : DO j=1,hir(i)%ntim

     found_right_time = .FALSE.

     FC_CYCLE : DO n=1,nfclengths

       IF (lprint_verif) THEN
          WRITE(6,*)'FC TIME',i,j,n    
          WRITE(6,*)'FC TIME',hir(i)%o(j)%date,hir(i)%o(j)%time
          WRITE(6,*)'FC TIME',fclen(n)
       ENDIF

       !
       ! Step time to verification time 
       ! If we have no observations inside the range then cycle
       !

       CALL adddtg(hir(i)%o(j)%date,hir(i)%o(j)%time*10000,&
                   fclen(n)*3600,wdate,wtime)
       wtime = wtime / 10000

       IF (lprint_verif) WRITE(6,*)'CHECK TIME VER ',wdate,wtime

       IF (obs(i)%o(obs(i)%ntim)%date < wdate) CYCLE J_CYCLE

       !
       ! Loop over all observations and all times
       !

       IF (lprint_verif) WRITE(6,*)'JJSTART AT START ',jjstart
       JJ_CYCLE : DO jj=jjstart,obs(i)%ntim

       IF (lprint_verif) WRITE(6,*)'CHECK TIME OBS ',   &
          obs(i)%o(jj)%date,obs(i)%o(jj)%time

       !
       ! Cycle FC_CYCLE if we have passed the model date
       !

       IF (obs(i)%o(jj)%date > wdate) CYCLE FC_CYCLE

       OBS_TEST :				&
       IF(obs(i)%o(jj)%date == wdate .AND.	&
          obs(i)%o(jj)%time == wtime ) THEN

          found_right_time = .TRUE.

          IF (lprint_verif) WRITE(6,*)'TIME VER ',wdate,wtime

          ! If this is right and existing date calc stat
          ! Save min/max date and set correct time index
          ! If observation exists calc stat
          ! Winds cannot be more than 180 wrong 
          ! Start search for next time at next position

          jjcheck(n) = jj

          !
          ! Find index for daily arrays
          !

          IF (lfcver) THEN
             tim_ind = n
          ELSE
             tim_ind = wtime/timdiff+1
          ENDIF
          mon_ind = mondiff(sdate/100,wdate/100)/monthvise_freq + 1
          yer_ind = year_map(MOD(wdate/100,100))

          stat(i)%active = .TRUE.
          IF (monthvise)  allstat(i,mon_ind)%active = .TRUE.
          IF (lyearvise) yearstat(i,yer_ind)%active = .TRUE.

          !
          ! Reject this observation if not all variables are present
          !
          IF ( all_var_present .AND. ANY(ABS( obs(i)%o(jj)%val -err_ind ) < 1.e-6 ))  CYCLE

          NPARVER_LOOP : DO k=1,nparver

             !
             ! Loop over all variables
             !

             IF (lprint_verif) WRITE(6,*)'DO VER ',OBSTYPE(k),obs(i)%o(jj)%val(k)


             !
             ! Special windp
             !
!            IF (use_kalman .AND. ( k == wp_ind )) THEN
!                     IF (obs(i)%o(jj)%val(k) < 0.1 ) obs(i)%o(jj)%val(k) = err_ind
!            ENDIF

             IF(ABS(obs(i)%o(jj)%val(k)-err_ind).LE.1.e-6) CYCLE NPARVER_LOOP

             !
             ! All EXP should have data, else do not verify
             !

             DO o=1,nexp_ver
               IF (ABS(hir(i)%o(j)%nal(o,n,k)-err_ind) < 1.e-6) THEN
                  IF (alert_missing ) THEN
                     WRITE(6,*) 'BAD MODEL DATA',i,j,o,n,hir(i)%o(j)%nal(:,n,k)
                     WRITE(6,*) 'Some model data may be missing, continue'
                     alert_missing = .false.
                  ENDIF
                  IF ( demand_equal ) hir(i)%o(j)%nal(:,n,k) = err_ind
                  CYCLE NPARVER_LOOP
               ENDIF
             ENDDO

             IF (lprint_verif) WRITE(6,*)'FOUND DATA'
             all_exp_verified = .TRUE.
             tmpdiff          = 0.

             !
             ! Special conditions
             !

             !!!IF ( lspecial_cond ) CALL special_cond(I!

 
             EXP_LOOP : DO o=1,nexp_ver

                IF( k == wp_ind ) THEN

                   !
                   ! Special for wind power
                   ! Reject zero value observations if wind is higher
                   !

                     IF  (obs(i)%o(jj)%val(k) < 1.e-6 ) THEN
                        IF (       ANY(hir(i)%o(j)%nal(:,n,k) > 100.)  .AND.     &
                             .NOT. ANY(hir(i)%o(j)%nal(:,n,k) < 50. )       )          THEN
                             WRITE(6,*)'Rejected wp obs', hir(i)%stnr,wdate,wtime,     &
                             obs(i)%o(jj)%val(k),hir(i)%o(j)%nal(:,n,k)
                             !obs(i)%o(jj)%val(k) = err_ind
                             obs(i)%o(jj)%val = err_ind
                             CYCLE NPARVER_LOOP
                        ENDIF
                     ENDIF

                ENDIF


                IF(k.EQ.pe_ind .AND. lfcver) THEN

                   !
                   ! Special for precipitation
                   !

                   IF (lprint_verif) WRITE(6,*)'PE ',i,n,o,ind_pe(n)
                   IF(fclen(n).EQ.12) THEN
                      diff_prep = hir(i)%o( j)%nal(o,n,k)
                   ELSEIF(fclen(n) > 12 .AND. ind_pe(n) > 0 ) THEN

                      IF (ABS(hir(i)%o(j)%nal(o,ind_pe(n),k)-err_ind)<1.e-6) THEN
                         IF (demand_equal ) hir(i)%o(j)%nal(:,ind_pe(n),k) = err_ind
                         all_exp_verified = .FALSE.
                         CYCLE EXP_LOOP
                      ENDIF

                      diff_prep = hir(i)%o(j)%nal(o,n        ,k) - &
                                  hir(i)%o(j)%nal(o,ind_pe(n),k)

                      IF (diff_prep < 0.) THEN
                         WRITE(6,*)'Model precipitation is negative',diff_prep
                         WRITE(6,'(2A,2I10,2I3)')expname(o),	&
                         ' station:',hir(i)%stnr,wdate,wtime,fclen(n)
                         all_exp_verified = .FALSE.
                         CYCLE EXP_LOOP
                      ENDIF

                   ELSE
                      all_exp_verified = .FALSE.
                      CYCLE EXP_LOOP
                   ENDIF
                ELSE
                   diff_prep=hir(i)%o(j)%nal(o,n,k)
                ENDIF

                diff =  diff_prep - obs(i)%o(jj)%val(k)

                IF(lprint_verif) WRITE(6,*)'diff ',diff

                !
                ! Wind direction
                !
                !IF(k == dd_ind.AND.ABS(diff) > 180.) diff = diff + SIGN(360.,180.-diff)

                !
                ! Gross error check
                !

                IF (qc(diff,k)) THEN
                   tmpdiff(o) = diff
                ELSE   

                  !
                  ! Reject erroneous observations
                  !

                  IF (lprint_gross) THEN
                     WRITE(6,'(3A,2I10,2I3)')'GROSS ERROR ',expname(o),	&
                     ' station:',hir(i)%stnr,wdate,wtime,fclen(n)
                     WRITE(6,*)'model,obs',diff_prep,obs(i)%o(jj)%val(k),obstype(k)
                  ENDIF

                  gross_error(i,k) = gross_error(i,k) + 1

                  IF(lreject_gross) THEN

                     all_exp_verified = .FALSE.

                     obs(i)%o(jj)%val(k) = err_ind

                     IF (lstat_gen)             stat(i)%s(1,k,tim_ind)%r =      &
                                                stat(i)%s(1,k,tim_ind)%r + 1
                     IF (monthvise)  allstat(i,mon_ind)%s(1,k,tim_ind)%r =  	&
                                     allstat(i,mon_ind)%s(1,k,tim_ind)%r + 1
                     IF (lyearvise) yearstat(i,yer_ind)%s(1,k,tim_ind)%r = 	&
                                    yearstat(i,yer_ind)%s(1,k,tim_ind)%r + 1

                     CYCLE NPARVER_LOOP

                  ENDIF 

                  !
                  ! If not rejected still use it
                  !

                  tmpdiff(o) = diff

                ENDIF

             ENDDO EXP_LOOP

             ! 
             ! Add statistics when we know if all experiments where valid
             ! 

             IF (all_exp_verified) THEN

                ! Update min and max date used

                mindate(i) = MIN(mindate(i),hir(i)%o(j)%date)
                maxdate(i) = MAX(maxdate(i),hir(i)%o(j)%date)


                IF (use_kalman) THEN
                   do_kalman = .TRUE.

                   IF ( k == wp_ind ) THEN
                      IF (ABS(obs(i)%o(jj)%val(k) - err_ind ) > 1.e-6 .AND.   &
                              obs(i)%o(jj)%val(k) < 0.5                     ) &
                              do_kalman = .FALSE.
                   ENDIF

                   IF ( do_kalman )                &
                   CALL update_kalman(i,n,k,       &
                        obs(i)%o(jj)%date,         &
                        obs(i)%o(jj)%time,         &
                        obs(i)%o(jj)%val(k),tmpdiff)

                   IF ( k == wp_ind ) CALL correct_windp(obs(i)%stnr,nexp,   &
                                           obs(i)%o(jj)%val(k),tmpdiff)

                ENDIF


                !
                ! Store data for scatter and frequency plotting
                !

                IF (lstat_gen .AND. (lplot_scat .OR. lplot_freq)) THEN


                   all_par_active(i,k) = 1

                   scat_data(k)%n = scat_data(k)%n + 1
                               oo = scat_data(k)%n

                   IF ( oo > MAXVAL(UBOUND(scat_data(k)%dat)) ) THEN
                      WRITE(6,*)' oo is larger than scat',oo,MAXVAL(UBOUND(scat_data(k)%dat))
                      WRITE(6,*)' increase maxtim_scat'
                      CALL abort
                   ENDIF
                   scat_data(k)%dat(:,oo) = (/obs(i)%o(jj)%val(k),tmpdiff/)

                ENDIF

                IF (monthvise .AND. (lplot_scat .OR. lplot_freq)) THEN

                        mon_par_active(i,k,mon_ind) = 1

                        mon_scat_data(k,mon_ind)%n = mon_scat_data(k,mon_ind)%n + 1
                                                oo = mon_scat_data(k,mon_ind)%n

                    IF ( oo > MAXVAL(UBOUND(mon_scat_data(k,mon_ind)%dat)) ) THEN
                      WRITE(6,*)' oo is larger than scat month',oo,MAXVAL(UBOUND(mon_scat_data(k,mon_ind)%dat))
                      WRITE(6,*)' increase maxtim_scat'
                      CALL abort
                   ENDIF
                   mon_scat_data(k,mon_ind)%dat(:,oo) = (/obs(i)%o(jj)%val(k),tmpdiff/)
                ENDIF

                IF (lyearvise .AND. (lplot_scat .OR. lplot_freq)) THEN

                        yer_par_active(i,k,mon_ind) = 1

                        yer_scat_data(k,yer_ind)%n = yer_scat_data(k,yer_ind)%n + 1
                                                oo = yer_scat_data(k,yer_ind)%n

                    IF ( oo > MAXVAL(UBOUND(yer_scat_data(k,yer_ind)%dat)) ) THEN
                      WRITE(6,*)' oo is larger than scat year',oo,MAXVAL(UBOUND(yer_scat_data(k,yer_ind)%dat))
                      WRITE(6,*)' increase maxtim_scat'
                      CALL abort
                   ENDIF
                   yer_scat_data(k,yer_ind)%dat(:,oo) = (/obs(i)%o(jj)%val(k),tmpdiff/)
                ENDIF


                ! 
                ! Add timeserie statistics
                ! 

                IF ( (ltimeserie_stat .OR. ltimeserie_stat_month)  &
                    .AND. (lallstat .OR. leach_station )         &
                    .AND. ( ANY(time_stat_fclen  == fclen(n))    &
                     .OR.   time_stat_fclen_diff == -1      )    & 
                ) THEN


                       IF (ltimeserie_stat       )     tim_par_active(        i,k) = 1
                       IF (ltimeserie_stat_month ) mon_tim_par_active(mon_ind,i,k) = 1

                    this_stat_time = -1
                    DO oo=time_stat_max,1,-1
                       IF (time_stat(oo)%date == obs(i)%o(jj)%date .AND.   &
                           time_stat(oo)%time == obs(i)%o(jj)%time ) THEN
                           this_stat_time = oo
                           EXIT
                       ENDIF
                    ENDDO

                    IF ( this_stat_time == -1 ) THEN

                       time_stat_max = time_stat_max + 1
                       oo = time_stat_max

                       time_stat(oo)%date      = obs(i)%o(jj)%date
                       time_stat(oo)%time      = obs(i)%o(jj)%time
                       IF ( lprint_verif ) WRITE(6,*)   &
                       'ADDED TIMESTAT MONTH',obs(i)%o(jj)%date,obs(i)%o(jj)%time

                    ELSE
                       oo = this_stat_time
                    ENDIF

                    time_stat(oo)%obs(k)    = time_stat(oo)%obs(k)    + &
                                              obs(i)%o(jj)%val(k)
                    time_stat(oo)%bias(:,k) = time_stat(oo)%bias(:,k) + tmpdiff
                    time_stat(oo)%rmse(:,k) = time_stat(oo)%rmse(:,k) + tmpdiff**2
                    time_stat(oo)%n(:,k)    = time_stat(oo)%n(:,k)    + 1

                ENDIF


                !
                ! The main verification
                !

                IF ( lstat_gen ) THEN
                  stat(i)%par_active(k) = 1
                  DO o=1,nexp
                     CALL add_stat(stat(i)%s(o,k,tim_ind),   &
                                   obs(i)%o(jj)%val(k),      &
                                   tmpdiff(o))
                  ENDDO 
                ENDIF

                !
                ! The monthly verification
                !
                IF (monthvise) THEN
                   allstat(i,mon_ind)%par_active(k) = 1
                   DO o=1,nexp
                     CALL add_stat(allstat(i,mon_ind)%s(o,k,tim_ind), &
                                   obs(i)%o(jj)%val(k),               &
                                   tmpdiff(o))
                   ENDDO 
                ENDIF


                !
                ! The seasonal verification
                !
                IF (lyearvise) THEN
                   yearstat(i,yer_ind)%par_active(k) = 1
                   DO o=1,nexp
                     CALL add_stat(yearstat(i,yer_ind)%s(o,k,tim_ind),&
                                   obs(i)%o(jj)%val(k),               &
                                   tmpdiff(o))
                   ENDDO 
                ENDIF

             ENDIF

          ENDDO NPARVER_LOOP

          IF (lprint_verif) WRITE(6,*)'DONE NPARVER_LOOP'

          CYCLE FC_CYCLE

       ENDIF OBS_TEST

       ENDDO JJ_CYCLE

       IF (lprint_verif) WRITE(6,*)'JJSTART ', jjstart

     ENDDO FC_CYCLE

     IF(found_right_time) THEN
        IF (lprint_verif) WRITE(6,*)'JJCHECK',jjcheck

        jjstart = MINVAL(jjcheck) 
        jjstart = MAX(jjstart,1) 

        IF (lprint_verif) WRITE(6,*)'CHANGED FROM JJCHECK ',jjstart

     ENDIF

    ENDDO J_CYCLE

   IF ( release_memory ) THEN

      IF (lprint_verif) WRITE(6,*)'Released memory for ',hir(i)%stnr
      DEALLOCATE(obs(i)%o,hir(i)%o)
  
   ENDIF

   IF ((ltimeserie_stat .OR. ltimeserie_stat_month) .AND.(lallstat .OR. leach_station )) THEN

     !
     ! Plot statistics for this station if requested
     !

     IF ( ltimeserie_stat .AND. leach_station .AND. time_stat_max /= 0 )     &
     CALL plot_p_stat(lunout,time_stat_max,nparver,   &
          obs(i)%stnr,nrun,time_stat(1:time_stat_max),par_active,0,0)

     IF ( ltimeserie_stat_month .AND.          &
          leach_station         .AND.          &
          time_stat_max /= 0          ) THEN
          DO l=1,maxmon
             CALL plot_p_stat(lunout,time_stat_max,nparver,         &
                  obs(i)%stnr,nrun,time_stat(1:time_stat_max),      &
                  par_active,months(l),months(l+1))
          ENDDO
     ENDIF

     IF (lallstat) THEN

       !
       ! Add the single station timeserie statistics 
       ! to the all station timeserie
       !

       
       IF (time_stat_max > 0 ) all_time_stat_active = all_time_stat_active + 1

       this_stat_time = 1
       ALL_TIME_STAT_LOOP : DO o = 1,all_time_stat_max
           TIME_STAT_LOOP : DO oo = this_stat_time,time_stat_max
            IF ( all_time_stat(o)%date == time_stat(oo)%date .AND.    &
                 all_time_stat(o)%time == time_stat(oo)%time       )THEN

              all_time_stat(o)%obs  = all_time_stat(o)%obs  + time_stat(oo)%obs  
              all_time_stat(o)%bias = all_time_stat(o)%bias + time_stat(oo)%bias 
              all_time_stat(o)%rmse = all_time_stat(o)%rmse + time_stat(oo)%rmse 
              all_time_stat(o)%n    = all_time_stat(o)%n    + time_stat(oo)%n    
              all_time_stat(o)%r    = all_time_stat(o)%r    + time_stat(oo)%r    
              this_stat_time = oo + 1
              EXIT TIME_STAT_LOOP 

            ELSEIF ( all_time_stat(o)%date <  time_stat(oo)%date ) THEN
              EXIT
            ELSEIF ( all_time_stat(o)%date >  time_stat(oo)%date ) THEN
              EXIT ALL_TIME_STAT_LOOP
            ELSE
              EXIT TIME_STAT_LOOP
            ENDIF

         ENDDO TIME_STAT_LOOP
       ENDDO ALL_TIME_STAT_LOOP

     ENDIF

     ! Clear
     DO o=1,all_time_stat_max
         time_stat(o)%obs  = 0.
         time_stat(o)%bias = 0.
         time_stat(o)%rmse = 0.
         time_stat(o)%n    = 0
         time_stat(o)%r    = 0
         time_stat(o)%date = 0
         time_stat(o)%time = 0
     ENDDO

     time_stat_max = 0

   ENDIF

   IF (lstat_gen .AND. (lplot_scat .OR. lplot_freq)) THEN 

      !
      ! Plot scatter and frequency plots for a single stations
      !
      
      IF ( leach_station ) THEN
         IF ( lplot_scat ) THEN
         CALL plot_scat_diff_new(lunout,nparver,obs(i)%stnr,nrun,0,scat_data,mindate(i),maxdate(i),par_active)
         IF ( diff_ind /= 0 ) &
         CALL plot_scat_comp(lunout,nparver,obs(i)%stnr,nrun,diff_ind,scat_data,mindate(i),maxdate(i),par_active)
         IF ( comp_ind /= 0 ) &
         CALL plot_scat_comp(lunout,nparver,obs(i)%stnr,nrun,comp_ind,scat_data,mindate(i),maxdate(i),par_active)
         IF (ldiff) &
         CALL plot_scat_diff_new(lunout,nparver,obs(i)%stnr,nrun,-1,scat_data,mindate(i),maxdate(i),par_active)
         ENDIF

         IF ( lplot_freq .AND. leach_station)        &
         CALL plot_freq_new(lunout,nparver,obs(i)%stnr,nrun,scat_data,mindate(i),maxdate(i),par_active)

      ENDIF

      IF (lallstat) THEN

         !
         ! Accumulate for all stations statistics
         !

         DO o=1,nparver
            ii =     scat_data(o)%n
            jj = all_scat_data(o)%n
                 all_scat_data(o)%dat(:,jj+1:jj+ii) = &
                     scat_data(o)%dat(:,1:ii)
   
                 all_scat_data(o)%n = jj + ii 
         ENDDO
      ENDIF

      scat_data%n = 0

   ENDIF

   IF (monthvise .AND. (lplot_scat .OR. lplot_freq)) THEN 

      !
      ! Plot monthvise scatter and frequency plots for a single stations
      !

      doing_monthvise = .TRUE.

      DO l=1,maxmon

         IF ( lplot_scat .AND. leach_station) THEN
            CALL plot_scat_diff_new( &
            lunout,nparver,obs(i)%stnr,nrun,0,mon_scat_data(:,l),months(l),months(l+1),par_active)
            IF ( diff_ind /= 0 ) &
            CALL plot_scat_comp(lunout,nparver,obs(i)%stnr,nrun,diff_ind,   &
                 mon_scat_data(:,l),months(l),months(l+1),par_active)
            IF ( comp_ind /= 0 ) &
            CALL plot_scat_comp(lunout,nparver,obs(i)%stnr,nrun,comp_ind,   &
                 mon_scat_data(:,l),months(l),months(l+1),par_active)
            IF (ldiff) &
            CALL plot_scat_diff_new( &
            lunout,nparver,obs(i)%stnr,nrun,-1,mon_scat_data(:,l),months(l),months(l+1),par_active)
         ENDIF

         IF ( lplot_freq .AND. leach_station )        &
         CALL plot_freq_new(lunout,nparver,obs(i)%stnr,nrun, &
                            mon_scat_data(:,l),    &
                            months(l),months(l+1),par_active)

      ENDDO

      IF (lallstat) THEN

         !
         ! Accumulate for all stations statistics
         !

         DO l=1,maxmon
            DO o=1,nparver
               ii =     mon_scat_data(o,l)%n
               jj = all_mon_scat_data(o,l)%n
                    all_mon_scat_data(o,l)%dat(:,jj+1:jj+ii) = &
                        mon_scat_data(o,l)%dat(:,1:ii)
                    all_mon_scat_data(o,l)%n = jj + ii 
            ENDDO
         ENDDO

      ENDIF

      mon_scat_data%n = 0

   ENDIF

 ENDDO STATION_CYCLE

 IF (ltiming) CALL add_timing(timing_id_loop,'Verify_loop')

 timing_id_plot = 0
 IF (ltiming) CALL add_timing(timing_id_plot,'Verify_plot')

 !
 ! Printout timeserie statistics all stations
 !

 IF ( ltimeserie_stat .AND.(lallstat .OR. leach_station )) THEN

   DEALLOCATE(time_stat)

   IF ( lallstat) THEN
     DO j=1,nparver
        par_active(j) = SUM(tim_par_active(:,j))
     ENDDO
     CALL plot_p_stat(lunout,all_time_stat_max,nparver,     &
          0,nrun,all_time_stat,par_active,0,0)

     IF ( .NOT. ltimeserie_stat_month ) THEN
        DEALLOCATE(all_time_stat)
        DEALLOCATE(tim_par_active)
     ENDIF
   ENDIF

 ENDIF


 !
 ! Printout overall statistics
 !

 IF (lstat_gen) THEN

    remember         = lplot_stat_month
    lplot_stat_month = .FALSE.
    CALL do_stat(mindate,maxdate)
    lplot_stat_month = remember

    IF ((lplot_scat .OR. lplot_freq ) .AND. lallstat) THEN 

      DO j=1,nparver
         DEALLOCATE(scat_data(j)%dat)
         par_active(j) = SUM(all_par_active(:,j))
      ENDDO

      IF ( lplot_scat) THEN
         CALL plot_scat_diff_new(                                 &
         lunout,nparver,0,nrun, 0,all_scat_data,MINVAL(mindate),MAXVAL(maxdate),par_active)

         IF ( diff_ind /= 0 )                                     &
         CALL plot_scat_comp(lunout,nparver,0,nrun,diff_ind,      &
                             all_scat_data,MINVAL(mindate),MAXVAL(maxdate),par_active)
         IF ( comp_ind /= 0 )                                     &
         CALL plot_scat_comp(lunout,nparver,0,nrun,comp_ind,      &
                             all_scat_data,MINVAL(mindate),MAXVAL(maxdate),par_active)
         IF (ldiff)                                               &
         CALL plot_scat_diff_new(lunout,nparver,0,nrun,-1,        &
                                 all_scat_data,MINVAL(mindate),MAXVAL(maxdate),par_active)
      ENDIF

      IF ( lplot_freq )        &
      CALL plot_freq_new(lunout,nparver,0,nrun,all_scat_data,MINVAL(mindate),MAXVAL(maxdate),par_active)


      DO j=1,nparver
         DEALLOCATE(all_scat_data(j)%dat)
      ENDDO

      DEALLOCATE(all_par_active)

    ENDIF

 ENDIF
  
 !
 ! Printout statistics month by month
 !

 IF (monthvise) THEN
    IF (lplot_scat .OR. lplot_freq ) THEN
       DO l=1,maxmon
          DO i=1,nparver
             par_active(i) = SUM(mon_par_active(:,i,l))
             DEALLOCATE(mon_scat_data(i,l)%dat)
          ENDDO
       ENDDO
    ENDIF

    doing_monthvise = .TRUE.

    WRITE(lunstat,*)
    WRITE(lunstat,*)'Month by Month'
    WRITE(lunstat,*)

    DO l=1,maxmon

       DO i=1,maxstn
          stat(i)%s          = allstat(i,l)%s
          stat(i)%active     = allstat(i,l)%active
          stat(i)%par_active = allstat(i,l)%par_active
       ENDDO

       mindate = months(l  )
       maxdate = months(l+1)
       CALL do_stat(mindate,maxdate)

       IF (lplot_scat .OR. lplot_freq ) THEN
          DO i=1,nparver
             par_active(i) = SUM(mon_par_active(:,i,l))
          ENDDO
       ENDIF
      
       IF ( lplot_scat) THEN
          CALL plot_scat_diff_new( &
          lunout,nparver,0,nrun, 0,all_mon_scat_data(:,l),mindate,maxdate,par_active)
          IF ( diff_ind /= 0 ) &
          CALL plot_scat_comp(lunout,nparver,0,nrun,diff_ind,   &
               all_mon_scat_data(:,l),mindate,maxdate,par_active)
          IF ( comp_ind /= 0 ) &
          CALL plot_scat_comp(lunout,nparver,0,nrun,comp_ind,   &
               all_mon_scat_data(:,l),mindate,maxdate,par_active)
          IF (ldiff) &
          CALL plot_scat_diff_new( &
          lunout,nparver,0,nrun,-1,all_mon_scat_data(:,l),mindate,maxdate,par_active)
       ENDIF

       IF ( lplot_freq )        &
       CALL plot_freq_new(lunout,nparver,0,nrun, &
                          all_mon_scat_data(:,l),    &
                          mindate,maxdate,par_active)

       IF ( lplot_scat .OR. lplot_freq ) THEN
          DO i=1,nparver
             DEALLOCATE(all_mon_scat_data(i,l)%dat)
          ENDDO
       ENDIF

    ENDDO

    IF ( lplot_scat .OR. lplot_freq ) DEALLOCATE(mon_par_active)
    DEALLOCATE(allstat)

    !
    ! Timeserie statistics in monthvise pieces
    !

    IF ( ltimeserie_stat_month .AND.(lallstat .OR. leach_station )) THEN

       IF ( ALLOCATED(time_stat)) DEALLOCATE(time_stat)

       IF ( lallstat) THEN
          DO l=1,maxmon
             DO j=1,nparver
               par_active(j) = SUM(mon_tim_par_active(l,:,j))
             ENDDO
             CALL plot_p_stat(lunout,all_time_stat_max,nparver,     &
                  0,nrun,all_time_stat,par_active,                  &
                  months(l),months(l+1))
          ENDDO
          DEALLOCATE(all_time_stat)
          DEALLOCATE(mon_tim_par_active)
       ENDIF

    ENDIF

    DEALLOCATE(months)
 
    doing_monthvise = .FALSE.

 ENDIF

 !
 ! Printout statistics for the seasons
 !

 IF(lyearvise) THEN

    lplot_stat_month = lplot_stat_year
    doing_monthvise = .TRUE.

    WRITE(lunstat,*)
    WRITE(lunstat,*)'Seasonal verification'
    WRITE(lunstat,*)

    ALLOCATE(months(lyear))

    IF(lyear.EQ.4 ) months =(/1,4,7,10/) 
    IF(lyear.EQ.12) months =(/1,2,3,4,5,6,7,8,9,10,11,12/)

    DO l=1,lyear

       DO i=1,maxstn
          stat(i)%s          = yearstat(i,l)%s
          stat(i)%active     = yearstat(i,l)%active
          stat(i)%par_active = yearstat(i,l)%par_active
       ENDDO

      
       mindate = year_map(months(l))
       CALL do_stat(mindate,mindate)

       IF ( lplot_scat .OR. lplot_freq ) THEN
          DO i=1,nparver
             par_active(i) = SUM(yer_par_active(:,i,l))
          ENDDO
       ENDIF

       IF ( lplot_scat) THEN
          CALL plot_scat_diff_new( &
          lunout,nparver,0,nrun, 0,yer_scat_data(:,l),mindate,mindate,par_active)
          IF ( diff_ind /= 0 ) &
          CALL plot_scat_comp(lunout,nparver,0,nrun,diff_ind,   &
               yer_scat_data(:,l),mindate,maxdate,par_active)
          IF ( comp_ind /= 0 ) &
          CALL plot_scat_comp(lunout,nparver,0,nrun,comp_ind,   &
               yer_scat_data(:,l),mindate,maxdate,par_active)
          IF (ldiff) &
          CALL plot_scat_diff_new( &
          lunout,nparver,0,nrun,-1,yer_scat_data(:,l),mindate,mindate,par_active)
       ENDIF

       IF ( lplot_freq )        &
       CALL plot_freq_new(lunout,nparver,0,nrun, &
                          yer_scat_data(:,l),mindate,mindate,par_active)

       IF ( lplot_scat .OR. lplot_freq ) THEN
          DO i=1,nparver
             DEALLOCATE(yer_scat_data(i,l)%dat)
          ENDDO
       ENDIF


   ENDDO

   IF ( lplot_scat .OR. lplot_freq ) DEALLOCATE(yer_par_active)
   DEALLOCATE(yearstat)
   DEALLOCATE(months)

   doing_monthvise = .FALSE.

 ENDIF

 DEALLOCATE(stat)

 CLOSE(lunstat)

 IF (ltiming) CALL add_timing(timing_id_plot,'Verify_plot')

 !
 ! Gross error statistics
 !

 jj = 0
 GROSS_LOOP : DO

    gross_pos = MAXLOC(gross_error)

    IF ( jj == obs(gross_pos(1))%stnr .OR.       &
         SUM(gross_error(gross_pos(1),:)) == 0 ) EXIT GROSS_LOOP

    IF ( jj == 0 ) THEN
       WRITE(6,*)
       WRITE(6,*)'GROSS ERROR STATISTICS'
       WRITE(6,*)
    ENDIF

    WRITE(6,*)'Station :',hir(gross_pos(1))%stnr,&
                          hir(gross_pos(1))%lat ,&
                          hir(gross_pos(1))%lon
    DO i=1,nparver
       IF ( gross_error(gross_pos(1),i) > 0 ) &
       WRITE(6,*)'Variable ',OBSTYPE(i),gross_error(gross_pos(1),i)
    ENDDO

    gross_error(gross_pos(1),:) = 0
    jj = obs(gross_pos(1))%stnr

 ENDDO GROSS_LOOP
 DEALLOCATE(gross_error)

 !IF (use_kalman) CALL clear_kalman

 IF (ltiming) CALL add_timing(timing_id,'Verify')

 RETURN
END SUBROUTINE verify
