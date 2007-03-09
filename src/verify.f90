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
 USE scatter
 USE timeserie
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
            maxper,                                     &
            timing_id,                                  &
            timing_id_init,                             &
            timing_id_loop,                             &
            timing_id_plot,                             &
            ind_pe(nfclengths),                         &
            lyear,year_map(12) = 0,                     &
            cdate,ctime,                                &
            nexp_ver,                                   &
            par_active(nparver),                        &
            len_scat,per_ind

 INTEGER, ALLOCATABLE :: periods(:)

 REAL              :: diff,diff_prep
 REAL, ALLOCATABLE :: tmpdiff(:)

 LOGICAL :: all_exp_verified = .TRUE.
 LOGICAL :: found_right_time = .FALSE.
 LOGICAL :: demand_equal     = .TRUE.
 LOGICAL :: do_kalman        = .TRUE.
 LOGICAL :: lscat_array      = .FALSE.

 CHARACTER(LEN=50) :: fname = '',cmon=''

 TYPE (statpar), ALLOCATABLE :: allstat(:,:)

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
 ! Find PE index locations
 !

 ind_pe = 0
 DO i=1,nfclengths
    IF(fclen(i) < pe_interval) CYCLE 
    ind_pe(i)=TRANSFER(MINLOC(ABS(fclen(1:nfclengths)-(fclen(i)-pe_interval))),ii)
    IF (fclen(i)-fclen(ind_pe(i)) < pe_interval ) ind_pe(i) = 0
 ENDDO


 ! Set min and max date
 mindate = 90000101
 maxdate = 0


 !
 ! Set some parameters depending on the 
 ! type of period we are working on
 !

 SELECT CASE(period_type)

 CASE(1)

    maxper = 1
    ALLOCATE(periods(maxper+1))
    periods = 0

    ii = get_maxtim(sdate,edate_obs,MIN(timdiff,fcint))
    ii = ii * timdiff * nfclengths / fcint
    len_scat = MAX(ii,maxtim_scat)


 CASE(2)

    maxper = get_maxmon(sdate,edate) 
    i = 0
    IF ( MOD(maxper,period_freq) /= 0 ) i = 1
    maxper = maxper / period_freq + i
 
    ALLOCATE(periods(maxper+1))

    periods(1) = sdate/100
    DO i=2,maxper+1
       periods(i) = monincr(periods(i-1),period_freq)
    ENDDO

    ii = 31*period_freq*24/MIN(timdiff,fcint)
    ii = ii * timdiff * nfclengths / fcint
    len_scat = MAX(ii,maxtim_scat)

 CASE(3)

    SELECT CASE(period_freq)
    CASE(1)
       ! Month by month
       year_map =(/1,2,3,4,5,6,7,8,9,10,11,12/)
       maxper = 12
       ALLOCATE(periods(maxper+1))
       periods = (/1,2,3,4,5,6,7,8,9,10,11,12,0/)
    CASE(3)
       ! Seasons DJF,MAM,JJA,SON
       year_map =(/1,1,2,2,2,3,3,3,4,4,4,1/)
       maxper = 4
       ALLOCATE(periods(maxper+1))
       periods = (/1,2,3,4,0/)
    CASE DEFAULT
       WRITE(6,*)'period_freq should be 1 or 3, changed to 3'
       period_freq = 3
       year_map =(/1,1,2,2,2,3,3,3,4,4,4,1/)
       maxper = 4
       ALLOCATE(periods(maxper+1))
       periods = (/1,2,3,4,0/)
    END SELECT

    ii = 31*period_freq*24/MIN(timdiff,fcint) * &
         (edate/10000 - sdate/10000 +1)
    ii = ii * timdiff * nfclengths / fcint
    len_scat = MAX(ii,maxtim_scat)

 CASE DEFAULT
    WRITE(6,*)'No such period_type defined',period_type
    CALL abort
 END SELECT

 doing_monthvise = ( period_type == 2 .OR. period_type == 3 )


 IF (lstat_gen) THEN

    !
    ! Allocate the array for general statistics
    ! by fclengh or time of day
    !

    ALLOCATE(allstat(maxstn,maxper))

    DO j=1,maxper
    DO i=1,maxstn
       ALLOCATE(allstat(i,j)%s(nexp,nparver,ntimver))
       ALLOCATE(allstat(i,j)%par_active(nparver))
    ENDDO
    ENDDO

    DO j = 1,maxper
    DO i = 1,maxstn
       allstat(i,j)%s = statistics(0.,0.,0.,0,0,0.)
       allstat(i,j)%par_active = 0
    ENDDO
    ENDDO
    allstat%active     = .FALSE.


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

 ENDIF


 lscat_array = (lplot_scat .OR. lplot_freq .OR. lprep_xml .OR. lplot_comp )

 IF ( lscat_array ) CALL allocate_scatter(len_scat,maxper)

 !
 ! Allocate array for timeserie statistics
 !

 IF ( ltimeserie_stat ) CALL allocate_timeserie(maxper,timdiff,edate_obs)


 !
 ! Prepare xml file for station statistics
 !

 IF ( lprep_xml ) CALL open_xml(lunxml,maxper,periods)
 
 IF (ltiming) CALL add_timing(timing_id_init,'Verify_init')

 ! --------------------------
 ! End of allocation buisness
 ! --------------------------

 timing_id_loop = 0
 IF (ltiming) CALL add_timing(timing_id_loop,'Verify_loop')


 IF ( lstat_gen ) THEN
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

 ENDIF 


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
       IF (release_memory) DEALLOCATE(obs(i)%o,hir(i)%o)
       CYCLE STATION_CYCLE
    ENDIF

    IF (hir(i)%stnr.NE.obs(i)%stnr) THEN
       WRITE(6,*)'Your stations does not agree',hir(i)%stnr,obs(i)%stnr
       CALL abort
    ENDIF

    IF (lprint_verif) WRITE(6,*)'DO STATION',i,hir(i)%stnr


    !
    ! Check if we should plot this station
    !

    leach_station = .FALSE.
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
       ! Skip this fclength if not all variables are present
       ! and all_var_present is TRUE
       !

       IF ( all_var_present .AND. ANY(ABS( hir(i)%o(j)%nal(:,n,:) - err_ind ) < 1.e-6 ))  CYCLE
       
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

          !
          ! Reject this observation if not all variables are present
          !

          IF ( all_var_present .AND. ANY(ABS( obs(i)%o(jj)%val - err_ind ) < 1.e-6 ))  CYCLE FC_CYCLE

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


          SELECT CASE(period_type) 
          CASE(1)
             per_ind = 1
          CASE(2)
             per_ind = mondiff(sdate/100,hir(i)%o(j)%date/100)/period_freq + 1
          CASE(3)
             per_ind = year_map(MOD(hir(i)%o(j)%date/100,100))
          END SELECT

          IF ( lstat_gen ) allstat(i,per_ind)%active = .TRUE.

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

             IF(ABS(obs(i)%o(jj)%val(k)-err_ind) <= 1.e-6) CYCLE NPARVER_LOOP

             !
             ! All EXP should have data, else do not verify
             !

             DO o=1,nexp_ver
               IF (ABS(hir(i)%o(j)%nal(o,n,k)-err_ind) < 1.e-6) THEN
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

                IF(k == pe_ind) THEN

                   !
                   ! Special for precipitation
                   !

                   IF(fclen(n) == pe_interval) THEN
                      diff_prep = hir(i)%o( j)%nal(o,n,k)
                   ELSEIF(fclen(n) > pe_interval .AND. ind_pe(n) > 0 ) THEN

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

                IF(obstype(k)(1:2)== 'DD'.AND.ABS(diff) > 180.) diff = diff + SIGN(360.,180.-diff)

                !
                ! Store this difference
                !

                tmpdiff(o) = diff

             ENDDO EXP_LOOP

             ! 
             ! Add statistics when we know if all experiments where valid
             ! 

             ALL_EXP_TEST : IF (all_exp_verified) THEN

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

                IF (lscat_array ) THEN

                   !
                   ! Store data for scatter and frequency plotting
                   !

                   all_par_active(per_ind,i,k) = 1

                   scat_data(k,per_ind)%n = scat_data(k,per_ind)%n + 1
                                       oo = scat_data(k,per_ind)%n

                   IF ( oo > MAXVAL(UBOUND(scat_data(k,per_ind)%dat)) ) THEN
                      WRITE(6,*)' oo is larger than scat month',oo,MAXVAL(UBOUND(scat_data(k,per_ind)%dat))
                      WRITE(6,*)' increase maxtim_scat'
                      CALL abort
                   ENDIF
                   scat_data(k,per_ind)%dat(:,oo) = (/obs(i)%o(jj)%val(k),tmpdiff/)
                ENDIF


                ! 
                ! Add timeserie statistics
                ! 

                IF ( ltimeserie_stat  &
                    .AND. ( ANY(time_stat_fclen  == fclen(n))    &
                     .OR.   time_stat_fclen_diff == -1      ))   &

                     CALL add_timeserie(per_ind,i,k,   &
                          obs(i)%o(jj)%date,obs(i)%o(jj)%time,   &
                          nexp,obs(i)%o(jj)%val(k),tmpdiff)


                IF ( lstat_gen ) THEN

                   !
                   ! The main verification
                   !
 
                   allstat(i,per_ind)%par_active(k) = 1
                   DO o=1,nexp
                      CALL add_stat(allstat(i,per_ind)%s(o,k,tim_ind), &
                           obs(i)%o(jj)%val(k),tmpdiff(o))
                   ENDDO 

                ENDIF 


             ENDIF ALL_EXP_TEST

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

   IF ( ltimeserie_stat .AND.(lallstat .OR. leach_station )) THEN

     !
     ! Plot statistics for this station if requested
     !
     IF ( period_type == 1 ) periods = 0

     IF ( time_stat_max /= 0 .AND. leach_station ) THEN
          DO l=1,maxper
             IF ( lprint_timeserie_stat ) &
             CALL print_p_stat(lunout,time_stat_max,nparver,        &
                  obs(i)%stnr,nrun,time_stat(1:time_stat_max),      &
                  par_active,periods(l),periods(l+1))
#ifdef MAGICS
             CALL plot_p_stat(lunout,time_stat_max,nparver,         &
                  obs(i)%stnr,nrun,time_stat(1:time_stat_max),      &
                  par_active,periods(l),periods(l+1))
#endif
          ENDDO
     ENDIF

     IF (lallstat) CALL add_all_time_stat

     ! Clear single station time_stat
     DO o=1,all_time_stat_max
         time_stat(o)%obs  = 0.
         time_stat(o)%bias = 0.
         time_stat(o)%rmse = 0.
         time_stat(o)%n    = 0
         time_stat(o)%date = 0
         time_stat(o)%time = 0
     ENDDO

     time_stat_max = 0

   ENDIF


   IF ( lscat_array ) THEN

      !
      ! Plot scatter and frequency plots for a single stations
      !

      DO l=1,maxper

         IF ( period_type == 1 ) periods = 0

         IF (lprep_xml) CALL prep_xml(lunxml,nparver,obs(i)%stnr,periods(l),scat_data(:,l))

         IF ( period_type == 1 ) THEN
            periods(l)   = mindate(i)
            periods(l+1) = maxdate(i)
         ENDIF

#ifdef MAGICS

         ! Plot normal scatterplot
         IF ( lplot_scat .AND. leach_station)                      &
            CALL plot_scat_comp(lunout,nparver,obs(i)%stnr,nrun,   &
                 scat_data(:,l),periods(l),periods(l+1),par_active,&
                 lplot_scat)

         ! Plot Xcrossplot
         IF ( lplot_comp .AND. leach_station)                      &
            CALL plot_scat_comp(lunout,nparver,obs(i)%stnr,nrun,   &
                 scat_data(:,l),periods(l),periods(l+1),par_active,.FALSE.)

         ! Plot frequencydistribution
         IF ( lplot_freq .AND. leach_station )               &
         CALL plot_freq_new(lunout,nparver,obs(i)%stnr,nrun, &
              scat_data(:,l),                                &
              periods(l),periods(l+1),par_active)

#endif

      ENDDO

      IF (lallstat) THEN

         !
         ! Accumulate for all stations statistics
         !

         DO l=1,maxper
            DO o=1,nparver
               ii =     scat_data(o,l)%n
               jj = all_scat_data(o,l)%n
                    all_scat_data(o,l)%dat(:,jj+1:jj+ii) = &
                        scat_data(o,l)%dat(:,1:ii)
                    all_scat_data(o,l)%n = jj + ii 
            ENDDO
         ENDDO

      ENDIF

      scat_data%n = 0

   ENDIF

 ENDDO STATION_CYCLE

 IF (ltiming) CALL add_timing(timing_id_loop,'Verify_loop')

 timing_id_plot = 0
 IF (ltiming) CALL add_timing(timing_id_plot,'Verify_plot')


 IF ( ltimeserie_stat .AND.(lallstat .OR. leach_station )) THEN

    !
    ! Printout timeserie statistics all stations
    !

    DEALLOCATE(time_stat)

    IF ( lallstat ) THEN

        IF ( period_type == 1 ) periods = 0

        DO l=1,maxper
           DO j=1,nparver
              par_active(j) = SUM(tim_par_active(l,:,j))
           ENDDO
           IF (lprint_timeserie_stat ) &
           CALL print_p_stat(lunout,all_time_stat_max,nparver,0,   &
                nrun,all_time_stat,par_active,periods(l),periods(l+1))
#ifdef MAGICS
           CALL plot_p_stat(lunout,all_time_stat_max,nparver,0,   &
                nrun,all_time_stat,par_active,periods(l),periods(l+1))
#endif
        ENDDO

     DEALLOCATE(all_time_stat)

   ENDIF

   DEALLOCATE(tim_par_active)

 ENDIF


 IF (lstat_gen) THEN

    !
    ! Printout general statistics all stations
    !

    DO l=1,maxper

       DO i=1,maxstn
          stat(i)%s          = allstat(i,l)%s
          stat(i)%active     = allstat(i,l)%active
          stat(i)%par_active = allstat(i,l)%par_active
          stat(i)%active     = allstat(i,l)%active
       ENDDO

       SELECT CASE(period_type)
       CASE(2,3)
          mindate = periods(l  )
          maxdate = periods(l+1)
       END SELECT

       CALL do_stat(mindate,maxdate)

    ENDDO

 ENDIF

 IF ( lscat_array ) THEN

    !
    ! Print or plot scatter statistics
    !

    DO l=1,maxper

       DO i=1,nparver
          par_active(i) = SUM(all_par_active(l,:,i))
       ENDDO

       IF ( period_type == 1 ) THEN
          periods(l)   = MINVAL(mindate)
          periods(l+1) = MAXVAL(maxdate)
       ENDIF


#ifdef MAGICS

       ! Plot normal scatterplot
       IF ( lplot_scat)                              &
       CALL plot_scat_comp(lunout,nparver,0,nrun,    &
            all_scat_data(:,l),                      &
            periods(l),periods(l+1),                 &
            par_active,lplot_scat)

       ! Plot Xcrossplot
       IF ( lplot_comp)                              &
       CALL plot_scat_comp(lunout,nparver,0,nrun,    &
            all_scat_data(:,l),                      &
            periods(l),periods(l+1),                 &
            par_active,.FALSE.)

       ! Plot frequencydistribution
       IF ( lplot_freq )                         &
       CALL plot_freq_new(lunout,nparver,0,nrun, &
            all_scat_data(:,l),                  &
            periods(l),periods(l+1),par_active)

#endif
    ENDDO

    CALL deallocate_scatter(maxper,nparver)

 ENDIF

 CLOSE(lunstat)

 IF ( lprep_xml ) THEN
 
    IF ( period_type == 1 ) periods = 0

    DO j=1,nparver
    DO i=1,maxper
        WRITE(cmon(1:8),'(I8.8)')periods(i)
        fname = TRIM(obstype(j))//'_'//TRIM(cmon)//'.xml'
        OPEN(lunxml,file=fname,POSITION='APPEND')
        WRITE(lunxml,'(A)')'</STAT>'
        CLOSE(lunxml)
    ENDDO
    ENDDO
 ENDIF

 !
 ! Deallocate
 !

 IF ( ALLOCATED(periods))  DEALLOCATE(periods)
 IF ( ALLOCATED(allstat)) DEALLOCATE(allstat)
 IF ( ALLOCATED(stat)) DEALLOCATE(stat)


 IF (ltiming) CALL add_timing(timing_id_plot,'Verify_plot')

 !IF (use_kalman) CALL clear_kalman

 IF (ltiming) CALL add_timing(timing_id,'Verify')

 RETURN
END SUBROUTINE verify
