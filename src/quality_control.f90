SUBROUTINE quality_control

!
! Loop through all observations and check
! the quality against qc_fclen. 
! qc_fclen could preferably be an analysis ( = 0 ).
!
! Ulf Andrae, SMHI, 2007
! 

 USE data
 USE functions

 IMPLICIT NONE

 !
 ! Local
 !

 INTEGER :: i,j,k,l,m,n,o,ii,                           &
            jj,jjstart,jjcheck(nfclengths),             &
            wdate,wtime,                                &
            ind_pe(nfclengths),                         &
            nver(nparver)

 INTEGER, ALLOCATABLE :: gross_error(:,:)

 REAL              :: diff(nexp),diff_prep,             &
                      bias(nparver),rmse(nparver)

 LOGICAL :: all_exp_verified = .TRUE.
 LOGICAL :: found_right_time = .FALSE.
 LOGICAL :: qc_control(nexp) 

 !----------------------------------------------------------

 WRITE(6,*)
 WRITE(6,*)'--QUALTITY CONTROL--'
 WRITE(6,*)

 bias = 0.
 rmse = 0.
 nver = 0

 !
 ! Check qc_flen
 !

 IF ( ALL(qc_fclen == -1) ) THEN

    !
    ! Set qc_fclen as all fc_len <= fcint
    !

    ii = 0
    DO i=1,nfclengths
       IF ( fclen(i) <= fcint ) THEN
          ii = ii + 1
          qc_fclen(ii) = fclen(i)
       ENDIF
    ENDDO

 ENDIF

 WRITE(6,*)
 DO i=1,nfclengths
    IF(qc_fclen(i) == -1 ) EXIT
    WRITE(6,*)'Quality check forecast length ',qc_fclen(i)
 ENDDO
 WRITE(6,*)


 !
 ! Find PE index locations
 !

 ind_pe = 0
 DO i=1,nfclengths
    IF(fclen(i) < pe_interval) CYCLE 
    ind_pe(i)=TRANSFER(MINLOC(ABS(fclen(1:nfclengths)-(fclen(i)-pe_interval))),ii)
    IF (fclen(i)-fclen(ind_pe(i)) < pe_interval ) ind_pe(i) = 0
 ENDDO

 IF ( .NOT. estimate_qc_limit ) THEN

    !
    ! Set up quality control levels
    !

    CALL set_qc_lim

    !
    ! Gross error tracking
    !

    ALLOCATE(gross_error(maxstn,nparver))
    gross_error = 0

 ENDIF

 !
 ! Loop over all stations
 !

 STATION_CYCLE : DO i=1,maxstn

    !
    ! Check if data are available, active and correct
    !

    IF ( .NOT. hir(i)%obs_is_allocated ) THEN
       WRITE(6,*)'Your model data is not allocated '
       CYCLE STATION_CYCLE
    ENDIF

    IF (.NOT.(hir(i)%active.AND.obs(i)%active)) THEN
       CYCLE STATION_CYCLE
    ENDIF

    IF (hir(i)%stnr /= obs(i)%stnr) THEN
       WRITE(6,*)'Your stations does not agree',hir(i)%stnr,obs(i)%stnr
       CALL abort
    ENDIF

    !
    ! Loop over all model times and forecast hours for this observation
    !

    jjstart = 1
    jjcheck = obs(i)%ntim

    J_CYCLE : DO j=1,hir(i)%ntim

     found_right_time = .FALSE.

     FC_CYCLE : DO n=1,nfclengths

       !
       ! Cycle if this fc hour should not be user for quality control
       !

       IF (.NOT. ANY(qc_fclen == fclen(n)) ) CYCLE FC_CYCLE

       !
       ! Step time to verification time 
       ! If we have no observations inside the range then cycle
       !

       CALL adddtg(hir(i)%o(j)%date,hir(i)%o(j)%time*10000,&
                   fclen(n)*3600,wdate,wtime)
       wtime = wtime / 10000

       IF (obs(i)%o(obs(i)%ntim)%date < wdate) CYCLE J_CYCLE

       !
       ! Loop over all observations and all times
       !

       JJ_CYCLE : DO jj=jjstart,obs(i)%ntim

       !
       ! Cycle FC_CYCLE if we have passed the model date
       !

       IF (obs(i)%o(jj)%date > wdate) CYCLE FC_CYCLE

       OBS_TEST :				&
       IF(obs(i)%o(jj)%date == wdate .AND.	&
          obs(i)%o(jj)%time == wtime ) THEN

          found_right_time = .TRUE.

          jjcheck(n) = jj

          NPARVER_LOOP : DO k=1,nparver

             !
             ! Loop over all variables
             !

             IF(ABS(obs(i)%o(jj)%val(k)-err_ind) < 1.e-6) CYCLE NPARVER_LOOP

             !
             ! All EXP should have data, else do not quality control
             !
!
!            DO o=1,nexp
!              IF (ABS(hir(i)%o(j)%nal(o,n,k)-err_ind) < 1.e-6) CYCLE NPARVER_LOOP
!            ENDDO

             qc_control       = .FALSE.
             diff             = err_ind
             !all_exp_verified = .TRUE.

             EXP_LOOP : DO o=1,nexp

                IF (ABS(hir(i)%o(j)%nal(o,n,k)-err_ind)<1.e-6) CYCLE EXP_LOOP

                IF(k == pe_ind) THEN

                   !
                   ! Special for precipitation
                   !

                   IF(fclen(n) == pe_interval) THEN
                      diff_prep = hir(i)%o( j)%nal(o,n,k)
                   ELSEIF(fclen(n) > pe_interval .AND. ind_pe(n) > 0 ) THEN

                      IF (ABS(hir(i)%o(j)%nal(o,ind_pe(n),k)-err_ind)<1.e-6) CYCLE EXP_LOOP
                         !all_exp_verified = .FALSE.

                      diff_prep = hir(i)%o(j)%nal(o,n        ,k) - &
                                  hir(i)%o(j)%nal(o,ind_pe(n),k)

                      IF (diff_prep < 0.) THEN
                         WRITE(6,*)'Model precipitation is negative',diff_prep
                         WRITE(6,'(2A,2I10,2I3)')expname(o),	&
                         ' station:',hir(i)%stnr,wdate,wtime,fclen(n)
                         CYCLE EXP_LOOP
                      ENDIF

                   ELSE
!                     all_exp_verified = .FALSE.
                      CYCLE EXP_LOOP
                   ENDIF
                ELSE
                   diff_prep=hir(i)%o(j)%nal(o,n,k)
                ENDIF

                diff(o) =  diff_prep - obs(i)%o(jj)%val(k)

                !
                ! Wind direction
                !

                 IF(k == dd_ind.AND.ABS(diff(o)) > 180.) &
                 diff(o) = diff(o) + SIGN(360.,180.-diff(o))

                !
                ! Gross error check
                !

                qc_control(o) = ( ABS(diff(o)) < qc_lim(k) )

                IF ( estimate_qc_limit ) THEN

                   !
                   ! Accumulate bias and rmse
                   !
                   
                   nver(k) = nver(k) + 1
                   bias(k) = bias(k) + diff(o)
                   rmse(k) = rmse(k) + diff(o)**2

                ENDIF

             ENDDO EXP_LOOP

             IF (.NOT. ANY(qc_control).AND. .NOT. estimate_qc_limit) THEN

                !
                ! Reject erroneous observations
                !

                IF (k /= pe_ind ) THEN
                   IF (lprint_gross ) THEN
                      WRITE(6,'(A,2I10,2I3)')'GROSS ERROR station:', &
                      hir(i)%stnr,wdate,wtime,fclen(n)
                      WRITE(6,*)obstype(k),qc_lim(k),     &
                      obs(i)%o(jj)%val(k),hir(i)%o(j)%nal(:,n,k)
                   ENDIF
                   gross_error(i,k)    = gross_error(i,k) + 1
                   obs(i)%o(jj)%val(k) = err_ind
                ELSEIF ( (fclen(n) == pe_interval).OR. &
                         (fclen(n) >  pe_interval .AND.  ind_pe(n) > 0 )) THEN
                   IF (lprint_gross ) THEN
                      WRITE(6,'(A,2I10,2I3)')'GROSS ERROR station:', &
                      hir(i)%stnr,wdate,wtime,fclen(n)
                      WRITE(6,*)obstype(k),qc_lim(k),     &
                      obs(i)%o(jj)%val(k),obs(i)%o(jj)%val(k)+diff
                   ENDIF
                   gross_error(i,k)    = gross_error(i,k) + 1
                   obs(i)%o(jj)%val(k) = err_ind
                ENDIF

             ENDIF

          ENDDO NPARVER_LOOP

          CYCLE FC_CYCLE

       ENDIF OBS_TEST

       ENDDO JJ_CYCLE

     ENDDO FC_CYCLE

     IF(found_right_time) THEN

        jjstart = MINVAL(jjcheck) 
        jjstart = MAX(jjstart,1) 

     ENDIF

    ENDDO J_CYCLE

 ENDDO STATION_CYCLE


 IF ( estimate_qc_limit ) THEN

    WRITE(6,*)
    WRITE(6,*)'Qualtiy control limits given by', qc_lim_scale,' times the STDV'

    DO k=1,nparver

       IF (ABS(qc_lim (k) - err_ind ) < 1.e-6 )       &
       qc_lim(k) = qc_lim_scale *                     &
       sqrt(ABS(rmse(k)/MAX(1.,FLOAT(nver(k)))        &
              -(bias(k)/MAX(1.,FLOAT(nver(o))))**2))

       WRITE(6,*)obstype(k),qc_lim(k)
    ENDDO
    WRITE(6,*)

    estimate_qc_limit = .FALSE.

 ELSE
    CALL sumup_gross(gross_error)
    DEALLOCATE(gross_error)
 ENDIF

 RETURN

END SUBROUTINE quality_control
