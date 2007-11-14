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
            ind_pe(nparver,nfclengths),                 &
            nver(nparver),nexp_found

 INTEGER, ALLOCATABLE :: gross_error(:,:),              &
                         total_amount(:,:)

 REAL              :: diff(nexp),diff_prep,             &
                      bias(nparver),rmse(nparver),      &
                      stdv

 LOGICAL :: found_right_time   = .FALSE.
 LOGICAL :: lprint_gross_first = .TRUE.
 LOGICAL :: qc_control(nexp)

 !----------------------------------------------------------

 IF ( print_qc > 0 ) THEN
    WRITE(6,*)
    WRITE(6,*)'--QUALTITY CONTROL--'
    WRITE(6,*)
 ENDIF

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

 IF ( print_qc > 0 ) THEN
    WRITE(6,*)
    DO i=1,nfclengths
       IF(qc_fclen(i) == -1 ) EXIT
       WRITE(6,*)'Quality check forecast length ',qc_fclen(i)
    ENDDO
 ENDIF


 !
 ! Find accumulation index locations
 !

 ind_pe = 0
 DO j=1,nparver
    IF ( accu_int(j) == 0 ) CYCLE
    IF (print_qc>1) WRITE(6,*)obstype(j),accu_int(j)
   DO i=1,nfclengths

    IF ( fclen(i) < accu_int(j) ) CYCLE 

    ind_pe(j,i)=TRANSFER(MINLOC(ABS(fclen(1:nfclengths)-(fclen(i)-accu_int(j)))),ii)

    IF (fclen(i)-fclen(ind_pe(j,i)) < accu_int(j) ) ind_pe(j,i) = 0

    IF (print_qc>1) WRITE(6,*)i,obstype(j),fclen(i),ind_pe(j,i)

   ENDDO
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
    gross_error  = 0

 ENDIF

 ALLOCATE(total_amount(maxstn,nparver))
 total_amount = 0

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
             ! Cycle if fclen should not be used but ONLY if this is
             ! NOT an accumulated value
             !

             IF ( (.NOT.ANY(qc_fclen == fclen(n))) .AND. (accu_int(k) == 0 ) ) CYCLE NPARVER_LOOP

             !
             ! Loop over all variables
             !

             IF(ABS(obs(i)%o(jj)%val(k)-err_ind) < 1.e-6) CYCLE NPARVER_LOOP

             !
             ! All EXP should have data, else do not quality control
             !

             qc_control       = .FALSE.
             diff             = err_ind
             nexp_found       = 0

             EXP_LOOP : DO o=1,nexp

                !IF (ABS(hir(i)%o(j)%nal(o,n,k)-err_ind)<1.e-6) THEN
                   !WRITE(6,*)'Skip '
                         !WRITE(6,*)hir(i)%stnr,hir(i)%o(j)%date,      &
                                   !hir(i)%o(j)%time,fclen(n),         &
                                   !hir(i)%o(j)%nal(o,n,k)
                !ENDIF
               
                IF (ABS(hir(i)%o(j)%nal(o,n,k)-err_ind)<1.e-6) CYCLE EXP_LOOP

                !IF(k == pe_ind) THEN
                 IF(accu_int(k) /= 0) THEN

                   !
                   ! Special for precipitation
                   !

                   IF(fclen(n) == pe_interval) THEN

                !WRITE(6,*)'Use '
                         !WRITE(6,*)hir(i)%stnr,hir(i)%o(j)%date,      &
                                   !hir(i)%o(j)%time,fclen(n),         &
                                   !hir(i)%o(j)%nal(o,n,k)

                      diff_prep = hir(i)%o( j)%nal(o,n,k)

                   ELSEIF(fclen(n) > pe_interval .AND. ind_pe(k,n) > 0 ) THEN

                !IF (ABS(hir(i)%o(j)%nal(o,ind_pe(k,n),k)-err_ind)<1.e-6) THEN
                         !WRITE(6,*)'Skip '
                         !WRITE(6,*)hir(i)%stnr,hir(i)%o(j)%date,      &
                                   !hir(i)%o(j)%time,fclen(n),         &
                                   !hir(i)%o(j)%nal(o,n,k)
                !ENDIF 

                      IF (ABS(hir(i)%o(j)%nal(o,ind_pe(k,n),k)-err_ind)<1.e-6) CYCLE EXP_LOOP

                      !IF ( prin_qc > 2 ) THEN
                         !WRITE(6,*)'Use '
                         !WRITE(6,*)hir(i)%stnr,hir(i)%o(j)%date,      &
                                   !hir(i)%o(j)%time,fclen(n),         &
                                   !hir(i)%o(j)%nal(o,n,k)
                         !WRITE(6,*)hir(i)%stnr,hir(i)%o(j)%date,      &
                                   !hir(i)%o(j)%time,fclen(ind_pe(k,n)), &
                                   !hir(i)%o(j)%nal(o,ind_pe(k,n),k)
                      !ENDIF


                      diff_prep = hir(i)%o(j)%nal(o,n        ,k) - &
                                  hir(i)%o(j)%nal(o,ind_pe(k,n),k)

                      IF (diff_prep < 0.) THEN
                         WRITE(6,*)'Model precipitation is negative',diff_prep
                         WRITE(6,'(2A,I10)')TRIM(expname(o)),' station:',hir(i)%stnr
                         WRITE(6,*)hir(i)%stnr,hir(i)%o(j)%date,      &
                                   hir(i)%o(j)%time,fclen(n),         &
                                   hir(i)%o(j)%nal(o,n,k)
                         WRITE(6,*)hir(i)%stnr,hir(i)%o(j)%date,      &
                                   hir(i)%o(j)%time,fclen(ind_pe(k,n)), &
                                   hir(i)%o(j)%nal(o,ind_pe(k,n),k)

                         CYCLE EXP_LOOP

                      ENDIF

                   ELSE
                      CYCLE EXP_LOOP
                   ENDIF
                ELSE
                   diff_prep=hir(i)%o(j)%nal(o,n,k)
                ENDIF

                diff(o) =  diff_prep - obs(i)%o(jj)%val(k)
            
                !
                ! Wind direction
                !

                 IF(obstype(k)(1:2) == 'DD'.AND.ABS(diff(o)) > 180.) &
                 diff(o) = diff(o) + SIGN(360.,180.-diff(o))

                !
                ! Gross error check
                !

                qc_control(o) = ( ABS(diff(o)) < qc_lim(k) )
                nexp_found    = nexp_found + 1


                IF ( estimate_qc_limit ) THEN

                   !
                   ! Accumulate bias and rmse
                   !
                   
                   nver(k) = nver(k) + 1
                   bias(k) = bias(k) + diff(o)
                   rmse(k) = rmse(k) + diff(o)**2

                ENDIF

             ENDDO EXP_LOOP

             IF ((.NOT. ANY(qc_control).AND. .NOT. estimate_qc_limit )    &
                  .AND. (nexp_found == nexp)) THEN

                !
                ! Reject erroneous observations
                !

                IF (accu_int(k) == 0 ) THEN
                   IF (print_qc > 1 ) THEN
                      WRITE(6,'(A,2I10,2I3)')'GROSS ERROR station:', &
                      hir(i)%stnr,hir(i)%o(j)%date,hir(i)%o(j)%time,fclen(n)
                      WRITE(6,*)obstype(k),qc_lim(k),     &
                      obs(i)%o(jj)%val(k),hir(i)%o(j)%nal(:,n,k)
                   ENDIF
                   gross_error(i,k)    = gross_error(i,k) + 1
                   obs(i)%o(jj)%val(k) = err_ind
                ELSEIF ( (fclen(n) == accu_int(k)).OR. &
                         (fclen(n) >  accu_int(k) .AND.  ind_pe(k,n) > 0 )) THEN
                   IF (print_qc > 1 ) THEN
                      IF (lprint_gross_first ) THEN
                         WRITE(6,'(A)')'GROSS ERROR station: stnr, date, time, fclen'
                         WRITE(6,'(A)')'Obstype, qc limit, obs, model'
                         lprint_gross_first = .FALSE.
                      ENDIF
                      WRITE(6,'(A,2I10,2I3)')'GROSS ERROR station:', &
                      hir(i)%stnr,hir(i)%o(j)%date,hir(i)%o(j)%time,fclen(n)
                      WRITE(6,*)obstype(k),qc_lim(k),     &
                      obs(i)%o(jj)%val(k),obs(i)%o(jj)%val(k)+diff
                   ENDIF
                   gross_error(i,k)    = gross_error(i,k) + 1
                   obs(i)%o(jj)%val(k) = err_ind
                ENDIF
             ENDIF

             total_amount(i,k)    = total_amount(i,k) + 1

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
    WRITE(6,*)' Qualtiy control limits (QC_LIM_SCALE STDV QC_LIM) '
    WRITE(6,*)

    DO k=1,nparver

       IF (ABS(qc_lim (k) - err_ind ) < 1.e-6 ) THEN
          stdv = sqrt(ABS(rmse(k)/MAX(1.,FLOAT(nver(k)))        &
                        -(bias(k)/MAX(1.,FLOAT(nver(k))))**2))
          qc_lim(k) = qc_lim_scale(k) * stdv
       ENDIF

       WRITE(6,*)obstype(k),qc_lim_scale(k),stdv,qc_lim(k)
    ENDDO
    WRITE(6,*)

    estimate_qc_limit = .FALSE.

 ELSE
    IF ( print_qc > 1 ) CALL sumup_gross(gross_error,total_amount)
    DEALLOCATE(gross_error)
 ENDIF
 DEALLOCATE(total_amount)

 RETURN

END SUBROUTINE quality_control
