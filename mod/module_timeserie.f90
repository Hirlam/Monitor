MODULE timeserie

 USE types

 IMPLICIT NONE

 SAVE

 INTEGER :: time_stat_max,         &
        all_time_stat_max,         &
        all_time_stat_active,      &
        time_stat_fclen_diff,      &
        last_time

 INTEGER, ALLOCATABLE :: tim_par_active(:,:,:)

 ! Switch for internal stdv and skewness
 LOGICAL :: do_vs = .FALSE.

 TYPE (stat_obs), ALLOCATABLE :: time_stat(:),all_time_stat(:)

 CONTAINS

 !
 ! -----------------------------------------------
 !

 SUBROUTINE allocate_timeserie(maxper,edate_obs)

  USE data, ONLY : nuse_fclen,use_fclen,nexp,nparver,   &
                   sdate,stime,fcint,lallstat,maxstn,   &
                   obint,show_var,show_skw

  USE functions, ONLY : get_maxtim

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: maxper,edate_obs

  INTEGER :: i,ii,cdate,ctime,wdate,wtime,ierr

  !
  ! Check minimum time_stat_fclen difference
  !

  time_stat_fclen_diff = -1
  IF ( use_fclen(1)         /= -1 ) time_stat_fclen_diff = use_fclen(1)
  IF ( time_stat_fclen_diff ==  0 ) time_stat_fclen_diff = fcint
  time_stat_fclen_diff = MIN(fcint,time_stat_fclen_diff)
  DO i=2,nuse_fclen
     time_stat_fclen_diff = MIN(time_stat_fclen_diff,use_fclen(i) - use_fclen(i-1))
  ENDDO

  !
  ! Allocate array for timeserie statistics
  !

  ii = get_maxtim(sdate,edate_obs,time_stat_fclen_diff)

  ALLOCATE(time_stat(ii),all_time_stat(ii),STAT=ierr)

  IF ( ierr /= 0 ) THEN
     WRITE(6,*)'Could not allocate time_stat data',ierr
  ENDIF

      time_stat_max    = 0
  all_time_stat_max    = 0
  all_time_stat_active = 0

  cdate = sdate
  ctime = stime

  do_vs = ( show_var .OR. show_skw )

  i = 0
  DO 

     i = i + 1

     ALLOCATE(                                 &
              time_stat(i)%obs(nparver),       &            
              time_stat(i)%bias(nexp,nparver), &
              time_stat(i)%rmse(nexp,nparver), &
              time_stat(i)%n(nparver),         &
              time_stat(i)%date,               &
              time_stat(i)%time            )

     time_stat(i)%obs  = 0.
     time_stat(i)%bias = 0.
     time_stat(i)%rmse = 0.
     time_stat(i)%n    = 0
     time_stat(i)%date = cdate
     time_stat(i)%time = ctime

     IF ( do_vs ) THEN

       ALLOCATE(                               &
              time_stat(i)%obs2(nparver),      &
              time_stat(i)%obs3(nparver),      &
              time_stat(i)%s2(nexp,nparver),   &
              time_stat(i)%s3(nexp,nparver))

       time_stat(i)%obs2 = 0.
       time_stat(i)%obs3 = 0.
       time_stat(i)%s2   = 0.
       time_stat(i)%s3   = 0. 

     ENDIF


     IF (lallstat) THEN

        ALLOCATE(                            &
        all_time_stat(i)%obs(nparver),       &            
        all_time_stat(i)%bias(nexp,nparver), &            
        all_time_stat(i)%rmse(nexp,nparver), &
        all_time_stat(i)%n(nparver),         &
        all_time_stat(i)%date,               &
        all_time_stat(i)%time            )

        all_time_stat(i)%date = cdate
        all_time_stat(i)%time = ctime

        all_time_stat(i)%obs  = 0.
        all_time_stat(i)%bias = 0.
        all_time_stat(i)%rmse = 0.
        all_time_stat(i)%n    = 0

        IF ( do_vs ) THEN
           ALLOCATE(                            &
           all_time_stat(i)%obs2(nparver),      &
           all_time_stat(i)%obs3(nparver),      &
           all_time_stat(i)%s2(nexp,nparver),   &
           all_time_stat(i)%s3(nexp,nparver))

           all_time_stat(i)%obs2 = 0.
           all_time_stat(i)%obs3 = 0.
           all_time_stat(i)%s2   = 0.
           all_time_stat(i)%s3   = 0. 

        ENDIF

     ENDIF

     CALL adddtg(cdate,ctime*10000,time_stat_fclen_diff*3600,wdate,wtime)

     wtime = wtime / 10000
     cdate = wdate
     ctime = wtime

     IF ( cdate > edate_obs ) EXIT

  ENDDO

      time_stat_max = i
  all_time_stat_max = i

     last_time = 0

  ALLOCATE(tim_par_active(maxper,maxstn,nparver))
  tim_par_active = 0

  ! Nullify unused pointers

  DO i=all_time_stat_max+1,ii

     NULLIFY(                   &
        time_stat(i)%obs,       &
        time_stat(i)%bias,      &
        time_stat(i)%rmse,      &
        time_stat(i)%n,         &
        time_stat(i)%date,      &
        time_stat(i)%time)

     NULLIFY(                   &
        all_time_stat(i)%obs,   &
        all_time_stat(i)%bias,  &
        all_time_stat(i)%rmse,  &
        all_time_stat(i)%n,     &
        all_time_stat(i)%date,  &
        all_time_stat(i)%time)

     IF ( do_vs ) THEN

        NULLIFY(                   &
           time_stat(i)%obs2,      &
           time_stat(i)%obs3,      &
           time_stat(i)%s2,        &
           time_stat(i)%s3,        &
           all_time_stat(i)%obs2,  &
           all_time_stat(i)%obs3,  &
           all_time_stat(i)%s2,    &
           all_time_stat(i)%s3)
         
     ENDIF

  ENDDO

  RETURN

 END SUBROUTINE allocate_timeserie

 !
 ! --------------------------------------------------------
 !

 SUBROUTINE add_timeserie(per_ind,station_ind,par_ind,   &
                          date,time,nexp,obs_data,exp_diff)

  IMPLICIT NONE

  ! Input
  INTEGER, INTENT(IN) :: per_ind,station_ind,par_ind,    &
                         date,time,nexp

  REAL,    INTENT(IN) :: obs_data,exp_diff(nexp)

  ! Local
  INTEGER :: oo,this_stat_time,dhour, &
             is,ie,ii,difdtg
  REAL    :: p_data(nexp)

  !--------------------------------------------------------
  
  ! 
  ! Add timeserie statistics
  ! 

  this_stat_time = -1
  oo = MAX(1,last_time)
  is = 1
  ie = 1
  ii = 1

  dhour = difdtg(time_stat(oo)%date,       &
                 time_stat(oo)%time*10000, &
                 date, 10000*time)/3600

  !
  ! Select search direction
  !
  SELECT CASE(dhour)
  CASE(0)
     this_stat_time = last_time
  CASE(1:)
    is = last_time + 1
    ie = time_stat_max
    ii = 1
  CASE(:-1)
    is = last_time - 1
    ie = 1
    ii = -1

    IF ( is < 0 ) THEN
       WRITE(6,*)'Stupid programmer in add_timeserie'
       CALL abort
    ENDIF

  CASE DEFAULT 
    CALL abort
  END SELECT

  IF ( this_stat_time == -1 ) THEN
    DO oo=is,ie,ii
     IF (time_stat(oo)%date == date .AND.   &
         time_stat(oo)%time == time ) THEN
         this_stat_time = oo
         EXIT
     ENDIF
    ENDDO
  ELSEIF ( this_stat_time == 0 ) THEN
    this_stat_time = 1
    oo = 1
  ENDIF

  IF ( this_stat_time == -1 ) THEN

     !
     ! No corresponding date was found, what went wrong?
     !

     WRITE(6,*)'Search',date,time,dhour
     DO oo=1,time_stat_max    
        WRITE(6,*)oo,time_stat(oo)%date,time_stat(oo)%time
     ENDDO

     CALL abort

  ELSE
     oo = this_stat_time
  ENDIF

  p_data = exp_diff + obs_data
  time_stat(oo)%obs(par_ind)    = time_stat(oo)%obs(par_ind)    + obs_data
  time_stat(oo)%bias(:,par_ind) = time_stat(oo)%bias(:,par_ind) + exp_diff
  time_stat(oo)%rmse(:,par_ind) = time_stat(oo)%rmse(:,par_ind) + exp_diff**2
  time_stat(oo)%n(par_ind)      = time_stat(oo)%n(par_ind) + 1

  IF ( do_vs ) THEN
     time_stat(oo)%obs2(par_ind)   = time_stat(oo)%obs2(par_ind)   + obs_data**2
     time_stat(oo)%obs3(par_ind)   = time_stat(oo)%obs3(par_ind)   + obs_data**3
     time_stat(oo)%s2(:,par_ind)   = time_stat(oo)%s2(:,par_ind)   + p_data**2
     time_stat(oo)%s3(:,par_ind)   = time_stat(oo)%s3(:,par_ind)   + p_data**3
  ENDIF

  tim_par_active(per_ind,station_ind,par_ind) = 1

  last_time = oo

 END SUBROUTINE add_timeserie

 !
 ! --------------------------------------------------------
 !

 SUBROUTINE add_all_time_stat

  IMPLICIT NONE

  INTEGER :: o

       !
       ! Add the single station timeserie statistics 
       ! to the all station timeserie
       !
       
       IF (last_time > 0 ) all_time_stat_active = all_time_stat_active + 1

       DO o = 1,all_time_stat_max
         all_time_stat(o)%obs  = all_time_stat(o)%obs  + time_stat(o)%obs  
         all_time_stat(o)%bias = all_time_stat(o)%bias + time_stat(o)%bias 
         all_time_stat(o)%rmse = all_time_stat(o)%rmse + time_stat(o)%rmse 
         all_time_stat(o)%n    = all_time_stat(o)%n    + time_stat(o)%n    
       ENDDO

       IF ( do_vs ) THEN
         DO o = 1,all_time_stat_max
              all_time_stat(o)%obs2 = all_time_stat(o)%obs2 + time_stat(o)%obs2
              all_time_stat(o)%obs3 = all_time_stat(o)%obs3 + time_stat(o)%obs3 
              all_time_stat(o)%s2   = all_time_stat(o)%s2   + time_stat(o)%s2
              all_time_stat(o)%s3   = all_time_stat(o)%s3   + time_stat(o)%s3
         ENDDO
       ENDIF

 END SUBROUTINE add_all_time_stat

 !
 ! --------------------------------------------------------
 !

 SUBROUTINE clear_timeserie

    IMPLICIT NONE

    INTEGER :: i

    IF ( ALLOCATED(time_stat) ) THEN
  
       DO i=1,SIZE(time_stat)

          IF ( .NOT. ASSOCIATED(time_stat(i)%date) ) CYCLE
     
          DEALLOCATE(time_stat(i)%obs,     &
                     time_stat(i)%bias,    &
                     time_stat(i)%rmse,    &
                     time_stat(i)%n,       &
                     time_stat(i)%date,    &
                     time_stat(i)%time)

          IF ( do_vs ) THEN
             DEALLOCATE(                   &
                     time_stat(i)%obs2,    &
                     time_stat(i)%obs3,    &
                     time_stat(i)%s2,      &
                     time_stat(i)%s3)
          ENDIF

       ENDDO

       DEALLOCATE(time_stat)

    ENDIF

    IF ( ALLOCATED(all_time_stat) ) THEN
  
       DO i=1,SIZE(all_time_stat)

          IF ( .NOT. ASSOCIATED(all_time_stat(i)%date) ) CYCLE

          DEALLOCATE(all_time_stat(i)%obs,     &
                     all_time_stat(i)%bias,    &
                     all_time_stat(i)%rmse,    &
                     all_time_stat(i)%n,       &
                     all_time_stat(i)%date,    &
                     all_time_stat(i)%time)

          IF ( do_vs ) THEN
             DEALLOCATE(                       &
                     all_time_stat(i)%obs2,    &
                     all_time_stat(i)%obs3,    &
                     all_time_stat(i)%s2,      &
                     all_time_stat(i)%s3)
          ENDIF

       ENDDO

       DEALLOCATE(all_time_stat)

    ENDIF

    IF ( ALLOCATED(tim_par_active) ) DEALLOCATE(tim_par_active)

 END SUBROUTINE clear_timeserie

 !
 ! --------------------------------------------------------
 !

 SUBROUTINE clear_single_time_stat

   ! Clear single station time_stat

   IMPLICIT NONE

   INTEGER :: o

   DO o=1,time_stat_max
      time_stat(o)%obs  = 0.
      time_stat(o)%bias = 0.
      time_stat(o)%rmse = 0.
      time_stat(o)%n    = 0
   ENDDO
   IF ( do_vs ) THEN
      DO o=1,time_stat_max
         time_stat(o)%obs2 = 0.
         time_stat(o)%obs3 = 0.
         time_stat(o)%s2   = 0.
         time_stat(o)%s3   = 0.
      ENDDO
   ENDIF

   last_time = 0

 END SUBROUTINE clear_single_time_stat

END MODULE timeserie
