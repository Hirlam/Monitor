MODULE sign_data

 !
 ! Structure to take care of accumulation of data 
 ! for significance test. Might be useful for other things,
 ! who knows?
 !
 ! Ulf Andrae, SMHI, 2010
 !

 USE types

 IMPLICIT NONE

 SAVE

 INTEGER :: sign_stat_max,          &
            sign_stat_last

 TYPE (sign_type), ALLOCATABLE :: sign_stat(:),all_sign_stat(:)

 LOGICAL :: lall_sign_stat

 CONTAINS

 !
 ! -----------------------------------------------
 !

 SUBROUTINE allocate_sign_data

  USE data, ONLY : nuse_fclen,nexp,nparver,   &
                   nini_hours,ini_hours,      &
                   sdate,stime,edate,etime,   &
                   fcint,lallstat,            &
                   sign_time_diff

  USE functions, ONLY : get_maxtim

  IMPLICIT NONE

  INTEGER :: i,j,n,ii,                   &
             cdate,ctime,wdate,wtime,    &
             sign_time

  !
  ! Allocate array for significance statistics
  !

  IF ( sign_time_diff == -1 ) THEN
     sign_time= fcint
  ELSE
     sign_time= sign_time_diff*24
  ENDIF
  ii = get_maxtim(sdate,edate,sign_time)

  ! Determine substepping
  IF ( sign_time < 24 ) THEN
    n = 0
  ELSE
    IF ( fcint > 24 ) CALL abort
    n = nini_hours
    ii = ii * n
  ENDIF

  ALLOCATE(sign_stat(ii))

  IF ( lallstat ) THEN
     ALLOCATE(all_sign_stat(ii))
     lall_sign_stat = .TRUE.
  ELSE
     lall_sign_stat = .FALSE.
  ENDIF

  cdate = sdate
  ctime = 0

  i = 0

  DO 

     IF ( sign_time < 24 ) THEN

        i = i + 1
        ALLOCATE(sign_stat(i)%n(nexp,nuse_fclen,nparver),   &
                 sign_stat(i)%r(nexp,nuse_fclen,nparver))
        sign_stat(i)%date = cdate
        sign_stat(i)%time = ctime
        sign_stat(i)%n    = 0
        sign_stat(i)%r    = 0.

        IF (lall_sign_stat) THEN

           ALLOCATE(all_sign_stat(i)%n(nexp,nuse_fclen,nparver),   &
                    all_sign_stat(i)%r(nexp,nuse_fclen,nparver))
           all_sign_stat(i)%date  = cdate
           all_sign_stat(i)%time  = ctime
           all_sign_stat(i)%n     = 0
           all_sign_stat(i)%r     = 0.

        ENDIF

     ELSE

        DO j=1,n
           i = i + 1
           ALLOCATE(sign_stat(i)%n(nexp,nuse_fclen,nparver),   &
                    sign_stat(i)%r(nexp,nuse_fclen,nparver))
           sign_stat(i)%date = cdate
           sign_stat(i)%time = ini_hours(j)
           sign_stat(i)%n    = 0
           sign_stat(i)%r    = 0.
           IF (lall_sign_stat) THEN
              ALLOCATE(all_sign_stat(i)%n(nexp,nuse_fclen,nparver),   &
                       all_sign_stat(i)%r(nexp,nuse_fclen,nparver))
              all_sign_stat(i)%date = cdate
              all_sign_stat(i)%time = ini_hours(j)
              all_sign_stat(i)%n    = 0
              all_sign_stat(i)%r    = 0.
           ENDIF
        ENDDO

     ENDIF

     CALL adddtg(cdate,ctime*10000,sign_time*3600,wdate,wtime)

     wtime = wtime / 10000
     cdate = wdate
     ctime = wtime

     IF ( cdate > edate ) EXIT

  ENDDO

  sign_stat_max  = i
  sign_stat_last = 1

  RETURN

 END SUBROUTINE allocate_sign_data

 !
 ! --------------------------------------------------------
 ! --------------------------------------------------------
 ! --------------------------------------------------------
 !

 SUBROUTINE add_sign_stat(date,time,              &
                          ill,ipar,nexp,mdiff)

  ! 
  ! Add significance statistics
  ! 

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: date,time,               &
                         ill,ipar,nexp

  REAL,    INTENT(IN) :: mdiff(nexp)

  INTEGER :: i,ii

  !--------------------------------------------------------

  !
  ! Find the corresponding date
  !
  
  ii = -1
  DO i=sign_stat_last,sign_stat_max
     IF (sign_stat(i)%date == date .AND.   &
         sign_stat(i)%time == time ) THEN
         ii = i
         EXIT
     ENDIF
  ENDDO

  IF ( ii /= -1 ) THEN

     !
     ! Accumulate RMSE
     !

     sign_stat(ii)%r(:,ill,ipar) = &
     sign_stat(ii)%r(:,ill,ipar) + mdiff(:)**2
     sign_stat(ii)%n(:,ill,ipar) = &
     sign_stat(ii)%n(:,ill,ipar) + 1

  sign_stat_last = ii
  ENDIF

 END SUBROUTINE add_sign_stat

 !
 ! --------------------------------------------------------
 ! --------------------------------------------------------
 ! --------------------------------------------------------
 !

 SUBROUTINE reset_sign_stat

  IMPLICIT NONE

  INTEGER :: i

  DO i=1,sign_stat_max
     sign_stat(i)%r(:,:,:) = 0.
     sign_stat(i)%n(:,:,:) = 0
  ENDDO
  
  sign_stat_last = 1

  RETURN

 END SUBROUTINE reset_sign_stat

 !
 ! --------------------------------------------------------
 ! --------------------------------------------------------
 ! --------------------------------------------------------
 !

 SUBROUTINE add_all_sign_stat

  IMPLICIT NONE

  INTEGER :: i

  DO i=1,sign_stat_max
     all_sign_stat(i)%r(:,:,:) = all_sign_stat(i)%r(:,:,:) + sign_stat(i)%r(:,:,:)
     all_sign_stat(i)%n(:,:,:) = all_sign_stat(i)%n(:,:,:) + sign_stat(i)%n(:,:,:)
  ENDDO
  
  RETURN

 END SUBROUTINE add_all_sign_stat

 !
 ! --------------------------------------------------------
 !

 SUBROUTINE clear_sign_data

    IMPLICIT NONE

    INTEGER :: i

    DO i=1,sign_stat_max
       DEALLOCATE(sign_stat(i)%r,sign_stat(i)%n)
    ENDDO

    DEALLOCATE(sign_stat)


    IF ( lall_sign_stat ) THEN
       DO i=1,sign_stat_max
          DEALLOCATE(all_sign_stat(i)%r,all_sign_stat(i)%n)
       ENDDO

       DEALLOCATE(all_sign_stat)
    ENDIF

    sign_stat_max  = 0
    sign_stat_last = 1

    lall_sign_stat = .FALSE.


 END SUBROUTINE clear_sign_data

END MODULE sign_data
