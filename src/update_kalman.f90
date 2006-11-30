SUBROUTINE update_kalman(i,n,l,vdate,vtime,obs,dat)

 USE data, ONLY : nexp,nparver,maxstn,fclen,kalman_fclen
 USE kalman
     
 IMPLICIT NONE

 INTEGER, INTENT(IN)    :: i,n,l
 INTEGER, INTENT(IN)    :: vdate,vtime
 REAL,    INTENT(IN)    :: obs
 REAL,    INTENT(INOUT) :: dat(nexp)

 INTEGER :: k,kk,m,             &
            y1,m1,d1,h1,        &
            y2,m2,d2,h2,        &
            diff,ierr

 REAL :: yt(1,1),value(2),d
 !------------------------------------------------------

 IF ( allocate_kalman ) CALL init_kalman(nexp,nparver,maxstn)

 IF ( ANY(kalman_fclen == fclen(n)) ) THEN

    DO k=nexp/2+1,nexp

       IF ( kalvar(k,l,i)%date /= 0 ) THEN

       y1 = kalvar(k,l,i)%date / 10000
       m1 = MOD(kalvar(k,l,i)%date,10000) / 100
       d1 = MOD(kalvar(k,l,i)%date,100  )
       h1 = kalvar(k,l,i)%time / 10000

       y2 = vdate / 10000
       m2 = MOD(vdate,10000) / 100
       d2 = MOD(vdate,100  )
       h2 = vtime / 10000

       CALL hourdiff(y2,m2,d2,h2,y1,m1,d1,h1,diff,ierr)

       d = kalvar(k,l,i)%d 
       kalvar(k,l,i)%d = 99.
       DO m=1,diff/kalman_frequency-1

          kalvar(k,l,i)%x = kalvar(k,l,i)%xn
          value(1) = 1.0
          value(2) = 1.0
          yt = 99.

          WRITE(6,*)'RELAX at',vdate,vtime,m
          CALL KALMAN2(kalvar(k,l,i)%xn,     &
                    yt,                   &
                    kalvar(k,l,i)%q,      &
                    kalvar(k,l,i)%f1,     &
                    kalvar(k,l,i)%f2,     &
                    kalvar(k,l,i)%q0,     &
                    kalvar(k,l,i)%d,      &
                    value)

          CALL print_kalman(k,l,i)
       ENDDO 
       kalvar(k,l,i)%d = d

       ENDIF

    ENDDO

 ENDIF

 EXP_LOOP : DO k=nexp/2+1,nexp

    kk = k - nexp/2
    IF ( ANY(kalman_fclen == fclen(n)) ) THEN

!      WRITE(6,*)'UPDATE at',vdate,vtime

       IF ( kalvar(k,l,i)%date == 0 ) THEN

          diff = kalman_frequency 

       ELSE

          y1 = kalvar(k,l,i)%date / 10000
          m1 = MOD(kalvar(k,l,i)%date,10000) / 100
          d1 = MOD(kalvar(k,l,i)%date,100  )
          h1 = kalvar(k,l,i)%time / 10000
   
          y2 = vdate / 10000
          m2 = MOD(vdate,10000) / 100
          d2 = MOD(vdate,100  )
          h2 = vtime / 10000

          CALL hourdiff(y2,m2,d2,h2,y1,m1,d1,h1,diff,ierr)
      
       ENDIF

       IF ( diff >= kalman_frequency ) THEN

           kalvar(k,l,i)%x    = kalvar(k,l,i)%xn
           kalvar(k,l,i)%date = vdate
           kalvar(k,l,i)%time = vtime

       ENDIF

       value(1) = 1.0
       value(2) = dat(kk) + obs

       yt       = - dat(kk)

       CALL KALMAN2(kalvar(k,l,i)%xn,     &
                    yt,                   &
                    kalvar(k,l,i)%q,      &
                    kalvar(k,l,i)%f1,     &
                    kalvar(k,l,i)%f2,     &
                    kalvar(k,l,i)%q0,     &
                    kalvar(k,l,i)%d,      &
                    value)
       CALL print_kalman(k,l,i)

    ENDIF

    dat(k) = ( dat(kk) + obs ) * kalvar(k,l,i)%x(2) + kalvar(k,l,i)%x(1) + dat(kk)

 ENDDO EXP_LOOP

 RETURN

END SUBROUTINE update_kalman

