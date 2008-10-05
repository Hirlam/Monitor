SUBROUTINE print_scat(lunout,nparver,nr,nrun,        &
           scat,p1,p2,par_active,full_scatter,       &
           uh,uf)

 !
 ! X scatter plots
 ! 
 ! Ulf Andrae, SMHI, 2007
 !

 USE types
 USE timing
 USE data, ONLY : obstype,expname,err_ind,nexp,station_name,csi, &
                  tag,show_fc_length,output_type,output_mode,    &
                  mparver,corr_pairs,flag_pairs,exp_pairs,       &
                  period_freq,maxfclenval,                       &
                  scat_min,scat_max,scat_magn,len_lab
 USE mymagics
 USE functions

 IMPLICIT NONE

 ! INPUT

 INTEGER,            INTENT(IN) :: lunout,nparver,nr,nrun,p1,p2
 INTEGER,            INTENT(IN) :: par_active(nparver)
 TYPE(scatter_type), INTENT(IN) :: scat(nparver)
 LOGICAL,            INTENT(IN) :: full_scatter,uh(nparver,0:23), &
                                   uf(nparver,0:maxfclenval)

 ! LOCAL

 INTEGER :: i,j,jj,k,kk,x,y,l,       &
            nexp_plot,pp1,ierr,      &
            period,len_loop,         &
            lcorr_pairs(mparver,2),  &
            lflag_pairs(mparver,2),  &
             lexp_pairs(mparver,2)

 CHARACTER(LEN= 10) :: cnum     =' '
 CHARACTER(LEN=100) :: fname    =' '
 CHARACTER(LEN= 50) :: my_tag   =' '
 CHARACTER(LEN=100) :: wtext    =' ', &
                       axist(2) =' ', &
                       wtext2   =' ', &
                       wtext3   =' ', &
                       wtext4   =' ', &
                       wname    =' ', &
                       titln    =' '
 CHARACTER(LEN=  1) :: prefix   =' '
 CHARACTER(LEN= 80) :: title    =' '

 REAL :: minax(2),maxax(2),rtmp

 REAL, ALLOCATABLE :: val(:,:)

 LOGICAL :: scatflag = .TRUE.

 !
 !-----------------------------------------------------
 !

 IF ( full_scatter ) THEN
    prefix ='s'
    titln = 'Scatterplot'
    lcorr_pairs = 0 
    lflag_pairs = 1
     lexp_pairs = 0
    DO i=1,nparver
       lcorr_pairs(i,:) = i
       lflag_pairs(i,:) = (/-1,1/)
    ENDDO
    len_loop = nparver
 ELSE
    prefix ='x'
    titln = 'Xcrossplot'
    lcorr_pairs = corr_pairs
    lflag_pairs = flag_pairs
     lexp_pairs =  exp_pairs
    len_loop = mparver
    IF ( output_mode /= 1 ) THEN
       WRITE(6,*)'output_mode has to be 1 when doing Xcrossplots'
       CALL abort
    ENDIF
 ENDIF

 ! Set filename period
 IF ( p1 < 999999 ) THEN
    period = p1
 ELSE
    period = 0
 ENDIF

 ! Allocate working data
 ALLOCATE(val(2,MAXVAL(scat%n)),STAT=ierr)
 IF ( ierr /= 0 ) THEN
    WRITE(6,*)'could not allocated val',MAXVAL(scat%n)
    CALL abort
 ENDIF

 DO j=1,len_loop

    ! Cycle if nothing is to be done
    IF ( ALL(lcorr_pairs(j,:) == 0 ) ) CYCLE

    IF ( ANY(lcorr_pairs(j,:) == 0 ) ) THEN
       WRITE(6,*)'Error in corr_pairs setting, skip this',lcorr_pairs(j,:)
       CYCLE
    ENDIF

    !
    ! Set title and axis names
    !
    IF (nr > 0) THEN
      IF(ALLOCATED(station_name)) THEN
        title = TRIM(titln)//' for '//TRIM(station_name(csi))
      ELSE
        WRITE(cnum,'(I8)')nr
        title = TRIM(titln)//' for '//TRIM(cnum)
      ENDIF
    ELSE
      WRITE(cnum,'(I4)')par_active(lcorr_pairs(j,1))
      title = TRIM(titln)//' for '//TRIM(cnum)//' stations'
      IF ( TRIM(tag) /= '#' ) title=TRIM(title)//' Area: '//TRIM(tag)
    ENDIF

    ! Line 2, time period
    IF (p1 == 0 ) THEN
    ELSEIF(p1 < 13) THEN

       SELECT CASE(period_freq) 
       CASE(1)
        WRITE(wtext4,'(A8,A8)')'Period: ',seasonal_name2(p1)
       CASE(3)
        WRITE(wtext4,'(A8,A8)')'Period: ',seasonal_name1(p1)
       END SELECT 

    ELSEIF(p1 < 999999 ) THEN
       pp1 = monincr(p1,period_freq-1)
       IF(p1 == pp1 ) THEN
          WRITE(wtext4,'(A8,I8)')'Period: ',p1
       ELSE
          WRITE(wtext4,'(A8,I8,A2,I8)')'Period: ',        &
          p1,' -',pp1
       ENDIF
    ELSE
       WRITE(wtext4,'(A8,I8,A1,I8)')'Period: ',p1,'-',p2
    ENDIF


    ! Find min and max

    IF ( scat_min(lcorr_pairs(j,1)) < scat_max(lcorr_pairs(j,1)) ) THEN

       minax = scat_min(lcorr_pairs(j,1))
       maxax = scat_max(lcorr_pairs(j,1))
       kk = scat(lcorr_pairs(j,1))%n

    ELSE

       minax = 0.
       maxax = 1.

       !
       ! Since we have demanded all_var_present
       ! all n should be equal and we only have 
       ! to test 1 case
       !

       kk = scat(lcorr_pairs(j,1))%n

       IF (kk > 0 )THEN

        DO k=1,2
   
         i = lcorr_pairs(j,k)

         SELECT CASE(lflag_pairs(j,k))
         CASE(-1)
            ! Observation
            minax(k) =  MINVAL(scat(i)%dat(1,1:kk))
            maxax(k) =  MAXVAL(scat(i)%dat(1,1:kk))
         CASE( 0,2)
            ! Bias
            IF( ALL( lexp_pairs(j,:) == 0 ) ) THEN
               minax(k) =  MINVAL(scat(i)%dat(2,1:kk))
               maxax(k) =  MAXVAL(scat(i)%dat(2,1:kk))
               DO jj=2,nexp
                      rtmp =  MINVAL(scat(i)%dat(1+jj,1:kk))
                  minax(k) =  MIN(rtmp,minax(k))
                      rtmp =  MAXVAL(scat(i)%dat(1+jj,1:kk))
                  maxax(k) =  MAX(rtmp,maxax(k))
               ENDDO
            ELSE
               x = lexp_pairs(j,k) + 1
               minax(k) =  MINVAL(scat(i)%dat(x,1:kk))
               maxax(k) =  MAXVAL(scat(i)%dat(x,1:kk))
            ENDIF
         CASE( 1)
            ! Model
            IF( ALL(lexp_pairs(j,:) == 0 ) ) THEN
               minax(k) =  MINVAL(scat(i)%dat(2,1:kk) + scat(i)%dat(1,1:kk))
               maxax(k) =  MAXVAL(scat(i)%dat(2,1:kk) + scat(i)%dat(1,1:kk))
               DO jj=2,nexp
                  rtmp =  MINVAL(scat(i)%dat(1+jj,1:kk) + scat(i)%dat(1,1:kk))
                  minax(k) =  MIN(rtmp,minax(k))
                  rtmp =  MAXVAL(scat(i)%dat(1+jj,1:kk) + scat(i)%dat(1,1:kk))
                  maxax(k) =  MAX(rtmp,maxax(k))
               ENDDO
            ELSE
               x = lexp_pairs(j,k) + 1
               minax(k) =  MINVAL(scat(i)%dat(x,1:kk) + scat(i)%dat(1,1:kk))
               maxax(k) =  MAXVAL(scat(i)%dat(x,1:kk) + scat(i)%dat(1,1:kk))
            ENDIF
         CASE DEFAULT
            WRITE(6,*)'No such option in flag_pairs',lflag_pairs(j,1)
         END SELECT

     ENDDO

     IF ( ALL( lflag_pairs(j,:) /= 0 )  .AND.    &
          (lcorr_pairs(j,1) == lcorr_pairs(j,2)) ) THEN
        ! If the parameters is the same the axis
        ! should be the same
         minax =  MINVAL(minax)
         maxax =  MAXVAL(maxax)
     ENDIF

     DO i=1,2
        ! Special wind direction case
        IF ( (obstype(lcorr_pairs(j,i))(1:2) == 'DD' ) .AND.   &
            (lflag_pairs(j,i) /= 0     )  ) THEN
            minax(i) =   0.
            maxax(i) = 360.
        ENDIF
     ENDDO

    ENDIF ! kk > 0

    ENDIF ! scat_min < scat_max

    IF ( show_fc_length )                      &
    CALL fclen_header(.TRUE.,maxfclenval,      &
                      uh(lcorr_pairs(j,1),:),  &
                      uf(lcorr_pairs(j,1),:),  &
                      wtext3)

    nexp_plot = 1
    IF( ALL(lexp_pairs(j,:) == 0 ) ) nexp_plot = nexp

    DO jj=1,nexp_plot

       ! Set filename
       i = lcorr_pairs(j,2)

       my_tag = TRIM(tag)//'_'//TRIM(expname(jj))
       CALL make_fname(prefix,period,nr,my_tag,          &
                       obstype(i)(1:2),                  &
                       obstype(i)(3:len_lab),            &
                       output_mode,output_type,fname)

       ! Open output file
       CALL open_output(fname)

       ! Copy the data
       DO k=1,2

           i = lcorr_pairs(j,k)
           CALL pname(obstype(i),wtext) 

           SELECT CASE(lflag_pairs(j,k))
           CASE(-1)
              val(k,1:kk) = scat(i)%dat(1,1:kk)
              axist(k)= 'OBS '//TRIM(wtext)
           CASE( 0)
              IF( ALL(lexp_pairs(j,:) == 0 ) ) THEN
                val(k,1:kk) = scat(i)%dat(1+jj,1:kk)
                axist(k) = TRIM(expname(jj))//' - OBS '//TRIM(wtext)
              ELSE
                x = lexp_pairs(j,k) + 1
                val(k,1:kk) = scat(i)%dat(x,1:kk)
                axist(k) = TRIM(expname(x-1))//' - OBS '//TRIM(wtext)
              ENDIF
           CASE( 1)
              IF( ALL(lexp_pairs(j,:) == 0 ) ) THEN
                val(k,1:kk) = scat(i)%dat(1+jj,1:kk) + &
                              scat(i)%dat(1   ,1:kk)
                axist(k) = TRIM(expname(jj))//' '//TRIM(wtext)
              ELSE
                x = lexp_pairs(j,k) + 1
                val(k,1:kk) = scat(i)%dat(x,1:kk) + &
                              scat(i)%dat(1   ,1:kk)
                axist(k) = TRIM(expname(x-1))//' '//TRIM(wtext)
              ENDIF
           CASE( 2)
              IF( ALL(lexp_pairs(j,:) == 0 ) ) THEN
                val(k,1:kk) = scat(i)%dat(1+jj,1:kk)
                axist(k) = TRIM(expname(jj))//' - OBS '//TRIM(wtext)
              ELSE
                x = lexp_pairs(j,1) + 1
                y = lexp_pairs(j,2) + 1
                val(k,1:kk) = scat(i)%dat(x,1:kk) - scat(i)%dat(y,1:kk)
                axist(k) = TRIM(expname(x-1))//' - '//TRIM(expname(y-1))//' '//TRIM(wtext)
              ENDIF
           END SELECT

           !
           ! Special case for wind direction
           !

           IF ( obstype(i)(1:2) == 'DD' .AND. lflag_pairs(j,k) /= 0 ) THEN
              DO l=1,kk
                 IF (val(k,l) > 360. ) THEN
                    val(k,l) = val(k,l) - 360.         
                 ELSEIF(val(k,l) <   0. ) THEN
                    val(k,l) = val(k,l) + 360.         
                 ENDIF
              ENDDO
           ENDIF
        ENDDO

        wtext ='                               '
        IF( full_scatter ) &
        CALL pname(obstype(lcorr_pairs(j,1)),wtext)

        CALL bin_cont(lunout,                         &
                      val(1,1:kk),val(2,1:kk),kk,     &
                      minax(1),maxax(1),              &
                      minax(2),maxax(2),              &
                      scat_magn(lcorr_pairs(j,1)),    &
                      fname,                          &
                      title,wtext,wtext3,wtext4,      &
                      axist(2),axist(1)) 


        CLOSE(lunout)

     ENDDO

  ENDDO

  DEALLOCATE(val)

RETURN
END SUBROUTINE print_scat
