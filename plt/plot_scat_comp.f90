SUBROUTINE plot_scat_comp(lunout,nparver,nr,nrun,    &
           scat,p1,p2,par_active,full_scatter)

 !
 ! X scatter plots
 ! 
 ! Ulf Andrae, SMHI, 2007
 !

 USE types
 USE timing
 USE data, ONLY : obstype,expname,err_ind,nexp,station_name,csi, &
                  fclen,nfclengths,                              &
                  show_fc_length,output_type,                    &
                  mparver,corr_pairs,flag_pairs,exp_pairs,       &
                  period_freq
 USE mymagics
 USE functions

 IMPLICIT NONE

 ! INPUT

 INTEGER,            INTENT(IN) :: lunout,nparver,nr,nrun,p1,p2
 INTEGER,            INTENT(IN) :: par_active(nparver)
 TYPE(scatter_type), INTENT(IN) :: scat(nparver)
 LOGICAL,            INTENT(IN) :: full_scatter

 ! LOCAL

 INTEGER :: i,j,jj,k,kk,x,y,l,       &
            nexp_plot,pp1,           &
            lcorr_pairs(mparver,2),  &
            lflag_pairs(mparver,2),  &
             lexp_pairs(mparver,2)

 CHARACTER(LEN= 10) :: cnum    =' '
 CHARACTER(LEN= 40) :: fname   =' '
 CHARACTER(LEN=100) :: wtext   =' ', &
                       axist(2)=' ', &
                       wtext2  =' ', &
                       wtext3  =' ', &
                       wtext4  =' ', &
                       wname   =' ', &
                       titln   =' '
 CHARACTER(LEN=  1) :: prefix  =' '
 CHARACTER(LEN= 80) :: title   =' '

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
    k = 0
    DO i=1,nparver
       DO j=1,nexp
          k = k + 1
          lcorr_pairs(k,:) = i
          lflag_pairs(k,:) = (/-1,1/)
           lexp_pairs(k,:) = j
       ENDDO
    ENDDO
 ELSE
    prefix ='x'
    titln = 'Xcrossplot'
    lcorr_pairs = corr_pairs
    lflag_pairs = flag_pairs
     lexp_pairs =  exp_pairs
 ENDIF

 ! Allocate working data
 ALLOCATE(val(2,MAXVAL(scat%n)))

 ! Set filename
 IF ( p1 < 999999 ) THEN
    CALL make_fname(prefix,p1,nr,nrun,fname,output_type)
 ELSE
    CALL make_fname(prefix, 0,nr,nrun,fname,output_type)
 ENDIF

 ! Open ps file
 CALL open_output(fname)

 CALL PSETC ('TEXT_COLOUR','BLACK')

 CALL psetc('PAGE_ID_LINE_SYSTEM_PLOT','ON'  )
 CALL psetc('PAGE_ID_LINE_ERRORS_PLOT','OFF' )
 CALL psetc('PAGE_ID_LINE_DATE_PLOT'  ,'ON'  )
 CALL psetc('PAGE_ID_LINE_QUALITY'    ,'HIGH')
 CALL psetc('PAGE_ID_LINE_LOGO_PLOT'  ,'OFF' )

 DO j=1,mparver
  
    ! Cycle is nothing is to be done
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

    IF ( show_fc_length ) THEN
       IF (nfclengths > 10 ) THEN
          wname='(A,2I3.2,A5,I2.2)'
          WRITE(wtext3,wname)'Forecast lengths used:',   &
          fclen(1:2),' ... ',fclen(nfclengths)
       ELSE
          wname='(A,XX(1X,I2.2))'
          WRITE(wname(4:5),'(I2.2)')nfclengths
          WRITE(wtext3,wname)'Forecast lengths used:',fclen(1:nfclengths)
          CALL PSETC('TEXT_LINE_4',wtext)
       ENDIF
    ENDIF

    nexp_plot = 1
    IF( ALL(lexp_pairs(j,:) == 0 ) ) nexp_plot = nexp

    DO jj=1,nexp_plot

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

        call bin_scat(val(1,1:kk),val(2,1:kk),kk,     &
                      minax(1),maxax(1),              &
                      minax(2),maxax(2),              &
                      scatflag,                       &
                      title,wtext,axist(2),axist(1),  &
                      wtext3,wtext4)

     ENDDO

  ENDDO

  CALL PCLOSE

  DEALLOCATE(val)

RETURN
END SUBROUTINE plot_scat_comp
