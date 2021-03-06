SUBROUTINE print_scat(lunout,nparver,nr,             &
           scat,p1,p2,par_active,full_scatter,       &
           uh,uf)

 !
 ! X scatter plots
 ! 
 ! Ulf Andrae, SMHI, 2007
 !

 USE types
 USE timing
 USE constants, ONLY : seasonal_name1,seasonal_name2
 USE data, ONLY : varprop,expname,err_ind,nexp,station_name,csi, &
                  tag,show_fc_length,output_type,output_mode,    &
                  mparver,corr_pairs,flag_pairs,exp_pairs,       &
                  period_freq,period_type,                       &
                  maxfclenval,                                   &
                  scat_min,scat_max,scat_magn,len_lab,           &
                  lscat_yave,cini_hours,exp_offset,plot_prefix
 USE functions

 IMPLICIT NONE

 INTEGER, PARAMETER :: nbin = 100

 ! INPUT

 INTEGER,            INTENT(IN) :: lunout,nparver,nr,p1,p2
 INTEGER,            INTENT(IN) :: par_active(nparver)
 TYPE(scatter_type), INTENT(IN) :: scat(nparver)
 LOGICAL,            INTENT(IN) :: full_scatter,uh(nparver,0:23), &
                                   uf(nparver,0:maxfclenval)

 ! LOCAL

 INTEGER :: i,j,jj,k,kk,x,y,l,m,     &
            nexp_plot,ierr,          &
            period,len_loop,         &
            lcorr_pairs(mparver,2),  &
            lflag_pairs(mparver,2),  &
             lexp_pairs(mparver,2),  &
            nlevmin,nlevmax,         &
            nlevels

 CHARACTER(LEN= 10) :: cnum     =' '
 CHARACTER(LEN=100) :: fname    =' ',sname =' '
 CHARACTER(LEN= 50) :: my_tag   =' '
 CHARACTER(LEN=100) :: wtext    =' ', &
                       wtext3   =' ', &
                       wtext4   =' ', &
                       titln    =' '
 CHARACTER(LEN=  5) :: prefix   =' '
 CHARACTER(LEN= 80) :: title    =' '

 CHARACTER(LEN=100), ALLOCATABLE :: axist(:,:)

 REAL :: minax(2),maxax(2),rtmp,rcnt

 REAL, ALLOCATABLE :: val(:,:),level(:)

 TYPE(scatter_bin) :: sbin(nexp)

 LOGICAL, ALLOCATABLE :: levcheck(:,:)

 LOGICAL :: print_data

 !
 !-----------------------------------------------------
 !

 IF ( full_scatter ) THEN
    prefix = TRIM(plot_prefix(10))
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
    prefix = TRIM(plot_prefix(11))
    titln = 'Xcrossplot'
    lcorr_pairs = corr_pairs
    lflag_pairs = flag_pairs
     lexp_pairs =  exp_pairs
    len_loop = mparver
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

 DO j=1,nexp
   ALLOCATE(sbin(j)%binx(0:nbin), &
            sbin(j)%biny(0:nbin), &
            sbin(j)%array(nbin,nbin))
 ENDDO

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
      IF ( TRIM(tag) /= '#' ) title=TRIM(title)//' Selection: '//TRIM(tag)
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

    ELSEIF(p1 < 9999 .OR. (period_type == 2 .AND. period_freq == 1)) THEN
       WRITE(wtext4,'(A8,I8)')'Period: ',p1
    ELSEIF(p1 < 999999 ) THEN
       WRITE(wtext4,'(A8,I6,A1,I6)')'Period: ',        &
       p1,'-',monincr(p1,period_freq-1)
    ELSE
       WRITE(wtext4,'(A8,I8,A1,I8)')'Period: ',        &
       p1,'-',p2
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

        ! Loop over the x- and y-axis
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
        IF ( (varprop(lcorr_pairs(j,i))%id == 'DD' ) .AND.   &
            (lflag_pairs(j,i) /= 0     )  ) THEN
            minax(i) =   0.
            maxax(i) = 360.
        ENDIF
     ENDDO

    ENDIF ! kk > 0

    ENDIF ! scat_min < scat_max

    IF ( show_fc_length )                            &
    CALL fclen_header(.TRUE.,maxfclenval,            &
                      uh(lcorr_pairs(j,1),:),        &
                      uf(lcorr_pairs(j,1),:),        &
                      varprop(lcorr_pairs(j,1))%acc, &
                      MAXVAL(exp_offset),            &
                      wtext3)

    nexp_plot = 1
    IF( ALL(lexp_pairs(j,:) == 0 ) ) nexp_plot = nexp

    nlevels = scat_magn(lcorr_pairs(j,1))

    ALLOCATE(levcheck(nlevels,nexp_plot), &
             level(nlevels),              &
             axist(2,nexp_plot))

    levcheck(:,:) = .FALSE.

    DO jj=1,nexp_plot

      ! Copy the data
      DO k=1,2

        i = lcorr_pairs(j,k)
        wtext = varprop(i)%text

        SELECT CASE(lflag_pairs(j,k))
        CASE(-1)
          val(k,1:kk) = scat(i)%dat(1,1:kk)
          axist(k,jj)= 'OBS '//TRIM(varprop(i)%text)
        CASE( 0)
          IF( ALL(lexp_pairs(j,:) == 0 ) ) THEN
            val(k,1:kk) = scat(i)%dat(1+jj,1:kk)
            axist(k,jj) = TRIM(expname(jj))//' - OBS '//TRIM(wtext)
          ELSE
            x = lexp_pairs(j,k) + 1
            val(k,1:kk) = scat(i)%dat(x,1:kk)
            axist(k,jj) = TRIM(expname(x-1))//' - OBS '//TRIM(wtext)
          ENDIF
        CASE( 1)
          IF( ALL(lexp_pairs(j,:) == 0 ) ) THEN
            val(k,1:kk) = scat(i)%dat(1+jj,1:kk) + &
                          scat(i)%dat(1   ,1:kk)
            axist(k,jj) = TRIM(expname(jj))//' '//TRIM(wtext)
          ELSE
            x = lexp_pairs(j,k) + 1
            val(k,1:kk) = scat(i)%dat(x,1:kk) + &
                          scat(i)%dat(1   ,1:kk)
            axist(k,jj) = TRIM(expname(x-1))//' '//TRIM(wtext)
          ENDIF
        CASE( 2)
          IF( ALL(lexp_pairs(j,:) == 0 ) ) THEN
            val(k,1:kk) = scat(i)%dat(1+jj,1:kk)
            axist(k,jj) = TRIM(expname(jj))//' - OBS '//TRIM(wtext)
          ELSE
            x = lexp_pairs(j,1) + 1
            y = lexp_pairs(j,2) + 1
            val(k,1:kk) = scat(i)%dat(x,1:kk) - scat(i)%dat(y,1:kk)
            axist(k,jj) = TRIM(expname(x-1))//' - '//TRIM(expname(y-1))//' '//TRIM(wtext)
          ENDIF
        END SELECT

        !
        ! Special case for wind direction
        !

        IF ( varprop(i)%id == 'DD' .AND. lflag_pairs(j,k) /= 0 ) THEN
          DO l=1,kk
            IF (val(k,l) > 360. ) THEN
              val(k,l) = val(k,l) - 360.         
            ELSEIF(val(k,l) <   0. ) THEN
              val(k,l) = val(k,l) + 360.         
            ENDIF
          ENDDO
        ENDIF
      ENDDO

      CALL bin_cont(val(1,1:kk),val(2,1:kk),kk,     &
                    minax,maxax,                    &
                    nlevels,level,                  &
                    levcheck(:,jj),                 &
                    nbin,sbin(jj)) 

    ENDDO

    nlevmax = 2
    DO jj=2,nlevels
      IF ( ANY(levcheck(jj,:))) nlevmax = jj
    ENDDO 
    nlevmin = nlevmax
    DO jj=nlevmax,2,-1
      IF ( ANY(levcheck(jj,:))) nlevmin = jj
    ENDDO 

    DO jj=1,nexp_plot

      ! Set filename
      i = lcorr_pairs(j,2)

      my_tag = TRIM(tag)//TRIM(cini_hours)//'_'//TRIM(expname(jj))
      IF( full_scatter ) THEN
        CALL make_fname(prefix,period,nr,my_tag,       &
                        varprop(i)%id,varprop(i)%lev,  &
                        output_mode,output_type,fname)
      ELSE
        WRITE(cnum(1:2),'(I2.2)')j
        my_tag = TRIM(tag)//TRIM(cini_hours)
        CALL make_fname(prefix,period,nr,my_tag,     &
                       cnum(1:2),0,               &
                       output_mode,output_type,fname)
      ENDIF

      ! Open output file
      CALL open_output(fname)

      wtext ='                               '
      l = lcorr_pairs(j,1)
      IF( full_scatter ) &
      wtext = TRIM(varprop(l)%text)//' ['//TRIM(varprop(l)%unit)//']'

      ! Print out the different points
      WRITE(lunout,'(2A)')'#HEADING_1 ',TRIM(title)
      WRITE(lunout,'(2A)')'#HEADING_2 ',TRIM(wtext)
      WRITE(lunout,'(2A)')'#HEADING_3 ',TRIM(wtext4)
      WRITE(lunout,'(2A)')'#HEADING_4 ',TRIM(wtext3)
       
      WRITE(lunout,'(2A)')'#YLABEL ',TRIM(axist(2,jj))
      WRITE(lunout,'(2A)')'#XLABEL ',TRIM(axist(1,jj))
      WRITE(lunout,*)'#XMIN ',minax(1)
      WRITE(lunout,*)'#XMAX ',maxax(1)
      WRITE(lunout,*)'#YMIN ',minax(2)
      WRITE(lunout,*)'#YMAX ',maxax(2)
      WRITE(lunout,*)'#MISSING  -999.'

      DO l=nlevmin,nlevmax

        IF ( level(l) <= level(l-1)) EXIT
        WRITE(cnum,'(I2.2)')l
        sname = TRIM(fname)//'_'//cnum
        WRITE(lunout,'(A,X,A,I10)')'#SLEVEL ',cnum,NINT(level(l))

        OPEN(UNIT=37,FILE=sname)
        print_data=.FALSE.
        DO m=1,nbin
          DO i=1,nbin
            IF (sbin(jj)%array(i,m) >= level(l-1) .AND.  &
                sbin(jj)%array(i,m) <  level(l  )      ) THEN
                WRITE(37,*)sbin(jj)%binx(i-1), &
                           sbin(jj)%biny(m-1)
                print_data=.TRUE.
            ENDIF
          ENDDO
        ENDDO
        IF(.NOT.print_data) WRITE(37,*) "-999.   -999."
        CLOSE(37)

      ENDDO

      IF ( lscat_yave ) THEN
        ! Write Y-axis average line
        WRITE(lunout,'(A)')'#SLEVEL ALL ALL'
        sname = TRIM(fname)//'_ALL'
        OPEN(UNIT=37,FILE=sname)
        DO i=1,nbin
          rtmp = 0.0
          rcnt = 0.0
          DO m=1,nbin
            rtmp = rtmp + &
                   sbin(jj)%array(i,m) * &
                   sbin(jj)%biny(m-1) 
            rcnt = rcnt + sbin(jj)%array(i,m)
          ENDDO
          IF ( rcnt > 0. ) &
          WRITE(37,*)sbin(jj)%binx(i-1),rtmp/rcnt
        ENDDO
        CLOSE(37)
      ENDIF

      WRITE(lunout,'(A)')'#END'
 
      CLOSE(lunout)

    ENDDO

    DEALLOCATE(levcheck,level,axist)

  ENDDO

  DEALLOCATE(val)
  DO j=1,nexp
    DEALLOCATE(sbin(j)%binx,sbin(j)%biny,sbin(j)%array)
  ENDDO

RETURN
END SUBROUTINE print_scat
