SUBROUTINE print_map(stnr,yymm,yymm2,ptype,per_ind,par_active)

 !
 ! Plot maps of station statistics
 !
 ! Ulf Andrae, SMHI, 2005
 !

 ! Modules
 USE mymagics
 USE timing
 USE data,     ONLY : maxstn,lfcver,        &
                      nexp,tag,output_type, &
                      output_mode,len_lab,  &
                      stat,                 &
                      obstype,expname,      &
                      show_fc_length,ldiff, &
                      ntimver,map_scale,    &
                      nparver,              &
                      used_hours,used_fclen,&
                      timdiff,time_shift,   &
                      map_rmse_interval,    &
                      map_bias_interval,    &
                      map_obs_interval,     &
                      use_fclen,show_times, &
                      lunout,               &
                      station_name,csi,     &
                      maxfclenval,          &
                      period_freq,accu_int

 IMPLICIT NONE

 ! Input

 INTEGER, INTENT(IN) :: stnr,yymm,yymm2,ptype,   &
                        per_ind,par_active(nparver)

 ! Local

 INTEGER, PARAMETER :: maxint = 6

 INTEGER :: i,j,k,kk,l,ll,m,            &
            hour(ntimver),              &
            maxn,                       &
            numstn,period,              &
            nexp_plot,ntimver_out

 INTEGER, ALLOCATABLE :: stn(:)

 REAL :: lmax,lint,   &
         interval(maxint+1) =(/-6.,-4.,-2.,0.,2.,4.,6./),       &
         rnum,bias,rmse,obs,                                    &
         minlat,maxlat,minlon,maxlon

 REAL, ALLOCATABLE :: lat(:),lon(:),dat(:,:)

 LOGICAL :: user_interval
 LOGICAL :: print_latlon

 CHARACTER(LEN=100) :: wtext = ' ',wtext1 = ' '
 CHARACTER(LEN=100) :: my_tag   = ' '
 CHARACTER(LEN=10 ) :: chour    = ' ',cdum = ' '
 CHARACTER(LEN=100) :: fname    = ' ',sname=' '
 CHARACTER(LEN=30)  :: wname    = ' '
 CHARACTER(LEN=30)  :: mtext    = ' '
 CHARACTER(LEN=50)  :: cunit    = ' ',carea = ' '
 CHARACTER(LEN=len_lab  ) :: ob_short=''
 CHARACTER(LEN=3) :: prefix = ' '

!-----------------------------------------------------------

 !
 ! Set specific settings depending on ptype
 ! 0 : bias
 ! 1 : rmse
 ! 2 : value
 !

 SELECT CASE(ptype)
 CASE(0)
    prefix = 'm_b'
    IF (lfcver) prefix = 'M_b'
    mtext = 'bias'
    nexp_plot = nexp
 CASE(1)
    prefix = 'm_r'
    IF (lfcver) prefix = 'M_r'
    mtext = 'rmse'
    nexp_plot = nexp
 CASE(2)
    prefix = 'm_o'
    IF (lfcver) prefix = 'M_o'
    mtext = ' '
    nexp_plot = nexp + 1
 CASE DEFAULT
    WRITE (6,*)'No such ptype in plot_map',ptype
    CALL abort
 END SELECT 

 IF (yymm < 999999 ) THEN
    period = yymm
 ELSE
    period = 0
 ENDIF

 IF ( ALL(show_times == -1) ) THEN
    ntimver_out = 1
 ELSE
    ntimver_out = ntimver
 ENDIF

 !
 ! Set plotting hours
 ! If map_hours not given (-1) plot all
 !

 IF (lfcver) THEN
    hour(1:ntimver)=use_fclen(1:ntimver)
 ELSE
    DO i=1,ntimver
       hour(i)=(i-1)*timdiff + time_shift
    ENDDO
 ENDIF

 !
 ! Allocate 
 !

 ALLOCATE(lat(maxstn),lon(maxstn),stn(maxstn),dat(nexp_plot,maxstn))

 dat = 0.
 lat = 0.
 lon = 0.
 stn = 0

 !
 ! Loop over all parameters and times
 !

 PAR_LOOP : DO j=1,nparver

    FC_LOOP  : DO kk=1,ntimver_out

       !
       ! Plot only the requested hours
       !
       IF ( ntimver_out /= 1 .AND. .NOT. ANY( show_times == hour(kk) )) CYCLE

       !
       ! Copy data and estimate max/min values 
       !

       maxn = 0
       lmax = 0.
       DO i=1,nexp 

         ll = 0

         DO l=1,maxstn

            IF ( ntimver_out == 1 ) THEN
             rnum = 0.
             bias = 0.
             rmse = 0.
              obs = 0.
             DO k = 1,ntimver
               rnum = rnum + FLOAT(stat(l)%s(i,j,k)%n)
               bias = bias +       stat(l)%s(i,j,k)%bias
               rmse = rmse +       stat(l)%s(i,j,k)%rmse
                obs = obs  +       stat(l)%s(i,j,k)%obs
             ENDDO
            ELSE
               rnum = FLOAT(stat(l)%s(i,j,kk)%n)
               bias =       stat(l)%s(i,j,kk)%bias
               rmse =       stat(l)%s(i,j,kk)%rmse
                obs =       stat(l)%s(i,j,kk)%obs
            ENDIF

            IF ( NINT(rnum) == 0 ) CYCLE

            ll = ll + 1

            SELECT CASE (ptype)
            CASE(0)
                dat(i,ll) = bias / MAX(rnum,1.)
            CASE(1)
                dat(i,ll) = SQRT (rmse / MAX(rnum,1.))
            CASE(2)

               IF ( ldiff ) THEN
                  dat(i,ll) = ( bias + obs ) / MAX(rnum,1.)
               ELSE
                  dat(i,ll) = ( bias + obs )
               ENDIF

               IF ( i == 1 ) THEN
                 IF ( ldiff ) THEN
                  dat(nexp+1,ll) = obs / rnum
                 ELSE
                  dat(nexp+1,ll) = obs
                 ENDIF
               ENDIF

            CASE DEFAULT
               WRITE (6,*)'No such ptype in plot_map',ptype
               CALL abort
            END SELECT

            IF ( i == 1 ) THEN
               lat(ll) = stat(l)%lat
               lon(ll) = stat(l)%lon
               stn(ll) = stat(l)%stnr
            ENDIF

            maxn  = MAX(maxn,NINT(rnum))
            lmax  = MAX(lmax,ABS(dat(i,ll)))

         ENDDO

         IF ( i == 1 ) numstn = ll

       ENDDO 

       !
       ! Set the intervals
       !

       SELECT CASE(ptype)
       CASE(0)
          user_interval = ( ABS(map_bias_interval(1,j) -   &
                            map_bias_interval(maxint+1,j) ) > 1.e-6 )
       CASE(1)
          user_interval = ( ABS(map_rmse_interval(1,j) -   &
                            map_rmse_interval(maxint+1,j) ) > 1.e-6 )
       CASE(2)
          user_interval = ( ABS( map_obs_interval(1,j) -   &
                            map_obs_interval(maxint+1,j) ) > 1.e-6 )
       END SELECT

       IF ( .NOT. user_interval ) THEN

          lmax = lmax*1.001
          SELECT CASE(ptype)
          CASE(0)
             lint = lmax / (maxint/2)
             DO m=-maxint/2,maxint/2
                interval(m+maxint/2+1) = m*lint
             ENDDO
          CASE(1,2)
             lint = lmax / maxint
             DO m=0,maxint
                interval(m+1) = m*lint
             ENDDO
          CASE DEFAULT
          END SELECT

       ELSE

          SELECT CASE(ptype)
          CASE(0)
             interval = map_bias_interval(:,j)
          CASE(1)
             interval = map_rmse_interval(:,j)
          CASE(2)
             interval = map_obs_interval(:,j)
          END SELECT

       ENDIF

       !
       ! Loop over all experiments
       !

       carea = '['
       minlon=MINVAL(lon(1:numstn))
       WRITE(cdum,'(I3)')NINT(minlon-1.)
       carea = TRIM(carea)//TRIM(cdum)//':'
       maxlon=MAXVAL(lon(1:numstn))
       WRITE(cdum(1:5),'(I3)')NINT(maxlon+1.)
       carea = TRIM(carea)//TRIM(cdum)//']['
       minlat=MINVAL(lat(1:numstn))
       WRITE(cdum(1:5),'(I3)')NINT(minlat-1.)
       carea = TRIM(carea)//TRIM(cdum)//':'
       maxlat=MAXVAL(lat(1:numstn))
       WRITE(cdum(1:5),'(I3)')NINT(maxlat+1.)
       carea = TRIM(carea)//TRIM(cdum)//']'

       EXP_LOOP : DO i=1,nexp_plot 

          IF ( ntimver_out == 1 ) THEN
             my_tag = TRIM(tag)//'_ALL'
          ELSE
             chour = ' '
             WRITE(chour,'(I2.2)')hour(kk)
             my_tag = TRIM(tag)//'_'//TRIM(chour)
          ENDIF

          my_tag = TRIM(my_tag)//'_'//TRIM(expname(i))

          CALL make_fname(prefix,period,stnr,     &
               my_tag,obstype(j)(1:2),            &
               obstype(j)(3:len_lab),             &
               output_mode,output_type,           &
               fname)

          CALL open_output(fname)

    ! Create headers
 
    ! Line 1
    IF(ALLOCATED(station_name).AND. stnr > 0 ) THEN
       wtext='Station: '//trim(station_name(csi))
    ELSE
       WRITE(wtext1(1:8),'(I8)')stnr
       wtext='Station: '//trim(wtext1(1:8))
    ENDIF
    IF (stnr == 0) THEN
       wname=''
       WRITE(wname(1:5),'(I5)')par_active(j)
       wtext=TRIM(wname)//' stations'
       IF ( TRIM(tag) /= '#' ) wtext='Area: '//TRIM(tag)//'  '//TRIM(wtext)
    ENDIF

    wtext = 'Exp: '//TRIM(expname(i))//'   '//TRIM(wtext)

    IF (yymm == 0 ) THEN
    ELSEIF(yymm < 13) THEN
     
       SELECT CASE(period_freq) 
       CASE(1)
        WRITE(wtext1,'(A8,A8)')'Period: ',seasonal_name2(yymm)
       CASE(3)
        WRITE(wtext1,'(A8,A8)')'Period: ',seasonal_name1(yymm)
       END SELECT 
     
    ELSEIF(yymm < 999999 ) THEN
       WRITE(wtext1,'(A8,I8)')'Period: ',yymm
    ELSE
       WRITE(wtext1,'(A8,I8,A1,I8)')'Period: ',yymm,'-',yymm2
    ENDIF

    wtext = TRIM(wtext)//'   '//TRIM(wtext1)

    WRITE(lunout,'(A,X,A)')'#HEADING_1',TRIM(wtext)

    ! Line 2
    CALL pname(obstype(j),wtext)

    ob_short = obstype(j)
    ob_short(3:6) = '   '
    CALL yunit(ob_short,cunit)

    wtext = TRIM(wtext)//' '//TRIM(mtext)//' ['//TRIM(cunit)//']'

    IF ( ntimver_out /= 1 ) THEN
       IF (lfcver) THEN
          WRITE(chour,'(I3.2,X,A1)')hour(kk),'H'
       ELSE
          WRITE(chour,'(I3.2,X,A3)')hour(kk),'UTC'
       ENDIF
       wtext = TRIM(wtext)//' at '//TRIM(chour)
    ENDIF

    WRITE(lunout,'(A,X,A)')'#HEADING_2',TRIM(wtext)

    ! Line 3
    IF ( ntimver_out == 1 ) THEN
            IF ( show_fc_length ) THEN
               CALL fclen_header(.TRUE.,maxfclenval,        &
                                 used_hours(j,per_ind,:),   &
                                 used_fclen(j,per_ind,:),   &
                                 accu_int(j),wtext)
            ENDIF
    ELSE
            CALL fclen_header(.NOT.lfcver,maxfclenval,   &
                              used_hours(j,per_ind,:),   &
                              used_fclen(j,per_ind,:),   &
                              accu_int(j),wtext)
    ENDIF


    WRITE(lunout,'(A,X,A)')'#HEADING_3',TRIM(wtext)

    ! Experiments and parameters and norms
    WRITE(lunout,'(A,X,A)')'#PAR',TRIM(obstype(j))

    WRITE(lunout,'(A,X,A)')'#XLABEL','Lon'
    WRITE(lunout,'(A,X,A)')'#YLABEL','Lat'


          WRITE(lunout,'(2A)')'#AREA ',TRIM(carea)

          DO l=2,maxint+1

             IF ( interval(l) <= interval(l-1)) EXIT
             WRITE(chour,'(I2.2)')l
             sname = TRIM(fname)//'_'//chour(1:2)
             OPEN(UNIT=37,FILE=sname)
             WRITE(lunout,'(A,X,A,X,2f8.1)')'#SLEVEL', &
             chour(1:2),interval(l-1),interval(l)
 
             print_latlon=.FALSE.
             DO ll=1,numstn
              IF ( dat(i,ll) >= interval(l-1) .AND.   &
                  dat(i,ll) <  interval(l  )      ) THEN
                  WRITE(37,*)lon(ll),lat(ll)
                  print_latlon=.TRUE.
              ENDIF
             ENDDO
             IF(.NOT.print_latlon) WRITE(37,*) "-999.   -999."
          ENDDO

          CLOSE(37)

          WRITE(lunout,'(A)')'#END'
          CLOSE(lunout)

       ENDDO EXP_LOOP
    ENDDO FC_LOOP  
 ENDDO PAR_LOOP 

 DEALLOCATE(lat,lon,stn,dat)

 RETURN

END SUBROUTINE print_map
