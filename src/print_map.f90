SUBROUTINE print_map(stnr,yymm,yymm2,ptype,per_ind,rar_active)

 !
 ! Plot maps of station statistics
 !
 ! Ulf Andrae, SMHI, 2005
 !

 ! Modules
 USE timing
 USE constants, ONLY : seasonal_name1,seasonal_name2
 USE data,     ONLY : maxstn,lfcver,        &
                      nexp,tag,output_type, &
                      output_mode,len_lab,  &
                      stat,                 &
                      varprop,expname,      &
                      show_fc_length,ldiff, &
                      ntimver,nparver,      &
                      used_hours,used_fclen,&
                      timdiff,time_shift,   &
                      map_stdv_interval,    &
                      map_rmse_interval,    &
                      map_bias_interval,    &
                      map_mabe_interval,    &
                      map_obs_interval,     &
                      use_fclen,show_times, &
                      lunout,               &
                      station_name,csi,     &
                      maxfclenval,          &
                      period_freq,          &
                      cini_hours

 IMPLICIT NONE

 ! Input

 INTEGER, INTENT(IN) :: stnr,yymm,yymm2,ptype,   &
                        per_ind,rar_active(nparver,ntimver)

 ! Local

 INTEGER, PARAMETER :: maxint = 6

 INTEGER :: i,j,k,kk,kki,l,ll,m,        &
            hour(ntimver),              &
            maxn,                       &
            numstn,period,              &
            nexp_plot,ntimver_out,      &
            map_hour,kk6,kk18

 INTEGER, ALLOCATABLE :: stn(:)

 REAL :: lmax,lint,   &
         interval(maxint+1) =(/-6.,-4.,-2.,0.,2.,4.,6./),       &
         rnum,bias,rmse,obs,mabe,                               &
         minlat,maxlat,minlon,maxlon

 REAL, ALLOCATABLE :: lat(:),lon(:),dat(:,:)

 LOGICAL :: user_interval,luh(0:23),luf(0:maxfclenval)
 LOGICAL :: print_latlon

 CHARACTER(LEN=100) :: wtext    = ' '
 CHARACTER(LEN=100) :: my_tag   = ' '
 CHARACTER(LEN=10 ) :: chour    = ' ',cdum = ' '
 CHARACTER(LEN=100) :: fname    = ' ',sname=' '
 CHARACTER(LEN=30)  :: wname    = ' '
 CHARACTER(LEN=30)  :: mtext    = ' '
 CHARACTER(LEN=50)  :: carea = ' '
 CHARACTER(LEN=3) :: prefix = ' '

!-----------------------------------------------------------

 !
 ! Set specific settings depending on ptype
 ! 0 : bias
 ! 1 : rmse
 ! 2 : value
 ! 3 : stdv
 ! 4 : mabe
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
 CASE(3)
    prefix = 'm_s'
    IF (lfcver) prefix = 'M_s'
    mtext = 'stdv'
    nexp_plot = nexp
 CASE(2)
    prefix = 'm_o'
    IF (lfcver) prefix = 'M_o'
    mtext = ' '
    nexp_plot = nexp + 1
 CASE(4)
    prefix = 'm_a'
    IF (lfcver) prefix = 'M_a'
    mtext = 'mae'
    nexp_plot = nexp
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
       IF ( hour(i) ==  6 ) kk6  = i
       IF ( hour(i) == 18 ) kk18 = i
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

    FC_LOOP  : DO kki=1,ntimver_out

       kk = 0
       !
       ! Plot only the requested hours
       ! Map min/max temp at 00|12 to 06|18
       !
       IF ( ( varprop(j)%id == 'TN'   .OR.  &
              varprop(j)%id == 'TX' ) .AND. &
                ( ntimver_out /= 1 )    .AND. &
                ( show_times(1) == 0  ) .AND. &
                ( show_times(2) == 12 ) .AND. &
                ( show_times(3) == -1 ) .AND. &
            .NOT. lfcver                      &
          ) THEN
          IF (.NOT. ANY( show_times == hour(kki) )) CYCLE
          IF ( hour(kki) == 0  ) kk = kk6
          IF ( hour(kki) == 12 ) kk = kk18
          map_hour = hour(kki)
       ELSE
          IF ( ntimver_out /= 1 .AND. .NOT. ANY( show_times == hour(kki) )) CYCLE
          kk = kki
          map_hour = hour(kki)
       ENDIF

       !
       ! Copy data and estimate max/min values 
       !

       maxn   = 0
       lmax   = 0.
       numstn = 0

       DO i=1,nexp 

         ll = 0

         DO l=1,maxstn

            IF ( ntimver_out == 1 ) THEN
             rnum = 0.
             bias = 0.
             mabe = 0.
             rmse = 0.
              obs = 0.
             DO k = 1,ntimver
               rnum = rnum + FLOAT(stat(l)%s(i,j,k)%n)
               bias = bias +       stat(l)%s(i,j,k)%bias
               mabe = mabe +       stat(l)%s(i,j,k)%mabe
               rmse = rmse +       stat(l)%s(i,j,k)%rmse
                obs = obs  +       stat(l)%s(i,j,k)%obs
             ENDDO
            ELSE
               rnum = FLOAT(stat(l)%s(i,j,kk)%n)
               bias =       stat(l)%s(i,j,kk)%bias
               mabe =       stat(l)%s(i,j,kk)%mabe
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
            CASE(3)
                dat(i,ll) = SQRT (ABS(rmse/MAX(rnum,1.)- &
                                     (bias/MAX(rnum,1.))**2))
            CASE(4)
                dat(i,ll) = mabe / MAX(rnum,1.)
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
       CASE(3)
          user_interval = ( ABS(map_stdv_interval(1,j) -   &
                            map_stdv_interval(maxint+1,j) ) > 1.e-6 )
       CASE(4)
          user_interval = ( ABS(map_mabe_interval(1,j) -   &
                            map_mabe_interval(maxint+1,j) ) > 1.e-6 )
       CASE DEFAULT
          user_interval = .FALSE.
       END SELECT

       IF ( .NOT. user_interval ) THEN

          lmax = lmax*1.001
          SELECT CASE(ptype)
          CASE(0)
             lint = lmax / (maxint/2)
             DO m=-maxint/2,maxint/2
                interval(m+maxint/2+1) = m*lint
             ENDDO
          CASE(1,2,3,4)
             lint = lmax / maxint
             DO m=0,maxint
                interval(m+1) = m*lint
             ENDDO
          CASE DEFAULT
             WRITE(6,*)'This case is not coded'
             CALL abort
          END SELECT

       ELSE

          SELECT CASE(ptype)
          CASE(0)
             interval = map_bias_interval(:,j)
          CASE(1)
             interval = map_rmse_interval(:,j)
          CASE(2)
             interval = map_obs_interval(:,j)
          CASE(3)
             interval = map_stdv_interval(:,j)
          CASE(4)
             interval = map_mabe_interval(:,j)
          END SELECT

       ENDIF

       !
       ! Loop over all experiments
       !
       
       IF ( numstn == 0 ) THEN
          minlon = -15.
          maxlon =  15
          minlat =   0.
          maxlat =  90.
       ELSE
          minlon=MINVAL(lon(1:numstn))
          maxlon=MAXVAL(lon(1:numstn))
          minlat=MINVAL(lat(1:numstn))
          maxlat=MAXVAL(lat(1:numstn))
       ENDIF

       carea = '['
       WRITE(cdum,'(I5)')NINT(minlon-1.)
       carea = TRIM(carea)//TRIM(cdum)//':'
       WRITE(cdum(1:5),'(I5)')NINT(maxlon+1.)
       carea = TRIM(carea)//TRIM(cdum)//']['
       WRITE(cdum(1:5),'(I5)')NINT(minlat-1.)
       carea = TRIM(carea)//TRIM(cdum)//':'
       WRITE(cdum(1:5),'(I5)')NINT(maxlat+1.)
       carea = TRIM(carea)//TRIM(cdum)//']'

       EXP_LOOP : DO i=1,nexp_plot 

          IF ( ntimver_out == 1 ) THEN
             my_tag = TRIM(tag)//TRIM(cini_hours)//'_ALL'
          ELSE
             chour = ' '
             WRITE(chour,'(I2.2)')map_hour
             my_tag = TRIM(tag)//TRIM(cini_hours)//'_'//TRIM(chour)
          ENDIF

          my_tag = TRIM(my_tag)//'_'//TRIM(expname(i))

          CALL make_fname(prefix,period,stnr,     &
               my_tag,                            &
               varprop(j)%id,varprop(j)%lev,      &
               output_mode,output_type,           &
               fname)

          CALL open_output(fname)

    ! Create headers
 
    ! Line 1
    IF(ALLOCATED(station_name).AND. stnr > 0 ) THEN
       wtext='Station: '//trim(station_name(csi))
    ELSE
       WRITE(cdum(1:8),'(I8)')stnr
       wtext='Station: '//trim(cdum(1:8))
    ENDIF
    IF (stnr == 0) THEN
       wname=''
       IF ( ntimver_out == 1 ) THEN
          WRITE(wname(1:5),'(I5)')MAXVAL(rar_active(j,:))
       ELSE
          WRITE(wname(1:5),'(I5)')rar_active(j,kk)
       ENDIF
       wtext=TRIM(wname)//' stations'
       IF ( TRIM(tag) /= '#' ) wtext='Selection: '//TRIM(tag)//'  '//TRIM(wtext)
    ENDIF

    wtext = 'Exp: '//TRIM(expname(i))//'   '//TRIM(wtext)
    WRITE(lunout,'(A,X,A)')'#HEADING_1',TRIM(wtext)

    ! Line 2
    wtext = ''
    IF (yymm == 0 ) THEN
    ELSEIF(yymm < 13) THEN
     
       SELECT CASE(period_freq) 
       CASE(1)
        WRITE(wtext,'(A8,A8)')'Period: ',seasonal_name2(yymm)
       CASE(3)
        WRITE(wtext,'(A8,A8)')'Period: ',seasonal_name1(yymm)
       END SELECT 
     
    ELSEIF(yymm < 999999 ) THEN
       WRITE(wtext,'(A8,I8)')'Period: ',yymm
    ELSE
       WRITE(wtext,'(A8,I8,A1,I8)')'Period: ',yymm,'-',yymm2
    ENDIF

    WRITE(lunout,'(A,X,A)')'#HEADING_2',TRIM(wtext)

    ! Line 3
    wtext = TRIM(varprop(j)%text)//' '//TRIM(mtext)//' ['//TRIM(varprop(j)%unit)//']'

    IF ( ntimver_out /= 1 ) THEN
       IF (lfcver) THEN
          WRITE(chour,'(I3.2,X,A1)')hour(kk),'H'
       ELSE
          WRITE(chour,'(I3.2,X,A3)')hour(kk),'UTC'
       ENDIF
       wtext = TRIM(wtext)//' at '//TRIM(chour)
    ENDIF

    WRITE(lunout,'(A,X,A)')'#HEADING_3',TRIM(wtext)

    ! Line 4
    IF ( ntimver_out == 1 ) THEN
       IF ( show_fc_length ) THEN
          CALL fclen_header(.TRUE.,maxfclenval,        &
                            used_hours(j,per_ind,:),   &
                            used_fclen(j,per_ind,:),   &
                            varprop(j)%acc,wtext)
       ENDIF
    ELSE
          luh = .FALSE.
          DO k=0,23
            IF ( .NOT. used_hours(j,per_ind,k) ) CYCLE
            DO l=0,maxfclenval
                IF ( .NOT. used_fclen(j,per_ind,l) ) CYCLE
                IF( MOD(k + l,24) == map_hour) luh(k) = .TRUE.
            ENDDO
          ENDDO
          luf = .FALSE.
          DO k=0,23
            IF ( .NOT. luh(k) ) CYCLE
            DO l=0,maxfclenval
               IF ( .NOT. used_fclen(j,per_ind,l) ) CYCLE
               IF( MOD(k + l,24) == map_hour) luf(l) = .TRUE.
            ENDDO
          ENDDO
         
          CALL fclen_header(.NOT.lfcver,maxfclenval,   &
                            luh,luf,                   &
                            varprop(j)%acc,wtext)
    ENDIF


    WRITE(lunout,'(A,X,A)')'#HEADING_4',TRIM(wtext)

    ! Experiments and parameters and norms
    WRITE(lunout,'(A,X,A)')'#PAR',TRIM(varprop(j)%id)

    WRITE(lunout,'(A,X,A)')'#XLABEL','Lon'
    WRITE(lunout,'(A,X,A)')'#YLABEL','Lat'


          WRITE(lunout,'(2A)')'#SELECTION ',TRIM(carea)

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

             CLOSE(37)

          ENDDO

          WRITE(lunout,'(A)')'#END'
          CLOSE(lunout)

       ENDDO EXP_LOOP
    ENDDO FC_LOOP  
 ENDDO PAR_LOOP 

 DEALLOCATE(lat,lon,stn,dat)

 RETURN

END SUBROUTINE print_map
