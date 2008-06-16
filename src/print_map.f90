SUBROUTINE print_map(stnr,yymm,yymm2,ptype,mtype,per_ind)

 !
 ! Plot maps of station statistics
 !
 ! Ulf Andrae, SMHI, 2005
 !

 ! Modules
 USE mymagics
 USE timing
 USE data,     ONLY : maxstn,lfcver,&
                      nexp,tag,output_type, &
                      output_mode,len_lab,  &
                      stat,period_freq,     &
                      obstype,expname,      &
                      show_fc_length,ldiff, &
                      ntimver,map_scale,    &
                      maxfclenval,nparver,  &
                      used_hours,used_fclen,&
                      timdiff,time_shift,   &
                      use_fclen,show_times, &
                      lunout


 IMPLICIT NONE

 ! Input

 INTEGER, INTENT(IN) :: stnr,yymm,yymm2,ptype,mtype,per_ind

 ! Local

 INTEGER, PARAMETER :: maxint = 6

 INTEGER :: i,j,k,kk,l,ll,m,            &
            hour(ntimver),              &
            maxn,                       &
            numstn,period,              &
            min_stnr,max_stnr,mid(1),   &
            nexp_plot,ntimver_out

 INTEGER, ALLOCATABLE :: stn(:),mcount(:)

 REAL :: lmax,lint,   &
         min_val,max_val,                                       &
         symbol_size(maxint),                                   &
         rnum,bias,rmse,obs

 REAL, ALLOCATABLE :: lat(:),lon(:),dat(:,:),mlat(:),mlon(:),mdat(:)

 LOGICAL :: found_hour    = .FALSE.
 LOGICAL :: mask(maxstn)

 CHARACTER(LEN=100) :: text     = ' ',wtext = ' '
 CHARACTER(LEN=100) :: my_tag   = ' '
 CHARACTER(LEN=10 ) :: chour    = ' '
 CHARACTER(LEN=50 ) :: cobsname = ' '
 CHARACTER(LEN=100) :: fname    = ' '
 CHARACTER(LEN=30)  :: wname    = ' '
 CHARACTER(LEN=30)  :: mtext    = ' '
 CHARACTER(LEN=50)  :: cunit    = ' '

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
    symbol_size = (/.4,.3,.2,.2,.3,.4/)
    mtext = 'Bias'
    nexp_plot = nexp
 CASE(1)
    prefix = 'm_r'
    IF (lfcver) prefix = 'M_r'
    symbol_size = (/.2,.25,.3,.35,.4,.45/)
    mtext = 'Rmse'
    nexp_plot = nexp
 CASE(2)
    prefix = 'm_o'
    IF (lfcver) prefix = 'M_o'
    symbol_size = (/0.40,0.375,0.35,0.3,0.275,0.25/)
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
       ! Loop over all experiments
       !

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

          DO ll=1,numstn
             WRITE(lunout,*)lat(ll),lon(ll),dat(i,ll)
          ENDDO

          CLOSE(lunout)

       ENDDO EXP_LOOP
    ENDDO FC_LOOP  
 ENDDO PAR_LOOP 

 DEALLOCATE(lat,lon,stn,dat)

 RETURN

END SUBROUTINE print_map

