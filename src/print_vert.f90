SUBROUTINE print_vert(lunout,nexp,nlev,nparver,ntimver,     &
                      s,stnr,yymm,yymm2,par_active,uh,uf)

 !
 ! Print vertical profile of RMS,STD and BIAS 
 ! Vertical coordinate is assumed to be pressure
 !
 ! Ulf Andrae, SMHI, 2008
 !

 USE types, ONLY : statistics
 USE mymagics
 USE functions
 USE timing
 USE data, ONLY : obstype,expname,station_name,                 &
                  csi,lfcver,maxfclenval,                       &
                  lev_lst,ltemp,nfclengths,                     &
                  show_fc_length,tag,                           &
                  show_bias,show_rmse,show_stdv,show_obs,       &
                  len_lab,period_freq,period_type,              &
                  output_type,output_mode,                      &
                  show_times,use_fclen,timdiff,time_shift,      &
                  z_is_pressure,len_lab


 IMPLICIT NONE

 INTEGER,           INTENT(IN) :: lunout,nexp,nlev,nparver,     &
                                  ntimver,stnr,yymm,yymm2,      &
                                  par_active(nparver)
 TYPE (statistics), INTENT(IN) :: s(nexp,nparver,ntimver)

 LOGICAL,           INTENT(IN) :: uh(nparver,0:23),uf(nparver,0:maxfclenval)

! Local

 INTEGER :: i,j,jj,j_ind,k,kk,              &
            ntimver_out,hour(ntimver),      &
            num(nexp,nlev),                 &
            period,jl(1)

 REAL    :: bias(nexp,nlev),       &
            rmse(nexp,nlev),       &
            stdv(nexp,nlev),       &
            rnum(nexp,nlev),       &
             obs(nexp,nlev),       &
            miny,maxy,diff,        &
            rcount_max

 LOGICAL :: legend_done = .FALSE.

 CHARACTER(LEN=100      ) :: wtext =' ',wtext1=' ',my_tag
 CHARACTER(LEN=100      ) :: fname =' ',wname =' '
 CHARACTER(LEN=len_lab  ) :: ob_short=''
 CHARACTER(LEN=7  ) :: cnum_case = '       '
 CHARACTER(LEN=1  ) :: prefix    = ' '
 CHARACTER(LEN=10 ) :: chour    = ' '

 LOGICAL, ALLOCATABLE :: ldum(:)

!------------------------------------------

 IF ( show_obs ) THEN
    show_rmse = .FALSE.
    show_bias = .FALSE.
    show_stdv = .FALSE.
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
 ! Set output filename
 !

 prefix = 'l'
 IF (lfcver) prefix = 'L'
 IF (yymm < 999999 ) THEN
    period = yymm
 ELSE
    period = 0
 ENDIF

 ! Plotting

 DO j=nlev,nparver,nlev
  DO kk=1,ntimver_out

    IF ( ntimver_out /= 1 .AND. .NOT. ANY( show_times == hour(kk) )) CYCLE

     num = 0.
    rnum = 0.
    bias = 0.
    rmse = 0.
    obs  = 0.

    DO i = 1,nexp
    DO jj= 1,nlev
       j_ind = (j/nlev-1)*nlev + jj
       IF ( ntimver_out == 1 ) THEN
        DO k = 1,ntimver
           num(i,jj) =  num(i,jj) +       s(i,j_ind,k)%n
          rnum(i,jj) = rnum(i,jj) + FLOAT(s(i,j_ind,k)%n)
          bias(i,jj) = bias(i,jj) +       s(i,j_ind,k)%bias
          rmse(i,jj) = rmse(i,jj) +       s(i,j_ind,k)%rmse
           obs(i,jj) =  obs(i,jj) +       s(i,j_ind,k)%obs
        ENDDO
       ELSE
           num(i,jj) =  num(i,jj) +       s(i,j_ind,kk)%n
          rnum(i,jj) = rnum(i,jj) + FLOAT(s(i,j_ind,kk)%n)
          bias(i,jj) = bias(i,jj) +       s(i,j_ind,kk)%bias
          rmse(i,jj) = rmse(i,jj) +       s(i,j_ind,kk)%rmse
           obs(i,jj) =  obs(i,jj) +       s(i,j_ind,kk)%obs
       ENDIF
    ENDDO
    ENDDO

    rnum = MAX(1.,rnum)

    stdv = SQRT(ABS(rmse/rnum - (bias/rnum)**2))
    IF ( show_obs ) THEN
       bias = (  bias + obs ) / rnum
       obs  = obs / rnum
    ELSE
       bias = bias / rnum
    ENDIF
    rmse = SQRT(rmse/rnum)

    IF ( ntimver_out == 1 ) THEN
          my_tag = TRIM(tag)//'_ALL'
    ELSE
          chour = ' '
          WRITE(chour,'(I2.2)')hour(kk)
          my_tag = TRIM(tag)//'_'//TRIM(chour)
    ENDIF

    CALL make_fname(prefix,period,stnr,my_tag,    &
                    obstype(j)(1:2),'0',          &
                    output_mode,output_type,      &
                    fname)

    CALL open_output(fname)

    DO jj=nlev,1,-1
       IF ( num(1,jj) == 0 ) CYCLE
       WRITE(lunout,*)lev_lst(jj),bias(:,jj),rmse(:,jj)
    ENDDO

    CLOSE(lunout)

 ENDDO
 ENDDO

 RETURN

END SUBROUTINE print_vert
