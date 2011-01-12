SUBROUTINE print_vert(lunout,nexp,nlev,nparver,ntimver,     &
                      s,stnr,yymm,yymm2,rar_active,uh,uf)

 !
 ! Print vertical profile of RMS,STD and BIAS 
 ! Vertical coordinate is assumed to be pressure
 !
 ! Ulf Andrae, SMHI, 2008
 !

 USE types, ONLY : statistics
 USE functions
 USE timing
 USE constants, ONLY : seasonal_name1,seasonal_name2
 USE data, ONLY : obstype,expname,station_name,                 &
                  csi,lfcver,maxfclenval,                       &
                  lev_lst,ltemp,nfclengths,                     &
                  show_fc_length,tag,                           &
                  show_bias,show_rmse,show_stdv,show_obs,       &
                  len_lab,period_freq,period_type,              &
                  output_type,output_mode,                      &
                  show_times,use_fclen,timdiff,time_shift,      &
                  z_is_pressure,len_lab,accu_int,err_ind


 IMPLICIT NONE

 INTEGER,           INTENT(IN) :: lunout,nexp,nlev,nparver,     &
                                  ntimver,stnr,yymm,yymm2,      &
                                  rar_active(nparver,ntimver)
 TYPE (statistics), INTENT(IN) :: s(nexp,nparver,ntimver)

 LOGICAL,           INTENT(IN) :: uh(nparver,0:23),uf(nparver,0:maxfclenval)

! Local

 INTEGER :: i,j,jj,j_ind,k,kk,kkk,          &
            ntimver_out,hour(ntimver),      &
            num(nexp,nlev),                 &
            period,jl(1),npp

 REAL    :: minnum,maxnum,ticnum,maxnum_t

 REAL, TARGET ::                   &
            bias(nexp,nlev),       &
            rmse(nexp,nlev),       &
            stdv(nexp,nlev),       &
            rnum(nexp,nlev),       &
             obs(nexp,nlev)

 TYPE print_pointer
    REAL, POINTER :: v(:)
 END TYPE

 TYPE(print_pointer), ALLOCATABLE :: pdat(:)

 CHARACTER(LEN=100      ) :: wtext =' ',wtext1=' ',my_tag
 CHARACTER(LEN=100      ) :: fname =' '
 CHARACTER(LEN=len_lab  ) :: ob_short=''
 CHARACTER(LEN=1  ) :: prefix    = ' '
 CHARACTER(LEN=10 ) :: chour    = ' '
 CHARACTER(LEN=30 ) :: ytitle=''
 CHARACTER(LEN=30 ) :: cform='   '

 LOGICAL, ALLOCATABLE :: ldum(:)

!------------------------------------------

 !
 ! Unify settings
 !
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

    !
    ! Create headings
    !

    ! Line 1
    IF(ALLOCATED(station_name)) THEN
       wtext='Station: '//trim(station_name(csi))
    ELSE
       WRITE(wtext(1:8),'(I8)')stnr
       wtext='Station: '//trim(wtext(1:8))
    ENDIF

    IF (stnr == 0) THEN
       wtext='Statistics for      stations'
       j_ind = (j/nlev-1)*nlev + 1
       IF ( ntimver_out == 1 ) THEN
          jj = MAXVAL(rar_active(j_ind:j_ind+nlev-1,:))
       ELSE
          jj = MAXVAL(rar_active(j_ind:j_ind+nlev-1,kk))
       ENDIF
       WRITE(wtext(1:4),'(I4)')jj
       wtext=TRIM(wtext(1:4))//' stations'
       IF ( TRIM(tag) /= '#' ) wtext=TRIM(wtext)//' Selection: '//TRIM(tag)
    ENDIF
    WRITE(lunout,'(A,X,A)')'#HEADING_1',TRIM(wtext)
    
    ! Line 2
    ob_short = obstype(j)
    ob_short(3:6) = '    '
    CALL pname(ob_short,wtext)
     
    ! Line 2
    IF ( yymm == 0 ) THEN
    ELSEIF(yymm < 13) THEN

       SELECT CASE(period_freq) 
       CASE(1)
        WRITE(wtext1,'(A8,A8)')'Period: ',seasonal_name2(yymm)
       CASE(3)
        WRITE(wtext1,'(A8,A8)')'Period: ',seasonal_name1(yymm)
       END SELECT 

       ELSEIF(yymm < 9999 .OR. ( period_type == 2 .AND. period_freq == 1)) THEN
       WRITE(wtext1,'(A8,I6)')'Period: ',yymm
    ELSEIF(yymm < 999999 ) THEN
       WRITE(wtext1,'(A8,I6,A2,I6)')'Period: ',        &
       yymm,' -',monincr(yymm,period_freq-1)
    ELSE
       WRITE(wtext1,'(A8,I8,A1,I8)')'Period: ',        &
       yymm,'-',yymm2
    ENDIF
    wtext = TRIM(wtext)//'  '//TRIM(wtext1)
    WRITE(lunout,'(A,X,A)')'#HEADING_2',TRIM(wtext)

    ! Line 3
    ! First find correct index for fclenth usage

    j_ind = (j/nlev-1)*nlev + 1
    IF ( ntimver_out == 1 ) THEN
       jl = 0
       DO kkk=1,ntimver
          jl = MAX(jl,MAXLOC(rar_active(j_ind:j_ind+nlev-1,kkk)) + j_ind - 1)
       ENDDO
    ELSE
          jl = MAXLOC(rar_active(j_ind:j_ind+nlev-1,kk)) + j_ind - 1
    ENDIF

    IF ( ntimver_out == 1 ) THEN
       CALL fclen_header(.TRUE.,maxfclenval,uh(jl,:),uf(jl,:),accu_int(jl),wtext)
    ELSE
       IF (lfcver) THEN
          ALLOCATE(ldum(0:hour(kk)))
          ldum           = .FALSE.
          ldum(hour(kk)) = .TRUE.
          CALL fclen_header(.TRUE.,hour(kk),uh(jl,:),ldum,accu_int(j),wtext)
          DEALLOCATE(ldum)
       ELSE
          WRITE(chour,'(I3.2,X,A3)')hour(kk),'UTC'
          CALL fclen_header(.TRUE.,maxfclenval,uh(jl,:),uf(jl,:),accu_int(j),wtext)
          wtext = 'Statistics at '//chour   //'  '//TRIM(wtext)
       ENDIF
    ENDIF
    WRITE(lunout,'(A,X,A)')'#HEADING_3',TRIM(wtext)

    ! Experiments and parameters and norms
    WRITE(lunout,'(A,X,A)')'#PAR',TRIM(obstype(j))

    npp = 0

    IF (show_obs ) npp = nexp + 1 
    IF (show_rmse) npp = nexp
    IF (show_stdv) npp = npp + nexp
    IF (show_bias) npp = npp + nexp

    ! Add one column for number of cases 
    npp = npp + 1

    IF ( show_obs ) THEN
       WRITE(lunout,'(A,X,I2)')'#NEXP',nexp+1
       WRITE(lunout,'(A,I2.2X,A)')'#EXP_',0,'OBS'
    ELSE
       WRITE(lunout,'(A,X,I2)')'#NEXP',nexp
    ENDIF
    DO i=1,nexp
       WRITE(lunout,'(A,I2.2X,A)')'#EXP_',i,expname(i)
    ENDDO

    ob_short = obstype(j)
    ob_short(3:6) = '   '
    CALL yunit(ob_short,ytitle)
    WRITE(lunout,'(A,X,A)')'#YLABEL','hPa'
    WRITE(lunout,'(A,X,A)')'#XLABEL',TRIM(ytitle)

    ! Time to write the parameters
 
    ALLOCATE(pdat(npp))
  
    k = 0
    IF ( show_rmse ) THEN
      DO i=1,nexp
        k=k+1
        pdat(k)%v => rmse(i,1:nlev)
        WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+1,'RMSE',TRIM(expname(i))
      ENDDO
    ENDIF 
    IF ( show_stdv ) THEN
      DO i=1,nexp
        k=k+1
        pdat(k)%v => stdv(i,1:nlev)
        WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+1,'STDV',TRIM(expname(i))
      ENDDO
    ENDIF 
    IF ( show_bias ) THEN
      DO i=1,nexp
        k=k+1
        pdat(k)%v => bias(i,1:nlev)
        WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+1,'BIAS',TRIM(expname(i))
      ENDDO
    ENDIF 
    IF ( show_obs ) THEN
      DO i=1,nexp
        k=k+1
        pdat(k)%v => bias(i,1:nlev)
        WRITE(lunout,'(A,I2.2,X,A)')'#COLUMN_',k+1,TRIM(expname(i))
      ENDDO
      k=k+1
      pdat(k)%v => obs(1,1:nlev)
      WRITE(lunout,'(A,I2.2,X,A)')'#COLUMN_',k+1,'OBS'
    ENDIF
    k=k+1
    pdat(k)%v => rnum(1,1:nlev)
    WRITE(lunout,'(A,I2.2,X,A)')'#COLUMN_',k+1,'CASES'


    minnum = MINVAL(rnum(1,1:nlev))
    maxnum_t = MAXVAL(rnum(1,1:nlev))
    minnum = FLOOR(LOG10(MAX(minnum,1.)))
    maxnum = FLOOR(LOG10(MAX(maxnum_t,1.)))
    minnum = 10.**(minnum)
    IF ( minnum < 10. ) minnum = 0.
    maxnum = 10.**(maxnum)
    maxnum = CEILING(maxnum_t/maxnum)*maxnum
    ticnum = tics(minnum,maxnum)

    WRITE(lunout,'(A,X,en15.5e2)')'#MINNUM',minnum
    WRITE(lunout,'(A,X,en15.5e2)')'#TICNUM',ticnum
    WRITE(lunout,'(A,X,en15.5e2)')'#MAXNUM',maxnum

    ! End of headings
    WRITE(lunout,'(A,X,en15.5e2)')'#MISSING',err_ind
    WRITE(lunout,'(A)')'#END'

    cform = '(f7.0,NN(x,en15.5e2))'
    WRITE(cform(7:8),'(I2.2)')npp

    DO jj=nlev,1,-1
       IF ( num(1,jj) == 0 ) CYCLE
       WRITE(lunout,cform)lev_lst(jj),(pdat(k)%v(jj),k=1,npp)
    ENDDO

    CLOSE(lunout)

    DO i=1,SIZE(pdat)
       NULLIFY(pdat(i)%v)
    ENDDO
    DEALLOCATE(pdat)

 ENDDO
 ENDDO

 RETURN

END SUBROUTINE print_vert
