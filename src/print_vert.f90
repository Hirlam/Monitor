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
 USE data, ONLY : varprop,expname,station_name,                 &
                  csi,lfcver,maxfclenval,                       &
                  lev_lst,ltemp,nfclengths,                     &
                  show_fc_length,tag,                           &
                  show_bias,show_rmse,show_stdv,show_obs,       &
                  show_mabe,                                    &
                  len_lab,period_freq,period_type,              &
                  output_type,output_mode,                      &
                  show_times,use_fclen,timdiff,time_shift,      &
                  len_lab,err_ind,cini_hours,exp_offset,        &
                  prefix_len,plot_prefix


 IMPLICIT NONE

 INTEGER,           INTENT(IN) :: lunout,nexp,nlev,nparver,     &
                                  ntimver,stnr,yymm,yymm2,      &
                                  rar_active(nparver,0:ntimver)
 TYPE (statistics), INTENT(IN) :: s(nexp,nparver,ntimver)

 LOGICAL,           INTENT(IN) :: uh(nparver,0:23),uf(nparver,0:maxfclenval)

! Local

 INTEGER :: i,j,l,m,jj,j_ind,k,kk,kkk,      &
            ntimver_out,hour(ntimver),      &
            num(nexp,nlev),                 &
            period,jl(1),npp

 REAL    :: minnum,maxnum,ticnum,maxnum_t

 REAL, TARGET ::                   &
            bias(nexp,nlev),       &
            mabe(nexp,nlev),       &
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
 CHARACTER(LEN=prefix_len) :: prefix    = ' '
 CHARACTER(LEN=10 ) :: chour    = ' '
 CHARACTER(LEN=30 ) :: cform='   '

 LOGICAL, ALLOCATABLE :: ldum(:)
 LOGICAL :: luh(0:23),lluh

!------------------------------------------

 !
 ! Unify settings
 !
 IF ( show_obs ) THEN
    show_rmse = .FALSE.
    show_mabe = .FALSE.
    show_bias = .FALSE.
    show_stdv = .FALSE.
 ENDIF

 IF ( ALL(show_times == -1) ) THEN
   ntimver_out = 1
 ELSE
   ntimver_out = TRANSFER(MINLOC(show_times),ntimver_out) - 1
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

 IF ( lfcver) THEN
   prefix = plot_prefix(5)
 ELSE
   prefix = plot_prefix(6)
 ENDIF
 
 IF (yymm < 999999 ) THEN
    period = yymm
 ELSE
    period = 0
 ENDIF

 ! Plotting

 DO j=nlev,nparver,nlev
  DO m=1,ntimver_out

    luh = uh(j,:)

    kk=0
    DO l=1,SIZE(hour) 
     IF ( show_times(m) == hour(l) ) kk=l
    ENDDO
    IF ( ntimver_out /= 1 .AND. kk==0 .AND. show_times(m) /= 99 ) CYCLE

     num = 0
    rnum = 0.
    mabe = 0.
    bias = 0.
    rmse = 0.
    obs  = 0.

    DO i = 1,nexp
    DO jj= 1,nlev
       j_ind = (j/nlev-1)*nlev + jj
       IF ( ntimver_out == 1 .OR. show_times(m)==99 ) THEN
        DO k = 1,ntimver
           num(i,jj) =  num(i,jj) +       s(i,j_ind,k)%n
          rnum(i,jj) = rnum(i,jj) + FLOAT(s(i,j_ind,k)%n)
          bias(i,jj) = bias(i,jj) +       s(i,j_ind,k)%bias
          mabe(i,jj) = mabe(i,jj) +       s(i,j_ind,k)%mabe
          rmse(i,jj) = rmse(i,jj) +       s(i,j_ind,k)%rmse
           obs(i,jj) =  obs(i,jj) +       s(i,j_ind,k)%obs
        ENDDO
       ELSE
           num(i,jj) =  num(i,jj) +       s(i,j_ind,kk)%n
          rnum(i,jj) = rnum(i,jj) + FLOAT(s(i,j_ind,kk)%n)
          bias(i,jj) = bias(i,jj) +       s(i,j_ind,kk)%bias
          mabe(i,jj) = mabe(i,jj) +       s(i,j_ind,kk)%mabe
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
       mabe = mabe / rnum
    ENDIF
    rmse = SQRT(rmse/rnum)

    IF ( ntimver_out == 1 .OR. show_times(m)==99 ) THEN
          my_tag = TRIM(tag)//TRIM(cini_hours)//'_ALL'
    ELSE
          chour = ' '
          WRITE(chour,'(I2.2)')hour(kk)
          my_tag = TRIM(tag)//TRIM(cini_hours)//'_'//TRIM(chour)
    ENDIF

    CALL make_fname(prefix,period,stnr,my_tag,    &
                    varprop(j)%id,0,              &
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
       IF ( ntimver_out == 1 .OR. show_times(m)==99 ) THEN
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
       WRITE(wtext1,'(A8,I6,A1,I6)')'Period: ',        &
       yymm,'-',monincr(yymm,period_freq-1)
    ELSE
       WRITE(wtext1,'(A8,I8,A1,I8)')'Period: ',        &
       yymm,'-',yymm2
    ENDIF

    jj=INDEX(TRIM(varprop(j)%text),' ',.TRUE.)
    wtext = TRIM(varprop(j)%text(1:jj))//'  '//TRIM(wtext1)
    WRITE(lunout,'(A,X,A)')'#HEADING_2',TRIM(wtext)

    ! Line 3
    ! First find correct index for fclenth usage

    j_ind = (j/nlev-1)*nlev + 1
    IF ( ntimver_out == 1 .OR. show_times(m)==99 ) THEN
       jl = 0
       DO kkk=1,ntimver
          jl = MAX(jl,MAXLOC(rar_active(j_ind:j_ind+nlev-1,kkk)) + j_ind - 1)
       ENDDO
    ELSE
          jl = MAXLOC(rar_active(j_ind:j_ind+nlev-1,kk)) + j_ind - 1
    ENDIF

    IF ( ntimver_out == 1 .OR. show_times(m)==99 ) THEN
       CALL fclen_header(.TRUE.,maxfclenval,luh,uf(jl(1),:), &
                         varprop(jl(1))%acc,MAXVAL(exp_offset),wtext)
    ELSE
       IF (lfcver) THEN
          ALLOCATE(ldum(0:hour(kk)))
          ldum           = .FALSE.
          ldum(hour(kk)) = .TRUE.
          CALL fclen_header(.TRUE.,hour(kk),luh,ldum,varprop(j)%acc, &
                            MAXVAL(exp_offset),wtext)
          DEALLOCATE(ldum)
       ELSE
          ALLOCATE(ldum(0:maxfclenval))
          ldum = .FALSE.
          luh  = .FALSE.
          WRITE(chour,'(I2.2,1X,A3)')hour(kk),'UTC'
          DO k=0,23
           lluh = .FALSE.
           DO jj= 1,nlev
              j_ind = (j/nlev-1)*nlev + jj
              lluh = lluh .OR. uh(j_ind,k)
           ENDDO
            IF ( .NOT. lluh ) CYCLE
            DO l=0,maxfclenval
             lluh = .FALSE.
             DO jj= 1,nlev
              j_ind = (j/nlev-1)*nlev + jj
              lluh = lluh .OR. uf(j_ind,l)
             ENDDO
              IF ( .NOT. lluh ) CYCLE
              IF( MOD(k + l,24) == hour(kk)) THEN 
                luh(k) = .TRUE.
                ldum(l) = .TRUE.
              ENDIF
            ENDDO
          ENDDO
          CALL fclen_header(.TRUE.,maxfclenval,luh,ldum,varprop(j)%acc, &
                            MAXVAL(exp_offset),wtext)
          wtext = 'Statistics at '//TRIM(chour)//'  '//TRIM(wtext)
          DEALLOCATE(ldum)
       ENDIF
    ENDIF
    WRITE(lunout,'(A,X,A)')'#HEADING_3',TRIM(wtext)

    ! Experiments and parameters and norms
    WRITE(lunout,'(A,X,A)')'#PAR',TRIM(varprop(j)%id)

    npp = 0

    IF (show_obs ) npp = nexp + 1 
    IF (show_rmse) npp = nexp
    IF (show_stdv) npp = npp + nexp
    IF (show_bias) npp = npp + nexp
    IF (show_mabe) npp = npp + nexp

    ! Add one column for number of cases 
    npp = npp + 1

    IF ( show_obs ) THEN
       WRITE(lunout,'(A,X,I2)')'#NEXP',nexp+1
       WRITE(lunout,'(A,I2.2,X,A)')'#EXP_',0,'OBS'
    ELSE
       WRITE(lunout,'(A,X,I2)')'#NEXP',nexp
    ENDIF
    DO i=1,nexp
       WRITE(lunout,'(A,I2.2,X,A)')'#EXP_',i,expname(i)
    ENDDO

    WRITE(lunout,'(A,X,A)')'#YLABEL','hPa'
    WRITE(lunout,'(A,X,A)')'#XLABEL',TRIM(varprop(j)%unit)

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
    IF ( show_mabe ) THEN
      DO i=1,nexp
        k=k+1
        pdat(k)%v => mabe(i,1:nlev)
        WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+1,'MAE',TRIM(expname(i))
      ENDDO
    ENDIF 
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
