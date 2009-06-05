SUBROUTINE print_stat2(lunout,nexp,nparver,ntimver,   &
                       s,stnr,yymm,yymm2,par_active,  &
                       uh,uf)

 USE types, ONLY : statistics
 USE mymagics
 USE functions
 USE timing
 USE data, ONLY : obstype,expname,station_name,                 &
                  csi,use_fclen,lfcver,                         &
                  maxfclenval,len_lab,output_mode,              &
                  nfclengths,nuse_fclen,tag,                    &
                  timdiff,time_shift,show_fc_length,ltiming,    &
                  show_bias,show_rmse,show_stdv,show_obs,       &
                  copied_obs,copied_mod,period_freq,period_type,&
                  output_type

 IMPLICIT NONE

 INTEGER, INTENT(IN) ::           &
 lunout,nexp,nparver,ntimver,     &
 stnr,yymm,yymm2,                 &
 par_active(nparver)

 TYPE (statistics), INTENT(IN) :: s(nexp,nparver,ntimver)

 LOGICAL, INTENT(IN) :: uh(nparver,0:23),uf(nparver,0:maxfclenval)

! Local

 INTEGER :: i,j,k,timing_id,ntimver_l, &
            period,npp

 REAL, TARGET, ALLOCATABLE :: &
            bias(:,:),        &
             obs(:,:),        &
            rmse(:,:),        &
            stdv(:,:),        &
            rnum(:,:)

 INTEGER, ALLOCATABLE :: hour(:)

 TYPE print_pointer
    REAL, POINTER :: v(:)
 END TYPE

 TYPE(print_pointer), ALLOCATABLE :: pdat(:)

 LOGICAL :: legend_done    = .FALSE.
 LOGICAL :: no_data_at_all = .FALSE.

 CHARACTER(LEN=100) :: wtext=' '
 CHARACTER(LEN=100) :: wtext1=' '
 CHARACTER(LEN=100) :: fname=' '
 CHARACTER(LEN= 30) :: wname=' '
 CHARACTER(LEN=  1) :: prefix = ' '
 CHARACTER(LEN=  6) :: ob_short = '      '
 CHARACTER(LEN=30 ) :: cform='   '

!------------------------------------------
 ! Init timing counter
 timing_id = 0
 IF (ltiming) CALL acc_timing(timing_id,'print_stat2')

 IF ( show_obs ) THEN
    show_rmse = .FALSE. 
    show_bias = .FALSE. 
    show_stdv = .FALSE. 
 ENDIF

 ! Set output filename

 prefix = 'v'
 IF (lfcver) prefix = 'V'
 IF (yymm < 999999 ) THEN
    period = yymm
 ELSE
    period = 0
 ENDIF


 ! Set number of hours

 IF (lfcver) THEN
   ntimver_l = nuse_fclen
 ELSE 
   ntimver_l = ntimver + 1
 ENDIF

 ALLOCATE(bias(ntimver_l,nexp),         &
          rmse(ntimver_l,nexp),         &
          stdv(ntimver_l,nexp),         &
          rnum(ntimver_l,nexp),         &
           obs(ntimver_l,nexp),         &
          hour(ntimver_l))

 IF (lfcver) THEN
    hour(1:ntimver_l)=use_fclen(1:ntimver_l)
 ELSE
    DO i=1,ntimver
       hour(i)=(i-1)*timdiff + time_shift
    ENDDO
    hour(ntimver_l) = 24
 ENDIF


 ! Printing

 DO j=1,nparver

    IF ( output_mode == 2 ) THEN
       CALL make_fname(prefix,period,stnr,tag,     &
                       obstype(j)(1:2),            &
                       obstype(j)(3:len_lab),      &
                       output_mode,output_type,    &
                       fname)
       CALL open_output(fname)
    ENDIF

    no_data_at_all = (MAXVAL(s(:,j,:)%n) == 0)

    IF ( no_data_at_all ) THEN

       rnum = 0.
        obs = 0.
       bias = 0.
       rmse = 0.
       stdv = 0.

    ELSE

       DO i=1,nexp
       DO k=1,ntimver
          rnum(k,i) = MAX(1.,float(s(i,j,k)%n))
           obs(k,i) =      s(i,j,k)%obs /rnum(k,i)
          bias(k,i) =      s(i,j,k)%bias/rnum(k,i)
          rmse(k,i) = sqrt(s(i,j,k)%rmse/rnum(k,i))
          stdv(k,i) = sqrt(ABS(s(i,j,k)%rmse/rnum(k,i) - (s(i,j,k)%bias/rnum(k,i))**2))
       ENDDO
       ENDDO

       IF (.NOT.lfcver) THEN
          rnum(ntimver_l,:) = rnum(1,:)
           obs(ntimver_l,:) =  obs(1,:)
          bias(ntimver_l,:) = bias(1,:)
          rmse(ntimver_l,:) = rmse(1,:)
          stdv(ntimver_l,:) = stdv(1,:)
       ENDIF

       IF ( show_obs) bias = bias + obs
    
    ENDIF ! no_data_at_all

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
       IF ( TRIM(tag) /= '#' ) wtext='Area: '//TRIM(tag)//' using '//TRIM(wtext)
    ENDIF
    WRITE(lunout,'(A,X,A)')'#HEADING_1',TRIM(wtext)

    ! Line 2
    IF (yymm == 0 ) THEN
    ELSEIF(yymm < 13) THEN

       SELECT CASE(period_freq) 
       CASE(1)
        WRITE(wtext,'(A8,A8)')'Period: ',seasonal_name2(yymm)
       CASE(3)
        WRITE(wtext,'(A8,A8)')'Period: ',seasonal_name1(yymm)
       END SELECT 

    ELSEIF(yymm < 9999 .OR. (period_type == 2 .AND. period_freq == 1)) THEN
       WRITE(wtext,'(A8,I8)')'Period: ',yymm
    ELSEIF(yymm < 999999 ) THEN
       WRITE(wtext,'(A8,I6,A2,I6)')'Period: ',        &
       yymm,' -',monincr(yymm,period_freq-1)
    ELSE
       WRITE(wtext,'(A8,I8,A1,I8)')'Period: ',        &
       yymm,'-',yymm2
    ENDIF
    WRITE(lunout,'(A,X,A)')'#HEADING_2',TRIM(wtext)

    ! Line 3
    IF ( show_fc_length ) THEN

       CALL pname(obstype(j),wtext)
       CALL fclen_header(( .NOT. lfcver .OR. ( nuse_fclen /= nfclengths )), &
                         maxfclenval,uh(j,:),uf(j,:),wtext1)
       wtext = TRIM(wtext)//'   '//TRIM(wtext1)
       WRITE(lunout,'(A,X,A)')'#HEADING_3',TRIM(wtext)

    ENDIF

    ! Experiments and parameters and norms
    WRITE(lunout,'(A,X,A)')'#PAR',TRIM(obstype(j))

    npp = 0
    IF ( lfcver ) THEN
       IF (show_rmse) npp = nexp
       IF (show_stdv) npp = npp + nexp
       IF (show_bias) npp = npp + nexp
    ELSE
       npp = 1 + nexp
    ENDIF
    ! Add one column for number of cases 
    npp = npp + 1

    IF ( lfcver ) THEN
       WRITE(lunout,'(A,X,I2)')'#NEXP',nexp
    ELSE
       WRITE(lunout,'(A,X,I2)')'#NEXP',nexp+1
       WRITE(lunout,'(A,I2.2X,A)')'#EXP_',0,'OBS'
    ENDIF
    DO i=1,nexp
       WRITE(lunout,'(A,I2.2X,A)')'#EXP_',i,expname(i)
    ENDDO

    ob_short = obstype(j)
    ob_short(3:6) = '   '
    CALL yunit(ob_short,ytitle)
    WRITE(lunout,'(A,X,A)')'#YLABEL',TRIM(ytitle)
    IF ( lfcver ) THEN
       WRITE(lunout,'(A,X,A)')'#XLABEL','Forecast length'
    ELSE
       WRITE(lunout,'(A,X,A)')'#XLABEL','Hour'
    ENDIF

    ! Time to write the parameters
 
    ALLOCATE(pdat(npp))
  
    k = 0
    IF (lfcver) THEN
     IF ( show_rmse ) THEN
      DO i=1,nexp
        k=k+1
        pdat(k)%v => rmse(1:ntimver_l,i)
        WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+1,'RMSE',TRIM(expname(i))
      ENDDO
     ENDIF 
     IF ( show_stdv ) THEN
      DO i=1,nexp
        k=k+1
        pdat(k)%v => stdv(1:ntimver_l,i)
        WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+1,'STDV',TRIM(expname(i))
      ENDDO
     ENDIF 
     IF ( show_bias ) THEN
      DO i=1,nexp
        k=k+1
        pdat(k)%v => bias(1:ntimver_l,i)
        !WRITE(6,*)'BIAS for ',expname(i),k
        WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+1,'BIAS',TRIM(expname(i))
      ENDDO
     ENDIF 
    ELSE 
      DO i=1,nexp
        k=k+1
        pdat(k)%v => bias(1:ntimver_l,i)
        !WRITE(6,*)'BIAS for ',expname(i),k
        WRITE(lunout,'(A,I2.2,X,A)')'#COLUMN_',k+1,TRIM(expname(i))
      ENDDO
      k=k+1
      pdat(k)%v => obs(1:ntimver_l,1)
      WRITE(lunout,'(A,I2.2,X,A)')'#COLUMN_',k+1,'OBS'
    ENDIF
    k=k+1
    pdat(k)%v => rnum(1:ntimver_l,1)
    WRITE(lunout,'(A,I2.2,X,A)')'#COLUMN_',k+1,'CASES'

    ! End of heading
    WRITE(lunout,'(A,X,en15.5e2)')'#MISSING',err_ind
    WRITE(lunout,'(A)')'#END'

    cform = '(I3.2,NN(x,en15.5e2))'
    WRITE(cform(7:8),'(I2.2)')npp

    DO i=1,ntimver_l
        WRITE(lunout,cform)hour(i),(pdat(k)%v(i),k=1,npp)
    ENDDO

    CLOSE(lunout)

    DO i=1,SIZE(pdat)
       NULLIFY(pdat(i)%v)
    ENDDO
    DEALLOCATE(pdat)

 ENDDO

 ! Clear memory
 DEALLOCATE(obs,bias,rmse,stdv,rnum,hour)

 IF (ltiming) CALL acc_timing(timing_id,'print_stat2')

 RETURN

END SUBROUTINE print_stat2
