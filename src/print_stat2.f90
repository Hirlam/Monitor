SUBROUTINE print_stat2(lunout,nexp,nparver,ntimver,   &
                       s,stnr,yymm,yymm2,par_active,  &
                       uh,uf)

 USE types, ONLY : statistics
 USE functions
 USE timing
 USE constants, ONLY : seasonal_name1,seasonal_name2
 USE data, ONLY : varprop,expname,station_name,                 &
                  csi,use_fclen,lfcver,                         &
                  maxfclenval,len_lab,output_mode,              &
                  nfclengths,nuse_fclen,tag,                    &
                  timdiff,time_shift,show_fc_length,ltiming,    &
                  show_bias,show_rmse,show_stdv,show_obs,       &
                  show_var,show_skw,                            &
                  copied_obs,copied_mod,period_freq,period_type,&
                  output_type,lprint_seasonal,err_ind,cini_hours

 IMPLICIT NONE

 INTEGER, INTENT(IN) ::           &
 lunout,nexp,nparver,ntimver,     &
 stnr,yymm,yymm2,                 &
 par_active(nparver)

 TYPE (statistics), INTENT(IN) :: s(nexp,nparver,ntimver)

 LOGICAL, INTENT(IN) :: uh(nparver,0:23),uf(nparver,0:maxfclenval)

! Local

 INTEGER :: i,j,k,timing_id,ntimver_l, &
            period,npp,ntypes,n,case_i

 REAL minnum,maxnum,ticnum,maxnum_t,   &
      zfc,zslask2

 REAL, TARGET, ALLOCATABLE :: &
            bias(:,:),        &
            rmse(:,:),        &
            stdv(:,:),        &
             skw(:,:),        &
           stdvi(:,:),        &
            skwo(:),          &
           stdvo(:),          &
             obs(:),          &
            rnum(:),          &
            snum(:)

 INTEGER, ALLOCATABLE :: hour(:)

 TYPE print_pointer
    REAL, POINTER :: v(:)
 END TYPE

 TYPE(print_pointer), ALLOCATABLE :: pdat(:)

 LOGICAL :: no_data_at_all = .FALSE.

 CHARACTER(LEN=100) :: wtext=' '
 CHARACTER(LEN=100) :: wtext1=' '
 CHARACTER(LEN=100) :: fname=' '
 CHARACTER(LEN= 30) :: wname=' '
 CHARACTER(LEN= 10) :: prefix = ' '
 CHARACTER(LEN=  6) :: ctype(4) = ' '
 CHARACTER(LEN=30 ) :: cform='   '

!------------------------------------------
 ! Init timing counter
 timing_id = 0
 IF (ltiming) CALL acc_timing(timing_id,'print_stat2')

 ! Force settings
 IF ( show_obs ) THEN
    show_rmse = .FALSE. 
    show_bias = .FALSE. 
    show_stdv = .FALSE. 
 ENDIF

 ! Set period

 IF (yymm < 999999 ) THEN
    period = yymm
 ELSE
    period = 0
 ENDIF

 ! Set number of hours

 IF (lfcver) THEN
   IF ( lprint_seasonal ) THEN
      ntimver_l = ntimver
   ELSE
      ntimver_l = nuse_fclen
   ENDIF
 ELSE 
   ntimver_l = ntimver + 1
 ENDIF

 ! Allocate

 ALLOCATE(bias(ntimver_l,nexp),         &
           obs(ntimver_l),              &
          rnum(ntimver_l),              &
          snum(ntimver_l),              &
          hour(ntimver_l))

 IF ( show_rmse .OR. show_stdv .OR. show_skw )          &
 ALLOCATE(rmse(ntimver_l,nexp),                         &
          stdv(ntimver_l,nexp))

 IF ( show_var .OR. show_skw )                          &
 ALLOCATE(stdvi(ntimver_l,nexp),stdvo(ntimver_l),       &
          skw(ntimver_l,nexp),skwo(ntimver_l))


 ! Examine what to do 

 ntypes = 0
 
 IF ( show_obs ) THEN
   ntypes = ntypes + 1 
   ctype(ntypes) = 'OBS'
 ENDIF

 IF ( show_rmse .OR. show_bias .OR. show_stdv ) THEN
   ntypes = ntypes + 1 
   ctype(ntypes) = 'BIAS'
 ENDIF

 IF ( show_var ) THEN
   ntypes = ntypes + 1 
   ctype(ntypes) = 'VAR'
 ENDIF

 IF ( show_skw ) THEN
   ntypes = ntypes + 1 
   ctype(ntypes) = 'SKW'
 ENDIF

 ! Fill the hour array
 IF (lfcver) THEN
    IF ( lprint_seasonal ) THEN
       DO i=1,ntimver
          hour(i)=i
       ENDDO
    ELSE
       hour(1:ntimver_l)=use_fclen(1:ntimver_l)
    ENDIF
 ELSE
    DO i=1,ntimver
       hour(i)=(i-1)*timdiff + time_shift
    ENDDO
    hour(ntimver_l) = 24
 ENDIF


 ! Printing

 DO j=1,nparver

    no_data_at_all = (MAXVAL(s(:,j,:)%n) == 0)

    IF ( no_data_at_all ) THEN

       rnum = 0.
       snum = 0.
        obs = 0.
       bias = 0.
       IF ( show_rmse .OR. show_stdv ) THEN
          rmse = 0.
          stdv = 0.
       ENDIF
       IF ( show_var .OR. show_skw ) THEN
         stdvi = 0.
         stdvo = 0.
           skw = 0.
          skwo = 0.
       ENDIF

    ELSE

       DO k=1,ntimver
          rnum(k) = MAX(1.,FLOAT(s(1,j,k)%n))
          snum(k) = FLOAT(s(1,j,k)%n)
       ENDDO
       DO k=1,ntimver
           obs(k) = s(1,j,k)%obs /rnum(k)
       ENDDO

       DO i=1,nexp
       DO k=1,ntimver
          bias(k,i) =      s(i,j,k)%bias/rnum(k)
       ENDDO
       ENDDO

       IF ( show_rmse .OR. show_stdv ) THEN
          DO i=1,nexp
          DO k=1,ntimver
             rmse(k,i) = SQRT(s(i,j,k)%rmse/rnum(k))
             stdv(k,i) = SQRT(ABS(s(i,j,k)%rmse/rnum(k) - (s(i,j,k)%bias/rnum(k))**2))
          ENDDO
          ENDDO
       ENDIF

       IF ( show_var .OR. show_skw ) THEN
         DO i=1,nexp
         DO k=1,ntimver
           zfc  = bias(k,i)+obs(k)
           stdvi(k,i) = SQRT(ABS(s(i,j,k)%s2/rnum(k) - zfc**2))

           zslask2 = s(i,j,k)%s3/rnum(k) - 3*s(i,j,k)%s2/rnum(k)*zfc + 2*zfc**3

           IF(zslask2 < 0.)then
              skw(k,i) = - (-zslask2)**0.333333
           ELSE
              skw(k,i) = zslask2**0.333333
           ENDIF

           stdvo(k) = SQRT(ABS(s(i,j,k)%obs2/rnum(k) - obs(k)**2))

           zslask2 =   s(i,j,k)%obs3/rnum(k) -   &
                     3*s(i,j,k)%obs2/rnum(k)*obs(k) + 2*obs(k)**3

           IF(zslask2 < 0)then
              skwo(k) = - (-zslask2)**0.333333
           ELSE
              skwo(k) = zslask2**0.333333
           ENDIF

         ENDDO
         ENDDO
       ENDIF

       IF (.NOT.lfcver) THEN

          rnum(ntimver_l) = rnum(1)
           obs(ntimver_l) =  obs(1)
          bias(ntimver_l,:) = bias(1,:)
          snum(ntimver_l) = snum(1)

          IF ( show_rmse .OR. show_stdv ) THEN
            rmse(ntimver_l,:) = rmse(1,:)
            stdv(ntimver_l,:) = stdv(1,:)
          ENDIF

          IF ( show_var .OR. show_skw ) THEN
            stdvi(ntimver_l,:) = stdvi(1,:)
              skw(ntimver_l,:) = skw(1,:)
            stdvo(ntimver_l)   = stdvo(1)
             skwo(ntimver_l)   = skwo(1)
          ENDIF

       ENDIF

       IF ( show_obs) THEN
         DO i=1,nexp
            bias(:,i) = bias(:,i) + obs
         ENDDO
       ENDIF
    
    ENDIF ! no_data_at_all

    ! Set output filename

    PLOT_TYPE : DO n=1,ntypes

    IF (lfcver) THEN
       IF ( lprint_seasonal ) THEN
          prefix = 'Y'
       ELSE
          prefix = 'V'
       ENDIF
    ELSE
       prefix = 'v'
    ENDIF

    SELECT CASE(TRIM(ctype(n)))
    CASE ('VAR','SKW')
       prefix = TRIM(prefix)//'_'//TRIM(ctype(n))
    END SELECT

    IF ( output_mode == 2 ) THEN
       wtext=TRIM(tag)//TRIM(cini_hours)
       CALL make_fname(prefix,period,stnr,         &
                       wtext,                      &
                       varprop(j)%id,              &
                       varprop(j)%lev,             &
                       output_mode,output_type,    &
                       fname)
       CALL open_output(fname)
    ENDIF


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
       IF ( TRIM(tag) /= '#' ) wtext='Selection: '//TRIM(tag)//' using '//TRIM(wtext)
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
    wtext = TRIM(varprop(j)%text)//'   '//TRIM(wtext)
    WRITE(lunout,'(A,X,A)')'#HEADING_2',TRIM(wtext)

    ! Line 3
    IF ( show_fc_length ) THEN

       CALL fclen_header(( .NOT. lfcver .OR. ( nuse_fclen /= nfclengths )), &
                         maxfclenval,uh(j,:),uf(j,:),varprop(j)%acc,wtext1)
       WRITE(lunout,'(A,X,A)')'#HEADING_3',TRIM(wtext1)

    ENDIF


    ! Experiments and parameters and norms
    WRITE(lunout,'(A,X,A)')'#PAR',TRIM(varprop(j)%id)

    npp = 0
    SELECT CASE(TRIM(ctype(n)))
    CASE('OBS')
       npp =   1 + nexp
    CASE('BIAS')
       IF (show_rmse) npp = npp + nexp
       IF (show_stdv) npp = npp + nexp
       IF (show_bias) npp = npp + nexp
    CASE('VAR')
       npp = 1 + nexp + npp
    CASE('SKW')
       npp = 1 + nexp + npp
    END SELECT

    ! Add one column for number of cases 
    npp = npp + 1

    IF ( lfcver ) THEN
       IF ( lprint_seasonal .OR. TRIM(ctype(n)) /= 'BIAS' ) THEN
          WRITE(lunout,'(A,X,I2)')'#NEXP',nexp+1
          WRITE(lunout,'(A,I2.2,X,A)')'#EXP_',0,'OBS'
       ELSE
          WRITE(lunout,'(A,X,I2)')'#NEXP',nexp
       ENDIF
    ELSE
       WRITE(lunout,'(A,X,I2)')'#NEXP',nexp+1
       WRITE(lunout,'(A,I2.2,X,A)')'#EXP_',0,'OBS'
    ENDIF

    DO i=1,nexp
       WRITE(lunout,'(A,I2.2,X,A)')'#EXP_',i,expname(i)
    ENDDO

    WRITE(lunout,'(A,X,A)')'#YLABEL',TRIM(varprop(j)%unit)
    IF ( lfcver ) THEN
       IF ( lprint_seasonal ) THEN
          WRITE(lunout,'(A,X,A)')'#XLABEL','Day of year'
       ELSE
          WRITE(lunout,'(A,X,A)')'#XLABEL','Forecast length'
       ENDIF
    ELSE
       WRITE(lunout,'(A,X,A)')'#XLABEL','Hour'
    ENDIF

    ! Time to write the parameters
 
    ALLOCATE(pdat(npp))
  
    k = 0
    SELECT CASE(TRIM(ctype(n)))

    CASE('BIAS')

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
        WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+1,'BIAS',TRIM(expname(i))
      ENDDO
     ENDIF 

     CASE('OBS')
      DO i=1,nexp
        k=k+1
        pdat(k)%v => bias(1:ntimver_l,i)
        WRITE(lunout,'(A,I2.2,X,A)')'#COLUMN_',k+1,TRIM(expname(i))
      ENDDO
      k=k+1
      pdat(k)%v => obs(1:ntimver_l)
      WRITE(lunout,'(A,I2.2,X,A)')'#COLUMN_',k+1,'OBS'

     CASE('VAR')

      DO i=1,nexp
        k=k+1
        pdat(k)%v => stdvi(1:ntimver_l,i)
        WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+1,'STDV',TRIM(expname(i))
      ENDDO
      k=k+1
      pdat(k)%v => stdvo(1:ntimver_l)
      WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+1,'STDV','OBS'

     CASE('SKW')
      DO i=1,nexp
        k=k+1
        pdat(k)%v => skw(1:ntimver_l,i)
        WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+1,'SKW',TRIM(expname(i))
      ENDDO
      k=k+1
      pdat(k)%v => skwo(1:ntimver_l)
      WRITE(lunout,'(A,I2.2,2(X,A))')'#COLUMN_',k+1,'SKW','OBS'

    END SELECT

    k=k+1
    pdat(k)%v => snum(1:ntimver_l)
    case_i = k
    WRITE(lunout,'(A,I2.2,X,A)')'#COLUMN_',k+1,'CASES'

    minnum = MINVAL(rnum(1:ntimver_l))
    maxnum_t = MAXVAL(rnum(1:ntimver_l))
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

    ! End of heading
    WRITE(lunout,'(A,X,en15.5e2)')'#MISSING',err_ind
    WRITE(lunout,'(A)')'#END'

    cform = '(I3.2,NN(x,en15.5e2))'
    WRITE(cform(7:8),'(I2.2)')npp

    
    DO i=1,ntimver_l
        IF ( pdat(case_i)%v(i) < 1. ) THEN
          DO k=1,npp
             pdat(k)%v(i) = err_ind
          ENDDO
        ENDIF
        WRITE(lunout,cform)hour(i),(pdat(k)%v(i),k=1,npp)
    ENDDO

    CLOSE(lunout)

    DO i=1,SIZE(pdat)
       NULLIFY(pdat(i)%v)
    ENDDO
    DEALLOCATE(pdat)

    ENDDO PLOT_TYPE

 ENDDO

 ! Clear memory
 IF(ALLOCATED(obs)  ) DEALLOCATE(obs)
 IF(ALLOCATED(bias) ) DEALLOCATE(bias)
 IF(ALLOCATED(rmse) ) DEALLOCATE(rmse)
 IF(ALLOCATED(stdv) ) DEALLOCATE(stdv)
 IF(ALLOCATED(rnum) ) DEALLOCATE(rnum)
 IF(ALLOCATED(snum) ) DEALLOCATE(snum)
 IF(ALLOCATED(hour) ) DEALLOCATE(hour)
 IF(ALLOCATED(skwo) ) DEALLOCATE(skwo)
 IF(ALLOCATED(skw)  ) DEALLOCATE(skw)
 IF(ALLOCATED(stdvi)) DEALLOCATE(stdvi)
 IF(ALLOCATED(stdvo)) DEALLOCATE(stdvo)

 IF (ltiming) CALL acc_timing(timing_id,'print_stat2')

 RETURN

END SUBROUTINE print_stat2
