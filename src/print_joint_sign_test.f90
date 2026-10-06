SUBROUTINE print_joint_sign_test(lunout,nexp,nparver,domain, &
                       stnr,yymm,yymm2,par_active,      &
                       uh,uf)

 USE, INTRINSIC :: ieee_arithmetic, ONLY : ieee_is_finite
 USE types, ONLY : statistics
 USE functions
 USE constants, ONLY : seasonal_name1,seasonal_name2
 USE sign_data, ONLY : all_sign_stat,sign_stat_max
 USE data, ONLY : varprop,expname,station_name,                 &
                  csi,use_fclen,lfcver,                         &
                  maxfclenval,len_lab,output_mode,              &
                  nfclengths,nuse_fclen,tag,                    &
                  time_shift,show_fc_length,                    &
                  period_freq,period_type,                      &
                  output_type,lprint_seasonal,                  &
                  control_exp_nr,sign_time_diff,err_ind,confint,&
                  cini_hours,exp_offset,plot_prefix,fexpname

 IMPLICIT NONE

 INTEGER, INTENT(IN) ::           &
 lunout,nexp,nparver,             &
 stnr,yymm,yymm2,                 &
 par_active(nparver)

 CHARACTER(LEN=4), INTENT(IN) :: domain

 LOGICAL, INTENT(IN) :: uh(nparver,0:23),uf(nparver,0:maxfclenval)

! Local

 INTEGER :: i,j,k,period,ncases(nuse_fclen),istart,iend
 INTEGER :: score_env_status,score_env_length

 REAL, ALLOCATABLE :: sdiff(:,:)
 REAL minnum,maxnum,ticnum,maxnum_t,offset(nexp)

 CHARACTER(LEN=100) :: wtext=' ',wtext1=' '
 CHARACTER(LEN=300) :: expnames=''
 CHARACTER(LEN=200) :: fname=' '
 CHARACTER(LEN= 60) :: wname=' '
 CHARACTER(LEN= 10) :: prefix = ' '
 CHARACTER(LEN=512) :: scorefile
 CHARACTER(LEN=100) :: safe_tag,safe_ini,label
 INTEGER :: scoreunit,scoreios,lead_hour
 LOGICAL :: score_exists,scorecard_export

!------------------------------------------

 ! Set period

 IF (yymm < 999999 ) THEN
    period = yymm
 ELSE
    period = 0
 ENDIF

 ! Select a subsection of our period
 IF ( yymm /= 0 ) THEN

    istart = 1
    iend   = sign_stat_max
    DO i=1,sign_stat_max
       IF ((all_sign_stat(i)%date/100 - yymm) == 0 ) THEN
          istart   = i 
          EXIT
       ENDIF
    ENDDO

    DO i=1,sign_stat_max
       IF ((all_sign_stat(i)%date/100 - yymm2) == 0 ) THEN
          iend   = i-1
          EXIT
       ENDIF
    ENDDO
 ELSE
    istart = 1
    iend   = sign_stat_max
 ENDIF

 ! Set number of hours

 ALLOCATE(sdiff(nuse_fclen,2))

 ! Only write the auxiliary text export when the scorecard stage requested it.
 scorecard_export=.FALSE.
 CALL GET_ENVIRONMENT_VARIABLE('SCORECARD_STAGE',LENGTH=score_env_length, &
                               STATUS=score_env_status)
 IF (score_env_status == 0 .AND. score_env_length > 0) scorecard_export=.TRUE.

 ! Printing
 j=0
 expnames=''
 DO i=1,nexp
   IF ( i == control_exp_nr ) CYCLE
   expnames = TRIM(expnames)//' '//TRIM(expname(i))
   j=j+1
   offset(i) = 0.125*(j-nexp+FLOOR(nexp/2.))
 ENDDO
 offset(control_exp_nr ) = 0.0
 expnames = adjustl(expnames)

 DO j=1,nparver
   DO i=nexp,1,-1

      IF ( i /= control_exp_nr ) &
      CALL  scorediffs(control_exp_nr,i,nuse_fclen,j,     &
                       istart,iend,                       &
                       .TRUE.,.FALSE.,confint,sdiff,ncases)

      IF ( i /= control_exp_nr .AND. scorecard_export ) THEN
         CALL write_scorecard_rows(i,j)
      ENDIF
    
      ! Set output filename

      wname = ''
      IF ( i /= control_exp_nr ) THEN
         prefix = 'sub_'//TRIM(plot_prefix(14))
      ELSE
         prefix = TRIM(plot_prefix(14))
      ENDIF
      IF ( TRIM(tag) /= '#' ) &
      wname = TRIM(tag)//TRIM(cini_hours)

      IF ( output_mode == 2 ) THEN
         CALL make_fname(prefix,period,stnr,wname,   &
                         varprop(j)%id,              &
                         varprop(j)%lev,             &
                         output_mode,output_type,    &
                         fname)
         IF ( i /= control_exp_nr ) &
         fname  = TRIM(fname)//'_'//TRIM(expname(i))
         CALL open_output(fname)
      ENDIF

      minnum = MINVAL(ncases)
      maxnum_t = MAXVAL(ncases)
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

    IF ( i == control_exp_nr ) &
    WRITE(lunout,'(A,X,A)')'#EXPNAMES',TRIM(expnames)

    ! Create headers
 
    ! Line 1
    WRITE(wname(1:2),'(I2)')NINT(confint)
    wtext = 'Normalized mean RMSE diff ('//wname(1:2)//'% conf)'
    wtext = TRIM(wtext)//' vs '//TRIM(expname(control_exp_nr))
    IF ( sign_time_diff /= -1 ) THEN
     WRITE(wtext1(1:1),'(I1)')sign_time_diff
     wtext = TRIM(wtext)//' with acc int of '//wtext1(1:1)//' days'
    ENDIF 
    WRITE(lunout,'(A,X,A)')'#HEADING_1',TRIM(wtext)
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
    WRITE(lunout,'(A,X,A)')'#HEADING_2',TRIM(wtext)

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
       WRITE(wtext,'(A8,I6,A1,I6)')'Period: ',        &
       yymm,'-',monincr(yymm,period_freq-1)
    ELSE
       WRITE(wtext,'(A8,I8,A1,I8)')'Period: ',        &
       yymm,'-',yymm2
    ENDIF
    WRITE(lunout,'(A,X,A)')'#HEADING_3',TRIM(wtext)

    ! Line 3
    IF ( show_fc_length ) THEN

       CALL fclen_header(( .NOT. lfcver .OR. ( nuse_fclen /= nfclengths )), &
                         maxfclenval,uh(j,:),uf(j,:),varprop(j)%acc,        &
                         MAXVAL(exp_offset),wtext1)
       wtext = TRIM(varprop(j)%text)//'   '//TRIM(wtext1)
       WRITE(lunout,'(A,X,A)')'#HEADING_4',TRIM(wtext)

    ENDIF

    ! Experiments and parameters and norms
    WRITE(lunout,'(A,X,A)')'#PAR',TRIM(varprop(j)%id)

    WRITE(lunout,'(A,X,A)')'#YLABEL',''
    IF ( lfcver ) THEN
          WRITE(lunout,'(A,X,A)')'#XLABEL','Forecast length'
    ENDIF

    ! Time to write the parameters
 
    ! End of heading
    WRITE(lunout,'(A,X,en15.5e2)')'#MISSING',err_ind
    WRITE(lunout,'(A)')'#END'

    DO k=1,nuse_fclen
      IF ( ncases(k) == 0 ) CYCLE
      WRITE(lunout,'(3(en15.5e2),I7)')offset(i)+use_fclen(k),sdiff(k,:),ncases(k)
    ENDDO

    CLOSE(lunout)

 ENDDO
 ENDDO

 ! Clear memory
 DEALLOCATE(sdiff)

 RETURN

CONTAINS

SUBROUTINE write_scorecard_rows(comparison,jpar)

 INTEGER, INTENT(IN) :: comparison,jpar
 INTEGER :: kk,lev,label_length,suffix_length
 CHARACTER(LEN=30) :: period_text,station_text,ref_text,cmp_text
 CHARACTER(LEN=100) :: raw_label
 CHARACTER(LEN=20) :: level_suffix

 safe_tag = safe_component(TRIM(tag))
 IF (TRIM(tag) == '#') safe_tag='ALL'
 safe_ini = safe_component(TRIM(cini_hours))
 IF (LEN_TRIM(safe_ini) > 0) THEN
    IF (safe_ini(1:1) == '_') safe_ini=safe_ini(2:)
 ENDIF
 IF (safe_ini == '') safe_ini='ALL'
 period_text=''
 station_text=''
 ref_text=''
 cmp_text=''
 WRITE(period_text,'(I0)') period
 WRITE(station_text,'(I0)') stnr
 WRITE(ref_text,'(I0)') control_exp_nr
 WRITE(cmp_text,'(I0)') comparison

 scorefile='joint_scores/joint_scores_'//TRIM(domain)//'_ref'// &
   TRIM(ref_text)//'_cmp'//TRIM(cmp_text)//'_period'//TRIM(period_text)// &
   '_station'//TRIM(station_text)//'_selection_'//TRIM(safe_tag)// &
   '_initial_'//TRIM(safe_ini)//'.txt'

 INQUIRE(FILE=TRIM(scorefile),EXIST=score_exists)
 IF (score_exists) THEN
    OPEN(NEWUNIT=scoreunit,FILE=TRIM(scorefile),STATUS='OLD', &
         POSITION='APPEND',ACTION='WRITE',IOSTAT=scoreios)
 ELSE
    OPEN(NEWUNIT=scoreunit,FILE=TRIM(scorefile),STATUS='NEW', &
         ACTION='WRITE',IOSTAT=scoreios)
 ENDIF
 IF (scoreios /= 0) THEN
    WRITE(6,*)'Could not open scorecard export for comparison ',comparison, &
              ': ',TRIM(scorefile),' IOSTAT=',scoreios
    STOP 1
 ENDIF

 IF (.NOT. score_exists) THEN
    WRITE(scoreunit,'(A)') '#schema_version=1'
    WRITE(scoreunit,'(A,A)') '#domain=',TRIM(domain)
    WRITE(scoreunit,'(A,I0)') '#reference_index=',control_exp_nr
    WRITE(scoreunit,'(A,A)') '#reference_id=',TRIM(fexpname(control_exp_nr))
    WRITE(scoreunit,'(A,A)') '#reference_display_name=',TRIM(expname(control_exp_nr))
    WRITE(scoreunit,'(A,I0)') '#comparison_index=',comparison
    WRITE(scoreunit,'(A,A)') '#comparison_id=',TRIM(fexpname(comparison))
    WRITE(scoreunit,'(A,A)') '#comparison_display_name=',TRIM(expname(comparison))
    WRITE(scoreunit,'(A,I0)') '#period=',period
    WRITE(scoreunit,'(A,I0)') '#station_scope=',stnr
    IF (TRIM(tag) == '#') THEN
       WRITE(scoreunit,'(A)') '#selection=ALL'
    ELSE
       WRITE(scoreunit,'(A,A)') '#selection=',TRIM(tag)
    ENDIF
    WRITE(scoreunit,'(A,A)') '#initial_time_group=',TRIM(safe_ini)
    WRITE(scoreunit,'(A,F6.2)') '#confidence_percent=',confint
    WRITE(scoreunit,'(A)') '#difference=reference RMSE - comparison RMSE after Monitor normalization'
    WRITE(scoreunit,'(A)') '#columns=variable lead_index lead_hour rmse_difference ci_half_width paired_cases'
 ENDIF

 raw_label=TRIM(varprop(jpar)%text)
 label=raw_label
 IF (domain == 'TEMP') THEN
    lev=varprop(jpar)%lev
    WRITE(level_suffix,'(I0,A)') lev,'hPa'
    label_length=LEN_TRIM(raw_label)
    suffix_length=LEN_TRIM(level_suffix)
    IF (label_length < suffix_length) THEN
       WRITE(label,'(A,1X,A)') TRIM(raw_label),TRIM(level_suffix)
    ELSEIF (raw_label(label_length-suffix_length+1:label_length) /= &
            TRIM(level_suffix)) THEN
       WRITE(label,'(A,1X,A)') TRIM(raw_label),TRIM(level_suffix)
    ENDIF
 ENDIF
 DO kk=1,nuse_fclen
    IF (ncases(kk) < 2) CYCLE
    IF (.NOT. ieee_is_finite(sdiff(kk,1)) .OR. &
        .NOT. ieee_is_finite(sdiff(kk,2))) CYCLE
    lead_hour=use_fclen(kk)
    WRITE(scoreunit,'(A,1X,I0,1X,I0,1X,ES24.16E3,1X,ES24.16E3,1X,I0)') &
       TRIM(label),kk,lead_hour,sdiff(kk,1),sdiff(kk,2),ncases(kk)
 ENDDO
 CLOSE(scoreunit,IOSTAT=scoreios)
 IF (scoreios /= 0) THEN
    WRITE(6,*)'Could not close scorecard export: ',TRIM(scorefile)
    STOP 1
 ENDIF

END SUBROUTINE write_scorecard_rows

FUNCTION safe_component(value) RESULT(safe)
 CHARACTER(LEN=*), INTENT(IN) :: value
 CHARACTER(LEN=100) :: safe
 INTEGER :: ii,code
 safe=''
 DO ii=1,MIN(LEN_TRIM(value),LEN(safe))
    code=IACHAR(value(ii:ii))
    IF ((code >= IACHAR('a') .AND. code <= IACHAR('z')) .OR. &
        (code >= IACHAR('A') .AND. code <= IACHAR('Z')) .OR. &
        (code >= IACHAR('0') .AND. code <= IACHAR('9')) .OR. &
         value(ii:ii) == '-') THEN
       safe(ii:ii)=value(ii:ii)
    ELSE
       safe(ii:ii)='_'
    ENDIF
 ENDDO
END FUNCTION safe_component

END SUBROUTINE print_joint_sign_test
