SUBROUTINE check_namelist

 !
 ! Cross check some options in the namelist
 !
 ! Ulf Andrae, SMHI, 2007
 !

 USE data
 
 IMPLICIT NONE

 INTEGER :: i,j,ctime
 CHARACTER(LEN=2) :: ci = ''

 !---------------------------------------------

 WRITE(6,*)
 WRITE(6,*)'-Check namelist options-'
 WRITE(6,*)

 !
 ! Conditional settings
 !

 lconditional = ( cond_param /= 0 )

 !
 ! Contingency settings
 !

 lcontingency = ( cont_param /= 0 )

 !
 ! Significance test
 !
 IF( .NOT. lfcver .OR. nexp == 1) THEN
    ! Significance test only if fcver
    lsign_test = .FALSE.
 ENDIF


 IF ( lplot_seasonal ) lplot_stat = .TRUE.

 !
 ! Check what kind of statistics we are producing
 !

 IF ( .NOT. lverify                   ) WRITE(6,*)' - No verification is done, only reading and/or QC'
 IF ( ltimeserie_stat                 ) WRITE(6,*)' - Time serie verification is done '
 IF ( plot_bias_map .OR. plot_obs_map ) WRITE(6,*)' - Maps will be created '
 IF ( lplot_vert                      ) WRITE(6,*)' - Vertical profiles will be verified'
 IF ( lplot_freq                      ) WRITE(6,*)' - Frequency distribution will be calculated'
 IF ( lplot_scat                      ) WRITE(6,*)' - Scatter plots will be produced '
 IF ( lplot_stat .AND. lfcver .AND. &
      .NOT. lplot_seasonal            ) WRITE(6,*)' - Verification against forecast time will be done'
 IF ( lsign_test .AND. lfcver .AND. &
      nexp > 1                        ) WRITE(6,*)' - Significance test will be done'
 IF ( lplot_stat .AND. lfcver .AND. &
      lplot_seasonal                  ) WRITE(6,*)' - Seasonal verification will be done'
 IF ( lplot_stat .AND. .NOT. lfcver   ) WRITE(6,*)' - Verification against time of day will be done'
 IF ( lcontingency                    ) WRITE(6,*)' - Contingency tables will be created'
 IF ( lconditional                    ) WRITE(6,*)' - Data selection is conditional'
 IF ( lprep_xml                       ) WRITE(6,*)' - XML files for stations statistics will be produced'

 WRITE(6,*)


 !
 ! Check maxstn
 !

 IF ( maxstn > max_maxstn ) THEN
    WRITE(6,*)'Your requested maxstn is',maxstn
    WRITE(6,*)'Maximum alloved is ',max_maxstn
    WRITE(6,*)'Decrease or recompile'
    CALL abort
 ENDIF

 ! Check ncla
    DO i=1,nparver
       IF ( ANY(ABS(pre_fcla(:,i)) > 1.e-6 ) ) THEN
         DO j=2,mpre_cla
           IF ( ABS( pre_fcla(j-1,i)-pre_fcla(j,i)) < 1.e-9 ) EXIT
         ENDDO
         ncla(i) = j - 2
         WRITE(6,*)'  Changed ncla to (par_ind/value)', i,ncla(i)
       ENDIF
    ENDDO
 ! Check tag

 IF ( tag == '#' ) THEN
    tag ='1'
    WRITE(6,*)'  Changed TAG to ', TRIM(tag)
 ENDIF

 WRITE(6,*)'  Current tag is ', TRIM(tag)


 ! Check nfclengths

 IF ( nfclengths == 0 ) THEN
    DO i=1,maxfclen
      IF ( fclen(i) /=-1 ) nfclengths = nfclengths + 1
    ENDDO
    WRITE(6,*)'  Changed NFCLENGTHS to', nfclengths
 ENDIF


 ! Check use_fclen

 IF ( use_fclen(1) == -1 ) THEN
    use_fclen = -1
    use_fclen(1:nfclengths)=fclen(1:nfclengths)
    nuse_fclen = nfclengths
    WRITE(6,*)'  Changed USE_FCLEN to', use_fclen(1:nuse_fclen)
 ELSE
    nuse_fclen = 0
    DO i=1,maxfclen
       IF( use_fclen(i) /= -1 ) nuse_fclen = nuse_fclen + 1
    ENDDO
    WRITE(6,*)'  Changed NUSE_FCLEN to', nuse_fclen
    WRITE(6,*)'           USE_FCLEN is', use_fclen(1:nuse_fclen)
 ENDIF

 ! Check that use_fclen is increasing

 DO i=2,nuse_fclen
    IF ( (use_fclen(i)-use_fclen(i-1)) <= 0)THEN
       WRITE(6,*)'use_fclen should increase ',use_fclen(1:nuse_fclen) 
       CALL abort
    ENDIF
 ENDDO


 ! Check ntimver

 IF( lfcver ) THEN

    !
    ! Number of times to verify has to be
    ! equal number of nuse_fclen
    !

    ntimver = nuse_fclen

    ! 
    ! Seasonal settings
    ! 

    IF ( lplot_seasonal ) ntimver = 366

    WRITE(6,*)'  Changed NTIMVER to', ntimver

 ELSE
    IF ( ntimver == 0 ) THEN
       WRITE(6,*)'Please set NTIMVER'
       CALL abort
    ENDIF
 ENDIF


 ! Check use_hours

 IF ( ini_hours(1) == -1 ) THEN
             i = 1
    nini_hours = 0
    DO 
       ini_hours(i)=(i-1) * fcint
       nini_hours = nini_hours + 1
       i=i+1
       IF ( ((i-1) * fcint) == 24 ) EXIT
    ENDDO
    cini_hours = '_ALL'
 ELSE
    nini_hours = 0
    cini_hours = ''
    DO i=1,24
       IF( ini_hours(i) /= -1 ) THEN
          nini_hours = nini_hours + 1
          WRITE(ci,'(I2.2)')ini_hours(i)
          cini_hours = TRIM(cini_hours)//'_'//ci
       ENDIF 
    ENDDO
 ENDIF
 WRITE(6,*)'  Changed NINI_HOURS to',nini_hours
 WRITE(6,*)'  Changed INI_HOURS to' , ini_hours(1:nini_hours)
 WRITE(6,*)'  Changed CINI_HOURS to' , TRIM(cini_hours)

 
 ! Check date 

 IF ( SDATE > EDATE ) THEN
    WRITE(6,*)' EDATE should be greater than SDATE '
    CALL abort
 ENDIF


 ! Adjust last obs date and time according to forecast length
 ctime = etime * 10000
 CALL adddtg(edate,ctime,3600*MAXVAL(fclen),edate_obs,etime_obs)
 etime_obs = etime_obs / 10000


 ! X plots
 all_var_present = ( ANY(corr_pairs /= 0) .OR. all_var_present )
 lplot_comp      = ( ANY(corr_pairs /= 0) )


 ! Contingency settings

 lcontingency = ( cont_param /= 0 )


 ! Do not allow timserie_wind < obint because it is not meaningful

 IF ( ltimeserie_stat ) THEN
    DO i=1,nparver
       IF ( timeserie_wind(i) < obint .AND. &
            timeserie_wind(i) > 0           ) THEN

          WRITE(6,*)'timserie_wind < obint not allowed'
          WRITE(6,*)'Parameter,obint,timeserie_wind:', &
          varprop(i)%id,obint,timeserie_wind(i)
    
          CALL abort

       ENDIF
    ENDDO
 ENDIF
 
 SELECT CASE(TRIM(graphics))
  CASE('gnuplot','GNUPLOT')
 
    output_type = 0

    lprint_timeserie_stat = ltimeserie_stat 
    print_bias_map        = plot_bias_map
    print_rmse_map        = plot_rmse_map
    print_stdv_map        = plot_stdv_map
    print_obs_map         = plot_obs_map
    lprint_vert           = lplot_vert
    lprint_freq           = lplot_freq
    lprint_scat           = lplot_scat
    lprint_stat           = lplot_stat
    lprint_comp           = lplot_comp
    lprint_seasonal       = lplot_seasonal

  CASE DEFAULT

     WRITE(6,*)'  Unknown graphics'
     CALL abort

 END SELECT

 ! More to come ....

 WRITE(6,*)
 WRITE(6,*)'-Done namelist checking-'
 WRITE(6,*)

 RETURN

END SUBROUTINE check_namelist
