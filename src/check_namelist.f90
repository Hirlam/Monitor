SUBROUTINE check_namelist

 !
 ! Cross check some options in the namelist
 !
 ! Ulf Andrae, SMHI, 2007
 !

 USE data
 
 IMPLICIT NONE

 INTEGER :: i,j

 !---------------------------------------------

 WRITE(6,*)
 WRITE(6,*)'-Check namelist options-'
 WRITE(6,*)

 !
 ! Check precipitation interval
 !

 IF ( pe_ind /= 0 ) THEN
    IF ( accu_int(pe_ind) == 0 ) accu_int(pe_ind) = pe_interval
    WRITE(6,*)'  Changed precipitation accumulation period to ',pe_interval
 ENDIF 
 pe_interval = accu_int(pe_ind)

 !
 ! Check scat_nlev
 !

 IF ( lplot_scat ) THEN
    DO i=1,nparver
       scat_magn(i) = scat_magn(i) * 4 
    ENDDO
 ENDIF

 ! Check ncla
    DO i=1,nparver
       IF ( ANY(ABS(pre_fcla(i,:)) > 1.e-6 ) ) THEN
         DO j=2,mpre_cla
           IF ( ABS( pre_fcla(i,j-1)-pre_fcla(i,j)) < 1.e-9 ) EXIT
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
 ENDIF


 ! Check ntimver

 IF( lfcver ) THEN

    !
    ! Number of times to verify has to be
    ! equal number of nuse_fclen
    !

    ntimver = nuse_fclen
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
    WRITE(6,*)'  Changed NINI_HOURS to',nini_hours
    WRITE(6,*)'  Changed INI_HOURS to' , ini_hours(1:nini_hours)
 ELSE
    nini_hours = 0
    DO i=1,24
       IF( ini_hours(i) /= -1 ) nini_hours = nini_hours + 1
    ENDDO
    WRITE(6,*)'  Changed NINI_HOURS to',nini_hours
 ENDIF

 
 ! Check date 

 IF ( SDATE > EDATE ) THEN
    WRITE(6,*)' EDATE should be greater than SDATE '
    CALL abort
 ENDIF


 ! Adjust last obs date and time according to forecast length
 CALL adddtg(edate,etime,3600*MAXVAL(fclen),edate_obs,etime_obs)


 ! X plots
 all_var_present = ( ANY(corr_pairs /= 0) .OR. all_var_present )
 lplot_comp      = ( ANY(corr_pairs /= 0) )


 ! Contingency settings
 lcontingency = ( cont_param /= 0 )


 ! More to come ....

 WRITE(6,*)
 WRITE(6,*)'-Done namelist checking-'
 WRITE(6,*)

 RETURN
END SUBROUTINE check_namelist
