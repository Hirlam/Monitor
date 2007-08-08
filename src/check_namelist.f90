SUBROUTINE check_namelist

 !
 ! Cross check some options in the namelist
 !

 USE data
 
 IMPLICIT NONE

 !---------------------------------------------

 ! Check date 

 IF ( SDATE > EDATE ) THEN
    WRITE(6,*)' EDATE should be greater than SDATE '
    CALL abort
 ENDIF

 IF( lfcver ) THEN

    !
    ! Number of times to verify has to be
    ! equal number of fclengths
    !
    ntimver = nfclengths

 ENDIF

 ! Adjust last obs date and time according to forecast length
 CALL adddtg(edate,etime,3600*MAXVAL(fclen),edate_obs,etime_obs)

 ! X plots
 all_var_present = ( ANY(corr_pairs /= 0) .OR. all_var_present )
 lplot_comp      = ( ANY(corr_pairs /= 0) )

 ! Contingency settings
 lcontingency = ANY( cont_ind /= 0 ) 

 ! More to come ....

 RETURN
END SUBROUTINE check_namelist
