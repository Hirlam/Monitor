SUBROUTINE check_namelist

 !
 ! Cross check some options in the namelist
 !

 USE data
 
 IMPLICIT NONE

 !---------------------------------------------

 IF( lfcver ) THEN

    !
    ! Number of times to verify has to be
    ! equal number of fclengths
    !
    ntimver = nfclengths

 ENDIF

 ! Adjust last obs date and time according to forecast length
 CALL adddtg(edate,etime,3600*MAXVAL(fclen),edate_obs,etime_obs)

 ! More to come ....

 RETURN
END SUBROUTINE check_namelist
