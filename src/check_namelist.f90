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

 IF( LTIMESERIE_STAT_MONTH .OR. LPLOT_STAT_MONTH ) THEN
    MONTHVISE = .TRUE.
 ENDIF

 ! More to come ....

 RETURN
END SUBROUTINE check_namelist
