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

 ! More to come ....

 RETURN
END SUBROUTINE check_namelist
