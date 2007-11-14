SUBROUTINE ini_namelist

 !
 ! Init _some_ namelist values 
 !
 ! Ulf Andrae, SMHI, 2007
 !

 USE data

 IMPLICIT NONE

 !------------------------------------

 ! Clear tag
 tag = '#'

 ! Clear stnlist
 stnlist = 0

 ! Quality control
 lquality_control   = .FALSE.
 estimate_qc_limit  = .FALSE.

END SUBROUTINE ini_namelist
