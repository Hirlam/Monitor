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

 ! Clear use_fclen
 use_fclen = -1
 
 ! Conditional
 lconditional = .FALSE.
 cond_param   = 0
 cond%ind     = -1
 cond%llim    = -err_ind
 cond%ulim    =  err_ind
 cond%lobs    = .TRUE.
 cond%all_mod = .FALSE.

 ! Contingency 
 lcontingency = .FALSE.
 cont_ind     = 0
 cont_param   = 0

 ! Selection
 reverse_selection = .FALSE.

 ! Station height selection
 lstn_hgt_check = .FALSE.

 ! Timserie window
 timeserie_wind = 0

 ! Show settings
 show_var = .FALSE.
 show_skw = .FALSE.

END SUBROUTINE ini_namelist
