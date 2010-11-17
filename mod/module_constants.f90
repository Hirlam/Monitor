 MODULE constants

  IMPLICIT NONE

  REAL, PARAMETER :: gravit    = 9.80665                ! Constant of gravity
  REAL, PARAMETER :: pi        = 3.141593
  REAL, PARAMETER :: rearth    = 6.37e6
  REAL, PARAMETER :: degm      = rearth*2.*pi/360.
  REAL, PARAMETER :: cp        = 1004.67
  REAL, PARAMETER :: levap     = 2.5e6
  REAL, PARAMETER :: rho       = 1.292
  REAL, PARAMETER :: tzero     = 273.15
  REAL, PARAMETER :: melt_heat = 3.34e5 ! J/kg
  REAL, PARAMETER :: rair      = 2.8704E2

  CHARACTER(LEN=3), PARAMETER  ::                      &
  seasonal_name1(4)=(/'DJF','MAM','JJA','SON'/),       &
  seasonal_name2(12)=(/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/)


 END MODULE constants
