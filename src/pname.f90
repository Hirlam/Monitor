 SUBROUTINE pname(p,u)

 USE DATA, ONLY : z_is_pressure

 IMPLICIT NONE

 ! Input
 CHARACTER(LEN=6) :: p ! Parameter

 ! Output
 CHARACTER(LEN=*) :: u ! Parameter

 ! Local
 CHARACTER(LEN=2) :: pp ! Parameter
 CHARACTER(LEN=4) :: ll ! Parameter

 pp = p(1:2)

 SELECT CASE(pp)

 CASE('HG')
   u='Height'
 CASE('LA')
   u='Latitude'
 CASE('FI')
   u='Height'
 CASE('RH')
   u='Relative Humidity'
 CASE('PS')
   u='Surface pressure'
 CASE('NN')
   u='Cloud cover'
 CASE('TD')
   u='Dew point temperature'
 CASE('TT')
   u='Temperature'
 CASE('VI')
   u='Visibility'
 CASE('FF')
   u='Wind speed'
 CASE('DD')
   u='Wind direction'
 CASE('WT')
   u='Sensible heat flux'
 CASE('WQ')
   u='Latent heat flux'
 CASE('QQ')
   u='Specific humidity'
 CASE('SW')
   u='Short wave radiation'
 CASE('UW')
   u='Momentum flux'
 CASE('NR')
   u='Net radiation'
 CASE('GR')
   u='Global radiation'
 CASE('SU')
   u='Shortwave radiation up'
 CASE('SD')
   u='Shortwave radiation down'
 CASE('LU')
   u='Longwave radiation up'
 CASE('LD')
   u='Longwave radiation down'
 CASE('LW')
   u='Longwave radiation'
 CASE('GS')
   u='Ground heat flux'
 CASE('HB')
   u='Surface heat budget residual'
 CASE('GC')
   u='Residual ground heat flux'
 CASE('PE','PD')
   u='Precipitation'
 CASE('RF')
   u='Runoff'
 CASE('TU')
   u='dT/dz/dU/dz'
 CASE('UZ')
   u='dU/dz'
 CASE('TZ')
   u='dT/dz'
 CASE('WP')
   u='Wind power'
 CASE('WH')
   u='Energy'
 CASE DEFAULT

 u = p

 END SELECT

 ll = p(3:6)
 IF (len_trim(ll) > 0 ) THEN
    IF ( z_is_pressure ) THEN
       u = TRIM(u) //' '//ll//' hPa'
    ELSE
       u = TRIM(u) //' '//ll//' m'
    ENDIF
 ENDIF

 RETURN

END SUBROUTINE pname

