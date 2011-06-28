 SUBROUTINE yunit(p,u) 

 USE DATA, ONLY : pe_interval

 IMPLICIT NONE

 ! Input
 CHARACTER(LEN=*) :: p ! Parameter

 ! Output
 CHARACTER(LEN=*) :: u ! Parameter

 CHARACTER(LEN=3) :: ctmp =''

 SELECT CASE(TRIM(p))

 CASE('HG','VI')
   u='m'
 CASE('LA')
   u='deg'
 CASE('FI')
   u='m'
 CASE('NN')
   u='octas'
 CASE('RH')
   u='%'
 CASE('PS')
   u='hPa'
 CASE('TD','TT','TN','TX')
   u='deg C'
 CASE('FF','FX','GG','GX')
   u='m/s'
 CASE('DD')
   u='deg'
 CASE('WT','WQ','SW','GS','GR','LW','LU','LD','NR','GC','HB','SU','SD')
   u='W/m^2'
 CASE('WP')
   u='kW'
 CASE('WH')
   u='kWh'
 CASE('QQ')
   u='g/kg'
 CASE('UW')
   u='m^2/s^2'
 CASE('RF','PD')
   u='mm/day'
 CASE('TU')
   u='Ks/m'
 CASE('TZ')
   u='deg C/m'
 CASE('UZ')
   u='s^-1'
 CASE('PE')
   IF (     pe_interval < 10 ) THEN
      WRITE(ctmp,'(I1)')pe_interval
   ELSEIF ( pe_interval < 100) THEN
      WRITE(ctmp,'(I2)')pe_interval
   ELSE
      WRITE(ctmp,'(I3)')pe_interval
   ENDIF
   u='mm'//'/'//TRIM(ctmp)//'h'
 CASE DEFAULT
 
 u = p
 
 END SELECT
 RETURN

END SUBROUTINE yunit
