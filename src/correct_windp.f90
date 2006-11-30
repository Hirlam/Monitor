SUBROUTINE correct_windp(stnr,nexp,obs,diff)

 USE windp

 IMPLICIT NONE

 INTEGER, INTENT(IN)    :: stnr,nexp
 REAL,    INTENT(IN)    :: obs
 REAL,    INTENT(INOUT) :: diff(nexp)

 INTEGER :: j,k
 REAL    :: maxwp
 ! ------------------------------------------------------------------

  windp_index = 0
  DO j=1,maxwindp
     IF (stnr == windp_stations(j)%nr ) THEN
        windp_index = j
     ENDIF
  ENDDO


  maxwp = MAXVAL(wind_map(windp_stations(windp_index)%typ_ind,:,1)) 
  DO k=1,nexp
     IF ( ( diff(k) + obs ) > SQRT(maxwp) )  diff(k) = SQRT(maxwp) - obs
     IF ( ( diff(k) + obs ) < 0.          )  diff(k) =             - obs
  ENDDO

  RETURN

END SUBROUTINE correct_windp
