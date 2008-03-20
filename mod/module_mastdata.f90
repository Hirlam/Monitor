MODULE mast_data

 IMPLICIT NONE

 SAVE

 INTEGER,          PARAMETER :: &
 max_flux_station = 6

 REAL,             PARAMETER :: &
 dz(max_flux_station) = (/200.,200.,200.,200.,200.,200./)

 CHARACTER(LEN=4), PARAMETER :: &
 stname(max_flux_station)=(/'SODA','CABA','KIVI','KUOP','ROVA','VALL'/)

 CONTAINS

 SUBROUTINE find_dz_model(exp,station,dz_model)

  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT( IN) :: exp,station
  REAL,             INTENT(OUT) :: dz_model

  SELECT CASE(TRIM(exp))

  CASE DEFAULT

     SELECT CASE(TRIM(station))

     CASE DEFAULT

        dz_model = 100.

     END SELECT

  END SELECT

 END SUBROUTINE find_dz_model

END MODULE mast_data
