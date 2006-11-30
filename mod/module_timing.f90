MODULE timing

IMPLICIT NONE

INTEGER, PARAMETER :: maxtiming                 = 25
INTEGER            :: timing_times(maxtiming,2) = 0
INTEGER            :: numtiming                 = 0
CHARACTER(LEN=25)  :: timing_names(maxtiming)   =''

!----------------------------------
!----------------------------------
!----------------------------------
CONTAINS
!----------------------------------
!----------------------------------
!----------------------------------
SUBROUTINE acc_timing(id,name)

IMPLICIT NONE

INTEGER :: i,id,id_local,count
CHARACTER(LEN=*) :: name

!----------------------------------

IF(id == 0) THEN

  DO i=1,numtiming
     IF(TRIM(timing_names(i)) == TRIM(name)) THEN
        id = i
        EXIT
     ENDIF
  ENDDO

   IF(id == 0) THEN
      CALL add_timing(id,name)
   ELSE
      CALL system_clock(count)
      timing_times(id,1)=count 
   ENDIF
ELSE

  CALL system_clock(count)
  timing_times(id,2) = timing_times(id,2) - 		&
                       timing_times(id,1) + count
ENDIF

RETURN

END SUBROUTINE acc_timing
!----------------------------------
!----------------------------------
!----------------------------------
SUBROUTINE add_timing(id,name)

IMPLICIT NONE

INTEGER :: id,count
CHARACTER(LEN=*) :: name

!----------------------------------

IF(id == 0) THEN

  IF (numtiming == maxtiming) THEN
     WRITE(6,*)'Increase maxtiming in module timing'
     WRITE(6,*)'This timing rejected:', TRIM(name)
     id = 0
     RETURN
  ENDIF

  numtiming = numtiming + 1
  id = numtiming
  timing_names(id) = TRIM(name)
  CALL system_clock(count)
  timing_times(id,1)=count 

ELSE
  CALL system_clock(count)
  timing_times(id,2) = - timing_times(id,1) + count
ENDIF

RETURN

END SUBROUTINE add_timing
!----------------------------------
!----------------------------------
!----------------------------------
SUBROUTINE view_timing

IMPLICIT NONE

INTEGER :: count,count_rate,i
REAL    :: timing_time

!----------------------------------

  CALL system_clock(count,count_rate)

  WRITE(6,*)
  WRITE(6,*)'------ TIMING --------'
  WRITE(6,*)

  DO i=1,numtiming

      timing_time =						&
      FLOAT(timing_times(i,2))/       	&
      FLOAT(count_rate)

      WRITE(6,'(A20,A10,f10.5,A8)')		&
      TRIM(timing_names(i)),' took:',	&
      timing_time,' seconds'

  ENDDO

  WRITE(6,*)
  WRITE(6,*)'------ TIMING --------'
  WRITE(6,*)
 
RETURN
END SUBROUTINE view_timing

END MODULE timing
