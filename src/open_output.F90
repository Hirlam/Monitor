SUBROUTINE open_output(fname)

 USE data, ONLY : output_type,lunout

 IMPLICIT NONE

 ! Input
 CHARACTER(LEN=*), INTENT(IN) :: fname

 SELECT CASE(output_type) 
 CASE(0)
    ! Open an ASCII file
    OPEN(UNIT=lunout,FILE=fname)
 CASE DEFAULT
    WRITE(6,*)'Can not handle output_type ',output_type
    CALL abort
 END SELECT 

 RETURN

END SUBROUTINE open_output
