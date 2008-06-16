SUBROUTINE open_output(fname)

 USE data, ONLY : output_type,lunout

 IMPLICIT NONE

 ! Input
 CHARACTER(LEN=*), INTENT(IN) :: fname

 SELECT CASE(output_type) 
 CASE(0)
    ! Open an ASCII file
    OPEN(UNIT=lunout,FILE=fname)
#ifdef MAGICS
 CASE(1)
    ! Open a postscript file
    CALL popen
    CALL psetc ('PS_DEVICE','ps_a4')
    CALL psetc ('PS_FILE_NAME',fname)
 CASE(2)
    ! Open a png file
    CALL popen
    CALL psetc('DEVICE','PNG')
    CALL psetc('DEVICE_FILE_NAME',fname)
    CALL pseti('DEVICE_WIDTH',640)  
 CASE(3)
    ! Open a jpeg file
    CALL popen
    CALL psetc('DEVICE','JPEG')
    CALL psetc('DEVICE_FILE_NAME',fname)
    CALL pseti('DEVICE_WIDTH',640)  
#endif
 CASE DEFAULT
    WRITE(6,*)'Can not handle output_type ',output_type
    CALL abort
 END SELECT 

 RETURN

END SUBROUTINE open_output
