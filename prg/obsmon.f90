PROGRAM obsmon
USE module_obsmon, ONLY : init_obsmon,process_odb,read_obsmon_namelist,finalize_obsmon,dtgstart,dtgend,fcint
IMPLICIT NONE
INTEGER           :: count,len,stat,ios
CHARACTER(LEN=10) :: val

INTERFACE
  SUBROUTINE usage(err)
    INTEGER,INTENT(IN),OPTIONAL :: err
  END SUBROUTINE usage
END INTERFACE

count=command_argument_count()
IF ( count /= 3 ) THEN
  CALL usage()
ELSE
  CALL get_command_argument(1,val,len,stat)
  IF ( stat /= 0 ) CALL USAGE(stat)
  READ(val,'(I10.10)',IOSTAT=ios) dtgstart
  IF ( ios /= 0 ) CALL USAGE(ios)
  CALL get_command_argument(2,val,len,stat)
  IF ( stat /= 0 ) CALL USAGE(stat)
  READ(val,'(I10.10)') dtgend 
  IF ( ios /= 0 ) CALL USAGE(ios)
  CALL get_command_argument(3,val,len,stat)
  IF ( stat /= 0 ) CALL USAGE(stat)
  READ(val,'(I10)') fcint
  IF ( ios /= 0 ) CALL USAGE(ios)


  ! Sanity
  IF (( dtgstart < 1900010100 ) .OR. ( dtgend < 1900010100 ) ) THEN
    WRITE(*,*) 'DTG-START and/or DTG-END is too low ',dtgstart,dtgend 
    CALL ABORT
  ENDIF
  IF ( dtgstart > dtgend ) THEN
    WRITE(*,*) 'DTG-END must be larger than DTG-START ',dtgstart,dtgend
    CALL ABORT
  ENDIF
  IF ( fcint < 1 ) THEN
    WRITE(*,*) 'FCINT can not be zero or negative ',fcint
    CALL ABORT
  ENDIF

  CALL init_obsmon()
  CALL process_odb()
  CALL finalize_obsmon()
ENDIF
END PROGRAM

SUBROUTINE usage(err)
  IMPLICIT NONE
  INTEGER,INTENT(IN),OPTIONAL :: err

  IF ( PRESENT(err)) THEN
    WRITE(*,*) 'ERROR CODE: ',err
    WRITE(*,*)
  ENDIF
  WRITE(*,*) 'Usage: obsmon DTG-START DTG-END FCINT'
  CALL abort()

END SUBROUTINE usage
