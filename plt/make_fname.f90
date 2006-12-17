SUBROUTINE make_fname(prefix,time,id,nrun,fname)

 USE data, ONLY : output_type

 IMPLICIT NONE

 ! Input
 CHARACTER(LEN=*), INTENT(IN ) :: prefix
 INTEGER,          INTENT(IN ) :: time,id,nrun
 CHARACTER(LEN=*), INTENT(OUT) :: fname

 CHARACTER(LEN=8) :: ctime
 CHARACTER(LEN=8) :: cid
 CHARACTER(LEN=1) :: cnrun
 CHARACTER(LEN=3) :: ext

 WRITE(ctime,'(I8.8)')time
 WRITE(cid  ,'(I8.8)')id
 WRITE(cnrun,'(I1.1)')nrun

 SELECT CASE(output_type) 
 CASE(1)
    ext = '.ps'
 CASE(2,3)
    ext = ''
 CASE DEFAULT
    WRITE(6,*)'Can not handle output_type ',output_type
    CALL abort
 END SELECT 

 fname = ''
 fname = TRIM(prefix)//'_'//ctime//'_'//cid//'_'//cnrun//TRIM(ext)

 WRITE(6,*)'DOING:',TRIM(fname)

 RETURN

END SUBROUTINE make_fname
