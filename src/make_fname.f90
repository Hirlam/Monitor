SUBROUTINE make_fname(prefix,time,id,tag,fname,output_type)

 !
 ! Create filenames for output files
 ! 
 ! Prefix _ Period _ Station_id + Domain . Ext
 !
 ! Ulf Andrae, SMHI, 2007
 !

 IMPLICIT NONE

 ! Input
 CHARACTER(LEN=*), INTENT(IN ) :: prefix,tag
 INTEGER,          INTENT(IN ) :: time,id,output_type
 CHARACTER(LEN=*), INTENT(OUT) :: fname

 !Local
 CHARACTER(LEN=8) :: ctime
 CHARACTER(LEN=8) :: cid
 CHARACTER(LEN=4) :: ext
 
 !----------------------------------------------------

 WRITE(ctime,'(I8.8)')time
 WRITE(cid  ,'(I8.8)')id

 SELECT CASE(output_type) 
 CASE(0)
    ext = '.txt'
 CASE(1)
    ext = '.ps'
 CASE(2,3)
    ext = ''
 CASE DEFAULT
    WRITE(6,*)'Can not handle output_type ',output_type
    CALL abort
 END SELECT 

 fname = ''
 fname = TRIM(prefix)//'_'//ctime//'_'//cid//'_'//TRIM(tag)//TRIM(ext)

 WRITE(6,*)'DOING:',TRIM(fname)

 RETURN

END SUBROUTINE make_fname
