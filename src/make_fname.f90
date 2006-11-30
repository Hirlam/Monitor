SUBROUTINE make_fname(prefix,time,id,nrun,fname)

 IMPLICIT NONE

 ! Input
 CHARACTER(LEN=*), INTENT(IN ) :: prefix
 INTEGER,          INTENT(IN ) :: time,id,nrun
 CHARACTER(LEN=*), INTENT(OUT) :: fname

 CHARACTER(LEN=8) :: ctime
 CHARACTER(LEN=8) :: cid
 CHARACTER(LEN=1) :: cnrun

 WRITE(ctime,'(I8.8)')time
 WRITE(cid  ,'(I8.8)')id
 WRITE(cnrun,'(I1.1)')nrun

 fname = ''
 fname = TRIM(prefix)//'_'//ctime//'_'//cid//'_'//cnrun//'.ps'

 WRITE(6,*)'DOING:',TRIM(fname)

 RETURN

END SUBROUTINE make_fname
