SUBROUTINE make_fname(prefix,time,id,tag,        &
                      param,level,               &
                      output_mode,output_type,   &
                      fname)

 !
 ! Create filenames for output files
 ! 
 ! Prefix _ Period _ Station_id + Domain . Ext
 !
 ! Ulf Andrae, SMHI, 2007
 !

 IMPLICIT NONE

 ! Input
 CHARACTER(LEN=*), INTENT(IN ) :: prefix,tag,param
 INTEGER,          INTENT(IN ) :: time,id,output_type, &
                                  output_mode,level
 CHARACTER(LEN=*), INTENT(OUT) :: fname

 ! Local

 INTEGER :: magn

 CHARACTER(LEN=8) :: ctime,clevel,cid
 CHARACTER(LEN=4) :: ext
 CHARACTER(LEN=4) :: cform = '(IX)'

 
 !----------------------------------------------------

 WRITE(ctime ,'(I8.8)')time
 WRITE(cid   ,'(I8.8)')id

 IF ( level == 0 ) THEN
    clevel = '0'
 ELSE
    magn=FLOOR(LOG10(FLOAT(level)))+1
    WRITE(cform(3:3),'(I1)')magn
    WRITE(clevel,cform)level
 ENDIF

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

 SELECT CASE (output_mode)
 CASE(1)
 fname = TRIM(prefix)//'_'//         &
                ctime//'_'//         &
                  cid//'_'//         &
            TRIM(tag)//TRIM(ext)
 CASE(2)
 fname = TRIM(prefix)//'_'//         &
                ctime//'_'//         &
                  cid//'_'//         &
            TRIM(tag)//'_'//         &
          TRIM(param)//'_'//         &
         TRIM(clevel)//TRIM(ext)
 CASE DEFAULT
    WRITE(6,*)'No option coded for this output_mode',output_mode
    CALL abort
 END SELECT

 WRITE(6,*)'DOING:',TRIM(fname)

 RETURN

END SUBROUTINE make_fname
