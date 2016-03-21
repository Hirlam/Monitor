SUBROUTINE check_path(date,path)

 ! DATE = YYYYMMDD

 IMPLICIT NONE

 INTEGER,          INTENT(IN   ) :: date

 CHARACTER(LEN=*), INTENT(INOUT) :: path

 INTEGER :: yy,mm,dd,ind
 CHARACTER(LEN=4) :: c4
 CHARACTER(LEN=2) :: c2

 !------------------------------------------------

 yy = date /10000
 mm = MOD (date/100,100)
 dd = MOD (date    ,100)

 DO
   ind = INDEX(path,'@YYYY@')
   IF ( ind == 0 ) EXIT
   WRITE(c4,'(I4.4)') yy
   path = TRIM(path(1:ind-1))//c4//TRIM(path(ind+6:))
 ENDDO

 DO
   ind = INDEX(path,'@MM@')
   IF ( ind == 0 ) EXIT
   WRITE(path(ind:ind+1),'(I2.2)') mm
   WRITE(c2,'(I2.2)') mm
   path = TRIM(path(1:ind-1))//c2//TRIM(path(ind+4:))
 ENDDO

 DO
   ind = INDEX(path,'@DD@')
   IF ( ind == 0 ) EXIT
   WRITE(c2,'(I2.2)') dd
   path = TRIM(path(1:ind-1))//c2//TRIM(path(ind+4:))
 ENDDO

 RETURN

END SUBROUTINE check_path

