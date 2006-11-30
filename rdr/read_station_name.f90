 SUBROUTINE read_station_name

 USE data

 IMPLICIT NONE

 INTEGER :: ierr,n,i
 
 OPEN(lunin,file='station_name.dat',STATUS='OLD',IOSTAT=ierr)

 IF(ierr.NE.0) RETURN

 READ(lunin,*) n

 ALLOCATE(station_name(n))

 DO i=1,n
    READ(lunin,'(A50)')station_name(i)
 ENDDO

 CLOSE(lunin)

 RETURN
 
 END SUBROUTINE read_station_name
