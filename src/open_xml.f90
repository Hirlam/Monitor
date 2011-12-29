SUBROUTINE open_xml(lunxml,maxper,periods)

 USE data,     ONLY : period_type,nparver,varprop,sdate,edate,tag

 IMPLICIT NONE

 ! Input

 INTEGER, INTENT(   IN) :: lunxml,maxper
 INTEGER, INTENT(INOUT) :: periods(maxper+1)

 ! Local

 INTEGER :: i,j 

 CHARACTER(LEN= 8) :: cmon
 CHARACTER(LEN=50) :: fname

 !---------------------------------------------------------

 !
 ! Prepare xml file for station statistics
 !

 IF ( period_type == 1 ) periods = 0

 DO j=1,nparver
    DO i=1,maxper

        WRITE(cmon(1:8),'(I8.8)')periods(i)
        fname = TRIM(varprop(j)%id)//'_'//TRIM(cmon)//'.xml'
        OPEN(lunxml,file=fname)

        WRITE(lunxml,'(A)')'<?xml version="1.0"?>'
        WRITE(lunxml,'(A)')'<?xml-stylesheet type="text/xsl" href="style.xsl"?>'
        WRITE(lunxml,'(A)')'<STAT>'
        WRITE(lunxml,'(3A)')'<TAG>',TRIM(tag),'</TAG>'
        WRITE(lunxml,'(3A)')'<VAR>',TRIM(varprop(j)%text),'</VAR>'
        WRITE(lunxml,'(3A)')'<UNIT>',TRIM(varprop(j)%unit),'</UNIT>'
        IF ( periods(i) == 0 ) THEN
           WRITE(lunxml,'(A,I8,A,I8,A)')'<PERIOD>',sdate,'-',edate,'</PERIOD>'
        ELSE
           WRITE(lunxml,'(A,I8,A)')'<PERIOD>',periods(i),'</PERIOD>'
        ENDIF

        CLOSE(lunxml)

    ENDDO
 ENDDO

END SUBROUTINE open_xml
