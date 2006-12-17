SUBROUTINE open_xml(lunxml,maxper,periods)

 USE data,     ONLY : period_type,nparver,obstype,sdate,edate
 USE mymagics, ONLY : pname,yunit

 IMPLICIT NONE

 ! Input

 INTEGER, INTENT(   IN) :: lunxml,maxper
 INTEGER, INTENT(INOUT) :: periods(maxper+1)

 ! Local

 INTEGER :: i,j 

 CHARACTER(LEN= 8) :: cmon
 CHARACTER(LEN=50) :: fname,cname,cunit

 !---------------------------------------------------------

 !
 ! Prepare xml file for station statistics
 !

 IF ( period_type == 1 ) periods = 0

 DO j=1,nparver
    CALL pname(obstype(j),cname)
    CALL yunit(obstype(j)(1:2),cunit)
    DO i=1,maxper

        WRITE(cmon(1:8),'(I8.8)')periods(i)
        fname = TRIM(obstype(j))//'_'//TRIM(cmon)//'.xml'
        OPEN(lunxml,file=fname)

        WRITE(lunxml,'(A)')'<?xml version="1.0"?>'
        WRITE(lunxml,'(A)')'<?xml-stylesheet type="text/xsl" href="style.xsl"?>'
        WRITE(lunxml,'(A)')'<STAT>'
        WRITE(lunxml,'(3A)')'<VAR>',TRIM(cname),'</VAR>'
        WRITE(lunxml,'(3A)')'<UNIT>',TRIM(cunit),'</UNIT>'
        IF ( periods(i) == 0 ) THEN
           WRITE(lunxml,'(A,I,A,I,A)')'<PERIOD>',sdate,'-',edate,'</PERIOD>'
        ELSE
           WRITE(lunxml,'(A,I,A)')'<PERIOD>',periods(i),'</PERIOD>'
        ENDIF

        CLOSE(lunxml)

    ENDDO
 ENDDO

END SUBROUTINE open_xml
