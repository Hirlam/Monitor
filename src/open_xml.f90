SUBROUTINE open_xml(lunxml,maxper,periods)

 USE constants
 USE functions
 USE data,     ONLY : period_type,period_freq, &
                      nparver,varprop,sdate,edate,tag

 IMPLICIT NONE

 ! Input

 INTEGER, INTENT(   IN) :: lunxml,maxper
 INTEGER, INTENT(INOUT) :: periods(maxper+1)

 ! Local

 INTEGER :: i,j 

 CHARACTER(LEN= 8) :: cmon
 CHARACTER(LEN=50) :: fname,wtext=''

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
           WRITE(wtext,'(I8,A1,I8)')sdate,'-',edate
        ELSEIF(periods(i) < 13) THEN

          SELECT CASE(period_freq)
          CASE(1)
           WRITE(wtext,'(A8,A8)')'Period: ',seasonal_name2(periods(i))
          CASE(3)
           WRITE(wtext,'(A8,A8)')'Period: ',seasonal_name1(periods(i))
          END SELECT

        ELSEIF(periods(i) < 9999 .OR. (period_type == 2 .AND. period_freq == 1)) THEN
          WRITE(wtext,'(A8,I8)')'Period: ',periods(i)
        ELSEIF(periods(i) < 999999 ) THEN
          WRITE(wtext,'(A8,I6,A1,I6)')'Period: ',        &
          periods(i),'-',monincr(periods(i),period_freq-1)
        ELSE
          WRITE(wtext,'(A8,I8,A1,I8)')'Period: ',        &
          sdate,'-',edate
        ENDIF

        WRITE(lunxml,'(3A)')'<PERIOD>',TRIM(wtext),'</PERIOD>'

        CLOSE(lunxml)

    ENDDO
 ENDDO

END SUBROUTINE open_xml
