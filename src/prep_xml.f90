SUBROUTINE prep_xml(lunxml,nparver,nr,time,scat)

 !
 ! Print and xml file including station
 ! statistics based on the scatter array
 !
 ! Ulf Andrae, SMHI, 2006
 !

 USE types
 USE timing
 USE data, ONLY : varprop,expname,nexp,ltiming

 IMPLICIT NONE

 ! INPUT

 INTEGER,            INTENT(IN) :: lunxml,nparver,nr,time
 TYPE(scatter_type), INTENT(IN) :: scat(nparver)

 ! LOCAL

 INTEGER :: j,jj,kk,l,timing_id

 REAL    :: xmean(nexp),ymean(nexp),          &
            stdevx(nexp),stdevy(nexp),        &
            bias(nexp),rmse(nexp),            &
            stdv(nexp),corr(nexp)

 REAL, ALLOCATABLE :: xval(:),yval(:)

 CHARACTER(LEN=100) :: fname
 CHARACTER(LEN=  8) :: ctime

 !
 !-----------------------------------------------------
 !

 !
 ! Init timing counter
 !

 timing_id = 0
 IF (ltiming) CALL acc_timing(timing_id,'prep_xml')

 ! Allocate work arrays
 ALLOCATE(xval(MAXVAL(scat%n)))
 ALLOCATE(yval(MAXVAL(scat%n)))

 DO j=1,nparver

    IF ( scat(j)%n < 2 ) CYCLE

    WRITE(ctime,'(I8.8)')time
    fname = TRIM(varprop(j)%id)//'_'//TRIM(ctime)//'.xml'
    OPEN(lunxml,file=fname,POSITION='APPEND')
    WRITE(lunxml,*)'<STATION>'

    DO jj=1,nexp

        kk = scat(j)%n

        xval(1:kk) = scat(j)%dat(1   ,1:kk)
        yval(1:kk) = scat(j)%dat(1+jj,1:kk) + scat(j)%dat(1   ,1:kk)

        IF ( varprop(j)%id == 'DD' ) THEN
           DO l=1,kk
              IF (yval(l) > 360. ) THEN
              yval(l) = yval(l) - 360.         
           ELSEIF(yval(l) <   0. ) THEN
              yval(l) = yval(l) + 360.         
           ENDIF
           ENDDO
        ENDIF

        CALL calc_corr(kk,xval(1:kk),yval(1:kk),   &
                       xmean(jj),ymean,(jj),       &
                       stdevx,(jj),stdevy(jj),     &
                       bias(jj),rmse(jj),          &
                       stdv(jj),corr(jj))


    ENDDO

    WRITE(lunxml,*)'<MEAN_OBS>',xmean(1),'</MEAN_OBS>'
    WRITE(lunxml,*)'<STDV_OBS>',stdevx(1),'</STDV_OBS>'

    DO jj=1,nexp
        WRITE(lunxml,*)'<EXP>'
        WRITE(lunxml,*)'<ID>',nr,'</ID>'
        WRITE(lunxml,*)'<NAME>',TRIM(expname(jj)),'</NAME>'
        WRITE(lunxml,*)'<NUM>',kk,'</NUM>'
        WRITE(lunxml,*)'<MEAN>',ymean(jj),'</MEAN>'
        WRITE(lunxml,*)'<STDV>',stdv(jj),'</STDV>'
        WRITE(lunxml,*)'<BIAS>',bias(jj),'</BIAS>'
        WRITE(lunxml,*)'<RMSE>',rmse(jj),'</RMSE>'
        WRITE(lunxml,*)'<CORR>',corr(jj),'</CORR>'
        WRITE(lunxml,*)'</EXP>'
    ENDDO

    WRITE(lunxml,*)'</STATION>'
    CLOSE(lunxml)

  ENDDO

  IF (ltiming) CALL acc_timing(timing_id,'prep_xml')

  DEALLOCATE(xval,yval)


RETURN
END SUBROUTINE prep_xml
