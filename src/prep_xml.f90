SUBROUTINE prep_xml(lunxml,nparver,nr,time,scat)

 !
 ! Print and xml file including station
 ! statistics based on the scatter array
 !
 ! Ulf Andrae, SMHI, 2006
 !

 USE types
 USE timing
 USE data, ONLY : obstype,expname,nexp,dd_ind,ltiming

 IMPLICIT NONE

 ! INPUT

 INTEGER,            INTENT(IN) :: lunxml,nparver,nr,time
 TYPE(scatter_type), INTENT(IN) :: scat(nparver)

 ! LOCAL

 INTEGER :: i,j,jj,k,kk,l,timing_id

 REAL    :: xmean,ymean,          &
            stdevx,stdevy,        &
            bias,rmse,corr

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
    fname = TRIM(obstype(j))//'_'//TRIM(ctime)//'.xml'
    OPEN(lunxml,file=fname,POSITION='APPEND')
    WRITE(lunxml,*)'<STATION>'

    DO jj=1,nexp

        kk = scat(j)%n

        xval(1:kk) = scat(j)%dat(1   ,1:kk)
        yval(1:kk) = scat(j)%dat(1+jj,1:kk) + scat(j)%dat(1   ,1:kk)

        IF ( j == dd_ind ) THEN
           DO l=1,kk
              IF (yval(l) > 360. ) THEN
              yval(l) = yval(l) - 360.         
           ELSEIF(yval(l) <   0. ) THEN
              yval(l) = yval(l) + 360.         
           ENDIF
           ENDDO
        ENDIF

        CALL calc_corr(kk,xval(1:kk),yval(1:kk),   &
                       xmean,ymean,stdevx,stdevy,  &
                       bias,rmse,corr)

        IF ( jj == 1 ) THEN
           WRITE(lunxml,*)'<MEAN_OBS>',xmean,'</MEAN_OBS>'
           WRITE(lunxml,*)'<STDV_OBS>',stdevx,'</STDV_OBS>'
        ENDIF

        WRITE(lunxml,*)'<EXP>'
        WRITE(lunxml,*)'<ID>',nr,'</ID>'
        WRITE(lunxml,*)'<NAME>',TRIM(expname(jj)),'</NAME>'
        WRITE(lunxml,*)'<NUM>',kk,'</NUM>'
        WRITE(lunxml,*)'<MEAN>',ymean,'</MEAN>'
        WRITE(lunxml,*)'<STDV>',stdevy,'</STDV>'
        WRITE(lunxml,*)'<BIAS>',bias,'</BIAS>'
        WRITE(lunxml,*)'<RMSE>',rmse,'</RMSE>'
        WRITE(lunxml,*)'<CORR>',corr,'</CORR>'
        WRITE(lunxml,*)'</EXP>'

     ENDDO

     WRITE(lunxml,*)'</STATION>'
     CLOSE(lunxml)

  ENDDO

  IF (ltiming) CALL acc_timing(timing_id,'prep_xml')

  DEALLOCATE(xval,yval)


RETURN
END SUBROUTINE prep_xml
