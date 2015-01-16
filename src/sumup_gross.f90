SUBROUTINE sumup_gross(gross_error,total_amount)

 USE types
 USE data, ONLY : obs,hir,maxstn,nparver,lunqc, &
                  varprop,nfclengths,qc_fclen

 IMPLICIT NONE

 INTEGER, INTENT(INOUT) :: gross_error(2,maxstn,nparver)
 INTEGER, INTENT(INOUT) :: total_amount(maxstn,nparver)

 INTEGER :: i,j,gross_pos(3),              &
            gross_sum(2,nparver),          &
            total_sum

 CHARACTER(LEN=100) :: cfclen
 CHARACTER(LEN=3) :: cwrk

 !
 ! Gross error statistics
 !

 gross_sum = 0

 cfclen = ''
 DO i=1,nfclengths
   IF(qc_fclen(i) == -1 ) EXIT
   IF ( qc_fclen(i) > 99 ) THEN
     WRITE(cwrk,'(I3.3)')qc_fclen(i)
   ELSE
     WRITE(cwrk,'(I2.2)')qc_fclen(i)
   ENDIF
   cfclen = TRIM(cfclen)//' '//TRIM(cwrk) 
 ENDDO

 j = 0
 GROSS_LOOP : DO

    gross_pos = MAXLOC(gross_error)

    IF ( j == obs(gross_pos(2))%stnr .OR.       &
         SUM(gross_error(:,gross_pos(2),:)) == 0 ) EXIT GROSS_LOOP

    IF ( j == 0 ) THEN
       WRITE(lunqc,*)
       WRITE(lunqc,*)
       WRITE(lunqc,*)'Quality control summary'
       WRITE(lunqc,*)' Forecast lengths used:',TRIM(cfclen)
       WRITE(lunqc,*)' Rejected: Did not pass the gross error check'
       WRITE(lunqc,*)' Skipped: No forecasts available for QC check'
       WRITE(lunqc,*)
       WRITE(lunqc,'(A8,A9,2(6X,A3))')'Station:','ID','LAT','LON'
       WRITE(lunqc,'(2X,A9,11X,2A12)')'PARAMETER','Rejected','Skipped'
    ENDIF

    WRITE(lunqc,*)
    WRITE(lunqc,'(A9,I8,2(2X,F7.3))') &
     'Station :',hir(gross_pos(2))%stnr,&
     hir(gross_pos(2))%lat ,&
     hir(gross_pos(2))%lon

    DO i=1,nparver
       IF ( ANY(gross_error(:,gross_pos(2),i) > 0 ) ) &
       WRITE(lunqc,'(2X,A20,2I12)')varprop(i)%text,gross_error(:,gross_pos(2),i)
    ENDDO

    gross_sum = gross_sum + gross_error(:,gross_pos(2),:)
    gross_error(:,gross_pos(2),:) = 0
    j = obs(gross_pos(2))%stnr

 ENDDO GROSS_LOOP


 !
 ! Print total
 !

 WRITE(lunqc,*)
 WRITE(lunqc,'(A,10X,A)') 'Rejection summary', &
  'Rejected    Skipped       Total'
 DO j=1,nparver
    total_sum = SUM(total_amount(:,j))
    WRITE(lunqc,'(2X,A10,10X,3I12)')varprop(j)%text,gross_sum(:,j),total_sum
 ENDDO

 RETURN

END SUBROUTINE sumup_gross
