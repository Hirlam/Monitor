SUBROUTINE sumup_gross(gross_error,total_amount)

 USE types
 USE data, ONLY : obs,hir,maxstn,nparver,obstype,lunqc

 IMPLICIT NONE

 INTEGER, INTENT(INOUT) :: gross_error(2,maxstn,nparver)
 INTEGER, INTENT(INOUT) :: total_amount(maxstn,nparver)

 INTEGER :: i,j,gross_pos(3),              &
            gross_sum(2,nparver),          &
            total_sum

 !
 ! Gross error statistics
 !

 gross_sum = 0

 j = 0
 GROSS_LOOP : DO

    gross_pos = MAXLOC(gross_error)

    IF ( j == obs(gross_pos(2))%stnr .OR.       &
         SUM(gross_error(:,gross_pos(2),:)) == 0 ) EXIT GROSS_LOOP

    IF ( j == 0 ) THEN
       WRITE(lunqc,*)
       WRITE(lunqc,*)'Quality control summary'
       WRITE(lunqc,*)
    ENDIF

    WRITE(lunqc,*)'Station :',hir(gross_pos(2))%stnr,&
                              hir(gross_pos(2))%lat ,&
                              hir(gross_pos(2))%lon
    DO i=1,nparver
       IF ( ANY(gross_error(:,gross_pos(2),i) > 0 ) ) &
       WRITE(lunqc,*)'Variable ',obstype(i),gross_error(:,gross_pos(2),i)
    ENDDO

    gross_sum = gross_sum + gross_error(:,gross_pos(2),:)
    gross_error(:,gross_pos(2),:) = 0
    j = obs(gross_pos(2))%stnr

 ENDDO GROSS_LOOP


 !
 ! Print total
 !

 WRITE(lunqc,*)
 WRITE(lunqc,*) 'Rejection statistics (rejected,skipped,total)'
 DO j=1,nparver
    total_sum = SUM(total_amount(:,j))
    WRITE(lunqc,'(A8,3I8)')obstype(j),gross_sum(:,j),total_sum
 ENDDO

 RETURN

END SUBROUTINE sumup_gross
