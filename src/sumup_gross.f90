SUBROUTINE sumup_gross(gross_error)

 USE types
 USE data, ONLY : obs,hir,maxstn,nparver,obstype

 IMPLICIT NONE

 INTEGER, INTENT(INOUT) :: gross_error(maxstn,nparver)
 INTEGER :: i,j,gross_pos(2),gross_sum(nparver)

 !
 ! Gross error statistics
 !

 gross_sum = 0

 j = 0
 GROSS_LOOP : DO

    gross_pos = MAXLOC(gross_error)

    IF ( j == obs(gross_pos(1))%stnr .OR.       &
         SUM(gross_error(gross_pos(1),:)) == 0 ) EXIT GROSS_LOOP

    IF ( j == 0 ) THEN
       WRITE(6,*)
       WRITE(6,*)'Quality control summary'
       WRITE(6,*)
    ENDIF

    WRITE(6,*)'Station :',hir(gross_pos(1))%stnr,&
                          hir(gross_pos(1))%lat ,&
                          hir(gross_pos(1))%lon
    DO i=1,nparver
       IF ( gross_error(gross_pos(1),i) > 0 ) &
       WRITE(6,*)'Variable ',obstype(i),gross_error(gross_pos(1),i)
    ENDDO

    gross_sum = gross_sum + gross_error(gross_pos(1),:)
    gross_error(gross_pos(1),:) = 0
    j = obs(gross_pos(1))%stnr

 ENDDO GROSS_LOOP

 !
 ! Print total
 !

 WRITE(6,*)
 WRITE(6,*) 'Total number of rejected observations'
 DO j=1,nparver
    WRITE(6,*)obstype(j),gross_sum(j)
 ENDDO

 RETURN

END SUBROUTINE sumup_gross
