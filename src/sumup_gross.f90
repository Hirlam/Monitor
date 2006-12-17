SUBROUTINE sumup_gross(gross_error)

 USE types
 USE data, ONLY : obs,hir,maxstn,nparver,obstype

 IMPLICIT NONE

 INTEGER, INTENT(INOUT) :: gross_error(maxstn,nparver)
 INTEGER :: i,j,gross_pos(2)

 !
 ! Gross error statistics
 !

 j = 0
 GROSS_LOOP : DO

    gross_pos = MAXLOC(gross_error)

    IF ( j == obs(gross_pos(1))%stnr .OR.       &
         SUM(gross_error(gross_pos(1),:)) == 0 ) EXIT GROSS_LOOP

    IF ( j == 0 ) THEN
       WRITE(6,*)
       WRITE(6,*)'GROSS ERROR STATISTICS'
       WRITE(6,*)
    ENDIF

    WRITE(6,*)'Station :',hir(gross_pos(1))%stnr,&
                          hir(gross_pos(1))%lat ,&
                          hir(gross_pos(1))%lon
    DO i=1,nparver
       IF ( gross_error(gross_pos(1),i) > 0 ) &
       WRITE(6,*)'Variable ',obstype(i),gross_error(gross_pos(1),i)
    ENDDO

    gross_error(gross_pos(1),:) = 0
    j = obs(gross_pos(1))%stnr

 ENDDO GROSS_LOOP

 RETURN

END SUBROUTINE sumup_gross
