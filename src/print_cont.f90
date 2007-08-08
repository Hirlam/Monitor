SUBROUTINE print_cont

 USE data
 USE contingency
 USE mymagics

 IMPLICIT NONE

 INTEGER :: i,j,k,l,m

 CHARACTER(LEN=  6) :: cform='(XXI7)'
 CHARACTER(LEN= 20) :: ctmp = '',ctmp2 = ''
 CHARACTER(LEN=100) :: cwrk = ''

 !----------------------------------------------------------------

 WRITE(luncont,*)
 DO i=1,ncont_param
    DO j=1,nparver

       IF ( j /= cont_table(i)%ind ) CYCLE
       cwrk = 'Contingency table for '
       CALL pname(obstype(j),ctmp)
       cwrk = TRIM(cwrk)//' '//TRIM(ctmp)
       ctmp = obstype(j)
       ctmp(3:6) = '   '
       CALL yunit(ctmp,ctmp2)
       cwrk = TRIM(cwrk)//' ('//TRIM(ctmp2)//')'

       WRITE(luncont,*)TRIM(cwrk)
       WRITE(luncont,*)'Limits ',cont_table(i)%limit(1:cont_table(i)%nclass)
       WRITE(luncont,*)'In table x=obs, y=fc'
       WRITE(luncont,*)'Total number of values',cont_table(i)%nval

       WRITE(cform(2:3),'I2.2')cont_table(i)%nclass

       DO l=1,nexp
          WRITE(luncont,*)'Experiment ',TRIM(expname(l))
          DO m=1,cont_table(i)%nclass
             WRITE(luncont,cform)cont_table(i)%table(l,1:,m)
          ENDDO
       ENDDO

    ENDDO
    WRITE(luncont,*)
 ENDDO

 RETURN

END SUBROUTINE print_cont
