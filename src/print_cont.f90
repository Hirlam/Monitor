SUBROUTINE print_cont

 USE data
 USE contingency
 USE mymagics

 IMPLICIT NONE

 INTEGER :: i,j,k,l,m

 INTEGER, ALLOCATABLE :: sumcol(:)

 CHARACTER(LEN= 25) :: cform='(A10,XXI9,X,A1,X,I9)'
 CHARACTER(LEN= 25) :: hform='(XXX,A)'
 CHARACTER(LEN= 20) :: ctmp = '',ctmp2 = ''
 CHARACTER(LEN=100) :: cwrk = ''

 !----------------------------------------------------------------

 WRITE(luncont,*)
 DO i=1,ncont_param
    DO j=1,nparver

       IF ( j /= cont_table(i)%ind ) CYCLE

       ALLOCATE(sumcol(0:cont_table(i)%nclass))

       cwrk = 'Contingency table for '
       CALL pname(obstype(j),ctmp)
       cwrk = TRIM(cwrk)//' '//TRIM(ctmp)
       ctmp = obstype(j)
       ctmp(3:6) = '   '
       CALL yunit(ctmp,ctmp2)
       cwrk = TRIM(cwrk)//' ('//TRIM(ctmp2)//')'

       WRITE(luncont,*)TRIM(cwrk)
       WRITE(luncont,*)'Limits ',cont_table(i)%limit(1:cont_table(i)%nclass)
       WRITE(luncont,*)'Each class is data <= limit, the very last > last limit'
       WRITE(luncont,*)'Total number of values',cont_table(i)%nval

       WRITE(hform(2:3),'(I2.2)')(cont_table(i)%nclass/2+1)*9+10
       WRITE(cform(6:7),'(I2.2)')cont_table(i)%nclass+1


       DO l=1,nexp

          DO m=0,cont_table(i)%nclass
             sumcol(m) = SUM(cont_table(i)%table(l,m,:))
          ENDDO

          !WRITE(luncont,*)'Experiment ',TRIM(expname(l))
          WRITE(luncont,hform)'OBSERVATION'
          DO m=0,cont_table(i)%nclass
             IF ( m == cont_table(i)%nclass/2 ) THEN
                WRITE(luncont,cform)TRIM(expname(l)),             &
                                    cont_table(i)%table(l,:,m),   &
                            '|',SUM(cont_table(i)%table(l,:,m))
             ELSE
                WRITE(luncont,cform)'          ',                 &
                                    cont_table(i)%table(l,:,m),   &
                            '|',SUM(cont_table(i)%table(l,:,m))
             ENDIF
          ENDDO
          WRITE(luncont,*)
          WRITE(luncont,cform)'SUM       ',sumcol,'|',SUM(sumcol)
       ENDDO

       DEALLOCATE(sumcol)

    ENDDO
    WRITE(luncont,*)
 ENDDO

 RETURN

END SUBROUTINE print_cont
