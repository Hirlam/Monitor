SUBROUTINE print_cont(p1,p2)

 USE data
 USE contingency
 USE mymagics

 IMPLICIT NONE

 INTEGER, INTENT(IN) :: p1,p2

 INTEGER :: i,j,l,m,period

 INTEGER, ALLOCATABLE :: sumcol(:)

 CHARACTER(LEN= 25) :: cform='(A20,XXI9,X,A1,X,I9)'
 CHARACTER(LEN= 25) :: hform='(XXX,A)'
 CHARACTER(LEN= 20) :: ctmp = '',ctmp2 = ''
 CHARACTER(LEN=100) :: cwrk = ''
 CHARACTER(LEN=  8) :: cperiod = ''

 !----------------------------------------------------------------
 ! Set filename
 IF ( p1 < 999999 ) THEN
    period = p1
 ELSE
    period = 0
 ENDIF

 WRITE(cperiod,'(I8.8)')period

 DO i=1,ncont_param
    DO j=1,nparver

       IF ( j /= cont_table(i)%ind ) CYCLE

      CALL make_fname('c',period,0,tag,           &
                       obstype(j)(1:2),           &
                       obstype(j)(3:len_lab),     &
                       output_mode,2,             &
                       contfile)

       contfile = TRIM(contfile)//'.html'

       OPEN(UNIT=luncont,FILE=contfile)
       WRITE(luncont,*)'<pre>'
       WRITE(luncont,*)

       ALLOCATE(sumcol(0:cont_table(i)%nclass))

       cwrk = 'Contingency table for'
       CALL pname(obstype(j),ctmp)
       cwrk = TRIM(cwrk)//' '//TRIM(ctmp)
       ctmp = obstype(j)
       ctmp(3:6) = '   '
       CALL yunit(ctmp,ctmp2)
       cwrk = TRIM(cwrk)//' ('//TRIM(ctmp2)//')'
       WRITE(luncont,*)TRIM(cwrk)

       cwrk = 'Area:'//TRIM(tag)//' '
       WRITE(luncont,*)TRIM(cwrk)

       IF ( period == 0 ) THEN
          WRITE(luncont,'(A,I8,A,I8)')' Period:',p1,'-',p2
       ELSE
          WRITE(luncont,'(A,I8)')' Period:',period
       ENDIF

       WRITE(luncont,*)'Limits ',cont_table(i)%limit(1:cont_table(i)%nclass)
       WRITE(luncont,*)'Each class is data <= limit, the very last > last limit'
       WRITE(luncont,*)'Total number of values',cont_table(i)%nval

       WRITE(hform(2:3),'(I2.2)')(cont_table(i)%nclass/2+1)*9+10
       WRITE(cform(6:7),'(I2.2)')cont_table(i)%nclass+1


       DO l=1,nexp

          DO m=0,cont_table(i)%nclass
             sumcol(m) = SUM(cont_table(i)%table(l,m,:))
          ENDDO

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

 CLOSE(luncont)

 RETURN

END SUBROUTINE print_cont
