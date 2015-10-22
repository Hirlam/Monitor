SUBROUTINE print_cont(p1,p2,nr,par_active,    &
                      uh,uf)

 USE constants, ONLY : seasonal_name1,seasonal_name2
 USE data
 USE contingency

 IMPLICIT NONE

 INTEGER, INTENT(IN) :: p1,p2,nr,par_active(nparver)

 LOGICAL,            INTENT(IN) :: uh(nparver,0:23),   &
                                   uf(nparver,0:maxfclenval)

 INTEGER :: i,j,l,m,period

 INTEGER, ALLOCATABLE :: sumcol(:)

 CHARACTER(LEN= 25) :: cform='(A30,XXI9,X,A1,X,I9)'
 CHARACTER(LEN= 25) :: hform='(XXXX,A)'
 CHARACTER(LEN=100) :: cwrk = '',wname=''
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

      cwrk = TRIM(tag)//TRIM(cini_hours)
      CALL make_fname('c',period,0,cwrk,           &
                       varprop(j)%id,varprop(j)%lev, &
                       output_mode,2,             &
                       contfile)

       contfile = TRIM(contfile)//'.html'

       OPEN(UNIT=luncont,FILE=contfile)
       WRITE(luncont,*)'<pre>'
       WRITE(luncont,*)

       ALLOCATE(sumcol(0:cont_table(i)%nclass))

       cwrk = 'Contingency table for'
       cwrk = TRIM(cwrk)//' '//TRIM(varprop(j)%text)
       cwrk = TRIM(cwrk)//' ('//TRIM(varprop(j)%unit)//')'
       WRITE(luncont,*)TRIM(cwrk)

       ! Line 1
       IF(ALLOCATED(station_name).AND. nr > 0 ) THEN
          cwrk=' Station: '//trim(station_name(csi))
       ELSE
          WRITE(cwrk(1:8),'(I8)')nr
          cwrk=' Station: '//trim(cwrk(1:8))
       ENDIF
       IF (nr == 0) THEN
          wname=''
          WRITE(wname(1:5),'(I5)')par_active(j)
          cwrk=TRIM(wname)//' stations'
          IF ( TRIM(tag) /= '#' ) cwrk=' Selection: '//TRIM(tag)//'  '//TRIM(cwrk)
       ENDIF

       WRITE(luncont,'(A)')TRIM(cwrk)

       ! Line 3
       wname = ''
       IF (p1 == 0 ) THEN
       ELSEIF(p1 < 13) THEN
   
         SELECT CASE(period_freq)
          CASE(1)
           WRITE(wname,'(A8,A8)')'Period: ',seasonal_name2(p1)
          CASE(3)
           WRITE(wname,'(A8,A8)')'Period: ',seasonal_name1(p1)
         END SELECT
   
       ELSEIF(p1 < 999999 ) THEN
          WRITE(wname,'(A8,I8)')'Period: ',p1
       ELSE
          WRITE(wname,'(A8,I8,A1,I8)')'Period: ',p1,'-',p2
       ENDIF

       WRITE(luncont,'(A)')TRIM(wname)

       ! Line 4
       IF ( show_fc_length ) THEN
          CALL fclen_header(.TRUE.,maxfclenval,        &
                            uh(j,:),uf(j,:),           &
                            varprop(j)%acc,cwrk)

       ENDIF
       WRITE(luncont,'(A)')TRIM(cwrk)

       WRITE(luncont,*)'Limits ',cont_table(i)%limit(1:cont_table(i)%nclass)
       WRITE(luncont,*)'Each class is data <= limit, the very last > last limit'
       WRITE(luncont,*)'Total number of values',cont_table(i)%nval

       WRITE(hform(2:4),'(I3.3)')(cont_table(i)%nclass/2+1)*9+10
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

       WRITE(luncont,*)
       CLOSE(luncont)

    ENDDO
 ENDDO


 RETURN

END SUBROUTINE print_cont
