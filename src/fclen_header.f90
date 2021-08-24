SUBROUTINE fclen_header(lfclen,nuf,uh,uf,ai,ofs,txt)

 IMPLICIT NONE

 LOGICAL,          INTENT( IN) :: lfclen
 INTEGER,          INTENT( IN) :: nuf,ai,ofs
 LOGICAL,          INTENT( IN) :: uh(0:23),uf(0:nuf)
 CHARACTER(LEN=*), INTENT(OUT) :: txt
 
 ! Local

 INTEGER :: i,ii,j,uhl(1:24)

 INTEGER, ALLOCATABLE :: fclen(:)

 LOGICAL :: lfirst = .TRUE.
 
 CHARACTER(LEN=20) :: wname = ''
 CHARACTER(LEN=50) :: whour = ''
 CHARACTER(LEN= 2) :: wh    = ''
 CHARACTER(LEN=20) :: wofs  = ''
 CHARACTER(LEN= 7) :: txt5  = ''

 !-----------------------------------------------
 
 lfirst = .TRUE.

 !
 ! Write initial hours used
 !
 whour = '{'
 j = 0
 uhl = -1
 DO i=0,23 
    IF (uh(i)) THEN
       WRITE(wh,'(I2.2)')i
       IF ( lfirst ) THEN
          whour = TRIM(whour)//wh
          lfirst = .FALSE.
       ELSE
          whour = TRIM(whour)//','//wh
       ENDIF

       j = j + 1
       uhl(j) = i

    ENDIF
 ENDDO
 whour = TRIM(whour)//'}'

 IF ( j == 8               .AND.  &
      uhl(2)-uhl(1) == 3   .AND.  &
      uhl(8) == 21 )              &
     whour = '{00,03,...,21}'
 IF ( j > 8 ) whour = '{All hours}'

 IF ( ofs /= 0 ) THEN
   WRITE(wofs,'(A8,I3.3,A7)')'Max lag:',ofs,'minutes'
 ENDIF

 IF ( lfclen ) THEN

    ii=0      
    IF ( nuf == 0 ) THEN
       ALLOCATE(fclen(1))
       fclen(1) = 0
       ii=1
    ELSE
       ALLOCATE(fclen(nuf))
       DO i=0,nuf
          IF (uf(i)) THEN
             ii = ii + 1
             fclen(ii) = i
          ENDIF
       ENDDO
    ENDIF
    txt =''
    IF (ii > 8 ) THEN
       IF ( ANY(fclen(1:ii) > 99) ) THEN
         wname='(2I4.3,A5,I3.3)'
       ELSE
         wname='(2I3.2,A5,I2.2)'
       ENDIF
       WRITE(txt,wname)fclen(1:2),' ... ',fclen(ii)
    ELSE
       IF ( ai == 0 ) THEN

          IF ( ANY(fclen(1:ii) > 99) ) THEN
            wname='(XX(1X,I3.3))'
          ELSE
            wname='(XX(1X,I2.2))'
          ENDIF
          WRITE(wname(2:3),'(I2.2)')MAX(ii,1)
          WRITE(txt,wname)fclen(1:ii)
       ELSE
          !
          ! Write fclen differences in case of accumulated values
          !
         
          txt =''
          DO i=1,ii
             IF ( fclen(i) > 99 ) THEN
               WRITE(txt5,'(I3.2,A1,I3.2)')fclen(i),'-',fclen(i)-ai
             ELSE
               WRITE(txt5,'(I2.2,A1,I2.2)')fclen(i),'-',fclen(i)-ai
             ENDIF
             txt = TRIM(txt)//' '//txt5
          ENDDO

       ENDIF
    ENDIF
    txt = 'Used '//TRIM(whour)//' +'//TRIM(txt)//' '//TRIM(wofs)
 ELSE
    txt = 'Hours: '//TRIM(whour)//' '//TRIM(wofs)
 ENDIF

 IF (lfclen ) DEALLOCATE(fclen)

END SUBROUTINE fclen_header
