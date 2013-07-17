SUBROUTINE fclen_header(lfclen,nuf,uh,uf,ai,txt)

 IMPLICIT NONE

 LOGICAL,          INTENT( IN) :: lfclen
 INTEGER,          INTENT( IN) :: nuf,ai
 LOGICAL,          INTENT( IN) :: uh(0:23),uf(0:nuf)
 CHARACTER(LEN=*), INTENT(OUT) :: txt
 
 ! Local

 INTEGER :: i,ii,j

 INTEGER, ALLOCATABLE :: fclen(:)

 LOGICAL :: lfirst = .TRUE.
 
 CHARACTER(LEN=20) :: wname = ''
 CHARACTER(LEN=50) :: whour = ''
 CHARACTER(LEN= 2) :: wh    = ''
 CHARACTER(LEN= 7) :: txt5  = ''

 !-----------------------------------------------
 
 lfirst = .TRUE.

 whour = '{'
 j = 0
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

    ENDIF
 ENDDO
 whour = TRIM(whour)//'}'

 IF ( j > 8 ) whour = '{All hours}'

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
       wname='(2I4.3,A5,I3.3)'
       WRITE(txt,wname)fclen(1:2),' ... ',fclen(ii)
    ELSE
       IF ( ai == 0 ) THEN
          wname='(XX(1X,I3.2))'
          WRITE(wname(2:3),'(I2.2)')MAX(ii,1)
          WRITE(txt,wname)fclen(1:ii)
       ELSE
          !
          ! Write fclen differences in case of accumulated values
          !
         
          txt =''
          DO i=1,ii
             WRITE(txt5,'(I3.2,A1,I3.2)')fclen(i),'-',fclen(i)-ai
             txt = TRIM(txt)//' '//txt5
          ENDDO

       ENDIF
    ENDIF
    txt = 'Used '//TRIM(whour)//' +'//TRIM(txt)
 ELSE
    txt = 'Hours: '//TRIM(whour)
 ENDIF

 IF (lfclen ) DEALLOCATE(fclen)

END SUBROUTINE fclen_header