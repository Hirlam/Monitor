SUBROUTINE fclen_header(lfclen,nuf,uh,uf,txt)

 IMPLICIT NONE

 LOGICAL,          INTENT( IN) :: lfclen
 INTEGER,          INTENT( IN) :: nuf
 LOGICAL,          INTENT( IN) :: uh(0:23),uf(0:nuf)
 CHARACTER(LEN=*), INTENT(OUT) :: txt
 
 ! Local

 INTEGER :: i,ii

 INTEGER, ALLOCATABLE :: fclen(:)

 LOGICAL :: lfirst = .TRUE.
 
 CHARACTER(LEN=20) :: wname = ''
 CHARACTER(LEN=50) :: whour = ''
 CHARACTER(LEN= 2) :: wh    = ''

 !-----------------------------------------------
 
 lfirst = .TRUE.

 whour = '{'
 DO i=0,23 
    IF (uh(i)) THEN
       WRITE(wh,'(I2.2)')i
       IF ( lfirst ) THEN
          whour = TRIM(whour)//wh
          lfirst = .FALSE.
       ELSE
          whour = TRIM(whour)//','//wh
       ENDIF

    ENDIF
 ENDDO
 whour = TRIM(whour)//'}'

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
    IF (ii > 10 ) THEN
       wname='(2I3.2,A5,I2.2)'
       WRITE(txt,wname)fclen(1:2),' ... ',fclen(ii)
    ELSE
       wname='(XX(1X,I2.2))'
       WRITE(wname(2:3),'(I2.2)')ii
       WRITE(txt,wname)fclen(1:ii)
    ENDIF
    txt = 'At '//TRIM(whour)//' +'//TRIM(txt)
 ELSE
    txt = 'Hours: '//TRIM(whour)
 ENDIF

 IF (lfclen ) DEALLOCATE(fclen)

END SUBROUTINE fclen_header
