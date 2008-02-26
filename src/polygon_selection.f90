SUBROUTINE polygon_selection

!
!  Station selection by polygon area
!

 USE data

 IMPLICIT NONE

 ! Parameters
 INTEGER, PARAMETER :: LUNPOL = 13 ! Unit for polygon area
 INTEGER :: i,ierr,npol

 REAL, DIMENSION(:,:), ALLOCATABLE :: pol

 LOGICAL :: inside

 !-----------------------------------------------------------------
 ! Read polygon file

 OPEN(lunpol,FILE=polyfile,IOSTAT=ierr,STATUS='OLD') 

 IF (ierr /= 0) THEN
    WRITE(6,*)' No polygon found, no selection performed'
    RETURN
 ENDIF

 READ(lunpol,*) npol

 ALLOCATE(pol(2,npol+1))
 pol = 0.

 DO i=1,npol
    READ(lunpol,*)pol(:,i)
 ENDDO

 CLOSE(lunpol)

 !
 ! Make first and last point equal
 !

 npol = npol + 1
 pol(:,npol) = pol(:,1)

 DO i=1,maxstn

    IF (.NOT. hir(i)%active) CYCLE
    hir(i)%active = inside(hir(i)%lon,hir(i)%lat,pol(2,:),pol(1,:),npol)

 ENDDO

 RETURN

END SUBROUTINE polygon_selection
! -----------------------------------------------
! -----------------------------------------------
! -----------------------------------------------
      Function Inside(a,b,x,y,n)
!     This function returns a value of true if the point 
!     a,b lies within the polygon defined by vertices
!     x(n),y(n) and a value of false othervise. The
!     first and last elements of the vectors x and y
!     must be identical (x(1)=x(n), y(1)=y(n)).
!
!     The boundary is excluded from the polygon
!    
!     Author: Carl Fortelius
!     Date:   13. JAN. 1997
!
      implicit none
      integer n,i,nle,nge
      real a, b, x(n), y(n), yc
      Logical inside

      inside=.false.

      nle=0
      nge=0
      Do i=2,n
      If ( (x(i-1).le.a) .neqv. (x(i).le.a)) then
        yc = y(i-1) + (y(i)-y(i-1))/(x(i)-x(i-1))*(a-x(i-1)) 
        if(yc.le. b) nle=nle+1        
        if(yc.ge. b) nge=nge+1        
      End if
      End do

      inside=  (mod(nle,2).ne.0) .and.(mod(nge,2).ne.0)

      End      
