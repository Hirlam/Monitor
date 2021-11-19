SUBROUTINE polygon_selection

!
!  Station selection by polygon area
!  Ulf Andrae, SMHI, 2002-2021
!

 USE data

 IMPLICIT NONE

 ! Parameters
 INTEGER, PARAMETER :: LUNPOL = 13 ! Unit for polygon area
 INTEGER :: i,j,k,ierr

 TYPE poltype
   REAL, DIMENSION(:,:), ALLOCATABLE :: pot
   INTEGER :: npol
 END TYPE poltype

 TYPE(poltype), ALLOCATABLE, TARGET :: pol(:)

 LOGICAL :: inside,ldum

 INTEGER, POINTER :: npol => NULL()

 !-----------------------------------------------------------------

 ! Check how many files
 k = 0
 DO j=1,SIZE(polyfile)
   IF ( TRIM(polyfile(j)) == '#' ) CYCLE
   k = k + 1
 ENDDO

 ALLOCATE(pol(k))

 ! Read polygon files
 k=0
 DO j=1,SIZE(polyfile)
   IF ( TRIM(polyfile(j)) == '#' ) CYCLE
   OPEN(lunpol,FILE=polyfile(j),IOSTAT=ierr,STATUS='OLD') 

   IF (ierr /= 0) THEN
      WRITE(6,*)' No polygon found, no selection performed'
      CYCLE
   ENDIF
   WRITE(6,*)'Read polygon from:',TRIM(polyfile(j))
   k=k+1

   npol => pol(k)%npol
   READ(lunpol,*) npol

   ALLOCATE(pol(k)%pot(2,npol+1))
   pol(k)%pot = 0.

   DO i=1,npol
    READ(lunpol,*)pol(k)%pot(:,i)
   ENDDO

   CLOSE(lunpol)

   !
   ! Make first and last point equal
   !

   npol = npol + 1
   pol(k)%pot(:,npol) = pol(k)%pot(:,1)

 ENDDO

 ! Perform the check
 DO i=1,maxstn
   IF (.NOT. hir(i)%active) CYCLE
   ldum=.FALSE.
   DO j=1,k
     ldum = ( ldum .OR. inside(hir(i)%lon,hir(i)%lat, &
             pol(j)%pot(2,:),pol(j)%pot(1,:),pol(j)%npol) )
   ENDDO
   hir(i)%active = ldum
 ENDDO

 !
 ! Cleaning
 !
 DO j=1,k
   DEALLOCATE(pol(j)%pot)
 ENDDO
 DEALLOCATE(pol)
 NULLIFY(npol)

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
