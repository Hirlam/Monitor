SUBROUTINE freq_dist(npar,ntim,ncla,            &
                     mincla,maxcla,classtype,   &
                     pre_fcla,                  &
                     data,fdat,fcla)

!
! Compute frequency distribution of a serie
! The classes are defined as
!
! [ fcla(1) - fcla(2) ] fcla(3) ] ... fcal(n) ]  
!
! Three choices of input are given
! 1.  mincla > maxcla  =>
!     mincla = fcla(1   ) = MINVAL(data)
!     maxcla = fcla(ncla) = MAXVAL(data)
!     npre_cla := 1
!
! 2. mincla < maxcla, given by user
!
! 3. ANY(pre_fcla) != 0.
!    Class definitions are given in pre_fcla
!
! Classtype defines linear(0) or log10(1)
!
! Ulf Andrae, ECMWF, 2003
!

 IMPLICIT NONE

 ! Input
 INTEGER, INTENT(IN    ) :: npar,ntim,ncla,        &
                            classtype
 REAL,    INTENT(INOUT ) :: mincla,maxcla
 REAL,    INTENT(IN    ) :: data(ntim,npar),       &
                            pre_fcla(ncla)
 REAL,    INTENT(OUT   ) :: fdat(ncla,npar),       &
                            fcla(ncla)

 ! Local

 INTEGER :: i,j,k,num_rej
 REAL :: dcla

 !----------------------------------------

 !
 ! Define classes
 !

 fcla = 0.
 IF (ANY(ABS(pre_fcla) > 1.e-6 ) ) THEN
 
    ! Classes defined by user

    fcla = pre_fcla

 ELSE

    IF (mincla > maxcla ) THEN

    ! Find min/max limits from data

       mincla =  MINVAL(data)
       maxcla =  MAXVAL(data)
       mincla =  mincla * (1 - SIGN(0.01,mincla))
       maxcla =  maxcla * (1 + SIGN(0.01,maxcla))
    ENDIF

    SELECT CASE(classtype) 

    CASE(0)
    
       ! Linear limits
   
       dcla = (maxcla - mincla ) / FLOAT(ncla-1)
       
       fcla(1) = mincla
       DO i = 2,ncla
          fcla(i) = fcla(i-1) + dcla
       ENDDO

!      work = data
   
    CASE(1)

       ! Logarithmic limits

       maxcla = LOG10(maxcla)
       mincla = LOG10(mincla)
       dcla = (maxcla - mincla ) / FLOAT(ncla)
       
       fcla(1) = mincla + dcla
       DO i = 2,ncla
          fcla(i) = fcla(i-1) + dcla
       ENDDO

!      work = LOG10(data)
   
    CASE DEFAULT
      
       WRITE(6,*)'This frequency type is not implemented', classtype
   
    END SELECT
 
 ENDIF

 !
 ! Classify data
 !
 
 fdat    = 0.

 DO j = 1,npar
    num_rej = 0
    IC : DO i = 1,ntim
    
      IF ( classtype /= 1 ) THEN
        k = 1
        IF( data(i,j) < fcla(k) ) THEN
         num_rej = num_rej + 1
         CYCLE IC
        ENDIF
      ENDIF

      KC : DO k = 2,ncla

         IF( data(i,j) <= fcla(k) ) THEN
            fdat(k,j) = fdat(k,j) + 1.
            CYCLE IC
         ENDIF

      ENDDO KC

      num_rej = num_rej + 1

    ENDDO IC

    IF (num_rej > 0 ) THEN
       WRITE(6,*)'Number of data excluded from the statistics :',num_rej,j
    ENDIF

 ENDDO


 RETURN

END SUBROUTINE freq_dist
