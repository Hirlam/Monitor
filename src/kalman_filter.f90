!SUBROUTINE kalman_test
!
 !IMPLICIT NONE
!
 !INTEGER, PARAMETER :: ntim = 28
 !INTEGER, PARAMETER :: nexp = 1
 !REAL :: dat(28) =(/-1,-3,-2,2,1,0,1,-1,-3,-5, -7,-7,-8,-11, -9,-13,-13,-12,-15,-15,-12,-15,-13,-12,-14,-12,-13,-15/)
 !REAL :: obs(28) =(/ 1, 2, 3,6,7,5,7, 3, 0, -1,-2,-3,-5, -4, -6, -7,-15,-18,-22,-20,-21,-22,-21,-20,-22,-20,-21,-22/)
!
!
 !CALL kalman_filter(ntim,nexp,obs,dat)
!
!END SUBROUTINE kalman_test
SUBROUTINE kalman_filter(ntim,nexp,obs,dat)
      
 USE kalman

 IMPLICIT NONE

 INTEGER, INTENT(IN) :: ntim,nexp
 REAL,    INTENT(INOUT) :: obs(ntim),dat(ntim,nexp)


 INTEGER :: I,J

 REAL :: Q(2,2),Q0(2,2),D(1,1),F1,F2


 REAL :: X(2),yt(1,1),value(2)
 !------------------------------------------------------


 Q0(1,1) = 20.
 Q0(2,2) = 10.


 EXP_LOOP : DO j=1,nexp

    Q       = Q0
    D       = 5.
    F1      = 0.001
    F2      = 0.007
!      WRITE(6,*)'MOD KOR OBS'

!      i=0
!      WRITE(6,*)i,dat(i+1,j),dat(i+1,j),obs(i+1)
    DO i=1,ntim-1

       value(1) = 1.0
       value(2) = dat(i,j) 
       yt         = obs(i) - dat(i,j)
       CALL KALMAN2(X,YT,Q,F1,F2,Q0,D,VALUE)

!      WRITE(6,*)i+1,dat(i+1,j),dat(i+1,j)+x(1)+x(2)*dat(i+1,j),obs(i+1)

       dat(i+1,j) = dat(i+1,j)+x(1)+x(2)*dat(i+1,j)

    ENDDO

 ENDDO EXP_LOOP


 RETURN
END SUBROUTINE kalman_filter
