MODULE kalman

 USE types

 IMPLICIT NONE

 SAVE

 INTEGER :: last_kalman_date = 0,               &
            last_kalman_time = 0,               &
            kalman_frequency = 24

 TYPE(kalman_type), ALLOCATABLE :: kalvar(:,:,:)

 LOGICAL :: allocate_kalman = .TRUE.


 CONTAINS 

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
SUBROUTINE print_kalman(i,j,k)

 IMPLICIT NONE

 INTEGER, INTENT(IN) :: i,j,k

 WRITE(6,*)'KALMAN ',kalvar(i,j,k)%q

 RETURN

END SUBROUTINE print_kalman
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
SUBROUTINE clear_kalman

 IMPLICIT NONE

 DEALLOCATE(kalvar)
 allocate_kalman=.TRUE.

 RETURN

END SUBROUTINE clear_kalman
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
SUBROUTINE init_kalman(nexp,nparver,maxstn)

 USE data, ONLY : kalvar_n,kalman_frequency_n
 IMPLICIT NONE

 INTEGER, INTENT(IN) :: nexp,nparver,maxstn

 INTEGER :: i,j,k

 ALLOCATE(kalvar(nexp,nparver,maxstn))


 DO k=1,maxstn
 DO j=1,nparver
    DO i=1,nexp
       kalvar(i,j,k)%q0 = kalvar_n(j)%q0
       kalvar(i,j,k)%q  = kalvar_n(j)%q0
       kalvar(i,j,k)%d  = kalvar_n(j)%d
       kalvar(i,j,k)%f1 = kalvar_n(j)%f1
       kalvar(i,j,k)%f2 = kalvar_n(j)%f2
       kalvar(i,j,k)%x  = 0.
       kalvar(i,j,k)%xn = 0.
       kalvar(i,j,k)%date = 0
       kalvar(i,j,k)%time = 0
    ENDDO
 ENDDO
 ENDDO

 kalman_frequency = kalman_frequency_n

 allocate_kalman = .FALSE.

 RETURN

END SUBROUTINE init_kalman

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      SUBROUTINE KALMAN1(XT,YT,QT,F1,F2,Q0,D)
!
      Real XT,YT,QT,F1,F2,Q0,D,Ybar,delta
!
!     A simple  1-dimensional Kalmanfilter to correct time varying
!     biases, for example generally too warm or too cold 2M
!     temperature forecasts from a NWP/MOS/PPM MODEL. When no data are
!     available the filter decreases slowly towards zero.
!
!     APRIL 1988. URBAN HJORTH-ANDERS PERSSON
!     Revised: NOVEMBER 1991. ANDERS PERSSON-JUHA KILPINEN
!
!     XT = Previous estimation of coefficient
!     QT = Estimated variance
!     Q0 = A priori variance (maximum variation interval)
!     YT = Last observed error in NWP/MOS/PPM forecast
!     D  = Assumed noise in observation, ie. the unexplained noise
!          (suitable choice is the square of the error  a f t e r
!          filtering)
!     F1  = fading term when data is missing ( typical size 0.1 - 0.01)
!     F2  = fading term to simulate "memory" ( typical size 0.03 - 0.001)
!     Ybar = the assumed, outside the filter applied correction
!
!     Filterequations
!
!         X(t+1) = A(t) * X(t) + e(t+1)
!         Y(t)   = B(t) * X(t) + z(t)
!
!         A(t) allows us to use any knowledge of the time evolution of
!         the coefficients, e.g. if they have a dependence on the season
!         whereby their evolution could be modeled as a sinus function.
!         Unfortunately this is not the case with the coefficients
!         in the error equations above. Only the error itself
!         seems to have a seasonal dependence. The only two assumptions
!         we will make about A(t) is that it is=1.0, except when data is
!         missing when it will be 1-F1, where F1 is 0.1-0.01, thus
!         making all the coeffcients converge to zero if any long period
!         without data. It is then assumed that the filter should be = 0
!         and no change to DMO should be made.
!
!         Var(e(t+1)) = C(t+1)
!         C is treated very simple in this program and is not affected
!         by any external input. This can of course be made according to
!         your own choice or experience e.g. by weighting in the success
!         of the last forecast C=C+(YT-B*XT)**2/(D+Q0.
!
!         The external influence in this program, e.g. when the snowcover is
!         coming or is melting away, may be achieved by increasing
!         the covariance and thus the "freedom" of the filter to help it
!         find a new balance.
!
!         Var(z(t)) = D(t) = D   here assumed constant=5, but can be
!         adjusted to different seasons or different data sources, eg.
!         Var(z(t) = 5.0 + abs(E) where E is the last observed error of
!         the  corrected forecast.
!
!         To facilate the understanding of the Kalman filter I have
!         organized it in a way similar to NWP optimum interpolation
!         (with which it has an organic relationship) by first computing
!         a "First Guess" and then add fresh observations to modify this
!         "First Guess". In contrast to normal OI (but in line with the
!         variational analysis) here also the variances are estimated.
!
!-----------Determine A(t)----------------------------------------------
!
      REAL :: A,B,C

      IF(D.gt.95) then
       A = 1.0-F1            ! data are missing, XT(t) -> 0
      ELSE
       A=1.0                 ! no model for XT(t) which is kept persistent
      ENDIF
!
      B = 1.0                ! the bias will be weighted = 1
!
!-------------Computation of "first guesses"---------------------------
!
      XT = A*XT        ! one step ahead "first guess" of coeff.
!
      C = Q0 * F2      ! back ground noise

! alternatively you may e.g. add C=C+((YT-Ybar)**2)*F2 to make the
! adaptivity dependent on the success of last kalmanfiltering.....

      QT = A**2*QT + C ! one step ahead "first guess" of variance
!
!-----Computation of the DELTA term essentially tells us how large-----
!     part of the observed "misfit" YT-Ybar will be used for correction
!
      DELTA = QT*B/(D+B*QT*B)
!
!--------------Update of XT --------------------------------------------
!
      Ybar = B*XT   !expected error, to be compared with observed YT
      XT = XT + DELTA*(YT-Ybar)  !new estimation of coefficient
      Ybar = XT   !bias correction to be applied to NWP/MOS/PPM forecast
!
!--------------Update of Q----------------------------------------------
!
      QT = QT * (1 - DELTA*B)**2 + D*DELTA**2 !new estimation of variance
!
!========================================================================
      RETURN
      END SUBROUTINE KALMAN1
!***************************************************************************
      SUBROUTINE KALMAN2(X,YT,Q,F1,F2,Q0,D,VALUE)
!
!     Two-dimensional Kalmanfilter (with subroutines) which can be extened
!     to any higher dimensions. The set-up is similar to KALMAN1 above.
!     For explanations see general comments under KALMAN1.
!
!     X(t-1) = Previous estimation of coefficient
!     Q  = Estimated variance (2 x 2 matrice)
!     Q0 = A priori variance (2 x 2 matrice)
!     YT = Last observed error in NWP/MOS/PPM forecast
!     D  = Assumed noise in observation, ie. the unexplained noise
!          (suitable choice is the square of the error  a f t e r
!          filtering)
!     F1  = fading term when data is missing ( typical size 0.1 - 0.01)
!     F2  = fading term to simulate "memory" ( typical size 0.03 - 0.001)
!     Ybar = the applied correction to forecast ( = X(t)*VALUE(t))
!
!     MAY 1988. URBAN HJORTH-ANDERS PERSSON
!     Revised: NOVEMBER 1991. ANDERS PERSSON-JUHA KILPINEN
!
!     Filterequations (for comments see KALMAN1):
!
!         X(t+1) = A(t) * X(t) + e(t+1)
!         Y(t)   = B(t) * X(t) + z(t)
!

      INTEGER :: I
      REAL :: F1,F2,CON
      REAL :: X(2),Q(2,2),Q0(2,2),D(1,1),YT(1,1),Ybar(1,1),VALUE(2)
!
      REAL :: A(2,2),E(2,2),C(2,2),DELTA(2,1),B(1,2),BP(2,1), &
      BQ(1,2),BQBP(1,1),QBP(2,1),DELB(2,2),DB(2,2),DB1(2,2),    &
      DBQ(2,2),DBQDB1(2,2),DEL1(2,2),DELD(2,1),DELDDEL1(2,2),   &
      Xbar(2,1),DBX(2,1),DELY(2,1),DBQBP(1,1),AQ(2,2),AQA(2,2)

!
      CALL ZERO(A,2,2)
      CALL ZERO(B,1,2)
      CALL ZERO(E,2,2)

!---------Determine A(t)----------------------------------------------
!
      IF(D(1,1) > 95.) then
       A(1,1) = 1-F1
       A(2,2) = 1-F1        ! data are missing, X(t) -> 0
      ELSE
       A(1,1) = 1.
       A(2,2) = 1.      ! no model for X(t) which is kept persistent
      ENDIF
!
      E(1,1) = 1.
      E(2,2) = 1.
      B(1,1) = 1.
      B(1,2) = VALUE(2)  ! = for example the DMO temperature
!
!-----------Computation of "first guesses"---------------------------
!
!-----of coefficients:
!
      Xbar(1,1)=A(1,1)*X(1)
      Xbar(2,1)=A(2,2)*X(2)  ! one step ahead "first guess" of coeff.
!
!-----of covariances:
!
!
      CALL MULKO(Q0,F2,C,2,2)    ! back ground noise

! you may e.g. add C(1,1)=C(1,1)+((YT(1,1)-(X(1)+X(2)*B(1,2)**2)*F2
! C(2,2)=C(2,2)+((YT(1,1)-(X(1)+X(2)*B(1,2)**2)*F2 as above in KALMAN1

      CALL MMULT3(A,Q,AQ,A,AQA,2,2,2,2)
      CALL MSUM(AQA,C,Q,2,2)   ! one step ahead "first guess" of
!                                Q similar to A**2*QT+C in KALMAN1
!
!-----Computation of DELTA which term essentially tells us how much -----
!     the observed "misfit" (YT-Ybar) will be used for correction
!
      CALL MTRAN(B,BP,1,2)
      CALL MMULT3(B,Q,BQ,BP,BQBP,1,2,2,1)
      CALL MSUM(D,BQBP,DBQBP,1,1)
      CON=1./DBQBP(1,1)
      CALL MMULT(Q,BP,QBP,2,2,1)
      CALL MULKO(QBP,CON,DELTA,2,1)

      if( ABS(YT(1,1) -99.) < 1.e-6 ) CALL MULKO(DELTA,0.,DELTA,2,1)
      if( D(1,1)>95.                ) CALL MULKO(DELTA,0.,DELTA,2,1)

      CALL MMULT(DELTA,B,DELB,2,1,2)
      CALL MDIFF(E,DELB,DB,2,2)      ! similar to 1-delta*B
!
!-----Update of coefficients X similar to XT=XT+delta(YT-Ybar)------------
!
      CALL MMULT(DB,Xbar,DBX,2,2,1)
      CALL MMULT(DELTA,YT,DELY,2,1,1)
      CALL MSUM(DBX,DELY,Xbar,2,1)
      X(1)=Xbar(1,1)
      X(2)=Xbar(2,1)
!
!-----Update of Q similar to (QT(1-DELTA*B)**2 + D*DELTA**2) in KALMAN1---
!
      CALL MTRAN(DB,DB1,2,2)
      CALL MMULT3(DB,Q,DBQ,DB1,DBQDB1,2,2,2,2)
      CALL MTRAN(DELTA,DEL1,2,1)
      CALL MMULT3(DELTA,D,DELD,DEL1,DELDDEL1,2,1,1,2)
      if( D(1,1)< 95.) CALL MSUM(DBQDB1,DELDDEL1,Q,2,2)
!     CALL MSUM(DBQDB1,DELDDEL1,Q,2,2)
!
!---------------------------------------------------------------------
      RETURN
      END SUBROUTINE KALMAN2
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
      SUBROUTINE MDIFF(A,B,C,DIM1,DIM2)
!     Computes difference between matrices A-B=C

      INTEGER DIM1,DIM2,I,J

      REAL :: A(DIM1,DIM2),B(DIM1,DIM2),C(DIM1,DIM2)

      DO 1 I=1,DIM1
      DO 1 J=1,DIM2
       C(I,J)=A(I,J)-B(I,J)
   1  CONTINUE

      RETURN
      END SUBROUTINE MDIFF
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!-------------------------------------------------------------------
      SUBROUTINE MMULT(A,B,C,DIM1,DIM2,DIM3)
!     Computes matrice product A*B=C

      INTEGER :: DIM1,DIM2,DIM3,I,J,K
      REAL    :: A(DIM1,DIM2),B(DIM2,DIM3),C(DIM1,DIM3)

      DO 1 I=1,DIM1
      DO 1 J=1,DIM3
       C(I,J)=0.
         DO 1 K=1,DIM2
           C(I,J)=C(I,J)+A(I,K)*B(K,J)
  1   CONTINUE

      RETURN
      END SUBROUTINE MMULT
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!-------------------------------------------------------------------
      SUBROUTINE MMULT3(A,B,AB,C,ABC,DIM1,DIM2,DIM3,DIM4)
!     Computes matrice product A*B*C=ABC

      INTEGER DIM1,DIM2,DIM3,DIM4
      REAL ::   A(DIM1,DIM2),  B(DIM2,DIM3),C(DIM3,DIM4), &
               AB(DIM1,DIM3),ABC(DIM1,DIM4)
      CALL MMULT(A,B,AB,DIM1,DIM2,DIM3)
      CALL MMULT(AB,C,ABC,DIM1,DIM3,DIM4)

      RETURN
      END SUBROUTINE MMULT3

!-------------------------------------------------------------------
      SUBROUTINE MSUM(A,B,C,DIM1,DIM2)
!     Computes matrice sum A+B=C

      INTEGER DIM1,DIM2,I,J
      REAL ::  A(DIM1,DIM2),B(DIM1,DIM2),C(DIM1,DIM2)

      DO 1 I=1,DIM1
      DO 1 J=1,DIM2
       C(I,J)=A(I,J)+B(I,J)
   1  CONTINUE

      RETURN
      END SUBROUTINE MSUM
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!-------------------------------------------------------------------
      SUBROUTINE MSUM3(A,B,C,ABC,DIM1,DIM2)
!     Computes matrice sum A+B+C=ABC

      INTEGER :: DIM1,DIM2
      REAL    :: A(DIM1,DIM2),B(DIM1,DIM2),C(DIM1,DIM2),ABC(DIM1,DIM2)

      CALL MSUM(A,B,ABC,DIM1,DIM2)
      CALL MSUM(ABC,C,ABC,DIM1,DIM2)

      RETURN
      END SUBROUTINE MSUM3
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!-------------------------------------------------------------------
      SUBROUTINE MTRAN(A,B,DIM1,DIM2)
!     Computes the transpose of matrice A into B

      INTEGER   :: DIM1,DIM2,I,J
      REAL      :: A(DIM1,DIM2),B(DIM2,DIM1)

      DO 1 I=1,DIM1
      DO 1 J=1,DIM2
       B(J,I)=A(I,J)
   1  CONTINUE

      RETURN
      END SUBROUTINE MTRAN
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!-------------------------------------------------------------------
      SUBROUTINE MULKO(A,B,C,DIM1,DIM2)
!     Multiplies matrice A with constant B into matrice C

      INTEGER DIM1,DIM2,I,J
      REAL :: A(DIM1,DIM2),C(DIM1,DIM2)
      REAL B

      DO 1 I=1,DIM1
      DO 1 J=1,DIM2
       C(I,J)=B*A(I,J)
   1  CONTINUE

      RETURN
      END SUBROUTINE MULKO
!--------------------------------------------------------------
      SUBROUTINE ZERO(A,DIM1,DIM2)
!     Sets all matrice A's elements to zero

      INTEGER DIM1,DIM2,I,J
      REAL :: A(DIM1,DIM2)

      DO 1 I=1,DIM1
      DO 1 J=1,DIM2
       A(I,J)=0.
   1  CONTINUE

      RETURN
      END SUBROUTINE ZERO
!----------------------------------------------------------------
      SUBROUTINE SETPARAM(Q1,Q2,D,F1,F2)
      INTEGER :: K
      REAL Q1,Q2,D,F1,F2

         Q1=20.
         Q2=10.
        D=5.
        F1=0.001
        F2=0.007

      RETURN
      END SUBROUTINE SETPARAM

!----------------------------------------------------------------
!----------------------------------------------------------------
!----------------------------------------------------------------

END MODULE kalman
