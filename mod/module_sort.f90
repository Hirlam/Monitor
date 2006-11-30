MODULE sort
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
 CONTAINS
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
SUBROUTINE BSORT(N,A,O)

! Bubble sort 
! Return index of sorted array
! A : real array to be sorted
! O : sorted index pointer
!
!--------------------------------------------------------------------- 
      IMPLICIT NONE

! Input/Output

      INTEGER, INTENT(IN ) :: N
      REAL   , INTENT(IN ) :: A(N)

      INTEGER, INTENT(OUT) :: O(N)

! Local
      INTEGER :: IA,IB,OW
      REAL    :: AA,BB

! --- SORT STATISTICS

      DO IA=1,N-1
      DO IB=IA+1,N

        AA = A(O(IA))
        BB = A(O(IB))

        IF ( AA < BB ) THEN

          OW    = O(IA)
          O(IA) = O(IB)
          O(IB) = OW
          
        END IF

      END DO
      END DO

      RETURN

END SUBROUTINE BSORT
!--------------------------------------------------------------------- 
!--------------------------------------------------------------------- 
!--------------------------------------------------------------------- 
SUBROUTINE SUB_BSORT(N,A,B,O,LIM)
! Bubble sort 
! Return index of sorted array
! A : real array to be sorted
! B : real array to be sorted by
! O : sorted index pointer
! LIM : Limit when finding similar values in B
!

!--------------------------------------------------------------------- 
      IMPLICIT NONE

! Input

      INTEGER, INTENT(IN ) :: N
      REAL   , INTENT(IN ) :: A(N),B(N),LIM
      INTEGER, INTENT(OUT) :: O(N)

! Local

      INTEGER :: IA,IB,OW,I1,I2,I
      REAL    :: AA,BB

      ! --- SORT STATISTICS

      I1 = 1
      I2 = 1

      DO I=2,N

         AA = B(O(I  ))
         BB = B(O(I-1))

         IF (ABS( AA - BB ) < LIM ) THEN
            I2 = I
         ELSE
            I1 = I
         ENDIF

         ! We have some similar values in B let's sort A

         IF ( I2 - I1 > 0 )  THEN

            DO IA = I1   ,I2-1
            DO IB = IA+1,I2

              AA = A(O(IA))
              BB = A(O(IB))

              IF ( AA < BB ) THEN
                OW    = O(IA)
                O(IA) = O(IB)
                O(IB) = OW
              END IF

            END DO
            END DO

         ENDIF

      ENDDO

      RETURN
END SUBROUTINE SUB_BSORT
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
END MODULE sort
