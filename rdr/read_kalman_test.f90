SUBROUTINE read_kalman_test

 USE data

 IMPLICIT NONE

 INTEGER :: i

 ! ------------------------------------------------------------------


 CALL allocate_mod
 CALL allocate_obs

 hir%stnr = 1
 obs%stnr = 1

 hir%active = .TRUE.
 obs%active = .TRUE.

 DO i=1,28
 ALLOCATE(hir(1)%o(i)%date,   &
          hir(1)%o(i)%time,   &
          hir(1)%o(i)%nal(1,1,1),   &
          obs(1)%o(i)%date,   &
          obs(1)%o(i)%time,   &
          obs(1)%o(i)%val(1))

          hir(1)%o(i)%date=sdate + i
          obs(1)%o(i)%date=sdate + i
          hir(1)%o(i)%time=00
          obs(1)%o(i)%time=00
 ENDDO

    hir(1)%ntim = 28
    obs(1)%ntim = 28

    hir(1)%o( 1)%nal = -1.
    hir(1)%o( 2)%nal = -3.
    hir(1)%o( 3)%nal = -2.
    hir(1)%o( 4)%nal =  2.
    hir(1)%o( 5)%nal =  1.
    hir(1)%o( 6)%nal =  0.
    hir(1)%o( 7)%nal =  1.
    hir(1)%o( 8)%nal = -1.
    hir(1)%o( 9)%nal = -3.
    hir(1)%o(10)%nal = -5.
    hir(1)%o(11)%nal = -7.
    hir(1)%o(12)%nal = -7.
    hir(1)%o(13)%nal = -8.
    hir(1)%o(14)%nal = -11.
    hir(1)%o(15)%nal = -9.
    hir(1)%o(16)%nal = -13.
    hir(1)%o(17)%nal = -13.
    hir(1)%o(18)%nal = -12.
    hir(1)%o(19)%nal = -15.
    hir(1)%o(20)%nal = -15.
    hir(1)%o(21)%nal = -12.
    hir(1)%o(22)%nal = -15.
    hir(1)%o(23)%nal = -13.
    hir(1)%o(24)%nal = -12.
    hir(1)%o(25)%nal = -14.
    hir(1)%o(26)%nal = -12.
    hir(1)%o(27)%nal = -13.
    hir(1)%o(28)%nal = -15.
    hir(1)%o(1)%nal = -1.
    hir(1)%o(1)%nal = -1.
    hir(1)%o(1)%nal = -1.
    hir(1)%o(1)%nal = -1.

    obs(1)%o( 1)%val =  1.
    obs(1)%o( 2)%val =  2.
    obs(1)%o( 3)%val =  3.
    obs(1)%o( 4)%val =  6.
    obs(1)%o( 5)%val =  7.
    obs(1)%o( 6)%val =  5.
    obs(1)%o( 7)%val =  7.
    obs(1)%o( 8)%val =  3.
    obs(1)%o( 9)%val =  0.
    obs(1)%o(10)%val = -1.
    obs(1)%o(11)%val = -2.
    obs(1)%o(12)%val = -3.
    obs(1)%o(13)%val = -5.
    obs(1)%o(14)%val = -4.
    obs(1)%o(15)%val = -6.
    obs(1)%o(16)%val = -7.
    obs(1)%o(17)%val = -15.
    obs(1)%o(18)%val = -18.
    obs(1)%o(19)%val = -22.
    obs(1)%o(20)%val = -20.
    obs(1)%o(21)%val = -21.
    obs(1)%o(22)%val = -22.
    obs(1)%o(23)%val = -21.
    obs(1)%o(24)%val = -20.
    obs(1)%o(25)%val = -22.
    obs(1)%o(26)%val = -20.
    obs(1)%o(27)%val = -21.
    obs(1)%o(28)%val = -22.

    RETURN

    END SUBROUTINE read_kalman_test
