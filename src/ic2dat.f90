FUNCTION ic2dat(kd)

  ! converts julian daynumber since 19000101 into gregorian yyyymmdd


  INTEGER, INTENT(IN) :: kd
  INTEGER :: ic2dat
  INTEGER :: iy, im, id, l, n

  l  = kd + 68569 + 2415020
  n  = 4*l / 146097
  l  = l - ( 146097*n + 3 ) / 4
  iy = 4000 * ( l+1 ) / 1461001
  l  = l - 1461 * iy / 4 + 31
  im = 80 * l / 2447
  id = l - 2447 * im / 80
  l  = im / 11
  im = im + 2 - 12 * l
  iy = 100 * ( n- 49 ) + iy + l
  ic2dat=10000*iy+im*100+id
END FUNCTION ic2dat

