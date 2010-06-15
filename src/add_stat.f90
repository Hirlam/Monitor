SUBROUTINE add_stat(s,o,d)

 USE types

 IMPLICIT NONE

 ! Input

 TYPE(statistics), INTENT(INOUT) :: s
 REAL,             INTENT(IN   ) :: o
 REAL,             INTENT(IN   ) :: d

 ! Local
 REAL :: p
 ! -------------------------------------
 
 s%obs  = s%obs  + o
 s%rmse = s%rmse + d**2
 s%bias = s%bias + d
 s%mabe = s%mabe + ABS(d)

 ! s2,obs2 needed for standard deviation calculation :
 ! p is the forecast value computed as 'bias' + obs:
 ! s3,obs3 needed for skewness calculation :

 p = d + o

 s%s2   = s%s2   + p**2
 s%obs2 = s%obs2 + o**2 
 s%s3   = s%s3   + p**3
 s%obs3 = s%obs3 + o**3
 s%mabe = s%mabe + ABS(d)
 s%n    = s%n    + 1

 RETURN
END SUBROUTINE add_stat
