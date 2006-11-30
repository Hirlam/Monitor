SUBROUTINE add_stat(s,o,d)

 USE types

 IMPLICIT NONE

 TYPE(statistics), INTENT(INOUT) :: s
 REAL,             INTENT(IN   ) :: o
 REAL,             INTENT(IN   ) :: d

 ! -------------------------------------
 
 s%obs  = s%obs  + o
 s%rmse = s%rmse + d**2
 s%bias = s%bias + d
 s%mabe = s%mabe + ABS(d)
 s%n    = s%n    + 1

 RETURN
END SUBROUTINE add_stat
