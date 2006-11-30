SUBROUTINE read_period(lls,lle,f,pl)

 IMPLICIT NONE

 ! In
 INTEGER :: lls,lle,pl

 ! Out
 ! f(:,1) - Period length
 ! f(:,2) - 1: Read, 0: Skip
 INTEGER :: f(3,2)

 ! Local
 INTEGER :: fc_interval,fc_period,fc_add

 !-----------------------------------------

 fc_interval=lle-lls
 fc_period = MAX(1,fc_interval/12) * pl

 SELECT CASE(pl)

 CASE(42,44) 

    IF(pl.EQ.44) THEN
     fc_add = 1
    ELSE
     fc_add = 0
    ENDIF
   
    IF(lls .eq. 0) THEN
   
       f(1,1) = fc_interval
       f(1,2) = 1
       f(2,1) = fc_period - 2*fc_interval -2*fc_add
       f(2,2) = 0
   
       IF(fc_interval.eq.6) THEN
          f(3,2) = 1
          f(3,1) = fc_interval 
       ELSE
          f(3,2) = 0
          f(3,1) = fc_interval + fc_add
       ENDIF
   
    ELSE
   
       f(1,1) = lls 
       f(1,2) = 0
       f(2,1) = fc_interval 
       f(2,2) = 1
       f(3,1) = fc_period - lls -fc_interval - fc_add
       f(3,2) = 0
   
    ENDIF

 CASE(12,36)

    IF(lls .eq. 0) THEN
      f(1,1) = fc_interval
      f(1,2) = 1
      f(2,1) = 0
      f(2,2) = 0
      f(3,1) = fc_period - fc_interval
      f(3,2) = 0
    ELSE
       f(1,1) = lls
       f(1,2) = 0
       f(2,1) = fc_interval 
       f(2,2) = 1
       f(3,1) = fc_period - lls - fc_interval
       f(3,2) = 0
    ENDIF

 END SELECT

 RETURN

END
