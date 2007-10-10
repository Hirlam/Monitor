FUNCTION idat2c(kd)

  !     for given kd(yyyymmdd) return day number since 19000101

  iy=kd/10000
  ir=kd-iy*10000
  IF(iy < 100)iy=iy+1900
  im=ir/100+1
  id=ir-im*100

  IF(im <= 3)THEN
     iy=iy-1
     im=im+12
  END IF

  ir=iy/100

  idat2c=365*iy-693923+iy/4 -ir+ir/4+INT(30.6001*im)+id

  ! the above code is more efficient than the one below, so
  ! the latter is commented out.  It is kept here for reference if
  ! ever the above code has to be extended for use with years
  ! BC: the code below is known to be correct even for years<0
  ! (but year 0 exists, and no Gregorian change in Oct 1582).
  !     DIMENSION IDAYS(12)
  !     DATA IDAYS/0,31,59,90,120,151,181,212,243,273,304,334/
  !     IY=KD/10000
  !     IR=KD-IY*10000
  !     IF(IY.GE.100)IY=IY-1900
  !     IM=IR/100
  !     ID=IR-IM*100
  !     IYDAY=IDAYS(IM)+ID
  !     if ( mod(iy,4  ).eq.0.and.
  !    +    (mod(iy,100).ne.0.or.
  !    +            mod(iy+300,400).eq.0))
  !    +then
  !        if(iy.gt.0)then
  !           if(im.gt.2)iyday=iyday+1
  !        else
  !           if(im.lt.3)iyday=iyday-1
  !        endif
  !     endif
  !     iym1=iy-1
  !     if(iy.lt.0)iym1=iy+1
  !     ily=(iym1)/4
  !     ily=ily-(iym1)/100
  !     ily=ily+(iym1+300)/400
  !     if(iy.lt.-300)ily=ily-1
  !     IDAT2C=IY*365+ILY+IYDAY
END FUNCTION idat2c
