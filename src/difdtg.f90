INTEGER FUNCTION difdtg(kd1,kt1,kd2,kt2)

  !     RETURNS 'DISTANCE' IN seconds
  !     FROM START date/time (yyyymmdd, hhmmss, resp.) to final

  ih1=kt1/10000
  ir =kt1-ih1*10000
  im1=ir/100
  is1=ir-im1*100
  ih2=kt2/10000
  ir =kt2-ih2*10000
  im2=ir/100
  is2=ir-im2*100
  difdtg=(idat2c(kd2)-idat2c(kd1))*3600*24 +  &
       (ih2-ih1)*3600 + (im2-im1)*60 + (is2-is1)
END FUNCTION difdtg
