SUBROUTINE hourdiff2(iy1,im1,id1,ih1,iy2,im2,id2,ih2,dhour,ierr)

  INTEGER :: iy1,im1,id1,ih1,iy2,im2,id2,ih2,dhour,ierr

  INTEGER :: difdtg

  INTEGER :: iym1,iymd2

  iym1 = (100*iy1 + im1)*100 + id1
  iym2 = (100*iy2 + im2)*100 + id2

  dhour = difdtg(iym2, 10000*ih2, iym1, 10000*ih1)/3600

  RETURN

END SUBROUTINE hourdiff2
