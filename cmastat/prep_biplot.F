      subroutine prep_biplot(outstr,length,levels,biasfg,biasan,
     x                       biascorr,count,iunit)
      implicit none
      character*(*) outstr
      integer length,levels
      real biascorr(levels)
      real biasfg(levels)
      real biasan(levels)
      integer count(levels)
      integer i,iunit


      do i=1,levels
      write(iunit,111) i,count(i),
     $                 biasfg(i),biasan(i),biascorr(i)
  
      enddo
 111  format(i4,i8,3f14.8)
      end

