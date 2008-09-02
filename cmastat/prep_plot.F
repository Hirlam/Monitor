      subroutine prep_plot(outstr,length,levels,rmsfg,biasfg,
     $                     rmsan,biasan,count,iunit)
      implicit none
      character*(*) outstr
      integer length,levels
      real rmsfg(levels)
      real biasfg(levels)
      real rmsan(levels)
      real biasan(levels)
      integer count(levels)
      integer i,iunit

      write(iunit,'(A)') outstr(1:length)

      do i=1,levels
      write(iunit,111) i,count(i),count(i),
     $                 biasfg(i),biasan(i)
  
      enddo

      do i=1,levels
      write(iunit,111) i,count(i),count(i),
     $                 rmsfg(i),rmsan(i)
      enddo

 111  format(i4,2i8,2f14.8)
      end

