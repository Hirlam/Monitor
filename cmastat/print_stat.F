      subroutine print_stat(iun,outstr,length,levels,
     $     rmsfg,biasfg,absbiasfg,rmsan,biasan,absbiasan,count)
      implicit none
      integer iun
      character*(*) outstr
      integer length,levels
      real rmsfg(levels)
      real biasfg(levels)
      real absbiasfg(levels)
      real rmsan(levels)
      real biasan(levels)
      real absbiasan(levels)
      integer count(levels)
      integer i

      write(*,'(A)')'======================================'
      write(*,'(A)') outstr(1:length)
      write(*,'(A22,I10)') 'First guess statistics',levels
      write(*,'(5A15)') 'noofobs','rms','bias','abs(bias)','level'

      write(iun,'(A)')'======================================'
      write(iun,'(A)') outstr(1:length)
      write(iun,'(A22,I10)') 'First guess statistics',levels
      write(iun,'(5A15)') 'noofobs','rms','bias','abs(bias)','level'

      do i=1,levels
         write(*,'(I15,3F15.4,i10)')count(i),rmsfg(i),biasfg(i),
     $        absbiasfg(i),i
         write(iun,'(I15,3F15.4,i10)')count(i),rmsfg(i),biasfg(i),
     $        absbiasfg(i),i
      enddo

      write(*,'(A19,i13)') 'Analysis statistics',levels
      write(*,'(5A15)') 'noofobs','rms','bias','abs(bias)','level'
      write(iun,'(A19,i13)') 'Analysis statistics',levels
      write(iun,'(5A15)') 'noofobs','rms','bias','abs(bias)','level'

      do i=1,levels
         write(*,'(I15,3F15.4,i10)')count(i),rmsan(i),biasan(i),
     $        absbiasan(i),i
         write(iun,'(I15,3F15.4,i10)')count(i),rmsan(i),biasan(i),
     $        absbiasan(i),i
      enddo
      end
