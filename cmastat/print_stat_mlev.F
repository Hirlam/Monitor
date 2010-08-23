      subroutine print_stat_mlev(iun,outstr,length,vertcor,vlen,levels,
     $     vert,rmsfg,biasfg,absbiasfg,rmsan,biasan,absbiasan,count)
      implicit none
      integer iun
      character*(*) outstr,vertcor
      integer length,vlen,levels
      real vert(levels)
      real rmsfg(levels)
      real biasfg(levels)
      real absbiasfg(levels)
      real rmsan(levels)
      real biasan(levels)
      real absbiasan(levels)
      integer count(levels)
      integer i

      write(*,'(A)')'======================================'
      write(*,'(A)') trim(outstr)
      write(*,'(A22,i10)') 'First guess statistics',levels
      write(*,'(A17,5A12)') vertcor(1:vlen),
     $     'noofobs','rms','bias','abs(bias)','level'

      write(iun,'(A)')'======================================'
      write(iun,'(A)') trim(outstr)
      write(iun,'(A22,i10)') 'First guess statistics',levels
      write(iun,'(A17,5A12)') vertcor(1:vlen),
     $     'noofobs','rms','bias','abs(bias)','level'

      do i=1,levels
         if (i.eq.1) then
            write(*,'(A7,A3,F7.0,I12,3F12.6,i10)')
     $           '   .   ',' > ',vert(i),
     $           count(i),rmsfg(i),biasfg(i),
     $           absbiasfg(i),i
            write(iun,'(A7,A3,F7.0,I12,3F12.6,i10)')
     $           '   .   ',' > ',vert(i),
     $           count(i),rmsfg(i),biasfg(i),
     $           absbiasfg(i),i

         else
            write(*,'(F7.0,A3,F7.0,I12,3F12.6,i10)')
     $           vert(i-1),' - ',vert(i),
     $           count(i),rmsfg(i),biasfg(i),
     $           absbiasfg(i),i
            write(iun,'(F7.0,A3,F7.0,I12,3F12.6,i10)')
     $           vert(i-1),' - ',vert(i),
     $           count(i),rmsfg(i),biasfg(i),
     $           absbiasfg(i),i
         endif
      enddo

      write(*,'(A19,i13)') 'Analysis statistics',levels
      write(*,'(A17,5A12)') vertcor,
     $     'noofobs','rms','bias','abs(bias)','level'
      write(iun,'(A19,i13)') 'Analysis statistics',levels
      write(iun,'(A17,5A12)') vertcor,
     $     'noofobs','rms','bias','abs(bias)','level'

      do i=1,levels
         if (i.eq.1) then
            write(*,'(A7,A3,F7.0,I12,3F12.6,i10)')'   .   ',' > ',
     $           vert(i),
     $           count(i),rmsan(i),biasan(i),
     $           absbiasan(i),i
            write(iun,'(A7,A3,F7.0,I12,3F12.6,i10)')'   .   ',' > ',
     $           vert(i),
     $           count(i),rmsan(i),biasan(i),
     $           absbiasan(i),i
         else
            write(*,'(F7.0,A3,F7.0,I12,3F12.6,i10)')
     $           vert(i-1),' - ',vert(i),
     $           count(i),rmsan(i),biasan(i),
     $           absbiasan(i),i
            write(iun,'(F7.0,A3,F7.0,I12,3F12.6,i10)')
     $           vert(i-1),' - ',vert(i),
     $           count(i),rmsan(i),biasan(i),
     $           absbiasan(i),i
         endif
      enddo
      end

