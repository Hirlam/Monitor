SUBROUTINE read_radiation
 
 USE DATA
 USE CONSTANTS
 USE FUNCTIONS

 IMPLICIT NONE

 INTEGER :: date,time,		&
            i,k,ierr,		&
            cdate,ctime,        &
            wdate,wtime,tinc,   &
            ind
 
 INTEGER :: tt,rh,dd,ff,gh,dh,iln,ss,lw,mark,m(9)
 INTEGER :: test_mark

 CHARACTER(LEN=70) :: fname='/data/proj/Radiation/bfr/XXXX/20710001.bfr'
 CHARACTER(LEN=70) :: wname=' '
 CHARACTER(LEN=70) :: ename='/data/proj/Radiation/bfr/XXXX/20710001.BFR'

!------------------------------------------

 !
 ! If obs array is not allocated
 ! do so and init arrays
 !

 IF (.NOT.obs(1)%obs_is_allocated) THEN

    ! Estimate maxtim if not given by user
    IF (maxtim.EQ.0) maxtim=get_maxtim(sdate,edate,1)
    IF(lprint_read) WRITE(6,*)'MAXTIM', maxtim

    ! Init obs array
    DO k = 1,maxstn
       ALLOCATE(obs(k)%o(maxtim))
    ENDDO
  
    obs%ntim       = 0
    obs%nexp       = nexp
    obs%nparver    = nparver
    obs%nfclengths = nfclengths
    obs%stnr       = stnlist

    obs%obs_is_allocated = .TRUE.

 ENDIF

 ! Copy time

 cdate = sdate
 ctime = stime

 ind=index(fname,'XXXX',.true.)

 STAT_LOOP : DO i=1,maxstn

    cdate  = sdate

    TIME_LOOP : DO 

      wname = fname
      WRITE(wname(ind:ind+12),'(I4.4,A1,2I4.4)') &
      stnlist(i),'/',stnlist(i),mod(cdate/100,10000)


      OPEN(lunin,file=wname,status='old',iostat=ierr)

      IF(ierr.NE.0) THEN
         WRITE(wname(ind+14:ind+16),'(A3)')'BFR'
         OPEN(lunin,file=wname,status='old',iostat=ierr)
         IF(ierr.NE.0) THEN
            WRITE(6,*)'Could not open',wname
            wdate=100*(cdate/100)+15
            CALL adddtg(wdate,0,24*30*3600,cdate,wtime)
            IF (cdate/100.GT.edate/100) EXIT TIME_LOOP
            CYCLE TIME_LOOP
         ENDIF
      ENDIF

      WRITE(6,*)'Open:',trim(wname)

      READ_LOOP : DO

         READ(lunin,*,iostat=ierr)date,time,tt,rh,dd,ff,gh,dh,iln,ss,lw,mark

         IF(ierr.NE.0    ) EXIT READ_LOOP

         date = date + 20000000
         IF(date.GT.edate) EXIT READ_LOOP

         IF(test_mark(mark).GT.0) THEN
            IF (lprint_read) WRITE(6,*)'SKIP',stnlist(i),date,time,mark,test_mark(mark)
            CYCLE READ_LOOP
         ENDIF

         
         ! Midnight converted to 00
         IF (time.EQ.24) THEN
            tinc = 23
            time = 00
         ELSE
            ! Change to UTC
            tinc = -1
         ENDIF

         CALL adddtg(date,time*10000,tinc*3600,wdate,wtime)
         IF (lprint_read) WRITE(6,*)'DATE',date,time*10000,tinc,wdate,wtime

         k = obs(i)%ntim + 1
         IF (lprint_read) WRITE(6,*)'K',k

         ALLOCATE(obs(i)%o(k)%date)
         ALLOCATE(obs(i)%o(k)%time)
         ALLOCATE(obs(i)%o(k)%val(nparver))

         obs(i)%ntim      = k
         obs(i)%o(k)%date = wdate
         obs(i)%o(k)%time = wtime/10000
         obs(i)%o(k)%val  = err_ind
            
         IF ( tt_ind /= 0 .AND. (ABS(tt + 999) /= 0) ) obs(i)%o(k)%val(tt_ind) = float(tt)*0.1
         IF ( ff_ind /= 0 .AND. (ABS(ff + 99) /= 0) ) obs(i)%o(k)%val(ff_ind) = float(ff)*0.1
         IF ( dd_ind /= 0 .AND. (ABS(dd + 99) /= 0) ) obs(i)%o(k)%val(dd_ind) = float(dd)
         IF ( gr_ind /= 0 .AND. (ABS(gh + 99) /= 0) ) obs(i)%o(k)%val(gr_ind) = float(gh)*0.1
         IF ( lw_ind /= 0 .AND. (ABS(lw + 99) /= 0) ) obs(i)%o(k)%val(lw_ind) = float(lw)*0.1

      ENDDO READ_LOOP

      CLOSE(lunin)

      wdate=100*(cdate/100)+15
      CALL adddtg(wdate,0,24*30*3600,cdate,wtime)
      IF (cdate/100.GT.edate/100) EXIT TIME_LOOP

    ENDDO TIME_LOOP

 ENDDO STAT_LOOP

 DO i=1,maxstn
    obs(i)%active = ( obs(i)%ntim > 0 )
 ENDDO

 RETURN

END SUBROUTINE read_radiation
!
!
!
!
INTEGER FUNCTION test_mark(mark)

 IMPLICIT NONE

 INTEGER :: mark,m(9),i,flag

 m(9) = mod(mark,10)
 m(8) = mod(mark,100)/10
 m(7) = mod(mark,1000)/100
 m(6) = mod(mark,10000)/1000
 m(5) = mod(mark,100000)/10000
 m(4) = mod(mark,1000000)/100000
 m(3) = mod(mark,10000000)/1000000
 m(2) = mod(mark,100000000)/10000000
 m(1) = mark/100000000

 flag = 0
 DO i=5,8
  SELECT CASE (m(i))
  CASE(2,3,4,5,6,7,8,9)
    flag = flag + 1
  END SELECT
 ENDDO

 test_mark = flag

RETURN
END
