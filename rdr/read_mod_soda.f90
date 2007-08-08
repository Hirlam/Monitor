SUBROUTINE read_mod_soda
 
 USE DATA
 USE CONSTANTS
 USE FUNCTIONS

 IMPLICIT NONE

 INTEGER :: date,time,i,j,k,kk,l,o,ierr, &
            cdate,ctime,              &
            wdate,wtime,ll,stat_i

 
 REAL :: val(7),val2(7)

 LOGICAL :: do_this_time

 CHARACTER(LEN=80) :: mname ='soda.dat'
 CHARACTER(LEN=80) :: wname =' '

 !------------------------------------------

 CALL allocate_mod

 stat_i = 1
 hir(stat_i)%stnr = 1
 hir(stat_i)%active = .TRUE.

 EXP_LOOP : DO o=1,nexp

    wname = TRIM(modpath(1))//TRIM(expname(o))//'_'//TRIM(mname)
    OPEN(lunin,file=wname,status='old',iostat=ierr)
    IF ( ierr /= 0 ) THEN
       WRITE(6,*)'Could not open ',TRIM(wname)
       CALL abort
    ENDIF

    WRITE(6,*)'Open ',TRIM(wname)

    do_this_time = .FALSE.

    READ_LOOP : DO

       READ(lunin,*,iostat=ierr)date,ll,val


       IF ( ierr /= 0 ) EXIT READ_LOOP

       IF ( date/100 < sdate ) CYCLE READ_LOOP
       IF ( date/100 > edate ) EXIT  READ_LOOP

       IF ( o == 1 ) THEN
          k = hir(stat_i)%ntim

          IF ( hir(stat_i)%ntim == 0 ) THEN
             do_this_time = .TRUE.
          ELSE
             do_this_time = ( hir(stat_i)%o(k)%date /= date/100 )
          ENDIF
       ELSE
          do_this_time = .FALSE.
          k = 0
          DO kk=1,hir(stat_i)%ntim
             IF ( hir(stat_i)%o(kk)%date == date/100 ) THEN
                k = kk
                EXIT
             ENDIF
          ENDDO
         
          IF ( k == 0 ) CYCLE READ_LOOP

       ENDIF
   
       IF (do_this_time) THEN
   
          k = hir(stat_i)%ntim + 1

          WRITE(6,*)'ALLOCATE',k,date
      
          ALLOCATE(hir(stat_i)%o(k)%date)
          ALLOCATE(hir(stat_i)%o(k)%time)
          ALLOCATE(hir(stat_i)%o(k)%nal(nexp,nfclengths,nparver))
   
          hir(stat_i)%ntim      = k
          hir(stat_i)%o(k)%date = date/100
          hir(stat_i)%o(k)%time = mod(date,100)
          hir(stat_i)%o(k)%nal  = err_ind

       ENDIF
   
       SELECT CASE(TRIM(expname(o)))
       CASE('ar025_31t0')
          IF ( wt_ind /= 0 ) hir(stat_i)%o(k)%nal(o,ll,wt_ind) = val(5)
          IF ( wq_ind /= 0 ) hir(stat_i)%o(k)%nal(o,ll,wq_ind) = val(7)
       CASE('al00_31t0')
          IF ( ll == 01 ) val2 = 0.
          IF ( wt_ind /= 0 ) hir(stat_i)%o(k)%nal(o,ll,wt_ind) = - (val(3) - val2(3)) / 3600.
          IF ( wq_ind /= 0 ) hir(stat_i)%o(k)%nal(o,ll,wq_ind) = - (val(4) - val2(4)) / 3600.
          val2 = val
       CASE DEFAULT
          CALL ABORT
       END SELECT

    ENDDO READ_LOOP
   
    CLOSE(lunin)
   
 ENDDO EXP_LOOP

 RETURN

END SUBROUTINE read_mod_soda
