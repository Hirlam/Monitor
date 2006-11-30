SUBROUTINE read_nasudden
 
 USE constants
 USE functions
 USE data
 USE timing

 IMPLICIT NONE

 INTEGER, PARAMETER :: nlev = 6
 REAL,    PARAMETER :: variance_limit = 1.e-1

 INTEGER :: yy,mm,dd,hh,mi,	&
            ly,lm,ld,lh,	&
            wdate,wtime,	&
            cdate,ctime,	&
            i,j,k,l,lstart,	&
            k_nr,			&
            ierr2,ierr,		&
            ii_cycle,		&
            flag = 0,		&
            nscan,month,	&
            cyear,			&
            stat_i
 
 REAL :: rk,rdum
 REAL, DIMENSION(nlev)    :: tt,ff

 LOGICAL :: adjusted_hour = .FALSE.

!------------------------------------------

 IF(print_read > 0 ) WRITE(6,*)'READ_NASUDDEN'

 CALL allocate_obs

 stat_i = 1
 obs(stat_i)%stnr   = 999
 obs(stat_i)%active = .TRUE.

 OPEN(lunin,file=obspath,status='old',iostat=ierr)
 IF(ierr.NE.0) CALL abort

 ly = 0
 lm = 0
 ld = 0
 lh = 0
  i = 0
  k = 0

 READ_LOOP : DO

    READ(lunin,*,iostat=ierr)yy,mm,dd,hh,mi,tt,ff,rdum

    IF ( ierr /= 0 ) EXIT READ_LOOP
                  
    !
    ! Adjust time to UTC 
    ! Shift 10 minutes to make accumulation eaiser
    !
   
    ! Some hh = 24 adjust to 00
    cdate = yy * 10000 + mm * 100 + dd
    ctime = hh * 10000 

    IF(hh.EQ.24) THEN 
       CALL adddtg(cdate,00,3600*24,wdate,wtime)
       cdate =  wdate
       ctime =  00
    ELSE
       ctime =  hh*10000+mi*100 
    ENDIF

    CALL adddtg(cdate,ctime,-10*60,wdate,wtime)
    yy = wdate / 10000
    mm = MOD(wdate/100,100)
    dd = MOD(wdate,100)
    hh = wtime / 10000
    mi = MOD(wtime/100,100)

    IF(lprint_read) WRITE(6,*)'FOUND',yy,mm,dd,hh,mi

    IF ( wdate < sdate ) CYCLE READ_LOOP
    IF ( wdate > edate ) EXIT  READ_LOOP

    SAME_TIME : IF(ly.EQ.yy.AND.lm.EQ.mm.AND.	&
                   ld.EQ.dd.AND.lh.EQ.hh     ) THEN


       IF(tt_ind.GT.0) obs(stat_i)%o(i)%val(1:6) =          &
                       obs(stat_i)%o(i)%val(1:6) + tt
       IF(ff_ind.GT.0) obs(stat_i)%o(i)%val(1:6) =          &
                       obs(stat_i)%o(i)%val(1:6) + ff

       k = k + 1

    ELSE


       IF ( k > 0 ) THEN
          IF ( k == 6 ) THEN
             rk = 1./FLOAT(k)
             IF(tt_ind.GT.0) obs(stat_i)%o(i)%val(1:6) =          &
                             obs(stat_i)%o(i)%val(1:6) * rk
             IF(ff_ind.GT.0) obs(stat_i)%o(i)%val(1:6) =          &
                             obs(stat_i)%o(i)%val(1:6) * rk
          ELSE
             WRITE(6,*)' Reject erroneous in obs data',      &
                        obs(stat_i)%o(i)%date,    &
                        obs(stat_i)%o(i)%time, k
             IF(tt_ind.GT.0) obs(stat_i)%o(i)%val(1:6) = err_ind
             IF(ff_ind.GT.0) obs(stat_i)%o(i)%val(1:6) = err_ind
          ENDIF
       ENDIF

       i = obs(stat_i)%ntim + 1 
       
       ALLOCATE(obs(stat_i)%o(i)%date)
       ALLOCATE(obs(stat_i)%o(i)%time)
       ALLOCATE(obs(stat_i)%o(i)%val(nparver))

       obs(stat_i)%ntim      = i
       obs(stat_i)%o(i)%date = wdate
       obs(stat_i)%o(i)%time = wtime/10000
       obs(stat_i)%o(i)%val  = err_ind

       k  = 0
       ly = yy
       lm = mm
       ld = dd
       lh = hh

       IF(tt_ind.GT.0) obs(stat_i)%o(i)%val(1:6) = tt
       IF(ff_ind.GT.0) obs(stat_i)%o(i)%val(1:6) = ff

       k = k + 1


    ENDIF SAME_TIME 

 ENDDO READ_LOOP

 CLOSE(lunin)
 DEALLOCATE(obs(stat_i)%o(obs(stat_i)%ntim)%date)
 DEALLOCATE(obs(stat_i)%o(obs(stat_i)%ntim)%time)
 DEALLOCATE(obs(stat_i)%o(obs(stat_i)%ntim)%val)

 obs(stat_i)%ntim   = obs(stat_i)%ntim - 1
 obs(stat_i)%active = ( obs(stat_i)%ntim > 0 )

 RETURN

END SUBROUTINE read_nasudden
