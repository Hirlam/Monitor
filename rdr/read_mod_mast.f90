SUBROUTINE read_mod_mast
 
 USE MAST_DATA
 USE DATA
 USE CONSTANTS
 USE FUNCTIONS

 IMPLICIT NONE

 INTEGER :: ii,k,kk,o,ierr,           &
            cdate,ctime,              &
            wdate,wtime,ll,stat_i,    &
            fc_ind,                   &
            stations(100),            &
            m,n,                      &
            max_found_stat,istnr
         
 
 REAL :: val(11),dz_model

 LOGICAL :: do_this_time,use_stnlist

 CHARACTER(LEN=12) :: ctstamp
 CHARACTER(LEN=200):: wname =' '
 CHARACTER(LEN=8)  :: ccdate =''
 CHARACTER(LEN=2)  :: cchour =''
 CHARACTER(LEN= 10) :: invar(11) = '#'

 !------------------------------------------
 invar( 1) = 'TMAST'
 invar( 2) = 'UNDEFINED'
 invar( 3) = 'TZ'
 invar( 4) = 'RHMAST'
 invar( 5) = 'FFMAST'
 invar( 6) = 'GR'
 invar( 7) = 'LU'
 invar( 8) = 'WT'
 invar( 9) = 'WQ'
 invar(10) = 'UNDEFINED'
 invar(11) = 'UW'

 stations = 0
 use_stnlist = ( MAXVAL(stnlist) > 0 )

 CALL allocate_mod

 STATION_LOOP : DO istnr=1,max_flux_station

    ! Check if this station should be used

    stat_i = 0
    IF ( use_stnlist ) THEN
       DO ii=1,maxstn
          IF (istnr == stnlist(ii) ) stat_i = ii
       ENDDO
       IF ( stat_i == 0 ) CYCLE STATION_LOOP
    ENDIF

    IF (stat_i == 0 ) THEN
       max_found_stat  = max_found_stat + 1
       stnlist(max_found_stat) = istnr
    ELSE
       max_found_stat  = stat_i
    ENDIF

    stations(istnr) = max_found_stat
    hir(max_found_stat)%active = .TRUE.
    hir(max_found_stat)%stnr   = istnr

    IF (max_found_stat > maxstn) THEN
       WRITE(6,*)'Increase maxstn',max_found_stat
       CALL abort
    ENDIF

    stat_i = stations(istnr)

    cdate=sdate
    ctime=stime

 TIME_LOOP : DO 

    IF ( cdate < sdate ) THEN
       wdate = cdate
       wtime = ctime*10000
       CALL adddtg(wdate,wtime,3600*fcint,cdate,ctime)
       ctime = ctime/10000
       CYCLE TIME_LOOP
    ENDIF

    IF ( cdate > edate ) EXIT  TIME_LOOP

    WRITE(ccdate,'(I8.8)')cdate
    WRITE(cchour,'(I2.2)')ctime

    EXP_LOOP : DO o=1,nexp

       wname = TRIM(modpath(o))//'Fcst_' //   &
               TRIM(expname(o))//'_'//        &
               TRIM(stname(istnr))//'_'//    &
               TRIM(ccdate)//'_'//TRIM(cchour)//'.txt'

       OPEN(lunin,file=wname,status='old',iostat=ierr)
       IF ( ierr /= 0 ) THEN
          WRITE(6,'(2A)')'Could not open ',TRIM(wname)
          CYCLE EXP_LOOP
       ENDIF

       WRITE(6,'(2A)')'Open ',TRIM(wname)

       CALL find_dz_model(expname(o),stname(istnr),dz_model)

       do_this_time = .FALSE.

       ll = 0
       READ_LOOP : DO
   
          READ(lunin,*,iostat=ierr)ctstamp,val

          IF ( ierr /= 0 ) EXIT READ_LOOP

          ll = ll + 1

          fc_ind = 0
          DO kk=1,nfclengths
             IF ( ll == fclen(kk) )  fc_ind = kk
          ENDDO

          IF ( fc_ind == 0 .AND. print_read > 1 ) WRITE(6,*)'Could not match fclen',ll
          IF ( fc_ind == 0 ) CYCLE READ_LOOP

          IF ( o == 1 ) THEN
             k = hir(stat_i)%ntim

             IF ( hir(stat_i)%ntim == 0 ) THEN
                do_this_time = .TRUE.
             ELSE
                do_this_time = ( hir(stat_i)%o(k)%date /= cdate .OR. &
                                 hir(stat_i)%o(k)%time /= ctime )
             ENDIF

          ELSE
             do_this_time = .FALSE.
             k = 0
             DO kk=1,hir(stat_i)%ntim
                IF ( hir(stat_i)%o(kk)%date == cdate .AND. &
                     hir(stat_i)%o(kk)%time == ctime ) THEN
                   k = kk
                   EXIT
                ENDIF
             ENDDO
         
             IF ( k == 0 ) CYCLE READ_LOOP
          ENDIF
   
          IF (do_this_time) THEN
   
             k = hir(stat_i)%ntim + 1
   
             ALLOCATE(hir(stat_i)%o(k)%date)
             ALLOCATE(hir(stat_i)%o(k)%time)
             ALLOCATE(hir(stat_i)%o(k)%nal(nexp,nfclengths,nparver))
   
             hir(stat_i)%ntim      = k
             hir(stat_i)%o(k)%date = cdate
             hir(stat_i)%o(k)%time = ctime
             hir(stat_i)%o(k)%nal  = err_ind

          ENDIF

          PARVER_LOOP : DO m=1,nparver
            INVAR_LOOP : DO n=1,SIZE(invar)
              IF ( varprop(m)%id == invar(n) ) THEN

                ! Check for missing data flag
                IF ( .NOT. qca(val(n),err_ind) ) CYCLE PARVER_LOOP

                ! Special treatment of some variabels
                SELECT CASE(invar(n))
                  CASE('TZ')
                    val(n) = val(n) / dz(istnr)
                END SELECT

                ! Check for missing data / gross error
                 IF ( qclr(val(n),varprop(m)%llim) .AND. &
                      qcur(val(n),varprop(m)%ulim) )     &
                hir(stat_i)%o(k)%nal(o,fc_ind,m) = val(n)

              ENDIF
            ENDDO INVAR_LOOP

          ENDDO PARVER_LOOP

       ENDDO READ_LOOP
   
       CLOSE(lunin)
   
    ENDDO EXP_LOOP

    wdate = cdate
    wtime = ctime*10000
    CALL adddtg(wdate,wtime,3600*fcint,cdate,ctime)
    ctime = ctime/10000

 ENDDO TIME_LOOP
 ENDDO STATION_LOOP

 RETURN

END SUBROUTINE read_mod_mast
