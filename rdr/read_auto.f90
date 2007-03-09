SUBROUTINE read_auto

 USE data
 USE functions
 USE constants

 IMPLICIT NONE


! nr     - description (chanel)
!  ------------------------------------
!  1      - stationsnummer
!  2      - lat
!  3      - lon
!  4      - stationshöjd
!  5      - nederbörd 1 timme (914)
!  6      - temperatur (33)
!  7      - daggpunkt (903)
!  8      - vindriktning (30)
!  9      - vindhastghet (31)
!  10     - total molnmängd (468)
!  11     - molnbas (442)
!  12     - sikt (340)
!  13     - ackumulerad nederbörd PW (920)
!  14     - ackumulerad snö PW (921)
!  15     - nederbördstyp (926)
!  16     - max nederbördstyp senaste timme MAX(926,927,928,929)
!  17     - ackumulerad nederbörd (Genonor) (5105)
!  18     - byvind (322)
!  19     - std av vindhastighet (905)
!  20     - x/8 på höjd 1 (460)
!  21     - höjd 1 (461)
!  22     - x/8 på höjd 2 (462)
!  23     - höjd 2 (463)
!  24     - x/8 på höjd 3 (464)
!  25     - höjd 3 (465)
!  26     - x/8 på höjd 4 (466)
!  27     - höjd 4 (467)
!  28     - barometerhöjd (953)
!  29     - nederbörd 3 timmar, summa av 914
!  30     - nederbörd 6 eller 12 timmar (915)

 INTEGER :: i,ii,j,k,l,			&
            ierr = 0,			&
            cdate = 999999,		&
            ctime = 999999,		&
            wdate = 999999,		&
            wtime = 999999,		&
            istnr = 0,			&
            stat_i,			&
            stations(100000),      	&
            max_found_stat
            
 
 
 REAL :: lat,lon,height,val(26)

 CHARACTER(LEN=100) :: fname =' '
 CHARACTER(LEN= 08) :: ndate =' '

 LOGICAL :: use_stnlist

!----------------------------------------------------------

 ! Init

 stations       = 0
 max_found_stat = 0
 use_stnlist    = ( MAXVAL(stnlist) > 0 )

 !
 ! Allocate observation array
 !

 CALL allocate_obs


 ! Copy time

 cdate = sdate
 ctime = stime
 wdate = cdate
 wtime = ctime
 
 !
 ! Loop over all times
 !


 TIME_LOOP : DO

    WRITE(ndate(1:8),'(I6.6,I2.2)')MOD(cdate,1000000),ctime/10000
    fname = TRIM(obspath)//'auto_01_'//ndate//'00'

    !
    ! Read obs data
    !

       OPEN(lunin,file=fname,status='old',iostat=ierr)

       IF (ierr /= 0) THEN
  
          IF( print_read > 1 )WRITE(6,*)'Could not open:',TRIM(fname)

          wdate = cdate
          wtime = ctime
          CALL adddtg(wdate,wtime,3600*obint,cdate,ctime)
          IF(cdate > edate_obs) EXIT TIME_LOOP
          CYCLE TIME_LOOP

       ENDIF

       IF (print_read > 0 ) WRITE(6,*)'READ ',TRIM(fname)

       READ_STATION_OBS : DO 

          READ(lunin,*,iostat=ierr)istnr,lat,lon,height,val

          IF (print_read > 1 ) WRITE(6,*)'READ'
          IF (print_read > 1 ) WRITE(6,*)istnr,lat,lon,height,val
          IF (print_read > 1 ) WRITE(6,*)

          IF (ierr  /= 0) EXIT READ_STATION_OBS

          IF (istnr == 0) CYCLE READ_STATION_OBS

          !
          ! Find station index
          !

          IF(stations(istnr) == 0) THEN
           
             stat_i = 0
             IF ( use_stnlist ) THEN
                DO ii=1,maxstn
                   IF (istnr == stnlist(ii) ) stat_i = ii
                ENDDO
                IF ( stat_i == 0 ) CYCLE READ_STATION_OBS
             ENDIF

             IF (stat_i == 0 ) THEN 
                max_found_stat  = max_found_stat + 1
                stnlist(max_found_stat)= istnr
             ELSE
                max_found_stat  = stat_i
             ENDIF

             stations(istnr) = max_found_stat 
             obs(max_found_stat)%active = .TRUE.
             obs(max_found_stat)%stnr   = istnr
             obs(max_found_stat)%lat    = lat
             obs(max_found_stat)%lon    = lon
             obs(max_found_stat)%hgt    = height

             IF (max_found_stat > maxstn) THEN
                WRITE(6,*)'Increase maxstn',max_found_stat
                CALL abort
             ENDIF

          ENDIF

          stat_i = stations(istnr)

          !
          ! Make Celcius
          !

          i = obs(stat_i)%ntim + 1

          ALLOCATE(obs(stat_i)%o(i)%date)
          ALLOCATE(obs(stat_i)%o(i)%time)
          ALLOCATE(obs(stat_i)%o(i)%val(nparver))

          obs(stat_i)%ntim      = i
          obs(stat_i)%o(i)%date = cdate
          obs(stat_i)%o(i)%time = ctime/10000
          obs(stat_i)%o(i)%val  = err_ind

          !
          ! Store requested data
          ! This is not correct yet ............
          !

          if (nn_ind /= 0 .AND. qca(val(1),-999.) .AND. &
                                qcl(val(1),nn_ind).AND. &
                                qcu(val(1),nn_ind)) obs(stat_i)%o(i)%val(nn_ind) = val(1)
          if (dd_ind /= 0 .AND. qca(val(1),-999.) .AND. &
                                qcl(val(2),dd_ind).AND. &
                                qcu(val(2),dd_ind)) obs(stat_i)%o(i)%val(dd_ind) = val(2)
          if (ff_ind /= 0 .AND. qca(val(1),-999.) .AND. &
                                qcl(val(3),ff_ind).AND. &
                                qcu(val(3),ff_ind)) obs(stat_i)%o(i)%val(ff_ind) = val(3)
          if (tt_ind /= 0 .AND. qca(val(1),-999.) .AND. &
                                qcl(val(4)-tzero,tt_ind).AND. &
                                qcu(val(4)-tzero,tt_ind)) obs(stat_i)%o(i)%val(tt_ind) = val(4) - tzero
          if (rh_ind /= 0 .AND. qca(val(1),-999.) .AND. &
                                qcl(val(5),rh_ind).AND. &
                                qcu(val(5),rh_ind)) obs(stat_i)%o(i)%val(rh_ind) = val(5)
          if (ps_ind /= 0 .AND. qca(val(1),-999.) .AND. &
                                qcl(val(6),ps_ind).AND. &
                                qcu(val(6),ps_ind)) obs(stat_i)%o(i)%val(ps_ind) = val(6)
          if (pe_ind /= 0 .AND. qca(val(1),-999.) .AND. &
                                qcl(val(7),pe_ind).AND. &
                                qcu(val(7),pe_ind)) obs(stat_i)%o(i)%val(pe_ind) = val(1)
          if (qq_ind /= 0 .AND. qca(val(1),-999.) .AND. &
                                qcl(val(8),qq_ind).AND. &
                                qcu(val(8),qq_ind)) obs(stat_i)%o(i)%val(qq_ind) = val(8) * 1.e3

          if (la_ind /= 0 ) obs(stat_i)%o(i)%val(la_ind) = obs(stat_i)%lat
          if (hg_ind /= 0 ) obs(stat_i)%o(i)%val(hg_ind) = obs(stat_i)%hgt

          IF (print_read > 1 ) WRITE(6,*)obs(stat_i)%o(i)%val

       ENDDO READ_STATION_OBS

       CLOSE(lunin)

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,3600*obint,cdate,ctime)
    IF(cdate > edate_obs) EXIT TIME_LOOP

 ENDDO TIME_LOOP

 IF (print_read > 0 ) WRITE(6,*) 'FOUND TIMES OBS',obs(1)%ntim

 DO i=1,maxstn
    obs(i)%active = ( obs(i)%ntim > 0 )
 ENDDO

 RETURN

END SUBROUTINE read_auto
