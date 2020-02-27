SUBROUTINE read_vobs_temp

 USE data
 USE functions
 USE timing
 USE constants

 IMPLICIT NONE

 REAL, PARAMETER :: mflag = -99.

 INTEGER :: i,ii,k,kk,kkk,kk_lev,       &
            m,n,mm,m2,mmp,m2p,          &
            ierr = 0,                   &
            cdate = 999999,             &
            ctime = 999999,             &
            wdate = 999999,             &
            wtime = 999999,             &
            istnr = 0,                  &
            stat_i,                     &
            num_temp,num_stat,          &
            num_temp_lev,               &
            my_temp_lev,                &
            stations(10000000),         &
            max_found_stat,             &
            wrk(mparver),               &
            version_flag,               &
            ipr,ifi,ninvar,             &
            old_ninvar
 
 REAL :: lat,lon,hgt,sub,sca
 REAL, ALLOCATABLE :: val(:),inacc(:)

 LOGICAL :: use_stnlist,cbl

 CHARACTER(LEN=200) :: fname =' ',path
 CHARACTER(LEN= 10) :: ndate =' '
 CHARACTER(LEN= 10), ALLOCATABLE :: invar(:)

!----------------------------------------------------------

 ! Init
 ipr = -1
 ifi = -1
 stations       = 0
 max_found_stat = 0
 version_flag   = 0
 old_ninvar     = -1
 ninvar         = 0
 INQUIRE(FILE='black.list',EXIST=cbl)

 use_stnlist =( MAXVAL(stnlist) > 0 )

 CALL allocate_obs

 ! Copy time

 cdate = sdate
 ctime = stime * 10000

 wrk = 0
 WHERE ( lev_lst > 0 ) wrk = 1
 my_temp_lev = SUM(wrk)

 !
 ! Loop over all times
 !

 i = 0

 TIME_LOOP : DO

 i = i + 1

 IF (print_read>1) WRITE(6,*)'TIME:',cdate,ctime/10000
 WRITE(ndate,'(I8.8,I2.2)')cdate,ctime/10000
 path = obspath
 CALL check_path(cdate,path)
 fname = TRIM(path)//'vobs'//ndate

 !
 ! Read obs data
 !

       OPEN(lunin,file=fname,status='old',iostat=ierr)

       IF (ierr /= 0) THEN
  
          IF( print_read > 0 ) WRITE(6,'(2A)')'MISS ',TRIM(fname)

          wdate = cdate
          wtime = ctime
          CALL adddtg(wdate,wtime,obint*3600,cdate,ctime)
          IF(cdate >  edate_obs) EXIT TIME_LOOP
          IF(cdate >= edate_obs .AND. ctime/10000 > etime_obs) EXIT TIME_LOOP

          i = i - 1
          CYCLE TIME_LOOP

       ENDIF

       IF (print_read > 0 ) WRITE(6,'(2A)')'READ ',TRIM(fname)

       version_flag = 0

       READ(lunin,'(1x,3I6)',IOSTAT=ierr)num_stat,num_temp,version_flag
       IF ( ierr /= 0 ) THEN
         WRITE(6,*)'Error reading first line of vobs file',ierr
         CALL abort
       ENDIF

       IF (num_temp == 0 ) THEN
          IF(print_read>1) WRITE(6,*)'SKIP',cdate,ctime
          i = i - 1
          CLOSE(lunin)
          wdate = cdate
          wtime = ctime
          CALL adddtg(wdate,wtime,obint*3600,cdate,ctime)
          IF(cdate > edate_obs) EXIT TIME_LOOP
          CYCLE TIME_LOOP
       ENDIF

      ! Skip the synop stations
      SELECT CASE (version_flag)
       CASE(0:3)
          READ(lunin,*)num_temp_lev
          DO k=1,num_stat
            READ(lunin,*)
          ENDDO
       CASE(4)
          READ(lunin,*)ninvar
          DO k=1,ninvar
            READ(lunin,*)
          ENDDO
          DO k=1,num_stat
            READ(lunin,*)
          ENDDO
       CASE DEFAULT
          WRITE(6,*)'Cannot handle this version flag',version_flag
       END SELECT

       SELECT CASE(version_flag)
        CASE(0)
         ninvar=7
         IF ( ALLOCATED(invar) ) DEALLOCATE(invar,val,inacc)
         ALLOCATE(invar(ninvar),val(ninvar),inacc(ninvar))
         invar = (/'PR','FI','TT','RH','DD','FF','QQ'/)
         ipr = 1
         ifi = 2
        CASE(1:2)
         ninvar=8
         IF ( ALLOCATED(invar) ) DEALLOCATE(invar,val,inacc)
         ALLOCATE(invar(ninvar),val(ninvar),inacc(ninvar))
         invar = (/'PR','FI','TT','RH','DD','FF','QQ','TD'/)
         ipr = 1
         ifi = 2
        CASE(4)
         ipr = -1 
         ifi = -1 
         READ(lunin,*)num_temp_lev
         READ(lunin,*)ninvar
          IF ( ninvar /= old_ninvar ) THEN
            IF ( ALLOCATED(invar) ) DEALLOCATE(invar,val,inacc)
            ALLOCATE(invar(ninvar),val(ninvar),inacc(ninvar))
         ENDIF
         DO i=1,ninvar
           READ(lunin,*)invar(i),inacc(i)
           IF ( invar(i) == 'PR' ) ipr = i
           IF ( invar(i) == 'PP' ) ipr = i
           IF ( invar(i) == 'FI' ) ifi = i
         ENDDO
         IF ( ipr == -1 .OR. ifi == -1 ) THEN
           WRITE(6,*)'FI or PR/PP not found'
           CALL abort
         ENDIF
        CASE DEFAULT
         WRITE(6,*)'Cannot handle this vobs-file version',version_flag
         CALL abort
       END SELECT

       old_ninvar = ninvar
       
       READ_STATION_OBS : DO k=1,num_temp

          SELECT CASE(version_flag)
          CASE(0)
             READ(lunin,*,iostat=ierr)istnr,lat,lon
             hgt = mflag
          CASE(1:2,4)
             READ(lunin,*,iostat=ierr)istnr,lat,lon,hgt
          CASE DEFAULT
             WRITE(6,*)'Cannot handle this vobs-file version',version_flag
             CALL abort
          END SELECT 

          IF(print_read>1) WRITE(6,*)istnr,lat,lon,hgt,ierr

          IF (ierr /= 0) THEN
             WRITE(6,*)'Problem in ',TRIM(fname)
             WRITE(6,*)'Error in reading the header of the TEMP observation ',istnr,lat,lon,hgt,ierr
             CALL abort
          ENDIF

          IF (istnr == 0) CYCLE READ_STATION_OBS
          IF (( ABS(lat - mflag) < 1.e-4 ) ) CYCLE READ_STATION_OBS
          IF (( ABS(lon - mflag) < 1.e-4 ) ) CYCLE READ_STATION_OBS

          !
          ! Find station index
          !

          IF(stations(istnr) == 0) THEN

             stat_i = 0
             IF ( use_stnlist ) THEN
                DO ii=1,maxstn
                   IF (istnr == stnlist(ii) ) stat_i = ii
                ENDDO
                IF (print_read>1) WRITE(6,*)'STATION ',istnr,stat_i
                IF ( stat_i == 0 ) THEN
                    DO kk=1,num_temp_lev
                       READ(lunin,*)
                    ENDDO
                    CYCLE READ_STATION_OBS
                ENDIF
             ENDIF

             IF (stat_i == 0 ) THEN
                max_found_stat  = max_found_stat + 1
                stnlist(max_found_stat) = istnr
             ELSE
                max_found_stat  = stat_i
             ENDIF

             IF (max_found_stat > maxstn) THEN
                WRITE(6,*)'Increase maxstn',max_found_stat
                CALL abort
             ENDIF

             stations(istnr) = max_found_stat
             obs(max_found_stat)%active = .TRUE.
             obs(max_found_stat)%stnr   = istnr
             obs(max_found_stat)%lat    = lat
             obs(max_found_stat)%lon    = lon

          ENDIF

          stat_i = stations(istnr)

          i = obs(stat_i)%ntim + 1 
         
          ALLOCATE(obs(stat_i)%o(i)%date)
          ALLOCATE(obs(stat_i)%o(i)%time)
          ALLOCATE(obs(stat_i)%o(i)%val(nparver))
   
          obs(stat_i)%ntim      = i
          obs(stat_i)%o(i)%date = cdate
          obs(stat_i)%o(i)%time = ctime/10000
          obs(stat_i)%o(i)%val  = err_ind

          READ_LEV_OBS : DO kk=1,num_temp_lev

          val = mflag
          SELECT CASE(version_flag)
          CASE(0)
             1001 format(1x,f5.0,f6.0,f6.1,f6.1,f5.0,f5.0,en13.3e2)
             READ(lunin,1001,iostat=ierr)val
          CASE(1:2,4)
             READ(lunin,*,iostat=ierr)val
          END SELECT 

          IF (print_read>1) WRITE(6,*)'READ',kk,val

          IF (ierr /= 0 ) CYCLE READ_LEV_OBS

          kk_lev = 0
          DO kkk=1,my_temp_lev
             IF (ABS(val(ipr) - lev_lst(kkk)) < 1.e-6) kk_lev = kkk
          ENDDO
          IF ( kk_lev == 0 ) CYCLE READ_LEV_OBS

          IF (print_read>1) WRITE(6,*)'ADD:',kk_lev,val

          PARVER_LOOP : DO m=1,nparver
            INVAR_LOOP : DO n=1,ninvar
              IF ( varprop(m)%id == invar(n) .AND. &
                   (ABS(varprop(m)%lev - val(ipr)) < 1.e-6 ) .AND. &
                   varprop(m)%active ) THEN

                ! Check for missing data flag
                IF ( .NOT. qca(val(n),mflag) ) CYCLE PARVER_LOOP

                ! Special treatment of some variabels
                sca = 1.0
                sub = 0.0
                SELECT CASE(invar(n)(1:2))

                CASE('TT')
                   sub = tzero
                CASE('QQ')
                   sca = 1.e3
                END SELECT

                IF ( do_you_like_me(cbl,obs(stat_i)%stnr,invar(n)) ) THEN
                 ! Check for missing data / gross error
                  IF ( qclr(val(n),varprop(m)%llim) .AND. &
                       qcur(val(n),varprop(m)%ulim) )     &
                  obs(stat_i)%o(i)%val(m) = ( val(n) - sub ) * sca
                ENDIF

              ENDIF
            ENDDO INVAR_LOOP

            ! Static pseudo variables
            SELECT CASE(varprop(m)%id)
             CASE('ISS')
               mm=find_var(ninvar,invar,'RH',val(ipr),varprop(m)%lev)
               m2=find_var(ninvar,invar,'TT',val(ipr),varprop(m)%lev)
               mmp=find_varprop('RH',val(ipr))
               m2p=find_varprop('TT',val(ipr))

               IF ( mm > 0 .AND. mmp > 0 .AND. &
                    m2 > 0 .AND. m2p > 0 ) THEN

                IF ( qca(val(mm),mflag)             .AND. &
                     qca(val(m2),mflag)             .AND. &
                     qclr(val(mm),varprop(mmp)%llim) .AND. &
                     qcur(val(mm),varprop(mmp)%ulim) .AND. &
                     qclr(val(m2),varprop(m2p)%llim) .AND. &
                     qcur(val(m2),varprop(m2p)%ulim) ) THEN
                 obs(stat_i)%o(i)%val(m) = &
                 get_iss(val(mm),val(m2))
                ENDIF

               ENDIF
            END SELECT

          IF ( print_read > 1 ) &
              WRITE(6,*)obs(stat_i)%stnr,varprop(m)%id,varprop(m)%lev, &
                        obs(stat_i)%o(i)%val(m)

          ENDDO PARVER_LOOP

       ENDDO READ_LEV_OBS

       IF(print_read>1) WRITE(6,*)'DONE',istnr

       ENDDO READ_STATION_OBS

       CLOSE(lunin)

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,obint*3600,cdate,ctime)
    IF(cdate >  edate_obs) EXIT TIME_LOOP
    IF(cdate >= edate_obs .AND. ctime/10000 > etime_obs) EXIT TIME_LOOP

 ENDDO TIME_LOOP

 WRITE(6,*) 'FOUND TIMES OBS',MAXVAL(obs(:)%ntim)

 DO i=1,maxstn
    obs(i)%active = ( obs(i)%ntim > 0 )
 ENDDO

 ! Clear memory

 IF ( ALLOCATED(invar) ) DEALLOCATE(invar,val)
 IF ( ALLOCATED(inacc) ) DEALLOCATE(inacc)

 RETURN

END
