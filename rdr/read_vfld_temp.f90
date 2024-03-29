SUBROUTINE read_vfld_temp

 !
 ! Read TEMP part of vfldEXPyyyymmddhhll
 ! and organize in data array
 ! for verification and plotting
 !
 ! Ulf Andrae, SMHI, 2004
 !

 USE data
 USE functions
 USE constants

 IMPLICIT NONE

 REAL, PARAMETER :: mflag = -99.

 INTEGER :: i,ii,j,k,kk,kkk,l,          &
            kk_lev,m,n,mm,m2,           &
            m2p,mmp,                    &
            ierr = 0,aerr=0,            &
            cdate = 999999,             &
            ctime = 999999,             &
            cdateo = 999999,            &
            ctimeo = 999999,            &
            wdate = 999999,             &
            wtime = 999999,             &
            istnr = 0,                  &
            stat_i,fcleno,              &
            num_temp,num_stat,          &
            num_temp_lev,my_temp_lev,   &
            stations(10000000),         &
            max_found_stat,             &
            wrk(mparver),               &
            version_flag,               &
            ipr,ifi,ninvar,             &
            old_ninvar,nvars,hh
 
 REAL :: lat,lon,hgt,sca,sub
 REAL, ALLOCATABLE :: val(:),inacc(:)

 LOGICAL :: allocated_this_time(maxstn),&
            found_any_time,use_stnlist,lfound,&
            read_error

 CHARACTER(LEN=299), ALLOCATABLE :: fname(:) 
 CHARACTER(LEN= 10), ALLOCATABLE :: invar(:)


!----------------------------------------------------------

 ! Init
 hgt            = err_ind
 ipr = -1
 ifi = -1
 stations       = 0
 max_found_stat = 0
 version_flag   = 0
 old_ninvar     = -1
 ninvar         = 0

 use_stnlist =( MAXVAL(stnlist) > 0 )

 CALL allocate_mod

 ! Copy time

 cdate = sdate
 ctime = stime*10000

 wrk = 0
 WHERE( lev_lst > 0 ) wrk = 1
 my_temp_lev = SUM(wrk)

 IF (print_read>1) WRITE(6,*)'MY_TEMP_LEV',my_temp_lev

 !
 ! Loop over all times
 !

 i = 0
 hh = ctime/10000

 ALLOCATE(fname(nexp))

 TIME_LOOP : DO

    allocated_this_time = .FALSE.
    found_any_time      = .FALSE.

    !
    ! Read model data
    !

    LL_LOOP  : DO j=1,nfclengths

    !
    ! Check that all files are available
    ! If not, skip this forecast length
    !

    SUB_EXP_LOOP : DO l=1,nexp

       IF ( fexpname(l) == '#' ) fexpname(l) = expname(l)
       IF ( exp_offset(l,hh) /= 0 ) THEN
         CALL adddtg(cdate,ctime,-exp_offset(l,hh)*60,cdateo,ctimeo)
         fcleno=fclen(j)*60 + exp_offset(l,hh)
       ELSE
         cdateo=cdate
         ctimeo=ctime
         fcleno=fclen(j)*60
       ENDIF
       
       CALL check_path(.FALSE.,cdateo,ctimeo,fcleno,fexpname(l), &
                       modpath(l),use_analysis(l),fname(l))

       INQUIRE(FILE=fname(l),EXIST=lfound)
       IF ( .NOT. lfound ) THEN
          IF (print_read > 0 ) &
          WRITE(6,'(2A)')'MISS ',TRIM(fname(l))
          CYCLE LL_LOOP 
       ENDIF
    ENDDO SUB_EXP_LOOP

    EXP_LOOP : DO l=1,nexp

       OPEN(lunin,file=fname(l),status='old',iostat=ierr)
       IF (ierr /= 0) THEN
          IF (print_read > 0 ) WRITE(6,'(2A)')'Could not open ',TRIM(fname(l))
          CYCLE LL_LOOP
       ENDIF
       IF (print_read > 0 ) WRITE(6,'(2A)')'READ ',TRIM(fname(l))

       version_flag = 0

       READ(lunin,'(1X,3I6)',IOSTAT=ierr)num_stat,num_temp,version_flag
       IF ( ierr /= 0 ) THEN
         WRITE(6,*)'Error reading first line of vfld file:',TRIM(fname(l))
         CLOSE(lunin)
         CYCLE LL_LOOP
       ENDIF
       IF (print_read > 1 ) WRITE(6,*)'NUM_STAT,NUM_TEMP,VERSION_FLAG',&
       num_stat,num_temp,version_flag

       IF ( num_temp == 0 ) THEN
          CLOSE(lunin)
          CYCLE LL_LOOP
       ENDIF

       read_error = .FALSE.
       SELECT CASE (version_flag)
        CASE(0:3)
          READ(lunin,*,IOSTAT=ierr)num_temp_lev
          read_error = ( read_error .OR. ierr /= 0 )
          DO k=1,num_stat
            READ(lunin,*,IOSTAT=ierr)
          ENDDO
        CASE(4,5)
          READ(lunin,*,IOSTAT=ierr)nvars
          read_error = ( read_error .OR. ierr /= 0 )
          DO k=1,nvars
            READ(lunin,*,IOSTAT=ierr)
          ENDDO
          DO k=1,num_stat
            READ(lunin,*,IOSTAT=ierr)
          ENDDO
        CASE DEFAULT
          WRITE(6,*)'Cannot handle this version flag',version_flag
          WRITE(6,*)'FILE:',TRIM(fname(l))
          CALL abort
       END SELECT

       IF (read_error) THEN
         WRITE(6,*)'Error reading vfld header:',TRIM(fname(l))
         CLOSE(lunin)
         CYCLE LL_LOOP
       ENDIF

       read_error=.FALSE.
       SELECT CASE(version_flag)
        CASE(0)
          ninvar=7
          IF ( ALLOCATED(invar) ) DEALLOCATE(invar,val,inacc)
          ALLOCATE(invar(ninvar),val(ninvar),inacc(ninvar))
          invar = (/'PR','FI','TT','RH','DD','FF','QQ'/)
          ipr = 1
          ifi = 2
        CASE(1:3)
          ninvar=8
          IF ( ALLOCATED(invar) ) DEALLOCATE(invar,val,inacc)
          ALLOCATE(invar(ninvar),val(ninvar),inacc(ninvar))
          invar = (/'PR','FI','TT','RH','DD','FF','QQ','TD'/)
          ipr = 1
          ifi = 2
        CASE(4,5)
          ipr = -1 
          ifi = -1 
          READ(lunin,*,IOSTAT=ierr)num_temp_lev
          read_error = ( read_error .OR. ierr /= 0 )
          READ(lunin,*,IOSTAT=ierr)ninvar
          read_error = ( read_error .OR. ierr /= 0 )
          IF (print_read>1)WRITE(6,*)'NUM_TEMP_LEV,NINVAR',&
          num_temp_lev,ninvar
          IF ( ninvar /= old_ninvar ) THEN
            IF ( ALLOCATED(invar) ) DEALLOCATE(invar,val,inacc)
            ALLOCATE(invar(ninvar),val(ninvar),inacc(ninvar))
          ENDIF
          DO i=1,ninvar
            READ(lunin,*,IOSTAT=ierr)invar(i),inacc(i)
           read_error = ( read_error .OR. ierr /= 0 )
            IF ( invar(i) == 'PR' ) ipr = i
            IF ( invar(i) == 'PP' ) ipr = i
            IF ( invar(i) == 'FI' ) ifi = i
          ENDDO
          IF ( ipr == -1 .OR. ifi == -1 ) THEN
            WRITE(6,*)'FI or PR/PP not found'
            CALL abort
          ENDIF
        CASE DEFAULT
          WRITE(6,*)'Cannot handle this vfld-file version',version_flag
          CALL abort
       END SELECT

       IF (read_error) THEN
         WRITE(6,*)'Error reading vfld header:',TRIM(fname(l))
         CLOSE(lunin)
         CYCLE LL_LOOP
       ENDIF

       old_ninvar = ninvar

       READ_STATION_MOD : DO k=1,num_temp

          SELECT CASE (version_flag)
          CASE(0)
             READ(lunin,*,iostat=ierr)istnr,lat,lon
             hgt = err_ind
          CASE(1:5)
             READ(lunin,*,iostat=ierr)istnr,lat,lon,hgt
          CASE DEFAULT
             WRITE(6,*)'Cannot handle this vfld-file version',version_flag
             CALL abort
          END SELECT 

          IF (ierr /= 0) CYCLE READ_STATION_MOD

          IF (print_read>1) WRITE(6,*)'Treat station',istnr
          !
          ! Find station index
          ! Search the first experiment only
          !

          IF ( l == 1 ) THEN
            IF(stations(istnr) == 0) THEN

             stat_i = 0
             IF ( use_stnlist ) THEN
                DO ii=1,maxstn
                   IF (istnr == stnlist(ii) ) stat_i = ii
                ENDDO
                IF ( stat_i == 0 ) THEN
                   DO kk=1,num_temp_lev
                      READ(lunin,*)
                   ENDDO
                   CYCLE READ_STATION_MOD
                ENDIF
             ENDIF

             IF (stat_i == 0 ) THEN
                max_found_stat  = max_found_stat + 1
                stnlist(max_found_stat) = istnr
                IF (print_read > 1 ) WRITE(6,*)'Added station ',istnr
             ELSE
                max_found_stat  = stat_i
             ENDIF

             IF (max_found_stat > maxstn) THEN
                WRITE(6,*)'Increase maxstn',max_found_stat
                CALL abort
             ENDIF

             stations(istnr) = max_found_stat
             hir(max_found_stat)%active = .TRUE.
             hir(max_found_stat)%stnr   = istnr
             hir(max_found_stat)%lat    = lat
             hir(max_found_stat)%lon    = lon

           ENDIF

          ELSE

           IF(stations(istnr) == 0) THEN
             DO kk=1,num_temp_lev
               READ(lunin,*)
             ENDDO
             CYCLE READ_STATION_MOD
           ENDIF

          ENDIF

          stat_i = stations(istnr)

          ! Store experiment specific station height
          hir(stat_i)%hgtmod(l) = hgt

          !
          ! Station found! Allocate data array if 
          ! this is a new time
          !
         
          IF ( .NOT. allocated_this_time(stat_i) ) THEN 

             hir(stat_i)%ntim =  hir(stat_i)%ntim + 1
             i = hir(stat_i)%ntim  

             ALLOCATE(hir(stat_i)%o(i)%date)
             ALLOCATE(hir(stat_i)%o(i)%time)
             ALLOCATE(hir(stat_i)%o(i)%nal(nexp,nfclengths,nparver),stat=aerr)

             IF ( aerr /= 0 ) THEN
                WRITE(6,*)'ERROR IN ALLOCATE',aerr
                CALL ABORT
             ENDIF
   
             hir(stat_i)%o(i)%date = cdate
             hir(stat_i)%o(i)%time = hh
             hir(stat_i)%o(i)%nal  = err_ind
 
             allocated_this_time(stat_i) = .TRUE.

             IF (print_read > 1 ) WRITE(6,*)'ALLOCATED',istnr,stat_i,cdate,hh
  
          ENDIF

          i = hir(stat_i)%ntim  

          READ_LEV_MOD : DO kk=1,num_temp_lev

          val = mflag

          SELECT CASE(version_flag)
          CASE(0:4)
             READ(lunin,*,iostat=ierr)val
          END SELECT 

          IF (ierr /= 0 ) THEN
            IF (print_read>1) WRITE(6,*)'Error reading val'
            CYCLE READ_LEV_MOD
          ENDIF

          kk_lev = 0
          DO kkk=1,my_temp_lev
             IF (ABS(val(ipr) - lev_lst(kkk)) < 1.e-6) kk_lev = kkk
          ENDDO
          IF (kk_lev == 0 ) CYCLE READ_LEV_MOD

          IF (print_read>1) WRITE(6,*)'KK_LEV',kk_lev,val(1),lev_lst(kk_lev)

          ! Do not use levels below model topography
          IF ( (ABS(hgt    - err_ind ) > 1.e-6) .AND. &
               (ABS(val(ifi) - mflag   ) > 1.e-6) .AND. &
                    val(ifi) < hgt                     ) CYCLE READ_LEV_MOD

          PARVER_LOOP : DO m=1,nparver
            INVAR_LOOP : DO n=1,ninvar
              IF ( varprop(m)%id == invar(n) .AND. &
                   (ABS(varprop(m)%lev - val(ipr)) < 1.e-6 ) .AND. &
                   varprop(m)%active ) THEN

                ! Check for missing data flag
                IF ( .NOT. qca(val(n),mflag) ) CYCLE PARVER_LOOP

                ! Special treatment of some variabels
                sub = 0.0
                sca = 1.0

                SELECT CASE(invar(n))

                CASE('TT')
                   sub = tzero
                CASE('QQ')
                   sca = 1.e3
                END SELECT

                ! Check for gross error
                ! Check for missing data / gross error
                IF ( qclr(val(n),varprop(m)%llim) .AND. &
                     qcur(val(n),varprop(m)%ulim) )     &
                hir(stat_i)%o(i)%nal(l,j,m) = ( val(n) - sub ) * sca

              ENDIF
            ENDDO INVAR_LOOP

            mm=find_var(ninvar,invar,varprop(m)%id,val(ipr),varprop(m)%lev)
            IF ( mm == 0 ) THEN

             !
             ! Set derived pseudo variables if not 
             ! already in the input data
             !

             SELECT CASE(TRIM(varprop(m)%id))
              CASE('ISS')
               mm=find_var(ninvar,invar,'RH',val(ipr),varprop(m)%lev)
               m2=find_var(ninvar,invar,'TT',val(ipr),varprop(m)%lev)
               IF ( mm > 0 .AND. m2 > 0 ) THEN
                 hir(stat_i)%o(i)%nal(l,j,m) =           &
                 get_iss(val(mm),val(m2))
               ENDIF
              CASE('TDD')
               mm=find_var(ninvar,invar,varprop(m)%id(1:2),val(ipr),varprop(m)%lev)
               m2=find_var(ninvar,invar,'TT',val(ipr),varprop(m)%lev)
               mmp=find_varprop(varprop(m)%id(1:2),val(ipr))
               m2p=find_varprop('TT',val(ipr))
               IF ( mm > 0 .AND. mmp > 0 .AND. &
                    m2 > 0 .AND. m2p > 0 ) THEN
                IF ( qclr(val(mm),varprop(mmp)%llim) .AND. &
                     qcur(val(mm),varprop(mmp)%ulim) .AND. &
                     qclr(val(m2),varprop(m2p)%llim) .AND. &
                     qcur(val(m2),varprop(m2p)%ulim) )     &
                  hir(stat_i)%o(i)%nal(l,j,m) =          &
                  val(m2) - val(mm)
               ENDIF
             END SELECT

            ENDIF

            IF ( print_read > 1 ) &
              WRITE(6,*)hir(stat_i)%stnr,varprop(m)%id,varprop(m)%lev, &
                        hir(stat_i)%o(i)%nal(l,j,m)

          ENDDO PARVER_LOOP
          
       ENDDO READ_LEV_MOD

       ENDDO READ_STATION_MOD

       CLOSE(lunin)

    ENDDO EXP_LOOP
    ENDDO LL_LOOP

    !
    ! Step time
    !

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,fcint*3600,cdate,ctime)
    hh = ctime / 10000
    IF(cdate > edate) EXIT TIME_LOOP
    IF(cdate == edate .AND. hh > etime) EXIT TIME_LOOP

 ENDDO TIME_LOOP

 DO i=1,maxstn
    hir(i)%active = ( hir(i)%ntim > 0 )
 ENDDO

 WRITE(6,*) 'FOUND TIMES MODEL',MAXVAL(hir(:)%ntim)

 ! Clear memory

 IF ( ALLOCATED(invar) ) DEALLOCATE(invar,val)
 IF ( ALLOCATED(inacc) ) DEALLOCATE(inacc)
 DEALLOCATE(fname)

 RETURN

END SUBROUTINE read_vfld_temp
