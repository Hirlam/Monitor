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

 INTEGER :: i,ii,j,k,kk,kkk,l,ll,       &
            kk_lev,m,n,                 &
            ierr = 0,aerr=0,            &
            cdate = 999999,             &
            ctime = 999999,             &
            cdateo = 999999,            &
            ctimeo = 999999,            &
            wdate = 999999,             &
            wtime = 999999,             &
            istnr = 0,                  &
            stat_i,                     &
            num_temp,num_stat,          &
            num_temp_lev,my_temp_lev,   &
            stations(10000000),         &
            max_found_stat,             &
            wrk(mparver),               &
            version_flag,               &
            ipr,ifi,ninvar,             &
            old_ninvar,nvars
 
 REAL :: lat,lon,hgt
 REAL, ALLOCATABLE :: val(:),inacc(:)

 LOGICAL :: allocated_this_time(maxstn),&
            found_any_time,use_stnlist,lfound

 CHARACTER(LEN=299) :: fname = ' ',path
 CHARACTER(LEN= 10) :: cwrk  ='yyyymmddhh',cwrko
 CHARACTER(LEN= 03) :: cfclen  ='  ',cfcleno
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

    WRITE(cwrk(1:10),'(I8,I2.2)')cdate,ctime/10000
    IF ( fclen(j) > 99 ) THEN
      WRITE(cfclen,'(I3.3)')fclen(j)
    ELSE
      WRITE(cfclen,'(I2.2,1X)')fclen(j)
    ENDIF

    SUB_EXP_LOOP : DO ll=1,nexp

       path = modpath(ll)
       CALL check_path(cdate,path)

       IF ( fexpname(ll) == '#' ) fexpname(ll) = expname(ll)
       IF ( exp_offset(ll) /= 0 ) THEN
         CALL adddtg(cdate,ctime,-exp_offset(ll)*3600,cdateo,ctimeo)
         WRITE(cwrko(1:10),'(I8,I2.2)')cdateo,ctimeo/10000
         IF ( ( fclen(j) + exp_offset(ll) ) > 99 ) THEN
           WRITE(cfcleno,'(I3.3)')fclen(j)+exp_offset(ll)
         ELSE
           WRITE(cfcleno,'(I2.2,1X)')fclen(j)+exp_offset(ll)
         ENDIF
         fname = TRIM(path)//'vfld'//TRIM(fexpname(ll))//cwrko//TRIM(cfcleno)
       ELSE
         fname = TRIM(path)//'vfld'//TRIM(fexpname(ll))//cwrk//TRIM(cfclen)
       ENDIF

       INQUIRE(FILE=fname,EXIST=lfound)
       IF ( .NOT. lfound ) THEN
          IF (print_read > 0 ) &
          WRITE(6,'(2A)')'MISS ',TRIM(fname)
          CYCLE LL_LOOP 
       ENDIF
    ENDDO SUB_EXP_LOOP

    EXP_LOOP : DO l=1,nexp

       path = modpath(l)
       CALL check_path(cdate,path)

       IF ( exp_offset(l) /= 0 ) THEN
         CALL adddtg(cdate,ctime,-exp_offset(l)*3600,cdateo,ctimeo)
         WRITE(cwrko(1:10),'(I8,I2.2)')cdateo,ctimeo/10000
         IF ( ( fclen(j) + exp_offset(l) ) > 99 ) THEN
           WRITE(cfcleno,'(I3.3)')fclen(j)+exp_offset(l)
         ELSE
           WRITE(cfcleno,'(I2.2,1X)')fclen(j)+exp_offset(l)
         ENDIF
         fname = TRIM(path)//'vfld'//TRIM(fexpname(l))//cwrko//TRIM(cfcleno)
       ELSE
         fname = TRIM(path)//'vfld'//TRIM(fexpname(l))//cwrk//TRIM(cfclen)
       ENDIF

       OPEN(lunin,file=fname,status='old',iostat=ierr)
       IF (ierr /= 0 .AND. print_read > 0 ) WRITE(6,*)'COULD NOT READ ',fname
       IF (ierr /= 0) CYCLE EXP_LOOP
       IF (print_read > 0 ) WRITE(6,*)'READ ',TRIM(fname)

       version_flag = 0

       READ(lunin,'(1X,3I6)',IOSTAT=ierr)num_stat,num_temp,version_flag
       IF ( ierr /= 0 ) THEN
         WRITE(6,*)'Error reading first line of vfld file',ierr
         CALL abort
       ENDIF
       IF (print_read > 1 ) WRITE(6,*)'NUM_STAT,NUM_TEMP,VERSION_FLAG',&
       num_stat,num_temp,version_flag

       IF ( num_temp == 0 ) THEN
          CLOSE(lunin)
          CYCLE EXP_LOOP
       ENDIF

       SELECT CASE (version_flag)
        CASE(0:3)
          READ(lunin,*)num_temp_lev

          DO k=1,num_stat
            READ(lunin,*)
          ENDDO
        CASE(4)
          READ(lunin,*)nvars
          DO k=1,nvars
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
        CASE(1:3)
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
          IF (print_read>1)WRITE(6,*)'NUM_TEMP_LEV,NINVAR',&
          num_temp_lev,ninvar
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
          WRITE(6,*)'Cannot handle this vfld-file version',version_flag
          CALL abort
       END SELECT

       old_ninvar = ninvar

       READ_STATION_MOD : DO k=1,num_temp

          SELECT CASE (version_flag)
          CASE(0)
             READ(lunin,*,iostat=ierr)istnr,lat,lon
             hgt = err_ind
          CASE(1:4)
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
             hir(stat_i)%o(i)%time = ctime/10000
             hir(stat_i)%o(i)%nal  = err_ind
 
             allocated_this_time(stat_i) = .TRUE.

             IF (print_read > 1 ) WRITE(6,*)'ALLOCATED',istnr,stat_i,cdate,ctime/10000
  
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
                SELECT CASE(invar(n))

                CASE('TT')
                   val(n) = val(n) - tzero
                CASE('QQ')
                   val(n) = val(n) * 1.e3
                END SELECT

                ! Check for gross error
                 IF ( qclr(val(n),varprop(m)%llim) .AND. &
                      qcur(val(n),varprop(m)%ulim) )     &
                hir(stat_i)%o(i)%nal(l,j,m) = val(n)

              ENDIF
            ENDDO INVAR_LOOP
            IF (print_read>1) WRITE(6,*)varprop(m)%id,hir(stat_i)%o(i)%nal(l,j,m)
          ENDDO PARVER_LOOP

       ENDDO READ_LEV_MOD

       ENDDO READ_STATION_MOD

       CLOSE(lunin)

    ENDDO EXP_LOOP
    ENDDO LL_LOOP

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,fcint*3600,cdate,ctime)
    IF(cdate > edate) EXIT TIME_LOOP
    IF(cdate == edate .AND. ctime/10000 > etime) EXIT TIME_LOOP

 ENDDO TIME_LOOP
 
 WRITE(6,*) 'FOUND TIMES MODEL',MAXVAL(hir(:)%ntim)

 DO i=1,maxstn
    hir(i)%active = ( hir(i)%ntim > 0 )
 ENDDO

 RETURN

END
