SUBROUTINE read_vfld

 !
 ! Read synop part of vfldEXPyyyymmddhhll 
 ! and organize in data array 
 ! for verification and plotting
 ! 
 ! Ulf Andrae, SMHI, 2004
 !

 USE data, only : hir,obs,varprop,exp_offset,fclen, &
                  expname,modpath,stime,etime,  &
                  stnlist,sdate,edate,nfclengths, &
                  nexp,err_ind,print_read,maxstn, &
                  fcint,lunin,nparver,qca,qclr,qcur, &
                  allocate_mod

 USE constants, only : gravit,tzero,tlapse

 IMPLICIT NONE

 REAL, PARAMETER :: mflag = -99.

 INTEGER :: i,ii,j,k,l,ll,m,mm,n,m2,    &
            ierr = 0,                   &
            cdate = 999999,             &
            ctime = 999999,             &
            cdateo = 999999,            &
            ctimeo = 999999,            &
            wdate = 999999,             &
            wtime = 999999,             &
            istnr = 0,                  &
            stat_i,                     &
            ninvar,old_ninvar,          &
            num_temp,num_stat,          &
            num_temp_lev,               &
            stations(10000000),         &
            max_found_stat,             &
            aerr,version_flag,          &
            old_version_flag
 
 INTEGER, ALLOCATABLE :: inacc(:)
 
 REAL :: lat,lon,hgt,rtmp,sca,sub
 REAL, ALLOCATABLE :: val(:)

 LOGICAL :: allocated_this_time(maxstn),&
            found_any_time,use_stnlist,lfound

 CHARACTER(LEN=200) :: fname = ' '
 CHARACTER(LEN= 10) :: cwrk  ='yyyymmddhh',cwrko
 CHARACTER(LEN= 03) :: cfclen,cfcleno
 CHARACTER(LEN= 10), ALLOCATABLE :: invar(:) 

 ! Functions
 INTEGER :: find_var

!----------------------------------------------------------

 ! Init 
 stations         = 0
 max_found_stat   = 0
 old_version_flag = -1
     version_flag = 0
 old_ninvar       = -1
     ninvar       = 0

 use_stnlist =( MAXVAL(stnlist) > 0 )

 CALL allocate_mod

 ! Copy time

 cdate = sdate
 ctime = stime*10000
 wdate = cdate
 wtime = ctime

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

       IF ( exp_offset(ll) /= 0 ) THEN
         CALL adddtg(cdate,ctime,-exp_offset(ll)*3600,cdateo,ctimeo)
         WRITE(cwrko(1:10),'(I8,I2.2)')cdateo,ctimeo/10000
         IF ( ( fclen(j) + exp_offset(ll) ) > 99 ) THEN
           WRITE(cfcleno,'(I3.3)')fclen(j)+exp_offset(ll)
         ELSE
           WRITE(cfcleno,'(I2.2,1X)')fclen(j)+exp_offset(ll)
         ENDIF
         fname = TRIM(modpath(ll))//'vfld'//TRIM(expname(ll))//cwrko//TRIM(cfcleno)
       ELSE
         fname = TRIM(modpath(ll))//'vfld'//TRIM(expname(ll))//cwrk//TRIM(cfclen)
       ENDIF

       INQUIRE(FILE=fname,EXIST=lfound)
       IF ( .NOT. lfound ) THEN
          IF (print_read > 0 ) &
          WRITE(6,'(2A)')'MISS ',TRIM(fname)
          CYCLE LL_LOOP 
       ENDIF
    ENDDO SUB_EXP_LOOP

    EXP_LOOP : DO l=1,nexp

       IF ( exp_offset(l) /= 0 ) THEN
         CALL adddtg(cdate,ctime,-exp_offset(l)*3600,cdateo,ctimeo)
         WRITE(cwrko(1:10),'(I8,I2.2)')cdateo,ctimeo/10000
         IF ( ( fclen(j) + exp_offset(l) ) > 99 ) THEN
           WRITE(cfcleno,'(I3.3)')fclen(j)+exp_offset(l)
         ELSE
           WRITE(cfcleno,'(I2.2,1X)')fclen(j)+exp_offset(l)
         ENDIF
         fname = TRIM(modpath(l))//'vfld'//TRIM(expname(l))//cwrko//TRIM(cfcleno)
       ELSE
         fname = TRIM(modpath(l))//'vfld'//TRIM(expname(l))//cwrk//TRIM(cfclen)
       ENDIF

       OPEN(lunin,file=fname,status='old',iostat=ierr)
       IF (ierr /= 0) THEN
          IF (print_read > 0 ) WRITE(6,'(2A)')'Could not open ',TRIM(fname)
          CYCLE EXP_LOOP
       ENDIF
       IF (print_read > 0 ) WRITE(6,'(2A)')'READ ',TRIM(fname)

       version_flag = 0

       READ(lunin,'(1X,3I6)',IOSTAT=ierr)num_stat,num_temp,version_flag
       IF ( ierr /= 0 ) THEN
         WRITE(6,*)'Error reading first line of vfld file',ierr
         CALL abort
       ENDIF

       IF ( version_flag /= old_version_flag ) THEN
           SELECT CASE(version_flag)
           CASE(0)
            IF ( ALLOCATED(invar) ) DEALLOCATE(invar,val)
             ninvar=8
             ALLOCATE(invar(ninvar),val(ninvar))
             invar = (/'NN','DD','FF','TT','RH','PS','PE','QQ'/)
           CASE(1)
            IF ( ALLOCATED(invar) ) DEALLOCATE(invar,val)
             ninvar=10
             ALLOCATE(invar(ninvar),val(ninvar))
             invar = (/'NN','DD','FF','TT','RH','PS','PE','QQ','VI','TD'/)
          CASE(2,3)
            IF ( ALLOCATED(invar) ) DEALLOCATE(invar,val)
             ninvar=15
             ALLOCATE(invar(ninvar),val(ninvar))
             invar = (/'NN','DD','FF','TT','RH', &
                       'PS','PE','QQ','VI','TD', &
                       'TX','TN','GG','GX','FX'/)
          CASE(4)

          CASE DEFAULT
             WRITE(6,*)'Cannot handle this vfld-file version',version_flag
             CALL abort
          END SELECT
       ENDIF 

       old_version_flag = version_flag

       SELECT CASE(version_flag)
       CASE(0:3)
         READ(lunin,*)num_temp_lev
       CASE(4)
          READ(lunin,*,IOSTAT=ierr)ninvar
          IF ( ninvar /= old_ninvar ) THEN
            IF ( ALLOCATED(invar) ) DEALLOCATE(invar,val,inacc)
            ALLOCATE(invar(ninvar),val(ninvar),inacc(ninvar))
            old_ninvar = ninvar
          ENDIF
          DO i=1,ninvar
            READ(lunin,*,IOSTAT=ierr)invar(i),inacc(i)
          ENDDO
       END SELECT

       !
       ! Read, identify and store station data
       !

       READ_STATION_MOD : DO k=1,num_stat

          val = mflag
          SELECT CASE(version_flag)
 
          CASE(0)
             hgt = err_ind
             READ(lunin,*,iostat=ierr)istnr,lat,lon,val(1:8)
          CASE(1)
             READ(lunin,*,iostat=ierr)istnr,lat,lon,hgt,val(1:10)
          CASE(2)
             READ(lunin,*,iostat=ierr)istnr,lat,lon,hgt,val(1:15)

             IF ( ALL( ABS(val(11:12) - mflag )> 1.e-6 ) ) THEN

                !
                ! Swap Tmax and Tmin since they are wrong in some versions of 
                ! the version 2 files
                !

                IF ( val(11) < val(12) ) THEN
                     rtmp = val(11)
                  val(11) = val(12)
                  val(12) = rtmp
                ENDIF
             ENDIF

          CASE(3:4)
             READ(lunin,*,iostat=ierr)istnr,lat,lon,hgt,val
          CASE DEFAULT
             WRITE(6,*)'Cannot handle this vfld-file version',version_flag
             CALL abort
          END SELECT

          IF (ierr /= 0) CYCLE READ_STATION_MOD

          !
          ! Find station index for the first experiment
          !

          IF ( l == 1 ) THEN
             
             SELECT CASE(stations(istnr))
             CASE(-1)
                CYCLE READ_STATION_MOD
             CASE( 0)

                stat_i = 0
                IF ( use_stnlist ) THEN
                   DO ii=1,maxstn
                      IF (istnr == stnlist(ii) ) THEN
                          stat_i = ii
                          EXIT
                      ENDIF
                   ENDDO
                   IF ( stat_i == 0 ) THEN
                      stations(istnr) = -1 
                      CYCLE READ_STATION_MOD
                   ENDIF
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
                hir(max_found_stat)%lat    = lat 
                hir(max_found_stat)%lon    = lon 
   
                IF (max_found_stat > maxstn) THEN
                   WRITE(6,*)'Increase maxstn',max_found_stat
                   CALL abort
                ENDIF

                IF (print_read > 1 ) WRITE(6,*)'ADDED',istnr,stations(istnr)

             END SELECT

          ELSE

             IF(stations(istnr) <= 0)  CYCLE READ_STATION_MOD

          ENDIF

          stat_i = stations(istnr)

          ! Store experiment specific station height
          hir(stat_i)%hgtmod(l)    = hgt 

          !
          ! Station found! Allocate data array if 
          ! this is a new time
          !
         
          IF (print_read > 1 .AND. hir(stat_i)%ntim > 0 ) &
          WRITE(6,*)'BOUND 1',istnr,UBOUND( hir(stat_i)%o(hir(stat_i)%ntim)%nal )
          IF (print_read > 1 .AND. hir(stat_i)%ntim > 0 ) &
          WRITE(6,*)'TEST',istnr,allocated_this_time(stat_i),stat_i

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
             found_any_time = .TRUE.

             IF (print_read > 1 ) WRITE(6,*)'ALLOCATED',istnr,stat_i,cdate,ctime/10000
  
          ENDIF

          i = hir(stat_i)%ntim  

          !
          ! Add data
          !
          !IF (print_read > 1 ) WRITE(6,*)'ADD',istnr,stat_i,cdate,ctime/10000,fclen(j)
          IF (print_read > 1 ) WRITE(6,*)'ADD',istnr,val
          !IF (print_read > 1 ) WRITE(6,*)'BOUND',istnr,UBOUND( hir(stat_i)%o(i)%nal )
          IF (print_read > 1 ) WRITE(6,*)'INVAR',invar

          PARVER_LOOP : DO m=1,nparver
            INVAR_LOOP : DO n=1,ninvar
              IF ( varprop(m)%id == invar(n) ) THEN

                ! Check for missing data flag
                IF ( .NOT. qca(val(n),mflag) ) CYCLE PARVER_LOOP

                ! Special treatment of some variabels
                sub = 0.0
                sca = 1.0
                SELECT CASE(invar(n))
 
                CASE('TT','TN','TX','TD')
                   sub = tzero
                CASE('QQ')
                   sca = 1.e3
                END SELECT

                ! Check for missing data / gross error
                 IF ( qclr(val(n),varprop(m)%llim) .AND. &
                      qcur(val(n),varprop(m)%ulim) )     &
                hir(stat_i)%o(i)%nal(l,j,m) = ( val(n) - sub ) * sca

              ENDIF
            ENDDO INVAR_LOOP

            ! Static pseudo variables
            SELECT CASE(TRIM(varprop(m)%id))
              CASE('LA')
               hir(stat_i)%o(i)%nal(l,j,m) = hir(stat_i)%lat
              CASE('HG')
               hir(stat_i)%o(i)%nal(l,j,m) = hgt
              CASE('TTHA','TNHA','TXHA')
               mm=find_var(ninvar,invar,varprop(m)%id(1:2))
               ! CALC hgt adjustment
               IF ( qclr(val(mm),varprop(m)%llim) .AND. &
                    qcur(val(mm),varprop(m)%ulim) .AND. &
                    qca(hir(stat_i)%hgt,err_ind) .AND. &
                    qca(obs(stat_i)%hgt,err_ind) )     &
               hir(stat_i)%o(i)%nal(l,j,m) =           &
               val(mm) - tzero + ((hir(stat_i)%hgtmod(l)-obs(stat_i)%hgt)*tlapse)
              CASE('TDD')
               mm=find_var(ninvar,invar,varprop(m)%id(1:2))
               m2=find_var(ninvar,invar,'TT')
               ! CALC hgt adjustment
               IF ( qclr(val(mm),varprop(m)%llim) .AND. &
                    qcur(val(mm),varprop(m)%ulim) .AND. &
                    qclr(val(m2),varprop(m)%llim) .AND. &
                    qcur(val(m2),varprop(m)%ulim) )     &
                 hir(stat_i)%o(i)%nal(l,j,m) =          &
                 val(m2) - val(mm)
            END SELECT

          ENDDO PARVER_LOOP

          IF (print_read > 1 ) WRITE(6,*)'VALUE',hir(stat_i)%o(i)%nal(l,j,:)

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
    IF(cdate >  edate) EXIT TIME_LOOP
    IF(cdate == edate .AND. ctime/10000 > etime) EXIT TIME_LOOP

 ENDDO TIME_LOOP

 DO i=1,maxstn
    hir(i)%active = ( hir(i)%ntim > 0 )
 ENDDO

 WRITE(6,*) 'FOUND TIMES MODEL',MAXVAL(hir(:)%ntim)

 ! Clear memory

 IF ( ALLOCATED(invar) ) DEALLOCATE(invar,val)
 IF ( ALLOCATED(inacc) ) DEALLOCATE(inacc)

 RETURN

END SUBROUTINE read_vfld

INTEGER function find_var(ninvar,invar,cvar)

 IMPLICIT NONE

 INTEGER, INTENT(IN) :: ninvar
 CHARACTER(LEN=*), INTENT(IN) :: invar(ninvar),cvar
 
 INTEGER :: i
 
 find_var = 0
 DO i=1,ninvar
    IF ( TRIM(cvar) == invar(i) ) THEN
       find_var = i
       EXIT
    ENDIF
 ENDDO
 
END function find_var
