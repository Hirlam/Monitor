SUBROUTINE odb_extract(cdtg)

#ifdef ODB_MONITOR
  USE odb_module
  USE odbutil
  USE module_obsmon, ONLY       : lusage,open_usage_files,close_usage_files,update_statistics,print_usage_files,&
                                  make_means_and_rms,write_output,verbose,lstat
  USE module_obstypes, ONLY     : all_obs,nused
  USE module_odb_extract, ONLY  : cstaid_odb,iobtyp_odb,icodetype_odb,ivarno_odb,ivertco_type_odb,&
                                  isensor_odb,idate_odb,itime_odb,istatus_acthdr_odb,istatus_blkhdr_odb,&
                                  istatus_pashdr_odb,istatus_rejhdr_odb,istatus_actbod_odb,istatus_blkbod_odb,&
                                  istatus_pasbod_odb,istatus_rejbod_odb,rlat_odb,rlon_odb,rpress_odb,ianflag_odb,&
                                  randep_odb,rfgdep_odb,robs_odb,rfinoe_odb,biascrl_odb,lsm_odb,alloc_odb_extracts,&
                                  dealloc_odb_extracts

  IMPLICIT NONE
  CHARACTER(LEN=10),INTENT(IN)               :: cdtg
  REAL(8), ALLOCATABLE                       :: x(:,:)
  INTEGER(4)                                 :: myproc,nproc,npools,jp,h
  INTEGER(4)                                 :: nrows, ncols, nra, rc
  INTEGER(4)                                 :: jr
  INTEGER                                    :: iobs
  CHARACTER(LEN=64)                          :: dbname, queryname
  CHARACTER(LEN=8)                           :: cstatid
  INTEGER                                    :: obs,nvar,i
  CHARACTER(LEN=20),ALLOCATABLE,DIMENSION(:) :: varnames
  REAL(8),ALLOCATABLE,DIMENSION(:)           :: vars
  CHARACTER(LEN=30)                          :: formatstr
  REAL                                       :: cpu
  INTEGER                                    :: totobs
  LOGICAL                                    :: lradians
  REAL,PARAMETER                             :: pi=3.14159

  cpu=0
  IF ( verbose > 0 ) THEN
   CALL cpu_time(cpu)
    WRITE(*,*) 'Start processing ODB for '//cdtg//'.  CPU time elapsed: ',cpu
  ENDIF

  ! Allocate arrays for odb extract
  CALL alloc_odb_extracts(1)

  call ec_putenv("ODB_CONSIDER_TABLES=hdr,body,modsufr,errstat,sat")
  rc = ODB_init(myproc=myproc,nproc=nproc)
  CALL getenv('ODB_CMA',dbname)
  npools = 0 ! Gets true value from ODB_open()
  h = ODB_open(dbname,'READONLY',npools)

  totobs=0
  DO obs=1,nused

    lradians=.FALSE.
    ! Satelite
    IF ( all_obs(obs)%lsat ) THEN
      write(cstatid,'(I8)') all_obs(obs)%sensor%satid
      queryname="obsmon_sat"
      nvar=5
      allocate(varnames(nvar))
      allocate(vars(nvar))
      varnames(1)="$obstype"
      varnames(2)="$varno"
      varnames(3)="$sensor"
      varnames(4)="$press"
      varnames(5)="$statid"
      vars(1)=all_obs(obs)%obnumber
      vars(2)=all_obs(obs)%var%nr
      vars(3)=all_obs(obs)%sensor%id
      vars(4)=all_obs(obs)%sensor%channel
      vars(5)=all_obs(obs)%sensor%satid
    ! Conventional + radar
    ELSEIF (( all_obs(obs)%var%nr == 195 ) .OR. ( all_obs(obs)%var%nr == 192 ))  THEN
      queryname="obsmon_conv2"
      nvar=6
      allocate(varnames(nvar))
      allocate(vars(nvar))
      varnames(1)="$obstype"
      varnames(2)="$varno"
      varnames(3)="$press1"
      varnames(4)="$press2"
      varnames(5)="$subtype1"
      varnames(6)="$subtype2"
      vars(1)=all_obs(obs)%obnumber
      vars(2)=all_obs(obs)%var%nr
      if (( all_obs(obs)%var%level1 == 0 ) .and. ( all_obs(obs)%var%level2 == 0 )) then
        vars(3)=0
        vars(4)=200000
      else
        vars(3)=all_obs(obs)%var%level1
        vars(4)=all_obs(obs)%var%level2
      endif
      vars(5)=all_obs(obs)%var%subtypestart
      vars(6)=all_obs(obs)%var%subtypeend
    ELSE
      queryname="obsmon_conv"
      nvar=6
      allocate(varnames(nvar))
      allocate(vars(nvar))
      varnames(1)="$obstype"
      varnames(2)="$varno"
      varnames(3)="$press1"
      varnames(4)="$press2"
      varnames(5)="$subtype1"
      varnames(6)="$subtype2"
      vars(1)=all_obs(obs)%obnumber
      vars(2)=all_obs(obs)%var%nr
      if (( all_obs(obs)%var%level1 == 0 ) .and. ( all_obs(obs)%var%level2 == 0 )) then
        vars(3)=0
        vars(4)=200000
      else
        vars(3)=all_obs(obs)%var%level1
        vars(4)=all_obs(obs)%var%level2
      endif
      vars(5)=all_obs(obs)%var%subtypestart
      vars(6)=all_obs(obs)%var%subtypeend
    ENDIF

    ! Initialization
    all_obs(obs)%var%count       =  0
    all_obs(obs)%var%fg_bias     =  0.
    all_obs(obs)%var%fg_abs_bias =  0.
    all_obs(obs)%var%fg_rms      =  0.
    all_obs(obs)%var%fg_dep      =  0.
    all_obs(obs)%var%fg_uncorr   =  0.
    all_obs(obs)%var%bc          =  0.
    all_obs(obs)%var%an_bias     =  0.
    all_obs(obs)%var%an_abs_bias =  0.
    all_obs(obs)%var%an_rms      =  0.
    all_obs(obs)%var%an_dep      =  0.

    ! Opening the usage files
    IF ( lusage ) THEN
      IF ( obs == 1 ) CALL SYSTEM("mkdir "//cdtg)
      IF ( obs == 1 ) CALL open_usage_files(obs,cdtg)
    ENDIF

    iobs=0
    WRITE(*,'(A,I4,A,I4,A,A)') ' Monitoring obs #',obs,'/',nused,' Exec query="'//trim(queryname)//'"'
      rc = ODB_select(h,queryname,nrows,ncols,nra=nra,poolno=-1,&
                     setvars=varnames,values=vars)
      IF (nrows > 0) THEN
      ALLOCATE(x(nra,0:ncols))
      rc = ODB_get(h,queryname,x,nrows,ncols,poolno=-1)
      DO jr=1,nrows

        !write(*,'(1p,(5x,10(1x,g10.2)))') x(jr,1:ncols)

        IF ( ncols /= 26 ) THEN
          WRITE(*,*) ' Inconsistency in NCOLS found and expected:',ncols
          CALL abort
        ENDIF
        ! Assign odb extract to module variables
        iobtyp_odb(1)=int(x(jr,1))
        icodetype_odb(1)=int(x(jr,2))
        !WRITE(cstaid_odb(1),'(F8.0)') x(jr,3)
        WRITE(cstaid_odb(1),FMT=pstring%fmt) x(jr,3)
        ivarno_odb(1)=int(x(jr,4))
        rlat_odb(1)=x(jr,5)
        rlon_odb(1)=x(jr,6)
        IF ( lradians ) THEN
          rlat_odb(1)=180.*rlat_odb(1)/pi
          rlon_odb(1)=180.*rlon_odb(1)/pi
        ENDIF
        ivertco_type_odb(1)=int(x(jr,7))
        rpress_odb(1)=x(jr,8)
        isensor_odb(1)=int(x(jr,9))
        idate_odb(1)=int(x(jr,10))
        itime_odb(1)=int(x(jr,11))
        istatus_acthdr_odb(1)=int(x(jr,12))
        istatus_blkhdr_odb(1)=int(x(jr,13))
        istatus_pashdr_odb(1)=int(x(jr,14))
        istatus_rejhdr_odb(1)=int(x(jr,15))
        istatus_actbod_odb(1)=int(x(jr,16))
        istatus_blkbod_odb(1)=int(x(jr,17))
        istatus_pasbod_odb(1)=int(x(jr,18))
        istatus_rejbod_odb(1)=int(x(jr,19))
        ianflag_odb(1)=x(jr,20)
        randep_odb(1)=x(jr,21)
        rfgdep_odb(1)=x(jr,22)
        robs_odb(1)=x(jr,23)
        rfinoe_odb(1)=x(jr,24)
        biascrl_odb(1)=x(jr,25)
        lsm_odb(1)=x(jr,26)

        ! Update statistics
        if ( lstat ) CALL update_statistics(obs,1)

        ! Print usage files
        IF ( lusage ) THEN
          CALL print_usage_files(obs,1,cdtg)
        ENDIF

        iobs=iobs+1
        totobs=totobs+1
      ENDDO
      DEALLOCATE(x)
    ENDIF
    rc = ODB_cancel(h,queryname,poolno=-1)

    ! Closing the usage file
    IF ( lusage ) THEN
      IF ( obs == nused ) CALL close_usage_files()
    ENDIF

    ! Make means and RMS and save for this observation 
    if ( lstat ) CALL make_means_and_rms(obs)

    ! Write output for this obs
    if ( lstat ) CALL write_output(cdtg,obs)

    IF ( verbose > 0 ) THEN
      WRITE(formatstr,'(A,I2,A)') '(A,',nvar,'(A,A,I8,A),A,A,A,I8)'
      WRITE(*,formatstr) ' ',(trim(varnames(i)),'=',int(vars(i)),' ',i=1,nvar),' => Number of obs from ',trim(dbname),' are:',iobs
    ENDIF
    DEALLOCATE(varnames,vars)
  END DO
 
  ! De-allocate arrays for odb extract
  CALL dealloc_odb_extracts()
  rc = ODB_close(h)
  rc = ODB_end()

  IF ( verbose > 0 ) THEN
   WRITE(*,*) 'Monitored a total number of ',totobs,' observations!'
   CALL cpu_time(cpu)
    WRITE(*,*) 'End processing ODB for '//cdtg//'.  CPU time elapsed: ',cpu
  ENDIF

#else
  WRITE(*,*) "Filetype ODB is not implemented yet"
  CALL ABORT()
#endif
END SUBROUTINE odb_extract 
