MODULE module_obsmon
#ifdef ODB_MONITOR
  USE sqlite,   ONLY      : SQLITE_DATABASE
#endif
  IMPLICIT NONE
  CHARACTER(LEN=10)        :: filetype
  CHARACTER(LEN=10)        :: odbbase
  INTEGER                  :: verbose
  INTEGER                  :: dtgstart,dtgend,ntimes,fcint
  LOGICAL                  :: lall
  LOGICAL                  :: lsynop
  LOGICAL                  :: lsynop_z
  LOGICAL                  :: lsynop_t2m
  LOGICAL                  :: lsynop_rh2m
  LOGICAL                  :: lsynop_td2m
  LOGICAL                  :: lsynop_u10m
  LOGICAL                  :: lsynop_v10m
  LOGICAL                  :: lsynop_snow
  LOGICAL                  :: lsynop_apd
  LOGICAL                  :: lship
  LOGICAL                  :: lship_z
  LOGICAL                  :: lship_t2m
  LOGICAL                  :: lship_td2m
  LOGICAL                  :: lship_rh2m
  LOGICAL                  :: lship_u10m
  LOGICAL                  :: lship_v10m
  LOGICAL                  :: lmetar
  LOGICAL                  :: lmetar_z
  LOGICAL                  :: ldribu
  LOGICAL                  :: ldribu_z
  LOGICAL                  :: ltemp
  LOGICAL                  :: ltemp_t
  LOGICAL                  :: ltemp_u
  LOGICAL                  :: ltemp_v
  LOGICAL                  :: ltemp_q
  LOGICAL                  :: laircraft
  LOGICAL                  :: laircraft_u
  LOGICAL                  :: laircraft_v
  LOGICAL                  :: laircraft_t
  LOGICAL                  :: lpilot
  LOGICAL                  :: lpilot_u
  LOGICAL                  :: lpilot_v
  LOGICAL                  :: latovs
  LOGICAL                  :: lamsu
  LOGICAL                  :: lamsua
  LOGICAL                  :: lamsub
  LOGICAL                  :: lmhs
  LOGICAL                  :: liasi
  LOGICAL                  :: lscatt
  LOGICAL                  :: lscatt_u10m
  LOGICAL                  :: lscatt_v10m
  LOGICAL                  :: lradar,lradarv,lradardbz,lradarrh
  CHARACTER(LEN=10),ALLOCATABLE,DIMENSION(:) :: cdate(:)
  LOGICAL                  :: lstat
  LOGICAL                  :: lalloc_all
  LOGICAL                  :: lusage
#ifdef ODB_MONITOR
  TYPE(SQLITE_DATABASE),SAVE :: db_usage
#endif

  NAMELIST / OBSMON / verbose,odbbase,filetype,lstat,lusage,&
                      lall, &
                      lsynop, lsynop_z, &
                      lship , lship_z,  &
                      ldribu, ldribu_z, &
                      lmetar, lmetar_z, &
                      ltemp , ltemp_t,  ltemp_u,  ltemp_v, ltemp_q, &
                      laircraft, laircraft_u, laircraft_v, laircraft_t, &
                      lpilot, lpilot_u, lpilot_v, &
                      latovs, &
                      lamsu,lamsua,lamsub, &
                      lmhs, &
                      liasi, &
                      lscatt, lscatt_u10m, lscatt_v10m, &
                      lradar,lradarv,lradardbz,lradarrh

  CONTAINS

  SUBROUTINE init_obsmon()
    USE module_obstypes, ONLY : alloc_obs,init_amsua,init_amsub,init_iasi,init_conv_surf,init_conv_vert,init_mhs,nused, &
                                init_radar,init_scatt
    IMPLICIT NONE
    LOGICAL                  :: dry
    INTEGER                  :: i

    CALL set_default_values()
    CALL read_obsmon_namelist()
    IF ( lusage ) THEN
      lalloc_all=.TRUE.
    ENDIF

    IF (( .NOT. lusage ) .AND. ( .NOT. lstat )) THEN
      WRITE(*,*) 'You must enable some statistics generation for this program to be meaningfull!'
      CALL ABORT()
    ENDIF

    dry=.TRUE.
    DO i=1,2
      nused=1
      IF ( i == 2 ) THEN
        dry=.FALSE.
      ENDIF

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! SYNOPs
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF ( ( lall ) .OR. ( lsynop ) ) THEN
        ! SYNOP Z
        IF ( ( lall ) .OR. ( lsynop ) .OR. ( lsynop_z) ) THEN
          CALL init_conv_surf(dry,"synop_z")
        ENDIF
        ! SYNOP T2M
        IF ( ( lall ) .OR. ( lsynop ) .OR. ( lsynop_t2m) ) THEN
          CALL init_conv_surf(dry,"synop_t2m")
        ENDIF
        ! SYNOP TD2M
        IF ( ( lall ) .OR. ( lsynop ) .OR. ( lsynop_td2m) ) THEN
          CALL init_conv_surf(dry,"synop_td2m")
        ENDIF
        ! SYNOP RH2M
        IF ( ( lall ) .OR. ( lsynop ) .OR. ( lsynop_rh2m) ) THEN
          CALL init_conv_surf(dry,"synop_rh2m")
        ENDIF
        ! SYNOP U10M
        IF ( ( lall ) .OR. ( lsynop ) .OR. ( lsynop_u10m) ) THEN
          CALL init_conv_surf(dry,"synop_u10m")
        ENDIF
        ! SYNOP V10M
        IF ( ( lall ) .OR. ( lsynop ) .OR. ( lsynop_v10m) ) THEN
          CALL init_conv_surf(dry,"synop_v10m")
        ENDIF
        ! SYNOP SNOW
        IF ( ( lall ) .OR. ( lsynop ) .OR. ( lsynop_snow) ) THEN
          CALL init_conv_surf(dry,"synop_snow")
        ENDIF
        IF ( ( lall ) .OR. ( lsynop ) .OR. ( lsynop_apd) ) THEN
          CALL init_conv_surf(dry,"synop_apd")
        ENDIF
      ENDIF
 
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! SHIPs
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF ( ( lall ) .OR. ( lship ) ) THEN
        ! SHIP Z
        IF ( ( lall ) .OR. ( lship ) .OR. ( lship_z) ) THEN
          CALL init_conv_surf(dry,"ship_z")
        ENDIF
        ! SHIP T2M
        IF ( ( lall ) .OR. ( lship ) .OR. ( lship_t2m) ) THEN
          CALL init_conv_surf(dry,"ship_t2m")
        ENDIF
        ! SHIP TD2M
        IF ( ( lall ) .OR. ( lship ) .OR. ( lship_td2m) ) THEN
          CALL init_conv_surf(dry,"ship_td2m")
        ENDIF
        ! SHIP RH2M
        IF ( ( lall ) .OR. ( lship ) .OR. ( lship_rh2m) ) THEN
          CALL init_conv_surf(dry,"ship_rh2m")
        ENDIF
        ! SHIP U10M
        IF ( ( lall ) .OR. ( lship ) .OR. ( lship_u10m) ) THEN
          CALL init_conv_surf(dry,"ship_u10m")
        ENDIF
        ! SHIP V10M
        IF ( ( lall ) .OR. ( lship ) .OR. ( lship_v10m) ) THEN
          CALL init_conv_surf(dry,"ship_v10m")
        ENDIF
      ENDIF

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! DRIBUs
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF ( ( lall ) .OR. ( ldribu ) ) THEN
        ! DRIBU Z
        IF ( ( lall ) .OR. ( ldribu ) .OR. ( ldribu_z) ) THEN
          CALL init_conv_surf(dry,"dribu_z")
        ENDIF
      ENDIF

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! METARs
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF ( ( lall ) .OR. ( lmetar ) ) THEN
        ! METAR Z
        IF ( ( lall ) .OR. ( lmetar ) .OR. ( lmetar_z) ) THEN
          CALL init_conv_surf(dry,"metar_z")
        ENDIF
      ENDIF

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! TEMPs
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF ( ( lall ) .OR. ( ltemp ) ) THEN
        CALL init_conv_vert(dry,"temp")
      ENDIF

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! aircraftS
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF ( ( lall ) .OR. ( laircraft ) ) THEN
        CALL init_conv_vert(dry,"aircraft")
      ENDIF
     

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! PILOT
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       IF ( ( lall ) .OR. ( lpilot ) ) THEN
        CALL init_conv_vert(dry,"pilot")
      ENDIF

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! ATOVS
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! AMSU A 
      IF ( ( lall ) .OR. ( latovs ) .OR. ( lamsu ) .OR. (lamsua) ) THEN
        CALL init_amsua(dry)
      ENDIF

      ! AMSU B
      IF ( ( lall ) .OR. ( latovs ) .OR. ( lamsu ) .OR. (lamsub) ) THEN
        CALL init_amsub(dry)
      ENDIF

      ! MHS 
      IF ( ( lall ) .OR. ( latovs ) .OR. ( lamsu ) .OR. (lmhs) ) THEN
        CALL init_mhs(dry)
      ENDIF

      ! IASI
      IF ( ( lall ) .OR. ( latovs ) .OR. ( liasi ) ) THEN
        CALL init_iasi(dry)
      ENDIF

      ! Scatterometer
      IF ( ( lall ) .OR. ( lscatt ) .OR. ( lscatt_u10m ) ) THEN
        CALL init_scatt(dry,"u10m")
      ENDIF

      ! Scatterometer
      IF ( ( lall ) .OR. ( lscatt ) .OR. ( lscatt_v10m) ) THEN
        CALL init_scatt(dry,"v10m")
      ENDIF

      ! RADAR
      IF ( ( lall ) .OR. ( lradar ) .OR. ( lradarv )) THEN
        CALL init_radar(dry,"radarv")
      ENDIF
      IF ( ( lall ) .OR. ( lradar ) .OR. ( lradardbz )) THEN
        CALL init_radar(dry,"radardbz")
      ENDIF
      IF ( ( lall ) .OR. ( lradar ) .OR. ( lradarrh )) THEN
        CALL init_radar(dry,"radarrh")
      ENDIF


      nused=nused-1
      IF ( dry ) THEN
        IF ( nused == 0 ) THEN
          WRITE(*,*) 'No observation types selected'
          CALL ABORT
        ENDIF
        WRITE(*,*) 'Monitoring ',nused,' number of observations'
        CALL alloc_obs(verbose)
      ENDIF
    ENDDO
  END SUBROUTINE init_obsmon

  SUBROUTINE finalize_obsmon()
    USE module_obstypes, ONLY : dealloc_obs
    IMPLICIT NONE

    CALL dealloc_obs()
  END SUBROUTINE finalize_obsmon

  SUBROUTINE process_odb()
    IMPLICIT NONE
    CHARACTER(LEN=10) :: cdtg 
    INTEGER           :: dtg,base_date,base_time,valid_date,valid_time
    INTEGER           :: yyyy,mm,dd
    INTEGER           :: hh,mn
    INTEGER           :: i,itime

    DO i=1,2 
      ntimes=0 
      dtg=dtgstart

      WRITE(cdtg(1:10),'(I10.10)') dtg
      DO WHILE ( dtg <= dtgend )
        ntimes=ntimes+1
        IF ( i == 2 ) THEN
          WRITE(*,*) cdtg
          cdate(ntimes)=cdtg
        ENDIF
 
        READ(cdtg(1:4), '(I4.4)') yyyy 
        READ(cdtg(5:6), '(I2.2)') mm
        READ(cdtg(7:8), '(I2.2)') dd
        READ(cdtg(9:10),'(I2.2)') hh
        mn=0
        base_date=yyyy*10000+mm*100+dd
        base_time=hh*10000+mn*100
        CALL adddtg(base_date,base_time,fcint*3600,valid_date,valid_time)
        WRITE(cdtg(1:8),'(I8.8)') valid_date
        hh=valid_time/10000
        WRITE(cdtg(9:10),'(I2.2)') hh
        READ(cdtg(1:10),'(I10.10)') dtg
      ENDDO 
      IF ( i == 1 ) THEN
        WRITE(*,*) 'NTIMES: ',ntimes
        ALLOCATE(cdate(ntimes))
      ENDIF
    ENDDO

    DO itime=1,ntimes
      SELECT CASE (trim(filetype))
        CASE ("ODB")
          CALL odb_extract(cdate(itime))
        CASE ("MANDALAY")
          CALL read_mandalay_extract(cdate(itime))
        CASE DEFAULT
          WRITE(*,*) "Filetype ",trim(filetype)," not implemented"
          CALL ABORT
      END SELECT

      WRITE(*,*) 'Finished processing odb '//cdate(itime)
    ENDDO

    DEALLOCATE(cdate)
  END SUBROUTINE process_odb

  SUBROUTINE set_default_values
    IMPLICIT NONE

    filetype        = "ODB"
    odbbase         = "ccma"
    lalloc_all      = .TRUE.
    lstat           = .TRUE.
    lusage          = .FALSE.
    lall            = .FALSE.
    lsynop          = .FALSE.
    lsynop_z        = .FALSE.
    lsynop_t2m      = .FALSE.
    lsynop_td2m     = .FALSE.
    lsynop_rh2m     = .FALSE.
    lsynop_u10m     = .FALSE.
    lsynop_v10m     = .FALSE.
    lsynop_snow     = .FALSE.
    lship           = .FALSE.
    lship_z         = .FALSE.
    lship_t2m       = .FALSE.
    lship_td2m      = .FALSE.
    lship_rh2m      = .FALSE.
    lship_u10m      = .FALSE.
    lship_v10m      = .FALSE.
    lmetar          = .FALSE.
    lmetar_z        = .FALSE.
    ldribu          = .FALSE.
    ldribu_z        = .FALSE.
    ltemp           = .FALSE.
    ltemp_t         = .FALSE.
    ltemp_u         = .FALSE.
    ltemp_v         = .FALSE.
    ltemp_q         = .FALSE.
    laircraft       = .FALSE.
    laircraft_u     = .FALSE.
    laircraft_v     = .FALSE.
    laircraft_t     = .FALSE.
    lpilot          = .FALSE.
    lpilot_u        = .FALSE.
    lpilot_v        = .FALSE.
    lamsu           = .FALSE.
    lamsua          = .FALSE.
    lamsub          = .FALSE.
    lmhs            = .FALSE.
    liasi           = .FALSE.
    lscatt          = .FALSE.
    lscatt_u10m     = .FALSE.
    lscatt_v10m     = .FALSE.
    lradar          = .FALSE.
    lradarv         = .FALSE.
    lradardbz       = .FALSE.
    lradarrh        = .FALSE.


  END SUBROUTINE set_default_values

  SUBROUTINE write_output(cdtg,ind)
    USE module_obstypes, ONLY      : all_obs,nused
    USE module_odb_extract, ONLY   : istatus
#ifdef ODB_MONITOR
    USE sqlite,          ONLY      : SQLITE_DATABASE,SQLITE_STATEMENT,SQLITE_COLUMN,&
                                     SQLITE_CHAR,SQLITE_INT,SQLITE_REAL,&
                                     sqlite3_open,sqlite3_column_props,sqlite3_error,sqlite3_errmsg,&
                                     sqlite3_begin,sqlite3_set_column,sqlite3_commit,sqlite3_create_table,&
                                     sqlite3_insert,sqlite3_column_query,sqlite3_prepare_select,sqlite3_next_row,&
                                     sqlite3_get_column,sqlite3_close
#endif
    IMPLICIT NONE
    CHARACTER(LEN=10),INTENT(in)               :: cdtg
    INTEGER,INTENT(IN),OPTIONAL                :: ind
    INTEGER                                    :: obs,ios,i,level,find,lind
    CHARACTER(LEN=80)                          :: outfile_stat
    CHARACTER(LEN=5)                           :: ch
    CHARACTER(LEN=10)                          :: obname,satname
    INTEGER                                    :: dtg
#ifdef ODB_MONITOR
    TYPE(SQLITE_DATABASE)                      :: db
    TYPE(SQLITE_COLUMN), DIMENSION(:), POINTER :: column
#endif
    CHARACTER(LEN=40),ALLOCATABLE,DIMENSION(:) :: colname
    CHARACTER(LEN=5)                           :: loc_type
    INTEGER                                    :: ii,offset,ncols

    read(cdtg,'(I10.10)') dtg
    IF (PRESENT(ind)) THEN
      find=ind
      lind=ind
    ELSE
      find=1
      lind=nused
    ENDIF

    ! Write output to file for this time and save data
    DO obs=find,lind
      IF ( lstat ) THEN

        IF ( obs == 1 ) CALL SYSTEM("mkdir "//cdtg)

        ! Open the file
        IF ( all_obs(obs)%lsat ) THEN
          WRITE(ch,'(I5)') all_obs(obs)%sensor%channel
          ch = adjustl(ch)
          outfile_stat=cdtg//"/"//TRIM(odbbase)//"_stat_"//TRIM(all_obs(obs)%sensor%name)//"_"//&
                                          TRIM(all_obs(obs)%sensor%satelite)//"_"//TRIM(ch)//""
          level=all_obs(obs)%sensor%channel
          obname=TRIM(all_obs(obs)%sensor%name)
          satname=TRIM(all_obs(obs)%sensor%satelite)
        ELSE
          outfile_stat=cdtg//"/"//TRIM(odbbase)//"_stat_"//TRIM(all_obs(obs)%name)//"_"//TRIM(all_obs(obs)%var%fname)//""
          level=all_obs(obs)%var%level
          obname=TRIM(all_obs(obs)%name)
          satname="undefined"
        ENDIF

#ifdef ODB_MONITOR
        ! Open SQLITE DB
        call sqlite3_open( TRIM(outfile_stat)//'.db', db )

        ! Define columns
        offset=7
        ncols=11
        allocate(column((ncols*3)+offset))
        allocate(colname((ncols*3)+offset))
        colname(1)="DTG"
        colname(2)="obnumber"
        colname(3)="obname"
        colname(4)="satname"
        colname(5)="varname"
        colname(6)="level"
        colname(7)="passive"
        do ii=1,3
          i=(ii-1)*ncols
          select case (ii)
             case (1)
               loc_type="total"
             case (2)
               loc_type="land"
             case (3)
               loc_type="sea"
          end select
          colname(i+offset+ 1)="nobs_"//trim(loc_type)
          colname(i+offset+ 2)="fg_bias_"//trim(loc_type)
          colname(i+offset+ 3)="fg_abs_bias_"//trim(loc_type)
          colname(i+offset+ 4)="fg_rms_"//trim(loc_type)
          colname(i+offset+ 5)="fg_dep_"//trim(loc_type)
          colname(i+offset+ 6)="fg_uncorr_"//trim(loc_type)
          colname(i+offset+ 7)="bc_"//trim(loc_type)
          colname(i+offset+ 8)="an_bias_"//trim(loc_type)
          colname(i+offset+ 9)="an_abs_bias_"//trim(loc_type)
          colname(i+offset+10)="an_rms_"//trim(loc_type)
          colname(i+offset+11)="an_dep_"//trim(loc_type)
        enddo

        call sqlite3_column_props( column(1), trim(colname(1)), SQLITE_INT )
        if(sqlite3_error(db)) write(*,*) 'WARNING: sqlite3_column_props '//TRIM(sqlite3_errmsg(db))
        call sqlite3_column_props( column(2), trim(colname(2)), SQLITE_INT )
        if(sqlite3_error(db)) write(*,*) 'WARNING: sqlite3_column_props '//TRIM(sqlite3_errmsg(db))
        call sqlite3_column_props( column(3), trim(colname(3)), SQLITE_CHAR )
        if(sqlite3_error(db)) write(*,*) 'WARNING: sqlite3_column_props '//TRIM(sqlite3_errmsg(db))
        call sqlite3_column_props( column(4), trim(colname(4)), SQLITE_CHAR )
        if(sqlite3_error(db)) write(*,*) 'WARNING: sqlite3_column_props '//TRIM(sqlite3_errmsg(db))
        call sqlite3_column_props( column(5), trim(colname(5)), SQLITE_CHAR )
        if(sqlite3_error(db)) write(*,*) 'WARNING: sqlite3_column_props '//TRIM(sqlite3_errmsg(db))
        call sqlite3_column_props( column(6), trim(colname(6)), SQLITE_INT )
        if(sqlite3_error(db)) write(*,*) 'WARNING: sqlite3_column_props '//TRIM(sqlite3_errmsg(db))
        call sqlite3_column_props( column(7), trim(colname(7)), SQLITE_INT )
        if(sqlite3_error(db)) write(*,*) 'WARNING: sqlite3_column_props '//TRIM(sqlite3_errmsg(db))
        do i=offset+1,(ncols*3)+offset
          call sqlite3_column_props( column(i), trim(colname(i)), SQLITE_REAL )
          if(sqlite3_error(db)) write(*,*) 'WARNING: sqlite3_column_props '//TRIM(sqlite3_errmsg(db))
        enddo

        ! Create table
        call sqlite3_create_table( db, 'obsmon', column )
        if(sqlite3_error(db)) write(*,*) 'WARNING: sqlite3_create_table '//TRIM(sqlite3_errmsg(db))

        ! Insert the values into the table.
        call sqlite3_begin( db )
        call sqlite3_set_column( column(1), dtg )
        if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
        call sqlite3_set_column( column(2), all_obs(obs)%obnumber )
        if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
        call sqlite3_set_column( column(3), TRIM(obname) )
        if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
        call sqlite3_set_column( column(4), TRIM(satname) )
        if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
        call sqlite3_set_column( column(5), TRIM(all_obs(obs)%var%name) )
        if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
        call sqlite3_set_column( column(6), level )
        if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
        call sqlite3_set_column( column(7), istatus(3) )
        if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
        do ii=1,3
          i=(ii-1)*ncols
          call sqlite3_set_column( column(i+offset+1),REAL(all_obs(obs)%var%count(ii)))
          if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
          call sqlite3_set_column( column(i+offset+2),all_obs(obs)%var%fg_bias(ii))
          if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
          call sqlite3_set_column( column(i+offset+3),all_obs(obs)%var%fg_abs_bias(ii))
          if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
          call sqlite3_set_column( column(i+offset+4),all_obs(obs)%var%fg_rms(ii))
          if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
          call sqlite3_set_column( column(i+offset+5),all_obs(obs)%var%fg_dep(ii))
          if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
          call sqlite3_set_column( column(i+offset+6),all_obs(obs)%var%fg_uncorr(ii))
          if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
          call sqlite3_set_column( column(i+offset+7),all_obs(obs)%var%bc(ii))
          if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
          call sqlite3_set_column( column(i+offset+8),all_obs(obs)%var%an_bias(ii))
          if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
          call sqlite3_set_column( column(i+offset+9),all_obs(obs)%var%an_abs_bias(ii))
          if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
          call sqlite3_set_column( column(i+offset+10),all_obs(obs)%var%an_rms(ii))
          if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
          call sqlite3_set_column( column(i+offset+11),all_obs(obs)%var%an_dep(ii))
          if(sqlite3_error(db)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db)))
        enddo
        call sqlite3_insert( db, 'obsmon', column )
        if(sqlite3_error(db)) CALL sql_error('sqlite3_insert '//TRIM(sqlite3_errmsg(db)))

        call sqlite3_commit( db )
        if(sqlite3_error(db)) CALL sql_error('sqlite3_commit '//TRIM(sqlite3_errmsg(db)))

        call sqlite3_close( db )

        deallocate(column)
        deallocate(colname)
#endif
      ENDIF
    ENDDO
  END SUBROUTINE write_output

  SUBROUTINE read_obsmon_namelist
    IMPLICIT NONE
    INTEGER   :: ios

    OPEN(4,IOSTAT=ios)
    IF ( ios == 0 ) THEN
      READ(4,NML=OBSMON,IOSTAT=ios)
      IF ( ios /= 0 ) THEN
        WRITE(*,*) "Can not read namelist from unit 4."
        WRITE(*,obsmon)
        CALL ABORT
      ENDIF
      IF ( VERBOSE > 0 ) WRITE(*,*) 'Namelist obsmon read from unit 4'
      IF ( VERBOSE > 5 ) WRITE(*,obsmon)
      CLOSE(4)
    ENDIF
  END SUBROUTINE read_obsmon_namelist

  FUNCTION IDAT2C(kd)
    IMPLICIT NONE
    INTEGER :: kd
    INTEGER :: iy,ir,im,id,idat2c
    ! for given kd(yyyymmdd) return day number since 19000101
    
    iy=kd/10000
    ir=kd-iy*10000
    if(iy.lt.100)iy=iy+1900
    im=ir/100+1
    id=ir-im*100 
    
    if(im.le.3)then
      iy=iy-1
      im=im+12
    endif
    
    ir=iy/100

    idat2c=365*iy-693923+iy/4-ir+ir/4+int(30.6001*im)+id
  END FUNCTION

  FUNCTION IC2DAT(kd)
    IMPLICIT NONE
    ! converts julian daynumber since 19000101 into gregorian yyyymmdd
    INTEGER,INTENT(IN) :: kd
    INTEGER            :: ic2dat
    INTEGER            :: iy, im, id, l, n
    
    L  = KD + 68569 + 2415020
    N  = 4*L / 146097 
    L  = L - ( 146097*N + 3 ) / 4
    IY = 4000 * ( L+1 ) / 1461001 
    L  = L - 1461 * IY / 4 + 31
    IM = 80 * L / 2447
    ID = L - 2447 * IM / 80
    L  = IM / 11
    IM = IM + 2 - 12 * L
    IY = 100 * ( N- 49 ) + IY + L
    ic2dat=10000*iy+im*100+id
  END FUNCTION

  SUBROUTINE ADDDTG(kdi,kti,kfp,kd,kt)
    IMPLICIT NONE
    !returns new date yyyymmdd and time hhmmss by adding kfp (seconds,
    !may be negative) to old values
    INTEGER,INTENT(IN)  :: kdi,kti,kfp
    INTEGER,INTENT(OUT) :: kd,kt
    INTEGER,PARAMETER   :: ispd=86400
    INTEGER             :: ic,id,ih,ir,im,is

    ! convert to century date
    ic=idat2c(kdi)

    ! split kfp into days and seconds
    id=kfp/ispd
    if(kfp.lt.0)id=id-1
    is=kfp-id*ispd

    ! decode kti into seconds, and add to seconds from kti
    ih=kti/10000
    ir=kti-ih*10000
    im=ir/100
    is=is + ir-im*100 + im*60 + ih*3600

    ! split seconds into days and seconds
    ir=is/ispd
    id=id+ir
    is=is-ir*ispd

    ! add days, convert to yyyymmdd format
    ic=ic+id
    kd=ic2dat(ic)

    ! convert seconds to hhmmss format
    ih=is/3600
    ir=is-ih*3600
    im=ir/60
    is=ir-im*60
    kt=ih*10000 + im*100 + is
  END SUBROUTINE

  SUBROUTINE open_usage_files(obs,cdtg)
#ifdef ODB_MONITOR
    USE sqlite,          ONLY     : SQLITE_COLUMN,SQLITE_CHAR,SQLITE_INT,SQLITE_REAL,&
                                    sqlite3_open,sqlite3_column_props,sqlite3_error,sqlite3_errmsg,&
                                    sqlite3_begin,sqlite3_set_column,sqlite3_create_table
#endif
    USE module_obstypes, ONLY     : all_obs
    IMPLICIT NONE
    INTEGER, INTENT(IN)                        :: obs
    CHARACTER(LEN=10),INTENT(IN)               :: cdtg
    CHARACTER(LEN=8)                           :: cymd
    CHARACTER(LEN=5)                           :: ch
    CHARACTER(LEN=80)                          :: outfile_usage,outfile_usage_stat
    INTEGER                                    :: ios,dtg
#ifdef ODB_MONITOR
    TYPE(SQLITE_COLUMN), DIMENSION(:), POINTER :: column
#endif
    CHARACTER(LEN=40),ALLOCATABLE,DIMENSION(:) :: colname
    INTEGER                                    :: i

    ! Opening the files
    IF ( lusage ) THEN
     read(cdtg,'(I10.10)') dtg
      IF ( all_obs(obs)%lsat ) THEN
        WRITE(ch,'(I5)') all_obs(obs)%sensor%channel
        ch = adjustl(ch)
        outfile_usage=cdtg//"/"//TRIM(odbbase)//"_map_usage_"//TRIM(all_obs(obs)%sensor%name)//&
                            "_"//TRIM(all_obs(obs)%sensor%satelite)//"_"//TRIM(ch)
      ELSE
        outfile_usage=cdtg//"/"//TRIM(odbbase)//"_map_usage_"//TRIM(all_obs(obs)%name)//&
                            "_"//TRIM(all_obs(obs)%var%name)
      ENDIF

#ifdef ODB_MONITOR
      ! Open SQLITE DB
      call sqlite3_open( TRIM(outfile_usage)//'.db', db_usage )

      ! Define columns
      allocate(column(18))
      allocate(colname(18))
      colname(1)="DTG"
      colname(2)="obnumber"
      colname(3)="obname"
      colname(4)="satname"
      colname(5)="varname"
      colname(6)="level"
      colname(7)="latitude"
      colname(8)="longitude"
      colname(9)="statid"
      colname(10)="obsvalue"
      colname(11)="fg_dep"
      colname(12)="an_dep"
      colname(13)="biascrl"
      colname(14)="active"
      colname(15)="rejected"
      colname(16)="passive"
      colname(17)="blacklisted"
      colname(18)="anflag"

      call sqlite3_column_props( column(1), trim(colname(1)), SQLITE_INT )
      if(sqlite3_error(db_usage)) write(*,*) 'ERROR: sqlite3_column_props '//TRIM(sqlite3_errmsg(db_usage))
      call sqlite3_column_props( column(2), trim(colname(2)), SQLITE_INT )
      if(sqlite3_error(db_usage)) write(*,*) 'ERROR: sqlite3_column_props '//TRIM(sqlite3_errmsg(db_usage))
      do i=3,5
        call sqlite3_column_props( column(i), trim(colname(i)), SQLITE_CHAR )
        if(sqlite3_error(db_usage)) write(*,*) 'ERROR: sqlite3_column_props '//TRIM(sqlite3_errmsg(db_usage))
      enddo
      call sqlite3_column_props( column(6), trim(colname(6)), SQLITE_INT )
      if(sqlite3_error(db_usage)) write(*,*) 'ERROR: sqlite3_column_props '//TRIM(sqlite3_errmsg(db_usage))
      do i=7,8
        call sqlite3_column_props( column(i), trim(colname(i)), SQLITE_REAL )
        if(sqlite3_error(db_usage)) write(*,*) 'ERROR: sqlite3_column_props '//TRIM(sqlite3_errmsg(db_usage))
      enddo
      call sqlite3_column_props( column(9), trim(colname(9)), SQLITE_CHAR )
      if(sqlite3_error(db_usage)) write(*,*) 'ERROR: sqlite3_column_props '//TRIM(sqlite3_errmsg(db_usage))
      do i=10,13
        call sqlite3_column_props( column(i), trim(colname(i)), SQLITE_REAL )
        if(sqlite3_error(db_usage)) write(*,*) 'ERROR: sqlite3_column_props '//TRIM(sqlite3_errmsg(db_usage))
      enddo
      do i=14,18
        call sqlite3_column_props( column(i), trim(colname(i)), SQLITE_INT )
        if(sqlite3_error(db_usage)) write(*,*) 'ERROR: sqlite3_column_props '//TRIM(sqlite3_errmsg(db_usage))
      enddo

      ! Create table
      call sqlite3_create_table( db_usage, 'usage', column )
      if(sqlite3_error(db_usage)) write(*,*) 'ERROR: sqlite3_create_table '//TRIM(sqlite3_errmsg(db_usage))
      CALL sqlite3_begin( db_usage )
      DEALLOCATE(column)
      DEALLOCATE(colname)
#endif
    ENDIF
  END SUBROUTINE open_usage_files

  SUBROUTINE close_usage_files()
#ifdef ODB_MONITOR
    USE sqlite,             ONLY : sqlite3_close,sqlite3_commit,sqlite3_error,sqlite3_errmsg
    IMPLICIT NONE

    ! Closing the files
    IF ( lusage ) THEN
      CALL sqlite3_commit( db_usage )
      IF(sqlite3_error(db_usage)) CALL sql_error('ERROR: sqlite3_commit '//TRIM(sqlite3_errmsg(db_usage)))
      call sqlite3_close( db_usage ) 
    ENDIF
#endif
  END SUBROUTINE close_usage_files

  SUBROUTINE make_means_and_rms(ind)
    USE module_obstypes, ONLY : all_obs,nused
    IMPLICIT NONE
    INTEGER,INTENT(IN),OPTIONAL  :: ind
    INTEGER                      :: obs,i,ostart,oend,count

    IF (PRESENT(ind)) THEN
      ostart=ind
      oend=ind
    ELSE
      ostart=1
      oend=nused
    ENDIF

    DO obs=ostart,oend
      DO i=1,3
        count=REAL(all_obs(obs)%var%count(i))
        IF ( count > 0. ) THEN
          all_obs(obs)%var%fg_bias(i)     = all_obs(obs)%var%fg_bias(i)/count
          all_obs(obs)%var%fg_abs_bias(i) = all_obs(obs)%var%fg_abs_bias(i)/count
          all_obs(obs)%var%fg_rms(i)      = SQRT(all_obs(obs)%var%fg_rms(i)/count)
          all_obs(obs)%var%fg_dep(i)      = all_obs(obs)%var%fg_dep(i)/count
          all_obs(obs)%var%fg_uncorr(i)   = all_obs(obs)%var%fg_uncorr(i)/count
          all_obs(obs)%var%bc(i)          = all_obs(obs)%var%bc(i)/count
          all_obs(obs)%var%an_bias(i)     = all_obs(obs)%var%an_bias(i)/count
          all_obs(obs)%var%an_abs_bias(i) = all_obs(obs)%var%an_abs_bias(i)/count
          all_obs(obs)%var%an_rms(i)      = SQRT(all_obs(obs)%var%an_rms(i)/count)
          all_obs(obs)%var%an_dep(i)      = all_obs(obs)%var%an_dep(i)/count
        ELSE
          all_obs(obs)%var%fg_bias(i)     =  0.
          all_obs(obs)%var%fg_abs_bias(i) =  0.
          all_obs(obs)%var%fg_rms(i)      =  0.
          all_obs(obs)%var%fg_dep(i)      =  0.
          all_obs(obs)%var%fg_uncorr(i)   =  0.
          all_obs(obs)%var%bc(i)          =  0.
          all_obs(obs)%var%an_bias(i)     =  0.
          all_obs(obs)%var%an_abs_bias(i) =  0.
          all_obs(obs)%var%an_rms(i)      =  0.
          all_obs(obs)%var%an_dep(i)      =  0.
        ENDIF
      ENDDO
    ENDDO
  END SUBROUTINE make_means_and_rms

  SUBROUTINE update_statistics(obs,i)
    USE module_obstypes, ONLY    : all_obs
    USE module_odb_extract, ONLY : cstaid_odb,iobtyp_odb,icodetype_odb,ivarno_odb,ivertco_type_odb,&
                                   isensor_odb,idate_odb,itime_odb,istatus_acthdr_odb,istatus_blkhdr_odb,&
                                   istatus_pashdr_odb,istatus_rejhdr_odb,istatus_actbod_odb,istatus_blkbod_odb,&
                                   istatus_pasbod_odb,istatus_rejbod_odb,rlat_odb,rlon_odb,rpress_odb,randep_odb,&
                                   rfgdep_odb,robs_odb,rfinoe_odb,biascrl_odb,lsm_odb,minlat,minlon,maxlon,maxlat,&
                                   rlat,rlon,istatus,ianflag_odb
    IMPLICIT NONE
    INTEGER,INTENT(IN)          :: obs
    INTEGER,INTENT(IN)          :: i
    INTEGER                     :: j,ii,ilast,iinc

    ! Check status
    istatus=0
    IF ((istatus_acthdr_odb(i) == 1).and.(istatus_actbod_odb(i) == 1)) THEN                             ! Active
      istatus=(/1,0,0,0,0/)
    ENDIF
    IF ((istatus_rejhdr_odb(i) == 1).OR.(istatus_rejbod_odb(i) == 1)) THEN                              ! Rejected
      istatus=(/0,1,0,0,0/)
    ELSEIF ((istatus_pashdr_odb(i) == 1).OR.(istatus_pasbod_odb(i) == 1)) THEN                          ! Passive
      istatus=(/0,0,1,0,0/)
    ELSEIF ((istatus_blkhdr_odb(i) == 1).OR.(istatus_blkbod_odb(i) == 1)) THEN                          ! Blacklisted
      istatus=(/0,0,0,1,0/)
    ENDIF
    istatus(5)=ianflag_odb(i)                                                                           ! Canari status

   
    ! Update statistics if observation is active or passive
    IF (( istatus(1) > 0 ) .OR. (istatus(5) > 0 ) .OR. (istatus(3) > 0 )) THEN
      ! Summation
      ! Check if land or sea point
      ! LSM=1 means land point
      ! Updating 1 and 2
      IF ( lsm_odb(i) >= 0.5 ) THEN
        ilast=2
        iinc=1
      ELSE
        ! LSM=0 means sea point
        ! Updating 1 and 3
        ilast=3
        iinc=2
      ENDIF
      DO ii=1,ilast,iinc
        all_obs(obs)%var%count(ii)        = all_obs(obs)%var%count(ii)+1
        all_obs(obs)%var%fg_bias(ii)      = all_obs(obs)%var%fg_bias(ii)+rfgdep_odb(i)
        all_obs(obs)%var%fg_abs_bias(ii)  = all_obs(obs)%var%fg_abs_bias(ii)+ABS(rfgdep_odb(i))
        all_obs(obs)%var%fg_rms(ii)       = all_obs(obs)%var%fg_rms(ii)+((rfgdep_odb(i))*(rfgdep_odb(i)))
        all_obs(obs)%var%fg_dep(ii)       = all_obs(obs)%var%fg_dep(ii)+(rfgdep_odb(i))
        all_obs(obs)%var%fg_uncorr(ii)    = all_obs(obs)%var%fg_uncorr(ii)+(rfgdep_odb(i)+biascrl_odb(i))
        all_obs(obs)%var%bc(ii)           = all_obs(obs)%var%bc(ii)+biascrl_odb(i)
        all_obs(obs)%var%an_bias(ii)      = all_obs(obs)%var%an_bias(ii)+randep_odb(i)
        all_obs(obs)%var%an_abs_bias(ii)  = all_obs(obs)%var%an_abs_bias(ii)+ABS(randep_odb(i))
        all_obs(obs)%var%an_rms(ii)       = all_obs(obs)%var%an_rms(ii)+((randep_odb(i))*(randep_odb(i)))
        all_obs(obs)%var%an_dep(ii)       = all_obs(obs)%var%an_dep(ii)+(randep_odb(i))
      ENDDO
    ENDIF

    IF ( lusage ) THEN
      rlat=rlat_odb(i)*57.2957795131
      rlon=rlon_odb(i)*57.2957795131
      IF (( rlat < minlat ) .OR. (minlat < -500. )) minlat=rlat
      IF (( rlon < minlon ) .OR. (minlon < -500. )) minlon=rlon
      IF (( rlat > maxlat ) .OR. (maxlat < -500. )) maxlat=rlat
      IF (( rlon > maxlon ) .OR. (maxlon < -500. )) maxlon=rlon

    ENDIF
  END SUBROUTINE update_statistics

  SUBROUTINE print_usage_files(obs,i,cdtg)
#ifdef ODB_MONITOR
    USE sqlite,             ONLY : SQLITE_COLUMN,sqlite3_begin,sqlite3_set_column,&
                                   sqlite3_error,sqlite3_commit,sqlite3_errmsg,sqlite3_insert
#endif
    USE module_odb_extract, ONLY : rpress_odb,rlat,rlon,istatus,ilimits,cstaid_odb,randep_odb,&
                                   rfgdep_odb,robs_odb,biascrl_odb
    USE module_obstypes,    ONLY : all_obs

    IMPLICIT NONE
    INTEGER,INTENT(IN)                         :: obs,i
    CHARACTER(LEN=10),INTENT(IN)               :: cdtg
    INTEGER                                    :: level,ios,dtg
    CHARACTER(LEN=10)                          :: obname,satname
#ifdef ODB_MONITOR
    TYPE(SQLITE_COLUMN), DIMENSION(:), POINTER :: column
#endif
    INTEGER                                    :: ii

    IF ( lusage ) THEN
      read(cdtg,'(I10.10)') dtg
      IF (( all_obs(obs)%lsat ) .OR. (all_obs(obs)%obnumber == 5 ))  THEN
        level=INT(rpress_odb(i))
      ELSE
        level=all_obs(obs)%var%level
      ENDIF
      IF ( all_obs(obs)%lsat ) THEN
        satname=TRIM(all_obs(obs)%sensor%satelite)
        obname=TRIM(all_obs(obs)%sensor%name)
      ELSE
        satname="undefined"
        obname=TRIM(all_obs(obs)%name)
      ENDIF
#ifdef ODB_MONITOR
      IF ( lusage ) THEN
        ! Define columns
        ALLOCATE(column(18))
        ! Insert the values into the table.
        CALL sqlite3_set_column( column(1), dtg )
        IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db_usage)))
        CALL sqlite3_set_column( column(2), all_obs(obs)%obnumber )
        IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db_usage)))
        CALL sqlite3_set_column( column(3), TRIM(obname) )
        IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db_usage)))
        CALL sqlite3_set_column( column(4), TRIM(satname) )
        IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db_usage)))
        CALL sqlite3_set_column( column(5), TRIM(all_obs(obs)%var%name) )
        IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db_usage)))
        CALL sqlite3_set_column( column(6), level )
        IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db_usage)))
        CALL sqlite3_set_column( column(7),rlat)
        IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db_usage)))
        CALL sqlite3_set_column( column(8),rlon)
        IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db_usage)))
        CALL sqlite3_set_column( column(9),TRIM(cstaid_odb(i)))
        IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db_usage)))
        CALL sqlite3_set_column( column(10),robs_odb(i))
        IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db_usage)))
        CALL sqlite3_set_column( column(11),rfgdep_odb(i))
        IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db_usage)))
        CALL sqlite3_set_column( column(12),randep_odb(i))
        IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db_usage)))
        CALL sqlite3_set_column( column(13),biascrl_odb(i))
        IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db_usage)))
        DO ii=1,5
          CALL sqlite3_set_column( column(13+ii),istatus(ii))
          IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_set_column '//TRIM(sqlite3_errmsg(db_usage)))
        ENDDO
        CALL sqlite3_insert( db_usage, 'usage', column )
        IF(sqlite3_error(db_usage)) CALL sql_error('sqlite3_insert '//TRIM(sqlite3_errmsg(db_usage)))

        DEALLOCATE(column)
      ENDIF
#endif
    ENDIF
  END SUBROUTINE print_usage_files

  SUBROUTINE sql_error(msg)
    IMPLICIT NONE
    CHARACTER(len=120) :: msg

    WRITE(*,*) "ERROR: "//msg
    CALL ABORT()
  END SUBROUTINE sql_error
END MODULE
