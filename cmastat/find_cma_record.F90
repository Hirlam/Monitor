SUBROUTINE find_cma_record(i,ind)
  USE module_obstypes, ONLY         : all_obs,nused
  USE module_obsmon,   ONLY         : verbose,lalloc_all,lusage,&
                                      update_statistics,print_usage_files
  USE module_odb_extract, ONLY      : cstaid_odb,iobtyp_odb,icodetype_odb,ivarno_odb,ivertco_type_odb,&
                                      isensor_odb,idate_odb,itime_odb,istatus_acthdr_odb,istatus_blkhdr_odb,&
                                      istatus_pashdr_odb,istatus_rejhdr_odb,istatus_actbod_odb,istatus_blkbod_odb,&
                                      istatus_pasbod_odb,istatus_rejbod_odb,rlat_odb,rlon_odb,rpress_odb,randep_odb,&
                                      rfgdep_odb,robs_odb,rfinoe_odb,biascrl_odb,lsm_odb,minlat,minlon,maxlon,maxlat,&
                                      rlat,rlon

  IMPLICIT NONE
  INTEGER,INTENT(IN)           :: i
  INTEGER,INTENT(IN),OPTIONAL  :: ind

  CHARACTER(len=8)             :: cstaid
  CHARACTER(len=10)            :: cdtg
  INTEGER                      :: findex,lindex,obs,ios
  INTEGER                      :: obstype,subtype,subtypestart,subtypeend,par,vertco
  LOGICAL                      :: lsat
  INTEGER                      :: isensor,level,level1,level2

  IF (PRESENT(ind)) THEN
    findex=ind
    lindex=ind
  ELSE
    findex=1
    lindex=nused
  ENDIF

  ! Sanity
  IF (( lusage ) .AND. ( .NOT. lalloc_all )) THEN
    WRITE(*,*) 'find_cma_records must have lalloc_all=.TRUE. if lusage=.TRUE.'
    CALL abort
  ENDIF

  ! If this observation type should not be monitored, not try to find it
  IF ( ANY (all_obs(:)%obnumber == iobtyp_odb(i) )) THEN

    obsloop: DO obs=findex,lindex

      obstype=all_obs(obs)%obnumber
      ! Only check for this observation if it has the the same observation type
      ! as the record from the extract
      IF ( iobtyp_odb(i) == all_obs(obs)%obnumber ) THEN                                                      ! Obtype
        
        ! Set short-names of observation properties for this type
        lsat=all_obs(obs)%lsat
        subtypestart=all_obs(obs)%var%subtypestart
        subtypeend=all_obs(obs)%var%subtypeend
        par=all_obs(obs)%var%nr
        vertco=all_obs(obs)%var%vertco
        isensor=all_obs(obs)%sensor%id

        ! Make sure we have consistency between different observation types
        ! Satelite
        IF ( lsat ) THEN
          cstaid=TRIM(cstaid_odb(i))
          read(cstaid(1:8),'(I8)',IOSTAT=ios) subtype 
          IF ( ios /= 0 ) THEN
            WRITE(*,*) i,ind,obs,obstype,cstaid,'#',cstaid_odb(i)
            CALL ABORT
          ENDIF
          level1=all_obs(obs)%sensor%channel
          level2=all_obs(obs)%sensor%channel
          level=INT(rpress_odb(i))
          subtypestart=all_obs(obs)%sensor%satid
          subtypeend=all_obs(obs)%sensor%satid
    
        ! TEMP
        ELSEIF ( obstype == 5 ) THEN
          subtype=icodetype_odb(i)
          level1=all_obs(obs)%var%level1
          level2=all_obs(obs)%var%level2
          level=INT(rpress_odb(i))
  
        ! Not satelite not TEMP
        ELSE
          subtype=icodetype_odb(i)
          level=all_obs(obs)%var%level
          level1=all_obs(obs)%var%level1
          level2=all_obs(obs)%var%level2
        ENDIF
 
        ! Print before testing
        IF ( verbose > 5 ) THEN
          write(*,*) 'testing1: ',iobtyp_odb(i),subtype,subtype,level,level,ivarno_odb(i),ivertco_type_odb(i),isensor_odb(i)
          write(*,*) 'testing2: ',obstype,subtypestart,subtypeend,level1,level2,par,vertco,isensor
        ENDIF
  
        !
        ! Check if the odb extract match our observation type
        !
        IF ((subtype >= subtypestart).AND.(subtype <= subtypeend)) THEN                                           ! Subtype
          IF (( level >= level1 ) .AND. ( level <= level2 )) THEN                                                 ! Vertical level/channel
            IF ((ivarno_odb(i) == par).AND.(ivertco_type_odb(i) == vertco).AND.(isensor_odb(i) == isensor)) THEN  ! Parameter/Vertical coordinate/sensor

              ! Summation
              IF ( verbose > 4 ) THEN
                write(*,*) 'test success1: ',iobtyp_odb(i),subtype,subtype,level,level,ivarno_odb(i),ivertco_type_odb(i),&
                                             isensor_odb(i)
                write(*,*) 'test success2: ',obstype,subtypestart,subtypeend,level1,level2,par,vertco,isensor
                write(*,*) 'test success3: ',rfgdep_odb(i),randep_odb(i),biascrl_odb(i),lsm_odb(i)
              ENDIF

              ! Update statistics
              CALL update_statistics(obs,i)

              ! Print usage information to files
              CALL print_usage_files(obs,i,cdtg)
             
              ! Observation now found. No need to continue to search for this record
              EXIT obsloop
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDDO obsloop
  ENDIF
END SUBROUTINE find_cma_record      
