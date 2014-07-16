SUBROUTINE read_mandalay_extract(cdtg)
  USE module_obstypes, ONLY     : all_obs,nused
  USE module_obsmon,   ONLY     : verbose,lalloc_all,lusage,&
                                  odbbase,open_usage_files,close_usage_files,make_means_and_rms,write_output
  USE module_odb_extract, ONLY  : cstaid_odb,iobtyp_odb,icodetype_odb,ivarno_odb,ivertco_type_odb,&
                                  isensor_odb,idate_odb,itime_odb,istatus_acthdr_odb,istatus_blkhdr_odb,&
                                  istatus_pashdr_odb,istatus_rejhdr_odb,istatus_actbod_odb,istatus_blkbod_odb,&
                                  istatus_pasbod_odb,istatus_rejbod_odb,rlat_odb,rlon_odb,rpress_odb,randep_odb,&
                                  rfgdep_odb,robs_odb,rfinoe_odb, biascrl_odb,lsm_odb,alloc_odb_extracts,dealloc_odb_extracts

  IMPLICIT NONE
  CHARACTER(LEN=10),INTENT(in) :: cdtg
  CHARACTER(LEN=21)            :: mandalay_file
  INTEGER,PARAMETER            :: iunit=11
  INTEGER                      :: ios,i,obs,ii
  INTEGER                      :: icount
  REAL                         :: cpu
  INTEGER,PARAMETER            :: nstatus=4
  INTEGER,PARAMETER            :: nlim=4

  INTERFACE
    SUBROUTINE find_cma_record(i,obs)
      INTEGER,INTENT(IN)          :: i
      INTEGER,INTENT(IN),OPTIONAL :: obs
    END SUBROUTINE find_cma_record
  END INTERFACE

  mandalay_file=TRIM(odbbase)//"_"//cdtg
 
  ! Initialization
  DO obs=1,nused
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
  ENDDO

  ! Open output file
  ios=0
  OPEN(UNIT=IUNIT,FILE=mandalay_file,FORM='FORMATTED',STATUS='OLD',IOSTAT=ios)
  IF (ios /= 0 ) THEN
    WRITE(*,*) 'Could not open odb based stat file: ',TRIM(mandalay_file)
    WRITE(*,*) 'No statistics are calclulated'
  ELSE

    ! Find the dimension of the extract file we will be working with
    IF ( lalloc_all ) THEN
      IF ( verbose > 0 ) THEN
        CALL cpu_time(cpu)
        WRITE(*,*) 'Start read-0.  CPU time elapsed: ',cpu
      ENDIF
      icount=0
      ! Find number of records in mandalay extract file
      DO WHILE (ios == 0 )
        READ(iunit,*,IOSTAT=ios)
        IF ( ios == 0 ) icount=icount+1
      ENDDO
      REWIND(iunit)
      IF ( verbose > 0 ) THEN
        CALL cpu_time(cpu)
        WRITE(*,*) 'End read-0.  CPU time elapsed: ',cpu
      ENDIF
      IF ( verbose > 0 ) WRITE(*,*) 'Found ',icount,' records in file ',TRIM(mandalay_file)
    ELSE
      icount=1
    ENDIF

    ! Allocate arrays for odb extract
    CALL alloc_odb_extracts(icount)
  
    IF ( verbose > 0 ) THEN
      CALL cpu_time(cpu)
      WRITE(*,*) 'Start read '//cdtg//'.  CPU time elapsed: ',cpu
    ENDIF

    i=0
    ios=0
    DO WHILE (ios == 0 )  
      i=i+1
      IF ( lalloc_all ) THEN
        ii=i
      ELSE
        ii=1
      ENDIF
      READ(iunit,*,IOSTAT=ios) iobtyp_odb(ii),icodetype_odb(ii),cstaid_odb(ii),ivarno_odb(ii),&
                               rlat_odb(ii),rlon_odb(ii),ivertco_type_odb(ii),rpress_odb(ii),isensor_odb(ii),&
                               idate_odb(ii),itime_odb(ii),istatus_acthdr_odb(ii),istatus_blkhdr_odb(ii),&
                               istatus_pashdr_odb(ii),istatus_rejhdr_odb(ii),istatus_actbod_odb(ii),&
                               istatus_blkbod_odb(ii),istatus_pasbod_odb(ii),istatus_rejbod_odb(ii),&
                               randep_odb(ii),rfgdep_odb(ii),robs_odb(ii),rfinoe_odb(ii),biascrl_odb(ii),lsm_odb(ii)
      IF ( ios == 0 ) THEN
        ! Print what is read if requested
        IF ( verbose > 5 ) THEN
          write(*,*) 'read1: ',iobtyp_odb(ii),icodetype_odb(ii),ivarno_odb(ii),ivertco_type_odb(ii),isensor_odb(ii),rpress_odb(ii)
          write(*,*) 'read2: ',rfgdep_odb(ii),randep_odb(i),biascrl_odb(ii)
        ENDIF

        ! If we do not allocate the content of the file we check now if the record match
        IF (.NOT. lalloc_all ) THEN
          ! The routine loop over all obs
          CALL find_cma_record(ii)

          ! Make means and RMS and save for this time step
          CALL make_means_and_rms()

          ! Write output for all obs
          CALL write_output(cdtg)

        ENDIF
      ELSE
        ! Adjust i ( counted one extra before EOF)
        i=i-1
        ! If e.g. emtpy  we adjust icount
        IF ( i < icount ) icount=i
      ENDIF
    ENDDO

    ! If we allocated the whole content of the file we check now if the records match
    IF ( lalloc_all ) THEN
      IF ( verbose > 0 ) THEN
        CALL cpu_time(cpu)
        WRITE(*,*) 'Finished read '//cdtg//'. CPU time elapsed: ',cpu,' Read ',i,'records'
      ENDIF
      IF ( lusage ) THEN
        DO obs=1,nused
        
          IF ( obs == 1 ) CALL SYSTEM("mkdir "//cdtg)

          ! Opening the usage files
          CALL open_usage_files(obs,cdtg)

          ! Calculating the statistics (ALL) sequentially for all observation types
          DO i=1,icount
            CALL find_cma_record(i,obs)
          ENDDO

          ! Closing the usage files
          CALL close_usage_files()
      
          ! Make means and RMS and save for this time step and this obs
          CALL make_means_and_rms(obs)
          
          ! Write output for this obs
          CALL write_output(cdtg,obs)
        ENDDO    
      ELSE
        ! If we loop over records first and then observations
        ! This is a faster option, but it would need a lot of 
        ! open files if we should be able to write usage maps
        ! this way.
        ! The find_cma_records routine loop over all obs
        DO i=1,icount
          CALL find_cma_record(i)
        ENDDO

        ! Make means and RMS and save for this time step
        CALL make_means_and_rms()

        ! Write output for all obs
        CALL write_output(cdtg)
      ENDIF
      IF ( verbose > 0 ) THEN
        CALL cpu_time(cpu)
        WRITE(*,*) 'Finished statistics '//cdtg//'. CPU time elapsed: ',cpu
      ENDIF
    ELSE
      IF ( verbose > 0 ) THEN
        CALL cpu_time(cpu)
        WRITE(*,*) 'Finished read and statistics '//cdtg//'. CPU time elapsed: ',cpu
      ENDIF
    ENDIF

    ! De-allocate temporary arrays used for reading
    CALL dealloc_odb_extracts()
  ENDIF
END SUBROUTINE read_mandalay_extract      
