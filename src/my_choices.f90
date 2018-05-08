SUBROUTINE my_choices

USE data, ONLY : data_to_verify,data_source,    &
                 sdate,edate,edate_obs,obint,   &
                 maxtim,  &
                 varlist,nparver,fcint
USE functions, ONLY : get_maxtim

IMPLICIT NONE

INTEGER :: maxtim_obs,maxtim_mod

!---------------------------------------

 IF (LEN(TRIM(data_source)) > 0 ) THEN

    !
    ! Select by string
    !

    WRITE(6,*)'Start reading data with selection ',TRIM(data_source)

    SELECT CASE (TRIM(data_source))
   
    CASE('mastdata')
   
       CALL read_obs_mast_date
       CALL read_mod_mast

    CASE('wind_test','WIND_TEST')
   
       CALL read_vobs
       CALL read_any

    CASE('vfld','VFLD')
   
       IF (maxtim == 0) THEN
            IF ( edate_obs == 0 ) edate_obs = edate
            maxtim_obs=get_maxtim(sdate,edate_obs,obint)
            maxtim_mod=get_maxtim(sdate,edate,fcint)
            maxtim = MAX(maxtim_obs,maxtim_mod)
       ENDIF

       !
       ! If we apply height adjustment we need to read the observations first
       ! if not we read model data first to get a smaller number of observations
       !

       IF ( ANY( varlist(1:nparver) == 'SPS'  ) .OR.  &
            ANY( varlist(1:nparver) == 'TTHA' ) .OR.  &
            ANY( varlist(1:nparver) == 'TNHA' ) .OR.  &
            ANY( varlist(1:nparver) == 'TXHA' ) ) THEN
         CALL read_vobs
         CALL read_vfld
       ELSE
         CALL read_vfld
         CALL read_vobs
       ENDIF
   
    CASE('vfld_temp','VFLD_TEMP')
   
       IF (maxtim == 0) THEN
            IF ( edate_obs == 0 ) edate_obs = edate
            maxtim_obs=get_maxtim(sdate,edate_obs,obint)
            maxtim_mod=get_maxtim(sdate,edate,fcint)
            maxtim = MAX(maxtim_obs,maxtim_mod)
       ENDIF

       CALL read_vfld_temp
       CALL read_vobs_temp
   
    CASE DEFAULT

       WRITE(6,*)'No such option ',TRIM(data_source)
       CALL abort

    END SELECT

 ELSE

 !
 ! Select by number
 ! This should be made redundant at
 ! some point
 !

 WRITE(6,*)'Start reading data with selection ',data_to_verify

 SELECT CASE (data_to_verify)

 CASE(3)

    CALL read_radiation
    CALL read_point

 CASE(8)

    CALL read_vobs
    CALL copy_obs

 CASE(9)

    CALL read_vobs
    CALL read_vfld_precip

 CASE(29)

    CALL read_auto
    CALL read_vfld

 CASE DEFAULT
    WRITE(6,*)'No such option ',data_to_verify
    CALL abort
 END SELECT

 ENDIF

 WRITE(6,*)'Done reading data'
 WRITE(6,*)

RETURN
END
