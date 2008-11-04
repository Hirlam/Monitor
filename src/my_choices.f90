SUBROUTINE my_choices

USE data, ONLY : data_to_verify,data_source

IMPLICIT NONE

CHARACTER(LEN=60) :: info = 'Your are running case '
CHARACTER(LEN=2)  :: ci   = ' '

!---------------------------------------

 IF (LEN(TRIM(data_source)) > 0 ) THEN

    !
    ! Select by string
    !

    WRITE(6,*)'Start reading data from ',TRIM(data_source)

    SELECT CASE (TRIM(data_source))
   
    CASE('mastdata')
   
       CALL read_obs_mast_date
       CALL read_mod_mast

    CASE('wind_test','WIND_TEST')
   
       CALL read_vobs
       CALL read_any

    CASE('vfld','VFLD')
   
       CALL read_vobs
       CALL read_vfld
   
    CASE('vfld_temp','VFLD_TEMP')
   
       CALL read_vobs_temp
       CALL read_vfld_temp
   
    CASE('vfld_hirvda_y4','VFLD_HIRVDA_Y4')
   
       CALL read_vobs_y4
       CALL read_vfld_hirvda_y4

    CASE('vfld_temp_hirvda_y4','VFLD_TEMP_HIRVDA_Y4')
   
       CALL read_vobs_temp_y4
       CALL read_vfld_temp_hirvda_y4

    CASE('htb','HTB')
   
       ! Helsinki testbed data
       CALL read_vhtb
       CALL read_vfld

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

 WRITE(6,*)'Start reading data from ',data_to_verify

 SELECT CASE (data_to_verify)

 CASE(1)

    CALL read_vobs
    CALL read_vfld

 CASE(2)

    CALL read_vobs_temp
    CALL read_vfld_temp

 CASE(3)

    CALL read_radiation
    CALL read_point

 CASE(8)

    CALL read_vobs
    CALL copy_obs

 CASE(9)

    CALL read_vobs
    CALL read_vfld_precip

 CASE(10)

    CALL read_voqc
    CALL read_vfld_hirvda

 CASE(11)

    CALL read_voqc_temp
    CALL read_vfld_temp_hirvda

 CASE(14)

    CALL read_voqc_y4
    CALL read_vfld_hirvda_y4

 CASE(15)

    CALL read_voqc_temp_y4
    CALL read_vfld_temp_hirvda_y4

 CASE(16)

    CALL read_vobs_dmi
    CALL read_vfld

 CASE(18)

    CALL read_vobs_temp_dmi
    CALL read_vfld_temp_dmi

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
