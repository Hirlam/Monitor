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
   
    CASE('WINDP_CUSTOM')

       CALL read_windp_oper_obs2_custom
       CALL read_windp_oper_mod_custom

    CASE('sodaflux')
   
       CALL read_soda
       CALL read_mod_soda

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

    CASE('windp_test')

       CALL read_windp_oper_obs2
       CALL read_windp_oper_mod

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

 CASE(6)

    CALL read_bak
    CALL read_vfld

 CASE(7)

    CALL read_bak
    CALL copy_obs

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

 CASE(12)

    CALL read_statgen
    CALL copy_obs

 CASE(13)

    CALL read_statgen
    CALL read_gp

 CASE(14)

    CALL read_voqc_y4
    CALL read_vfld_hirvda_y4

 CASE(15)

    CALL read_voqc_temp_y4
    CALL read_vfld_temp_hirvda_y4

 CASE(16)

    CALL read_vobs_dmi
    CALL read_vfld

 CASE(17)

    CALL read_windp_obs
    CALL copy_obs

 CASE(18)

    CALL read_vobs_temp_dmi
    CALL read_vfld_temp_dmi

 CASE(19)

    CALL read_windp_obs
    CALL read_windp_mod

 CASE(20)

    CALL read_kalman_test

 CASE(21)

    CALL read_nasudden
    CALL read_mod_nasudden

 CASE(22)

    CALL read_windp_oper_obs
    CALL read_windp_oper_mod

 CASE(23)

    CALL read_windp_oper_obs2
    CALL read_windp_oper_mod

 CASE(24)

    CALL read_vindstat
    CALL copy_obs
    
 CASE(25)

    CALL read_windp_oper_mod_day
    CALL read_vindstat

 CASE(27)

    CALL read_windp_oper_obs2
    CALL read_windp_oper_mod_plot

 CASE(28)

    CALL read_windp_oper_obs3
    CALL read_windp_oper_mod2

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
