SUBROUTINE my_choices

USE data, ONLY : data_to_verify

IMPLICIT NONE

CHARACTER(LEN=60) :: info = 'Your are running case '
CHARACTER(LEN=2)  :: ci   = ' '

!---------------------------------------

 WRITE(ci,'(I2.2)')data_to_verify
 info = TRIM(info)//' '//ci

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

 CASE(5)

    CALL read_cabauw
    CALL read_ddh

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

    info = ' Read windpower data'
    
 CASE(20)

    CALL read_kalman_test

    info = ' Read Kalman test'

 CASE(21)

    CALL read_nasudden
    CALL read_mod_nasudden

 CASE(22)

    CALL read_windp_oper_obs
    CALL read_windp_oper_mod

 CASE(23)

    CALL read_windp_oper_obs2
!   CALL copy_obs
    CALL read_windp_oper_mod

 CASE(24)

    CALL read_vindstat
    CALL copy_obs
    
 CASE(25)

    CALL read_windp_oper_mod_day
    CALL read_vindstat

 CASE DEFAULT
    WRITE(6,*)'No such option ',data_to_verify
    CALL abort
 END SELECT

 WRITE(6,*)
 WRITE(6,*)TRIM(info)
 WRITE(6,*)

RETURN
END
