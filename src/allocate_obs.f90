SUBROUTINE allocate_obs

 USE data
 USE functions, ONLY : get_maxtim
 !
 ! Allocated observation station array
 !

 IMPLICIT NONE

 INTEGER :: k,si,ei

 IF (ANY(obs%obs_is_allocated)) THEN
    WRITE(6,*)'Warning, some obs stations are allocated'
    WRITE(6,*)'Stupid programmer, abort'
    CALL abort
 ENDIF

 ! Estimate maxtim if not given by user
 IF ( edate_obs == 0 ) edate_obs = edate
 IF (maxtim == 0) maxtim=get_maxtim(sdate,edate_obs,obint)

 WRITE(6,*)'Maxtim for observations is ',maxtim
 !
 ! If model array is not allocated 
 ! do so and init arrays
 !

 si = sdate     * 100 + stime
 ei = edate_obs * 100 + etime_obs

 DO k = 1,maxstn
    ALLOCATE(obs(k)%o(maxtim))
    IF ( use_pos ) THEN
       ALLOCATE(obs(k)%pos(si:ei))
       obs(k)%pos = 0
    ELSE
       NULLIFY(obs(k)%pos)
    ENDIF
 ENDDO
   
 obs%ntim       = 0
 obs%nexp       = nexp
 obs%stnr       = 0
 obs%nparver    = nparver
 obs%nfclengths = nfclengths

 obs%lat        = err_ind
 obs%lon        = err_ind
 obs%hgt        = err_ind

 obs%active     = .FALSE.

 obs%obs_is_allocated = .TRUE.

 RETURN

END SUBROUTINE allocate_obs
