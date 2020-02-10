 SUBROUTINE allocate_mod

 USE data
 USE functions, ONLY : get_maxtim
 !
 ! Allocated model station array
 !

 IMPLICIT NONE

 INTEGER :: k

 IF (ANY(hir%obs_is_allocated)) THEN
    WRITE(6,*)'Warning, some model stations are allocated'
    WRITE(6,*)'Stupid programmer, abort'
    CALL abort
 ENDIF

 IF ( maxtim == 0 )  maxtim=get_maxtim(sdate,edate,fcint)

 WRITE(6,*)'Maxtim for model data is ',maxtim

 !
 ! If model array is not allocated 
 ! do so and init arrays
 !

 DO k = 1,maxstn
    ALLOCATE(hir(k)%o(maxtim),&
             hir(k)%hgtmod(nexp))
    NULLIFY(hir(k)%pos)
    hir(k)%hgtmod = err_ind
 ENDDO
   
 hir%ntim       = 0
 hir%stnr       = 0
 hir%nexp       = nexp
 hir%nparver    = nparver
 hir%nfclengths = nfclengths
 hir%lat        = err_ind
 hir%lon        = err_ind
 hir%active     = .FALSE.

 hir%obs_is_allocated = .TRUE.

 RETURN

END SUBROUTINE allocate_mod
