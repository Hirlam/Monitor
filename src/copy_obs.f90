SUBROUTINE copy_obs

 USE data

 IMPLICIT NONE
 
 INTEGER :: i,k

 DO k = 1,maxstn
    ALLOCATE(hir(k)%o(maxtim))
 ENDDO
   
 hir%nexp       = obs%nexp
 hir%nfclengths = obs%nfclengths
 hir%nparver    = obs%nparver
 hir%ntim       = obs%ntim
 hir%stnr       = obs%stnr
 hir%lat        = obs%lat
 hir%lon        = obs%lon
 hir%active     = obs%active

 DO k=1,maxstn
    DO i=1,hir(k)%ntim
       ALLOCATE(hir(k)%o(i)%date)
       ALLOCATE(hir(k)%o(i)%time)
       ALLOCATE(hir(k)%o(i)%nal(nexp,nfclengths,nparver))
       hir(k)%o(i)%date        = obs(k)%o(i)%date
       hir(k)%o(i)%time        = obs(k)%o(i)%time
       hir(k)%o(i)%nal(1,1,:)  = obs(k)%o(i)%val
    ENDDO
 ENDDO

 hir%obs_is_allocated   = obs%obs_is_allocated

 WRITE(6,*)' Copied observations to model array'

 copied_obs = .TRUE.

 RETURN

END SUBROUTINE copy_obs
