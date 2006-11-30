SUBROUTINE copy_mod

 USE data

 IMPLICIT NONE
 
 INTEGER :: i,k

 DO k = 1,maxstn
    ALLOCATE(obs(k)%o(maxtim))
 ENDDO
   
 obs%nexp       = 1
 obs%nfclengths = 1
 obs%nparver    = hir%nparver
 obs%ntim       = hir%ntim
 obs%stnr       = hir%stnr
 obs%active     = hir%active

 DO k=1,maxstn
    DO i=1,obs(k)%ntim
       ALLOCATE(obs(k)%o(i)%date)
       ALLOCATE(obs(k)%o(i)%time)
       ALLOCATE(obs(k)%o(i)%val(nparver))
       obs(k)%o(i)%date = hir(k)%o(i)%date
       obs(k)%o(i)%time = hir(k)%o(i)%time
       obs(k)%o(i)%val  = hir(k)%o(i)%nal(1,1,:)
    ENDDO
 ENDDO

 copied_mod = .TRUE.

 WRITE(6,*)' Copied model data to observation array'
 RETURN

END SUBROUTINE copy_mod
