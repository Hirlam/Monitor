SUBROUTINE conditional(nexp,nfclen,npar,fi, &
                       obs,hir,             &
                       use_this)

 !
 ! Apply conditional selection on data.
 ! Selection can be done on a mod/obs interval.
 ! For model selection ALL or ANY data could be set 
 ! as requirement.
 !
 ! Ulf Andrae, SMHI, 2011
 !

 USE data, ONLY : cond,cond_param,err_ind
 IMPLICIT NONE


 ! Input
 INTEGER,      INTENT( IN) :: nexp,nfclen,npar,fi
 REAL, TARGET, INTENT( IN) :: obs(npar),hir(nexp,nfclen,npar)
 LOGICAL,      INTENT(OUT) :: use_this

 ! Local

 INTEGER :: i,j,k
 LOGICAL :: luse(cond_param),euse(nexp)


 !--------------------------------------------------------------------------

 ! Init
 luse(:) = .FALSE.

 !
 ! Loop over all conditions, find matching parameter
 ! and apply the condition
 !

 DO i=1,cond_param

    DO j=1,npar

      IF ( j /= cond(i)%ind ) CYCLE

      IF ( cond(i)%lobs ) THEN

        ! Check observations
        luse(i) = ( obs(j) > cond(i)%llim .AND. &
                    obs(j) < cond(i)%ulim .AND. &
                ABS(obs(j) - err_ind ) > 1.e-6 )
           
      ELSE

        ! Check model data
        euse(:) = .FALSE.

        DO k=1,nexp

           euse(k) = ( hir(k,fi,j) > cond(i)%llim .AND. &
                       hir(k,fi,j) < cond(i)%ulim .AND. &
                   ABS(hir(k,fi,j) - err_ind ) > 1.e-6 )

        ENDDO

        IF ( cond(i)%all_mod ) THEN
           ! Require ALL experiments to fulfill the condition
           luse(i) = ALL(euse(:))
        ELSE
           ! Require ANY experiment to fulfill the condition
           luse(i) = ANY(euse(:))
        ENDIF
   
      ENDIF

    ENDDO

 ENDDO
 
 ! To be used or not to be used?

 use_this = ALL(luse(:))

 RETURN

END SUBROUTINE conditional
