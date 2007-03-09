SUBROUTINE set_qc_lim

 USE data

 IMPLICIT NONE

 INTEGER :: k, wrk(mparver),nlev

 CHARACTER(LEN=20) ::  cform='(2A5,XX(f9.2))'
 CHARACTER(LEN=20) :: cform2='(A10,XX(f9.2))'
 

!----------------------------

 DO k=1,nparver

    IF ( ABS(qc_lim(k) - err_ind ) > 1.e-6 ) CYCLE

    !  Multi level fields
 IF (ltemp) THEN

    IF     (lev_typ(k).EQ.dd_ind) THEN
       qc_lim(k) = dd_lim
    ELSEIF (lev_typ(k).EQ.fi_ind) THEN
       qc_lim(k) = fi_lim
    ELSEIF (lev_typ(k).EQ.ff_ind) THEN
       qc_lim(k) = ff_lim
    ELSEIF (lev_typ(k).EQ.tt_ind) THEN
       qc_lim(k) = tt_lim
    ELSEIF (lev_typ(k).EQ.rh_ind) THEN
       qc_lim(k) = rh_lim
    ELSEIF (lev_typ(k).EQ.qq_ind) THEN
       qc_lim(k) = qq_lim
    ELSE
       WRITE(6,*)'No quality control for this variable',obstype(k)
       CALL abort
    ENDIF

 ELSE

    IF     (k.EQ.ps_ind) THEN
       qc_lim(k) = ps_lim
    ELSEIF (k.EQ.pe_ind) THEN
       qc_lim(k) = pe_lim
    ELSEIF (k.EQ.pd_ind) THEN
       qc_lim(k) = pd_lim
    ELSEIF (k.EQ.rh_ind) THEN
       qc_lim(k) = rh_lim
    ELSEIF (k.EQ.tt_ind) THEN
       qc_lim(k) = tt_lim
    ELSEIF (k.EQ.ff_ind) THEN
       qc_lim(k) = ff_lim
    ELSEIF (k.EQ.dd_ind) THEN
       qc_lim(k) = dd_lim
    ELSEIF (k.EQ.sw_ind) THEN
       qc_lim(k) = sw_lim
    ELSEIF (k.EQ.nr_ind) THEN
       qc_lim(k) = nr_lim
    ELSEIF (k.EQ.gr_ind) THEN
       qc_lim(k) = gr_lim
    ELSEIF (k.EQ.wq_ind) THEN
       qc_lim(k) = wq_lim
    ELSEIF (k.EQ.wt_ind) THEN
       qc_lim(k) = wt_lim
    ELSEIF (k.EQ.uw_ind) THEN
       qc_lim(k) = uw_lim
    ELSEIF (k.EQ.su_ind) THEN
       qc_lim(k) = su_lim
    ELSEIF (k.EQ.lw_ind) THEN
       qc_lim(k) = lw_lim
    ELSEIF (k.EQ.nn_ind) THEN
       qc_lim(k) = nn_lim
    ELSEIF (k.EQ.qq_ind) THEN
       qc_lim(k) = qq_lim
    ELSE
       WRITE(6,*)'No quality control for this variable',obstype(k)
       CALL abort
    ENDIF

 ENDIF

 ENDDO

 !
 ! Summarize qc limits
 !

 IF ( ltemp ) THEN
    wrk = 0
    WHERE(lev_lst > 0 ) wrk = 1
    nlev = SUM(wrk)

    WRITE( cform(6:7),'(I2.2)')nlev
    WRITE(cform2(6:7),'(I2.2)')nlev

    WRITE(6,cform2)'Levels are',lev_lst(1:nlev)
    DO k=1,nparver,nlev
       WRITE(6,cform)obstype(k)(1:2),' is ',qc_lim(k:k+nlev-1)
    ENDDO

 ELSE

    DO k=1,nparver
       WRITE(6,*)obstype(k),' limit is ',qc_lim(k)
    ENDDO

 ENDIF
 
 WRITE(6,*)

END SUBROUTINE set_qc_lim
