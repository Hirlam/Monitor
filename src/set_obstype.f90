SUBROUTINE set_obstype

 USE DATA

 IMPLICIT NONE

 INTEGER :: i,ii,               &
            wrk(mparver),nlev

 ! ---------------------------------------------------------------------

 ALLOCATE(obstype(0:nparver))
 obstype='XXXXXX'

 IF (ltemp ) THEN

    wrk = 0
    WHERE(lev_lst > 0 ) wrk = 1
    nlev = SUM(wrk)

    ! Init lev_typ

    IF ( fi_ind /= 0 ) lev_typ((fi_ind-1)*nlev + 1:fi_ind*nlev) = fi_ind
    IF ( tt_ind /= 0 ) lev_typ((tt_ind-1)*nlev + 1:tt_ind*nlev) = tt_ind
    IF ( rh_ind /= 0 ) lev_typ((rh_ind-1)*nlev + 1:rh_ind*nlev) = rh_ind
    IF ( dd_ind /= 0 ) lev_typ((dd_ind-1)*nlev + 1:dd_ind*nlev) = dd_ind
    IF ( ff_ind /= 0 ) lev_typ((ff_ind-1)*nlev + 1:ff_ind*nlev) = ff_ind
    IF ( qq_ind /= 0 ) lev_typ((qq_ind-1)*nlev + 1:qq_ind*nlev) = qq_ind


    DO i=1,mparver

       ii = MOD(i,nlev) 
       IF (ii == 0 ) ii = nlev

       IF(tt_ind > 0 .AND. lev_typ(i) == tt_ind) &
       WRITE(obstype(i),'(A2,I4.4)')  'TT',NINT(lev_lst(ii))

       IF(qq_ind > 0 .AND. lev_typ(i) == qq_ind) &
       WRITE(obstype(i),'(A2,I4.4)')  'QQ',NINT(lev_lst(ii))

       IF(ff_ind > 0 .AND. lev_typ(i) == ff_ind) &
       WRITE(obstype(i),'(A2,I4.4)')  'FF',NINT(lev_lst(ii))

       IF(dd_ind > 0 .AND. lev_typ(i) == dd_ind) &
       WRITE(obstype(i),'(A2,I4.4)')  'DD',NINT(lev_lst(ii))

       IF(fi_ind > 0 .AND. lev_typ(i) == fi_ind) &
       WRITE(obstype(i),'(A2,I4.4)')  'FI',NINT(lev_lst(ii))

       IF(rh_ind > 0 .AND. lev_typ(i) == rh_ind) &
       WRITE(obstype(i),'(A2,I4.4)')  'RH',NINT(lev_lst(ii))

    ENDDO

 ELSE

    IF(hg_ind.GT.0) obstype(hg_ind) = 'HG'
    IF(la_ind.GT.0) obstype(la_ind) = 'LA'
    IF(wp_ind.GT.0) obstype(wp_ind) = 'WP'
    IF(wh_ind.GT.0) obstype(wh_ind) = 'WH'
    IF(tt_ind.GT.0) obstype(tt_ind) = 'TT'
    IF(ff_ind.GT.0) obstype(ff_ind) = 'FF'
    IF(dd_ind.GT.0) obstype(dd_ind) = 'DD'
    IF(uw_ind.GT.0) obstype(uw_ind) = 'UW'
    IF(wt_ind.GT.0) obstype(wt_ind) = 'WT'
    IF(wq_ind.GT.0) obstype(wq_ind) = 'WQ'
    IF(rh_ind.GT.0) obstype(rh_ind) = 'RH'
    IF(sw_ind.GT.0) obstype(sw_ind) = 'SW'
    IF(su_ind.GT.0) obstype(su_ind) = 'SU'
    IF(sd_ind.GT.0) obstype(sd_ind) = 'SD'
    IF(lu_ind.GT.0) obstype(lu_ind) = 'LU'
    IF(ld_ind.GT.0) obstype(ld_ind) = 'LD'
    IF(lw_ind.GT.0) obstype(lw_ind) = 'LW'
    IF(nr_ind.GT.0) obstype(nr_ind) = 'NR'
    IF(gr_ind.GT.0) obstype(gr_ind) = 'GR'
    IF(gs_ind.GT.0) obstype(gs_ind) = 'GS'
    IF(gc_ind.GT.0) obstype(gc_ind) = 'GC'
    IF(hb_ind.GT.0) obstype(hb_ind) = 'HB'
    IF(ps_ind.GT.0) obstype(ps_ind) = 'PS'
    IF(pe_ind.GT.0) obstype(pe_ind) = 'PE'
    IF(pd_ind.GT.0) obstype(pd_ind) = 'PD'
    IF(nn_ind.GT.0) obstype(nn_ind) = 'NN'
    IF(fi_ind.GT.0) obstype(fi_ind) = 'FI'
    IF(rf_ind.GT.0) obstype(rf_ind) = 'RF'
    IF(qq_ind.GT.0) obstype(qq_ind) = 'QQ'
    IF(tz_ind.GT.0) obstype(tz_ind) = 'TZ'
    IF(uz_ind.GT.0) obstype(uz_ind) = 'UZ'
    IF(tu_ind.GT.0) obstype(tu_ind) = 'TU'

 ENDIF

END SUBROUTINE set_obstype
