SUBROUTINE read_ddh

! 
! Read ERA40 ddh files and organize for evalutaion
! 
! Ulf Andrae  ECMWF  2003
! 
 
 USE constants
 USE data
 USE functions
 USE timing

 IMPLICIT NONE

 INTEGER :: i,j,k,l,		&
            ierr,ierr2,		&
            ii_cycle,		&
            flag = 0,		&
            cyear,			&
            date,date2,		&
            ind,idum,idum2,	&
            idat,idat2,		&
            fc_start,fc_interval,	&
            fc_cycle(3,2),fc_period,	&
            timing_id

 REAL :: rdum,rhour,rhour2,itim,itim2,		&
         th,uh,vh,qh,psh,swnh,lwnh,lwdh,swdh,uwh,vwh,wth,wqh,lsp,cvp

 CHARACTER(LEN=70) :: hname ='../dat/ddh/AME_YYYY_sfc1ld_XX'
 CHARACTER(LEN=70) :: hname2='../dat/ddh/AME_YYYY_flx_XX'
 CHARACTER(LEN=70) :: cdum ='',cdum2=''
 CHARACTER(LEN= 1) :: cdum3 =''

!------------------------------------------

 !
 ! If model array is not allocated 
 ! do so and init arrays
 !

 IF (.NOT.ANY(hir%obs_is_allocated)) CALL allocate_mod

 !
 ! Loop over all stations
 !

 STATION_LOOP : DO k=1,maxstn

 ! Try to use database if requested
 IF(use_database) THEN
    CALL read_database(k,1,ierr)
    IF(ierr.EQ.0) CYCLE
 ENDIF

 cyear = sdate / 10000

 IF (stnlist(k).GT.100) CYCLE
 WRITE(6,*)'READ ERA40',stnlist(k)

 EXP_LOOP : DO l=1,1

 IF (lprint_read) WRITE(6,*)'EXP:',expname(l)

 i=0
 FILE_LOOP_M : DO

 ! Open the first input file
 cdum=hname
 ind=index(hname,'YYYY')
 WRITE(cdum(ind:ind+3),'(I4)')cyear
 ind=index(hname,'XX')
 WRITE(cdum(ind:ind+1),'(I2.2)')hir(k)%stnr
 IF (lprint_read) WRITE(6,*)'hname:',cdum
 OPEN(lunin,file=cdum,status='old',iostat=ierr,position='rewind')

 IF (ierr.NE.0) THEN
    WRITE(6,*)'Could not open ',cdum
    cyear = cyear + 1
    IF(cyear.gt.edate/10000) EXIT FILE_LOOP_M
    CYCLE FILE_LOOP_M
 ENDIF

 WRITE(6,*)'OPEN ',cdum

 ! Open the second input file
 cdum2=hname2
 ind=index(hname2,'YYYY')
 WRITE(cdum2(ind:ind+3),'(I4)')cyear
 ind=index(hname2,'XX')
 WRITE(cdum2(ind:ind+1),'(I2.2)')hir(k)%stnr
 IF (lprint_read) WRITE(6,*)'hname2: ',cdum2
 OPEN(lunin2,file=cdum2,status='old',iostat=ierr2,position='rewind')

 IF (ierr2.NE.0) THEN
    WRITE(6,*)'Could not open ',cdum2
    CALL abort
 ENDIF

 !
 ! Some files have 00,06,12,18 and some only 00,12
 !

 IF (check_this_file(lunin).EQ.1) THEN
    call read_period(12,24,fc_cycle,36)
 ELSE
    call read_period(12,24,fc_cycle,42)
 ENDIF
 
 IF(lprint_read) WRITE(6,*) fc_cycle(:,1)
 IF(lprint_read) WRITE(6,*) fc_cycle(:,2)

 WRITE(6,*)'OPEN ',TRIM(cdum2)

 READ(lunin,*,iostat=ierr)cdum3
 READ(lunin,*,iostat=ierr)cdum3
 READ(lunin,*,iostat=ierr)cdum3

 READ(lunin2,*,iostat=ierr)cdum3
 READ(lunin2,*,iostat=ierr)cdum3
 READ(lunin2,*,iostat=ierr)cdum3

 READ_LOOP_M  : DO
 READ_CYCLE_M : DO ii_cycle=1,3

    IF (fc_cycle(ii_cycle,2).eq.1) THEN

       DO j=1,fc_cycle(ii_cycle,1)

          READ(lunin,*,iostat=ierr)									&
          idat,itim,date,rhour,idum,swdh,lwdh,swnh,lwnh,wqh,wth,	&
          lsp,cvp,rdum,rdum,rdum,                             		&
          rdum,rdum,rdum,rdum,rdum,  	                      		&
          rdum,rdum,rdum,											&
          th,qh,uh,vh
      
          READ(lunin2,*,iostat=ierr2)								&
          idat2,itim2,date2,rhour2,idum2,uwh,vwh

          IF(ierr.eq.-1) EXIT READ_LOOP_M

          IF (ierr.ne.0) THEN

             WRITE(6,*)'Could not read file properly ',TRIM(cdum)
             WRITE(6,*)												&
             idat,itim,date,rhour,idum,rdum,rdum,swnh,lwnh,wqh,wth,	&
             lsp,cvp,rdum,rdum,rdum,                             	&
             rdum,rdum,rdum,rdum,rdum,                        		&
             rdum,rdum,rdum,										&
             th,qh,uh,vh

             CALL abort

          ENDIF

          IF (ierr2.ne.0) THEN
             WRITE(6,*)'Could not read file properly ',TRIM(cdum2)
             WRITE(6,*)												&
             idat2,itim2,date2,rhour2,idum2,uwh,vwh

             CALL abort

          ENDIF

          IF (lprint_read) WRITE(6,*)'Found:',idat,itim,date,rhour,i
          IF(date.LT.sdate) CYCLE 
          IF(date.GT.edate) EXIT  FILE_LOOP_M

          i = i + 1

          !  Era40

          IF (lprint_read) WRITE(6,*)'Time',idat,itim,date,rhour,i

          ALLOCATE(hir(k)%o(i)%date)
          ALLOCATE(hir(k)%o(i)%time)
          ALLOCATE(hir(k)%o(i)%nal(nexp,nfclengths,nparver))

          hir(k)%o(i)%date  = date
          hir(k)%o(i)%time  = NINT(rhour)
          hir(k)%o(i)%nal  = err_ind

          IF(pe_ind.GT.0) hir(k)%o(i)%nal(l,1,pe_ind)    = (lsp + cvp) * 3600.
          IF(ff_ind.GT.0) hir(k)%o(i)%nal(l,1,ff_ind)    = SQRT(uh*uh + vh*vh)
          IF(dd_ind.GT.0) hir(k)%o(i)%nal(l,1,dd_ind)    = atan2(uh,vh)*180./pi + 180.
          IF(tt_ind.GT.0) hir(k)%o(i)%nal(l,1,tt_ind)    = th - 273.15
          IF(wt_ind.GT.0) hir(k)%o(i)%nal(l,1,wt_ind)    = (wth + 0.61 * th * wqh * cp/levap)
          IF(nr_ind.GT.0) hir(k)%o(i)%nal(l,1,nr_ind)    = swnh + lwnh
          IF(sd_ind.GT.0) hir(k)%o(i)%nal(l,1,sd_ind)    = swdh 
          IF(su_ind.GT.0) hir(k)%o(i)%nal(l,1,su_ind)    = swdh - swnh
          IF(ld_ind.GT.0) hir(k)%o(i)%nal(l,1,ld_ind)    = lwdh 
          IF(lu_ind.GT.0) hir(k)%o(i)%nal(l,1,lu_ind)    = lwdh - lwnh
          IF(gr_ind.GT.0) hir(k)%o(i)%nal(l,1,gr_ind)    = swdh 
          IF(wq_ind.GT.0) hir(k)%o(i)%nal(l,1,wq_ind)    = wqh
          IF(gs_ind.GT.0) hir(k)%o(i)%nal(l,1,gs_ind)    =  swnh + lwnh + wqh + wth
          IF(gc_ind.GT.0) hir(k)%o(i)%nal(l,1,gc_ind)    =  swnh + lwnh + wqh + wth
          IF(hb_ind.GT.0) hir(k)%o(i)%nal(l,1,hb_ind)    =  0.
          IF(uw_ind.GT.0) hir(k)%o(i)%nal(l,1,uw_ind)    = - SQRT (uwh*uwh + vwh*vwh)

          IF (lprint_read) WRITE(6,*)'Dat',hir(k)%o(i)%nal(l,1,:)
          
       ENDDO

    ELSEIF (fc_cycle(ii_cycle,2).eq.0) THEN

       DO j=1,fc_cycle(ii_cycle,1)
          READ(lunin,*,IOSTAT=ierr)
       ENDDO

    ELSE

       WRITE(6,*)'Stupid programmer, error in fc_cycle',fc_cycle(ii_cycle,2)
       CALL abort

    ENDIF

 ENDDO READ_CYCLE_M
 ENDDO READ_LOOP_M

 CLOSE(lunin)
 cyear = cyear + 1

 ENDDO FILE_LOOP_M
 ENDDO EXP_LOOP

 hir(k)%ntim = i

 write(6,*) 'STATION : ',hir(k)%stnr
 write(6,*) 'FOUND TIMES MODEL',hir(k)%ntim,maxtim
 write(6,*) 'MODEL PERIOD',hir(k)%o(1)%date,hir(k)%o(hir(k)%ntim)%date
 hir(k)%active = .TRUE.

 ! Try to use database if requested
 IF(use_database) CALL write_database(k,1)

 ENDDO STATION_LOOP

 RETURN
END
