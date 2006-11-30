SUBROUTINE read_windp_mod

 USE constants
 USE data, only : obs,hir,expname,maxstn,    &
                  nexp,nparver,nfclengths,   &
                  err_ind,                   &
                  sdate,edate,stime,etime,   &
                  stnlist,print_read,        &
                  modpath,lunin,             &
                  fclen,wp_ind,fcint,        &
                  allocate_mod,ff_ind,       &
                  dd_ind,tt_ind,tz_ind,uz_ind,tu_ind,             &
                  special_flag
 USE windp

 IMPLICIT NONE

 INTEGER, PARAMETER :: maxz = 61

 INTEGER :: i,j,jj,k,kk,l,ierr,                 &
            yy,mm,dd,hh,                        &
            wdate,wtime,odate,otime,            &
            cdate,ctime,                        &
            u_index,jj_scale,                   &
            ab_lev_ind,                         &
            z_ind,read_counter

 REAL :: Pmsl, ps, ts, t2, u10m, v10m,          &
         uu,vv,w,psl,psu,wp,zd,tt,              &
         tz,uz,tu,vz,                           &
         z(57:61),                              &
         t(57:61),                              &
         v(57:61),                              &
         u(57:61)

 LOGICAL :: time_is_not_allocated = .TRUE.


 CHARACTER(LEN=99) :: cname =''
 CHARACTER(LEN=99) :: windp_name(maxwindp)
 CHARACTER(LEN= 3) :: cstn  = ''
 CHARACTER(LEN= 8) :: datec = ''

 ! ------------------------------------------------------------------

 ! Setup windp
 CALL init_windp

 CALL allocate_mod

 hir%stnr = stnlist

 !
 ! Loop over all times
 !

 STATION_LOOP : DO i=1,maxstn
 
  windp_index = 0
  DO j=1,maxwindp
     IF (hir(i)%stnr == windp_stations(j)%nr ) THEN
        WRITE(cstn ,'(I3.3)')windp_stations(j)%nr
        windp_index = j
     ENDIF
  ENDDO

  IF ( windp_index == 0 ) CYCLE STATION_LOOP

  IF(print_read > 0)WRITE(6,*)'Found station :',cstn

  CALL print_windp_station

  hir(i)%active = .TRUE.

  j = 0

  cdate = sdate
  ctime = stime

  !
  ! Set lat and lon
  !

  hir(i)%lat = windp_stations(windp_index)%lat
  hir(i)%lon = windp_stations(windp_index)%lon

  obs(i)%lat = windp_stations(windp_index)%lat
  obs(i)%lon = windp_stations(windp_index)%lon

 
 TIME_LOOP : DO 

    !
    ! Read model data
    !

    time_is_not_allocated = .TRUE.

    EXP_LOOP : DO k=1,nexp

       WRITE(datec,'(I8.8)')cdate

       cname = TRIM(modpath(k))//TRIM(expname(k))   &
               //'_'//cstn//'_'//datec//'00.txt'

       OPEN(lunin,file=cname,status='old',iostat=ierr)
   
       IF (ierr.NE.0) THEN
     
          IF(print_read > 0)WRITE(6,*)'Could not open:',TRIM(cname)
          CYCLE EXP_LOOP
   
       ENDIF

       jj = 0

       IF (print_read > 0 )WRITE(6,*)'READ ',TRIM(cname)

       read_counter = 0
       DAY_TIME_LOOP : DO 
   
          READ(lunin,*,iostat=ierr)                &
            Pmsl, ps, ts, t2, u10m, v10m,          &
            t(60), u(60), v(60),                   &
            t(59), u(59), v(59),                   &
            t(58), u(58), v(58),                   &
            t(57), u(57), v(57)
   
          IF ( ierr /= 0 ) EXIT DAY_TIME_LOOP

          IF ( TRIM(expname(k)) == 'al026' ) THEN
             ts = ts - tzero
             t2 = t2 - tzero
             t  = t  - tzero
          ENDIF

          read_counter = read_counter + 1

          IF ( .NOT. TRIM(expname(k)) == 'RL05'     &
               .AND. MOD(read_counter,24/nfclengths) /= 0 ) CYCLE DAY_TIME_LOOP

          ! Calculate z

          IF ( TRIM(expname(k)) == 'RL05' ) THEN
             ab_lev_ind = 1
          ELSEIF ( TRIM(expname(k)) == 'al026' ) THEN
             ab_lev_ind = 3
          ELSE
             ab_lev_ind = 2
          ENDIF

          z_ind = 0
          z     = 0.

          DO l=60,57,-1

             psl =  ab_half(ab_lev_ind,l+1,1) +  ab_half(ab_lev_ind,l+1,2) * ps * 100.
             psu =  ab_half(ab_lev_ind,l  ,1) +  ab_half(ab_lev_ind,l  ,2) * ps * 100. 
             
             !
             ! Pressure is on half levels but we need full levles
             !

             z(l) = z(l+1) + ALOG ( 2* psl / (psu+psl) ) * rair * ( t(l) + tzero ) / gravit

             IF ( windp_stations(windp_index)%hubhgt >  z(l+1) .AND.           &
                  windp_stations(windp_index)%hubhgt <= z(l  )       ) THEN
                  z_ind = l
                  EXIT
             ENDIF
          ENDDO

          IF ( z_ind == 0 ) THEN
 !                WRITE(6,*) 'Could not find any reasonable level'
 !                WRITE(6,*)'Hub' ,windp_stations(windp_index)%hubhgt 
 !                WRITE(6,*)'Z' ,z
 !                WRITE(6,*)'Set z_ind to 57'
                  z_ind = 57
          ENDIF

!         ! Interpolate wind linear between leves
          w = (windp_stations(windp_index)%hubhgt -  z(z_ind+1)) /    &
                          (z(z_ind) -  z(z_ind+1)) 

          uu = u(z_ind) * ( 1-w) + u(z_ind+1) *w
          vv = v(z_ind) * ( 1-w) + v(z_ind+1) *w
          tt = t(z_ind) * ( 1-w) + t(z_ind+1) *w + tzero

          ! Calculate gradient
          tz = (t(z_ind) - t(z_ind+1)) / (z(z_ind) - z(z_ind+1))
          uz = (u(z_ind) - u(z_ind+1)) / (z(z_ind) - z(z_ind+1))
          vz = (v(z_ind) - v(z_ind+1)) / (z(z_ind) - z(z_ind+1))

          tu = gravit * tz / ( tt * (uz**2 + vz**2 )) 

          uz = SQRT(u(z_ind)**2+v(z_ind)**2) - SQRT(u(z_ind+1)**2+v(z_ind+1)**2)
          uz = uz / (z(z_ind) - z(z_ind+1))

          zd     =  atan2(uu,vv)*180./pi + 180.

          uu =  SQRT(uu*uu+vv*vv) 


          !
          ! Station is found, add time
          !
      
          jj = jj + 1
      
          IF ( time_is_not_allocated ) THEN

             j  =  j + 1

             ALLOCATE(hir(i)%o(j)%date)
             ALLOCATE(hir(i)%o(j)%time)
             ALLOCATE(hir(i)%o(j)%nal(nexp,nfclengths,nparver))
             hir(i)%o(j)%nal = err_ind
      
             hir(i)%ntim      = j
             hir(i)%o(j)%nal  = err_ind
             hir(i)%o(j)%date = cdate
             hir(i)%o(j)%time = 00

             time_is_not_allocated = .FALSE.

          ENDIF
          
          jj_scale = jj

          IF ( TRIM(expname(k)) == 'RL05' ) THEN
             !
             ! Special for ECMWF data 
             ! Only available with 3h interval
             !
                  jj_scale = 0
                  DO  kk =1,nfclengths
                      IF ( fclen(kk) == 3*jj ) jj_scale = kk
                  ENDDO
                  IF ( jj_scale == 0 ) CALL abort
          ENDIF

          ! Find interpolation weight for powercurve
          u_index =  FLOOR(uu)
          w       = uu-u_index
          wp      =                                                           &
            wind_map(windp_stations(windp_index)%typ_ind,u_index  ,1) * (1-w) &
          + wind_map(windp_stations(windp_index)%typ_ind,u_index+1,1) * ( w )  

          IF (ff_ind /= 0) hir(i)%o(j)%nal(k,jj_scale,ff_ind)  = uu
          IF (dd_ind /= 0) hir(i)%o(j)%nal(k,jj_scale,dd_ind)  = zd
          IF (tz_ind /= 0) hir(i)%o(j)%nal(k,jj_scale,tz_ind)  = tz
          IF (uz_ind /= 0) hir(i)%o(j)%nal(k,jj_scale,uz_ind)  = uz
          IF (tu_ind /= 0) hir(i)%o(j)%nal(k,jj_scale,tu_ind)  = tu

          SELECT CASE(special_flag) 

          CASE (1)
             IF (wp_ind /= 0) hir(i)%o(j)%nal(k,jj_scale,wp_ind)  = SQRT(wp)
          CASE DEFAULT
             IF (wp_ind /= 0) hir(i)%o(j)%nal(k,jj_scale,wp_ind)  = wp
          END SELECT

          IF ( jj_scale == nfclengths ) EXIT DAY_TIME_LOOP
   
       ENDDO DAY_TIME_LOOP

       CLOSE(lunin)

    ENDDO EXP_LOOP

    !
    ! Step time
    !

    wdate = cdate
    wtime = ctime
    CALL adddtg(wdate,wtime,fcint*3600,cdate,ctime)


    IF(cdate >  edate) EXIT TIME_LOOP
    IF(cdate >= edate .AND. ctime/10000 > etime) EXIT TIME_LOOP

  ENDDO TIME_LOOP

 ENDDO STATION_LOOP

 DO i=1,maxstn
    hir(i)%active = ( hir(i)%ntim > 0 )
 ENDDO

 RETURN

END SUBROUTINE read_windp_mod
