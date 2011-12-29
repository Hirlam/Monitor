SUBROUTINE setup_varprop

 !
 ! Setup the properties for the selected variables
 !
 ! Ulf Andrae, SMHI, 2011
 !

 USE DATA

 IMPLICIT NONE

 INTEGER :: i,j,k,wrk(mparver),nlev, &
            magn

 CHARACTER(LEN=4) :: cform = '(IX)'
 CHARACTER(LEN=9) :: clev = ' '


 TYPE(variable), POINTER :: v

 ! ---------------------------------------------------------------------

 ! Find the number of levels
 IF (ltemp ) THEN
    wrk = 0
    WHERE(lev_lst > 0 ) wrk = 1
    nlev = SUM(wrk)
 ELSE
    nlev = 1
 ENDIF

 ! Find the total number of variables
 i = 1
 DO WHILE ( varlist(i) /= '#' ) 
   i = i + 1
 ENDDO
 nparver = ( i - 1 ) * nlev


 ! Allocate
 ALLOCATE(varprop(0:nparver))

 ! Set the levels
 IF (ltemp ) THEN

    ! Init lev_typ

    i = 1
    k = 0
    DO WHILE ( varlist(i) /= '#' ) 
      DO j=1,nlev
        k = k + 1
        varprop(k)%id=TRIM(varlist(i))
        varprop(k)%lev=lev_lst(j)
      ENDDO
      i = i + 1
    ENDDO

 ELSE

    DO i=1,nparver
      varprop(i)%id=TRIM(varlist(i))
      varprop(i)%lev = 0
    ENDDO

 ENDIF


 !
 ! Set properties for the variables
 !

 DO i=1,nparver
  v => varprop(i)
  SELECT CASE(varprop(i)%id)
  CASE('TT')
    varprop(i) = variable(v%lev,0,tt_lim,tt_ulim,tt_llim,v%id,'Temperature','deg C')
  CASE('FF')
    varprop(i) = variable(v%lev,0,ff_lim,ff_ulim,ff_llim,v%id,'Wind speed','m/s')
  CASE('FX')
    varprop(i) = variable(v%lev,0,ff_lim,ff_ulim,ff_llim,v%id,'Max wind speed','m/s')
  CASE('GG')
    varprop(i) = variable(v%lev,0,ff_lim,ff_ulim,ff_llim,v%id,'Wind gust','m/s')
  CASE('GX')
    varprop(i) = variable(v%lev,0,ff_lim,ff_ulim,ff_llim,v%id,'Max wind gust','m/s')
  CASE('HG')
    varprop(i) = variable(v%lev,0,xx_lim,xx_ulim,xx_llim,v%id,'Height','m')
  CASE('LA')
    varprop(i) = variable(v%lev,0,xx_lim,xx_ulim,xx_llim,v%id,'Latitude','deg')
  CASE('FI')
    varprop(i) = variable(v%lev,0,fi_lim,fi_ulim,fi_llim,v%id,'Height','m')
  CASE('RH')
    varprop(i) = variable(v%lev,0,rh_lim,rh_ulim,rh_llim,v%id,'Relative Humidity','%')
  CASE('PS')
    varprop(i) = variable(v%lev,0,ps_lim,ps_ulim,ps_llim,v%id,'Surface Pressure','hPa')
  CASE('NN')
    varprop(i) = variable(v%lev,0,nn_lim,nn_ulim,nn_llim,v%id,'Cloud cover','octas')
  CASE('TD')
    varprop(i) = variable(v%lev,0,td_lim,td_ulim,td_llim,v%id,'Dew point temperature','deg C')
  CASE('TN')
    varprop(i) = variable(v%lev,0,tt_lim,tt_ulim,tt_llim,v%id,'Minimum temperature','deg C')
  CASE('TX')
    varprop(i) = variable(v%lev,0,tt_lim,tt_ulim,tt_llim,v%id,'Maximum temperature','deg C')
  CASE('VI')
    varprop(i) = variable(v%lev,0,vi_lim,vi_ulim,vi_llim,v%id,'Visibility','m')
  CASE('DD')
    varprop(i) = variable(v%lev,0,dd_lim,dd_ulim,dd_llim,v%id,'Wind direction','deg')
  CASE('WT')
    varprop(i) = variable(v%lev,0,wt_lim,wt_ulim,wt_llim,v%id,'Sensible heat flux','w/m^2')
  CASE('WQ')
    varprop(i) = variable(v%lev,0,wq_lim,wq_ulim,wq_llim,v%id,'Latent heat flux','w/m^2')
  CASE('QQ')
    varprop(i) = variable(v%lev,0,qq_lim,qq_ulim,qq_llim,v%id,'Specific humidity','g/Kg')
  CASE('SW')
    varprop(i) = variable(v%lev,0,sw_lim,sw_ulim,sw_llim,v%id,'Short wave radiation','W/m^2')
  CASE('UW')
    varprop(i) = variable(v%lev,0,uw_lim,uw_ulim,uw_llim,v%id,'Momentum flux','m^2/s^2')
  CASE('NR')
    varprop(i) = variable(v%lev,0,nr_lim,nr_ulim,nr_llim,v%id,'Net radiation','W/m^2')
  CASE('GR')
    varprop(i) = variable(v%lev,0,gr_lim,gr_ulim,gr_llim,v%id,'Global radiation','W/m^2')
  CASE('SU')
    varprop(i) = variable(v%lev,0,su_lim,su_ulim,su_llim,v%id,'Short wave radiation up','W/m^2')
  CASE('SD')
    varprop(i) = variable(v%lev,0,sd_lim,sd_ulim,sd_llim,v%id,'Short wave radiation down','W/m^2')
  CASE('LU')
    varprop(i) = variable(v%lev,0,lu_lim,lu_ulim,lu_llim,v%id,'Long wave radiation up','W/m^2')
  CASE('LD')
    varprop(i) = variable(v%lev,0,ld_lim,ld_ulim,ld_llim,v%id,'Long wave radiation down','W/m^2')
  CASE('LW')
    varprop(i) = variable(v%lev,0,lw_lim,lw_ulim,lw_llim,v%id,'Long wave radiation','W/m^2')
  CASE('GS')
    varprop(i) = variable(v%lev,0,xx_lim,xx_ulim,xx_llim,v%id,'Ground heat flux','W/m^2')
  CASE('GC')
    varprop(i) = variable(v%lev,0,xx_lim,xx_ulim,xx_llim,v%id,'Residual ground heat flux','W/m^2')
  CASE('HB')
    varprop(i) = variable(v%lev,0,xx_lim,xx_ulim,xx_llim,v%id,'Surface heat budget residual','W/m^2')
  CASE('PE')
    varprop(i) = variable(v%lev,12,pe_lim,pe_ulim,pe_llim,v%id,'Precipitation','mm')
  CASE('PD')
    varprop(i) = variable(v%lev,24,pd_lim,pd_ulim,pd_llim,v%id,'Precipitation','mm')
  CASE('RF')
    varprop(i) = variable(v%lev,0,xx_lim,xx_ulim,xx_llim,v%id,'Runoff','mm/day')
  CASE('TU')
    varprop(i) = variable(v%lev,0,xx_lim,xx_ulim,xx_llim,v%id,'dT/dz/dU/dz','Ks/m')
  CASE('UZ')
    varprop(i) = variable(v%lev,0,xx_lim,xx_ulim,xx_llim,v%id,'dU/dz','s^-1')
  CASE('TZ')
    varprop(i) = variable(v%lev,0,xx_lim,xx_ulim,xx_llim,v%id,'dT/dz','deg C/m')
  CASE('WP')
    varprop(i) = variable(v%lev,0,xx_lim,xx_ulim,xx_llim,v%id,'Wind power','kW')
  CASE('WH')
    varprop(i) = variable(v%lev,0,xx_lim,xx_ulim,xx_llim,v%id,'Energy','kWh')
  CASE DEFAULT
    WRITE(6,*)'Warning, not default settings for ',v%id
    varprop(i) = variable(v%lev,0,xx_lim,xx_ulim,xx_llim,v%id,v%id,v%id)
  END SELECT
 ENDDO

 i = 1
 DO WHILE ( setprop(i)%id /= '#' .OR. i <= nparver ) 
   DO j=1,nparver

      v => varprop(j)

      WRITE(6,*)' CHECK ', setprop(i)%id , v%id
      IF ( v%id  == setprop(i)%id  .AND. &
           (v%lev == setprop(i)%lev .OR. setprop(i)%lev == -1 ) ) THEN
      WRITE(6,*)' MATCH ', setprop(i)%id , v%id

         IF ( setprop(i)%text /= '#' ) v%text = setprop(i)%text
         IF ( setprop(i)%unit /= '#' ) v%unit = setprop(i)%unit
         IF ( setprop(i)%acc /=  -1  ) v%acc  = setprop(i)%acc

         IF ( ABS(setprop(i)%lim -err_ind) > 1.e-6  ) v%lim  = setprop(i)%lim
         IF ( ABS(setprop(i)%ulim-err_ind) > 1.e-6  ) v%ulim = setprop(i)%ulim
         IF ( ABS(setprop(i)%llim-err_ind) > 1.e-6  ) v%llim = setprop(i)%llim

      ENDIF

   ENDDO
   i = i+1
   
 ENDDO

 !
 ! Set name for multilayer variables
 !

 IF ( ltemp ) THEN
   IF ( lev_lst(1) > lev_lst(2) ) THEN
     vert_unit = 'hPa'
   ELSE 
     vert_unit = 'm'
   ENDIF
   DO j=1,nparver
     v => varprop(j)
     magn=FLOOR(LOG10(FLOAT(v%lev)))+1
     WRITE(cform(3:3),'(I1)')magn
     WRITE(clev,cform)v%lev
     v%text = TRIM(v%text)//' '//TRIM(clev)//TRIM(vert_unit)
   ENDDO
 ENDIF

 !
 ! Set unit for accumulated variables
 ! Disabled, done from namelist
 !

!DO j=1,nparver
!  v => varprop(j)
!  IF ( v%acc == 0 ) CYCLE
!  magn=FLOOR(LOG10(FLOAT(v%acc)))+1
!  WRITE(cform(3:3),'(I1)')magn
!  WRITE(clev,cform)v%acc
!  v%unit = TRIM(v%unit)//'/'//TRIM(clev)//'h'
!ENDDO

 !
 ! Summarize the settings
 !

 WRITE(6,*)
 WRITE(6,*)' Variable setting summary '

 DO i=1,nparver
  WRITE(6,*)
  WRITE(6,*)'NAME:',TRIM(varprop(i)%text)
  WRITE(6,*)'ID:  ',TRIM(varprop(i)%id)
  WRITE(6,*)'UNIT:',TRIM(varprop(i)%unit)
  WRITE(6,*)'LEVEL:',varprop(i)%lev
  WRITE(6,*)'ACC:',varprop(i)%acc
  WRITE(6,*)'LOW/UP LIM:',varprop(i)%llim,varprop(i)%ulim
 ENDDO
 WRITE(6,*)

END SUBROUTINE setup_varprop
