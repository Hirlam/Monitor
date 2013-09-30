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
  CASE('TT','TTHA')
    varprop(i) = variable(v%lev,0,50.,400.,-200.,v%id,'Temperature','deg C')
  CASE('FF')
    varprop(i) = variable(v%lev,0,50.,200.,0.,v%id,'Wind speed','m/s')
  CASE('FX')
    varprop(i) = variable(v%lev,0,50.,200.,0.,v%id,'Max wind speed','m/s')
  CASE('GG')
    varprop(i) = variable(v%lev,0,50.,200.,0.,v%id,'Wind gust','m/s')
  CASE('GX')
    varprop(i) = variable(v%lev,0,50.,200.,0.,v%id,'Max wind gust','m/s')
  CASE('HG')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Height','m')
  CASE('LA')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Latitude','deg')
  CASE('FI')
    varprop(i) = variable(v%lev,0,500.,1.e9,0.,v%id,'Height','m')
  CASE('RH')
    varprop(i) = variable(v%lev,0,100.,100.,0.,v%id,'Relative Humidity','%')
  CASE('PS')
    varprop(i) = variable(v%lev,0,50.,1100.,0.,v%id,'Surface Pressure','hPa')
  CASE('NN')
    varprop(i) = variable(v%lev,0,10.,8.,0.,v%id,'Cloud cover','octas')
  CASE('TD')
    varprop(i) = variable(v%lev,0,50.,400.,-200.,v%id,'Dew point temperature','deg C')
  CASE('TN')
    varprop(i) = variable(v%lev,0,50.,400.,-200.,v%id,'Minimum temperature','deg C')
  CASE('TX')
    varprop(i) = variable(v%lev,0,50.,400.,-200.,v%id,'Maximum temperature','deg C')
  CASE('VI')
    varprop(i) = variable(v%lev,0,5.e5,1.e12,0.,v%id,'Visibility','m')
  CASE('DD')
    varprop(i) = variable(v%lev,0,720.,360.,0.,v%id,'Wind direction','deg')
  CASE('WT')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Sensible heat flux','w/m^2')
  CASE('WQ')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Latent heat flux','w/m^2')
  CASE('QQ')
    varprop(i) = variable(v%lev,0,50.,100.,0.,v%id,'Specific humidity','g/Kg')
  CASE('SW')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Short wave radiation','W/m^2')
  CASE('UW')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Momentum flux','m^2/s^2')
  CASE('NR')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Net radiation','W/m^2')
  CASE('GR')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Global radiation','W/m^2')
  CASE('SU')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Short wave radiation up','W/m^2')
  CASE('SD')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Short wave radiation down','W/m^2')
  CASE('LU')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Long wave radiation up','W/m^2')
  CASE('LD')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Long wave radiation down','W/m^2')
  CASE('LW')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Long wave radiation','W/m^2')
  CASE('GS')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Ground heat flux','W/m^2')
  CASE('GC')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Residual ground heat flux','W/m^2')
  CASE('HB')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Surface heat budget residual','W/m^2')
  CASE('PE')
    varprop(i) = variable(v%lev,12,50.,500.,0.,v%id,'Precipitation','mm')
  CASE('PD')
    varprop(i) = variable(v%lev,24,50.,500.,0.,v%id,'Precipitation','mm')
  CASE('RF')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Runoff','mm/day')
  CASE('TU')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'dT/dz/dU/dz','Ks/m')
  CASE('UZ')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'dU/dz','s^-1')
  CASE('TZ')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'dT/dz','deg C/m')
  CASE('WP')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Wind power','kW')
  CASE('WH')
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,'Energy','kWh')
  CASE DEFAULT
    varprop(i) = variable(v%lev,0,err_ind,err_ind,err_ind,v%id,v%id,v%id)
  END SELECT
 ENDDO

 i = 1
 DO WHILE ( setprop(i)%id /= '#' .OR. i <= nparver ) 
   DO j=1,nparver

      v => varprop(j)

      IF ( v%id  == setprop(i)%id  .AND. &
           (v%lev == setprop(i)%lev .OR. setprop(i)%lev == -1 ) ) THEN

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
