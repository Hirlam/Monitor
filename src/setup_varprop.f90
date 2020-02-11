SUBROUTINE setup_varprop

 !
 ! Setup the properties for the selected variables
 !
 ! Ulf Andrae, SMHI, 2011-2017
 !

 USE DATA

 IMPLICIT NONE

 INTEGER :: i,j,k,wrk(mparver),nlev, &
            magn,obswrk(0:23)

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
        varprop(k)%lev=INT(lev_lst(j))
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
  CASE('TT','TTHA','TTP1','TTP2')
    varprop(i) = variable(v%lev,0,0,50.,400.,-200.,v%id,'Temperature','deg C',.FALSE.)
  CASE('FF','FFP1','FFP2')
    varprop(i) = variable(v%lev,0,0,50.,200.,0.,v%id,'Wind speed','m/s',.FALSE.)
  CASE('FX')
    varprop(i) = variable(v%lev,0,0,50.,200.,0.,v%id,'Max wind speed','m/s',.FALSE.)
  CASE('GG')
    varprop(i) = variable(v%lev,0,0,50.,200.,0.,v%id,'Wind gust','m/s',.FALSE.)
  CASE('GX')
    varprop(i) = variable(v%lev,6,3,50.,200.,0.,v%id,'Max wind gust','m/s',.FALSE.)
  CASE('HG')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Height','m',.FALSE.)
  CASE('LA')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Latitude','deg',.FALSE.)
  CASE('FI')
    varprop(i) = variable(v%lev,0,0,500.,1.e9,0.,v%id,'Height','m',.FALSE.)
  CASE('RH')
    varprop(i) = variable(v%lev,0,0,100.,120.,0.,v%id,'Relative Humidity','%',.FALSE.)
  CASE('PS')
    varprop(i) = variable(v%lev,0,0,50.,1100.,0.,v%id,'Surface Pressure','hPa',.FALSE.)
  CASE('SPS')
    varprop(i) = variable(v%lev,0,0,50.,1100.,0.,v%id,'Station Pressure','hPa',.FALSE.)
  CASE('NN')
    varprop(i) = variable(v%lev,0,0,10.,8.,0.,v%id,'Cloud cover','octas',.FALSE.)
  CASE('TD','TDP1','TDP2')
    varprop(i) = variable(v%lev,0,0,50.,400.,-200.,v%id,'Dew point temperature','deg C',.FALSE.)
  CASE('TN','TNP1','TNP2')
    varprop(i) = variable(v%lev,12,2,50.,400.,-200.,v%id,'Minimum temperature','deg C',.FALSE.)
  CASE('TX','TXP1','TXP2')
    varprop(i) = variable(v%lev,12,3,50.,400.,-200.,v%id,'Maximum temperature','deg C',.FALSE.)
  CASE('VI')
    varprop(i) = variable(v%lev,0,0,5.e5,1.e12,0.,v%id,'Visibility','m',.FALSE.)
  CASE('DD','DDP1','DDP2')
    varprop(i) = variable(v%lev,0,0,720.,360.,0.,v%id,'Wind direction','deg',.FALSE.)
  CASE('WT')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Sensible heat flux','w/m^2',.FALSE.)
  CASE('WQ')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Latent heat flux','w/m^2',.FALSE.)
  CASE('QQ','QQP1','QQP2')
    varprop(i) = variable(v%lev,0,0,50.,100.,0.,v%id,'Specific humidity','g/Kg',.FALSE.)
  CASE('SW')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Short wave radiation','W/m^2',.FALSE.)
  CASE('UW')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Momentum flux','m^2/s^2',.FALSE.)
  CASE('NR')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Net radiation','W/m^2',.FALSE.)
  CASE('GR')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Global radiation','W/m^2',.FALSE.)
  CASE('SU')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Short wave radiation up','W/m^2',.FALSE.)
  CASE('SD')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Short wave radiation down','W/m^2',.FALSE.)
  CASE('LU')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Long wave radiation up','W/m^2',.FALSE.)
  CASE('LD')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Long wave radiation down','W/m^2',.FALSE.)
  CASE('LW')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Long wave radiation','W/m^2',.FALSE.)
  CASE('GS')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Ground heat flux','W/m^2',.FALSE.)
  CASE('GC')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Residual ground heat flux','W/m^2',.FALSE.)
  CASE('HB')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Surface heat budget residual','W/m^2',.FALSE.)
  CASE('PE')
    varprop(i) = variable(v%lev,12,0,50.,500.,0.,v%id,'Precipitation','mm',.FALSE.)
  CASE('PD')
    varprop(i) = variable(v%lev,24,0,50.,500.,0.,v%id,'Precipitation','mm',.FALSE.)
  CASE('RF')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Runoff','mm/day',.FALSE.)
  CASE('TU')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'dT/dz/dU/dz','Ks/m',.FALSE.)
  CASE('UZ')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'dU/dz','s^-1',.FALSE.)
  CASE('TZ')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'dT/dz','deg C/m',.FALSE.)
  CASE('WP')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Wind power','kW',.FALSE.)
  CASE('WH')
    varprop(i) = variable(v%lev,0,0,err_ind,err_ind,err_ind,v%id,'Energy','kWh',.FALSE.)
  CASE DEFAULT
    varprop(i) = variable(v%lev,0,0,err_ind,-err_ind,err_ind,v%id,v%id,v%id)
  END SELECT
 ENDDO

 !
 ! Copy namelist settings
 !

 i = 1
 DO WHILE ( setprop(i)%id /= '#' .OR. i <= nparver ) 
   DO j=1,nparver

      v => varprop(j)

      IF ( v%id  == setprop(i)%id  .AND. &
           (v%lev == setprop(i)%lev .OR. setprop(i)%lev == -1 ) ) THEN

         v%active = setprop(i)%active 

         IF ( setprop(i)%text /= '#'   ) v%text     = setprop(i)%text
         IF ( setprop(i)%unit /= '#'   ) v%unit     = setprop(i)%unit
         IF ( setprop(i)%acc /=  -1    ) v%acc      = setprop(i)%acc
         IF ( setprop(i)%acctype /= -1 ) v%acctype  = setprop(i)%acctype

         IF ( ABS(setprop(i)%lim -err_ind) > 1.e-6  ) v%lim  = setprop(i)%lim
         IF ( ABS(setprop(i)%ulim-err_ind) > 1.e-6  ) v%ulim = setprop(i)%ulim
         IF ( ABS(setprop(i)%llim-err_ind) > 1.e-6  ) v%llim = setprop(i)%llim

      ENDIF

   ENDDO
   i = i+1
   
 ENDDO

 ! Set obstime 
 DO i=1,nparver
   IF ( ALL(obstime(i,:) == -1) ) THEN
     obstime(i,:) = 1
   ELSEIF ( ANY(obstime(i,:) /= -1) ) THEN
     obswrk(:) = obstime(i,:)
     obstime(i,:) = 0
     DO k=0,23
      IF ( obswrk(k) /= -1 ) obstime(i,obswrk(k)) = 1
     ENDDO
   ENDIF
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
 ! Summarize the settings
 !

 WRITE(6,*)
 WRITE(6,*)' Variable setting summary '

 DO i=1,nparver
  WRITE(6,*)'Variable:',i
  WRITE(6,*)' NAME:',TRIM(varprop(i)%text)
  WRITE(6,*)' ID:  ',TRIM(varprop(i)%id)
  WRITE(6,*)' UNIT:',TRIM(varprop(i)%unit)
  WRITE(6,*)' LEVEL:',varprop(i)%lev
  WRITE(6,*)' ACC/ACCTYPE:',varprop(i)%acc,varprop(i)%acctype
  WRITE(6,*)' LOW/UP LIM:',varprop(i)%llim,varprop(i)%ulim
  WRITE(6,*)' ACTIVE:',varprop(i)%active
  WRITE(6,*)' OBSTIME:',obstime(i,:)
 ENDDO
 WRITE(6,*)

END SUBROUTINE setup_varprop
