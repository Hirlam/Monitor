SUBROUTINE plot_scat_comp(lunout,nparver,nr,nrun,    &
           diff_ind,scat,p1,p2,par_active)

 !
 ! Cross scatter plots
 ! 
 ! -1000 < diff_ind <    0 : 
 !  Plot model variable diff_ind as a function of observation variable 1 ... N
 !     0 < diff_ind < 1000 :
 !  Plot observation of variable 1 as a function of model variable 1 ... N
 ! 
 ! diff_ind < -1000 : 
 !  Plot model variable 1 ... N  as a function of observation variable diff_ind
 ! diff_ind >  1000 :
 !  Plot observation variable 1 ... N as a function of model variable diff_ind
 !
 ! ldiff = TRUE will give the bias instead of full value
 ! 
 ! Ulf Andrae, SMHI, 2006
 !

 USE types
 USE timing
 USE data, ONLY : obstype,expname,err_ind,nexp,station_name,csi, &
                  fclen,nfclengths,                              &
                  show_fc_length,ltiming,ldiff,output_type
 USE mymagics

 IMPLICIT NONE

 ! INPUT

 INTEGER,            INTENT(IN) :: lunout,nparver,nr,nrun,diff_ind,p1,p2
 INTEGER,            INTENT(IN) :: par_active(nparver)
 TYPE(scatter_type), INTENT(IN) :: scat(nparver)

 ! LOCAL

 INTEGER :: i,j,jj,k,kk,timing_id,miff_ind
 CHARACTER(LEN= 40) :: fname=' '
 CHARACTER(LEN=100) :: wtext  =' ', &
                       wtext1 =' ', &
                       wtext2 =' ', &
                       wtext3 =' ', &
                       wtext4 =' ', &
                       wname  =' '
 CHARACTER(LEN=  1) :: prefix = ' '
 CHARACTER(LEN= 80) :: title =' '

 REAL :: miny,maxy,minx,maxx,rtmp,diff_flag

 REAL, ALLOCATABLE :: xval(:),yval(:)

 LOGICAL :: scatflag,lcomp


 !
 !-----------------------------------------------------
 !


 !
 ! Init timing counter
 !

 timing_id = 0
 IF (ltiming) CALL acc_timing(timing_id,'plot_scatt_comp')

 ALLOCATE(xval(MAXVAL(scat%n)))
 ALLOCATE(yval(MAXVAL(scat%n)))

 SELECT CASE(diff_ind)
 CASE(:-1001)
     prefix = 'E'
     lcomp  = .TRUE.
     miff_ind = diff_ind + 1000
 CASE(-1000:-1)
     prefix  = 'C'
     lcomp  = .FALSE.
     miff_ind = diff_ind 
 CASE(1:1000)
     prefix = 'c'
     lcomp  = .FALSE.
     miff_ind = diff_ind 
 CASE(1001:)
     prefix = 'e'
     lcomp  = .TRUE.
     miff_ind = diff_ind - 1000
 CASE DEFAULT
     WRITE(6,*)'No such option in diff_ind'
     CALL abort
 END SELECT

 IF ( ldiff ) THEN
     diff_flag = 0.0
     scatflag = .FALSE.
 ELSE
     scatflag = .TRUE.
     diff_flag = 1.0
 ENDIF
 
 !WRITE(6,*)
 !WRITE(6,*)'Choices in plot_scat_comp:'
 !WRITE(6,*)'prefix   ',prefix
 !WRITE(6,*)'lcomp    ',lcomp
 !WRITE(6,*)'miff_ind ',miff_ind
 !WRITE(6,*)'diff_ind ',diff_ind
 !WRITE(6,*)


  ! Set filename
  CALL make_fname(prefix,p1,nr,nrun,fname,output_type)

  ! Open ps file
  CALL open_output(fname)

  CALL PSETC ('TEXT_COLOUR', 'BLACK')

  CALL psetc('PAGE_ID_LINE_SYSTEM_PLOT','ON')
  CALL psetc('PAGE_ID_LINE_ERRORS_PLOT','OFF')
  CALL psetc('PAGE_ID_LINE_DATE_PLOT','ON')
  CALL psetc('PAGE_ID_LINE_QUALITY','HIGH')
  CALL psetc('PAGE_ID_LINE_LOGO_PLOT','OFF')

  DO j=1,nparver

   ! Set title and axis names
   IF (nr > 0) THEN
      IF(ALLOCATED(station_name)) THEN
        WRITE(title,'(A,A)')'Difference scatterplot ',	&
        TRIM(station_name(csi))
      ELSE
        WRITE(title,'(A,I)')'Difference scatterplot for station ',nr
      ENDIF
   ELSE
      title='Difference scatterplot for      stations'
      WRITE(title(28:31),'(I4)')par_active(j)
   ENDIF

     ! Find min and max
     miny = 0.
     minx = 0.
     maxy = 1.
     maxx = 1.

     kk = scat(j)%n

     IF (kk > 0 )THEN

      IF ( lcomp ) THEN
        IF ( miff_ind < 0 ) THEN
           minx =  MINVAL(scat(ABS(miff_ind))%dat(1,1:kk))
           maxx =  MAXVAL(scat(ABS(miff_ind))%dat(1,1:kk))
        ELSE IF ( miff_ind > 0 ) THEN
           minx =  MINVAL(scat(miff_ind)%dat(1+1,1:kk) + scat(miff_ind)%dat(1,1:kk))
           maxx =  MAXVAL(scat(miff_ind)%dat(1+1,1:kk) + scat(miff_ind)%dat(1,1:kk))
        ENDIF

        miny =  MINVAL(scat(ABS(j))%dat(1+1,1:kk) + diff_flag * scat(ABS(j))%dat(1,1:kk))
        maxy =  MAXVAL(scat(ABS(j))%dat(1+1,1:kk) + diff_flag * scat(ABS(j))%dat(1,1:kk))

        DO jj=2,nexp
          rtmp =  MINVAL(scat(ABS(j))%dat(1+jj,1:kk) + diff_flag * scat(ABS(j))%dat(1,1:kk))
          miny =  MIN(rtmp,miny)
          rtmp =  MAXVAL(scat(ABS(j))%dat(1+jj,1:kk) + diff_flag * scat(ABS(j))%dat(1,1:kk))
          maxy =  MAX(rtmp,maxy)

        IF ( miff_ind > 0 ) THEN
           rtmp =  MINVAL(scat(miff_ind)%dat(1+jj,1:kk) + scat(miff_ind)%dat(1,1:kk))
           minx =  MAX(rtmp,minx)
           rtmp =  MAXVAL(scat(miff_ind)%dat(1+jj,1:kk) + scat(miff_ind)%dat(1,1:kk))
           maxx =  MAX(rtmp,maxx)
        ENDIF

        ENDDO

      ELSE

        IF ( miff_ind < 0 ) THEN
           minx =  MINVAL(scat(j)%dat(1,1:kk))
           maxx =  MAXVAL(scat(j)%dat(1,1:kk))
        ELSE IF ( miff_ind > 0 ) THEN
           minx =  MINVAL(scat(j)%dat(1,1:kk) + scat(j)%dat(2,1:kk))
           maxx =  MAXVAL(scat(j)%dat(1,1:kk) + scat(j)%dat(2,1:kk))
        ENDIF

        miny =  MINVAL(scat(ABS(miff_ind))%dat(1+1,1:kk) + diff_flag * scat(ABS(miff_ind))%dat(1,1:kk))
        maxy =  MAXVAL(scat(ABS(miff_ind))%dat(1+1,1:kk) + diff_flag * scat(ABS(miff_ind))%dat(1,1:kk))

        DO jj=2,nexp
          rtmp =  MINVAL(scat(ABS(miff_ind))%dat(1+jj,1:kk) + diff_flag * scat(ABS(miff_ind))%dat(1,1:kk))
          miny =  MIN(rtmp,miny)
          rtmp =  MAXVAL(scat(ABS(miff_ind))%dat(1+jj,1:kk) + diff_flag * scat(ABS(miff_ind))%dat(1,1:kk))
          maxy =  MAX(rtmp,maxy)

          IF ( diff_ind > 0 ) THEN
             rtmp =  MINVAL(scat(j)%dat(1+jj,1:kk) + scat(j)%dat(1,1:kk))
             minx =  MAX(rtmp,minx)
             rtmp =  MAXVAL(scat(j)%dat(1+jj,1:kk) + scat(j)%dat(1,1:kk))
             maxx =  MAX(rtmp,maxx)
          ENDIF

        ENDDO
      ENDIF

     ENDIF

     DO jj=1,nexp

        IF ( ldiff ) THEN
           IF ( lcomp ) THEN
              IF (miff_ind > 0) THEN
                 wtext1 = TRIM(expname(jj))//' - OBS '//TRIM(obstype(j))
                 wtext2= 'MOD '//TRIM(obstype(miff_ind))
              ELSEIF (miff_ind < 0) THEN
                 wtext1 = TRIM(expname(jj))//' - OBS '//TRIM(obstype(j))
                 wtext2= 'OBS '//TRIM(obstype(-miff_ind))
              ENDIF
           ELSE
              IF (miff_ind > 0) THEN
                 wtext1 = TRIM(expname(jj))//' - OBS '//TRIM(obstype(miff_ind))
                 wtext2= 'MOD '//TRIM(obstype(j))
              ELSEIF (miff_ind < 0) THEN
                 wtext1 = TRIM(expname(jj))//' - OBS '//TRIM(obstype(-miff_ind))
                 wtext2= 'OBS '//TRIM(obstype(j))
              ENDIF
           ENDIF
        ELSE
           IF ( lcomp ) THEN
              IF (miff_ind > 0) THEN
                 wtext1 = TRIM(expname(jj))//' '//TRIM(obstype(j))
                 wtext2= 'MOD '//TRIM(obstype(miff_ind))
              ELSEIF (miff_ind < 0) THEN
                 wtext1 = TRIM(expname(jj))//' '//TRIM(obstype(j))
                 wtext2= 'OBS '//TRIM(obstype(-miff_ind))
              ENDIF
           ELSE
              IF (miff_ind > 0) THEN
                 wtext1 = TRIM(expname(jj))//' '//TRIM(obstype(miff_ind))
                 wtext2= 'MOD '//TRIM(obstype(j))
              ELSEIF (miff_ind < 0) THEN
                 wtext1 = TRIM(expname(jj))//' '//TRIM(obstype(-miff_ind))
                 wtext2= 'OBS '//TRIM(obstype(j))
              ENDIF
           ENDIF
        ENDIF

        IF ( show_fc_length ) THEN
        IF (nfclengths > 10 ) THEN
           wname='(A,2I3.2,A5,I2.2)'
           WRITE(wtext3,wname)'Forecast lengths used:',   &
           fclen(1:2),' ... ',fclen(nfclengths)
        ELSE
           wname='(A,XX(1X,I2.2))'
           WRITE(wname(4:5),'(I2.2)')nfclengths
           WRITE(wtext3,wname)'Forecast lengths used:',fclen(1:nfclengths)
           CALL PSETC('TEXT_LINE_4',wtext)
        ENDIF
        ENDIF

        kk = scat(j)%n

        IF ( lcomp ) THEN
           IF ( miff_ind < 0 ) THEN
              xval(1:kk) = scat(-miff_ind)%dat(1   ,1:kk)
           ELSE IF ( miff_ind > 0 ) THEN
              xval(1:kk) = scat(miff_ind)%dat(1+jj,1:kk) + scat(miff_ind)%dat(1   ,1:kk)
           ENDIF
           yval(1:kk) = scat(j)%dat(1+jj,1:kk) + diff_flag * &
                        scat(j)%dat(1   ,1:kk)
        ELSE
           IF ( miff_ind < 0 ) THEN
              xval(1:kk) = scat(j)%dat(1   ,1:kk)
           ELSE IF ( miff_ind > 0 ) THEN
              xval(1:kk) = scat(j)%dat(1+jj,1:kk) + scat(j)%dat(1   ,1:kk)
           ENDIF
           yval(1:kk) = scat(ABS(miff_ind))%dat(1+jj,1:kk) + diff_flag * &
                        scat(ABS(miff_ind))%dat(1   ,1:kk)
        ENDIF

        CALL pname(obstype(j),wtext)

        call bin_scat(xval(1:kk),yval(1:kk),kk,					&
                      minx,maxx,miny,maxy,						&
                      scatflag,									&
                      title,wtext,wtext1,wtext2,				&
                      wtext3,wtext4)

     ENDDO

  ENDDO

  CALL PCLOSE
  IF (ltiming) CALL acc_timing(timing_id,'plot_scatt_comp')

  DEALLOCATE(xval,yval)


RETURN
END SUBROUTINE plot_scat_comp
