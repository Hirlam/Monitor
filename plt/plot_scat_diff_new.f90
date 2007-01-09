SUBROUTINE plot_scat_diff_new(lunout,nparver,nr,nrun,    &
           diff_ind,scat,p1,p2,par_active)

 USE types
 USE functions
 USE timing
 USE data, ONLY : obstype,expname,err_ind,nexp,station_name,csi, &
                  fclen,nfclengths,                              &
                  show_fc_length,ltiming,dd_ind,                 &
                  period_freq,output_type
 USE mymagics

 IMPLICIT NONE

 ! INPUT

 INTEGER,            INTENT(IN) :: lunout,nparver,nr,nrun,diff_ind,p1,p2
 INTEGER,            INTENT(IN) :: par_active(nparver)
 TYPE(scatter_type), INTENT(IN) :: scat(nparver)

 ! LOCAL

 INTEGER :: i,j,jj,k,kk,l,timing_id,pp1
 CHARACTER(LEN=40) :: fname=' '
 CHARACTER(LEN=100) :: wtext  =' ', &
                       wtext1 =' ', &
                       wtext2 =' ', &
                       wtext3 =' ', &
                       wtext4 =' ', &
                       wname  =' ', &
                       cdum   =' '

 CHARACTER(LEN=1) :: prefix = ' '
 CHARACTER(LEN=80) :: title =' '

 REAL :: miny,maxy,minx,maxx,rtmp,diff_flag
 
 REAL :: sxmean, symean, sstdevx, sstdevy, sbias, srmse, sstdevd, scorr

 REAL, ALLOCATABLE :: xval(:),yval(:)

 LOGICAL :: scatflag

 !
 !-----------------------------------------------------
 !

 !
 ! Init timing counter
 !

 timing_id = 0
 IF (ltiming) CALL acc_timing(timing_id,'plot_scatt_diff')

 ALLOCATE(xval(MAXVAL(scat%n)))
 ALLOCATE(yval(MAXVAL(scat%n)))

 IF (diff_ind < 0) THEN
     prefix  = 'S'
     scatflag = .FALSE.
     diff_flag = 0.0
   ELSEIF (diff_ind == 0) THEN
     prefix = 's'
     scatflag = .TRUE.
     diff_flag = 1.0
   ENDIF


  ! Set filename
  IF ( p1 < 999999 ) THEN
     CALL make_fname(prefix,p1,nr,nrun,fname,output_type)
  ELSE
     CALL make_fname(prefix, 0,nr,nrun,fname,output_type)
  ENDIF

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
      IF (diff_ind == 0) THEN
        IF(ALLOCATED(station_name)) THEN
           WRITE(title,'(A,A)')'Scatterplot ',	&
           trim(station_name(csi))
        ELSE
           WRITE(title,'(A,I)')'Scatterplot for station ',nr
        ENDIF
      ELSE
        IF(ALLOCATED(station_name)) THEN
           WRITE(title,'(A,A)')'Difference scatterplot ',	&
           TRIM(station_name(csi))
        ELSE
           WRITE(title,'(A,I)')'Difference scatterplot for station ',nr
        ENDIF
      ENDIF
   ELSE
      IF (diff_ind == 0) THEN
         title='Scatterplot for      stations'
         WRITE(title(17:20),'(I4)')par_active(j)
      ELSE
         title='Difference scatterplot for      stations'
         WRITE(title(28:31),'(I4)')par_active(j)
      ENDIF
   ENDIF

    ! Line 2
    IF (p1 == 0 ) THEN
    ELSEIF(p1 < 13) THEN

       SELECT CASE(period_freq) 
       CASE(1)
        WRITE(wtext4,'(A8,A8)')'Period: ',seasonal_name2(p1)
       CASE(3)
        WRITE(wtext4,'(A8,A8)')'Period: ',seasonal_name1(p1)
       END SELECT 

    ELSEIF(p1 < 999999 ) THEN
       pp1 = monincr(p1,period_freq-1)
       IF(p1 == pp1 ) THEN
          WRITE(wtext4,'(A8,I8)')'Period: ',p1
       ELSE
          WRITE(wtext4,'(A8,I8,A2,I8)')'Period: ',        &
          p1,' -',pp1
       ENDIF
    ELSE
       WRITE(wtext4,'(A8,I8,A1,I8)')'Period: ',p1,'-',p2
    ENDIF


     ! Find min and max
     miny = 0.
     minx = 0.
     maxy = 1.
     maxx = 1.

     kk = scat(j)%n
     IF (kk > 0 )THEN

        minx =  MINVAL(scat(j)%dat(1,1:kk))
        maxx =  MAXVAL(scat(j)%dat(1,1:kk))
        miny =  MINVAL(scat(j)%dat(1+1,1:kk) + diff_flag * scat(j)%dat(1,1:kk))
        maxy =  MAXVAL(scat(j)%dat(1+1,1:kk) + diff_flag * scat(j)%dat(1,1:kk))

        DO jj=2,nexp
          rtmp =  MINVAL(scat(j)%dat(1+jj,1:kk) + diff_flag * scat(j)%dat(1,1:kk))
          miny =  MIN(rtmp,miny)
          rtmp =  MAXVAL(scat(j)%dat(1+jj,1:kk) + diff_flag * scat(j)%dat(1,1:kk))
          maxy =  MAX(rtmp,maxy)
        ENDDO

        IF (diff_ind == 0 ) THEN
         rtmp = MIN(miny,minx)        
         miny = rtmp
         minx = rtmp
         rtmp = MAX(maxy,maxx)        
         maxx = rtmp
         maxy = rtmp
        ENDIF
     ENDIF
     IF ( j == dd_ind ) THEN
        minx = 0
        maxx = 360
        IF (diff_ind == 0 ) THEN
           miny = 0
           maxy = 360
        ENDIF
     ENDIF

     DO jj=1,nexp

        IF (diff_ind == 0) THEN
           wtext1 = expname(jj)
           wtext2 = 'OBS'
        ELSEIF (diff_ind > 0) THEN
           wtext1 = TRIM(expname(jj))//' - OBS '//TRIM(obstype(diff_ind))
           wtext2= 'OBS '//TRIM(obstype(j))
        ELSEIF (diff_ind < 0) THEN
           wtext1 = TRIM(expname(jj))//' - OBS '//TRIM(obstype(j))
           wtext2= 'OBS XX'
           WRITE(wtext2(5:6),'(A2)')obstype(j)
        ENDIF

        IF ( show_fc_length ) THEN
           IF (nfclengths > 10 ) THEN
             WRITE(cdum,'(I3)')fclen(nfclengths)
             WRITE(wname,'(I3,X,I3)')fclen(1:2)
             WRITE(wtext3,*)'Forecast lengths used:'//TRIM(wname)//' ... '//TRIM(cdum)
           ELSE
             wname='(A,XX(1X,I3))'
             WRITE(wname(4:5),'(I2.2)')nfclengths
             WRITE(wtext3,wname)'Forecast lengths used:',fclen(1:nfclengths)
           ENDIF
        ENDIF

        kk = scat(j)%n

        xval(1:kk) = scat(j)%dat(1   ,1:kk)
        yval(1:kk) = scat(j)%dat(1+jj,1:kk) + diff_flag * &
                     scat(j)%dat(1   ,1:kk)


        IF ( j == dd_ind .AND. diff_ind == 0 ) THEN
           DO l=1,kk
              IF (yval(l) > 360. ) THEN
              yval(l) = yval(l) - 360.         
           ELSEIF(yval(l) <   0. ) THEN
              yval(l) = yval(l) + 360.         
           ENDIF
           ENDDO
        ENDIF

        CALL pname(obstype(j),wtext)

        call bin_scat(xval(1:kk),yval(1:kk),kk,		&
                      minx,maxx,miny,maxy,		&
                      scatflag,				&
                      title,wtext,wtext1,wtext2,wtext3,wtext4)

     ENDDO

  ENDDO

  CALL PCLOSE
  IF (ltiming) CALL acc_timing(timing_id,'plot_scatt_diff')

  DEALLOCATE(xval,yval)

! IF ( nr /= 0 ) CLOSE(47)

RETURN
END SUBROUTINE plot_scat_diff_new
