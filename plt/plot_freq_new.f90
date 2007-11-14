SUBROUTINE plot_freq_new(lunout,nparver,nr,nrun,scat,p1,p2,par_active,uh,uf)

 !
 ! Plot Frequency distribution
 !
 ! Ulf Andrae, SMHI, 2007
 !

 USE types
 USE functions
 USE mymagics
 USE timing
 USE data, ONLY : nexp,station_name,err_ind,csi,obstype, &
                  expname,gr_ind,pe_ind,pd_ind,          &
                  lfcver,                                &
                  show_fc_length,                        &
                  ltiming,tag,maxfclenval,               &
                  ncla,classtype,pre_fcla,               &
                  mincla,maxcla,my_ymax,my_ymin,         &
                  mpre_cla,copied_mod,copied_obs,        &
                  period_freq,output_type

 IMPLICIT NONE

 REAL,    PARAMETER :: spxl      = 23. ! SUB_PAGE_X_LENGTH

 ! Input
 INTEGER, INTENT(IN) :: lunout,nparver,nr,nrun,     &
                        p1,p2,                      &
                        par_active(nparver)

 TYPE(scatter_type), INTENT(IN) :: scat(nparver)

 LOGICAL,            INTENT(IN) :: uh(nparver,0:23),   &
                                   uf(nparver,0:maxfclenval)


 ! Local
 INTEGER :: i,j,k,l,m,n,ncl,       		&
            timing_id,lnexp,pp1

 REAL :: dcla,fdat_sum,bar_width,               &
         bfac,maxy,miny

 REAL, ALLOCATABLE :: work(:,:),                &
                      pcla(:),fcla(:),          &
                      fdat(:,:),zero(:),zdat(:)

 LOGICAL :: reset_class

 CHARACTER(LEN=40) :: fname = ''
 CHARACTER(LEN=90) :: wtext = '',wtext2 = ''
 CHARACTER(LEN=20) :: wname = ''
 CHARACTER(LEN=20) :: cdum  = ''

!-----------------------------------------------------
 ! Init timing counter
 timing_id = 0
 IF (ltiming) CALL acc_timing(timing_id,'plot_freq')

 IF ( copied_mod .OR. copied_obs ) THEN
    lnexp = nexp
 ELSE
    lnexp = nexp + 1
 ENDIF

 ! Set filename
 IF ( p1 < 999999 ) THEN
    CALL make_fname('f',p1,nr,tag,fname,output_type)
 ELSE
    CALL make_fname('f', 0,nr,tag,fname,output_type)
 ENDIF

 CALL open_output(fname)


 
 DO j=1,nparver
   
    ncl = ncla(j)
    ALLOCATE(pcla(ncl),                &
             fcla(ncl),                &
             fdat(ncl,nexp+1),         &
             zero(ncl-1),              &
             zdat(ncl-1))

    zero = 0.

    reset_class = ( mincla(j) > maxcla(j) )

    n = scat(j)%n

    !
    ! Copy data to work array
    !

    ALLOCATE(work(n,lnexp))

    work(:,lnexp) = scat(j)%dat(1,1:n)
    DO k=1,nexp
       work(1:n,k) =  scat(j)%dat(k+1,1:n) + &
                      scat(j)%dat(1  ,1:n) 
    ENDDO

    IF ( obstype(j)(1:2) == 'DD' ) THEN
       DO k=1,nexp
        DO m=1,n
          IF(work(m,k) > 360. )THEN
            work(m,k) = work(m,k) - 360.         
          ELSEIF(work(m,k) <   0. )THEN
            work(m,k) = work(m,k) + 360.         
          ENDIF
        ENDDO
       ENDDO
    ENDIF
    
    IF (n > 0 ) THEN
       
       IF ( reset_class )THEN

          maxcla(j) = 0.
          mincla(j) = 1.

          IF (j == gr_ind) THEN
             mincla(j) = 1.
             maxcla(j) = 1000.
          ENDIF

       ENDIF
  
       CALL freq_dist(lnexp,n,ncl,                    &
                      mincla(j),maxcla(j),classtype(j),   &
                      pre_fcla(j,1:ncl),work,fdat,fcla)

    ELSE

       fdat = 0.
       DO i=1,ncl
          fcla(i) = i
       ENDDO

    ENDIF

    ! Plotting
   
    dcla      = fcla(ncl) - fcla(ncl-1)
    bar_width = spxl / float(ncl*lnexp)
    bfac      = bar_width*dcla*(ncl)/spxl

    DO i=1,lnexp
       fdat_sum  = SUM(fdat(:,i))
       fdat(:,i) = fdat(:,i) / MAX(1.,fdat_sum)
    ENDDO

    ! Rescale lowest precipitation class
    IF ( j == pe_ind .OR. j == pd_ind ) THEN
       fdat(2,:) = 0.1*fdat(2,:)
    ENDIF

    maxy      = MAXVAL(fdat)
    miny      = 0.

    IF (n == 0 ) maxy = 1.0

    IF ( ABS(my_ymax - err_ind ) > 1.e-6 ) maxy = my_ymax
    IF ( ABS(my_ymin - err_ind ) > 1.e-6 ) miny = my_ymin

    ! Plotting part
   
    CALL pnew('SUPER_PAGE')
    CALL psetr  ('SUBPAGE_X_POSITION',             3.5)
    CALL psetr  ('SUBPAGE_Y_POSITION',             3.0)
    CALL psetr  ('SUBPAGE_Y_LENGTH',              18.0)
    CALL psetr  ('SUBPAGE_X_LENGTH',              spxl)

    CALL psetc('TEXT_COLOUR','BLACK')
    CALL psetc('TEXT_QUALITY','HIGH')
    CALL psetc('PAGE_ID_LINE_SYSTEM_PLOT','ON')
    CALL psetc('PAGE_ID_LINE_ERRORS_PLOT','OFF')
    CALL psetc('PAGE_ID_LINE_DATE_PLOT','ON')
    CALL psetc('PAGE_ID_LINE_QUALITY','HIGH')
    CALL psetc('PAGE_ID_LINE_LOGO_PLOT','OFF')
   
    CALL psetc('LEGEND','ON')
    CALL psetc ('AXIS_GRID','ON')
   
    CALL psetc ('AXIS_ORIENTATION','VERTICAL') 
    CALL psetc ('AXIS_TITLE_TEXT','Relative frequency')
    CALL preset('AXIS_TICK_INTERVAL')
    CALL psetr ('AXIS_MIN_VALUE',miny)
    CALL psetr ('AXIS_MAX_VALUE',maxy)
    CALL paxis
   
    CALL psetc ('AXIS_ORIENTATION','HORIZONTAL') 
    CALL psetc ('AXIS_TYPE','REGULAR')
    CALL preset('AXIS_TICK_INTERVAL')
    CALL psetr ('AXIS_MIN_VALUE',fcla(1) )
    CALL psetr ('AXIS_MAX_VALUE',fcla(ncl) + dcla)

    wtext =''
    CALL yunit(obstype(j),wtext)

    IF (classtype(j) == 1) wtext = 'log10('//TRIM(wtext)//')'

    CALL psetc ('AXIS_TITLE_TEXT',wtext)
    CALL paxis
   
    CALL psetc ('GRAPH_TYPE','BAR')    
    CALL psetr ('GRAPH_BAR_WIDTH',bar_width)
    CALL psetc ('GRAPH_SHADE','ON')    
    CALL PSETC ('GRAPH_MISSING_DATA_MODE','DROP')
   
    pcla = fcla - 0.5*dcla + 0.5*bfac
    DO i=lnexp,1,-1

       zdat = fdat(2:ncl,i)
       CALL psetc  ('LEGEND_USER_TEXT' ,expname(i))
       CALL psetr  ('GRAPH_BAR_WIDTH',1.0*bar_width)
       CALL psetc  ('GRAPH_SHADE_COLOUR',linecolor(i))
       CALL pset1r ('GRAPH_BAR_X_VALUES',pcla(2:ncl),ncl-1)
       CALL pset1r ('GRAPH_BAR_Y_LOWER_VALUES',zero,ncl-1)
       CALL pset1r ('GRAPH_BAR_Y_UPPER_VALUES',zdat,ncl-1)
       CALL pgraph
       pcla = pcla + bfac

    ENDDO

    !
    ! Set title text
    !

    CALL pseti('TEXT_LINE_COUNT',3)

    ! Line 1
    IF(ALLOCATED(station_name)) THEN
       WRITE(wtext,'(2A)')'Frequency distribution for ',  &
       trim(station_name(csi))
    ELSE
       WRITE(wtext,'(A,I8)')'Frequency distribution for station ',nr
    ENDIF
    IF (nr == 0) THEN
       WRITE(wtext(1:4),'(I4)')par_active(j)
       wtext='Frequency distribution for '//TRIM(wtext(1:4))//' stations'
    ENDIF
    CALL psetc('TEXT_LINE_1',wtext)

    ! Line 2
    wtext  = ''
    wtext2 = ''
    CALL pname(obstype(j),wtext)
    IF (j == pe_ind .OR. j == pd_ind ) THEN
       wtext = TRIM(wtext)//' - Lowest class scaled by 0.1!'
    ENDIF

    IF (p1 == 0 ) THEN
    ELSEIF(p1 < 13) THEN

       SELECT CASE(period_freq) 
       CASE(1)
        WRITE(wtext2,'(A8,A8)')'Period: ',seasonal_name2(p1)
       CASE(3)
        WRITE(wtext2,'(A8,A8)')'Period: ',seasonal_name1(p1)
       END SELECT 

    ELSEIF ( p1 < 999999 ) THEN
       pp1 = monincr(p1,period_freq-1)
       IF(p1 == pp1) THEN
          WRITE(wtext2,'(A8,I6)')'Period: ',p1
       ELSE
          WRITE(wtext2,'(A8,I6,A1,I6)')'Period: ',        &
          p1,'-',monincr(p1,period_freq-1)
       ENDIF
    ELSE
       WRITE(wtext2,'(A8,I8,A1,I8)')'Period: ',p1,'-',p2
    ENDIF

    wtext = TRIM(wtext)//'  '//TRIM(wtext2)
    CALL psetc('TEXT_LINE_2',wtext)

    ! Line 3
    wtext2 ='' 
    wname = ''

    WRITE(wtext2,'(I)')NINT(fdat_sum)
    WRITE(wname ,'(I2)')ncl-1
    wtext ='Number of cases'//TRIM(wtext2)//'  Number of classes '//TRIM(wname)
    CALL psetc('TEXT_LINE_3',wtext)
    
    ! Line 4
    IF ( show_fc_length ) THEN
       CALL pseti('TEXT_LINE_COUNT',4)
       CALL fclen_header(.TRUE.,maxfclenval,uh(j,:),uf(j,:),wtext)
       CALL PSETC('TEXT_LINE_4',wtext)
    ENDIF

    CALL ptext

    ! Clear memory
    DEALLOCATE(work,pcla,fcla,fdat,zero,zdat)
   
 ENDDO

 CALL pclose

 IF (ltiming) CALL acc_timing(timing_id,'plot_freq')

 RETURN
END SUBROUTINE plot_freq_new
