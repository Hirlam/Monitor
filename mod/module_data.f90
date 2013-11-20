MODULE data

 USE types
 USE functions

 IMPLICIT NONE

 SAVE

! Constants
 REAL, PARAMETER :: err_ind = -999.e6 ! Error flag
 REAL, PARAMETER :: eps     = 1.e-6   ! Real equality comparison limit

! I/O
 INTEGER, PARAMETER :: lunnam  = 10    ! Namelist unit
 INTEGER, PARAMETER :: lunin   = 13
 INTEGER, PARAMETER :: lunin2  = 16
 INTEGER, PARAMETER :: lunout  = 14
 INTEGER, PARAMETER :: lunstat = 15
 INTEGER, PARAMETER :: lunxml  = 47
 INTEGER, PARAMETER :: lunqc   = 48
 INTEGER, PARAMETER :: len_lab = 6
 INTEGER, PARAMETER :: maxexp  = 10     ! Max experiments
 INTEGER, PARAMETER :: mparver = 200    ! Max parameters to verify
 INTEGER, PARAMETER :: maxfclen= 48     ! Maximum number of forecast lengths
 INTEGER, PARAMETER :: maxfclenval= 240 ! Maximum forecast length value in hours
 INTEGER, PARAMETER :: max_map_interval = 7 ! Maximum number of map intervals

! Main arrays 
 TYPE (station), ALLOCATABLE, DIMENSION(:) :: obs,hir
 TYPE (statpar), ALLOCATABLE :: stat(:)


! Some global values
 INTEGER :: nrun             = 0
 INTEGER :: timdiff          = 0
 INTEGER :: csi              = 0 ! Current station index
 INTEGER :: active_stations  = 0 ! Number of active stations
 INTEGER :: active_stations_ver  = 0 ! Number of active stations for verification

 LOGICAL :: doing_monthvise = .FALSE.
 LOGICAL :: copied_obs      = .FALSE.
 LOGICAL :: copied_mod      = .FALSE.
 LOGICAL :: z_is_pressure   = .TRUE.
 

 CHARACTER(LEN=50) :: formdb ='(I12,I4,XX(en15.5e2))'
 CHARACTER(LEN=50), ALLOCATABLE :: station_name(:)
 CHARACTER(LEN=10) :: vert_unit = 'hPa'

!
! NAMELIST
!

 ! Time control
 INTEGER :: period_type   = 1    ! Time of period to work with
                                 ! 1 : sdate - edate
                                 ! 2 : sdate - edate in consecutive monthvise pieces like 200611,200612, ...
                                 ! 3 : sdate - edate in seasonal monthvise pieces like, DJF,MAM,JJA,SON 
 INTEGER :: period_freq   = 1    ! Accumulation frequency in months for
                                 ! period_type 2 and 3
                                 ! for period_type 3 only a frequency of 1 and 3
                                 ! is allowed
                                 ! 
 INTEGER :: sdate  = 19990901    ! Start date
 INTEGER :: stime  = 0           ! Start time
 INTEGER :: edate  = 20000320    ! End date
 INTEGER :: etime  = 23          ! End time
 INTEGER :: edate_obs  = 0       ! End date for observation
 INTEGER :: etime_obs  = 0       ! End time for observation
 INTEGER :: maxtim = 0           ! Estimated number of times 
                                 ! set to 0 to let the program decide
 INTEGER :: maxtim_scat = 0      ! Estimated number of times for
                                 ! scatter/frequency plots
                                 ! set to 0 to let the program decide

 ! Station selection by list or area
 INTEGER, PARAMETER ::  max_maxstn  = 20000
 INTEGER :: maxstn                   = 1  ! Max stations
 INTEGER :: stnlist(max_maxstn)      = 0  ! Station numbers
 INTEGER :: stnlist_bl(max_maxstn)   = 0  ! Black listed stations
 INTEGER :: stnlist_plot(max_maxstn) = -1 ! Additional stations to plot
                                          ! -1 gives none 
                                          !  0 gives all 
 TYPE (box) :: cbox                       ! Area box (S,W,N,E) corners
 LOGICAL    :: lpoly = .FALSE.            ! Area selection by polygon
 CHARACTER(LEN=100 )  :: polyfile ='poly.dat'
 LOGICAL    :: reverse_selection          ! Reverse your selection

 LOGICAL    :: lstn_hgt_check             ! Station height selection
 REAL       :: hgt_llim,hgt_ulim          ! Lower and upper limit for stations
                                          ! in meters

 ! Experiment and name
 INTEGER           :: nexp = 1                  ! Number of experiments
 CHARACTER(LEN=50) :: expname(maxexp)='OBS'     ! Name of experiments
 INTEGER           :: smallest_exp_ind = 0      ! The location of the experiment with the smallest domain
 INTEGER           :: exp_offset(maxexp) = 0    ! Define the offset of initial hours
 CHARACTER(LEN=99) :: statname='statistics.html'! Name of output statistics file
 CHARACTER(LEN=50) :: name='Unknown'            ! Station name
 CHARACTER(LEN=50) :: tag='#'                   ! Tag on plot
 CHARACTER(LEN=50) :: cini_hours=''             ! String for ini hours in file name
 CHARACTER(LEN=99) :: obspath        ='../'     ! Path to observation data
 CHARACTER(LEN=299) :: modpath(maxexp)='../'     ! Path to model data

 ! Fc len and times to verify
 INTEGER :: nfclengths          =  0            ! Number of fclengths to verify
 INTEGER :: ntimver             =  1            ! Number of hours to verify
 INTEGER :: show_times(maxfclen)= -1            ! Hours of days to show
 INTEGER :: nshow_times         =  0            ! number of hours of days to show
 INTEGER ::  ini_hours(24)      = -1            ! FC start hours to use
 INTEGER :: nini_hours          =  0            ! number of init hours to use
 INTEGER ::  use_fclen(maxfclen)= -1            ! fclengths to use (in hours)
 INTEGER :: nuse_fclen          =  0            ! number of used fclengths
 INTEGER :: fclen(maxfclen)     = -1            ! fclengths to read in hours
 INTEGER :: fcint               =  6            ! Interval between forecasts in hour
 INTEGER :: obint               =  1            ! Interval between observations in hour
                                                ! Not applicable everywhere 
 INTEGER :: time_shift                 =  0     ! Shift in time when doing daily average
 INTEGER :: yearvise_wind              =  7     ! Accumulation period for seasonal verify
 INTEGER :: timeserie_wind(mparver)    =  0     ! Accumulation period for timeserie plots
 INTEGER :: window_pos                 =  0     ! Plot position of period
                                                ! -1 Beginning
                                                !  0 Centre
                                                !  1 End

 LOGICAL, ALLOCATABLE ::   used_hours(:,:,:)    ! Indicator for hours really used
 LOGICAL, ALLOCATABLE ::   used_fclen(:,:,:)    ! fclengths really used

 ! Frequency distribution plots
 INTEGER, PARAMETER :: mpre_cla  = 100  ! Max number of predined classes
 INTEGER :: ncla(mparver)        = 26   ! Number of classes
 INTEGER :: classtype(mparver)   = 0    ! Linear classtypes
 REAL    :: pre_fcla(mpre_cla,mparver) = 0.   ! Predefined classes
 REAL    :: maxcla(mparver)      = 0.   ! Maximum class value
 REAL    :: mincla(mparver)      = 1.   ! Minimum class value

 ! Data to verify 
 INTEGER            :: data_to_verify = 0     ! Which case to select in my_choice.f to be obsoloete
 CHARACTER(LEN=300) :: data_source  = ''      ! Character string selection

 ! Selection
 CHARACTER(LEN=len_lab)      :: varlist(mparver) = '#'
 TYPE(variable)              :: setprop(mparver) = variable(0,0,err_ind,err_ind,err_ind,'#','#','#')
 TYPE(variable), &
 ALLOCATABLE,TARGET          :: varprop(:) 

 ! Graphics
 CHARACTER(LEN=50) :: graphics='GNUPLOT'

 ! Parameters to verify
 INTEGER :: nparver = mparver
 REAL    :: lev_lst(mparver) = 0. ! List of vertical levels
                                  ! if lev_lst(n) < lev_lst(n+1) height is assumed
                                  ! if lev_lst(n) > lev_lst(n+1) pressure is assumed
 ! Some limits
 REAL :: sumup_tolerance     =  0.00    ! Require X% coverage when take average over time_wind
 REAL :: my_xmax             =  err_ind ! Give your own axis limits
 REAL :: my_xmin             =  err_ind !
 REAL :: my_ymin             =  err_ind !
 REAL :: my_ymax             =  err_ind !

 ! Flags
 LOGICAL :: ltiming          = .FALSE.  ! Measure performance (use gprof instead)
 LOGICAL :: ltemp            = .FALSE.  ! Multi level fields
 LOGICAL :: lfcver           = .FALSE.  ! Verify by fclength else by time of day
 LOGICAL :: leach_station    = .FALSE.  ! Verify each station separately
 LOGICAL :: lallstat         = .TRUE.   ! Show statistics for all stations
 LOGICAL :: lfindplot        = .FALSE.  ! Call findplot obsolete at the moment
 LOGICAL :: lplot_seasonal   = .FALSE.  ! Create seasonal plots
 LOGICAL :: lprint_seasonal  = .FALSE.  ! Create seasonal plots
 LOGICAL :: ltimeserie       = .FALSE.  ! Plot timeseries for all stations
 LOGICAL :: ltimeserie_stat  = .FALSE.  ! Plot timeserie statistics 
 LOGICAL :: lprint_timeserie_stat  = .FALSE.   ! Print timeserie statistics 
 LOGICAL :: lplot_freq       = .FALSE.  ! Make frequency distribution plots
 LOGICAL :: lprint_freq      = .FALSE.  ! Make frequency distribution printfiles
 LOGICAL :: lprint_scat      = .FALSE.  ! Make scatterplots
 LOGICAL :: lscat_yave       = .FALSE.  ! Make Y-axis average line
 LOGICAL :: lplot_scat       = .FALSE.  ! Make scatterplots
 LOGICAL :: lprep_xml        = .FALSE.  ! Create statistics in xml format based
                                        ! on scatter data
 LOGICAL :: lverify          = .TRUE.   ! Call verify main subroutine
 LOGICAL :: lstat_gen        = .TRUE.   ! Calculate general statistics
 LOGICAL :: lprint_selection = .FALSE.  ! Print selected stations
 LOGICAL :: lprint_read      = .FALSE.  ! Print diagnostics when reading, obsolete
 INTEGER ::  print_read      = 1        ! Print diagnostics when reading
 LOGICAL :: lprint_summary   = .FALSE.  ! Print comprehensive station summary
 LOGICAL :: lprint_verif     = .FALSE.  ! Print diagnostics
 LOGICAL :: lprint_findp     = .FALSE.  ! Print diagnostics
 LOGICAL :: lprint_do_stat   = .FALSE.  ! Print diagnostics
 LOGICAL :: lplot_vert       = .FALSE.  ! Create plot file for vertical temp statistics
 LOGICAL :: lprint_vert      = .FALSE.  ! Create print file for vertical temp statistics
 LOGICAL :: lprint_stat      = .FALSE.  ! Create plot file for statistics
 LOGICAL :: lplot_stat       = .FALSE.  ! Create plot file for statistics
 LOGICAL :: use_database     = .FALSE.  ! Try to use database, obsolete
 LOGICAL :: release_memory   = .FALSE.  ! Release memory as soon as data have been used
 LOGICAL :: gap_filled_data  = .FALSE.  ! Allow gap filled data, obsolete
 LOGICAL :: ldiff            = .FALSE.  ! Plot observation - model departure
 LOGICAL :: lnorm            = .FALSE.  ! Plot ( mod - obs ) / obs
 LOGICAL :: show_fc_length   = .TRUE.   ! Give used fc lengths on plot
 LOGICAL :: all_var_present  = .FALSE.  ! Demand that all variables must have values
                                        ! At the same time
 LOGICAL :: use_pos  = .FALSE.          ! CALL verify_pos


 ! Parameters for maps
 LOGICAL :: plot_bias_map          = .FALSE.  ! Plot map with bias
 LOGICAL :: plot_rmse_map          = .FALSE.  ! Plot map with rmse
 LOGICAL :: plot_stdv_map          = .FALSE.  ! Plot map with stdv
 LOGICAL :: print_bias_map         = .FALSE.  ! Print map with bias
 LOGICAL :: print_rmse_map         = .FALSE.  ! Print map with rmse
 LOGICAL :: print_stdv_map         = .FALSE.  ! Print map with stdv
 LOGICAL :: plot_obs_map           = .FALSE.  ! Plot map with observations
 LOGICAL :: print_obs_map          = .FALSE.  ! Print map with observations
 REAL    ::  map_obs_interval(max_map_interval,mparver)= -1.      ! Set your own obs interval 
 REAL    :: map_bias_interval(max_map_interval,mparver)= -1.      ! Set your own bias interval
 REAL    :: map_rmse_interval(max_map_interval,mparver)= -1.      ! Set your own rmse interval
 REAL    :: map_stdv_interval(max_map_interval,mparver)= -1.      ! Set your own rmse interval


 ! What to plot on verification plots
 LOGICAL :: show_bias        = .TRUE.   ! Plot bias
 LOGICAL :: show_rmse        = .TRUE.   ! Plot rmse
 LOGICAL :: show_stdv        = .FALSE.  ! Plot stdv
 LOGICAL :: show_var         = .FALSE.  ! Plot forecasts and obs 'internal' stdv
 LOGICAL :: show_skw         = .FALSE.  ! Plot forecasts and obs 'internal' skewness
 LOGICAL :: show_obs         = .FALSE.  ! Plot full obs turns bias/rmse/stdv to false


 ! Special conditions
 INTEGER :: special_flag     = 0        ! Flag for special conditions
 LOGICAL :: lspecial_cond    = .FALSE.  ! Use special conditions


 ! Output type
 INTEGER :: output_type = 1                   ! 1 = ps, 2 = png, 3 = jpg
 INTEGER :: output_mode = 1                   ! 1 = multi page, 2 = single page

 ! Quality control
 INTEGER :: print_qc           = 1            ! Quality control output level (0,1,2)
 LOGICAL :: lquality_control   = .FALSE.      ! Pre verification quality control
 LOGICAL :: estimate_qc_limit  = .FALSE.      ! Calculate STDV for given
                                              ! qc_fclen
 INTEGER :: qc_fclen(maxfclen)    = -1        ! fclengths to be used for qc
 REAL    :: qc_lim(mparver)       = err_ind   ! Quality control limits
 REAL    :: qc_lim_scale(mparver) = 4.0       ! Scale estimated qc_lim by this
                                              ! to be copied from XX_lim ...

 ! Cross correlations of variables an biases
 INTEGER :: corr_pairs(mparver,2) = 0         ! Pairs of variables to compare,
                                              ! specify variable number
 INTEGER :: flag_pairs(mparver,2) = 1         ! -1 means observation
                                              !  0 means difference
                                              !  1 means model
 INTEGER :: exp_pairs(mparver,2)  = 0         ! Pairs of experiment to pick
                                              !
 LOGICAL ::  lplot_comp = .FALSE.             ! Not a namelist variable
 LOGICAL :: lprint_comp = .FALSE.             ! Not a namelist variable
 REAL    :: scat_min(mparver)     = 1.        ! Minimum value in scatterplot
 REAL    :: scat_max(mparver)     = 0.        ! Maximum value in scatterplot
 INTEGER :: scat_magn(mparver)    = 24        ! Number of scale levels for 
                                              ! observations classes in scatterplot
                                              ! The actual number of levels are
                                              ! scat_magn / 4 

 ! Conditional flags

 LOGICAL :: lconditional = .FALSE.
 INTEGER :: cond_param   = 0
 TYPE(conditional_type) cond(mparver)

 ! Contingency flags

 LOGICAL :: lcontingency = .FALSE.

 INTEGER :: cont_param          = 0
 INTEGER :: cont_ind(mparver)   = 0
 INTEGER :: cont_class(mparver) = 0
 REAL    :: cont_lim(mpre_cla,mparver) = 0.0 !

 !
 ! Significance test settings
 !

 LOGICAL :: lsign_test       = .FALSE.        ! Perform student-t test
 INTEGER :: control_exp_nr   = 1              ! Which experiment is the control
 INTEGER :: sign_time_diff   = -1             ! Min time difference in days for sign test. -1 implies fcint
 REAL    :: confint          = 90.            ! Confidence interval for significance test

 !
 ! Namelist 
 !
 namelist/namver/sdate,stime,edate,etime,               &
                 maxstn,maxtim,maxtim_scat,             &
                 ntimver,time_shift,                    &
                 nfclengths,fclen,fcint,obint,          &
                 use_fclen,nuse_fclen,                  &
                 ini_hours,nini_hours,                  &
                 show_times,nshow_times,                &
                 timeserie_wind,window_pos,             &
                 stnlist,stnlist_bl,stnlist_plot,       &
                 nexp,expname,tag,                      &
                 exp_offset,                            &
                 smallest_exp_ind,                      &
                 varlist,setprop,                       &
                 lev_lst,                               &
                 name,statname,                         &
                 obspath,modpath,                       &
                 lfcver,leach_station,ltiming,          &
                 lallstat,                              &
                 lplot_seasonal,lprint_seasonal,        &
                 lfindplot,                             &
                 lstat_gen,lverify,                     & 
                 lprint_stat,lplot_stat,                &
                 lprint_vert,lplot_vert,                &
                 ltimeserie,lplot_scat,lprep_xml,       &
                 lprint_scat,lscat_yave,                &
                 lcontingency,                          &
                 ltimeserie_stat,                       &
                 lprint_timeserie_stat,                 &
                 plot_bias_map,                         &
                 plot_rmse_map,                         &
                 plot_stdv_map,                         &
                 print_bias_map,                        &
                 print_rmse_map,                        &
                 print_stdv_map,                        &
                 plot_obs_map,                          &
                 print_obs_map,                         &
                 map_obs_interval,                      &
                 map_rmse_interval,                     &
                 map_stdv_interval,                     &
                 map_bias_interval,                     &
                 lprint_selection,                      &
                 lprint_summary,                        &
                 lprint_read,print_read,                &
                 lprint_verif,lprint_findp,             &
                 lprint_do_stat,                        &
                 release_memory,                        &
                 lprint_freq,lplot_freq,                &
                 cbox,lpoly,polyfile,                   &
                 reverse_selection,                     &
                 data_to_verify,data_source,            &
                 ltemp,                                 &
                 use_database,                          &
                 sumup_tolerance,                       &
                 my_xmin,my_xmax,my_ymin,my_ymax,       &
                 lspecial_cond,special_flag,            &
                 gap_filled_data,ldiff,lnorm,           &
                 show_fc_length,all_var_present,        &
                 use_pos,output_type,output_mode,       &
                 ncla,classtype,pre_fcla,               &
                 maxcla,mincla,                         &
                 show_bias,show_rmse,show_stdv,show_obs,&
                 show_var,show_skw,                     &
                 period_type,period_freq,               &
                 print_qc,                              &
                 lquality_control,qc_fclen,qc_lim,      &
                 estimate_qc_limit,qc_lim_scale,        &
                 corr_pairs,flag_pairs,exp_pairs,       &
                 scat_min,scat_max,scat_magn,           &
                 cont_ind,cont_class,cont_lim,          &
                 cont_param,                            &
                 cond_param,cond,                       &
                 graphics,                              &
                 lsign_test,control_exp_nr,             &
                 sign_time_diff,confint,                &
                 lstn_hgt_check,hgt_llim,hgt_ulim


CONTAINS

!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
LOGICAL FUNCTION qca(a,b)

 IMPLICIT NONE

 REAL, INTENT(IN) :: a,b

 qca = (ABS(a-b) > eps )

END FUNCTION qca
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
LOGICAL FUNCTION qcur(diff,lim)

 IMPLICIT NONE

 REAL    :: diff,lim

!----------------------------

 IF ( ABS(lim-err_ind) < eps ) THEN
   qcur = .TRUE.
 ELSE
   qcur = (diff <= lim)
 ENDIF

END FUNCTION qcur
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
LOGICAL FUNCTION qclr(diff,lim)

 IMPLICIT NONE

 REAL    :: diff,lim

!----------------------------

 IF ( ABS(lim-err_ind) < eps ) THEN
   qclr = .TRUE.
 ELSE
   qclr = (diff >= lim)
 ENDIF

END FUNCTION qclr
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
 SUBROUTINE allocate_mod

 !
 ! Allocated model station array
 !

 IMPLICIT NONE

 INTEGER :: k

 IF (ANY(hir%obs_is_allocated)) THEN
    WRITE(6,*)'Warning, some model stations are allocated'
    WRITE(6,*)'Stupid programmer, abort'
    CALL abort
 ENDIF

 IF ( maxtim == 0 )  maxtim=get_maxtim(sdate,edate,fcint)

 WRITE(6,*)'Maxtim for model data is ',maxtim

 !
 ! If model array is not allocated 
 ! do so and init arrays
 !

 DO k = 1,maxstn
    ALLOCATE(hir(k)%o(maxtim),&
             hir(k)%hgtmod(nexp))
    NULLIFY(hir(k)%pos)
    hir(k)%hgtmod = err_ind
 ENDDO
   
 hir%ntim       = 0
 hir%stnr       = 0
 hir%nexp       = nexp
 hir%nparver    = nparver
 hir%nfclengths = nfclengths
 hir%lat        = err_ind
 hir%lon        = err_ind
 hir%active     = .FALSE.

 hir%obs_is_allocated = .TRUE.

 RETURN

END SUBROUTINE allocate_mod
SUBROUTINE allocate_obs

 !
 ! Allocated observation station array
 !

 IMPLICIT NONE

 INTEGER :: k,si,ei

 IF (ANY(obs%obs_is_allocated)) THEN
    WRITE(6,*)'Warning, some obs stations are allocated'
    WRITE(6,*)'Stupid programmer, abort'
    CALL abort
 ENDIF

 ! Estimate maxtim if not given by user
 IF ( edate_obs == 0 ) edate_obs = edate
 IF (maxtim == 0) maxtim=get_maxtim(sdate,edate_obs,obint)

 WRITE(6,*)'Maxtim for observations is ',maxtim
 !
 ! If model array is not allocated 
 ! do so and init arrays
 !

 si = sdate     * 100 + stime
 ei = edate_obs * 100 + etime_obs

 DO k = 1,maxstn
    ALLOCATE(obs(k)%o(maxtim))
    IF ( use_pos ) THEN
       ALLOCATE(obs(k)%pos(si:ei))
       obs(k)%pos = 0
    ELSE
       NULLIFY(obs(k)%pos)
    ENDIF
 ENDDO
   
 obs%ntim       = 0
 obs%nexp       = nexp
 obs%stnr       = 0
 obs%nparver    = nparver
 obs%nfclengths = nfclengths

 obs%lat        = err_ind
 obs%lon        = err_ind
 obs%hgt        = err_ind

 obs%active     = .FALSE.

 obs%obs_is_allocated = .TRUE.

 RETURN

END SUBROUTINE allocate_obs
END MODULE data
