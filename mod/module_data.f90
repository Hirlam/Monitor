MODULE data

 USE types
 USE functions

 IMPLICIT NONE

 SAVE

! Constants
 REAL, PARAMETER :: err_ind = -999.e6 ! Error flag

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
                                        ! Remember to change colours in module_mymagics as well
 INTEGER, PARAMETER :: mparver = 200    ! Max parameters to verify
 INTEGER, PARAMETER :: maxfclen= 48     ! Maximum number of forecast lengths
 INTEGER, PARAMETER :: maxfclenval= 240 ! Maximum forecast length value in hours


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
 

 CHARACTER(LEN=30) :: formdb ='(I12,I4,XX(en15.5e2))'
 CHARACTER(LEN=len_lab), ALLOCATABLE :: obstype(:)
 CHARACTER(LEN=50), ALLOCATABLE :: station_name(:)

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
 INTEGER, PARAMETER ::  max_maxstn  = 5000
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

 ! Experiment and name
 INTEGER           :: nexp = 1                  ! Number of experiments
 CHARACTER(LEN=30) :: expname(maxexp)='OBS'     ! Name of experiments
 CHARACTER(LEN=30) :: statname='statistics.html'    ! Name of output statistics file
 CHARACTER(LEN=50) :: name='Unknown'            ! Station name
 CHARACTER(LEN=50) :: tag='#'                   ! Tag on plot
 CHARACTER(LEN=99) :: obspath        ='../'     ! Path to observation data
 CHARACTER(LEN=99) :: modpath(maxexp)='../'     ! Path to model data

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
 CHARACTER(LEN=100) :: data_source  = ''      ! Character string selection

 !TYPE(variable) :: tt =(0,0.,0.,0.,0.,'TT','Temperature','Deg')

 ! Graphics
 CHARACTER(LEN=20) :: graphics='GNUPLOT'

 ! Parameters to verify
 INTEGER :: nparver = mparver
 INTEGER :: tt_ind = 0 ! Temperature
 INTEGER :: tn_ind = 0 ! Min temperature
 INTEGER :: tx_ind = 0 ! Max temperature
 INTEGER :: td_ind = 0 ! Dew point temperature
 INTEGER :: vi_ind = 0 ! Visibility
 INTEGER :: ff_ind = 0 ! Wind speed
 INTEGER :: fx_ind = 0 ! Max wind speed
 INTEGER :: gg_ind = 0 ! Wind gust,instantaneous
 INTEGER :: gx_ind = 0 ! Max wind gust
 INTEGER :: dd_ind = 0 ! Wind direction
 INTEGER :: uw_ind = 0 ! momentum flux
 INTEGER :: wt_ind = 0 ! heat flux
 INTEGER :: wq_ind = 0 ! latent heat flux
 INTEGER :: rh_ind = 0 ! Relative humidity
 INTEGER :: sw_ind = 0 ! Short wave radiation
 INTEGER :: lw_ind = 0 ! Long wave radiation
 INTEGER :: su_ind = 0 ! Shortwave radiation up
 INTEGER :: sd_ind = 0 ! Shortwave radiation down
 INTEGER :: lu_ind = 0 ! Long wave radiation up
 INTEGER :: ld_ind = 0 ! Long wave radiation down
 INTEGER :: nr_ind = 0 ! Net Radiation
 INTEGER :: gr_ind = 0 ! Global Radiation
 INTEGER :: gs_ind = 0 ! Ground heat flux
 INTEGER :: gc_ind = 0 ! Ground heat flux calculated ( Rn + H + L )
 INTEGER :: ps_ind = 0 ! Surface pressure
 INTEGER :: pe_ind = 0 ! Precipitaton
 INTEGER :: pd_ind = 0 ! Precipitaton, daily
 INTEGER :: nn_ind = 0 ! Cloud cover
 INTEGER :: fi_ind = 0 ! Geopotential
 INTEGER :: rf_ind = 0 ! Runoff
 INTEGER :: qq_ind = 0 ! Specific humidity
 INTEGER :: hb_ind = 0 ! Surface heat budget residual
 INTEGER :: wp_ind = 0 ! Wind power
 INTEGER :: wh_ind = 0 ! Energy (kWh)
 INTEGER :: tz_ind = 0 ! dT/dz
 INTEGER :: uz_ind = 0 ! dU/dz
 INTEGER :: tu_ind = 0 ! dT/dU
 INTEGER :: la_ind = 0 ! Latitude
 INTEGER :: hg_ind = 0 ! Station height
 INTEGER :: lev_typ(mparver) = 0  ! No need to set, done in set_obstype.f
 REAL    :: lev_lst(mparver) = 0. ! List of vertical levels
                                  ! if lev_lst(n) < lev_lst(n+1) height is assumed
                                  ! if lev_lst(n) > lev_lst(n+1) pressure is assumed

 ! Gross Error limits
 REAL :: fi_lim = 500. 
 REAL :: tt_lim = 50. 
 REAL :: td_lim = 50. 
 REAL :: vi_lim = 500000. 
 REAL :: ff_lim = 50. 
 REAL :: dd_lim = 720. 
 REAL :: rh_lim = 100. 
 REAL :: ps_lim = 50. 
 REAL :: pe_lim = 50. 
 REAL :: pd_lim = 50. 
 REAL :: lw_lim = 500. 
 REAL :: su_lim = 500. 
 REAL :: sd_lim = 500. 
 REAL :: lu_lim = 500. 
 REAL :: ld_lim = 500. 
 REAL :: wt_lim = 300. 
 REAL :: wq_lim = 300. 
 REAL :: sw_lim = 500. 
 REAL :: qq_lim = 50.
 REAL :: uw_lim = 2.
 REAL :: nr_lim = 900.
 REAL :: gr_lim = 1000.
 REAL :: nn_lim = 10.

 ! Upper limits
 REAL :: fi_ulim = 1.e9
 REAL :: tt_ulim = 400.
 REAL :: td_ulim = 400.
 REAL :: vi_ulim = 1.e12
 REAL :: ff_ulim = 200.
 REAL :: dd_ulim = 360.
 REAL :: rh_ulim = 100.
 REAL :: ps_ulim = 1100.
 REAL :: pe_ulim = 500.
 REAL :: pd_ulim = 500.
 REAL :: lw_ulim = 500. 
 REAL :: su_ulim = 500. 
 REAL :: sd_ulim = 500. 
 REAL :: lu_ulim = 500. 
 REAL :: ld_ulim = 500. 
 REAL :: wt_ulim = 300. 
 REAL :: wq_ulim = 300. 
 REAL :: sw_ulim = 500. 
 REAL :: qq_ulim = 100.
 REAL :: uw_ulim = 2.
 REAL :: nr_ulim = 900.
 REAL :: gr_ulim = 1000.
 REAL :: nn_ulim = 8.

 ! Lower limits
 REAL :: fi_llim = 0.
 REAL :: tt_llim = -200.
 REAL :: td_llim = -200.
 REAL :: vi_llim = 0.
 REAL :: ff_llim = 0.
 REAL :: dd_llim = 0.
 REAL :: rh_llim = 0.
 REAL :: ps_llim = 0.
 REAL :: pe_llim = 0.
 REAL :: pd_llim = 0.
 REAL :: lw_llim = 0. 
 REAL :: su_llim = 0. 
 REAL :: sd_llim = 0. 
 REAL :: lu_llim = 0. 
 REAL :: ld_llim = 0. 
 REAL :: wt_llim = 0. 
 REAL :: wq_llim = 0. 
 REAL :: sw_llim = 0. 
 REAL :: qq_llim = 0.
 REAL :: uw_llim = -2.
 REAL :: nr_llim = -900.
 REAL :: gr_llim = 0
 REAL :: nn_llim = 0.

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

 ! Parameters for bias_map
 REAL    :: map_scale                 = 2.0e7    ! Map scale
 REAL    :: MAP_VERTICAL_LONGITUDE    = 0.0
 REAL    :: MAP_CENTRE_LATITUDE       = 55.      ! Map centre lat
 REAL    :: MAP_CENTRE_LONGITUDE      = 25.      ! Map centre lon
 REAL    :: MAP_LOWER_LEFT_LATITUDE   = 40.
 REAL    :: MAP_LOWER_LEFT_LONGITUDE  = 10.
 REAL    :: MAP_UPPER_RIGHT_LATITUDE  = 70.
 REAL    :: MAP_UPPER_RIGHT_LONGITUDE = 50.

 ! POLAR_STEREOGRAPHIC and MERCATOR is tested
 CHARACTER(LEN=50) :: MAP_PROJECTION      = 'POLAR_STEREOGRAPHIC'

 ! Use CENTRE or CORNER
 CHARACTER(LEN=50) :: MAP_AREA_DEFINITION = 'CENTRE'

 LOGICAL :: plot_bias_map          = .FALSE.  ! Plot map with biases
 LOGICAL :: print_bias_map         = .FALSE.  ! Print map with biases
 LOGICAL :: plot_obs_map           = .FALSE.  ! Plot map with observations
 LOGICAL :: print_obs_map          = .FALSE.  ! Print map with observations
 REAL    ::  map_obs_interval(7,mparver)= -1.      ! Set your own obs interval 
 REAL    :: map_bias_interval(7,mparver)= -1.      ! Set your own bias interval
 REAL    :: map_rmse_interval(7,mparver)= -1.      ! Set your own rmse interval
 INTEGER :: map_type                    =  0       ! 0 plots symbols, 1 plots numbers


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


 ! Special precipitation thing
 INTEGER :: pe_interval = 12
 INTEGER :: accu_int(mparver)      =0         ! Accumulation interval in hours for variable
                                              ! 0 means the value is instant

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

 ! Contingency flags

 LOGICAL :: lcontingency = .FALSE.

 INTEGER :: cont_param          = 0
 INTEGER :: cont_ind(mparver)   = 0
 INTEGER :: cont_class(mparver) = 0
 REAL    :: cont_lim(mpre_cla,mparver) = 0.0 !

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
                 nparver,                               &
                 tt_ind,ff_ind,dd_ind,uw_ind,wt_ind,    &
                 sw_ind,lw_ind,lu_ind,ld_ind,           &
                 su_ind,sd_ind,td_ind,vi_ind,           &
                 ps_ind,pe_ind,pd_ind,rh_ind,           &
                 nn_ind,fi_ind,rf_ind,nr_ind,wq_ind,    &
                 qq_ind,gs_ind,gc_ind,gr_ind,hb_ind,    &
                 tz_ind,uz_ind,tu_ind,la_ind,hg_ind,    &
                 wp_ind,wh_ind,tn_ind,tx_ind,           &
                 gg_ind,gx_ind,fx_ind,                  &
                 lev_typ,lev_lst,                       &
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
                 lprint_scat,                           &
                 lcontingency,                          &
                 ltimeserie_stat,                       &
                 lprint_timeserie_stat,                 &
                 plot_bias_map,                         &
                 print_bias_map,                        &
                 map_scale,                             &
                 plot_obs_map,                          &
                 print_obs_map,                         &
                 map_obs_interval,                      &
                 map_rmse_interval,                     &
                 map_bias_interval,map_type,            &
                 MAP_VERTICAL_LONGITUDE,                &
                 MAP_CENTRE_LATITUDE,                   &
                 MAP_CENTRE_LONGITUDE,                  &
                 MAP_LOWER_LEFT_LATITUDE,               &
                 MAP_LOWER_LEFT_LONGITUDE,              &
                 MAP_UPPER_RIGHT_LATITUDE,              &
                 MAP_UPPER_RIGHT_LONGITUDE,             &
                 MAP_PROJECTION,                        &
                 MAP_AREA_DEFINITION,                   &
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
                 fi_lim,tt_lim,ff_lim,dd_lim,rh_lim,    &
                 ps_lim,pe_lim,sw_lim,lw_lim,lu_lim,    &
                 ld_lim,qq_lim,su_lim,sd_lim,           &
                 uw_lim,wt_lim,nr_lim,gr_lim,wq_lim,    &
                 nn_lim,td_lim,vi_lim,                  &
                 ff_llim,                               &
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
                 period_type,period_freq,pe_interval,   &
                 print_qc,accu_int,                     &
                 lquality_control,qc_fclen,qc_lim,      &
                 estimate_qc_limit,qc_lim_scale,        &
                 corr_pairs,flag_pairs,exp_pairs,       &
                 scat_min,scat_max,scat_magn,           &
                 cont_ind,cont_class,cont_lim,          &
                 cont_param,                            &
                 graphics

CONTAINS

!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
LOGICAL FUNCTION qca(a,b)

 IMPLICIT NONE

 REAL, INTENT(IN) :: a,b

 qca = (ABS(a-b) > 1.e-6)

END FUNCTION qca
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
LOGICAL FUNCTION qc(diff,k)

 IMPLICIT NONE

 INTEGER :: k
 REAL    :: diff,diff_lim

!----------------------------

 !  Multi level fields
 IF (ltemp) THEN

    IF     (lev_typ(k).EQ.dd_ind) THEN
       diff_lim = dd_lim
    ELSEIF (lev_typ(k).EQ.fi_ind) THEN
       diff_lim = fi_lim
    ELSEIF (lev_typ(k).EQ.ff_ind) THEN
       diff_lim = ff_lim
    ELSEIF (lev_typ(k).EQ.td_ind) THEN
       diff_lim = td_lim
    ELSEIF (lev_typ(k).EQ.tt_ind) THEN
       diff_lim = tt_lim
    ELSEIF (lev_typ(k).EQ.rh_ind) THEN
       diff_lim = rh_lim
    ELSEIF (lev_typ(k).EQ.qq_ind) THEN
       diff_lim = qq_lim
    ELSE
       diff_lim = ABS(diff) +1.
    ENDIF

 ELSE

    IF     (k.EQ.ps_ind) THEN
       diff_lim = ps_lim
    ELSEIF (k.EQ.pe_ind) THEN
       diff_lim = pe_lim
    ELSEIF (k.EQ.pd_ind) THEN
       diff_lim = pd_lim
    ELSEIF (k.EQ.rh_ind) THEN
       diff_lim = rh_lim
    ELSEIF (k.EQ.tt_ind) THEN
       diff_lim = tt_lim
    ELSEIF (k.EQ.td_ind) THEN
       diff_lim = td_lim
    ELSEIF (k.EQ.vi_ind) THEN
       diff_lim = vi_lim
    ELSEIF (k.EQ.ff_ind) THEN
       diff_lim = ff_lim
    ELSEIF (k.EQ.dd_ind) THEN
       diff_lim = dd_lim
    ELSEIF (k.EQ.sw_ind) THEN
       diff_lim = sw_lim
    ELSEIF (k.EQ.nr_ind) THEN
       diff_lim = nr_lim
    ELSEIF (k.EQ.gr_ind) THEN
       diff_lim = gr_lim
    ELSEIF (k.EQ.wq_ind) THEN
       diff_lim = wq_lim
    ELSEIF (k.EQ.wt_ind) THEN
       diff_lim = wt_lim
    ELSEIF (k.EQ.uw_ind) THEN
       diff_lim = uw_lim
    ELSEIF (k.EQ.su_ind) THEN
       diff_lim = su_lim
    ELSEIF (k.EQ.lw_ind) THEN
       diff_lim = lw_lim
    ELSEIF (k.EQ.nn_ind) THEN
       diff_lim = nn_lim
    ELSEIF (k.EQ.qq_ind) THEN
       diff_lim = qq_lim
    ELSE
       diff_lim = ABS(diff) +1.
    ENDIF

 ENDIF

 qc = (ABS(diff) <= diff_lim)

END FUNCTION qc
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
LOGICAL FUNCTION qcl(diff,k)

 IMPLICIT NONE

 INTEGER :: k
 REAL    :: diff,diff_lim

!----------------------------

 !  Multi level fields
 IF (ltemp) THEN

    IF     (lev_typ(k).EQ.dd_ind) THEN
       diff_lim = dd_llim
    ELSEIF (lev_typ(k).EQ.fi_ind) THEN
       diff_lim = fi_llim
    ELSEIF (lev_typ(k).EQ.ff_ind) THEN
       diff_lim = ff_llim
    ELSEIF (lev_typ(k).EQ.td_ind) THEN
       diff_lim = td_llim
    ELSEIF (lev_typ(k).EQ.tt_ind) THEN
       diff_lim = tt_llim
    ELSEIF (lev_typ(k).EQ.rh_ind) THEN
       diff_lim = rh_llim
    ELSEIF (lev_typ(k).EQ.qq_ind) THEN
       diff_lim = qq_llim
    ELSE
       diff_lim = ABS(diff) +1.
    ENDIF

 ELSE

    IF     (k.EQ.ps_ind) THEN
       diff_lim = ps_llim
    ELSEIF (k.EQ.pe_ind) THEN
       diff_lim = pe_llim
    ELSEIF (k.EQ.pd_ind) THEN
       diff_lim = pd_llim
    ELSEIF (k.EQ.rh_ind) THEN
       diff_lim = rh_llim
    ELSEIF (k.EQ.tt_ind) THEN
       diff_lim = tt_llim
    ELSEIF (k.EQ.td_ind) THEN
       diff_lim = td_llim
    ELSEIF (k.EQ.vi_ind) THEN
       diff_lim = vi_llim
    ELSEIF (k.EQ.ff_ind) THEN
       diff_lim = ff_llim
    ELSEIF (k.EQ.dd_ind) THEN
       diff_lim = dd_llim
    ELSEIF (k.EQ.sw_ind) THEN
       diff_lim = sw_llim
    ELSEIF (k.EQ.nr_ind) THEN
       diff_lim = nr_llim
    ELSEIF (k.EQ.gr_ind) THEN
       diff_lim = gr_llim
    ELSEIF (k.EQ.wq_ind) THEN
       diff_lim = wq_llim
    ELSEIF (k.EQ.wt_ind) THEN
       diff_lim = wt_llim
    ELSEIF (k.EQ.uw_ind) THEN
       diff_lim = uw_llim
    ELSEIF (k.EQ.su_ind) THEN
       diff_lim = su_llim
    ELSEIF (k.EQ.lw_ind) THEN
       diff_lim = lw_llim
    ELSEIF (k.EQ.nn_ind) THEN
       diff_lim = nn_llim
    ELSEIF (k.EQ.qq_ind) THEN
       diff_lim = qq_llim
    ELSE
       diff_lim = diff - 1.
    ENDIF

 ENDIF

 qcl = ( diff >= diff_lim)

END FUNCTION qcl
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
LOGICAL FUNCTION qcu(diff,k)

 IMPLICIT NONE

 INTEGER :: k
 REAL    :: diff,diff_lim

!----------------------------

 !  Multi level fields
 IF (ltemp) THEN

    IF     (lev_typ(k).EQ.dd_ind) THEN
       diff_lim = dd_ulim
    ELSEIF (lev_typ(k).EQ.fi_ind) THEN
       diff_lim = fi_ulim
    ELSEIF (lev_typ(k).EQ.ff_ind) THEN
       diff_lim = ff_ulim
    ELSEIF (lev_typ(k).EQ.tt_ind) THEN
       diff_lim = tt_ulim
    ELSEIF (lev_typ(k).EQ.td_ind) THEN
       diff_lim = td_ulim
    ELSEIF (lev_typ(k).EQ.rh_ind) THEN
       diff_lim = rh_ulim
    ELSEIF (lev_typ(k).EQ.qq_ind) THEN
       diff_lim = qq_ulim
    ELSE
       diff_lim = ABS(diff) +1.
    ENDIF

 ELSE

    IF     (k.EQ.ps_ind) THEN
       diff_lim = ps_ulim
    ELSEIF (k.EQ.pe_ind) THEN
       diff_lim = pe_ulim
    ELSEIF (k.EQ.pd_ind) THEN
       diff_lim = pd_ulim
    ELSEIF (k.EQ.rh_ind) THEN
       diff_lim = rh_ulim
    ELSEIF (k.EQ.tt_ind) THEN
       diff_lim = tt_ulim
    ELSEIF (k.EQ.td_ind) THEN
       diff_lim = td_ulim
    ELSEIF (k.EQ.vi_ind) THEN
       diff_lim = vi_ulim
    ELSEIF (k.EQ.ff_ind) THEN
       diff_lim = ff_ulim
    ELSEIF (k.EQ.dd_ind) THEN
       diff_lim = dd_ulim
    ELSEIF (k.EQ.sw_ind) THEN
       diff_lim = sw_ulim
    ELSEIF (k.EQ.nr_ind) THEN
       diff_lim = nr_ulim
    ELSEIF (k.EQ.gr_ind) THEN
       diff_lim = gr_ulim
    ELSEIF (k.EQ.wq_ind) THEN
       diff_lim = wq_ulim
    ELSEIF (k.EQ.wt_ind) THEN
       diff_lim = wt_ulim
    ELSEIF (k.EQ.uw_ind) THEN
       diff_lim = uw_ulim
    ELSEIF (k.EQ.su_ind) THEN
       diff_lim = su_ulim
    ELSEIF (k.EQ.lw_ind) THEN
       diff_lim = lw_ulim
    ELSEIF (k.EQ.nn_ind) THEN
       diff_lim = nn_ulim
    ELSEIF (k.EQ.qq_ind) THEN
       diff_lim = qq_ulim
    ELSE
       diff_lim = diff + 1.
    ENDIF

 ENDIF

 qcu = (ABS(diff) <= diff_lim)

END FUNCTION qcu
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

 maxtim=get_maxtim(sdate,edate,fcint)

 WRITE(6,*)'Maxtim for model data is ',maxtim

 !
 ! If model array is not allocated 
 ! do so and init arrays
 !

 DO k = 1,maxstn
    ALLOCATE(hir(k)%o(maxtim))
    NULLIFY(hir(k)%pos)
 ENDDO
   
 hir%ntim       = 0
 hir%stnr       = 0
 hir%nexp       = nexp
 hir%nparver    = nparver
 hir%nfclengths = nfclengths
 
 hir%lat        = err_ind
 hir%lon        = err_ind
 hir%hgt        = err_ind

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
