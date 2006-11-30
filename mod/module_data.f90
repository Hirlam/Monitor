MODULE data

 USE types
 USE functions

 IMPLICIT NONE

! Constants
 REAL, PARAMETER :: err_ind = -999.e6 ! Error flag

! I/O
 INTEGER, PARAMETER :: lunnam  = 10	! Namelist unit
 INTEGER, PARAMETER :: lunin   = 13
 INTEGER, PARAMETER :: lunin2  = 16
 INTEGER, PARAMETER :: lunout  = 14
 INTEGER, PARAMETER :: lunstat = 15
 INTEGER, PARAMETER :: len_lab = 6
 INTEGER, PARAMETER :: maxexp  = 5	! Max experiments
 INTEGER, PARAMETER :: mparver = 100	! Max parameters to verify
 INTEGER, PARAMETER :: maxfclen= 48	! Maximum number of forecast lengths


! Main arrays 
 TYPE (station), ALLOCATABLE, DIMENSION(:) :: obs,hir
 TYPE (statpar), ALLOCATABLE :: stat(:)


! Some global values
 INTEGER :: nrun             = 0
 INTEGER :: ntimmax          = 0
 INTEGER :: ntimmax_findplot = 0
 INTEGER :: timdiff          = 0
 INTEGER :: csi              = 0 ! Current station index
 INTEGER :: active_stations  = 0 ! Number of active stations
 INTEGER :: active_stations_ver  = 0 ! Number of active stations for verification

 LOGICAL :: doing_monthvise = .FALSE.
 LOGICAL :: copied_obs      = .FALSE.
 LOGICAL :: copied_mod      = .FALSE.

 CHARACTER(LEN=30) :: formdb ='(I12,I4,XX(en15.5e2))'
 CHARACTER(LEN=len_lab), ALLOCATABLE :: obstype(:)
 CHARACTER(LEN=50), ALLOCATABLE :: station_name(:)

!
! NAMELIST
!

 ! Time control
 INTEGER :: sdate  = 19990901 	 ! Start date
 INTEGER :: stime  = 0 		 ! Start time
 INTEGER :: edate  = 20000320 	 ! End date
 INTEGER :: etime  = 0 		 ! End time
 INTEGER :: maxtim = 0           ! Estimated number of times 
                                 ! set to 0 to let the program decide
 INTEGER :: maxtim_scat = 0      ! Estimated number of times for
                                 ! scatter/frequency plots
                                 ! set to 0 to let the program decide

 ! Station selection by list or area
 INTEGER :: maxstn             = 1  ! Max stations
 INTEGER :: stnlist(5000)      = 0  ! Station numbers
 INTEGER :: stnlist_bl(5000)   = 0  ! Black listed stations
 INTEGER :: stnlist_plot(5000) = -1 ! Additional stations to plot
                                    ! -1 gives none 
                                    !  0 gives all 
 TYPE (box) :: cbox                 ! Area box (S,W,N,E) corners
 LOGICAL    :: lpoly = .FALSE.      ! Area selection by polygon

 ! Experiment and name
 INTEGER           :: nexp = 1			! Number of experiments
 CHARACTER(LEN=30) :: expname(maxexp)='OBS'     ! Name of experiments
 CHARACTER(LEN=30) :: statname='hirlamX.dat'    ! Name of output statistics file
 CHARACTER(LEN=50) :: name='Unknown'		! Station name
 CHARACTER(LEN=99) :: obspath        ='../'	! Path to observation data
 CHARACTER(LEN=99) :: modpath(maxexp)='../'	! Path to model data

 ! Fc len and times to verify
 INTEGER :: nfclengths       = 1                ! Number of fclengths to verify
 INTEGER :: ntimver          = 1                ! Number of hours to verify
 INTEGER :: fclen(maxfclen)  = 0                ! fclengths to read in hours
 INTEGER :: fcint            = 6                ! Interval between forecasts in hour
 INTEGER :: obint            = 1                ! Interval between observations in hour
                                                ! Not applicable everywhere 
 INTEGER :: time_shift                 =  0     ! Shift in time when doing daily average
 INTEGER :: monthvise_freq             =  1     ! Frequency, in months, of monthvise verify
 INTEGER :: yearvise_freq              =  3     ! Frequency, in months, of seasonal verify, 
                                                ! only 1 (monthly) or 3 (seasonal as in DJF) available
 INTEGER :: yearvise_wind              =  7     ! Accumulation period for seasonal verify
 INTEGER :: timeserie_wind             =  0     ! Accumulation period for timeserie plots
 INTEGER :: window_pos                 =  0     ! Plot position of period
                                                ! -1 Beginning
                                                !  0 Centre
                                                !  1 End
 INTEGER :: time_stat_fclen(maxfclen)  = -1     ! Timeserie_stat fclen usage, -1 means any

 ! Frequency distribution plots
 INTEGER :: ncla                 = 26   ! Number of classes
 INTEGER :: classtype            = 0    ! Linear classtypes
 INTEGER :: npre_cla             = 1    ! Number of predefined classes
 INTEGER, PARAMETER :: mpre_cla  = 100  ! Max number of predined classes
 REAL    :: pre_fcla(mpre_cla)   = 0.   ! Predefined classes
 REAL    :: maxcla               = 0.   ! Maximum class value
 REAL    :: mincla               = 1.   ! Minimum class value

 ! Data to verify 
 INTEGER :: data_to_verify = 0          ! Which case to select in my_choice.f

 ! Parameters to verify
 INTEGER :: nparver = mparver
 INTEGER :: tt_ind = 0 ! Temperature
 INTEGER :: ff_ind = 0 ! Wind speed
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
 INTEGER :: diff_ind = 0 ! Pointer to difference scatter plot index
 INTEGER :: comp_ind = 0 ! Pointer to difference scatter plot index
 INTEGER :: lev_typ(mparver) = 0  ! No need to set, done in set_obstype.f
 REAL    :: lev_lst(mparver) = 0  ! List of vertical levels
                                  ! if lev_lst(n) < lev_lst(n+1) height is assumed
                                  ! if lev_lst(n) > lev_lst(n+1) pressure is assumed

 ! Gross Error limits
 REAL :: fi_lim = 500. 
 REAL :: tt_lim = 50. 
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
 REAL :: tt_ulim = 200.
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
 LOGICAL :: ltiming          = .FALSE.	! Measure performance (use gprof instead)
 LOGICAL :: ltemp            = .FALSE.	! Multi level fields
 LOGICAL :: lfcver           = .FALSE. 	! Verify by fclength else by time of day
 LOGICAL :: leach_station    = .FALSE.	! Verify each station separately
 LOGICAL :: lallstat         = .TRUE.	! Show statistics for all stations
 LOGICAL :: monthvise        = .FALSE.	! Show monthly statistics
 LOGICAL :: lfindplot        = .FALSE.	! Call findplot obsolete at the moment
 LOGICAL :: ldaymean         = .FALSE.	! Calculate daily means 
 LOGICAL :: ltimeserie       = .FALSE.	! Plot timeseries for all stations
 LOGICAL :: ltimeserie_stat  = .FALSE.	! Plot timeserie statistics for all stations
 LOGICAL :: ltimeserie_stat_month  = .FALSE.	! Plot timeserie statistics for all stations month by month
 LOGICAL :: lplot_freq       = .FALSE.	! Make frequency distribution plots
 LOGICAL :: lplot_scat       = .FALSE.	! Make scatterplots
 LOGICAL :: lverify          = .TRUE.	! Call verify main subroutine
 LOGICAL :: lstat_gen        = .TRUE.	! Calculate general statistics
 LOGICAL :: lprint_selection = .FALSE.	! Print selected stations
 LOGICAL :: lprint_read      = .FALSE.	! Print diagnostics when reading, obsolete
 INTEGER ::  print_read      = 1        ! Print diagnostics when reading
 LOGICAL :: lprint_summary   = .FALSE.	! Print comprehensive station summary
 LOGICAL :: lprint_verif     = .FALSE.	! Print diagnostics
 LOGICAL :: lprint_findp     = .FALSE.	! Print diagnostics
 LOGICAL :: lprint_do_stat   = .FALSE.	! Print diagnostics
 LOGICAL :: lplot_vert       = .FALSE.	! Create plot file for vertical temp statistics
 LOGICAL :: lplot_vert_month = .FALSE.	! Create plot file for monthly vertical temp statistics 
 LOGICAL :: lplot_stat       = .FALSE.	! Create plot file for statistics
 LOGICAL :: lplot_stat_month = .FALSE.	! Create plot file for monthly statistics
 LOGICAL :: lplot_stat_year  = .FALSE.	! Create plot file for seasonal statistics
 LOGICAL :: lprint_gross     = .FALSE.	! Print gross error diagnostics
 LOGICAL :: lreject_gross    = .FALSE.  ! Reject gross error data
 LOGICAL :: lyearvise        = .FALSE.  ! Do seasonal cycle
 LOGICAL :: use_database     = .FALSE.  ! Try to use database, obsolete
 LOGICAL :: release_memory   = .TRUE.   ! Release memory as soon as data have been used
 LOGICAL :: release_memory_f = .FALSE.  ! Release memory after findplot, obsolete
 LOGICAL :: gap_filled_data  = .FALSE.  ! Allow gap filled data, obsolete
 LOGICAL :: ldiff            = .FALSE.  ! Plot observation - model departure
 LOGICAL :: lnorm            = .FALSE.  ! Plot ( mod - obs ) / obs
 LOGICAL :: show_fc_length   = .TRUE.   ! Give used fc lengths on plot
 LOGICAL :: all_var_present  = .FALSE.  ! Demand that all variables must have values
                                        ! At the same time
 LOGICAL :: use_pos  = .FALSE.          ! CALL verify_pos

 ! Parameters for bias_map
 INTEGER ::  map_hours(maxfclen)   = 0        ! Hours to plot -1 gives all available
 INTEGER :: nmap_hours             = 0        ! Number of hours to plot
 REAL    :: map_scale              = 2.0e7    ! Map scale
 REAL    :: MAP_CENTRE_LATITUDE    = 55.      ! Map centre lat
 REAL    :: MAP_CENTRE_LONGITUDE   = 25.      ! Map centre lon
 LOGICAL :: plot_bias_map          = .FALSE.  ! Plot map with biases
 LOGICAL :: plot_obs_map           = .FALSE.  ! Plot map with observations
 REAL    :: map_obs_interval(10,7) = -1.      ! Set your own obs interval 
 REAL    :: map_bias_interval(10,7)= -1.      ! Set your own bias interval
 REAL    :: map_rmse_interval(10,7)= -1.      ! Set your own rmse interval
 INTEGER :: map_type               =  0       ! 0 plots symbols, 1 plots numbers


 ! What to plot on verification plots
 LOGICAL :: show_bias        = .TRUE.   ! Plot bias
 LOGICAL :: show_rmse        = .TRUE.   ! Plot rmse
 LOGICAL :: show_stdv        = .FALSE.  ! Plot stdv
 LOGICAL :: show_obs         = .FALSE.  ! Plot full obs turnes the others to false


 ! Special conditions
 INTEGER :: special_flag     = 0        ! Flag for special conditions
 LOGICAL :: lspecial_cond    = .FALSE.  ! Use special conditions


 ! Kalman filter
 TYPE(kalman_type) kalvar_n(mparver)          ! Kalman settings
 LOGICAL :: use_kalman             = .FALSE.  ! Kalman filter values
 INTEGER :: kalman_frequency_n     = 24       ! Expected data frequency
 INTEGER :: kalman_fclen(maxfclen) = 0        ! fclen to use in kalman filter


 ! Namlist 
 namelist/namver/sdate,stime,edate,etime,		&
                 maxstn,maxtim,maxtim_scat,             &
                 ntimver,time_shift,                    &
                 nfclengths,fclen,fcint,obint,          &
                 yearvise_freq,yearvise_wind,	        &
                 timeserie_wind,window_pos,             &
                 monthvise_freq,			&
                 stnlist,stnlist_bl,stnlist_plot,	&
                 nexp,expname,				&
                 nparver,				&
                 tt_ind,ff_ind,dd_ind,uw_ind,wt_ind,	&
                 sw_ind,lw_ind,lu_ind,ld_ind,		&
                 su_ind,sd_ind,				&
                 ps_ind,pe_ind,pd_ind,rh_ind,		&
                 nn_ind,fi_ind,rf_ind,nr_ind,wq_ind,    &
                 qq_ind,gs_ind,gc_ind,gr_ind,hb_ind,	&
                 tz_ind,uz_ind,tu_ind,la_ind,hg_ind,    &
                 wp_ind,wh_ind,       			&
                 diff_ind,comp_ind,   			&
                 lev_typ,lev_lst,                       &
                 name,statname,				&
                 obspath,modpath,                       &
                 lfcver,leach_station,ltiming,          &
                 lallstat,                              &
                 monthvise,lfindplot,                   &
                 lstat_gen,lverify,          		&
                 lplot_stat,ldaymean,                   &
                 lplot_vert_month,lplot_vert,     	&
                 ltimeserie,lplot_scat,                 &
                 ltimeserie_stat,                       &
                 ltimeserie_stat_month,                 &
                 time_stat_fclen,                       &
                 lplot_stat_month,                      &
                 lplot_stat_year,                       &
                 use_kalman,kalvar_n,                   &
                 kalman_fclen,kalman_frequency_n,       &
                 plot_bias_map,map_hours,nmap_hours,    &
                 map_scale,plot_obs_map,                &
                 map_obs_interval,                      &
                 map_rmse_interval,                     &
                 map_bias_interval,map_type,            &
                 MAP_CENTRE_LATITUDE,                   &
                 MAP_CENTRE_LONGITUDE,                  &
                 lprint_gross,lprint_selection,         &
                 lprint_summary,                        &
                 lprint_read,print_read,                &
                 lprint_verif,lprint_findp,	        &
                 lprint_do_stat,lreject_gross,		&
                 release_memory,lplot_freq,		&
                 release_memory_f,       		&
                 cbox,lpoly,data_to_verify,ltemp,       &
                 lyearvise,use_database,	        &
                 fi_lim,tt_lim,ff_lim,dd_lim,rh_lim,    &
                 ps_lim,pe_lim,sw_lim,lw_lim,lu_lim,	&
                 ld_lim,qq_lim,su_lim,sd_lim,		&
                 uw_lim,wt_lim,nr_lim,gr_lim,wq_lim,	&
                 nn_lim,                                &
                 ff_llim,                               &
                 sumup_tolerance,			&
                 my_xmin,my_xmax,my_ymin,my_ymax,	&
                 lspecial_cond,special_flag,		&
                 gap_filled_data,ldiff,lnorm,           &
                 show_fc_length,all_var_present,        &
                 use_pos,                               &
                 ncla,classtype,npre_cla,pre_fcla,      &
                 maxcla,mincla,                         &
                 show_bias,show_rmse,show_stdv,show_obs

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
       diff_lim = diff + 1.
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
       diff_lim = diff - 1.
    ENDIF

 ENDIF

 qcu = (ABS(diff) <= diff_lim)

END FUNCTION qcu
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
 SUBROUTINE write_database(stat_i,flag)

  IMPLICIT NONE

  ! Input

  INTEGER, INTENT(IN) :: stat_i,flag
 
  ! Local

  INTEGER :: i,ii,j,ierr,ind

  CHARACTER(LEN=80) :: dbname ='XXX_00000000'
  CHARACTER(LEN=30) :: form =' '

  LOGICAL :: all_data_incorrect

!-----------------------------------------------

  ! Check if we really have station data


  SELECT CASE(flag)

  CASE(0)

     ! This is an observation
     WRITE(6,*)'NTIM',obs%ntim
   
     IF(obs(stat_i)%ntim == 0) THEN
        WRITE(6,*)'No data to store for station ', stat_i,obs(stat_i)%stnr
        RETURN
     ENDIF
     
     ! Set station name and open file
     WRITE(dbname(1:3),'(A3)')'OBS'
     WRITE(dbname(5:12),'(I8.8)')obs(stat_i)%stnr
   
     !  Open file
   
     OPEN(lunout,file=dbname,STATUS='NEW',IOSTAT=ierr) 
   
     IF(ierr.NE.0) THEN
        WRITE(6,*)'Database file exists :',TRIM(dbname)
        WRITE(6,*)'Skip writing'
        RETURN
     ENDIF

     ii = 0
     DO i=1,obs(stat_i)%ntim
        all_data_incorrect = .TRUE.
        DO j=1,obs(stat_i)%nparver
         all_data_incorrect = (all_data_incorrect .AND. 		&
                               ABS( obs(stat_i)%o(i)%val(j)-err_ind ).LT. 1.e-6 )
        ENDDO

        IF (all_data_incorrect)  CYCLE 
        ii = ii + 1
     ENDDO
       
     ! Write station caracteristics
     WRITE(lunout,*)obs(stat_i)%stnr,			&
                    ii,							&
                    obs(stat_i)%nexp,			&
                    obs(stat_i)%nfclengths,	&
                    obs(stat_i)%nparver			
   
     WRITE(lunout,*)obs(stat_i)%lat,obs(stat_i)%lon
   
     ! Write parameter identifier
   
     WRITE(lunout,*)obstype(1:nparver)
   
     ! Write data
   
     i = INDEX(formdb,'XX')
     form = formdb
     WRITE(form(i:i+1),'(I2.2)')nparver
   
  
     DO i=1,obs(stat_i)%ntim
        all_data_incorrect = .TRUE.
        DO j=1,obs(stat_i)%nparver
         all_data_incorrect = (all_data_incorrect .AND. 		&
                               ABS( obs(stat_i)%o(i)%val(j)-err_ind ).LT. 1.e-6 )
        ENDDO

        IF (all_data_incorrect)  CYCLE 
        WRITE(lunout,form)			&
        obs(stat_i)%o(i)%date,obs(stat_i)%o(i)%time,obs(stat_i)%o(i)%val
     ENDDO

  CASE(1)

     ! This is a model file

     IF(hir(stat_i)%ntim.EQ.0) THEN
        WRITE(6,*)'No data to store for station ', stat_i,hir(stat_i)%stnr
        RETURN
     ENDIF

     ! Set station name and open file
     WRITE(dbname(1:3),'(A3)')'MOD'
     WRITE(dbname(5:12),'(I8.8)')hir(stat_i)%stnr

     !  Open file

     OPEN(lunout,file=dbname,STATUS='NEW',IOSTAT=ierr)

     IF(ierr.NE.0) THEN
        WRITE(6,*)'Database file exists :',TRIM(dbname)
        WRITE(6,*)'Skip writing'
        RETURN
     ENDIF

     ! Write station caracteristics
     WRITE(lunout,*)hir(stat_i)%stnr,           &
                    hir(stat_i)%ntim,           &
                    hir(stat_i)%nexp,           &
                    hir(stat_i)%nfclengths, &
                    hir(stat_i)%nparver

     WRITE(lunout,*)hir(stat_i)%lat,hir(stat_i)%lon

     ! Write parameter identifier

     WRITE(lunout,*)obstype(1:nparver)

     ! Write data

     i = INDEX(formdb,'XX')
     form = formdb
     WRITE(form(i:i+1),'(I2.2)')nparver*nexp*nfclengths

     DO i=1,hir(stat_i)%ntim
        WRITE(lunout,form)          &
        hir(stat_i)%o(i)%date,hir(stat_i)%o(i)%time,hir(stat_i)%o(i)%nal
     ENDDO


  CASE DEFAULT
 
     WRITE(6,*)'No such flag in write_database ',flag
     CALL abort

  END SELECT

  CLOSE(lunout)

  WRITE(6,*)'New database file created :',TRIM(dbname)

  RETURN
  
 END SUBROUTINE write_database
!-----------------------------------------------
!-----------------------------------------------
!-----------------------------------------------
 SUBROUTINE read_database(stat_i,flag,ierr)

  IMPLICIT NONE

  ! Input

  INTEGER, INTENT(IN   ) :: stat_i,flag
  INTEGER, INTENT(INOUT) :: ierr

  ! Local
  INTEGER :: i,ii,j,k,l,				&
             obspointer(nparver),		&
             sdate_read,edate_read

  TYPE (station) :: tmp

  CHARACTER(LEN=80) :: dbname ='XXX_00000000'
  CHARACTER(LEN=30) :: form =' '
  CHARACTER(LEN=len_lab ), ALLOCATABLE, DIMENSION(:) :: parid 

!-----------------------------------------------

  SELECT CASE(flag)

  CASE(0)

  ! If the station is active we probably have read it before

  IF (obs(stat_i)%active) RETURN

  ! Check if we really have station data in database

  WRITE(dbname(1:3),'(A3)')'OBS'
  WRITE(dbname(5:12),'(I8.8)')obs(stat_i)%stnr

  OPEN(lunin,file=dbname,STATUS='OLD',IOSTAT=ierr)

  ! Leave gracefully if the station is not in database

  IF(ierr.NE.0) THEN
     WRITE(6,*)'Could not find ',TRIM(dbname)
     obs(stat_i)%active = .FALSE.
     obs(stat_i)%ntim   = 0
     RETURN
  ENDIF

  !
  ! Read station caracteristics
  !

  READ(lunin,*,IOSTAT=ierr)		&
               tmp%stnr,        &
               tmp%ntim,        &
               tmp%nexp,        &
               tmp%nfclengths,  &
               tmp%nparver

  IF(ierr.NE.0) THEN
     WRITE(6,*)'Could not read ',TRIM(dbname), ierr
     WRITE(6,*)'Error reading stnr,ntim,nexp,nfclengths,nparver',	&
                tmp%stnr,tmp%ntim,tmp%nexp,tmp%nfclengths,tmp%nparver
     obs(stat_i)%active = .FALSE.
     obs(stat_i)%ntim   = 0
     CALL abort
  ENDIF

  READ(lunin,*,IOSTAT=ierr)tmp%lat,tmp%lon

  IF(ierr.NE.0) THEN
     WRITE(6,*)'Could not read ',TRIM(dbname),ierr
     WRITE(6,*)'Error reading lat,lon',tmp%lat,tmp%lon
     obs(stat_i)%active = .FALSE.
     obs(stat_i)%ntim   = 0
     CALL abort
  ENDIF

  WRITE(6,*)'Could read ',TRIM(dbname)

  !
  ! Leave gracefully if the station is empty or
  ! station number does not match
  !

  IF(obs(stat_i)%stnr.NE.tmp%stnr) THEN
     WRITE(6,*)'Station number does not agree'
     WRITE(6,*)tmp%stnr,obs(stat_i)%stnr

     obs(stat_i)%active = .FALSE.
     obs(stat_i)%ntim   = 0

     CLOSE(lunin)
     RETURN

  ENDIF
  
  IF (tmp%ntim*tmp%nexp*tmp%nfclengths*tmp%nparver.EQ.0) THEN

     WRITE(6,*)'No data in ',TRIM(dbname)
     WRITE(6,*)'tmp%ntim,tmp%nexp,tmp%nfclengths,tmp%nparver'
     WRITE(6,*) tmp%ntim,tmp%nexp,tmp%nfclengths,tmp%nparver
     obs(stat_i)%active = .FALSE.
     obs(stat_i)%ntim   = 0

     CLOSE(lunin)

     RETURN

  ENDIF

  !
  ! READ and identify parameter identifier
  !

  ALLOCATE(parid(tmp%nparver))

  READ(lunin,*)parid
  obspointer = 0
  DO i=1,tmp%nparver
     WHERE(obstype(1:nparver).EQ.parid(i))
        obspointer =  i 
     ENDWHERE
  ENDDO

  DEALLOCATE(parid)

  !
  ! Allocate and init tmp array
  !

  ALLOCATE(tmp%o(1))

  ALLOCATE(tmp%o(1)%date)
  ALLOCATE(tmp%o(1)%time)
  ALLOCATE(tmp%o(1)%val(tmp%nparver))

  tmp%o(1)%date = nint(err_ind)
  tmp%o(1)%time = nint(err_ind)
  tmp%o(1)%val  = err_ind
 
  WRITE(6,*)'READING',stat_i,obs(stat_i)%stnr
  !
  ! Read data
  !

  ! If we have already got the model we should only store 
  ! the eqv. period

  IF ( hir(stat_i)%obs_is_allocated) THEN
     sdate_read = hir(stat_i)%o(1)%date
     edate_read = hir(stat_i)%o(hir(stat_i)%ntim)%date
     WRITE(6,*)'Changed period (locally):',sdate_read,edate_read
  ELSE
     sdate_read = sdate
     edate_read = edate
  ENDIF

  i = INDEX(formdb,'XX')
  form = formdb
  WRITE(form(i:i+1),'(I2.2)')tmp%nparver

  ii = 0
  DO i=1,tmp%ntim
     READ(lunin,form,IOSTAT=ierr)          &
     tmp%o(1)%date,tmp%o(1)%time,tmp%o(1)%val

     IF (ierr.NE.0) THEN
        WRITE(6,*)'Error reading '
        WRITE(6,*) tmp%o(1)%date,tmp%o(1)%time,tmp%o(1)%val
        CALL ABORT
     ENDIF

     IF(tmp%o(1)%date.LT.sdate_read) CYCLE
     IF(tmp%o(1)%date.GT.edate_read) EXIT

     ii = ii + 1
     
     ALLOCATE(obs(stat_i)%o(ii)%date)
     ALLOCATE(obs(stat_i)%o(ii)%time)
     ALLOCATE(obs(stat_i)%o(ii)%val(nparver))

     obs(stat_i)%o(ii)%date = tmp%o(1)%date
     obs(stat_i)%o(ii)%time = tmp%o(1)%time
     obs(stat_i)%o(ii)%val = err_ind

     DO j=1,nparver

        IF(obspointer(j).GT.0) obs(stat_i)%o(ii)%val(j) = tmp%o(1)%val(obspointer(j)) 

     ENDDO

  ENDDO

  CLOSE(lunout)
  
  obs(stat_i)%ntim = ii

  IF (obs(stat_i)%ntim.GT.0) THEN
     write(6,*) 
     write(6,*) 'STATION ',obs(stat_i)%stnr
     write(6,*) 'FOUND TIMES OBS',obs(stat_i)%ntim,maxtim
     write(6,*) 'OBS PERIOD',obs(stat_i)%o(1)%date,obs(stat_i)%o(obs(stat_i)%ntim)%date
     obs(stat_i)%active = .TRUE.
  ELSE
     write(6,*) 'STATION ',obs(stat_i)%stnr
     write(6,*) 'FOUND NO DATA',obs(stat_i)%ntim,maxtim
     obs(stat_i)%active = .FALSE.
  ENDIF

  ierr = 0

  !
  ! End of observation part
  !

  CASE(1)

  ! If the station is active we probably have read it before

  IF (hir(stat_i)%active) RETURN

  ! Check if we really have station data in database

  WRITE(dbname(1:3),'(A3)')'MOD'
  WRITE(dbname(5:12),'(I8.8)')hir(stat_i)%stnr

  OPEN(lunin,file=dbname,STATUS='OLD',IOSTAT=ierr)

  ! Leave gracefully if the station is not in database

  IF(ierr.NE.0) THEN
     WRITE(6,*)'Could not find ',TRIM(dbname)
     hir(stat_i)%active = .FALSE.
     hir(stat_i)%ntim   = 0
     RETURN
  ENDIF

  !
  ! Read station caracteristics
  !

  READ(lunin,*,IOSTAT=ierr)		&
               tmp%stnr,        &
               tmp%ntim,        &
               tmp%nexp,        &
               tmp%nfclengths,  &
               tmp%nparver

  IF (lprint_read) WRITE(6,*)	&
               tmp%stnr,        &
               tmp%ntim,        &
               tmp%nexp,        &
               tmp%nfclengths,  &
               tmp%nparver

  IF(ierr.NE.0) THEN
     WRITE(6,*)'Could not read ',TRIM(dbname), ierr
     WRITE(6,*)'Error reading stnr,ntim,nexp,nfclengths,nparver',	&
                tmp%stnr,tmp%ntim,tmp%nexp,tmp%nfclengths,tmp%nparver
     hir(stat_i)%active = .FALSE.
     hir(stat_i)%ntim   = 0
     CALL abort
  ENDIF

  READ(lunin,*,IOSTAT=ierr)tmp%lat,tmp%lon
  IF (lprint_read) WRITE(6,*)tmp%lat,tmp%lon

  IF(ierr.NE.0) THEN
     WRITE(6,*)'Could not read ',TRIM(dbname),ierr
     WRITE(6,*)'Error reading lat,lon',tmp%lat,tmp%lon
     hir(stat_i)%active = .FALSE.
     hir(stat_i)%ntim   = 0
     CALL abort
  ENDIF

  WRITE(6,*)'Could read ',TRIM(dbname)

  !
  ! Leave gracefully if the station is empty or
  ! station number does not match
  !

  IF(hir(stat_i)%stnr.NE.tmp%stnr) THEN
     WRITE(6,*)'Station number does not agree'
     WRITE(6,*)tmp%stnr,hir(stat_i)%stnr

     hir(stat_i)%active = .FALSE.
     hir(stat_i)%ntim   = 0

     CLOSE(lunin)
     RETURN

  ENDIF
  
  IF (tmp%ntim*tmp%nexp*tmp%nfclengths*tmp%nparver.EQ.0.OR.		&
      tmp%nexp*tmp%nfclengths.NE.nexp*nfclengths) THEN

     WRITE(6,*)'No data in',TRIM(dbname)
     WRITE(6,*)'tmp%ntim,tmp%nexp,tmp%nfclengths,tmp%nparver'
     WRITE(6,*) tmp%ntim,tmp%nexp,tmp%nfclengths,tmp%nparver
     WRITE(6,*)'    ntim,    nexp,    nfclengths,    nparver'
     WRITE(6,*) hir(stat_i)%ntim,    nexp,     nfclengths,    nparver

     hir(stat_i)%active = .FALSE.
     hir(stat_i)%ntim   = 0

     CLOSE(lunin)

     RETURN

  ENDIF

  !
  ! READ and identify parameter identifier
  !

  ALLOCATE(parid(tmp%nparver))

  READ(lunin,*)parid
  obspointer = 0
  DO i=1,tmp%nparver
     WHERE(obstype(1:nparver).EQ.parid(i))
        obspointer =  i 
     ENDWHERE
  ENDDO

  IF(lprint_read) WRITE(6,*)'OBSPOINTER',obspointer,parid

  DEALLOCATE(parid)

  !
  ! Allocate and init tmp array
  !

  ALLOCATE(tmp%o(1))
  ALLOCATE(tmp%o(1)%date)
  ALLOCATE(tmp%o(1)%time)
  ALLOCATE(tmp%o(1)%nal(tmp%nexp,tmp%nfclengths,tmp%nparver))

  tmp%o(1)%date = nint(err_ind)
  tmp%o(1)%time = nint(err_ind)
  tmp%o(1)%nal  = err_ind
 
  !
  ! Read data
  !

  ! If we have already got the observations we should only store 
  ! the eqv. period

  IF ( obs(stat_i)%obs_is_allocated ) THEN
     WRITE(6,*)stat_i,obs(stat_i)%stnr,obs(stat_i)%ntim
     sdate_read = obs(stat_i)%o(1)%date
     edate_read = obs(stat_i)%o(obs(stat_i)%ntim)%date
     IF (lprint_read) WRITE(6,*)'Changed period (locally):',sdate_read,edate_read
  ELSE
     sdate_read = sdate
     edate_read = edate
  ENDIF


  IF(lprint_read) WRITE(6,*)'READ DATA',sdate_read,edate_read

  i = INDEX(formdb,'XX')
  form = formdb
  WRITE(form(i:i+1),'(I2.2)')tmp%nexp*tmp%nfclengths*tmp%nparver

  ii = 0
  DO i=1,tmp%ntim
     READ(lunin,form)          &
     tmp%o(1)%date,tmp%o(1)%time,tmp%o(1)%nal

     IF(tmp%o(1)%date.LT.sdate_read) CYCLE
     IF(tmp%o(1)%date.GT.edate_read) EXIT


     ii = ii + 1
     

     ALLOCATE(hir(stat_i)%o(ii)%date)
     ALLOCATE(hir(stat_i)%o(ii)%time)
     ALLOCATE(hir(stat_i)%o(ii)%nal(nexp,nfclengths,nparver))

     hir(stat_i)%o(ii)%date = tmp%o(1)%date
     hir(stat_i)%o(ii)%time = tmp%o(1)%time
     hir(stat_i)%o(ii)%nal = err_ind

     DO l=1,nparver
     DO k=1,nfclengths
     DO j=1,nexp

        IF(obspointer(l).GT.0) hir(stat_i)%o(ii)%nal(j,k,l) = tmp%o(1)%nal(j,k,obspointer(l)) 

     ENDDO
     ENDDO
     ENDDO

  ENDDO

  CLOSE(lunout)
  
  hir(stat_i)%ntim = ii

  write(6,*) 
  write(6,*) 'STATION ',hir(stat_i)%stnr
  write(6,*) 'FOUND TIMES MOD',hir(stat_i)%ntim,maxtim
  write(6,*) 'MOD PERIOD',hir(stat_i)%o(1)%date,hir(stat_i)%o(hir(stat_i)%ntim)%date
  hir(stat_i)%active = .TRUE.

  ierr = 0

  END SELECT

  DEALLOCATE(tmp%o)

  RETURN

 END SUBROUTINE read_database
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

 ! Estimate maxtim if not given by user
 IF (maxtim == 0) maxtim=get_maxtim(sdate,edate,1)

 !
 ! If model array is not allocated 
 ! do so and init arrays
 !

 DO k = 1,maxstn
    ALLOCATE(hir(k)%o(maxtim))
 ENDDO
   
 hir%ntim       = 0
 hir%stnr       = 0
 hir%nexp       = nexp
 hir%nparver    = nparver
 hir%nfclengths = nfclengths
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
 IF (maxtim == 0) maxtim=get_maxtim(sdate,edate,1)

 !
 ! If model array is not allocated 
 ! do so and init arrays
 !

 si = sdate * 100 + stime
 ei = edate * 100 + etime

 DO k = 1,maxstn
    ALLOCATE(obs(k)%o(maxtim))
    IF ( use_pos ) THEN
       ALLOCATE(obs(k)%pos(si:ei))
       obs(k)%pos = 0
    ENDIF
 ENDDO
   
 obs%ntim       = 0
 obs%nexp       = nexp
 obs%stnr       = 0
 obs%lat        = 0.0
 obs%lon        = 0.0
 obs%hgt        = 0.0
 obs%nparver    = nparver
 obs%nfclengths = nfclengths
 obs%active     = .FALSE.

 obs%obs_is_allocated = .TRUE.

 RETURN

END SUBROUTINE allocate_obs
END MODULE data
