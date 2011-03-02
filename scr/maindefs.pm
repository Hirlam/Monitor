
%nameread=(
 'read_section' => {
    'SDATE'   => $ENV{SDATE},
    'EDATE'   => $ENV{EDATE},
    'NEXP'    => $nexp,
    'EXPNAME' => $exp,
    'MODPATH' => $modpath,
    'OBSPATH' => '\''.$ENV{OBSPATH}.'\'',
    'LQUALITY_CONTROL' => 'T',
    'ESTIMATE_QC_LIMIT'=> 'T',
    'MAXSTN'           => 5000,
    'STNLIST'          => 0,
    'STNLIST_PLOT'     => $ENV{STNLIST_PLOT},
    'LVERIFY'          => 'F',
    'PRINT_READ'       => 1,
    'PERIOD_TYPE'      => $ENV{PERIOD_TYPE},
    'OUTPUT_TYPE'      => $ENV{OUTPUT_TYPE},
    'OUTPUT_MODE'      => 2,
    'GRAPHICS'         => '\''.$ENV{GRAPHICS}.'\'',
 },
) ;

 # Default options to be preset in all namelists

%def=(
  'def' => {
     'LVERIFY'=>'T',
     'LSIGN_TEST'=>'F',
     'LPLOT_VERT'=>'F',
     'LPLOT_FREQ'=>'F',
     'LPLOT_SCAT'=>'F',
     'LTIMESERIE_STAT'=>'F',
     'LSTAT_GEN'=> 'F',
     'LPLOT_STAT'=> 'F',
     'LPLOT_SEASONAL'=> 'F',
     'LPREP_XML'=> 'F',
     'PLOT_BIAS_MAP' =>'F',
     'LFCVER'        =>'T',
     'LDIFF'         => 'T',
     'SHOW_OBS'      => 'F',
     'STNLIST'       => 0,
     'LPOLY'         => 'F',
     'CBOX%ACTIVE'   => 'F',
  },
) ;


%selectionloop=(
 'SEAS' => {
 # Seasonal plots
 'LSTAT_GEN'  => 'T',
 'SHOW_BIAS'  => 'F',
 'SHOW_STDV'  => 'F',
 'SHOW_RMSE'  => 'F',
 'SHOW_OBS'   => 'T',
 'LPLOT_SEASONAL'=> 'T',
 },
 'GEN' => {
 # Fclen plots
 'LSTAT_GEN'  => 'T',
 'LSIGN_TEST' => 'T',
 'SIGN_TIME_DIFF' => '-1',
 'LPLOT_STAT' => 'T',
 'SHOW_BIAS' => 'T',
 'SHOW_RMSE' => 'T',
 'SHOW_VAR'  => 'T',
 },

 'MAP' => {
 # Map plots
 'PLOT_BIAS_MAP' => 'T',
 'LSTAT_GEN'     => 'T',
 'LPLOT_STAT'    => 'F',
 'LFCVER'        => 'F',
 'SHOW_TIMES'    => '00,12',
 },

 'TIME' => {
  # Timeseries
  'LTIMESERIE_STAT'=> 'T',
  'USE_FCLEN' => join(',',split(' ',$ENV{FCLEN_TIME})),
 },

 'scat_ver' => {
  # Scatterplots and freq,
  'LPREP_XML' => 'T',
  'LPLOT_FREQ'=> 'T',
  'LPLOT_SCAT'=> 'T',
  'USE_FCLEN' => join(',',split(' ',$ENV{FCLEN_SCAT})),
 },

 'DAYVAR' => {
  # Daily variation
  'LPLOT_STAT' => 'T',
  'LSTAT_GEN'  => 'T',
  'LFCVER'     => 'F',
  'USE_FCLEN'  => join(',',split(' ',$ENV{FCLEN_DAYVAR})),
  'SHOW_OBS'   => 'T',
  'SHOW_VAR'   => 'F',
 },

 'VERT' => {
  # Vertical profile
  'LPLOT_VERT' => 'T',
  'LSTAT_GEN'  => 'T',
  'LFCVER'     => 'F',
  'USE_FCLEN'  => join(',',split(' ',$ENV{FCLEN_TEMP_VERT})),
  'SHOW_TIMES'    => '00,12',
 },

);

