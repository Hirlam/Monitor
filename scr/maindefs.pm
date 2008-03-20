
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
    'MAXSTN'           => 4000,
    'STNLIST'          => 0,
    'LVERIFY'          => 'F',
    'PRINT_READ'       => 1,
    'PERIOD_TYPE'      => $ENV{PERIOD_TYPE},
    'OUTPUT_TYPE'      => $ENV{OUTPUT_TYPE},
    'OUTPUT_MODE'      => 2,
 },
) ;

 # Default options to be preset in all namelists

%def=(
  'def' => {
     'LVERIFY'=>'T',
     'LPLOT_VERT'=>'F',
     'LPLOT_FREQ'=>'F',
     'LPLOT_SCAT'=>'F',
     'LTIMESERIE_STAT'=>'F',
     'LSTAT_GEN'=> 'F',
     'LPLOT_STAT'=> 'F',
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


%arealoop=(
 # Map and fclen plots
 'map_ver' => {
 'PLOT_BIAS_MAP' =>'T',
 'LSTAT_GEN'  => 'T',
 'LPLOT_STAT' => 'T',
 'SHOW_BIAS' => 'T',
 'SHOW_RMSE' => 'T',
 },

 'TIME' => {
  # Timeseries
  'LTIMESERIE_STAT'=> 'T',
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
 },

 'VERT' => {
  # Vertical profile
  'LPLOT_VERT' => 'T',
  'LSTAT_GEN'  => 'T',
  'LFCVER'     => 'F',
 },

);

