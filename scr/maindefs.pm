sub Env_or {
    my($var,$default) = @_;
    if (exists $ENV{$var}) {
        return $ENV{$var} . ',';
    } else {
        $default = '0' unless $default;
        return "$default" . ',';
    }
}


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
    'MAXSTN'           => 20000,
    'STNLIST'          => 0,
    'STNLIST_PLOT'     => $ENV{STNLIST_PLOT},
    'LVERIFY'          => 'F',
    'PRINT_READ'       => 1,
    'PERIOD_TYPE'      => $ENV{PERIOD_TYPE},
    'PERIOD_FREQ'      => $ENV{PERIOD_FREQ},
    'OUTPUT_TYPE'      => $ENV{OUTPUT_TYPE},
    'OUTPUT_MODE'      => 2,
    'USE_ANALYSIS'     => ("$ENV{USE_ANALYSIS}" or '20*F'),

 },
) ;

 # Default options to be preset in all namelists

%def=(
  'def' => {
     'LVERIFY'=>'T',
     'LSIGN_TEST'=>'F',
     'LSIGN_TEST_JOINT'=>'F',
     'LPLOT_VERT'=>'F',
     'LPLOT_FREQ'=>'F',
     'LPLOT_SCAT'=>'F',
     'LTIMESERIE_STAT'=>'F',
     'LSTAT_GEN'=> 'F',
     'LPLOT_STAT'=> 'F',
     'LPLOT_SEASONAL'=> 'F',
     'LPREP_XML'=> 'F',
     'PLOT_BIAS_MAP' =>'F',
     'PLOT_RMSE_MAP' =>'F',
     'PLOT_STDV_MAP' =>'F',
     'PLOT_MABE_MAP' =>'F',
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
 'LSIGN_TEST_JOINT' =>  &Env_or('SIGN_TEST_JOINT','F'),
 'CONFINT'    => '90.',
 'SIGN_TIME_DIFF' => '-1',
 'LPLOT_STAT' => 'T',
 'SHOW_BIAS' => 'T',
 'SHOW_MABE' => 'F',
 'SHOW_RMSE' => 'F',
 'SHOW_STDV' => 'T',
 'SHOW_VAR'  => 'T',
 },

 'MAP' => {
 # Map plots
 'PLOT_MABE_MAP' => 'F',
 'PLOT_BIAS_MAP' => 'T',
 'PLOT_RMSE_MAP' => 'T',
 'LSTAT_GEN'     => 'T',
 'LPLOT_STAT'    => 'F',
 'LFCVER'        => 'F',
 #'SHOW_TIMES'    => '00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23',
 'SHOW_TIMES'    => '00,06,12,18',
 'USE_FCLEN' => join(',',split(' ',$ENV{FCLEN_MAP})),
 },

 'TIME' => {
  # Timeseries
  'LTIMESERIE_STAT'=> 'T',
  'SHOW_MABE' => 'F',
  'SHOW_BIAS' => 'T',
  'SHOW_STDV' => 'T',
  'SHOW_RMSE' => 'F',
  'USE_FCLEN' => join(',',split(' ',$ENV{FCLEN_TIME})),
 },

 'scat_ver' => {
  # Scatterplots and freq,
  'LPREP_XML' => 'T',
  'LPLOT_FREQ'=> 'T',
  'LPLOT_SCAT'=> 'T',
  'LSCAT_YAVE'=> 'F',
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

@default_plot_prefix = (

 'V',       # 1  Lead time
 'v',       # 2  Daily cycle
 'ps',      # 3  Timeserie
 'PS',      # 4  Timeserie diff
 'L',       # 5  Profile by lead time
 'l',       # 6  Profile by valid hour
 'm',       # 7  Maps by valid hour
 'M',       # 8  Maps by lead time
 'f',       # 9  Frequency
 's',       # 10 Scatter
 'x',       # 11 Scatter diff
 'Y',       # 12 Seasonal
 'sign',    # 13 Significance
 'jsign'    # 14 Significance joint plots

);
@custom_plot_prefix = (

 'a',       # 1  Lead time
 'b',       # 2  Daily cycle
 'c',       # 3  Timeserie
 'd',       # 4  Timeserie diff
 'e',       # 5  Profile by lead time
 'f',       # 6  Profile by valid hour
 'g',       # 7  Maps by valid hour
 'h',       # 8  Maps by lead time
 'i',       # 9  Frequency
 'j',       # 10 Scatter
 'k',       # 11 Scatter diff
 'l',       # 12 Seasonal
 'm',       # 13 Significance
 'n'        # 14 Significance joint plots

);

if ( $ENV{CUSTOM_PLOT_PREFIX} ) {
  @plot_prefix = @custom_plot_prefix ;
 } else {
  @plot_prefif = @default_plot_prefix ;
};

;
