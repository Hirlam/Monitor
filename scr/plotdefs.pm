
%plotdefs=(

 'def'=>{
   'TWIND_SURF' => 06,
   'TWIND_TEMP' => 12,
   'QC_LIM_SCALE' => 5.,
   'MAP_BIAS_INTERVAL'=> '7*-1',
 },

 'VI'=>{
   'TEXT'        => 'Visibility',
   'CONT_CLASS'  => 4,
   'CONT_LIM'    => '1000.,5000.,10000.,20000.',
   'PRE_FCLA'    => '1000.,5000.,10000.,20000.',
   'MAP_BIAS_INTERVAL'=> '-20000.,-10000.,-1000.,0.,1000.,10000.,20000.',
 },

 'NN'=>{
   'TEXT'        => 'Cloud cover',
   'CONT_CLASS'  => 8,
   'PRE_FCLA'    => '0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5',
   'CONT_LIM'    => '0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5',
   'MAP_BIAS_INTERVAL'=> '-6.,-4.,-2.,0.,2.,4.,6.',
 },

 'PS'=>{
   'TEXT'        => 'Mslp',
   'MAP_BIAS_INTERVAL'=> '-1.5,-1.0,-0.5,0.,0.5,1.0,1.5',
 },
 'SPS'=>{
   'TEXT'              => 'Station pressure',
   'MAP_BIAS_INTERVAL' => '-1.5,-1.0,-0.5,0.,0.5,1.0,1.5',
   'UNIT'              => 'hPa',
 },

 'TTHA'=>{
   'TEXT'        => 'T2m, height corr.',
   'TEXT_TEMP'   => 'Temperature' ,
   'MAP_BIAS_INTERVAL'=> '-8.,-2.,-0.5,0.,0.5,2.,8.',
   'CONT_CLASS'  => 12,
   'CONT_LIM'    => '-30.,-20.,-15.,-10.,-5.,0.,5.,10.,15.,20.,25.,30.',
   'PRE_FCLA'    => '-30.,-20.,-15.,-10.,-5.,0.,5.,10.,15.,20.,25.,30.',
   'UNIT'       => 'deg C',
 },

 'TT'=>{
   'TEXT'        => 'T2m',
   'TEXT_TEMP'   => 'Temperature' ,
   'MAP_BIAS_INTERVAL'=> '-8.,-2.,-0.5,0.,0.5,2.,8.',
   'CONT_CLASS'  => 12,
   'CONT_LIM'    => '-30.,-20.,-15.,-10.,-5.,0.,5.,10.,15.,20.,25.,30.',
   'PRE_FCLA'    => '-30.,-20.,-15.,-10.,-5.,0.,5.,10.,15.,20.,25.,30.',
 },

 'TN'=>{
   'TWIND_SURF'  => 12,
   'TEXT'        => 'Min T2m',
   'MAP_BIAS_INTERVAL'=> '-6.,-4.,-2.,0.,2.,4.,6.',
   'ACC'         => 12,
   'ACCTYPE'     => 2,
   'OBSTIME'     => '6,18',
 },
 
 'TX'=>{
   'TWIND_SURF'  => 12,
   'TEXT'        => 'Max T2m',
   'MAP_BIAS_INTERVAL'=> '-6.,-4.,-2.,0.,2.,4.,6.',
   'ACC'         => 12,
   'ACCTYPE'     => 3,
   'OBSTIME'     => '6,18',
 },
 
 'TD'=>{
   'TEXT'        => 'Td2m',
   'TEXT_TEMP'   => 'Dew point T' ,
   'MAP_BIAS_INTERVAL'=> '-6.,-4.,-2.,0.,2.,4.,6.',
 },

 'TDD'=>{
   'TEXT'              => 'Td2m deficit',
   'TEXT_TEMP'         => 'Dew point deficit' ,
   'MAP_BIAS_INTERVAL' => '-6.,-4.,-2.,0.,2.,4.,6.',
   'UNIT'              => 'deg C',
   'LIM'               => '50.',
   'ULIM'              => '400.',
   'LLIM'              => '-200.',
 },

 'FI'=>{
   'TEXT_TEMP'        => 'Geopotential',
 },

 'FF'=>{
   'TEXT'        => 'U10m',
   'TEXT_TEMP'   => 'Wind speed',
   'CONT_CLASS'  => 7,
   'CONT_LIM'=> '1.5,3.3,5.5,8.0,10.8,13.9,24.5',
   'PRE_FCLA'=> '1.5,3.3,5.5,8.0,10.8,13.9,24.5',
   'MAP_BIAS_INTERVAL'=> '-10.,-5.,-2.5,0.,2.5,5.,10.'
 },

 'FX'=>{
   'TEXT'        => 'Max U10m',
   'MAP_BIAS_INTERVAL'=> '-10.,-5.,-2.5,0.,2.5,5.,10.'
 },

 'GG'=>{
   'TEXT'        => 'Wind Gust',
   'MAP_BIAS_INTERVAL'=> '-10.,-5.,-2.5,0.,2.5,5.,10.'
 },

 'GX'=>{
   'TEXT'        => 'Max Wind Gust',
   'MAP_BIAS_INTERVAL'=> '-10.,-5.,-2.5,0.,2.5,5.,10.',
   'ACC'         => 1,
   'ACCTYPE'     => 3,
 },

 'DD'=>{
   'TEXT'        => 'Wind direction',
   'TEXT_TEMP'   => 'Wind direction',
   'PRE_FCLA'    => '10.,30.,50.,70.,90.,110.,130.,150.,170.,190.,210.,230.,250.,270.,290.,310.,330.,350.',
   'MAP_BIAS_INTERVAL'=> '-90.,-60.,-30.,0.,30.,60.,90.',
 },

 'PE1'=>{
   'TEXT'        => '1h Precipitation',
   'CONT_CLASS'  => 9,
   'CONT_LIM'    =>'0.1,0.2,0.3,0.5,1.0,2.0,5.0,10.0,20.0',
   'PRE_FCLA'    =>'0.1,0.2,0.3,0.5,1.0,2.0,5.0,10.0,20.0',
   'TWIND_SURF'  => 00,
   'ACC'         => 1,
   'UNIT'        => 'mm/1h',
   'LLIM'        => '0.',
   'ULIM'        => '500.',
 },

 'PE3'=>{
   'TEXT'        => '3h Precipitation',
   'CONT_CLASS'  => 9,
   'CONT_LIM'    =>'0.1,0.2,0.5,1.0,2.0,5.0,10.0,15.0,20.0',
   'PRE_FCLA'    =>'0.1,0.2,0.5,1.0,2.0,5.0,10.0,15.0,20.0',
   'TWIND_SURF'  => 00,
   'ACC'         => 3,
   'LLIM'        => '0.',
   'UNIT'        => 'mm/3h',
   'LLIM'        => '0.',
   'ULIM'        => '500.',
   'OBSTIME'     => '3,9,15,21',
 },

 'PE6'=>{
   'TEXT'        => '6h Precipitation',
   'CONT_CLASS'  => 9,
   'CONT_LIM'    =>'0.1,0.2,0.5,1.0,2.0,5.0,10.0,20.0,35.0',
   'PRE_FCLA'    =>'0.1,0.2,0.5,1.0,2.0,5.0,10.0,20.0,35.0',
   'TWIND_SURF'  => 00,
   'ACC'         => 6,
   'UNIT'        => 'mm/6h',
   'LLIM'        => '0.',
   'ULIM'        => '500.',
   'OBSTIME'     => '0,12',
 },

 'PE12'=>{
   'TEXT'        => '12h Precipitation',
   'CONT_CLASS'  => 11,
   'CONT_LIM'    =>'0.1,0.2,0.5,1.0,2.0,5.0,10.0,20.0,35.0,50.0,70.0',
   'PRE_FCLA'    =>'0.1,0.2,0.5,1.0,2.0,5.0,10.0,20.0,35.0,50.0,70.0',
   'TWIND_SURF'  => 00,
   'ACC'         => 12,
   'UNIT'        => 'mm/12h',
   'LLIM'        => '0.',
   'ULIM'        => '500.',
 },

 'PE' => $plotdefs{PE12},

 'PE24'=>{
   'TEXT'        => '24h Precipitation',
   'CONT_CLASS'  => 12,
   'CONT_LIM'    =>'0.1,0.2,0.5,1.0,2.0,5.0,10.0,20.0,35.0,50.0,70.0,100.0',
   'PRE_FCLA'    =>'0.1,0.2,0.5,1.0,2.0,5.0,10.0,20.0,35.0,50.0,70.0,100.0',
   'TWIND_SURF'  => 00,
   'ACC'         => 24,
   'UNIT'        => 'mm/24h',
   'LLIM'        => '0.',
   'ULIM'        => '1000.',
   'OBSTIME'     => '6',
 },

 'CH'=>{
   'TEXT'        => 'Cloud base',
   'UNIT'        => 'm',
   'LLIM'        => 0.,
   'CONT_CLASS'  => 8,
   'CONT_LIM'    => '70.,150.,300.,600.,1000.,1500.,2500.,5000.',
   'PRE_FCLA'    => '70.,150.,300.,600.,1000.,1500.,2500.,5000.',
   'MAP_BIAS_INTERVAL'=> '-1000.,-500.,-250.,0.,250.,500.,1000.',
 }, 

 'N75'=>{
   'TEXT'        => 'clouds <7500m',
   'UNIT'        => 'octas',
   'LLIM'        => 0.,
   'CONT_CLASS'  => 7,
   'CONT_LIM'    => '1.,2.,3.,4.,5.,6.,7.',
   'PRE_FCLA'    => '1.,2.,3.,4.,5.,6.,7.',
   'MAP_BIAS_INTERVAL'=> '-4.,-2.,-1.,0.,1.,2.,4.',
 }, 

 'LC'=>{
   'TEXT'        => 'Low clouds',
   'UNIT'        => 'octas',
   'LLIM'        => 0.,
   'CONT_CLASS'  => 7,
   'CONT_LIM'    => '1.,2.,3.,4.,5.,6.,7.',
   'PRE_FCLA'    => '1.,2.,3.,4.,5.,6.,7.',
   'MAP_BIAS_INTERVAL'=> '-4.,-2.,-1.,0.,1.,2.,4.',
 }, 

 'QQ'=>{
   'TEXT'        => 'Q2m',
   'TEXT_TEMP'   => 'Specific humidity' ,
   'MAP_BIAS_INTERVAL'=> '-3.,-2.,-1.,0.,1.,2.,3.',
 },

 'RH'=>{
   'TEXT'        => 'Rh2m',
   'TEXT_TEMP'   => 'Relative humidity' ,
   'CONT_CLASS'  => 7,
   'CONT_LIM'    => '30.,50.,65.,75.,85.,95.,100.',
   'PRE_FCLA'    => '30.,50.,65.,75.,85.,95.,100.',
   'MAP_BIAS_INTERVAL'=> '-25.,-10.,-5.,0.,5.,10.,25.',
 },

 'GR'=>{ 'TEXT'        => 'Global radation',
         'TWIND_SURF'  => 01,
 },
 'WT'=>{ 'TEXT'        => 'Sens heat flux', 
         'TWIND_SURF'  => 01,
 },
 'WQ'=>{ 'TEXT'        => 'Lat heat flux', 
         'TWIND_SURF'  => 01,
 },
 'UW'=>{ 'TEXT'        => 'Momentum flux', 
         'TWIND_SURF'  => 01,
 },
 'TZ'=>{ 'TEXT'        => 'dT/dz', 
         'TWIND_SURF'  => 01,
 },
 'LU'=>{ 'TEXT'        => 'Long wave up', 
         'TWIND_SURF'  => 01,
 },
 'TMAST'=>{
   'TEXT'   => 'Temperature' ,
   'UNIT'         => 'deg C',
 },
 'RHMAST'=>{
   'TEXT'   => 'Relative hum' ,
   'UNIT'         => '%',
 },
 'FFMAST'=>{
   'TEXT'   => 'Wind speed' ,
   'UNIT'         => 'm/s',
 },
   
);

# Insert patch info
 @patchvar = ('TT','TN','TX','DD','FF','RH','QQ','TD');
 @npatch = (1,2);

foreach $patch ( @patchvar ) { 
  foreach $n ( @npatch ) { 
    $lpar = $patch.'P'.$n;
    unless ( exists $plotdefs{$lpar} ) { 
      for $role ( sort keys %{ $plotdefs{$patch} } ) {
          ${plotdefs}{$lpar}{$role}=${plotdefs}{$patch}{$role};
      } ;
      $plotdefs{$lpar}{'TEXT'} = $plotdefs{$lpar}{'TEXT'}.' '.$n.' patch';
    } ;
  } ;
} ;

1 ;

