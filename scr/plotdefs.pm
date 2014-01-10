
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
   'CONT_CLASS'  => 7,
   'CONT_LIM'    => '1.,2.,3.,4.,5.,6.,7.',
   'PRE_FCLA'    => '1.,2.,3.,4.,5.,6.,7.',
   'MAP_BIAS_INTERVAL'=> '-6.,-4.,-2.,0.,2.,4.,6.',
 },

 'PS'=>{
   'TEXT'        => 'Mslp',
   'MAP_BIAS_INTERVAL'=> '-1.5,-1.0,-0.5,0.,0.5,1.0,1.5',
 },

 'TTHA'=>{
   'TEXT'        => 'T2m, height adjusted',
   'TEXT_TEMP'   => 'Temperature' ,
   'MAP_BIAS_INTERVAL'=> '-6.,-4.,-2.,0.,2.,4.,6.',
   'UNIT',       => 'deg C',
 },

 'TT'=>{
   'TEXT'        => 'T2m',
   'TEXT_TEMP'   => 'Temperature' ,
   'MAP_BIAS_INTERVAL'=> '-6.,-4.,-2.,0.,2.,4.,6.',
 },
 
 'TN'=>{
   'TWIND_SURF'  => 12,
   'TEXT'        => 'Min T2m',
   'MAP_BIAS_INTERVAL'=> '-6.,-4.,-2.,0.,2.,4.,6.',
   'ACC'         => 12,
   'ACCTYPE'     => 2,
 },
 
 'TX'=>{
   'TWIND_SURF'  => 12,
   'TEXT'        => 'Max T2m',
   'MAP_BIAS_INTERVAL'=> '-6.,-4.,-2.,0.,2.,4.,6.',
   'ACC'         => 12,
   'ACCTYPE'     => 3,
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
   'ACC'         => 6,
   'ACCTYPE'     => 3,
 },

 'DD'=>{
   'TEXT'        => 'Wind direction',
   'TEXT_TEMP'   => 'Wind direction',
   'PRE_FCLA'    => '10.,30.,50.,70.,90.,110.,130.,150.,170.,190.,210.,230.,250.,270.,290.,310.,330.,350.',
   'MAP_BIAS_INTERVAL'=> '-90.,-60.,-30.,0.,30.,60.,90.',
 },

 'PE'=>{
   'TEXT'        => '12h Precipitation',
   'CONT_CLASS'  => 7,
   'CONT_LIM'    =>'0.1,0.3,1.0,3.0,10.0,30.0,100.0',
   'PRE_FCLA'    =>'0.1,0.3,1.0,3.0,10.0,30.0,100.0',
   'MAP_BIAS_INTERVAL'=> '-20.,-10.,-5.,0.,5.,10.,15.',
   'TWIND_SURF'       => 00,
   'ACC'         => 12,
   'UNIT'         => 'mm/12h',
   'LLIM'         => '0.',
   'ULIM'         => '500.',
 },

 'PE1'=>{
   'TEXT'        => '1h Precipitation',
   'CONT_CLASS'  => 7,
   'CONT_LIM'    =>'0.1,0.3,1.0,3.0,10.0,30.0,100.0',
   'PRE_FCLA'    =>'0.1,0.3,1.0,3.0,10.0,30.0,100.0',
   'MAP_BIAS_INTERVAL'=> '-20.,-10.,-5.,0.,5.,10.,15.',
   'TWIND_SURF'       => 00,
   'ACC'         => 1,
   'UNIT'         => 'mm/1h',
   'LLIM'         => '0.',
   'ULIM'         => '500.',
 },

 'PE3'=>{
   'TEXT'        => '3h Precipitation',
   'CONT_CLASS'  => 7,
   'CONT_LIM'    =>'0.1,0.3,1.0,3.0,10.0,30.0,100.0',
   'PRE_FCLA'    =>'0.1,0.3,1.0,3.0,10.0,30.0,100.0',
   'MAP_BIAS_INTERVAL'=> '-20.,-10.,-5.,0.,5.,10.,15.',
   'TWIND_SURF'       => 00,
   'ACC'         => 3,
   'UNIT'         => 'mm/3h',
   'LLIM'         => '0.',
   'ULIM'         => '500.',
 },

 'PE6'=>{
   'TEXT'        => '6h Precipitation',
   'CONT_CLASS'  => 7,
   'CONT_LIM'    =>'0.1,0.3,1.0,3.0,10.0,30.0,100.0',
   'PRE_FCLA'    =>'0.1,0.3,1.0,3.0,10.0,30.0,100.0',
   'MAP_BIAS_INTERVAL'=> '-20.,-10.,-5.,0.,5.,10.,15.',
   'TWIND_SURF'       => 00,
   'ACC'         => 6,
   'UNIT'         => 'mm/6h',
   'LLIM'         => '0.',
   'ULIM'         => '500.',
 },

 'PE12'=>{
   'TEXT'        => '12h Precipitation',
   'CONT_CLASS'  => 7,
   'CONT_LIM'    =>'0.1,0.3,1.0,3.0,10.0,30.0,100.0',
   'PRE_FCLA'    =>'0.1,0.3,1.0,3.0,10.0,30.0,100.0',
   'MAP_BIAS_INTERVAL'=> '-20.,-10.,-5.,0.,5.,10.,15.',
   'TWIND_SURF'       => 00,
   'ACC'         => 12,
   'UNIT'         => 'mm/12h',
   'LLIM'         => '0.',
   'ULIM'         => '500.',
 },

 'PE24'=>{
   'TEXT'        => '24h Precipitation',
   'CONT_CLASS'  => 7,
   'CONT_LIM'    =>'0.1,0.3,1.0,3.0,10.0,30.0,100.0',
   'PRE_FCLA'    =>'0.1,0.3,1.0,3.0,10.0,30.0,100.0',
   'MAP_BIAS_INTERVAL'=> '-20.,-10.,-5.,0.,5.,10.,15.',
   'TWIND_SURF'       => 00,
   'ACC'         => 24,
   'UNIT'         => 'mm/24h',
   'LLIM'         => '0.',
   'ULIM'         => '1000.',
 },

 'CH'=>{
   'TEXT'        => 'Cloud base',
   'UNIT'        => 'm',
   'LLIM'        => 0.,
 }, 

 'LC'=>{
   'TEXT'        => 'Low clouds',
   'UNIT'        => 'octas',
   'LLIM'        => 0.,
 }, 

 'QQ'=>{
   'TEXT'        => 'Q2m',
   'TEXT_TEMP'   => 'Specific humidity' ,
   'MAP_BIAS_INTERVAL'=> '-3.,-2.,-1.,0.,1.,2.,3.',
 },

 'RH'=>{
   'TEXT'        => 'Rh2m',
   'TEXT_TEMP'   => 'Relative humidity' ,
   'MAP_BIAS_INTERVAL'=> '-15.,-10.,-5.,0.,5.,10.,15.',
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
