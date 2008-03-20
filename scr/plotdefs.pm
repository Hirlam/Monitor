
%plotdefs=(

 'def'=>{
   'TWIND_SURF' => 06,
   'TWIND_TEMP' => 12,
   'QC_LIM_SCALE' => 5.,
   'TEXT' => 'Unknown',
   'TEXT_TEMP' => 'Unknown',
   'MAP_BIAS_INTERVAL'=> '7*-1',
 },

 'VI'=>{
   'TEXT'        => 'Visibility',
   'CONT_CLASS'  => 4,
   'CONT_LIM'    => '1000.,5000.,10000.,20000.',
   'MAP_BIAS_INTERVAL'=> '-20000.,-10000.,-1000.,0.,1000.,10000.,20000.',
 },

 'NN'=>{
   'TEXT'        => 'Cloud cover',
   'CONT_CLASS'  => 7,
   'CONT_LIM'    => '1.,2.,3.,4.,5.,6.,7.',
   'MAP_BIAS_INTERVAL'=> '-6.,-4.,-2.,0.,2.,4.,6.',
 },

 'PS'=>{
   'TEXT'        => 'Mslp',
   'MAP_BIAS_INTERVAL'=> '-1.5,-1.0,-0.5,0.,0.5,1.0,1.5',
 },

 'TT'=>{
   'TEXT'        => 'T2m',
   'TEXT_TEMP'        => 'Temperature' ,
   'MAP_BIAS_INTERVAL'=> '-6.,-4.,-2.,0.,2.,4.,6.',
 },
 
 'TD'=>{
   'TEXT'        => 'Td2m',
   'TEXT_TEMP'        => 'Dew point T' ,
   'MAP_BIAS_INTERVAL'=> '-6.,-4.,-2.,0.,2.,4.,6.',
 },

 'FI'=>{
   'TEXT_TEMP'        => 'Geopotential',
 },

 'FF'=>{
   'TEXT'        => 'U10m',
   'TEXT_TEMP'   => 'Wind speed',
   'CONT_CLASS'  => 7,
   'CONT_LIM'=> '1.5,3.3,5.5,8.0,10.8,13.9,24.5',
   'MAP_BIAS_INTERVAL'=> '-10.,-5.,-2.5,0.,2.5,5.,10.'
 },

 'DD'=>{
   'TEXT'        => 'Wind direction',
   'TEXT_TEMP'        => 'Wind direction',
   'MAP_BIAS_INTERVAL'=> '-90.,-60.,-30.,0.,30.,60.,90.',
 },

 'PE'=>{
   'TEXT'        => 'Precipitation',
   'CONT_CLASS'  => 7,
   'CONT_LIM'    =>'0.1,0.3,1.0,3.0,10.0,30.0,100.0',
   'MAP_BIAS_INTERVAL'=> '-20.,-10.,-5.,0.,5.,10.,15.',
   'TWIND_SURF'       => 00,
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

 'GR'=>{ 'TEXT'        => 'Global rad',
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
   
);
