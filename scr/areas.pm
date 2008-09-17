 %areas=(
  'ALL' => {
   'STNLIST'=> 0,
   'MAP_SCALE'=>5.0e7,
   'MAP_CENTRE_LATITUDE'=>55.,
   'MAP_CENTRE_LONGITUDE'=>25.,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'Qingdao' => {
   'CBOX%ACTIVE' => 'T',
   'CBOX%SLAT' => '30.',
   'CBOX%WLON' => '110.',
   'CBOX%NLAT' => '45.',
   'CBOX%ELON' => '130.',
   'MAP_SCALE'=>5.0e5,
   'MAP_PROJECTION'=> '\'MERCATOR\'',
   'MAP_AREA_DEFINITION'=>'\'CORNER\'',
   'MAP_LOWER_LEFT_LATITUDE'=>30,
   'MAP_LOWER_LEFT_LONGITUDE'=>110,
   'MAP_UPPER_RIGHT_LATITUDE'=>40,
   'MAP_UPPER_RIGHT_LONGITUDE'=>130,
   },
  'EWGLAM' => {
   'STNLIST'=> '01001,01025,01049,01098,01102,01152,01205,
       01271,01317,01384,01415,01452,01494,
       02020,02096,02120,02186,02222,02226,02286,
       02316,02366,
       02376,02418,02444,02464,02512,02562,02590,
       02640,02807,02836,02864,02897,02911,02917,
       02929,02935,02944,02952,02958,02970,02972,
       02974,
       03005,03026,03066,03075,03091,03100,03140,
       03162,03171,03240,03257,03302,03222,03360,
       03496,03502,03534,03566,03715,03772,03797,
       03808,03862,03895,03917,03953,03957,03962,
       03969,03970,03976,03980,
       06011,06030,06052,06110,06180,06235,06260,
       06290,06310,06400,06447,06476,06479,06610,
       06720,
       07005,07015,07027,07037,07070,07110,07130,
       07157,07168,07180,07190,07222,07240,07255,
       07280,07292,07315,07434,07460,07481,07510,
       07535,07577,07602,07630,07645,07650,07690,
       07747,07761,07790,
       08001,08015,08023,08027,08045,08084,08160,
       08181,08202,08261,08280,08284,08306,08314,
       08348,08360,08373,08391,08482,08487,08495,
       08538,08549,08562,08571,08575,08579,
       10035,10063,10091,10113,10131,10147,10162,
       10224,10280,10315,10338,10361,10384,10400,
       10439,10469,10488,10490,10510,10548,10637,
       10655,10685,10708,10729,10738,10763,10776,
       10803,10870,10893,
       11010,11035,11120,11150,11240,11448,11518,
       11541,11659,11723,11782,11858,11903,11934,
       11968,
       12100,12115,12135,12195,12205,12250,12295,
       12300,12330,12375,12400,12424,12465,12495,
       12500,12520,12550,12566,12570,12580,12772,
       12812,12843,12860,12882,12925,12935,12960,
       12982,
       15010,15020,15090,15120,15197,15200,15230,
       15235,15310,15346,15360,15410,15420,14450,
       15470,15480,15511,15526,15552,15614,15625,
       15640,15655,15712,
       16020,16045,16059,16080,16090,16105,16120,
       16149,16170,16181,16230,16289,16300,16320,
       16360,16405,16420,16453,16470,16490,16506,
       16520,16560,16597,16622,16627,16641,16648,
       16650,16667,16682,16684,16716,16723,16732,
       16743,16749,16754,
       17022,17050,17060,17112,17116,17124,17150,
       17184,17188,17240,17292,17300,
       22106,22408,22602,22802,22820,22892,26038,
       26063,26215,26258,26406,26422,26477,26524,
       26544,26629,26702,26730,26850,26863,
       33008,33036,33041,33088,33177,33317,33345,
       33393,33526,33562,33587,33631,33658,33663,
       33777,33815,33837',
   'MAP_SCALE'=>3.2e7,
   'MAP_CENTRE_LATITUDE'=>50.,
   'MAP_VERTICAL_LONGITUDE'=>15.,
   'MAP_CENTRE_LONGITUDE'=>27.,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'Greenland' => {
   'STNLIST'=> '4208,4214,4220,4242,4250,4253,4260,4266,4272,4285,4320,4351,4373',
   'MAP_SCALE'=>2.0e7,
   'MAP_CENTRE_LATITUDE'=>70.,
   'MAP_CENTRE_LONGITUDE'=>-35.,
   'MAP_VERTICAL_LONGITUDE'=>-40.,
   'MAP_PROJECTION '=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'South_of_45' => {
   'CBOX%ACTIVE' => 'T',
   'CBOX%SLAT' => '10.',
   'CBOX%WLON' => '-180.',
   'CBOX%NLAT' => '45.',
   'CBOX%ELON' => '180.',
   'MAP_SCALE'=>3.5e7,
   'MAP_CENTRE_LATITUDE'=>32.,
   'MAP_CENTRE_LONGITUDE'=>18.,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'North_of_60' => {
   'CBOX%ACTIVE' => 'T',
   'CBOX%SLAT' => '60.',
   'CBOX%WLON' => '-180.',
   'CBOX%NLAT' => '90.',
   'CBOX%ELON' => '180.',
   'MAP_SCALE'=>3.0e7,
   'MAP_CENTRE_LATITUDE'=>75.,
   'MAP_CENTRE_LONGITUDE'=>20.,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'Ireland_England' => {
   'CBOX%ACTIVE' => 'T',
   'CBOX%SLAT' => '50.',
   'CBOX%WLON' => '-11.',
   'CBOX%NLAT' => '60.',
   'CBOX%ELON' => '2.',
   'MAP_SCALE'=>7.5e6,
   'MAP_CENTRE_LATITUDE'=>55.,
   'MAP_CENTRE_LONGITUDE'=>0.,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'Northern_North_Sea' => {
   'CBOX%ACTIVE' => 'T',
   'CBOX%SLAT' => '55.',
   'CBOX%WLON' => '-1.5',
   'CBOX%NLAT' => '61.',
   'CBOX%ELON' => '9.',
   'MAP_SCALE'=>1.0e7,
   'MAP_CENTRE_LATITUDE'=>58.,
   'MAP_CENTRE_LONGITUDE'=>5.,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'Netherland' => {
   'CBOX%ACTIVE' => 'T',
   'CBOX%SLAT' => '51.',
   'CBOX%WLON' => '1.5',
   'CBOX%NLAT' => '54.5',
   'CBOX%ELON' => '9.',
   'MAP_SCALE'=>5.5e6,
   'MAP_CENTRE_LATITUDE'=>53.,
   'MAP_CENTRE_LONGITUDE'=>9.,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'Spain_Portugal' => {
   'CBOX%ACTIVE' => 'T',
   'CBOX%SLAT' => '35.',
   'CBOX%WLON' => '-10',
   'CBOX%NLAT' => '44.',
   'CBOX%ELON' => '4',
   'MAP_SCALE'=>8.5e6,
   'MAP_CENTRE_LATITUDE'=>40.,
   'MAP_CENTRE_LONGITUDE'=>0.,
   'MAP_VERTICAL_LONGITUDE'=>-5,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },   
  'Scandinavia' => {
   'CBOX%ACTIVE' => 'T',
   'CBOX%SLAT' => '55.',
   'CBOX%WLON' => '8',
   'CBOX%NLAT' => '70.',
   'CBOX%ELON' => '32',
   'MAP_SCALE'=>1.5e7,
   'MAP_CENTRE_LATITUDE'=>62.,
   'MAP_CENTRE_LONGITUDE'=>27.,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'France' => {
   'CBOX%ACTIVE' => 'T',
   'CBOX%SLAT' => '44.',
   'CBOX%WLON' => '-5',
   'CBOX%NLAT' => '50.',
   'CBOX%ELON' => '8',
   'MAP_SCALE'=>9.0e6,
   'MAP_CENTRE_LATITUDE'=>47.,
   'MAP_CENTRE_LONGITUDE'=>3.,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'Finland' => { 
   'STNLIST'=> '02790, 02823, 02862, 02863, 02873, 02889, 02907, 02910,
    02911, 02920, 02958, 02964, 02965, 02967, 02972, 02979,
    02980, 02984, 02987, 02988, 02991, 02992, 02993, 02721,
    02737, 02740, 02751, 02757, 02759, 02769, 02777, 02789,
    02795, 02801, 02808, 02814, 02816, 02824, 02834, 02844,
    02869, 02872, 02874, 02875, 02915, 02929, 02942, 02945,
    02948, 02966, 02978, 89014, 02701, 02705, 02714, 02715,
    02716, 02717, 02719, 02722, 02723, 02726, 02739, 02745,
    02799, 02800, 02803, 02804, 02813, 02815, 02817, 02818,
    02820, 02821, 02825, 02827, 02835, 02847, 02868, 02876,
    02710, 02711, 02725, 02727, 02728, 02729, 02730, 02731,
    02732, 02733, 02734, 02735, 02736, 02738, 02752, 02753,
    02761, 02762, 02763, 02765, 02772, 02778, 02780, 02781,
    02783, 02787, 02788, 02791, 02793, 02796, 02833, 02943,
    02703, 02743, 02746, 02747, 02754, 02758, 02773, 02794,
    02828, 02831, 02950, 02996, 02704, 02706, 02750, 02756,
    02767, 02768, 02770, 02771, 02797, 02798, 02811, 02812,
    02819, 02829, 02830, 02832, 02755, 02805, 02807, 02836,
    02845, 02849, 02866, 02897, 02913, 02917, 02924, 02935,
    02939, 02944, 02947, 02952, 02963, 02971, 02974, 02976,
    02981, 02982, 02741',
   'MAP_SCALE'=>1.e7,
   'MAP_CENTRE_LATITUDE'=>65.,
   'MAP_CENTRE_LONGITUDE'=>35.,
   'MAP_VERTICAL_LONGITUDE'=>20.,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'FinlandArea' => { 
   'CBOX%ACTIVE' => 'T',
   'CBOX%SLAT' => '60.',
   'CBOX%WLON' => '20',
   'CBOX%NLAT' => '70.',
   'CBOX%ELON' => '30',
   'MAP_SCALE'=>1.e7,
   'MAP_CENTRE_LATITUDE'=>65.,
   'MAP_CENTRE_LONGITUDE'=>35.,
   'MAP_VERTICAL_LONGITUDE'=>20.,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'Denmark' => {
   'STNLIST'=> '06030,06041,06043,06049,06052,
                06058,06060,06070,06072,06073,
		06074,06079,06080,06081,06096,
		06102,06104,06110,06116,06118,06119,
		06120,06123,06126,06135,06138,06141,
		06149,06151,06154,06156,06165,06168,
		06170,06180,06181,06190,06193',
   'MAP_SCALE'=>3.6e6,
   'MAP_CENTRE_LATITUDE'=>55.,
   'MAP_CENTRE_LONGITUDE'=>12.,
   'MAP_VERTICAL_LONGITUDE'=>10.,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'Norway' => {
   'STNLIST'=> '01025,01049,01102,01152,01205,01241,
      01271,01317,01384,01415,01452,01494',
   'MAP_SCALE'=>1.0e7,
   'MAP_CENTRE_LATITUDE'=>63.,
   'MAP_CENTRE_LONGITUDE'=>20.,
   'MAP_VERTICAL_LONGITUDE'=>-10.,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'Sweden' => {
   'STNLIST'=> '02013,02020,02024,02029,02031,02036,02038,02044,
                02049,02054,02055,02064,02072,02080,02095,02096,
                02096,02101,02102,02103,02104,02110,02120,02121,
                02124,02128,02130,02135,02141,02146,02147,02149,
                02151,02154,02161,02173,02174,02176,02181,02184,
                02186,02188,02191,02196,02196,02199,02206,02209,
                02216,02217,02219,02221,02222,02226,02230,02231,
                02233,02236,02242,02243,02244,02245,02247,02254,
                02256,02260,02261,02263,02265,02267,02269,02271,
                02282,02284,02286,02287,02292,02293,02297,02302,
                02303,02307,02308,02311,02317,02319,02321,02324,
                02324,02325,02329,02331,02337,02338,02343,02347,
                02349,02354,02355,02366,02368,02378,02382,02403,
                02408,02410,02411,02413,02417,02418,02423,02426,
                02429,02432,02435,02440,02441,02443,02449,02450,
                02452,02453,02456,02458,02460,02464,02468,02469,
                02472,02476,02481,02482,02484,02486,02487,02488,
                02490,02493,02496,02499,02500,02501,02505,02507,
                02513,02515,02516,02518,02520,02521,02526,02536,
                02539,02540,02542,02545,02546,02548,02549,02550,
                02552,02553,02554,02556,02557,02558,02559,02562,
                02563,02565,02566,02566,02567,02574,02575,02583,
                02587,02589,02590,02595,02598,02602,02603,02605,
                02607,02609,02611,02616,02618,02620,02620,02622,
                02623,02625,02628,02632,02635,02636,02644,02646,
                02648,02661,02664,02670,02680',
   'MAP_SCALE'=>1.0e7,
   'MAP_CENTRE_LATITUDE'=>65.,
   'MAP_CENTRE_LONGITUDE'=>20.,
   'MAP_PROJECTION'=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'Baltex' => {
   'LPOLY'               => 'T',
   'POLYFILE'            => '\'Baltex.poly\'',
   'MAP_SCALE'           => 1.5e7,
   'MAP_CENTRE_LATITUDE' => 60.,
   'MAP_CENTRE_LONGITUDE'=> 30.,
   'MAP_VERTICAL_LONGITUDE'=>20.,
   'MAP_PROJECTION'      => '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'Baltic_sea' => {
   'LPOLY'               => 'T',
   'POLYFILE'            => '\'Baltic_sea.poly\'',
   'MAP_SCALE'           => 1.5e7,
   'MAP_CENTRE_LATITUDE' => 60.,
   'MAP_CENTRE_LONGITUDE'=> 30.,
   'MAP_VERTICAL_LONGITUDE'=>20.,
   'MAP_PROJECTION'      => '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'Eumou' => {
   'STNLIST'=> '1238, 1245,1288,1340,1350,1359,1364,1393,1433,1434,1441,1446,
            1447,2013,2020,2029,2102,2103,2110,2121,2130,2209,2221,2233,2256,
            2302,2303,2307,2308,2311,2317,2325,3039,3041,3065,3072,3148,3227,
            4208,4214,4220,4242,4250,4253,4260,4266,4272,4285,4320,4351,4373,
            6496,6605,6608,6609,6612,6616,6619,6625,6627,6628,6631,6637,
            6638,6639,6645,6655,6656,6659,6660,6669,6675,6679,6680,6681,
            6689,6702,6704,6717,6722,6724,6727,6730,6734,6735,6738,6744,
            6745,6748,6750,6751,6753,6756,6759,6780,6782,6783,6784,6785,
            6786,6788,6789,6791,6792,6794,6797,6798,7270,7360,7469,7471,
            7497,7549,7552,7555,7558,7560,7591,8053,8055,8075,8080,8094,
            8112,8117,8130,8140,8141,8148,8157,8202,8210,8213,8215,8219,
            8220,8221,8222,8223,8224,8226,8227,8231,8232,8233,8235,
            8272,8280,8335,8348,8349,8417,8419,8420,8553,8560,8567,
            8568,8571,8575,8912,8922,10427,10452,10453,10507,10526,10528,
            10544,10552,10557,10558,10564,10574,10578,10579,10582,10616,10635,
            10645,10684,10685,10686,10724,10777,10782,10791,10796,10815,10818,
            10827,10828,10836,10837,10838,10850,10856,10857,10858,10859,10865,
            10908,10912,10945,10946,10948,10954,10961,10962,10963,10980,10982,
            11001,11008,11015,11019,11020,11021,11024,11078,11112,11119,11120,
            11127,11128,11135,11136,11138,11141,11142,11143,11144,11146,11147,
            11149,11152,11155,11157,11165,11170,11174,11176,11180,11185,11201,
            11204,11212,11214,11220,11227,11234,11241,11252,11255,11260,11261,
            11263,11265,11270,11272,11280,11292,11301,11305,11308,11310,11312,
            11314,11316,11320,11322,11325,11337,11340,11343,11345,11346,11348,
            11351,11354,11355,11356,11357,11358,11360,11362,11365,11375,11378,
            11382,11384,11385,11414,11423,11457,11464,11487,11538,11628,11636,
            11643,11659,11669,11683,11710,11730,11766,11787,11916,11918,11930,
            11933,11934,11938,11952,11958,12510,12625,12650,12851,13289,13361,
            13363,13367,13369,13378,13459,13465,13473,13481,13492,13493,13573,
            13575,13576,13577,13578,13579,13580,13582,13583,13584,13585,13590,
            13593,13594,13598,13619,13629,14006,14007,14008,14010,14012,14024,
            14112,14219,14235,14324,14330,14650,14652,14653,14654,14656,15015,
            15033,15069,15088,15099,15107,15108,15118,15119,15127,15138,15148,
            15162,15163,15165,15168,15170,15184,15215,15217,15238,15254,15261,
            15262,15277,15279,15280,15282,15284,15285,15296,15297,15300,15301,
            15302,15315,15316,15317,15319,15320,15324,15325,15344,15345,15502,
            15600,15601,15605,15613,15614,15615,15627,15725,15726,16008,16014,
            16021,16022,16033,16040,16041,16052,16054,16058,16072,16073,16111,
            16114,16115,16124,16129,16134,16135,16136,16164,16165,16168,16178,
            16179,16204,16219,16226,16227,16252,16258,16261,16262,16263,16300,
            16316,16317,16344,16345,16364,16434,16435,16442,16450,16465,16524,
            16531,16532,16537,16538,16541,16545,16613,16632,16710,17070,17074,
            17080,17082,17084,17086,17088,17090,17092,17095,17096,17098,17099,
            17100,17120,17124,17127,17128,17129,17130,17131,17135,17140,17155,
            17160,17162,17170,17188,17189,17190,17193,17194,17195,17199,17200,
            17202,17203,17204,17205,17210,17238,17240,17241,17244,17248,
            17250,17255,17260,17265,17270,17272,17275,17280,17281,17282,17285',
   'MAP_SCALE'=>3.2e7,
   'MAP_CENTRE_LATITUDE'=>50.,
   'MAP_CENTRE_LONGITUDE'=>25.,
   'MAP_PROJECTION '=> '\'POLAR_STEREOGRAPHIC\'',
   'MAP_AREA_DEFINITION '=> '\'CENTRE\'',
   },
  'EAall' => {
   'STNLIST'=> ' 50136, 50353, 50434, 50468, 50527, 50548, 50557, 50564, 50603, 50632, 50658,
 50727, 50745, 50756, 50774, 50788, 50844, 50854, 50888, 50915, 50949, 50953,
 50963, 50968, 50978, 50983, 51053, 51076, 51087, 51133, 51156, 51243, 51288,
 51334, 51379, 51431, 51463, 51467, 51495, 51542, 51573, 51644, 51656, 51709,
 51711, 51716, 51730, 51747, 51765, 51777, 51811, 51818, 51828, 51839, 51886,
 52112, 52118, 52203, 52267, 52323, 52378, 52418, 52436, 52495, 52533, 52602,
 52652, 52681, 52713, 52737, 52754, 52787, 52818, 52836, 52866, 52908, 52955,
 52983, 52996, 53068, 53083, 53149, 53192, 53231, 53276, 53336, 53352, 53391,
 53463, 53480, 53487, 53502, 53513, 53529, 53543, 53564, 53588, 53593, 53614,
 53646, 53673, 53698, 53705, 53723, 53764, 53772, 53787, 53798, 53845, 53863,
 53898, 53915, 53923, 53959, 53975, 54012, 54026, 54027, 54049, 54094, 54096,
 54102, 54115, 54135, 54157, 54161, 54186, 54208, 54218, 54226, 54236, 54259,
 54273, 54292, 54308, 54311, 54324, 54337, 54342, 54346, 54374, 54377, 54386,
 54401, 54405, 54423, 54436, 54471, 54493, 54497, 54511, 54527, 54534, 54539,
 54602, 54618, 54662, 54715, 54725, 54727, 54751, 54753, 54776, 54808, 54823,
 54826, 54836, 54843, 54857, 54863, 54909, 54916, 54929, 54945, 55228, 55279,
 55299, 55472, 55578, 55591, 55664, 55696, 55773, 56004, 56018, 56021, 56029,
 56033, 56046, 56065, 56079, 56080, 56096, 56106, 56116, 56137, 56144, 56146,
 56152, 56167, 56172, 56182, 56187, 56193, 56196, 56247, 56257, 56287, 56294,
 56312, 56357, 56374, 56385, 56444, 56462, 56492, 56571, 56586, 56651, 56671,
 56684, 56691, 56739, 56748, 56751, 56763, 56768, 56778, 56786, 56838, 56886,
 56946, 56951, 56954, 56959, 56964, 56966, 56969, 56977, 56985, 57014, 57025,
 57036, 57046, 57067, 57071, 57083, 57127, 57131, 57178, 57193, 57237, 57245,
 57259, 57265, 57279, 57290, 57297, 57306, 57328, 57348, 57378, 57399, 57411,
 57426, 57447, 57461, 57476, 57494, 57503, 57516, 57554, 57584, 57598, 57604, 
 57633, 57655, 57662, 57679, 57687, 57707, 57713, 57731, 57745, 57766, 57776,
 57793, 57799, 57816, 57832, 57845, 57853, 57866, 57902, 57916, 57922, 57932,
 57957, 57972, 57993, 58027, 58040, 58102, 58144, 58150, 58203, 58208, 58221,
 58238, 58251, 58265, 58314, 58321, 58338, 58345, 58362, 58424, 58437, 58457,
 58472, 58477, 58506, 58527, 58543, 58556, 58569, 58606, 58633, 58646, 58660,
 58665, 58666, 58715, 58725, 58730, 58731, 58752, 58754, 58813, 58834, 58847,
 58849, 58853, 58911, 58921, 58926, 58931, 58944, 58965, 58968, 58974, 59007,
 59023, 59046, 59058, 59072, 59082, 59087, 59096, 59102, 59117, 59134, 59152,
 59158, 59162, 59209, 59211, 59254, 59265, 59278, 59280, 59287, 59293, 59316,
 59345, 59348, 59353, 59358, 59362, 59368, 59417, 59431, 59456, 59493, 59501,
 59553, 59554, 59556, 59559, 59562, 59567, 59632, 59644, 59658, 59663, 59673,
 59758, 59792, 59838, 59845, 59855, 59948, 59981, 59985, 59997, 47003, 47005,
 47008, 47014, 47016, 47020, 47022, 47025, 47028, 47031, 47035, 47037, 47039,
 47041, 47046, 47050, 47052, 47055, 47058, 47060, 47061, 47065, 47067, 47068,
 47069, 47070, 47075, 45004, 45005, 45007, 45032, 45035, 45039, 45044, 45045,
 47401, 47402, 47404, 47405, 47406, 47407, 47409, 47411, 47412, 47413, 47417,
 47418, 47420, 47421, 47423, 47424, 47425, 47426, 47428, 47430, 47433, 47435,
 47440, 47441, 47474, 47476, 47477, 47479, 47481, 47483, 47487, 47488, 47489,
 47490, 47512, 47513, 47515, 47516, 47520, 47542, 47545, 47549, 47551, 47553,
 47557, 47567, 47569, 47570, 47573, 47574, 47575, 47576, 47580, 47581, 47582,
 47583, 47584, 47585, 47587, 47588, 47590, 47591, 47592, 47595, 47597, 47598, 
 47600, 47602, 47604, 47605, 47606, 47607, 47610, 47612, 47615, 47616, 47617, 
 47618, 47620, 47622, 47624, 47626, 47629, 47631, 47632, 47634, 47636, 47637,
 47638, 47639, 47640, 47641, 47642, 47643, 47646, 47648, 47649, 47651, 47652,
 47653, 47654, 47655, 47656, 47657, 47658, 47660, 47661, 47662, 47663, 47666,
 47668, 47670, 47671, 47672, 47674, 47675, 47677, 47678, 47679, 47680, 47681, 
 47682, 47684, 47685, 47686, 47687, 47688, 47690, 47692, 47696, 47702, 47704,
 47706, 47707, 47709, 47715, 47716, 47721, 47727, 47730, 47735, 47738, 47740,
 47741, 47742, 47743, 47744, 47746, 47747, 47749, 47750, 47754, 47755, 47756,
 47759, 47761, 47762, 47764, 47765, 47766, 47767, 47768, 47769, 47770, 47771,
 47772, 47774, 47776, 47777, 47778, 47779, 47780, 47782, 47783, 47784, 47786,
 47787, 47788, 47789, 47790, 47793, 47794, 47796, 47799, 47800, 47803, 47805,
 47807, 47808, 47809, 47810, 47811, 47812, 47813, 47814, 47815, 47817, 47818,
 47819, 47821, 47822, 47823, 47824, 47827, 47829, 47830, 47831, 47835, 47836,
 47837, 47838, 47840, 47843, 47844, 47850, 47851, 47852, 47853, 47854, 47855,
 47856, 47857, 47860, 47870, 47872, 47880, 47881, 47882, 47883, 47884, 47887,
 47890, 47891, 47892, 47893, 47895, 47897, 47898, 47899, 47909, 47912, 47917,
 47918, 47919, 47925, 47926, 47927, 47928, 47929, 47930, 47931, 47933, 47936,
 47938, 47940, 47942, 47945, 47971, 47981, 47991, 44203, 44207, 44212, 44213,
 44214, 44215, 44218, 44225, 44230, 44231, 44232, 44237, 44239, 44241, 44256,
 44259, 44265, 44272, 44275, 44277, 44282, 44284, 44285, 44287, 44288, 44292,
 44294, 44298, 44302, 44304, 44305, 44313, 44314, 44317, 44336, 44341, 44347, 
 44352, 44354, 44358, 44373, 47090, 47091, 47092, 47095, 47098, 47099, 47100,
 47101, 47102, 47105, 47106, 47108, 47110, 47112, 47113, 47114, 47115, 47118,
 47119, 47120, 47121, 47122, 47124, 47125, 47126, 47127, 47128, 47129, 47130,
 47131, 47133, 47135, 47136, 47137, 47138, 47139, 47140, 47141, 47142, 47143,
 47146, 47151, 47152, 47153, 47155, 47156, 47158, 47159, 47161, 47162, 47165,
 47166, 47167, 47168, 47169, 47170, 47175, 47182, 47184, 47185, 47189, 47192',
   'MAP_SCALE'=>5.0e7,
   'MAP_PROJECTION'=> '\'MERCATOR\'',
   'MAP_AREA_DEFINITION'=>'\'CORNER\'',
   'MAP_LOWER_LEFT_LATITUDE'=>15,
   'MAP_LOWER_LEFT_LONGITUDE'=>100,
   'MAP_UPPER_RIGHT_LATITUDE'=>50,
   'MAP_UPPER_RIGHT_LONGITUDE'=>140,
   },
  'China' => {
   'STNLIST'=> ' 50136, 50353, 50434, 50468, 50527, 50548, 50557, 50564, 50603,
       50632, 50658, 50727, 50745, 50756, 50774, 50788, 50844, 50854,
       50888, 50915, 50949, 50953, 50963, 50968, 50978, 50983, 51053,
       51076, 51087, 51133, 51156, 51243, 51288, 51334, 51379, 51431,
       51463, 51467, 51495, 51542, 51573, 51644, 51656, 51709, 51711,
       51716, 51730, 51747, 51765, 51777, 51811, 51818, 51828, 51839,
       51886, 52112, 52118, 52203, 52267, 52323, 52378, 52418, 52436,
       52495, 52533, 52602, 52652, 52681, 52713, 52737, 52754, 52787,
       52818, 52836, 52866, 52908, 52955, 52983, 52996, 53068, 53083,
       53149, 53192, 53231, 53276, 53336, 53352, 53391, 53463, 53480,
       53487, 53502, 53513, 53529, 53543, 53564, 53588, 53593, 53614,
       53646, 53673, 53698, 53705, 53723, 53764, 53772, 53787, 53798,
       53845, 53863, 53898, 53915, 53923, 53959, 53975, 54012, 54026,
       54027, 54049, 54094, 54096, 54102, 54115, 54135, 54157, 54161,
       54186, 54208, 54218, 54226, 54236, 54259, 54273, 54292, 54308,
       54311, 54324, 54337, 54342, 54346, 54374, 54377, 54386, 54401,
       54405, 54423, 54436, 54471, 54493, 54497, 54511, 54527, 54534,
       54539, 54587, 54602, 54618, 54662, 54715, 54725, 54727, 54751,
       54753, 54776, 54808, 54823, 54826, 54836, 54843, 54857, 54863,
       54909, 54916, 54929, 54945, 55228, 55279, 55299, 55472, 55578,
       55591, 55664, 55696, 55773, 56004, 56018, 56021, 56029, 56033,
       56046, 56065, 56079, 56080, 56096, 56106, 56116, 56137, 56144,
       56146, 56152, 56167, 56172, 56182, 56187, 56193, 56196, 56247,
       56257, 56287, 56294, 56312, 56357, 56374, 56385, 56444, 56462,
       56492, 56571, 56586, 56651, 56671, 56684, 56691, 56739, 56748,
       56751, 56763, 56768, 56778, 56786, 56838, 56886, 56946, 56951,
       56954, 56959, 56964, 56966, 56969, 56977, 56985, 57014, 57025,
       57036, 57046, 57067, 57071, 57083, 57127, 57131, 57178, 57193,
       57237, 57245, 57259, 57265, 57279, 57290, 57297, 57306, 57328,
       57348, 57378, 57399, 57411, 57426, 57447, 57461, 57476, 57494,
       57503, 57516, 57554, 57584, 57598, 57604, 57633, 57655, 57662,
       57679, 57687, 57707, 57713, 57731, 57745, 57749, 57766, 57776,
       57793, 57799, 57816, 57832, 57845, 57853, 57866, 57902, 57916,
       57922, 57932, 57957, 57972, 57993, 58027, 58040, 58102, 58141,
       58144, 58150, 58203, 58208, 58221, 58238, 58251, 58265, 58314,
       58321, 58338, 58345, 58362, 58424, 58437, 58457, 58472, 58477,
       58506, 58527, 58543, 58556, 58569, 58606, 58633, 58646, 58660,
       58665, 58666, 58715, 58725, 58730, 58731, 58752, 58754, 58813,
       58834, 58847, 58849, 58853, 58911, 58921, 58926, 58931, 58944,
       58965, 58968, 58974, 59007, 59023, 59046, 59058, 59072, 59082,
       59087, 59096, 59102, 59117, 59134, 59152, 59158, 59162, 59209,
       59211, 59254, 59265, 59278, 59280, 59287, 59293, 59316, 59345,
       59348, 59353, 59358, 59362, 59368, 59417, 59431, 59456, 59493,
       59501, 59553, 59554, 59556, 59559, 59562, 59567, 59632, 59644,
       59658, 59663, 59673, 59758, 59792, 59838, 59845, 59855, 59948,
       59981, 59985, 59997, 45004, 45005, 45007, 45009, 45010, 45031,
       45032, 45033, 45034, 45035, 45036, 45037, 45038, 45039, 45040,
       45041, 45042, 45043, 45044, 45045, 45011, 45013, 45018, 45020',
   'MAP_SCALE'=>5.0e7,
   'MAP_PROJECTION'=> '\'MERCATOR\'',
   'MAP_AREA_DEFINITION'=>'\'CORNER\'',
   'MAP_LOWER_LEFT_LATITUDE'=>15,
   'MAP_LOWER_LEFT_LONGITUDE'=>100,
   'MAP_UPPER_RIGHT_LATITUDE'=>50,
   'MAP_UPPER_RIGHT_LONGITUDE'=>140,
   },
  'EAmou' => {
   'STNLIST'=> ' 50527,50603,50632,50727,50915,51053,51076,51087,51133,
        51156,51379,51431,51463,51467,51495,51542,51644,51656,
        51709,51711,51716,51730,51747,51765,51777,51811,51818,
        51828,51839,51886,52118,52203,52267,52323,52378,52418,
        52436,52495,52533,52602,52652,52681,52713,52737,52754,
        52787,52818,52836,52866,52908,52955,52983,52996,53068,
        53083,53149,53192,53231,53276,53336,53352,53391,53463,
        53480,53487,53502,53513,53529,53543,53564,53588,53593,
        53614,53646,53673,53705,53723,53764,53772,53787,53845,
        53863,53915,53923,53975,54012,54102,54115,54186,54208,
        54218,54308,54311,54386,54401,54405,54826,55228,55279,
        55299,55472,55578,55591,55664,55696,55773,56004,56018,
        56021,56029,56033,56046,56065,56079,56080,56096,56106,
        56116,56137,56144,56146,56152,56167,56172,56182,56187,
        56193,56196,56247,56257,56287,56294,56312,56357,56374,
        56385,56444,56462,56571,56586,56651,56671,56684,56691,
        56739,56748,56751,56763,56768,56778,56786,56838,56886,
        56946,56951,56954,56959,56964,56969,56977,56985,57014,
        57025,57046,57067,57127,57237,57633,57707,57713,57776,
        57816,57832,57902,57922,58437,58506,58931,59007,59209,
        59353,47016,47022,47031,47617,47618,47620,47622,47637,
        47639,47640,47690,47709,47721,47821,44203,44207,44212,
        44213,44214,44215,44218,44225,44230,44231,44232,44237,
        44239,44241,44256,44259,44265,44272,44275,44277,44282,
        44284,44285,44287,44288,44292,44294,44298,44302,44304,
        44305,44313,44314,44317,44336,44341,44347,44352,44354,
        44358,44373,47100,47175',
   'MAP_SCALE'=>5.0e7,
   'MAP_PROJECTION'=> '\'MERCATOR\'',
   'MAP_AREA_DEFINITION'=>'\'CORNER\'',
   'MAP_LOWER_LEFT_LATITUDE'=>15,
   'MAP_LOWER_LEFT_LONGITUDE'=>100,
   'MAP_UPPER_RIGHT_LATITUDE'=>50,
   'MAP_UPPER_RIGHT_LONGITUDE'=>140,
   },
  'EAcoast' => {
   'STNLIST'=> ' 
       59316, 54527, 47800, 47740, 47605, 47600, 47765, 47891, 47887, 47898,
       47895, 47899, 47772, 47778, 47663, 47636, 47651, 47655, 47675, 47662,
       54776, 47050, 47060, 47068, 47102, 47069, 47112, 47129, 47140, 47165,
       47815, 47830, 47817, 47843, 47185, 47189, 47184, 47169, 47175, 47170,
       47168, 47162, 47182, 47159, 47143, 47152, 47138, 47130, 47106, 47105,
       47090, 47061, 47041, 47046, 47025, 47055, 47750, 47746, 47741, 47755,
       47807, 47827, 47837, 47936, 47909, 54539, 54337, 54471, 54662, 54843,
       54753, 54751, 54863, 54857, 54945, 58040, 58150, 58251, 58362, 58472,
       58457, 58265, 58477, 58569, 58660, 58666, 58752, 58754, 58847, 59567,
       58968, 59559, 59562, 59358, 58944, 59501, 59134, 58974',
   'MAP_SCALE'=>5.0e7,
   'MAP_PROJECTION'=> '\'MERCATOR\'',
   'MAP_AREA_DEFINITION'=>'\'CORNER\'',
   'MAP_LOWER_LEFT_LATITUDE'=>15,
   'MAP_LOWER_LEFT_LONGITUDE'=>110,
   'MAP_UPPER_RIGHT_LATITUDE'=>50,
   'MAP_UPPER_RIGHT_LONGITUDE'=>140,
   },
  'Korea' => {
   'STNLIST'=> ' 47003,47005,47008,47014,47016,47020,47022,47025,47028,
      47031,47035,47037,47039,47041,47046,47050,47052,47055,
      47058,47060,47061,47065,47067,47068,47069,47070,47075,
      47080,47090,47091,47092,47094,47095,47098,47099,47100,
      47101,47102,47103,47105,47106,47107,47108,47110,47111,
      47112,47113,47114,47115,47116,47118,47119,47120,47121,
      47122,47124,47125,47126,47127,47128,47129,47130,47131,
      47132,47133,47134,47135,47136,47137,47138,47139,47140,
      47141,47142,47143,47144,47146,47148,47151,47152,47153,
      47155,47156,47158,47159,47160,47161,47162,47165,47166,
      47167,47168,47169,47170,47175,47182,47184,47185,47187,
      47189,47192',
   'MAP_SCALE'=>2.0e7,
   'MAP_PROJECTION'=> '\'MERCATOR\'',
   'MAP_AREA_DEFINITION'=>'\'CORNER\'',
   'MAP_LOWER_LEFT_LATITUDE'=>25,
   'MAP_LOWER_LEFT_LONGITUDE'=>110,
   'MAP_UPPER_RIGHT_LATITUDE'=>50,
   'MAP_UPPER_RIGHT_LONGITUDE'=>140,
   },
  'Japan' => {
   'STNLIST'=> ' 
      47401,47402,47404,47405,47406,47407,47409,47411,47412,
      47413,47417,47418,47420,47421,47423,47424,47425,47426,
      47428,47430,47433,47435,47440,47441,47474,47476,47477,
      47479,47481,47483,47487,47488,47489,47490,47512,47513,
      47515,47516,47520,47542,47545,47549,47551,47553,47557,
      47567,47569,47570,47573,47574,47575,47576,47580,47581,
      47582,47583,47584,47585,47587,47588,47590,47591,47592,
      47595,47597,47598,47600,47602,47604,47605,47606,47607,
      47610,47612,47615,47616,47617,47618,47620,47622,47624,
      47626,47629,47631,47632,47634,47636,47637,47638,47639,
      47640,47641,47642,47643,47646,47648,47649,47651,47652,
      47653,47654,47655,47656,47657,47658,47660,47661,47662,
      47663,47666,47668,47670,47671,47672,47674,47675,47677,
      47678,47679,47680,47681,47682,47684,47685,47686,47687,
      47688,47690,47692,47696,47702,47704,47706,47707,47709,
      47715,47716,47721,47727,47730,47735,47738,47740,47741,
      47742,47743,47744,47746,47747,47749,47750,47754,47755,
      47756,47759,47761,47762,47764,47765,47766,47767,47768,
      47769,47770,47771,47772,47774,47776,47777,47778,47779,
      47780,47782,47783,47784,47786,47787,47788,47789,47790,
      47793,47794,47796,47799,47800,47803,47805,47807,47808,
      47809,47810,47811,47812,47813,47814,47815,47817,47818,
      47819,47821,47822,47823,47824,47827,47829,47830,47831,
      47835,47836,47837,47838,47840,47843,47844,47850,47851,
      47852,47853,47854,47855,47856,47857,47860,47870,47872,
      47880,47881,47882,47883,47884,47887,47890,47891,47892,
      47893,47895,47897,47898,47899,47909,47912,47917,47918,
      47919,47925,47926,47927,47928,47929,47930,47931,47933,
      47936,47938,47940,47942,47945,47971,47981,47991',
   'MAP_SCALE'=>5.0e7,
   'MAP_PROJECTION'=> '\'MERCATOR\'',
   'MAP_AREA_DEFINITION'=>'\'CORNER\'',
   'MAP_LOWER_LEFT_LATITUDE'=>15,
   'MAP_LOWER_LEFT_LONGITUDE'=>110,
   'MAP_UPPER_RIGHT_LATITUDE'=>50,
   'MAP_UPPER_RIGHT_LONGITUDE'=>140,
   },
 );
