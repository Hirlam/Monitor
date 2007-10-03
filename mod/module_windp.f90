MODULE windp

 IMPLICIT NONE

 TYPE windp_type
      CHARACTER(LEN=100) :: desc
      INTEGER :: nr
      INTEGER :: x
      INTEGER :: y
      REAL    :: lat
      REAL    :: lon
      INTEGER :: id
      INTEGER :: typ_ind
      REAL    :: hubhgt
      REAL    :: hgt
 END TYPE windp_type


 INTEGER, PARAMETER :: maxwindp      = 41
 INTEGER, PARAMETER :: maxwindp_type =  8

 INTEGER :: windp_index

 REAL :: wind_map(0:maxwindp_type,0:50,2),      &
         ab_half(3,61,2)

 TYPE (windp_type) :: windp_stations(maxwindp)

 CHARACTER(LEN=20) :: windtyp_name(0:maxwindp_type)

 LOGICAL :: init_done = .FALSE.

 CONTAINS

 SUBROUTINE init_windp

 IMPLICIT NONE

 INTEGER :: i,i_max

 !----------------------------------------------------------
 !
 ! Effektkurvor för olika typer av vindkraftverk
 ! Kurvorna är tagna ur dokumentationen från 
 ! projektplatsen
 !
 ! Radnumret anger vindhastighet (m/s)
 ! Första kolumnen anger motsvarande effekt i kW
 ! För vindhastigheter utanför det givna intervallet är effekten == 0
 ! Samtliga siffror är för normal luftdensitet 1.225 kg/m^3
 !
 !

 IF ( init_done ) RETURN

 windtyp_name(0) = ' No type'
 windtyp_name(1) = 'WindWorld  600kW'
 windtyp_name(2) = 'WindWorld 2000kW'
 windtyp_name(3) = 'Vestas     850kW'
 windtyp_name(4) = 'Vestas     660kW'
 windtyp_name(5) = 'Vestas     600kW'
 windtyp_name(6) = 'NEG Micon  900kW'
 windtyp_name(7) = 'NEG Micon  750kW'
 windtyp_name(8) = 'Enercon   1500kW'

 wind_map = 0.

 ! WindWorld 600kW = 1
 wind_map(1, 5,:) = (/16.72 , 0.158/)
 wind_map(1, 6,:) = (/60.88 , 0.332/)
 wind_map(1, 7,:) = (/113.27, 0.389/)
 wind_map(1, 8,:) = (/179.85, 0.414/)
 wind_map(1, 9,:) = (/253.71, 0.410/)
 wind_map(1,10,:) = (/334.40, 0.394/)
 wind_map(1,11,:) = (/423.95, 0.375/)
 wind_map(1,12,:) = (/497.49, 0.339/)
 wind_map(1,13,:) = (/557.13, 0.299/)
 wind_map(1,14,:) = (/610.52, 0.262/)
 wind_map(1,15,:) = (/613.24, 0.214/)
 wind_map(1,16,:) = (/630.77, 0.181/)
 wind_map(1,17,:) = (/630.87, 0.151/)
 wind_map(1,18,:) = (/624.03, 0.126/)
 wind_map(1,19,:) = (/600.86, 0.103/)
 wind_map(1,20,:) = (/579.21, 0.085/)
 !
 ! Extra wind speed added from experience 2006-03-31
 !
!wind_map(1,21,:) = (/550.21, 0.000/)
!wind_map(1,22,:) = (/550.21, 0.000/)
!wind_map(1,23,:) = (/550.21, 0.000/)
!wind_map(1,24,:) = (/550.21, 0.000/)
!wind_map(1,25,:) = (/550.21, 0.000/)
!wind_map(1,26,:) = (/550.21, 0.000/)
!wind_map(1,27,:) = (/550.21, 0.000/)

 ! Vestas 2000kW   = 2
 wind_map(2, 4:18, 1) = (/66.3, 152., 280., 457., 690.,978., 1296., 1598., 1818., 1935., 1980., 1995., 1999., 2000., 2000./)
 wind_map(2, 19:25,1) = 2000.

 ! Vestas 850kW    = 3
 wind_map(3, 4:18, 1) = (/25.5,67.4,125.,203.,304.,425.,554.,671.,759.,811.,836.,846.,849.,850.,850./)
 wind_map(3, 19:25,1) = 850.

 ! Vestas 660kW    = 4
 wind_map(4, 4:18, 1) = (/2.,9.,43.8,96.7,166., 252., 350., 450., 538., 600., 635., 651., 657., 659., 660./)
 wind_map(4, 19:25,1) = 660.

 ! Vestas     600kW = 5
 wind_map(5,3 ,:)=(/0.,     0./)
 wind_map(5,4 ,:)=(/0.,     0./)
 wind_map(5,5 ,:)=(/22.,    0.207/)
 wind_map(5,6 ,:)=(/65.,    0.355/)
 wind_map(5,7 ,:)=(/120.,   0.412/)
 wind_map(5,8 ,:)=(/188.,   0.433/)
 wind_map(5,9 ,:)=(/268.,   0.433/)
 wind_map(5,10,:)=(/356.,   0.42/)
 wind_map(5,11,:)=(/440.,   0.39/)
 wind_map(5,12,:)=(/510.,   0.348/)
 wind_map(5,13,:)=(/556.,   0.298/)
 wind_map(5,14,:)=(/582.,   0.25/)
 wind_map(5,15,:)=(/594.,   0.207/)
 wind_map(5,16,:)=(/598.,   0.172/)
 wind_map(5,17,:)=(/600.,   0.144/)
 wind_map(5,18,:)=(/600.,   0.121/)
 wind_map(5,19,:)=(/600.,   0.103/)
 wind_map(5,20,:)=(/600.,   0.088/)
 wind_map(5,21,:)=(/600.,   0.076/)
 wind_map(5,22,:)=(/600.,   0.066/)
 wind_map(5,23,:)=(/600.,   0.058/)
 wind_map(5,24,:)=(/600.,   0.051/)
 wind_map(5,25,:)=(/600.,   0.045/)

 ! NEG Micon  900kW = 6
 wind_map(6,03,:) = (/    0.,  0.        /)
 wind_map(6,04,:) = (/   27.,  0.324     /)
 wind_map(6,05,:) = (/   68.,  0.418     /)
 wind_map(6,06,:) = (/  118.,  0.42      /)
 wind_map(6,07,:) = (/  199.,  0.446     /)
 wind_map(6,08,:) = (/  304.,  0.456     /)
 wind_map(6,09,:) = (/  421.,  0.444     /)
 wind_map(6,10,:) = (/  541.,  0.416     /)
 wind_map(6,11,:) = (/  640.,  0.37      /)
 wind_map(6,12,:) = (/  725.,  0.323     /)
 wind_map(6,13,:) = (/  791.,  0.277     /)
 wind_map(6,14,:) = (/  839.,  0.235     /)
 wind_map(6,15,:) = (/  872.,  0.199     /)
 wind_map(6,16,:) = (/  891.,  0.167     /)
 wind_map(6,17,:) = (/  900.,  0.141     /)
 wind_map(6,18,:) = (/  898.,  0.118     /)
 wind_map(6,19,:) = (/  892.,  0.1       /)
 wind_map(6,20,:) = (/  882.,  0.085     /)
 wind_map(6,21,:) = (/  871.,  0.072     /)
 wind_map(6,22,:) = (/  860.,  0.062     /)
 wind_map(6,23,:) = (/  852.,  0.054     /)
 wind_map(6,24,:) = (/  846.,  0.047     /)
 wind_map(6,25,:) = (/  843.,  0.041     /)
 
 ! NEG Micon  750kW

 wind_map(7,3 ,:)=(/0.     ,0.   /)
 wind_map(7,4 ,:)=(/19.5   ,0.273/)
 wind_map(7,5 ,:)=(/53.1   ,0.38 /)
 wind_map(7,6 ,:)=(/97.4   ,0.403/)
 wind_map(7,7 ,:)=(/155.3  ,0.405/)
 wind_map(7,8 ,:)=(/244.6  ,0.427/)
 wind_map(7,9 ,:)=(/349.2  ,0.429/)
 wind_map(7,10,:)=(/462.2  ,0.414/)
 wind_map(7,11,:)=(/564.5  ,0.379/)
 wind_map(7,12,:)=(/640.5  ,0.332/)
 wind_map(7,13,:)=(/696.3  ,0.284/)
 wind_map(7,14,:)=(/729.8  ,0.238/)
 wind_map(7,15,:)=(/745.5  ,0.198/)
 wind_map(7,16,:)=(/750.   ,0.164/)
 wind_map(7,17,:)=(/744.6  ,0.136/)
 wind_map(7,18,:)=(/734.8  ,0.113/)
 wind_map(7,19,:)=(/723.   ,0.094/)
 wind_map(7,20,:)=(/711.9  ,0.08 /)
 wind_map(7,21,:)=(/701.4  ,0.068/)
 wind_map(7,22,:)=(/694.3  ,0.058/)
 wind_map(7,23,:)=(/692.8  ,0.051/)
 wind_map(7,24,:)=(/695.2  ,0.045/)
 wind_map(7,25,:)=(/700.6  ,0.04 /)

 ! Enercon  1500kW = 8
 wind_map(8,1    ,1) = 0.0
 wind_map(8,2    ,1) = 0.0
 wind_map(8,3    ,1) = 7.5
 wind_map(8,4    ,1) = 48.0
 wind_map(8,5    ,1) = 104.0
 wind_map(8,6    ,1) = 200.0
 wind_map(8,7    ,1) = 340.0
 wind_map(8,8    ,1) = 515.0
 wind_map(8,9    ,1) = 748.0
 wind_map(8,10   ,1) = 1025.
 wind_map(8,11   ,1) = 1348.
 wind_map(8,12   ,1) = 1670.
 wind_map(8,13   ,1) = 1760.
 wind_map(8,14:25,1) = 1800.

 CALL print_windp_type
 !
 ! Koppling mellan stationsnummer och typ av vindkraftverk
 !
 !----------------------------------------------------------
 !
 !----------------------------------------------------------
 ! Id       nr     Namn                                 Kommentar
 ! 3395007         Annelöv Boställe produktion          Park,Enercon 1500kW
 !          714,715
 ! 3617624  736    Badene produktion                    NEG Micon 900
 ! 3394348  680    Bertels 3:1 Hablingbo produktion     NEG Micon 750
 ! 3312105  633    Bjälbo 2, Mjölbya                    Typ saknas
 ! 3758936  722    Frittorp produktion                  Vestas 850
 ! 3628435  602    Högby 1 produktion, Skänninge        Vestas 850
 ! 3765759         Karlebytorp windproduktion           Typ saknas
 ! 3656662  301    Kastlösa V09 produktion              Windworld 600
 ! 3298933  295    Kastlösa verk 3, Borgholm            Windworld 600
 ! 3650962  725    Kärrets gård 2 produktion            Vestas 850
 ! 3641606  393    Kärrets gård produktion              Vestas 660
 ! 3617740         Mönarp 1 produktion                  Typ saknas
 ! 3614663  728    Skeby/Tungelunda vind 1 produktion   Vestas 850
 ! 3629363  730    Skeby/Tungelunda vind 2 produktion   Vestas 850
 ! 3614618  729    Skeby/Tungelunda vind 3 produktion   Vestas 850
 ! 3618393  409    Storugn 5 produktion                 Vestas 660
 ! 3618400  410    Storugn 6 produktion                 Vestas 660
 ! 3280112  266    Tornsvalan                           Vestas 600
 ! 3773456         Vannborgaparken 1 produktion         Typ saknas
 ! 3773465         Vannborgaparken 2 produktion         Typ saknas
 ! 3281200  332    Ventosum 10                          Vestas 660
 ! 3281246  325    Ventosum 3                           Vestas 660
 ! 3281219  327    Ventosum 5                           Vestas 660
 ! 3656519  622    Vitåkra produktion                   Vestas 850
 ! 3298924         Kastlösaparken BHM                   Park,Windworld 600
 !          343,294,344,298,300,345,303,304
 ! 3757143         Västraby 5:3 och 6:8, Löberöd        Park,Vestas 2000
 !          790,791,792,793

 !
 ! Koppling mellan stationsnummer och typ av vindkraftverk
 !

 !
 i=1
 windp_stations(i) = windp_type(' Kärrets Gård 2, Långås ',725,6323960,1298190,56.998,12.487,3650962,3,52.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Kärrets Gård           ',393,6323774,1298586,56.997,12.493,3641606,4,47.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Ventosum 10            ',332,6318333,1297750,56.948,12.484,3281200,4,47.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Ventosum 3             ',325,6317813,1298301,56.943,12.493,3281246,4,47.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Ventosum 5             ',327,6317942,1298162,56.944,12.491,3281219,4,47.,0.)
 i=i+1

 windp_stations(i) = windp_type(' Uvered V1              ',707,6472753,1334372,58.347,12.979,      0,0,0.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Uvered V2              ',708,6472911,1334581,58.349,12.982,      0,0,0.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Uvered V3              ',709,6473089,1334790,58.350,12.986,      0,0,0.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Uvered V4              ',710,6473227,1334999,58.352,12.989,      0,0,0.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Uvered V5              ',711,6473386,1335208,58.353,12.993,      0,0,0.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Ryda                   ',797,6466400,1329380,58.288,12.899,      0,0,52.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Frittorp               ',722,6473178,1323594,58.347,12.795,3758936,3,52.,0.)
 i=i+1

 windp_stations(i) = windp_type(' Annevind 1             ',714,6193382,1326463,55.838,13.038,3395007,8,70,0.)
 i=i+1
 windp_stations(i) = windp_type(' Annevind 2             ',715,6194299,1326275,55.846,13.034,3395007,8,70,0.)
 i=i+1

 windp_stations(i) = windp_type(' Bjälbo 2               ',633,6472634,1452036,58.375,14.988,3312105,0,0.,0.)
 i=i+1

 windp_stations(i) = windp_type(' Högby 1                ',602,6471467,1458455,58.365,15.098,3628435,3,52.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Skeby/Tungelunda Vind 2',730,6466312,1446173,58.317,14.890,3629363,3,52.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Skeby/Tungelunda Vind 3',729,6466591,1446076,58.320,14.888,3614618,3,52.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Skeby/Tungelunda Vind 1',728,6466033,1446269,58.315,14.891,3614663,3,52.,0.)
 i=i+1

 windp_stations(i) = windp_type(' Kastlösa verk 1        ',343,6257087,1535356,56.440,16.382,3298924,1,42.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Kastlösa verk 2        ',294,6256000,1535000,56.431,16.376,3298924,1,42.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Kastlösa verk 6        ',344,6257242,1535550,56.442,16.385,3298924,1,42.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Kastlösa verk 7        ',298,6256000,1535000,56.431,16.376,3298924,1,42.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Kastlösa verk 8        ',300,6256000,1535000,56.431,16.376,3298924,1,42.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Kastlösa verk 12       ',345,6257152,1535782,56.441,16.388,3298924,1,42.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Kastlösa verk 13       ',303,6256000,1535000,56.431,16.376,3298924,1,42.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Kastlösa verk 14       ',304,6256000,1535000,56.431,16.376,3298924,1,42.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Kastlösa verk 3        ',295,6256000,1535000,56.431,16.376,3298933,1,42.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Gans Näs               ',536,6333594,1646265,57.106,18.222,      0,0,42.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Kastlösa Verk 9        ',301,6256000,1535000,56.431,16.376,3656662,1,42.,0.)
 i=i+1

 windp_stations(i) = windp_type(' Storugn 5, Lärbro      ',409,6416681,1680203,57.839,18.843,3618393,4,47.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Storugn 6, Lärbro      ',410,6416656,1680535,57.838,18.849,3618400,4,47.,0.)
 i=i+1

 windp_stations(i) = windp_type(' Tornsvalan             ',266,6402371,1677791,57.711,18.792,3280112,5,00.,0.)
 i=i+1

 windp_stations(i) = windp_type(' Bertels, Hablingbo     ',680,6337221,1643348,57.139,18.176,3394348,7,44.,0.)
 i=i+1

 windp_stations(i) = windp_type(' Västraby 1 SV          ',790,6187800,1358150,55.798,13.546,3757143,2,80.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Västraby 2 NV          ',791,6188195,1358153,55.802,13.546,3757143,2,80.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Västraby 3 SO          ',792,6187988,1358411,55.800,13.550,3757143,2,80.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Västraby 4 NO          ',793,6188383,1358414,55.804,13.550,3757143,2,80.,0.)
 i=i+1

 windp_stations(i) = windp_type(' Vitåkra                ',622,6162755,1385118,55.581,13.986,3656519,3,52.,0.)
 i=i+1
 windp_stations(i) = windp_type(' Badene 2               ',736,6438290,1377420,58.052,13.732,3617624,6,52.,0.)

 ! Näsudden
 i=i+1
 windp_stations(i) = windp_type(' Näsudden               ',999,6438290,1377420,58.052,13.732,3617624,6,52.,0.)

 i_max = i
 WRITE(6,*) '                           Wind power station summary'
 WRITE(6,'(3x,A50,X,A10,A5,x,2(A7,x),A7,x,A20)')   &
 'Name','ID','NR','Lat','Lon','Hubhgt','Type'
 DO i=1,i_max

  IF ( .FALSE. ) THEN
  WRITE(6,'(I2,x,A50,X,I10,x,I5,x,2(f7.3,x),f7.1,x,A20)')i, &
  TRIM(windp_stations(i)%desc),             &
       windp_stations(i)%id  ,              &
       windp_stations(i)%nr  ,              &
       windp_stations(i)%lat  ,             &
       windp_stations(i)%lon  ,             &
       windp_stations(i)%hubhgt  ,          &
  TRIM(windtyp_name(windp_stations(i)%typ_ind))
  ENDIF
  WRITE(6,'(I10,X,I3,x,4(f7.3,x),A50)')&
       windp_stations(i)%nr  ,              &
       windp_stations(i)%typ_ind,           &
       windp_stations(i)%lat  ,             &
       windp_stations(i)%lon  ,             &
       windp_stations(i)%hubhgt  ,          &
       windp_stations(i)%hgt  ,             &
  TRIM(windp_stations(i)%desc)

 ENDDO
 WRITE(6,*)

 ! Vertical level definiton

 ! RL05

 ab_half(1,1,:) = (/0.0000000E+00,0.0000000E+00/) 
 ab_half(1,2,:) = (/20.00000,0.0000000E+00/) 
 ab_half(1,3,:) = (/38.42534,0.0000000E+00/) 
 ab_half(1,4,:) = (/63.64780,0.0000000E+00/) 
 ab_half(1,5,:) = (/95.63696,0.0000000E+00/) 
 ab_half(1,6,:) = (/134.4833,0.0000000E+00/) 
 ab_half(1,7,:) = (/180.5844,0.0000000E+00/) 
 ab_half(1,8,:) = (/234.7791,0.0000000E+00/) 
 ab_half(1,9,:) = (/298.4958,0.0000000E+00/) 
 ab_half(1,10,:) = (/373.9719,0.0000000E+00/) 
 ab_half(1,11,:) = (/464.6182,0.0000000E+00/) 
 ab_half(1,12,:) = (/575.6511,0.0000000E+00/) 
 ab_half(1,13,:) = (/713.2180,0.0000000E+00/) 
 ab_half(1,14,:) = (/883.6604,0.0000000E+00/) 
 ab_half(1,15,:) = (/1094.835,0.0000000E+00/) 
 ab_half(1,16,:) = (/1356.475,0.0000000E+00/) 
 ab_half(1,17,:) = (/1680.640,0.0000000E+00/) 
 ab_half(1,18,:) = (/2082.274,0.0000000E+00/) 
 ab_half(1,19,:) = (/2579.889,0.0000000E+00/) 
 ab_half(1,20,:) = (/3196.422,0.0000000E+00/) 
 ab_half(1,21,:) = (/3960.292,0.0000000E+00/) 
 ab_half(1,22,:) = (/4906.707,0.0000000E+00/) 
 ab_half(1,23,:) = (/6018.020,0.0000000E+00/) 
 ab_half(1,24,:) = (/7306.633,0.0000000E+00/) 
 ab_half(1,25,:) = (/8765.055,7.5823496E-05/) 
 ab_half(1,26,:) = (/10376.12,4.6139490E-04/) 
 ab_half(1,27,:) = (/12077.45,1.8151561E-03/) 
 ab_half(1,28,:) = (/13775.32,5.0811172E-03/) 
 ab_half(1,29,:) = (/15379.80,1.1142910E-02/) 
 ab_half(1,30,:) = (/16819.47,2.0677876E-02/) 
 ab_half(1,31,:) = (/18045.18,3.4121163E-02/) 
 ab_half(1,32,:) = (/19027.70,5.1690407E-02/) 
 ab_half(1,33,:) = (/19755.11,7.3533833E-02/) 
 ab_half(1,34,:) = (/20222.20,9.9674702E-02/) 
 ab_half(1,35,:) = (/20429.86,0.1300225/) 
 ab_half(1,36,:) = (/20384.48,0.1643843/) 
 ab_half(1,37,:) = (/20097.40,0.2024759/) 
 ab_half(1,38,:) = (/19584.33,0.2439331/) 
 ab_half(1,39,:) = (/18864.75,0.2883230/) 
 ab_half(1,40,:) = (/17961.36,0.3351549/) 
 ab_half(1,41,:) = (/16899.47,0.3838921/) 
 ab_half(1,42,:) = (/15706.45,0.4339629/) 
 ab_half(1,43,:) = (/14411.12,0.4847715/) 
 ab_half(1,44,:) = (/13043.22,0.5357099/) 
 ab_half(1,45,:) = (/11632.76,0.5861684/) 
 ab_half(1,46,:) = (/10209.50,0.6355475/) 
 ab_half(1,47,:) = (/8802.355,0.6832686/) 
 ab_half(1,48,:) = (/7438.805,0.7287858/) 
 ab_half(1,49,:) = (/6144.316,0.7715966/) 
 ab_half(1,50,:) = (/4941.777,0.8112534/) 
 ab_half(1,51,:) = (/3850.913,0.8473749/) 
 ab_half(1,52,:) = (/2887.697,0.8796569/) 
 ab_half(1,53,:) = (/2063.780,0.9078839/) 
 ab_half(1,54,:) = (/1385.913,0.9319403/) 
 ab_half(1,55,:) = (/855.3618,0.9518215/) 
 ab_half(1,56,:) = (/467.3335,0.9676452/) 
 ab_half(1,57,:) = (/210.3939,0.9796627/) 
 ab_half(1,58,:) = (/65.88924,0.9882701/) 
 ab_half(1,59,:) = (/7.367743,0.9940194/) 
 ab_half(1,60,:) = (/0.0000000E+00,0.9976301/) 
 ab_half(1,61,:) = (/0.0000000E+00,1.000000/) 

 ! E11 F05

 ab_half(2,1,:) = (/0.0000000E+00,0.0000000E+00/) 
 ab_half(2,2,:) = (/2000.064,0.0000000E+00/) 
 ab_half(2,3,:) = (/4033.396,3.7503196E-07/) 
 ab_half(2,4,:) = (/6074.416,2.3632497E-04/) 
 ab_half(2,5,:) = (/8099.623,9.2329504E-04/) 
 ab_half(2,6,:) = (/10087.91,2.2480651E-03/) 
 ab_half(2,7,:) = (/12020.05,4.3811649E-03/) 
 ab_half(2,8,:) = (/13879.08,7.4657723E-03/) 
 ab_half(2,9,:) = (/15649.74,1.1630505E-02/) 
 ab_half(2,10,:) = (/17318.85,1.6978256E-02/) 
 ab_half(2,11,:) = (/18874.84,2.3599014E-02/) 
 ab_half(2,12,:) = (/20308.03,3.1558752E-02/) 
 ab_half(2,13,:) = (/21610.23,4.0912181E-02/) 
 ab_half(2,14,:) = (/22774.96,5.1691636E-02/) 
 ab_half(2,15,:) = (/23797.13,6.3919902E-02/) 
 ab_half(2,16,:) = (/24673.21,7.7599049E-02/) 
 ab_half(2,17,:) = (/25400.90,9.2723131E-02/) 
 ab_half(2,18,:) = (/25979.34,0.1092675/) 
 ab_half(2,19,:) = (/26408.79,0.1272005/) 
 ab_half(2,20,:) = (/26690.75,0.1464744/) 
 ab_half(2,21,:) = (/26827.77,0.1670351/) 
 ab_half(2,22,:) = (/26823.48,0.1888148/) 
 ab_half(2,23,:) = (/26682.39,0.2117409/) 
 ab_half(2,24,:) = (/26410.00,0.2357284/) 
 ab_half(2,25,:) = (/26012.54,0.2606893/) 
 ab_half(2,26,:) = (/25497.09,0.2865254/) 
 ab_half(2,27,:) = (/24871.31,0.3131362/) 
 ab_half(2,28,:) = (/24143.56,0.3404136/) 
 ab_half(2,29,:) = (/23322.65,0.3682483/) 
 ab_half(2,30,:) = (/22417.99,0.3965253/) 
 ab_half(2,31,:) = (/21439.22,0.4251298/) 
 ab_half(2,32,:) = (/20396.45,0.4539433/) 
 ab_half(2,33,:) = (/19299.93,0.4828490/) 
 ab_half(2,34,:) = (/18160.13,0.5117279/) 
 ab_half(2,35,:) = (/16987.58,0.5404649/) 
 ab_half(2,36,:) = (/15792.87,0.5689437/) 
 ab_half(2,37,:) = (/14586.46,0.5970540/) 
 ab_half(2,38,:) = (/13378.69,0.6246860/) 
 ab_half(2,39,:) = (/12179.69,0.6517370/) 
 ab_half(2,40,:) = (/10999.25,0.6781073/) 
 ab_half(2,41,:) = (/9846.779,0.7037051/) 
 ab_half(2,42,:) = (/8731.205,0.7284440/) 
 ab_half(2,43,:) = (/7660.889,0.7522467/) 
 ab_half(2,44,:) = (/6643.533,0.7750434/) 
 ab_half(2,45,:) = (/5686.124,0.7967747/) 
 ab_half(2,46,:) = (/4794.806,0.8173900/) 
 ab_half(2,47,:) = (/3974.842,0.8368518/) 
 ab_half(2,48,:) = (/3230.455,0.8551322/) 
 ab_half(2,49,:) = (/2564.809,0.8722185/) 
 ab_half(2,50,:) = (/1979.889,0.8881090/) 
 ab_half(2,51,:) = (/1476.413,0.9028189/) 
 ab_half(2,52,:) = (/1053.751,0.9163764/) 
 ab_half(2,53,:) = (/709.8160,0.9288278/) 
 ab_half(2,54,:) = (/441.0326,0.9402343/) 
 ab_half(2,55,:) = (/242.1632,0.9506768/) 
 ab_half(2,56,:) = (/106.3017,0.9602531/) 
 ab_half(2,57,:) = (/24.73731,0.9690818/) 
 ab_half(2,58,:) = (/0.0000000E+00,0.9773003/) 
 ab_half(2,59,:) = (/0.0000000E+00,0.9850684/) 
 ab_half(2,60,:) = (/0.0000000E+00,0.9925671/) 
 ab_half(2,61,:) = (/0.0000000E+00,1.000000/) 

 ! al026

 ab_half(3,01,:) = (/ -6.980895996093750E-004 ,0.000000000000000E+000 /)
 ab_half(3,02,:) = (/ 2006.05587387085  ,    0.000000000000000E+000 /)
 ab_half(3,03,:) = (/ 3996.76492691040  ,    0.000000000000000E+000 /)
 ab_half(3,04,:) = (/ 5923.68038558960  ,    7.924735546112061E-004 /)
 ab_half(3,05,:) = (/ 7748.44461441040  ,    3.049686551094055E-003 /)
 ab_half(3,06,:) = (/ 9441.11788558960  ,    7.330864667892456E-003 /)
 ab_half(3,07,:) = (/ 10978.8664894104  ,    1.408702880144119E-002 /)
 ab_half(3,08,:) = (/ 12344.7897605896  ,    2.366895973682404E-002 /)
 ab_half(3,09,:) = (/ 13526.9524269104  ,    3.633181005716324E-002 /)
 ab_half(3,10,:) = (/ 14517.6569480896  ,    5.224297940731049E-002 /)
 ab_half(3,11,:) = (/ 15312.7258644104  ,    7.148683071136475E-002 /)
 ab_half(3,12,:) = (/ 15911.1335105896  ,    9.407258033752441E-002 /)
 ab_half(3,13,:) = (/ 16314.4368019104  ,    0.119938731193542      /)
 ab_half(3,14,:) = (/ 16526.6647605896  ,    0.148961544036865      /)
 ab_half(3,15,:) = (/ 16553.8977394104  ,    0.180958986282349      /)
 ab_half(3,16,:) = (/ 16404.2975730896  ,    0.215699076652527      /)
 ab_half(3,17,:) = (/ 16087.7414894104  ,    0.252904653549194      /)
 ab_half(3,18,:) = (/ 15615.9616355896  ,    0.292260646820068      /)
 ab_half(3,19,:) = (/ 15002.2258644104  ,    0.333419442176819      /)
 ab_half(3,20,:) = (/ 14261.4147605896  ,    0.376008272171021      /)
 ab_half(3,21,:) = (/ 13409.8118019104  ,    0.419634699821472      /)
 ab_half(3,22,:) = (/ 12465.0319480896  ,    0.463892579078674      /)
 ab_half(3,23,:) = (/ 11445.8352394104  ,    0.508369922637939      /)
 ab_half(3,24,:) = (/ 10372.0085105896  ,    0.552652835845947      /)
 ab_half(3,25,:) = (/ 9263.98367691040  ,    0.596334218978882      /)
 ab_half(3,26,:) = (/ 8142.78194808960  ,    0.639018058776855      /)
 ab_half(3,27,:) = (/ 7029.44461441040  ,    0.680326819419861      /)
 ab_half(3,28,:) = (/ 5944.81319808960  ,    0.719907999038696      /)
 ab_half(3,29,:) = (/ 4909.01492691040  ,    0.757438659667969      /)
 ab_half(3,30,:) = (/ 3940.95382308960  ,    0.792634010314941      /)
 ab_half(3,31,:) = (/ 3057.76297378540  ,    0.825252056121826      /)
 ab_half(3,32,:) = (/ 2274.18722152710  ,    0.855100393295288      /)
 ab_half(3,33,:) = (/ 1601.89139175415  ,    0.882042765617371      /)
 ab_half(3,34,:) = (/ 1048.74532699585  ,    0.906004667282104      /)
 ab_half(3,35,:) = (/ 618.015903472900  ,    0.926980376243591      /)
 ab_half(3,36,:) = (/ 307.624721527100  ,    0.945038676261902      /)
 ab_half(3,37,:) = (/ 109.202304840088  ,    0.960329413414001      /)
 ab_half(3,38,:) = (/ 7.26729965209961  ,    0.973089814186096      /)
 ab_half(3,39,:) = (/ 0.000000000000000E+000 ,0.983650326728821      /)
 ab_half(3,40,:) = (/ 0.000000000000000E+000 ,0.992441534996033      /)
 ab_half(3,41,:) = (/ 0.000000000000000E+000 , 1.00000000000000      /)

 ab_half(3,50:61,:) = ab_half(3,30:41,:)  

 init_done = .TRUE.

 RETURN

 END SUBROUTINE init_windp
 SUBROUTINE print_windp_station
 
  IMPLICIT NONE

  INTEGER :: i

  i = windp_index

  WRITE(6,'(I10,X,I3,x,I5,x,4(f7.3,x),A50)')&
       windp_stations(i)%id  ,              &
       windp_stations(i)%typ_ind,           &
       windp_stations(i)%nr  ,              &
       windp_stations(i)%lat  ,             &
       windp_stations(i)%lon  ,             &
       windp_stations(i)%hubhgt  ,          &
       windp_stations(i)%hgt  ,             &
  TRIM(windp_stations(i)%desc)

! WRITE(6,'(I2,x,A50,X,I10,x,I5,x,2(f7.3,x),f7.1,x,A20)')i, &
! TRIM(windp_stations(i)%desc),             &
!      windp_stations(i)%id  ,              &
!      windp_stations(i)%nr  ,              &
!      windp_stations(i)%lat  ,             &
!      windp_stations(i)%lon  ,             &
!      windp_stations(i)%hubhgt  ,          &
! TRIM(windtyp_name(windp_stations(i)%typ_ind))

 END SUBROUTINE print_windp_station
 SUBROUTINE print_windp_type
 
  IMPLICIT NONE

  CHARACTER(LEN=50) :: fname= 'windp_type'
  CHARACTER(LEN=50) :: wrk  = ''
  CHARACTER(LEN=02) :: cwrk = ''

  INTEGER :: i,j

  DO i=1,maxwindp_type
  
     WRITE(cwrk,'(I2.2)')i
     wrk = TRIM(fname)//'_'//cwrk//'.dat'
     OPEN(10,FILE=wrk)

     WRITE(10,'(A50)')windtyp_name(i)
     DO j=1,50
        WRITE(10,*)j,wind_map(i,j,1)
     ENDDO
     CLOSE(10)

  ENDDO

 END SUBROUTINE print_windp_type

 END MODULE windp
