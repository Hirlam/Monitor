#!/usr/bin/perl

 # Create input files for WebgraF based on the 
 # definitions in Env_exp
 #

 $TYPE=$ARGV[0]  or die "Please give SURF or TEMP as argument \n";

 use plotdefs ;
 use maindefs ;

 #
 # Define which things to plot
 #

 @plots = split(' ',$ENV{SURFPLOT}) ;
 foreach ( @plots ) { $plots{$_} = 1 ; } ;

 # File extentions
 @ext = ('','png','1.png','1.jpg');
 $EXT = $ext[$ENV{OUTPUT_TYPE}] ;

 # Experiment

 @exp=split(' ',$ENV{EXP});
 $nexp = scalar(@exp) -1 ;
 $expname ='\''.join('\',\'',@exp).'\'';
 $nlev = 0 ; 
 $lev_lst = 0;

 # Area
 @areas = split(' ',$ENV{$TYPE.'AREAS'});
 $areas ='\''.join('\',\'',@areas).'\'';

 # Time handling
 $period_type = $ENV{PERIOD_TYPE}; 
 @period_v = ('','\'00000000\'','\'gen_date\',\'00YYYYMM\',\'00'.$ENV{EDATE}.'\',\'00'.$ENV{IDATE}.'\'');
 @period_t = ('','\'00000000\'','\'gen_date\',\'YYYYMM\',\''.$ENV{EDATE}.'\',\''.$ENV{IDATE}.'\'');
 
 # Download button
 $download="do_debug = false ;" ;
 if ( $ENV{OUTPUT_TYPE} eq 1 ) { $download="$download download=['ps']" ; } ;
 

 if ( $TYPE =~/SURF/ ) {

 #
 # Surface 
 #

 $type ="Surface" ;
 $pdir = $type ;

 @par=split(' ',$ENV{SURFPAR});
 $partext='\''.join('\',\'',@par).'\'';
 $npar = scalar(@par) -1 ;

 @text = ();
 foreach $par ( @par ) { @text = (@text,$plotdefs{$par}{'TEXT'}) ; } ;
 $text='\''.join('\',\'',@text).'\'';



 # Create text for general page (Surface/Temp)
 # based on users choice

 @plottype     =();
 @plottype_txt =();

 if ( exists $plots{'GEN'} ) {
	 @plottype     = (@plottype,'V');
	 @plottype_txt = (@plottype_txt,'Fc length ver') ; } ;
 if ( exists $plots{'TIME'} ) { 
	 @plottype     = (@plottype,'PS','ps');
	 @plottype_txt = (@plottype_txt,'Timeserie stat','Timeserie'); } ;
 if ( exists $plots{'FREQ'} ) { 
	 @plottype     = (@plottype,'f') ;
	 @plottype_txt = (@plottype_txt,'Freq dist.') ; } ;
 if ( exists $plots{'DAYVAR'} ) {
	 @plottype     = (@plottype,'v') ;
	 @plottype_txt = (@plottype_txt,'Dayvar') ; } ;

 # Build xml text
 @xml     = ();
 @xml_txt = ();

 if ( exists $plots{'XML'} ) { 
   @xml  = (@xml,'../Surface/[4]_[1].xml');
   @xml_txt = ('Stations');
 } ;

 if ( exists $plots{'CONT'} ) {
    @xml     = ('../Surface/contingency_[3]_[1]_[4].html',@xml);
    @xml_txt = ('Cont '.$_,@xml_txt);
 };
 if ( exists $plots{'GEN'} ) {
    @xml     = ('../Surface/SURF_LL_[3].html',@xml);
    @xml_txt = ('Stat '.$_,@xml_txt);
 };

@xml     = ('../Surface/quality.html',@xml) ;
@xml_txt = ('Quality control',@xml_txt);

$xml     ='my_xml=[\''.join('\',\'',@xml).'\']';
$xml_txt ='my_xml_txt=[\''.join('\',\'',@xml_txt).'\']';

$plottype     ='\''.join('\',\'',@plottype).'\'';
$plottype_txt ='\''.join('\',\'',@plottype_txt).'\'';

if (( exists $plots{'TIME'}   ) ||
    ( exists $plots{'DAYVAR'} ) ||
    ( exists $plots{'FREQ'}   ) ||
    ( exists $plots{'GEN'}    )    ) { &gen_stat ; } ;

if ( exists $plots{'SCAT'} ) { &scatter ; } ;
if ( exists $plots{'MAP'}  ) { &map ;     }; 

} else {

 #
 # Define which things to plot for temp
 #

 %plots = ();

 @plots = split(' ',$ENV{TEMPPLOT}) ;
 foreach ( @plots ) { $plots{$_} = 1 ; } ;

 # 
 # Temp
 # 

 $type ="Temp" ;
 $pdir = $type ;
 @text = ();

 @lev = split(' ',$ENV{LEV_LST});
 $nlev = scalar(@lev) -1 ;
 $lev_lst ='\''.join('\',\'',@lev).'\'';

 @par=split(' ',$ENV{TEMPPAR});
 $partext='\''.join('\',\'',@par).'\'';
 $npar = scalar(@par) -1 ;
 foreach $par ( @par ) { @text = (@text,$plotdefs{$par}{'TEXT_TEMP'}) ; } ;
 $text='\''.join('\',\'',@text).'\'';

 @plottype     =();
 @plottype_txt =();

 if ( exists $plots{'GEN'} ) { 
	@plottype =(@plottype,'V');
	@plottype_txt =(@plottype_txt,'Fc length ver') ; } ;
 if ( exists $plots{'TIME'} ) {
	@plottype =(@plottype,'PS','ps');
    @plottype_txt =(@plottype_txt,'Timeserie stat','Timeserie'); } ;
 if ( exists $plots{'FREQ'} ) { 
	@plottype =(@plottype,'F') ;
	@plottype_txt =(@plottype_txt,'Freq dist.') ; } ;
 if ( exists $plots{'DAYVAR'} ) { 
	@plottype =(@plottype,'v') ;
	@plottype_txt =(@plottype_txt,'Dayvar') ; } ;

 @xml     = ();
 @xml_txt = ();

 if ( exists $plots{'XML'} ) { 
   @xml  = (@xml,'../Prof_Temp/[4]_[1].xml');
   @xml_txt = ('Stations');
 } ;

 if ( exists $plots{'CONT'} ) {
   @xml     = ('../Prof_Temp/contingency_[3].html',@xml);
   @xml_txt = ('Cont '.$_,@xml_txt);
 };
 if ( exists $plots{'GEN'} ) {
   @xml     = ('../Prof_Temp/TEMP_LL_[3].html',@xml);
   @xml_txt = ('Stat '.$_,@xml_txt);
 };

@xml     = ('../Prof_Temp/quality.html',@xml) ;
@xml_txt = ('Quality control',@xml_txt);

$xml     ='my_xml=[\''.join('\',\'',@xml).'\']';
$xml_txt ='my_xml_txt=[\''.join('\',\'',@xml_txt).'\']';

$plottype     ='\''.join('\',\'',@plottype).'\'';
$plottype_txt ='\''.join('\',\'',@plottype_txt).'\'';

if (( exists $plots{'TIME'}   ) ||
    ( exists $plots{'DAYVAR'} ) ||
    ( exists $plots{'FREQ'}   ) ||
    ( exists $plots{'GEN'}    )    ) { &gen_stat ; } ;

if ( exists $plots{'SCAT'} ) { &scatter ;     } ;
if ( exists $plots{'MAP'}  ) { &map ;         }; 
if ( exists $plots{'VERT'} ) { &profile ;     }; 

};

##################################
##################################
##################################
sub profile {
#
# Vert
#

if ( exists $arealoop{'VERT'}{'SHOW_TIMES'} ) {
 $prof_hours ='\''.join('\',\'',split(',',$arealoop{'VERT'}{'SHOW_TIMES'})).'\'';
} else {
$prof_hours = '\'ALL\'';
 }; 


open INPUT, "> input.js" ;
print INPUT "
// Input file

title = 'Vertical profiles'
framec='Tomato'

v[0] = ['l']
t[0] = ['diff']
v[1] = [$period_v[$period_type]]
t[1] = [$period_t[$period_type]]
v[2] = ['00000000']
t[2] = v[2] ;
v[3] =[$areas] ;
t[3] = v[3] ;
v[4] =[$prof_hours] ;
t[4] =v[4]
v[5] = [$partext]
t[5] = [$text]
v[6] =[0] ;
t[6] =[0] ;
mname = ['Type','Period','Station','Area','Time (UTC)','Parameter','Dum']

loc = ['l','t','l','t','l','l','l']
$download

pdir ='Prof_Temp/'
ext='$EXT'
help = '$ENV{HELP}'; hide_help = false ;
do_send = true ;
$xml
$xml_txt

" ;

close INPUT ;
$web='$WEBCALL -e Prof_'.$type.' -f input.js';
system($web);

} ;

##################################
##################################
##################################
sub map {

 #
 # Bias maps
 #

 open INPUT, "> input.js" ;
 print INPUT "
// Input file

title = '$type bias maps'

framec='Teal'

v[0] = ['M']
t[0] = ['By time of day']
v[1] = [$period_v[$period_type]]
t[1] = [$period_t[$period_type]]
v[2] = ['00000000']
t[2] = v[2] ;
v[3] =[$areas] ;
t[3] = v[3] ;
v[4] = [$partext]
t[4] = [$text]
v[5] =[$lev_lst]
v[5] =v[5].reverse()
t[5] =v[5]
v[6] =[$expname]
t[6] =v[6]
v[7] =['ALL'] ;
t[7] = v[7] ;
v[8] = ['b','r']
t[8] = ['Bias','Rmse']

spec_name =[0,8,1,2,3,7,6,4,5]

mname = ['Type','Period','Station','Area','Parameter','Level','Exp','Hour','Error']
loc =['l','l','t','l','t','l','l','t']
$download
pdir ='$pdir/'
ext='$EXT'
help = '${HELP}'; hide_help = false ;
do_send = true ;
$xml_txt
$xml

" ;
close INPUT ;

$web='$WEBCALL -e Map_'.$type.' -f input.js';
system($web);

} ;
##################################
##################################
##################################
sub gen_stat {
#
# Gen statistics definition file
#

open INPUT, "> input.js" ;
print INPUT "

title = '$type verification' 

framec='Goldenrod'

v[0] = [$plottype];
t[0] = [$plottype_txt];
v[1] = [$period_v[$period_type]]
t[1] = [$period_t[$period_type]]
v[2] = ['00000000']
t[2] = v[2]
v[3] =[$areas] ;
t[3] = v[3] ;
v[4] = [$partext];
t[4] = [$text]
v[5] =[$lev_lst] ;
v[5] = v[5].reverse()
t[5] = v[5]
loc = ['l','t','l','t','l','t']

$download ;

mname = ['Type','Period','Station','Area','Parameter','Level']
help = '$ENV{HELP}'; hide_help = false ;
pdir ='$type/'
ext='$EXT'
do_send = true ;
$xml_txt
$xml
";


close INPUT ;

$web='$WEBCALL -e '.$type.' -f input.js';
system($web);

} ;
##################################
##################################
##################################
sub scatter {
#
# Scatter definition file
#

open INPUT, "> input.js" ;

print INPUT "

title = '$type scatterplots'

framec='lightcoral'

v[0] = ['s']
t[0] = ['Full scatter']
v[1] = [$period_v[$period_type]]
t[1] = [$period_t[$period_type]]
v[2] = ['00000000']
t[2] = v[2] ;
v[3] =  [$areas] ;
t[3] = v[3] ;
v[4] = [$partext]
t[4] = [$text]
v[5] = [$lev_lst]
v[5] = v[5].reverse()
t[5] = v[5]
v[6] = [$expname]
t[6] = [$expname]

mname = ['Type','Period','Station','Area','Parameter','Level','Exp']
loc = ['l','t','l','t','l','l','t']
spec_name =[0,1,2,3,6,4,5]
$download
pdir ='$type/'
ext='$EXT'
help = '$ENV{HELP}'; hide_help = false ;
do_send = true ;
$xml_txt
$xml
" ;

close INPUT ;

$web='$WEBCALL -e '.$type.'_scat -f input.js';
system($web);

} ;