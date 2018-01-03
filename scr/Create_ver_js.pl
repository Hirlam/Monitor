#!/usr/bin/perl

 # Create input files for WebgraF based on the 
 # definitions in Env_exp
 #

 $TYPE=$ARGV[0]  or die "Please give SURF or TEMP as argument \n";

 use plotdefs ;
 use maindefs ;
 use skilldefs ;

 # Add local deviations 
 eval { require $ENV{LOCALDEFS}; } if $ENV{LOCALDEFS} ;

 $WEBCALL=$ENV{WEBCALL} ;

 #
 # Define which things to plot
 #

 # File extentions
 @ext = ('','png','1.png','1.jpg','svg');
 $EXT = $ext[$ENV{OUTPUT_TYPE}] ;

 # Experiment

 $exp = $ENV{DISPLAY_EXP} or $exp = $ENV{EXP} ;
 my @exp=split(' ',$exp);
 $nexp = scalar(@exp) ;
 $expname ='\''.join('\',\'',@exp).'\'';
 $nlev = 0 ; 
 $lev_lst = 0;

 # Selection
 @selections = split(' ',$ENV{$TYPE.'SELECTION'});
 $selections ='\''.join('\',\'',@selections).'\'';

 # Initial hours
 @ini_hours = split(' ',$ENV{$TYPE.'INI_HOURS'});
 $ini_hours ='\''.join('\',\'',@ini_hours).'\'';

 # Time handling
 $period_type = $ENV{PERIOD_TYPE}; 
 $period_freq = $ENV{PERIOD_FREQ} or $period_freq = 1 ;
 @period_v = ('','\'00000000\'',
              '\'gen_date\',\'00YYYYMM\',\'00'.$ENV{EDATE}.'\',\'00'.$ENV{IDATE}.'\'',
              '\'month_num\',1,'.12/$period_freq.',1,7') ;
 if ( $period_freq eq 3 ) {
   @period_t = ('','\'00000000\'','\'gen_date\',\'YYYYMM\',\''.$ENV{EDATE}.'\',\''.$ENV{IDATE}.'\'',
              '\'season_txt\',1,'.12/$period_freq.',1') ;
 } else {
   @period_t = ('','\'00000000\'','\'gen_date\',\'YYYYMM\',\''.$ENV{EDATE}.'\',\''.$ENV{IDATE}.'\'',
              '\'month_txt\',1,'.12/$period_freq.','.$period_freq) ;
 } 

 # Stations
 if ( $ENV{STNLIST_PLOT} ne -1 ) {
    @stations = split(',',$ENV{STNLIST_PLOT});
    @stations_txt = split(',',$ENV{STNLIST_PLOT_TXT});
 } ;
 @stations=('00000000',@stations) ;
 @stations_txt=('ALL',@stations_txt) ;
 $stations     ='\''.join('\',\'',@stations).'\'';
 $stations_txt ='\''.join('\',\'',@stations_txt).'\'';
 
 # Download button
 $download="do_debug = false ;" ;
 if ( $ENV{OUTPUT_TYPE} eq 1 ) { $download="$download download=['ps']" ; } ;
 

 if ( $TYPE =~/SURF/ ) {

    #
    # Surface 
    #

    @plots = ();

    @plots = split(' ',$ENV{SURFPLOT}) ;
    foreach ( @plots ) { $plots{$_} = 1 ; } ;

    $type ="Surface" ;
    $pdir = $type ;

    @par=split(' ',$ENV{SURFPAR});
    $partext='\''.join('\',\'',@par).'\'';
    $npar = scalar(@par) -1 ;

    @text = ();
    foreach $par ( @par ) { unless ( exists $plotdefs{$par}{'TEXT'} ) { $plotdefs{$par}{'TEXT'} = $par ; } ; } ;
    foreach $par ( @par ) { @text = (@text,$plotdefs{$par}{'TEXT'}) ; } ;
    $text='\''.join('\',\'',@text).'\'';

    if ( exists $plots{'CONT'} ) {

       @skill_par  = ();
       @skill_text = ();

       foreach $par ( @par ) {
 
          if ( exists $plotdefs{$par}{'CONT_CLASS'} ) {
             @skill_par  = (@skill_par,$par) ;
             @skill_text = (@skill_text,$plotdefs{$par}{'TEXT'}) ; } ;
          } ;
         $skill_partext='\''.join('\',\'',@skill_par).'\'';
         $skill_text='\''.join('\',\'',@skill_text).'\'';
        
         $nskill = scalar (@skill_par) ;

         delete $plots{'CONT'} if ( $nskill == 0 ) ;

    } ;

    &finalize_plot ;

} else {

    #
    # Temp
    #

    @plots = ();
    
    @plots = split(' ',$ENV{TEMPPLOT}) ;
    foreach ( @plots ) { $plots{$_} = 1 ; } ;

    $type ="Temp" ;
    $pdir = $type ;

    @lev = split(' ',$ENV{LEV_LST});
    $nlev = scalar(@lev) -1 ;
    $lev_lst ='\''.join('\',\'',@lev).'\'';

    @par=split(' ',$ENV{TEMPPAR});
    $partext='\''.join('\',\'',@par).'\'';
    $npar = scalar(@par) -1 ;

    @text = ();
    foreach $par ( @par ) { 
        if ( exists($plotdefs{$par}{'TEXT_TEMP'}) ) {
          @text = (@text,$plotdefs{$par}{'TEXT_TEMP'}) ; 
        } else {
          @text = (@text,$plotdefs{$par}{'TEXT'}) ; 
        } ;
    } ;
    $text='\''.join('\',\'',@text).'\'';

    &finalize_plot ;

};

##################################
##################################
##################################
sub profile {
#
# Vert
#

if ( exists $selectionloop{'VERT'}{'SHOW_TIMES'} ) {
 $prof_hours ='\''.join('\',\'',split(',',$selectionloop{'VERT'}{'SHOW_TIMES'})).'\'';
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
v[2] = [$stations]
t[2] = [$stations_txt]
v[3] =[$selections] ;
t[3] = v[3] ;
v[4] =[$prof_hours] ;
t[4] =v[4]
v[5] = [$partext]
t[5] = [$text]
v[6] =[0] ;
t[6] =[0] ;
v[7] =[$ini_hours] ;
t[7] = v[7] ;
mname = ['Type','Period','Station','Selection','Time (UTC)','Parameter','Dum','Initial time']
spec_name =[0,1,2,3,7,4,5,6]

loc = ['l','t','t','t','l','l','l','t']
$download

pdir ='Temp_prof/'
ext='$EXT'
help = '$ENV{HELP}'; hide_help = false ;
do_send = true ;
do_show_remember = true ;
do_remember = true ;
$xml
$xml_txt

" ;

close INPUT ;
$web="$WEBCALL -e ${type}_prof -f input.js";
system($web);

} ;

##################################
##################################
##################################
sub map {

 #
 # Bias maps
 #

 if ( exists $selectionloop{'MAP'}{'SHOW_TIMES'} ) {
    $map_hours ='\''.join('\',\'',split(',',$selectionloop{'MAP'}{'SHOW_TIMES'})).'\'';
 } else {
    $map_hours = '\'ALL\'';
 }; 

 my @mp  = ();
 my @mpt = ();

 if ( exists $selectionloop{'MAP'}{'PLOT_MABE_MAP'} ) {
  if ( $selectionloop{'MAP'}{'PLOT_MABE_MAP'} eq 'T' ) {
    @mp  = (@mp ,'a'   );
    @mpt = (@mpt,'Mae');
  } ;
 } ;

 if ( exists $selectionloop{'MAP'}{'PLOT_BIAS_MAP'} ) {
  if ( $selectionloop{'MAP'}{'PLOT_BIAS_MAP'} eq 'T' ) {
    @mp  = (@mp ,'b'   );
    @mpt = (@mpt,'Bias');
  } ;
 } ;

 if ( exists $selectionloop{'MAP'}{'PLOT_RMSE_MAP'} ) {
  if ( $selectionloop{'MAP'}{'PLOT_RMSE_MAP'} eq 'T' ) {
    @mp  = (@mp ,'r'   );
    @mpt = (@mpt,'Rmse');
  } ;
 } ;

 if ( exists $selectionloop{'MAP'}{'PLOT_STDV_MAP'} ) {
  if ( $selectionloop{'MAP'}{'PLOT_STDV_MAP'} eq 'T' ) {
    @mp  = (@mp ,'s'   );
    @mpt = (@mpt,'Stdv');
  } ;
 } ;

 $mp  ='\''.join('\',\'',@mp ).'\'';
 $mpt ='\''.join('\',\'',@mpt).'\'';

 if ( $selectionloop{'MAP'}{'LFCVER'} eq 'T' ) {
    $map_prefix ='\'M\'' ;
    $map_hour_title ='\'Fclen\'' ;
 } else {
    $map_prefix ='\'m\'' ;
    $map_hour_title ='\'Hour\'' ;
 } ;

 open INPUT, "> input.js" ;
 print INPUT "
// Input file

title = '$type maps'

framec='Teal'

v[0] = [$map_prefix]
t[0] = v[0]
v[1] = [$period_v[$period_type]]
t[1] = [$period_t[$period_type]]
v[2] = ['00000000']
t[2] = ['ALL']
v[3] =[$selections] ;
t[3] = v[3] ;
v[4] = [$partext]
t[4] = [$text]
v[5] =[$lev_lst]
v[5] =v[5].reverse()
t[5] =v[5]
v[6] =[$expname]
t[6] =v[6]
v[7] =[$map_hours] ;
t[7] = v[7] ;
v[8] = [$mp];
t[8] = [$mpt];
v[9] =[$ini_hours] ;
t[9] = v[9] ;

spec_name =[0,8,1,2,3,9,7,6,4,5]

mname = ['Type','Period','Station','Selection','Parameter','Level','Exp',$map_hour_title,'Error','Initial time']
loc =['l','l','t','t','t','l','l','l','l','l','l']
$download
pdir ='$pdir/'
ext='$EXT'
help = '$ENV{HELP}'; hide_help = false ;
do_send = true ;
do_show_remember = true ;
do_remember = true ;
$xml_txt
$xml

" ;
close INPUT ;

$web="$WEBCALL -e ${type}_map -f input.js";
print "$web\n";
system($web);

} ;
##################################
##################################
##################################
sub cont {

 unless ( $ENV{SCORELIST} ) { return ; } ;

 @ssd = ();
 @sst = ();

 @SS = split(" ",$ENV{SCORELIST}) ;
 @ST = split(" ",$ENV{SCORETYPES}) ;

 foreach $S (@SS) {
 @ssd = (@ssd,$skill_score_def{$S}.'c');
 @sst = (@sst,$skill_score_txt{$S});
 }
 $sst='\''.join('\',\'',@sst).'\'';
 $ssd='\''.join('\',\'',@ssd).'\'';

 $stt='\''.join('\',\'',@ST).'\'';

 open INPUT, "> input.js" ;
 print INPUT "
// Input file

title = '$type skill scores'

framec='RoyalBlue'

v[0] = [$ssd]
t[0] = [$sst]
v[1] = [$period_v[$period_type]]
t[1] = [$period_t[$period_type]]
v[2] = ['00000000']
t[2] = ['ALL']
v[3] =[$selections] ;
t[3] = v[3] ;
v[4] = [$skill_partext]
t[4] = [$skill_text]
v[5] =[$lev_lst]
v[5] =v[5].reverse()
t[5] =v[5]
v[6] =[$stt]
t[6] =v[6]
v[7] =[$ini_hours] ;
t[7] = v[7] ;

spec_name =[0,1,2,3,7,4,5,6]

mname = ['Score','Period','Station','Selection','Parameter','Level','Type','Initial time']
loc =['t','l','t','t','l','l','l','l']
$download
pdir ='$pdir/'
ext='$EXT'
help = '$ENV{HELP}'; hide_help = false ;
do_send = true ;
do_show_remember = true ;
do_remember = true ;
$xml_cont_txt
$xml_cont

" ;
close INPUT ;

$web="$WEBCALL -e ${type}_skill -f input.js";
print "$web\n";
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
v[2] = [$stations]
t[2] = [$stations_txt]
v[3] = [$selections] ;
t[3] = v[3] ;
v[4] = [$partext];
t[4] = [$text]
v[5] = [$lev_lst] ;
v[5] = v[5].reverse()
t[5] = v[5]
v[6] = [$ini_hours] ;
t[6] = v[6] ;
mname = ['Type','Period','Station','Selection','Parameter','Level','Initial time']
loc = ['l','l','t','t','t','l','l']
spec_name =[0,1,2,3,6,4,5]

$download ;

help = '$ENV{HELP}'; hide_help = false ;
pdir ='$type/'
ext='$EXT'
do_send = true ;
do_show_remember = true ;
do_remember = true ;
$xml_txt
$xml
";


close INPUT ;

$web="$WEBCALL -e $type -f input.js";
print "$web\n";
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
v[2] = [$stations]
t[2] = [$stations_txt]
v[3] = [$selections] ;
t[3] = v[3] ;
v[4] = [$partext]
t[4] = [$text]
v[5] = [$lev_lst]
v[5] = v[5].reverse()
t[5] = v[5]
v[6] = [$expname]
t[6] = [$expname]
v[7] = [$ini_hours] ;
t[7] = v[7] ;

mname = ['Type','Period','Station','Selection','Parameter','Level','Exp','Initial time']
loc = ['l','t','t','t','l','l','t']
spec_name =[0,1,2,3,7,6,4,5]
$download
pdir ='$type/'
ext='$EXT'
help = '$ENV{HELP}'; hide_help = false ;
do_send = true ;
do_show_remember = true ;
do_remember = true ;
$xml_txt
$xml
" ;

close INPUT ;

$web="$WEBCALL -e ${type}_scat -f input.js";
print "$web\n";
system($web);

} ;
##################################
##################################
##################################
sub sign {
#
# Significance test definition file
#

 $refexp =shift @exp ;
 foreach (@exp) {
    $tmp =$refexp."_".$_;
    @tmp =(@tmp,$tmp);
 } ;

$sexpname ='\''.join('\',\'',@tmp).'\'';

open INPUT, "> input.js" ;

print INPUT "

title = '$type significance test'

framec='OliveDrab'

v[0] = ['sign']
t[0] = ['Significance']
v[1] = [$period_v[$period_type]]
t[1] = [$period_t[$period_type]]
v[2] = [$stations]
t[2] = [$stations_txt]
v[3] = [$selections] ;
t[3] = v[3] ;
v[4] = [$partext]
t[4] = [$text]
v[5] = [$lev_lst]
v[5] = v[5].reverse()
t[5] = v[5]
v[6] = [$sexpname]
t[6] = [$sexpname]
v[7] = [$ini_hours] ;
t[7] = v[7] ;

mname = ['Type','Period','Station','Selection','Parameter','Level','Exp','Initial time']
loc = ['l','l','t','t','l','l','t','l']
spec_name =[0,1,2,3,7,6,4,5]
$download
pdir ='$type/'
ext='$EXT'
help = '$ENV{HELP}'; hide_help = false ;
do_send = true ;
do_show_remember = true ;
do_remember = true ;
$xml_txt
$xml
" ;

close INPUT ;

$web="$WEBCALL -e ${type}_sign -f input.js";
print "$web\n";
system($web);

} ;
##################################
##################################
##################################
sub finalize_plot {

 # Create text for general page (Surface/Temp)
 # based on users choice

 @plottype     =();
 @plottype_txt =();

 if ( exists $plots{'GEN'} ) {

   @plottype     = (@plottype,'V');
   @plottype_txt = (@plottype_txt,'Fc length ver') ; 
 
   if ( exists $selectionloop{'GEN'}{'SHOW_VAR'} ) {
      if ( $selectionloop{'GEN'}{'SHOW_VAR'} eq 'T' ) {
	 @plottype     = (@plottype,'V_VAR',);
	 @plottype_txt = (@plottype_txt,'Fc length var') ; 
     };
   } ;
   if ( exists $selectionloop{'GEN'}{'SHOW_SKW'} ) {
      if ( $selectionloop{'GEN'}{'SHOW_SKW'} eq 'T' ) {
	 @plottype     = (@plottype,'V_SKW');
	 @plottype_txt = (@plottype_txt,'Fc length skw') ; 
     };
   } ;

 } ;

 if ( exists $plots{'SEAS'} ) {
	 @plottype     = (@plottype,'Y');
	 @plottype_txt = (@plottype_txt,'Seasonal') ; } ;
 if ( exists $plots{'TIME'} ) { 
	 @plottype     = (@plottype,'PS','ps');
	 @plottype_txt = (@plottype_txt,'Timeserie stat','Timeserie'); } ;
 if ( exists $plots{'FREQ'} ) { 
	 @plottype     = (@plottype,'f') ;
	 @plottype_txt = (@plottype_txt,'Freq dist.') ; } ;
 if ( exists $plots{'DAYVAR'} ) {
	 @plottype     = (@plottype,'v') ;
	 @plottype_txt = (@plottype_txt,'Dayvar') ;

   if ( exists $selectionloop{'DAYVAR'}{'SHOW_VAR'} ) {
      if ( $selectionloop{'DAYVAR'}{'SHOW_VAR'} eq 'T' ) {
	 @plottype     = (@plottype,'v_VAR',);
	 @plottype_txt = (@plottype_txt,'Dayvar var') ; 
     };
   } ;
   if ( exists $selectionloop{'DAYVAR'}{'SHOW_SKW'} ) {
      if ( $selectionloop{'DAYVAR'}{'SHOW_SKW'} eq 'T' ) {
	 @plottype     = (@plottype,'v_SKW');
	 @plottype_txt = (@plottype_txt,'Dayvar skw') ; 
     };
   } ;

 } ;
 # Build xml text

 if ( exists $plots{'XML'} ) { 
   @xml  = (@xml,"$pdir/[4]_[1].xml");
   @xml_txt = ('Stations');
 } ;

 if ( exists $plots{'GEN'} ) {
    @xml     = ("$pdir/TABLE_LL_[3].html",@xml);
    @xml_txt = ("Stat".$_,@xml_txt);
 };
 if ( exists $plots{'SEAS'} ) {
    @xml     = ("$pdir/TABL_SEAS_[3].html",@xml);
    @xml_txt = ("Seasonal".$_,@xml_txt);
 };

@xml     = ("$pdir/quality.html",@xml) ;
@xml_txt = ("Quality control",@xml_txt);

if ( exists $plots{'CONT'} ) {
    @xml_cont     = (@xml,"$pdir/c_[1]_00000000_[3]_[7]_[4]_0.html");
    @xml_cont_txt = (@xml_txt,"Cont");
};

@xml     = ('All',@xml);
@xml_txt = ('Graphics',@xml_txt);
@xml_cont     = ('All',@xml_cont);
@xml_cont_txt = ('Graphics',@xml_cont_txt);

$xml     ='my_con=[\''.join('\',\'',@xml).'\']';
$xml_txt ='my_con_txt=[\''.join('\',\'',@xml_txt).'\']';

$xml_cont     ='my_con=[\''.join('\',\'',@xml_cont).'\']';
$xml_cont_txt ='my_con_txt=[\''.join('\',\'',@xml_cont_txt).'\']';

$plottype     ='\''.join('\',\'',@plottype).'\'';
$plottype_txt ='\''.join('\',\'',@plottype_txt).'\'';

if (( exists $plots{'TIME'}   ) ||
    ( exists $plots{'DAYVAR'} ) ||
    ( exists $plots{'FREQ'}   ) ||
    ( exists $plots{'SEAS'}   ) ||
    ( exists $plots{'GEN'}    )    ) { &gen_stat ; } ;

if ( exists $plots{'SCAT'} ) { &scatter ; } ;
if ( exists $plots{'MAP'}  ) { &map ;     }; 
if ( exists $plots{'CONT'} ) { &cont ;    }; 

if ( exists $selectionloop{'GEN'}{'LSIGN_TEST'} ) {
   if ( exists $plots{'GEN'} && 
        $nexp gt 1           &&  
        $selectionloop{'GEN'}{'LSIGN_TEST'} eq 'T' ) { &sign ; } ;
}

# Vertical profiles
if ( $TYPE eq 'TEMP' && exists $plots{'VERT'} ) { &profile ; };

}
