#!/usr/bin/perl

 #
 # Build namelist for verification
 #
 # Usage: Build_namelist.pl -t TYPE -p INPAR
 # where :
 # TYPE is SURF or TEMP
 # INPAR is any parameter or parameters such as "NN" or "FF DD NN"
 #
 # Main plotting definitions are defined in maindefs.pm
 # Parameter definitions are defined in plotdefs.pm
 # Different areas are defined in areas.pm
 #
 # Ulf Andrae, SMHI, 2008
 #

 use areas ;
 use plotdefs ;
 use maindefs ;

 unless ( @ARGV ) { die "Please give SURF or TEMP as argument \n" ; } ;

 $n=0;

 while ( <@ARGV> ) {
   if ( /-t/) { $type   = $ARGV[$n+1] or die "Please use -t TYPE    \n"; } ;
   if ( /-p/) { $inpar  = $ARGV[$n+1] or die "Please use -p PAR(S)  \n"; } ;
   $n++ ;
 }

 unless ( $type ) { die "Please give -t TYPE where type is SURF or TEMP \n"; } ;

 #
 # Define which things to plot
 #

 @plots = split(' ',$ENV{$type.'PLOT'}) or die "${type}PLOT not defined \n";
 foreach ( @plots ) { $plots{$_} = 1 ; } ;
 for my $key ( keys %plots ) {
   my $value = $plots{$key};
 } ;

 #
 # Define experiments, obs and modelpath
 #

 @exp     = split(' ',$ENV{EXP}) ;
 @modpath = split(' ',$ENV{MODPATH}) ;
 $exp     = '\''.join('\',\'',@exp).'\'';
 $modpath = '\''.join('\',\'',@modpath).'\'';
 $obint   = $ENV{'OBINT_'.$type};

 $nameread{'read_section'}{'NEXP'}   =scalar(@exp) ;
 $nameread{'read_section'}{'EXPNAME'}=$exp;
 $nameread{'read_section'}{'MODPATH'}=$modpath ;
 $nameread{'read_section'}{'OBINT'}=$obint;
 $nameread{'read_section'}{'FCINT'}=$ENV{'FCINT_'.$type};
 $nameread{'read_section'}{'FCLEN'}=join(',',split(' ',$ENV{'FCLEN_'.$type}));

 #
 # Define variables
 #

 $i=0;
 $j=0;
 %tmp =();

 #
 # Define TEMP specific things
 #

 if ( $type =~ /TEMP/ ) {

    #
    # Define TEMP specific things
    #

    if ( $ENV{ALL_AT_ONCE} eq "no" && defined $ENV{"LEV_LST_$inpar"} ) {
       @lev = split(' ',$ENV{"LEV_LST_$inpar"});
    } else {
       @lev = split(' ',$ENV{LEV_LST});
    }

    $nlev = scalar(@lev);
    $nameread{'read_section'}{'LEV_LST'} = join(',',@lev);
    $nameread{'read_section'}{'LTEMP'} = 'T';
    if ( $ENV{DATA_SOURCE} ) {
       $nameread{'read_section'}{'DATA_SOURCE'} = '\''.$ENV{DATA_SOURCE}.'\''; 
    } else {
       $nameread{'read_section'}{'DATA_SOURCE'} = '\'vfld_temp\'';
    };

    $arealoop{'TIME'}{'USE_FCLEN'} = join(',',split(' ',$ENV{FCLEN_TEMP_TIME})) ;

 } else {

    #
    # Define SURF specific things
    #

    $nlev = 1;

    if ( $ENV{DATA_SOURCE} ) {
       $nameread{'read_section'}{'DATA_SOURCE'} = '\''.$ENV{DATA_SOURCE}.'\''; 
    } else {
       $nameread{'read_section'}{'DATA_SOURCE'} = '\'vfld\'';
    };

    $arealoop{'TIME'}{'USE_FCLEN'} = join(',',split(' ',$ENV{FCLEN_SURF_TIME})) ;

 };


 # Define variable specific things
 foreach ( split(' ',$inpar) ) {

      $i++ ;
      $par = $_.'_IND';
      $nameread{'read_section'}{$par} = $i;

      # Copy the default values
      for $role ( sort keys %{ $plotdefs{'def'} } ) {
          $tmp{$_}{$role}=$plotdefs{'def'}{$role};
      } ;

      # Remove Web stuff
      delete $tmp{$_}{'TEXT'} ;
      delete $tmp{$_}{'TEXT_TEMP'} ;

      # Copy the values for this parameter
      for $role ( sort keys %{ $plotdefs{$_} } ) {
          $tmp{$_}{$role}=$plotdefs{$_}{$role};
      } ;

      # Add information for each level
      $ilev = 1 ;
      while ( $ilev le $nlev ) {
	 $k = ($i-1)*$nlev + $ilev ;
         if ( exists $tmp{$_}{'CONT_CLASS'} ) {
            $j++ ;
	    $arealoop{'scat_ver'}{'CONT_CLASS('.$j.')'}=$tmp{$_}{'CONT_CLASS'};
            $arealoop{'scat_ver'}{'CONT_IND('.$j.')'}=$k ;
            $arealoop{'scat_ver'}{'CONT_LIM(1:'.$tmp{$_}{'CONT_CLASS'}.','.$j.')'  }=$tmp{$_}{'CONT_LIM'};
         } ;

         if ( exists $tmp{$_}{'PRE_FCLA'} ) {
            $arealoop{'scat_ver'}{'PRE_FCLA(:,'.$k.')'}=$tmp{$_}{'PRE_FCLA'};
         } ;

         if ( exists $plots{'MAP'} ) {
		 $arealoop{'MAP'}{'MAP_BIAS_INTERVAL(1:7,'.$k.')'}=$tmp{$_}{'MAP_BIAS_INTERVAL'}; 
	 } ;

         $arealoop{'TIME'}{'TIMESERIE_WIND('.$k.')'}=$tmp{$_}{'TWIND_'.$type};
         $nameread{'read_section'}{'QC_LIM_SCALE('.$k.')'}=$tmp{$_}{'QC_LIM_SCALE'};

         $ilev++ ;
      } ;

 } ;

 if ( $j gt 0 ) { $arealoop{'scat_ver'}{'CONT_PARAM'}=$j; } ;
 if ( exists $plots{'DAYVAR'} ) { $arealoop{'DAYVAR'}{'NTIMVER'}=24/$obint ; } ;
 if ( exists $plots{'VERT'}   ) { $arealoop{'VERT'}{'NTIMVER'}=24/$obint   ; } ;
 if ( exists $plots{'MAP'}    ) {
    if ( $arealoop{'MAP'}{'LFCVER'} =~/F/) { $arealoop{'MAP'}{'NTIMVER'}=24/$obint   ; }; 
 }; 

 $nameread{'read_section'}{'NPARVER'} = $i*$nlev ;

 # Build the namelist

 $default='namelist';
 @lists=('nameread');
 &set_def;
 &join_lists ;
 &print_list($default) ;


 # Define the areas
 @areas = split(' ',$ENV{$type."AREAS"}) ;

 # Build the namelist for each area
 $area_num = 0;

 foreach $area ( @areas ) {

   unless  (  exists $areas{$area} ) { die "$area not defined in areas.pm \n"; } ;

   $area_num++ ;

   # Only plot single stations for the first area
   if ( $area_num gt 1 ) { $def{'def'}{'STNLIST_PLOT'} = '-1' } ;

   # Set default tag according to area
   $def{'def'}{'TAG'} = '\''.$area.'\'' ;

   #
   # Make sure STNLIST is defined only once and 
   # that we have trailing zeros in the stnlist
   #

   if ( exists $areas{$area}{'STNLIST'} ) {
   unless ( $areas{$area}{'STNLIST'} =~ /\*/ ) {
      $nstnlist = split(',',$areas{$area}{'STNLIST'});
      $nstn_not_used = $nameread{'read_section'}{'MAXSTN'}-$nstnlist ;
      unless ( $nstn_not_used eq 0 ) {
         $areas{$area}{'STNLIST'} = $areas{$area}{'STNLIST'}.','.$nstn_not_used.'*0';
      } ;
   } ;
   } else {
      $areas{$area}{'STNLIST'} = '0';
   } ;
   $areas{$area}{'STNLIST'}=~ s/,,/,/g ;

   #
   # Merge the default list with the different plot types
   #

   %tmp=();
   $default='tmp';
   @lists=('arealoop');
   &set_def ;
   &join_lists ;

   # Merge user defined tag and area
   &merge_role('tmp','arealoop','TAG','\'','_'.$area.'\'');

   # Copy the area map settings
   for $role ( keys %{ $areas{$area} } ) {
      if ( $role =~/MAP/ && exists $plots{'MAP'} ) {
        ${$default}{'MAP'}{$role}=$areas{$area}{$role};
      } else {
         for $key ( keys %${default} ) {
            ${$default}{$key}{$role}=$areas{$area}{$role};
         } ;
      } ;
   } ;

   # Set output table names
   ${$default}{'SEAS'}{'STATNAME'}   = '\'TABLE_SEAS_'.$area.'.html\'' ;
   ${$default}{'GEN'}{'STATNAME'}    = '\'TABLE_LL_'.$area.'.html\'' ;
   ${$default}{'DAYVAR'}{'STATNAME'} = '\'TABLE_HH_'.$area.'.html\'' ;
   ${$default}{'VERT'}{'STATNAME'}   = '\'TABLE_VV_'.$area.'.html\'' ;

   # Modify scatter plot settings
   unless ( exists $plots{'SCAT'} ) { ${$default}{'scat_ver'}{'LPLOT_SCAT'   } = 'F' ; } ;
   unless ( exists $plots{'FREQ'} ) { ${$default}{'scat_ver'}{'LPLOT_FREQ'   } = 'F' ; } ;
   unless ( exists $plots{'XML'}  ) { ${$default}{'scat_ver'}{'LPREP_XML'    } = 'F' ; } ;
   unless ( exists $plots{'CONT'} ) { ${$default}{'scat_ver'}{'CONT_PARAM'   } =  0  ; } ;

   # Only produce xml for the first area
   if ( $area_num gt 1 ) { ${$default}{'scat_ver'}{'LPREP_XML'} = 'F' } ;


   # 
   # Remove things not asked for
   #

   unless ( exists $plots{'VERT'}   ) { delete ${$default}{'VERT'} ;   } ;
   unless ( exists $plots{'DAYVAR'} ) { delete ${$default}{'DAYVAR'} ; } ;
   unless ( exists $plots{'TIME'}   ) { delete ${$default}{'TIME'} ;   } ;
   unless ( exists $plots{'GEN'}    ) { delete ${$default}{'GEN'} ;    };
   unless ( exists $plots{'SEAS'}   ) { delete ${$default}{'SEAS'} ;   };
   unless ( exists $plots{'MAP'}    ) { delete ${$default}{'MAP'} ;    };
   unless ( exists $plots{'SCAT'} || 
	    exists $plots{'XML'}  ||
	    exists $plots{'FREQ'} ||
	    exists $plots{'CONT'}    ) { delete ${$default}{'scat_ver'} ;};


   #
   # Print the final namelist
   #

   &print_list($default) ;

};
#################################################################
sub set_def{

  ${$default} = ();

  foreach $testop ( @lists ) {
   for $key ( keys %${testop} ) {
	 for $role ( sort keys %{ $def{'def'} } ) {
          ${$default}{$key}{$role}=$def{'def'}{$role};
      } ;
   } ;
  } ;


} ;
#################################################################
sub join_lists{
 
  foreach $testop ( @lists ) {

   for $key ( keys %${testop} ) {
      for $role ( sort keys %{ ${$testop}{$key} } ) {
          ${$default}{$key}{$role}=${$testop}{$key}{$role};
      } ;
   } ;

  } ;

} ;
#################################################################
sub merge_role{

   $out_list  = shift @_ ;
   $sub_list  = shift @_ ;
   $sub_role  = shift @_ ;
   $sub_lv    = shift @_ ;
   $sub_rv    = shift @_ ;

   for $key ( keys %${sub_list} ) {
      if ( exists ${$sub_list}{$key}{$sub_role} ) {
          ${$out_list}{$key}{$sub_role}=$sub_lv.${$sub_list}{$key}{$sub_role}.$sub_rv ;
      } ;
   } ;

} ;
#################################################################
sub print_list{

  $input = shift @_ ;

  for $key ( keys %${input} ) {
     print "\&NAMVER\n";
     for $role ( sort keys %{ ${$input}{$key} } ) {
         print "   $role=${$input}{$key}{$role},\n";
     } ;
     print " \/\n";
  };

};
#################################################################
sub print_hash{

	print " DEFAULT : $default \n";

  for $key ( keys %${default} ) {
     print "$key \n" ;
     for $role ( sort keys %{ ${$default}{$key} } ) {
         print "   $role=${$default}{$key}{$role},\n";
     } ;
  };

};
#################################################################
