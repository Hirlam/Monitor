#!/usr/bin/perl

 #
 # Build namelist for verification
 #
 # Usage: Build_namelist.pl TYPE INPAR
 # where :
 # TYPE is SURF or TEMP
 # Inpar is any parameter or parameters such as "NN" or "FF DD NN"
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

    @lev = split(' ',$ENV{LEV_LST});
    $nlev = scalar(@lev);
    $nameread{'read_section'}{'LEV_LST'} = join(',',@lev);
    $nameread{'read_section'}{'LTEMP'} = 'T';
    $nameread{'read_section'}{'DATA_SOURCE'} = '\'vfld_temp\'';

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
            $arealoop{'scat_ver'}{'CONT_LIM('.$j.',1:'.$tmp{$_}{'CONT_CLASS'}.')'  }=$tmp{$_}{'CONT_LIM'};
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

 $nameread{'read_section'}{'NPARVER'} = $i*$nlev ;

 # Build the namelist

 $default='namelist';
 @lists=('nameread');
 &set_def;
 &join_lists ;
 &print_list ;


 # Define the areas
 @areas = split(' ',$ENV{$type."AREAS"}) ;

 # Build the namelist for each area
 $area_num = 0;

 foreach $area ( @areas ) {

   $area_num++ ;

   $def{'def'}{'TAG'} = '\''.$area.'\'' ;

   %tmp=();
   $default='tmp';
   @lists=('arealoop');
   &set_def ;
   &join_lists ;

   for $role ( keys %{ $areas{$area} } ) {
      if ( $role =~/MAP/ && exists $plots{'MAP'} ) {
        ${$default}{'MAP'}{$role}=$areas{$area}{$role};
      } else {
         for $key ( keys %${default} ) {
            ${$default}{$key}{$role}=$areas{$area}{$role};
         } ;
      } ;
   } ;

   if ( exists $areas{$area}{'STNLIST'} ) {
      $def{'def'}{'STNLIST'}=
      $areas{$area}{'STNLIST'}.','.$nameread{'read_section'}{'MAXSTN'}.'*0',
      $def{'def'}{'STNLIST'}=~ s/,,/,/g ;
   } ;

   ${$default}{'GEN'}{'STATNAME'} = '\''.$type.'_LL_'.$area.'.html\'' ;
   ${$default}{'DAYVAR'}{'STATNAME'} = '\''.$type.'_HH_'.$area.'.html\'' ;
   ${$default}{'VERT'}{'STATNAME'}   = '\''.$type.'_VV_'.$area.'.html\'' ;

   unless ( exists $plots{'SCAT'} ) { ${$default}{'scat_ver'}{'LPLOT_SCAT'   } = 'F' ; } ;
   unless ( exists $plots{'FREQ'} ) { ${$default}{'scat_ver'}{'LPLOT_FREQ'   } = 'F' ; } ;
   unless ( exists $plots{'XML'}  ) { ${$default}{'scat_ver'}{'LPREP_XML'    } = 'F' ; } ;
   unless ( exists $plots{'CONT'} ) { ${$default}{'scat_ver'}{'CONT_PARAM'   } =  0  ; } ;

   # Only produce xml for the first area
   if ( $area_num gt 1 ) {  ${$default}{'scat_ver'}{'LPREP_XML'    } = 'F' } ;

   # 
   # Remove things not asked for
   #

   unless ( exists $plots{'VERT'}   ) { delete ${$default}{'VERT'} ;   } ;
   unless ( exists $plots{'DAYVAR'} ) { delete ${$default}{'DAYVAR'} ; } ;
   unless ( exists $plots{'TIME'}   ) { delete ${$default}{'TIME'} ;   } ;
   unless ( exists $plots{'GEN'}    ) { delete ${$default}{'GEN'} ;    };
   unless ( exists $plots{'MAP'}    ) { delete ${$default}{'MAP'} ;    };
   unless ( exists $plots{'SCAT'} || 
	    exists $plots{'XML'}  ||
	    exists $plots{'FREQ'} ||
	    exists $plots{'CONT'}    ) { delete ${$default}{'scat_ver'} ;};

   &print_list ;

};
#################################################################
sub set_def{

  ${$default} = ();

  foreach $testop ( @lists ) {
   for $key ( keys %${testop} ) {
	 for $role ( sort keys %{ $def{def} } ) {
          ${$default}{$key}{$role}=$def{def}{$role};
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
sub print_list{

  for $key ( keys %${default} ) {
     print "\&NAMVER\n";
     for $role ( sort keys %{ ${$default}{$key} } ) {
         print "   $role=${$default}{$key}{$role},\n";
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
