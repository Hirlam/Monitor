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
 # Different selections are defined in selection.pm
 #
 # Ulf Andrae, SMHI, 2008
 #

 use selection ;
 use plotdefs ;
 use maindefs ;
 use skilldefs ;

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
 $nameread{'read_section'}{'FCLEN'}=join(',',split(' ',$ENV{'FCLEN_READ_'.$type})) or
 $nameread{'read_section'}{'FCLEN'}=join(',',split(' ',$ENV{'FCLEN_'.$type})) ;

 #
 # Define variables
 #

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

    $selectionloop{'TIME'}{'USE_FCLEN'} = join(',',split(' ',$ENV{FCLEN_TEMP_TIME})) ;

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

    $selectionloop{'TIME'}{'USE_FCLEN'} = join(',',split(' ',$ENV{FCLEN_SURF_TIME})) ;

 };

 # Set index for all parameters, to be used laters
 $i=0;
 %par_ind = ();
 my @varlist=split(' ',$inpar);
 foreach (@varlist) {
    $i++ ;
   $par_ind{$_} = $i  ;
 } ;

 $varlist ='\''.join('\',\'',@varlist).'\'';

 $nameread{'read_section'}{'varlist'} = $varlist ;
 # Define variable specific things
 $i=0;
 foreach ( split(' ',$inpar) ) {

      $i++ ;

      # Copy the default values
      for $role ( sort keys %{ $plotdefs{'def'} } ) {
          $tmp{$_}{$role}=$plotdefs{'def'}{$role};
      } ;


      # Copy the values for this parameter
      for $role ( sort keys %{ $plotdefs{$_} } ) {
          $tmp{$_}{$role}=$plotdefs{$_}{$role};
      } ;

      # Add information for each level
      $ilev = 1 ;
      while ( $ilev <= $nlev ) {
	 $k = ($i-1)*$nlev + $ilev ;

         # Contingency settings
         if ( exists $tmp{$_}{'CONT_CLASS'} ) {
            $j++ ;
	    $selectionloop{'scat_ver'}{'CONT_CLASS('.$j.')'}=$tmp{$_}{'CONT_CLASS'};
            $selectionloop{'scat_ver'}{'CONT_IND('.$j.')'}=$k ;
            $selectionloop{'scat_ver'}{'CONT_LIM(1:'.$tmp{$_}{'CONT_CLASS'}.','.$j.')'  }=$tmp{$_}{'CONT_LIM'};

            @SKILL = split(' ',$ENV{SCORELIST});
            foreach $skill (@SKILL) {
              unless ( exists($skill_score_def{$skill}) ) { 
               die "This skill score is not defined: $skill \n";
              } ;
            } ;

         } ;

         # Copy temp text to text
         if ( exists $tmp{$_}{'TEXT_TEMP'} && $type =~ /TEMP/ ) {
            $tmp{$_}{'TEXT'} = $tmp{$_}{'TEXT_TEMP'} ;
            delete $tmp{$_}{'TEXT_TEMP'} ;
         } ;

         # Fill the setprop values
         foreach $prop ('TEXT','UNIT') {         
          if ( exists $tmp{$_}{$prop} ) {
            $nameread{'read_section'}{'SETPROP('.$k.')%ID'}='\''.$_.'\'' ;
            $nameread{'read_section'}{'SETPROP('.$k.')%'.$prop}='\''.$tmp{$_}{$prop}.'\'';
          } ;
         } ;

         foreach $prop ('ACCTYPE','ACC','LIM','LLIM','ULIM') {         
          if ( exists $tmp{$_}{$prop} ) {
            $nameread{'read_section'}{'SETPROP('.$k.')%ID'}='\''.$_.'\'' ;
            $nameread{'read_section'}{'SETPROP('.$k.')%'.$prop}=$tmp{$_}{$prop} ;
          } ;
         } ;

         # Frequency plots
         if ( exists $tmp{$_}{'PRE_FCLA'} ) {
            $selectionloop{'scat_ver'}{'PRE_FCLA(:,'.$k.')'}=$tmp{$_}{'PRE_FCLA'};
         } ;

         # Map
         if ( exists $plots{'MAP'} ) {
		 $selectionloop{'MAP'}{'MAP_BIAS_INTERVAL(1:7,'.$k.')'}=$tmp{$_}{'MAP_BIAS_INTERVAL'}; 
	 } ;

         # Timeserie settings
         $selectionloop{'TIME'}{'TIMESERIE_WIND('.$k.')'}=$tmp{$_}{'TWIND_'.$type};
         $nameread{'read_section'}{'QC_LIM_SCALE('.$k.')'}=$tmp{$_}{'QC_LIM_SCALE'};

         $ilev++ ;
      } ;

 } ;

 if ( $j > 0 ) { $selectionloop{'scat_ver'}{'CONT_PARAM'}=$j; } ;
 if ( exists $plots{'DAYVAR'} ) { $selectionloop{'DAYVAR'}{'NTIMVER'}=24/$obint ; } ;
 if ( exists $plots{'VERT'}   ) { $selectionloop{'VERT'}{'NTIMVER'}=24/$obint   ; } ;
 if ( exists $plots{'MAP'}    ) {
    if ( $selectionloop{'MAP'}{'LFCVER'} =~/F/) { $selectionloop{'MAP'}{'NTIMVER'}=24/$obint   ; }; 
 }; 

 # Build the namelist

 $default='namelist';
 @lists=('nameread');
 &set_def;
 &join_lists ;
 &print_list($default) ;


 # Define the selections
 if ( $ENV{$type."SELECTION"} ) {
   @selections = split(' ',$ENV{$type."SELECTION"}) ;
 } else {
   if ( $ENV{$type."AREAS"} ) {
      die "${type}AREAS is depreciated please use ${type}SELECTION instead \n";
   } 
   die "Please give ${type}SELECTION \n";
 } ;

 # Define the initial_hours
 @ini_hours = ('ALL');
 if ( $ENV{$type."INI_HOURS"} ) {
   @ini_hours = split(' ',$ENV{$type."INI_HOURS"}) ;
 } ;

 # Build the namelist for each selection
 $selection_num = 0;

 foreach $ini_hour ( @ini_hours ) {

  if ( $ini_hour =~ /ALL/ ) {
     $def{'def'}{'INI_HOURS'} = '24*-1',
  } else { 
     $def{'def'}{'INI_HOURS'} = "$ini_hour,23*-1",
  } ;

 foreach $selection ( @selections ) {

   unless  (  exists $selections{$selection} ) { die "$selection not defined in selection.pm \n"; } ;

   $selection_num++ ;

   # Only plot single stations for the first selection
   if ( $selection_num > 1 ) { $def{'def'}{'STNLIST_PLOT'} = '-1' } ;

   # Set default tag according to selection
   $def{'def'}{'TAG'} = '\''.$selection.'\'' ;

   #
   # Make sure STNLIST is defined only once and 
   # that we have trailing zeros in the stnlist
   #

   if ( exists $selections{$selection}{'STNLIST'} ) {
   unless ( $selections{$selection}{'STNLIST'} =~ /\*/ ) {
      $nstnlist = split(',',$selections{$selection}{'STNLIST'});
      $nstn_not_used = $nameread{'read_section'}{'MAXSTN'}-$nstnlist ;
      if ( $nstn_not_used ) {
         $selections{$selection}{'STNLIST'} = $selections{$selection}{'STNLIST'}.','.$nstn_not_used.'*0';
      } ;
   } ;
   } else {
      $selections{$selection}{'STNLIST'} = '0';
   } ;
   $selections{$selection}{'STNLIST'}=~ s/,,/,/g ;

   #
   # Make conditional selections if set
   #

   if ( exists $selections{$selection}{'COND%IND'} ) {

     $i++ ;
     @PAR = split(',',$selections{$selection}{'COND%IND'}) ;
     $cond_param = scalar(@PAR) ;

     @ind = () ;
     foreach $par ( @PAR ) {
      @ind = (@ind,$par_ind{$par}) ;
     } ;
      
     for $cond ( keys %{ $selections{$selection} } ) {
       if ( $cond =~/COND/ )  {
         @tmp = split('%',$cond) ;
         if ( @tmp[1] eq 'IND' ) {
           $cc = $tmp[0]."(1:$cond_param)%".$tmp[1] ;
           $selections{$selection}{$cc} = join(',',@ind);
         } else {
           $cc = "$tmp[0](1:$cond_param)%$tmp[1]" ;
           $selections{$selection}{$cc} = $selections{$selection}{$cond} ;
         } ;
         delete ( $selections{$selection}{$cond} ) ;
       } ;
     } ;
     $selections{$selection}{'COND_PARAM'} = $cond_param ;

   } ;
    
   #
   # Merge the default list with the different plot types
   #

   %tmp=();
   $default='tmp';
   @lists=('selectionloop');
   &set_def ;
   &join_lists ;

   # Merge user defined tag and selection
   &merge_role('tmp','selectionloop','TAG','\'','_'.$selection.'\'');

   # Copy the selection map settings
   for $role ( keys %{ $selections{$selection} } ) {
      if ( $role =~/MAP/ && exists $plots{'MAP'} ) {
        ${$default}{'MAP'}{$role}=$selections{$selection}{$role};
      } else {
         for $key ( keys %${default} ) {
            ${$default}{$key}{$role}=$selections{$selection}{$role};
         } ;
      } ;
   } ;

   # Set output table names
   ${$default}{'SEAS'}{'STATNAME'}   = '\'TABLE_SEAS_'.$selection.'.html\'' ;
   ${$default}{'GEN'}{'STATNAME'}    = '\'TABLE_LL_'.$selection.'.html\'' ;
   ${$default}{'DAYVAR'}{'STATNAME'} = '\'TABLE_HH_'.$selection.'.html\'' ;
   ${$default}{'VERT'}{'STATNAME'}   = '\'TABLE_VV_'.$selection.'.html\'' ;

   # Modify scatter plot settings
   unless ( exists $plots{'SCAT'} ) { ${$default}{'scat_ver'}{'LPLOT_SCAT'   } = 'F' ; } ;
   unless ( exists $plots{'FREQ'} ) { ${$default}{'scat_ver'}{'LPLOT_FREQ'   } = 'F' ; } ;
   unless ( exists $plots{'XML'}  ) { ${$default}{'scat_ver'}{'LPREP_XML'    } = 'F' ; } ;
   unless ( exists $plots{'CONT'} ) { ${$default}{'scat_ver'}{'CONT_PARAM'   } =  0  ; } ;

   # Only produce xml for the first selection
   if ( $selection_num > 1 ) { ${$default}{'scat_ver'}{'LPREP_XML'} = 'F' } ;


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
