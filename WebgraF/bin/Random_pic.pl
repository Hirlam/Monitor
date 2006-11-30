#!/usr/bin/perl -w

 #
 # Ulf Andrae, SMHI, 2005
 #

 # Change you working directory here        
 #############################################
 $BASE = "/home/Ulf.Andrae/WebgraF" ; 
 #############################################

#
# Get input
 
chdir $BASE or die "Could not go to $BASE \n" ;


#
# List projects and entrys
#
$fig="nada" ;

while ( $fig =~ /nada/ ) {
chdir $BASE or die "Could not go to $BASE \n" ;
	@PROJECTS = &get_dir ;
	my $length = $#PROJECTS + 1;

	# get random number between 0 - $length and copy to $length
	 $length = rand($length);
	#
	# # make sure you have an integer
	 $length = int $length; 
	 $project = $PROJECTS[$length] ;
		$project =~ s/ //g ;
		chdir $project ;
		@ENTRYS = &get_dir ;
	$length = $#ENTRYS + 1;
	$length = rand($length);
	$length = int $length; 
	$entry = $ENTRYS[$length] ;
		$entry =~ s/ //g ;
		chdir $entry ;
        opendir MYDIR, "." ;
        local @FILES = grep !/^\.\.?/, readdir MYDIR ;
        @FILES = grep !/.ps/, @FILES  ;
        @FILES = grep !/.txt/, @FILES  ;
        @FILES = grep !/.dat/, @FILES  ;
        @FILES = grep !/.js/, @FILES ;
        @FILES = grep !/html/, @FILES  ;
	$length = $#FILES + 1;
	if ( $length > 0 ) {
	   $length = rand($length);
	   $length = int $length; 
	   $fig = $FILES[$length] ;

	   if ( -d "$fig" ) {

	      chdir $fig ;
              opendir MYDIR, "." ;
              local @FILES = grep !/^\.\.?/, readdir MYDIR ;
              @FILES = grep !/.ps/, @FILES  ;
              @FILES = grep !/.txt/, @FILES  ;
              @FILES = grep !/.dat/, @FILES  ;
              @FILES = grep !/.js/, @FILES ;
              @FILES = grep !/html/, @FILES  ;
	      $length = $#FILES + 1;

	      if ( $length == 0 ) { exit ; } ;

	      $length = rand($length);
	      $length = int $length; 
	      $fig = $fig."/".$FILES[$length];

	   } ;

	   print "project='$project'\n" ;
	   print "entry='$entry'\n" ;
	   print "ranfig='$fig'\n" ;
	   exit ;

        } ;
} ;



#
# Add an infofile to the project or entry
#

#######################
sub update_input_list {
	
 $OFILE="input_list.js" ;
 open OFILE, "> $OFILE";
 $i=-1 ;
 foreach ( @_ ) { 
	 s/ //g;
	 $i++; print OFILE "input_list[$i]='$_.js' \n" ; };
 close OFILE ;
 system("chmod 777 $OFILE") ;
  

};

#######################
sub get_dir {

 opendir MYDIR, "." ;
 local @FILES = grep !/^\.\.?/, readdir MYDIR ;
 @FILES = grep !/src/, @FILES ;
 @FILES = grep !/bin/, @FILES ;
 @FILES = grep !/port/, @FILES ;
 close MYDIR ;
 local @F ;
 foreach ( @FILES ) { if ( -d $_ ) { @F = (@F," $_ ") ; }; };
 @F = sort @F ;
 return @F ;
};
#######################
sub make_index {

$OFILE="input.js" ;
open OFILE, "> $OFILE";
print OFILE "info='info.html' \n";

$i=0 ;
@P = &get_dir ;
$P=join('\',\'',@P) ;
$V ="mname=['".$P."']";
$V =~s/ //g;
print OFILE "$V \n ";

foreach ( @P ) {
	s/ //g;
	chdir $_ or die "Could not go to $_ \n" ;
	@E = &get_dir ;
	$E=join('\',\'',@E) ;
	$V ="v[$i]=['".$E."']";
	$V =~s/ //g;
	print OFILE "$V \n";
	print OFILE "t[$i]=v[$i] \n";
	chdir ".." ;
	$i++ ;
};

close OFILE ;
foreach ( @P ) {
	s/ //g;
	$INFO=$_."/".$_.".html" ;
	 unless ( -s  $INFO ) { system("ln -s ../info.html $INFO") ; };
}

system("chmod 777 *") ;
};

