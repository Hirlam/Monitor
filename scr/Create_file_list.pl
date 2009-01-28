#!/usr/bin/perl
#
 # List files from the current directory

 opendir MYDIR, "." ;
 @FILES = grep !/^\.\.?/, readdir MYDIR ;
 close MYDIR ;

 if ( @ARGV ) { 
  @FILES = grep /$ARGV[0]/, @FILES ; 
 } ;

 $FILES = sort @FILES ;

 print "@FILES";

