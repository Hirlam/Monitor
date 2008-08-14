#!/usr/bin/perl
#

# Create gnuplot plots from verobs .txt files

SCAN_INPUT : foreach $input_file (@ARGV) {

 @legend = ();
 @column = ();

 # Examine file name

 @tmp = split('_',$input_file);
 $prefix = shift(@tmp);
 @tmp = split('.txt',$input_file);
 $output_file = shift(@tmp).".ps";

 open FILE, "< $input_file" ;

 SCAN_FILE : while ( <FILE> ) {
   chomp ;
   if ( $_ =~ /#END/) { last SCAN_FILE ; } ;
   if ( $_ =~ /CASES/)    { next SCAN_FILE; } ;
   if ( $_ =~ /#HEADING_1/) { $heading1=substr($_,11);next SCAN_FILE;  } ;
   if ( $_ =~ /#HEADING_2/) { $heading2=substr($_,11);next SCAN_FILE;  } ;
   if ( $_ =~ /#HEADING_3/) { $heading3=substr($_,11);next SCAN_FILE;  } ;
   if ( $_ =~ /#HEADING_4/) { $heading4=substr($_,11);next SCAN_FILE;  } ;
   if ( $_ =~ /#YLABEL/) { $ylabel=substr($_,8);next SCAN_FILE;  } ;
   if ( $_ =~ /#XLABEL/) { $xlabel=substr($_,8);next SCAN_FILE ;} ;
   if ( $_ =~ /#MISSING/) { $missing=substr($_,10);next SCAN_FILE ;} ;
   if ( $_ =~ /#COLUMN/ ) {
       @legend = (@legend,substr($_,10) );
       @column = (@column,substr($_,8,3)   );
       next SCAN_FILE ;
                         } ;
 } ;

 close FILE ;

 # Start writing the plotting file

 # Print the header
 &header ;

 # File type dependent options

 PLOT_TYPES : {

 if ( $prefix =~ /ps/ ||  $prefix =~ /PS/ ) { &timeserie ; last PLOT_TYPES ; } 
 if ( $prefix =~ /v/ ||  $prefix =~ /V/   ) { &gen_stat ; last PLOT_TYPES ; } 

 print "Skip unknown file : $input_file \n";
 close GP ;
 next SCAN_INPUT ;
 
 } ;

 $plot = "plot ";

 $i = -1;
 foreach (@legend){
    $i++ ; 
    if ( $i gt 0 ) { $plot = "$plot,";};
    $plot = $plot." '$input_file' using 1:".$column[$i]." title '$legend[$i]' with linespoints lt $i lw 1";

 } ;
 

 # Call gnuplot

 print GP "$plot";
 system("gnuplot plot.gp");
 close GP ;

 print "Created: $output_file \n";

} ;
#################################################################
#################################################################
#################################################################
sub header {

open GP,">plot.gp";

print GP <<EOF;
set terminal postscript landscape enhanced colour "Times-roman" 14
set output '$output_file'
set missing "$missing"
set label "$heading1" at screen 0.55,0.94 c font "Helvetica,18"
set label "$heading2" at screen 0.55,0.90 c font "Helvetica,14"
set label "$heading3" at screen 0.55,0.86 c font "Helvetica,14"
set label "$heading4" at screen 0.55,0.82 c font "Helvetica,14"
set xlabel "$xlabel"
set ylabel "$ylabel"
set timefmt "%Y%m%d %H"
set grid
EOF
} ;
#################################################################
#################################################################
#################################################################
sub timeserie {

print GP <<EOF;
set xdata time
set format x "%d/%m\\n%H"
EOF
} ;
#################################################################
#################################################################
#################################################################
sub gen_stat {
} ;
