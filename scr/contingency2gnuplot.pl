#!/usr/bin/perl
#
# Create gnuplot plots from contingency_tables in .html files
#
# Usage: contingency2gnuplot.pl *.html, where *.html is the html-files produced by 
# The HARMONIE verification.
#
# Author: Calle Fortelius, June 2009, based on verobs2gnuplot.pl
#

SCAN_INPUT: foreach $input_file (@ARGV) {

    print "Process:$input_file \n";

    @heading   = ();
    @workfiles     = ();
    @limits     = ();
    @enames     = ();
    $missing = -99;


    # Examine file name

    @tmp    = split( '.html', $input_file );
    $prefix = shift(@tmp);

    # PS or PNG as output
    if ( $ENV{OUTPUT_TYPE} eq 1 ) {
        $output_file = $prefix . ".ps";
        $terminal    = "set terminal postscript landscape enhanced colour";
    }
    else {
        $output_file = $prefix . ".1.png";
        $terminal    = "set terminal png";
    }

    open FILE, "< $input_file";

    SCAN_FILE: while (<FILE>) {
        #  
        # Scan through the file and extract the necessary information
        #

        chomp;
        $line = $_; @line=split(' ', $line); $len=@line;
        if ( $line =~  "</pre></body></html>"  )  { last SCAN_FILE; }

        if ( $line =~ /Contingency/ ) {
            @TMP = split(' ',$line); 
            $unit = pop(@TMP);
            $unit =~ s/\((.*)\)/$1/;
            push(@heading,$line);
            next SCAN_FILE;
        }

        if ( $line =~ /Area/ )   {
           push(@heading,$line);
           next SCAN_FILE; }

        if ( $line =~ /Period/ )   {
           push(@heading,$line);
           next SCAN_FILE; }
        if ( $line =~ /Limits/ )   { @thresholds = split(' ',substr( $line, 9 ));
                                     $thresholdscan = 1; next SCAN_FILE; }
        if ( $len and ($line !~ /Each/) and $thresholdscan ){
           push(@thresholds,@line) ; next SCAN_FILE; }
        if ( $line =~ /Each/ ) {$thresholdscan = 0 ;next SCAN_FILE;}
        if ( $line =~ /Total/ ) { @tmp = split (' ',$line ) ; $number_of_values = $tmp[4]; next SCAN_FILE;}
        if ( $line =~ /OBSERVATION/ ) {$datascan = 1 ; @AoA = (); next SCAN_FILE; } # We have found a multiple
                                                                                  # contingency table
        if ( $len and ($line !~ /SUM/) and $datascan ){
              if ( (@line[0] * 1) ne @line[0] )
              {$exp = $line[0];  push(@enames,$exp); shift(@line);} # not a number, but expname
              push @AoA, [@line] ; next SCAN_FILE; }
        if ( ($line =~ /SUM/) && $datascan ){  # We have now read the entire table and are ready for 
                                                 # spliting in into multiple 2 by 2 contingency tables
                                                 # correspondiong to the respective thresholds. The event
                                                 # to be verified is that "there was more than $threshold
                                                 # mm of precipitation". Note that for n classes there are
                                                 # n+1 values, as the last value contains the number of 
                                                 # events exceeding he last threshold.
	   $datascan=0;
           $classes=scalar(@thresholds);
           push(@workfiles, $prefix . "_" . $exp . ".scores");

           #for (my $f=0; $f <= $classes; $f++){
              #for (my $o=0; $o <= $classes; $o++){ 
              #   print "$AoA[$f][$o] ";} 
              #print "\n";	   }

           open (SCOREFILE, ">$workfiles[@workfiles-1]");
           for (my $class=0; $class <= $classes-1; $class++){
               my $a = 0;my $b = 0;my $c = 0;my $d = 0;
               for (my $o=$class+1; $o <= $classes; $o++){ # obs yes; fc yes
               for (my $f=$class+1; $f <= $classes; $f++){ $a = $a + $AoA[$f][$o] }}

               for (my $o=0; $o <= $class; $o++){            # obs no; fc yes
               for (my $f=$class+1; $f <= $classes; $f++){ $b = $b + $AoA[$f][$o] }}

               for (my $o=$class+1; $o <= $classes; $o++){ # obs yes; fc no
               for (my $f=0; $f <= $class; $f++){ $c = $c + $AoA[$f][$o] }}

               for (my $o=0; $o <= $class; $o++){           # obs no; fc no
               for (my $f=0; $f <= $class; $f++){ $d = $d + $AoA[$f][$o] }}
               my $FAR = $missing; if ($a+$b > 0) {$FAR = $b/($a+$b);}
               my $POD = $missing; if ($a+$c > 0) {$POD = $a/($a+$c);}
               print SCOREFILE " @thresholds[$class] $FAR $POD";
	   } #loop over classes
           print SCORFEFILE "\n"; close (SCOREFILE);
      next SCAN_FILE; }
 }
close FILE;

# Set colors for score plots
@colors = ("-1","1","3","4","8","5","6","9","7");
@markers = ("-1","1","3","4","8","5","6","9","7");
#
# Write the plotting file
&header;
&plot_wilson;

# Call gnuplot
system("gnuplot plot.gp");
print "Created:$output_file \n";

next SCAN_INPUT;}


#################################################################
#################################################################
#################################################################
sub header {

    # Create header
    $len_head = scalar(@heading) ;
    $heading =$heading[0];
    for ($i=1;$i<$len_head;$i++ ) { $heading=$heading."\\n  $heading[$i]"; } ;

    open GP, ">plot.gp";

    print GP <<EOF;
$terminal
set output '$output_file'
set missing "$missing"
set title "$heading" 

set style line 1 lt 0 lw 1 pt 5 # use black thin lines
set style line 2 lt 8 lw 1 pt 1 # use black thicker lines
EOF
}
#################################################################
#################################################################
#################################################################
sub plot_wilson {
  
print GP <<EOF;
set xlabel "False alarm ratio"
set ylabel "Hit rate (bias and Threat score)"
set yrange [0:1]
set xrange [0:1]
#unset key
EOF

# make labels for each experiment and for the isolines:
$i=0;
foreach $exp (@enames) {$i++; 
$y=0.02+0.03*int(($i-1)/3);$x=0.01+(($i-1)%3)/3;
print GP "set label '$exp' at $x,$y textcolor lt $colors[$i-1] \n";
 }
print GP <<EOF; 
set label '1.2' at 0.18,0.97 textcolor lt 0
set label '1.5' at 0.35,0.97 textcolor lt 0
set label '2.0' at 0.50,0.97 textcolor lt 0
set label '3.0' at 0.65,0.97 textcolor lt 0
plot \\
EOF

$f=-1;
foreach (@workfiles) {
   $f++; $linetype=$colors[$f];
   $t=0;
   foreach $threshold (@thresholds) {
     $x=3*$t+2; $y=3*$t+3;
     if($f eq 0){@xx = split('\.',$threshold);$title=$xx[0] . "." .  substr($xx[1],0,2) . " $unit";}
     else {$title="";}
     $t++;
     print GP <<EOF;
     '$workfiles[$f]' using $x:$y title '$title' with points pointtype $t lt $linetype, \\
EOF
}}
# The next lined will add the frequency bias and the threat score to the plot:
print GP <<EOF;
0.2*(1-x) title 'bias' ls 1,\\
0.4*(1-x) title '' ls 1,\\
0.6*(1-x) title '' ls 1,\\
0.8*(1-x) title '' ls 1,\\
1*(1-x) title '' ls 1,\\
1.2*(1-x) title '' ls 1,\\
1.5*(1-x) title '' ls 1,\\
2*(1-x) title '' ls 1,\\
3*(1-x) title '' ls 1,\\
8*(1-x) title '' ls 1,\\
0.11*(x-1)/((0.11+1)*x -1) title 'Threat score' ls 2,\\
0.2*(x-1)/((0.2+1)*x -1) title '' ls 2,\\
0.3*(x-1)/((0.3+1)*x -1) title '' ls 2,\\
0.4*(x-1)/((0.4+1)*x -1) title '' ls 2,\\
0.5*(x-1)/((0.5+1)*x -1) title '' ls 2,\\
0.6*(x-1)/((0.6+1)*x -1) title '' ls 2,\\
0.7*(x-1)/((0.7+1)*x -1) title '' ls 2,\\
0.8*(x-1)/((0.8+1)*x -1) title '' ls 2,\\
0.9*(x-1)/((0.9+1)*x -1) title '' ls 2


EOF
close GP   }
