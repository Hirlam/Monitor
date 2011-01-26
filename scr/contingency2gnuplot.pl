#!/usr/bin/perl
#
# Create gnuplot plots from contingency_tables in .html files
#
# Usage: contingency2gnuplot.pl *.html, where *.html is the html-files produced by 
# The HARMONIE verification.
#
# Author: Calle Fortelius, June 2009, based on verobs2gnuplot.pl
#
@col_def_lt  = (1,2,3,4,5,6,8,7);
@col_def_lt  = (0,@col_def_lt);


SCAN_INPUT: foreach $input_file (@ARGV) {

    print "Process:$input_file \n";

    @heading    = ();
    @workfiles  = ();
    @workfiles2 = ();
    @limits     = ();
    @enames     = ();
    $missing = -99;


    # Examine file name and parameter tag

    @tmp    = split( '.html', $input_file );
    $prefix = shift(@tmp);

    # Set log/normal scale depending on parameter
    @tmp    = split( '_', $prefix );
    $partag = $tmp[-2];
    $xscale = "";
    if ( $partag eq "PE" ) { $xscale="set logscale x" ; } ;

    @EXT = ('','.ps','.1.png','.1.jpg','.svg') ;

    # PS or PNG as output
    if ( $ENV{OUTPUT_TYPE} eq 1 ) {
        $terminal    = "set terminal postscript landscape enhanced colour";
    } elsif ( $ENV{OUTPUT_TYPE} eq 2 ) {
        $terminal    = "set terminal png";
    } elsif ( $ENV{OUTPUT_TYPE} eq 3 ) {
        $terminal    = "set terminal jpeg";
    } elsif ( $ENV{OUTPUT_TYPE} eq 4 ) {
        $terminal    = "set terminal svg enhanced fsize 8 ";
    } else {
      die "Unknown OUTPUT_TYPE $ENV{OUTPUT_TYPE}\n";
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
            $line =~ s/Contingency table//;
            push(@heading,$line);
            next SCAN_FILE;
        }

        if ( $line =~ /Selection/ )   {
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
        if ( ($line =~ /SUM/) && $datascan ){ 

           # We have now read the entire table and are ready for
           # spliting in into multiple 2 by 2 contingency tables
           # correspondiong to the respective thresholds. The event
           # to be verified is that "there was more than $threshold
           # mm of precipitation". Note that for n classes there are
           # n+1 values, as the last value contains the number of
           # events exceeding he last threshold.

	   $datascan=0;
           $classes=scalar(@thresholds);
           push(@workfiles, $prefix . "_" . $exp . ".scores");
           push(@workfiles2,$prefix . "_" . $exp . ".scores2");

           open (SCOREFILE, ">$workfiles[@workfiles-1]");
           open (SCOREFILE2,">$workfiles2[@workfiles2-1]");

           for (my $class=0; $class <= $classes-1; $class++){

               my $a = 0;my $b = 0;my $c = 0;my $d = 0;

               # forecast\obs observed not obs
               # forecast        a       b
               # not forecast    c       d

               for (my $o=$class+1; $o <= $classes; $o++){ # obs yes; fc yes
               for (my $f=$class+1; $f <= $classes; $f++){ $a = $a + $AoA[$f][$o] }}

               for (my $o=0; $o <= $class; $o++){            # obs no; fc yes
               for (my $f=$class+1; $f <= $classes; $f++){ $b = $b + $AoA[$f][$o] }}

               for (my $o=$class+1; $o <= $classes; $o++){ # obs yes; fc no
               for (my $f=0; $f <= $class; $f++){ $c = $c + $AoA[$f][$o] }}

               for (my $o=0; $o <= $class; $o++){           # obs no; fc no
               for (my $f=0; $f <= $class; $f++){ $d = $d + $AoA[$f][$o] }}

               my $nn=$a+$b+$c+$d;

               #False alarm RATIO:
               my $FAR = $missing; if ($a+$b > 0) {$FAR = $b/($a+$b);}
               #Probability of detection:
               my $POD = $missing; if ($a+$c > 0) {$POD = $a/($a+$c);}
               #False alarm RATE:
               my $FA  = $missing; if( $b+$d > 0) {$FA  = $b/($b+$d);}
               #Kuipers index:
               my $KUI = $missing; if ($a+$c > 0 and $b+$d > 0) {$KUI = ($a*$d-$b*$c)/(($b + $d)*($a + $c));}
               # Frequency bias
               my $FBI = $missing; if ($a+$c > 0) {$FBI = ($a + $b)/($a + $c);}
               #Area index:
               my $AI = $missing;
               unless ($b == 0 or $c == 0) {

                  $AI=($a*$d-$b*$c)/(($b + $d)*($a + $c)) + $c / ($b + $d) *              
                       log( $nn * $c /(( $a+$c)*($c+$d))) + $b / ($a + $c) * 
                       log( $nn * $b /(( $b+$d)*($a+$b)));

                  if ( ($a*$d-$b*$c)/(($b + $d)*($a + $c)) < 0) {$AI=-$AI;}

	       }
	       # Symmetric Extreme Dependency Score (Hogan et al. 2009, QJRMS):
               my $SEDS = $missing;
               unless ( $a == 0 or $a == $nn ) {
                 $SEDS=( ( log( ($a+$b)/$nn ) + log( ($a+$c)/$nn ) ) / 
                         log($a/$nn) ) - 1;
               }
	       # Extremal Dependency Index
               # (Ferro and Stephenson, submitted to Wea. and Forecasting, 2010):
               my $EDI = $missing;
               unless ( $a == 0 or $b == 0 or ( $c == 0 and $d == 0 ) ) {
                 $EDI=( log($b/($b+$d)) - log($a/($a+$c)) ) / 
                      ( log($b/($b+$d)) + log($a/($a+$c)) );
               }
	       # Symmetric Extremal Dependency Index
               # (Ferro and Stephenson, submitted to Wea. and Forecasting, 2010):
               my $SEDI = $missing;
               unless ( $a == 0 or $b == 0 or $c == 0 or $d == 0 ) {
                 $SEDI=( log($b/($b+$d)) - log($a/($a+$c)) -
                         log(1-($b/($b+$d))) + log(1-($a/($a+$c))) ) /
                       ( log($b/($b+$d)) + log($a/($a+$c)) +
                         log(1-($b/($b+$d))) + log(1-($a/($a+$c)))  );
               }
               #Range: -1/3 to 1, 0 indicates no skill.   Perfect score: 1.
               #Characteristics: Measures the fraction of observed and/or forecast events that were correctly 
               #predicted, adjusted for hits associated with random chance (for example, it is easier to 
               #correctly forecast rain occurrence in a wet climate than in a dry climate). The ETS is often 
               #used in the verification of rainfall in NWP models because its "equitability" allows scores to
               #be compared more fairly across different regimes. Sensitive to hits. Because it penalises 
               #both misses and false alarms in the same way, it does not distinguish the source of forecast 
               #error.
               # ETS =  a-random_hits                            (a+b)(a+c) 
               #       ________________    , where random hits ________________
               #        a+b+c-random_hits                         a+b+c+d

	       my $ETS = $missing;
               unless ($nn == 0){
                   my $randomhit=($a+$b)*($a+$c)/($nn);
                   unless ($a+$b+$c-$randomhit == 0){$ETS = ($a-$randomhit)/($a+$b+$c-$randomhit);}
		 }

              #Observed frequency important weather:
               my $OFREQ = $missing; if ($nn > 0)  {$OFREQ=(($a+$c)/$nn);}
               #Modelled frequencey important weather:
               my $MFREQ = $missing;  if ($nn > 0) {$MFREQ=(($a+$b)/$nn);}
	   
               print SCOREFILE  " @thresholds[$class] $FAR $POD";
               print SCOREFILE2 "@thresholds[$class] $FAR $POD $FA $KUI $FBI $AI $SEDS $EDI $SEDI $ETS $OFREQ $MFREQ $nn \n";
	   } #loop over classes

           print SCOREFILE "\n";
           close (SCOREFILE);
           close (SCOREFILE2);

      next SCAN_FILE; }
 }
close FILE;

# Set colors for score plots
@colors  = ("-1","1","3","4","8","5","6","9","7");
@markers = ("-1","1","3","4","8","5","6","9","7");
#
# Wilson plot
$output_file = $prefix . $EXT[$ENV{OUTPUT_TYPE}] ;
&header("Contingency table");
&plot_wilson;

# False alarm rate
$output_file = "fr".$prefix . $EXT[$ENV{OUTPUT_TYPE}] ;
&header("False alarm rate") ;
&gen_plot('FAR',4);
&freq ;

#Kuiper skill score
$output_file = "k".$prefix . $EXT[$ENV{OUTPUT_TYPE}] ;
&header("Kupiers skill score");
&gen_plot('KSS',5);

#Frequency bias
$output_file = "fb".$prefix . $EXT[$ENV{OUTPUT_TYPE}] ;
&header("Frequency bias") ;
&gen_plot('Freq bias',6);

# Area index
$output_file = "ai".$prefix . $EXT[$ENV{OUTPUT_TYPE}] ;
&header("Area index") ;
&gen_plot('AI',7);

# SEDS
$output_file = "seds".$prefix . $EXT[$ENV{OUTPUT_TYPE}] ;
&header("Symmetric Extreme Dependency Score") ;
&gen_plot('SEDS',8);

# EDI
$output_file = "edi".$prefix . $EXT[$ENV{OUTPUT_TYPE}] ;
&header("Extremal Dependency Index") ;
&gen_plot('EDI',9);

# SEDI
$output_file = "sedi".$prefix . $EXT[$ENV{OUTPUT_TYPE}] ;
&header("Symmetric Extremal Dependency Index") ;
&gen_plot('SEDI',10);

# ETS
$output_file = "ets".$prefix . $EXT[$ENV{OUTPUT_TYPE}] ;
&header("Equitable threat score") ;
&gen_plot('ETS',11);

#Frequency
$output_file = "f".$prefix . $EXT[$ENV{OUTPUT_TYPE}] ;
&header("Frequency") ;
&freq ;

next SCAN_INPUT;}


#################################################################
#################################################################
#################################################################
sub plot {
   # plot the files
   system("gnuplot plot1.gp");
   print "Created:$output_file \n";
}
#################################################################
#################################################################
#################################################################
sub header {

    # Create header
    $len_head = scalar(@heading) ;
    $heading =$_[0].$heading[0];
    for ($i=1;$i<$len_head;$i++ ) { $heading=$heading."\\n  $heading[$i]"; } ;

    open GP, ">plot1.gp";

    print GP <<EOF;
$terminal
set output "$output_file"
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
EOF

# make labels for each experiment
$i=0;
foreach $exp (@enames) {
   $i++;
   $y=0.02+0.03*int(($i-1)/3);$x=0.01+(($i-1)%3)/3;
   print GP "set label '$exp' at $x,$y textcolor lt $colors[$i-1] \n";
 }

# make labels for the isolines:
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
close GP

&plot ;

}
#################################################################
#################################################################
#################################################################
sub gen_plot {

 ($yunit,$i) = @_ ;

print GP <<EOF;
set grid
set xlabel "$unit"
set ylabel "$yunit"
$xscale
EOF

$plot = "plot ";

    $f=-1;
    foreach (@workfiles2) {
	$f++;
        if ( $f gt 0 ) { $plot = "$plot,"; }
        $plot .="'$_' using 1:$i title '$enames[$f]' with linespoints lt $col_def_lt[$f+1] lw 2 pt 7";
    } ;

print GP "$plot";
close GP

&plot ;

}
#################################################################
#################################################################
#################################################################
sub freq{

print GP <<EOF;
set grid
set xlabel "$unit"
set ylabel "Frequency"
$xscale
EOF

$plot = "plot ";

    $f=-1;
    $plot .="'$workfiles2[0]' using 1:12 title 'OBS' with linespoints lt $col_def_lt[@workfiles2+1] lw 2 pt 7";
    foreach (@workfiles2) {
	$f++;
        $plot .=",'$_' using 1:13 title '$enames[$f]' with linespoints lt $col_def_lt[$f+1] lw 2 pt 7";
    } ;

print GP "$plot";
close GP

&plot ;

}
