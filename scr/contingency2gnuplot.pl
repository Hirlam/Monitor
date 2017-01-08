#!/usr/bin/perl
#
# Create gnuplot plots from contingency_tables in .html files
#
# Usage: contingency2gnuplot.pl *.html, where *.html is the html-files produced by 
# The HARMONIE verification.
#
# Author: Calle Fortelius, June 2009, based on verobs2gnuplot.pl
#

push @INC, "$ENV{SCR}";
require skilldefs ;

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
    if ( $partag eq "PE12" ) { $xscale="set logscale x" ; } ;
    if ( $partag eq "PE6" ) { $xscale="set logscale x" ; } ;
    if ( $partag eq "PE3" ) { $xscale="set logscale x" ; } ;
    if ( $partag eq "PE1" ) { $xscale="set logscale x" ; } ;
    if ( $partag eq "PE" ) { $xscale="set logscale x" ; } ;
    if ( $partag eq "CH" ) { $xscale="set logscale x" ; } ;

    @EXT = ('','.ps','.1.png','.1.jpg','.svg') ;

    # PS,PNG,JPG or SVG as output
    if ( $ENV{OUTPUT_TYPE} ) { $output_type = $ENV{OUTPUT_TYPE} }else { $output_type=1; } ;
    if ( $output_type eq 1 ) {
        $terminal    = "set terminal postscript landscape enhanced colour";
    } elsif ( $output_type eq 2 ) {
        $terminal    = "set terminal png";
    } elsif ( $output_type eq 3 ) {
        $terminal    = "set terminal jpeg";
    } elsif ( $output_type eq 4 ) {
        $terminal    = "set terminal svg enhanced fsize 8 ";
    } else {
        die "Unknown OUTPUT_TYPE $output_type\n";
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

        if ( $line =~ /Used / )   {
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
           # correspondiong to the respective thresholds. The events
           # to be verified are as follows:
           #
           #    1. classwise verification ($seletor=classwise)
           #    class=0:                   datavalue <= $threshold[0]
           #    class=i,i=1, .. classes-1: $threshold[i] < datavalue <= $threshold[i+1]
           #    class=classes:             $threshold[classes] < value
           #
           #    2. thresholdwise verification ($seletor=thresholdwise)
           #    class=i,i=0, .. classes: datavalue > $threshold[i]

	   $datascan=0;
           $classes=scalar(@thresholds);
           push(@workfiles, $prefix . "_" . $exp . ".scores");
           push(@workfiles2,$prefix . "_" . $exp . ".scores2");

           open (SCOREFILE, ">$workfiles[@workfiles-1]");
           open (SCOREFILE2,">$workfiles2[@workfiles2-1]");

           for (my $class=0; $class <= $classes; $class++){

               my $a = 0;my $b = 0;my $c = 0;my $d = 0;

               # forecast\obs observed not obs
               # forecast        a       b
               # not forecast    c       d

               # selector can have values "classes" or "thresholds"
               $selector='classes';
               if ( $ENV{SELECTOR} ) { $selector = $ENV{SELECTOR} };

               if ($selector eq 'thresholds'){
                  for (my $o=$class+1; $o <= $classes; $o++){ # obs yes; fc yes
                  for (my $f=$class+1; $f <= $classes; $f++){ $a = $a + $AoA[$f][$o] }}

                  for (my $o=0; $o <= $class; $o++){            # obs no; fc yes
                  for (my $f=$class+1; $f <= $classes; $f++){ $b = $b + $AoA[$f][$o] }}

                  for (my $o=$class+1; $o <= $classes; $o++){ # obs yes; fc no
                  for (my $f=0; $f <= $class; $f++){ $c = $c + $AoA[$f][$o] }}
 
                  for (my $o=0; $o <= $class; $o++){           # obs no; fc no
                  for (my $f=0; $f <= $class; $f++){ $d = $d + $AoA[$f][$o] }}
	       }
	       elsif ($selector eq 'classes'){
                  $a=$AoA[$class][$class];

                  for (my $o=0; $o <= $classes; $o++){ 
                      unless ($o == $class) {$b = $b + $AoA[$class][$o];}
                  }

                  for (my $f=0; $f <= $classes; $f++){
                      unless ($f == $class){$c = $c + $AoA[$f][$class];}
                  }

                  for (my $o=0; $o <= $classes; $o++){           # obs no; fc no
                  for (my $f=0; $f <= $classes; $f++){ 
                     unless ($o == $class or $f == $class) {$d = $d + $AoA[$f][$o];} 
  
                 }}
		}
               else {die "Your chosen selector: $selector is not supported\n"}
               my $nn=$a+$b+$c+$d;
               my $n2=2;

               #False alarm RATIO:
               my $FAR = $missing; if ($a+$b > 0) {$FAR = $b/($a+$b);}
               #Probability of detection:
               my $POD = $missing; if ($a+$c > 0) {$POD = $a/($a+$c);}
               #False alarm RATIO Complimentary class :
               my $FARC = $missing; if ($c+$d > 0) {$FARC = $c/($c+$d);}
               #Probability of detection Complimentary class :
               my $PODC = $missing; if ($d+$b > 0) {$PODC = $d/($d+$b);}
               #False alarm RATE:
               my $FA  = $missing; if( $b+$d > 0) {$FA  = $b/($b+$d);}
               #Kuipers index:
               my $KUI = $missing; if ($a+$c > 0 and $b+$d > 0) {$KUI = ($a*$d-$b*$c)/(($b + $d)*($a + $c));}
               # Frequency bias
               my $FBI = $missing; if ($a+$c > 0) {$FBI = ($a + $b)/($a + $c);}
               # Threat score
               my $TS  = $missing; if ($a+$b+$c > 0) {$TS = $a/($a+$b+$c);}
               # Correlation or square root of Doolittle first score :
#               my $COR  = $missing; if ($a+$b+$c+$d > 0) {$COR = ($a*$d-$b*$c)*($a*$d-$b*$c)/(($b + $d)*($a + $c)*($a + $b)*($c + $d));}
               my $COR  = $missing; if ( ($b + $d)*($a + $c)*($a + $b)*($c + $d) > 0) {$COR = ($a*$d-$b*$c)/sqrt(($b + $d)*($a + $c)*($a + $b)*($c + $d));}
               # Heidke skill score or Doolittle second score :
               my $HEI  = $missing; if ( ($a + $b)*($b + $d) + ($a + $c)*($c + $d) > 0) {$HEI = 2*($a*$d-$b*$c)/(($a + $b)*($b + $d) + ($a + $c)*($c + $d));}
               #Area index:
               my $AI = $missing;
	       unless ($KUI == $missing) {
		 my $ZAIB = 0;
		 my $ZAIC = 0;
		 if( $b > 0 ) {$ZAIB = $b / ($a + $c) *
                       log( $nn * $b /(( $b+$d)*($a+$b))); }
		 if( $c > 0 ) {$ZAIC = $c / ($b + $d) *
                       log( $nn * $c /(( $a+$c)*($c+$d))); }
                 $AI = $KUI + $ZAIB + $ZAIC ;

                 if ( ($a*$d-$b*$c)/(($b + $d)*($a + $c)) < 0) {$AI=-$AI;}

	       }
	       # Symmetric Extreme Dependency Score (Hogan et al. 2009, QJRMS):
               my $SEDS = $missing;
               unless ( $a == 0 or $a == $nn ) {
                 $SEDS=( ( log( ($a+$b)/$nn ) + log( ($a+$c)/$nn ) ) / 
                         log($a/$nn) ) - 1;
               }
	       # Extreme Dependency Score (Stephenson et al. 2008, Meteorol. Appl., 15, 41-50.):
               my $EDS = $missing;
               unless ( $a == 0 or $a == $nn ) {
                 $EDS=( ( 2*log( ($a+$b)/$nn ) ) / log($a/$nn) ) - 1;
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
	   
               my $centralx=$missing; 
               my $lowerlimit=$missing;
               if($class == 0){ 
                  if($xscale ne "set logscale x"){
                      $centralx=$thresholds[$class]-0.5*abs($thresholds[$class]-$thresholds[$class+1]);
                      $lowerlimit=$thresholds[$class]; }
                  else{
		    if ($thresholds[$class] <= 0){
                        die "\n \n ERROR: \n Your limit of $thresholds[$class] for variable $partag \n does not allow a logarithmic scaling \n \n \n ";}
                      $centralx=$thresholds[$class]*0.3;
                      $lowerlimit=$thresholds[$class]; }
               } 
               elsif($class == $classes){
                   $centralx=$thresholds[$class-1]+0.5*abs($thresholds[$class-1]-$thresholds[$class-2]);
                   $lowerlimit=$missing;
               }
               else {
                   $centralx=0.5*($thresholds[$class-1]+$thresholds[$class]);
                   $lowerlimit=$thresholds[$class];
	       }
	     
               print SCOREFILE  "$lowerlimit $FAR $POD ";
               print SCOREFILE2 "$centralx $lowerlimit $FAR $POD $FA $KUI $FBI $AI $SEDS $EDI $SEDI $ETS $OFREQ $MFREQ $nn $TS $EDS $FARC $PODC $COR $HEI \n";
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
if ($ENV{'SCORELIST'}=~'WILSON'){
$ctype = 'WILSON';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&plot_wilson; }

# False alarm rate
if ($ENV{'SCORELIST'}=~'FA'){
$ctype = 'FA';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},5); }

# False alarm ratio
if ($ENV{'SCORELIST'}=~'FAR'){
$ctype = 'FAR';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},3); }

# False alarm ratio complimentary class
if ($ENV{'SCORELIST'}=~'FARC'){
$ctype = 'FARC';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},18); }

#Kuiper skill score
if ($ENV{'SCORELIST'}=~'KSS'){
$ctype = 'KSS';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},6); }

#Frequency bias
if ($ENV{'SCORELIST'}=~'Frequencybias'){
$ctype = 'Frequencybias';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},7); }

# Area index
if ($ENV{'SCORELIST'}=~'AI'){
$ctype = 'AI';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},8); }

# EDS
if ($ENV{'SCORELIST'}=~'EDS'){
$ctype = 'EDS';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},17); }


# SEDS
if ($ENV{'SCORELIST'}=~'SEDS'){
$ctype = 'SEDS';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},9); }

# EDI
if ($ENV{'SCORELIST'}=~'EDI'){
$ctype = 'EDI';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},10); }

# SEDI
if ($ENV{'SCORELIST'}=~'SEDI'){
$ctype = 'SEDI';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},11); }

# ETS
if ($ENV{'SCORELIST'}=~'ETS'){
$ctype = 'ETS';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},12); }

# TS
if ($ENV{'SCORELIST'}=~'TS'){
$ctype = 'TS';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},16); }

# POD
if ($ENV{'SCORELIST'}=~'POD'){
$ctype = 'POD';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},4); }

# PODC
if ($ENV{'SCORELIST'}=~'PODC'){
$ctype = 'PODC';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},19); }

# COR
if ($ENV{'SCORELIST'}=~'COR'){
$ctype = 'COR';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},20); }

# HEI
if ($ENV{'SCORELIST'}=~'HEI'){
$ctype = 'HEI';
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&gen_plot($skill_score_txt{$ctype},21); }

#Frequency
if ($ENV{'SCORELIST'}=~'Frequency'){
$ctype = 'Frequency';
$output_file = "f".$prefix."_". $selector . $EXT[$output_type] ;
$output_file = $skill_score_def{$ctype}.$prefix."_". $selector . $EXT[$output_type] ;
&header($skill_score_txt{$ctype}) ;
&freq ; }

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
set datafile missing "$missing"
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
   #print GP "set label '$exp' at $x,$y textcolor lt $colors[$i-1] \n";
   print GP "set label '$exp' at $x,$y textcolor lt $col_def_lt[$i] \n";
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
   #$f++; $linetype=$colors[$f];
   $f++; $linetype=$col_def_lt[$f + 1];
   for(my $t=0; $t <= $classes; $t++){
     $x=3*$t+2; $y=3*$t+3;

     if($f eq 0){
       if ($selector eq 'classes'){
 
        if($t == 0){
           @xx = split('\.',$thresholds[$t]);
           $upper=$xx[0] . "." .  substr($xx[1],0,2);
           $lower="<= ";
       }
       elsif($t == $classes){
           @xx = split('\.',$thresholds[$t-1]);
           $upper=$xx[0] . "." .  substr($xx[1],0,2);
           $lower="> ";
       }
       else{
           @xx = split('\.',$thresholds[$t-1]);
           $lower=$xx[0] . "." .  substr($xx[1],0,2) . " ... ";
           @xx = split('\.',$thresholds[$t]);
           $upper=$xx[0] . "." .  substr($xx[1],0,2);
	 }        
       }
       elsif ($selector eq 'thresholds'){
         $lower=""; $upper="";
         if ($t < $classes){
           @xx = split('\.',$thresholds[$t]);
           $upper=$xx[0] . "." .  substr($xx[1],0,2);
           $lower="> ";
       }
       }

     $title=$lower . $upper . " $unit";
     }
     else {$title="";}
     &ptitle ;
     my $tplus=$t+1;
     print GP <<EOF;
     '$workfiles[$f]' using $x:$y title '$title' with points pointtype $tplus lt $linetype, \\
EOF
   }}
# The next lines will add the frequency bias and the threat score to the plot:
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
set xlabel "$selector $unit"
set ylabel "$yunit"
$xscale
EOF
my $hgt=0.05;
my $ctr=0;
my $xcolumn=0;
if ($selector eq 'classes') {$xcolumn=1}
elsif ($selector eq 'thresholds') {$xcolumn=2}

foreach $threshold (@thresholds){
   $ctr++;
   print GP " \n set arrow  $ctr from  $threshold,graph $hgt to  $threshold,graph 0.01 lt -1 lw 2";
}
$plot = "\n plot ";

    $f=-1;
    foreach (@workfiles2) {
	$f++;
        if ( $f gt 0 ) { $plot = "$plot, "; }
        $title = $enames[$f];
        &ptitle ;
        $plot .="'$_' using $xcolumn:$i title '$title' with linespoints lt $col_def_lt[$f+1] lw 2 pt 7";
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
set xlabel "$selector $unit"
set ylabel "Frequency"
$xscale
EOF
my $hgt=0.05;
my $ctr=0;
my $xcolumn=0;
if ($selector eq 'classes') {$xcolumn=1}
elsif ($selector eq 'thresholds') {$xcolumn=2}
foreach $threshold (@thresholds){
   $ctr++;
   print GP " \n set arrow  $ctr from  $threshold,graph $hgt to  $threshold,graph 0.01 lt -1 lw 2";
}
$plot = "\n plot ";

    $f=-1;
    $plot .="'$workfiles2[0]' using $xcolumn:13 title 'OBS' with linespoints lt $col_def_lt[@workfiles2+1] lw 2 pt 7";
    foreach (@workfiles2) {
	$f++;
        $title = $enames[$f];
        &ptitle ;
        $plot .=",'$_' using $xcolumn:14 title '$title' with linespoints lt $col_def_lt[$f+1] lw 2 pt 7";
    } ;

print GP "$plot";
close GP

&plot ;

}
#################################################################
#################################################################
#################################################################
sub ptitle{ if ( $output_type == 1 ) { $title =~s/_/\\_/g ; } ; }
