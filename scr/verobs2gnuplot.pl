#!/usr/bin/perl
#

# Create gnuplot plots from verobs .txt files

SCAN_INPUT: foreach $input_file (@ARGV) {

    @legend = ();
    @column = ();
    @sfile  = ();
    @sint   = ();

    # Examine file name

    @tmp    = split( '_', $input_file );
    $prefix = shift(@tmp);
    @tmp    = split( '.txt', $input_file );

    # PS or PNG as output
    if ( $ENV{OUTPUT_TYPE} eq 1 ) {
        $output_file = shift(@tmp) . ".ps";
        $terminal    = "set terminal postscript landscape enhanced colour";
    }
    else {
        $output_file = shift(@tmp) . ".1.png";
        $terminal    = "set terminal png";
    }

    open FILE, "< $input_file";

SCAN_FILE: while (<FILE>) {
        chomp;
        if ( $_ =~ /#END/ )  { last SCAN_FILE; }
        if ( $_ =~ /#HEADING_1/ ) {
            $heading1 = substr( $_, 11 );
            next SCAN_FILE;
        }
        if ( $_ =~ /#HEADING_2/ ) {
            $heading2 = substr( $_, 11 );
            next SCAN_FILE;
        }
        if ( $_ =~ /#HEADING_3/ ) {
            $heading3 = substr( $_, 11 );
            next SCAN_FILE;
        }

        if ( $_ =~ /#HEADING_4/ ) {
            $heading4 = substr( $_, 11 );
            next SCAN_FILE;
        }
        if ( $_ =~ /#YLABEL/ ) { $ylabel = substr( $_, 8 ); next SCAN_FILE; }
        if ( $_ =~ /#XLABEL/ ) { $xlabel = substr( $_, 8 ); next SCAN_FILE; }
        if ( $_ =~ /#XMIN/   ) { @tmp = split (' ',$_ ) ; $xmin   = $tmp[1]; next SCAN_FILE; }
        if ( $_ =~ /#XMAX/   ) { @tmp = split (' ',$_ ) ; $xmax   = $tmp[1]; next SCAN_FILE; }
        if ( $_ =~ /#XMIN/   ) { @tmp = split (' ',$_ ) ; $ymin   = $tmp[1]; next SCAN_FILE; }
        if ( $_ =~ /#XMAX/   ) { @tmp = split (' ',$_ ) ; $ymax   = $tmp[1]; next SCAN_FILE; }
        if ( $_ =~ /#MISSING/ ) {
            $missing = substr( $_, 10 );
            next SCAN_FILE;
        }

        if ( $_ =~ /#COLUMN/ ) {
            @legend = ( @legend, substr( $_, 10 ) );
            @column = ( @column, substr( $_, 8, 3 ) );
            next SCAN_FILE;
        }
        if ( $_ =~ /#SLEVEL/ ) {
            @tmp = split( ' ', $_ );
            @sfile = ( @sfile, $tmp[1] );
            @sint  = ( @sint,  $tmp[2] );
            next SCAN_FILE;
        }
    }

    close FILE;

    # Start writing the plotting file

    # Print the header
    &header;

    # File type dependent options

PLOT_TYPES: {

        if ( $prefix =~ /ps/ || $prefix =~ /PS/ ) {
            &timeserie;
            last PLOT_TYPES;
        }
        if ( $prefix =~ /v/ || $prefix =~ /V/ ) {
            &gen_stat;
            last PLOT_TYPES;
        }
        if ( $prefix =~ /l/ || $prefix =~ /L/ ) {
            &plot_vert;
            last PLOT_TYPES;
        }
        if ( $prefix =~ /f/ || $prefix =~ /F/ ) {
            &plot_freq;
            last PLOT_TYPES;
        }
        if ( $prefix =~ /s/ || $prefix =~ /S/ ) {
            &plot_scat;
            last PLOT_TYPES;
        }

        print "Skip unknown file : $input_file \n";
        close GP;
        next SCAN_INPUT;

    }

    # Call gnuplot

    print GP "$plot";
    close GP;
    system("gnuplot plot.gp");

    print "Created: $output_file \n";

}
#################################################################
#################################################################
#################################################################
sub plot_command {

    $plot = "plot ";

    $i = -1;
    foreach (@legend) {
        $i++;
        if ( $i gt 0 ) { $plot = "$plot,"; }
        $plot = $plot . " '$input_file' using 1:" . $column[$i];
        if ( $_ =~ /CASES/ ) {
          $plot = $plot . " title '$legend[$i]' with linespoints lt 0 lw 2 axis x1y2 ";
        } else {
          $col_id=$i+1;
          $plot = $plot . " title '$legend[$i]' with linespoints lt $col_id lw 2 pt 7";
	}
    }

}
#################################################################
#################################################################
#################################################################
sub header {

    open GP, ">plot.gp";

    print GP <<EOF;
$terminal
set output '$output_file'
set missing "$missing"
set title "$heading1\\n \\$heading2\\n \\$heading3\\n \\$heading4 "

set xlabel "$xlabel"
set ylabel "$ylabel"
set timefmt "%Y%m%d %H"
set grid
EOF
}
#################################################################
#################################################################
#################################################################
sub timeserie {

    print GP <<EOF;
set y2range [0:]
set y2label "No cases"
set y2tics 0,1000
set xdata time
set format x "%d/%m\\n%H"
EOF

    &plot_command ;

}
#################################################################
#################################################################
#################################################################
sub gen_stat {

    print GP <<EOF;
set y2range [0:]
set y2label "No cases"
set y2tics 0,1000
EOF

    &plot_command ;

}
#################################################################
#################################################################
#################################################################
sub plot_vert {

    print GP <<EOF;
set yrange [10:1000] reverse
set x2range [0:]
set x2label "No cases"
set x2tics 0,300
EOF
    $plot = "plot ";

    $i = -1;
    foreach (@legend) {
        $i++;
        if ( $i gt 0 ) { $plot = "$plot,"; }
        $plot = $plot . " '$input_file' using " . $column[$i] . ":1";
        if ( $_ =~ /CASES/ ) {
          $plot = $plot . " title '$legend[$i]' with linespoints lt 0 lw 2 axis x2y1";
        } else {
          $col_id=$i+1;
          $plot = $plot . " title '$legend[$i]' with linespoints lt $col_id lw 2 pt 7";
        }
      }

}
#################################################################
#################################################################
#################################################################
sub plot_freq {
    &plot_command ;
EOF
}
#################################################################
#################################################################
#################################################################
sub plot_scat {
  
print GP <<EOF;
set key outside 
EOF
    $plot = "plot ";

    $i = -1;
    foreach (@sfile) {
        $i++;
        if ( $i gt 0 ) { $plot = "$plot,"; }
        $col_id=$i+1;
        $plot = $plot . " '$input_file"."_".$_."' title '$sint[$i]' lt $col_id ps 2 pt 7";
    }

}
