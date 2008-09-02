#!/usr/bin/perl
#

# Create gnuplot plots from verobs .txt files

SCAN_INPUT: foreach $input_file (@ARGV) {

    @legend = ();
    @column = ();

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
        if ( $_ =~ /#MISSING/ ) {
            $missing = substr( $_, 10 );
            next SCAN_FILE;
        }

        if ( $_ =~ /#COLUMN/ ) {
            @legend = ( @legend, substr( $_, 10 ) );
            @column = ( @column, substr( $_, 8, 3 ) );
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

        print "Skip unknown file : $input_file \n";
        close GP;
        next SCAN_INPUT;

    }

    # Call gnuplot

    print GP "$plot";
    system("gnuplot plot.gp");
    close GP;

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
        $plot = $plot . " title '$legend[$i]' with linespoints lt $i lw 2";
	print "CASES $_\n";
        if ( $_ =~ /CASES/ ) {
	  $plot = $plot . " axis x1y2 ";
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

# right axis definitions-------------

set y2range [0:]
set y2label "No cases"
set y2tics 0,1000
# ------------------------------------

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
set xdata time
set format x "%d/%m\\n%H"
EOF

    &plot_command;

}
#################################################################
#################################################################
#################################################################
sub gen_stat {

    &plot_command;

}
#################################################################
#################################################################
#################################################################
sub plot_vert {

    print GP <<EOF;
set yrange [10:1000] reverse
EOF
    $plot = "plot ";

    $i = -1;
    foreach (@legend) {
        $i++;
        if ( $i gt 0 ) { $plot = "$plot,"; }
        $plot = $plot . " '$input_file' using " . $column[$i] . ":1";
        $plot = $plot . " title '$legend[$i]' with linespoints lt $i lw 2";
      }

}
