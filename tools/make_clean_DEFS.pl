#!/pack/local/bin/perl -w

system('grep \#if */*/*.[Ffc]* >input.fil');

open IN,"<input.fil";
@input=<IN>;
close IN;

$nummatch=0;
foreach $file (@input) {
    $nummatch++;
#($dum,$define)=split("def",$file);

    ($filename,$defines)=split(":",$file);
    $tmp=$defines;
    $tmp=~s/ //g;
#    print substr($tmp,0,1),"\n";
    if (substr($tmp,0,1) eq "#") {
	$defines=~s/#ifdef//g;
	$defines=~s/#ifndef//g;
	$defines=~s/\(/ /g;
	$defines=~s/\)/ /g;
	$defines=~s/defined/ /g;
	$defines=~s/\&/ /g;
	$defines=~s/\!/ /g;
#    print $defines;
	(@subdefs)=split(" ",$defines);
	foreach $subdef (@subdefs) {
	    $subdef=~s/#if//g;
	    $subdef=~s/\ //g;
	    $subdef=~s/\n//g;
	    $subdef=~s/\|//g;
	    if (length($subdef) >0 )
	    {
		$filename=~s/\.F90/\.o/;
		$filename=~s/\.F/\.o/;
		$filename=~s/\.f/\.o/;
		$filename=~s/\.c/\.o/;
#	    print $filename," ",$subdef,"\n";
		$isdeffiles{$subdef}.=$filename." ";
	    }
	}
    }
}

foreach $defines (sort keys %isdeffiles){
    @files=split(" ",$isdeffiles{$defines});
    print "clean_$defines :\n";
    for ($i=0;$i<=$#files;$i++) {
	if ($i>0) {
	    if ($files[$i] ne $files[$i-1]) {
		print "\t-\$(RM) $files[$i]\n";
	    }
	} else {
	    print "\t-\$(RM) $files[$i]\n";
	}
    }
    print "\n";
}


