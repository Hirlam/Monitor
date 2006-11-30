#!/usr/bin/perl -w

use File::Find;
use Cwd;

$pwd=cwd();
find(\&wanted_mod,".");
find(\&wanted_use_inc,".");
find(\&wanted_use,".");

sub wanted_use_inc {
  if (((/\Z\.h/) || (/\Z\.inc/)) && !((/\Z\,v/) || (/\Z\.html/))) {
    open(FILE,"<$_");
    @inputfile=<FILE>;
    @match=grep(/^(\s*)use/i,@inputfile);
    foreach (@match) {
      s/,(\s*)only(.*)//i;
      s/(\s*)use(\s*)//i;
      s/(\s*)\!(.*)//i;
      chop;
    }
    $modulesinclude{$_}.=join " ",@match;
    print "$_ $modulesinclude{$_} \n";
    close(FILE);
  }
}

sub wanted_mod {
  if ((/\.F/) && (!/\Z\,v/)) {
    open(FILE,"<$_");
    @match=grep 
      (!/(procedure)|(call)|(subroutine)|(write)|(use)|(\Ac)|(\A\!)/i,
       grep(!/^(\s*)(end)/i,grep(s/module//i,<FILE>)));
    foreach (@match) {s/\s*//g;}
    if ($match[0]){
      $modules{$_}=join " ",@match;
      $modpath{$_}=cwd();
      $modpath{$_}=~s/$pwd/\$\(ROOTDIR\)\/\$\(OBJDIR\)/;
    }
    close(FILE);
  }
}

sub wanted_use {
  if ((/\.F/) && (!/\Z\,v/)) {
    open(OUTFILE,">>make.moddepend");
    open(FILE,"<$_");
    @inputfile=<FILE>;
    @match=grep(/^(\s*)use/i,@inputfile);
    foreach (@match) {
      s/,(\s*)only(.*)//i;
      s/(\s*)use(\s*)//i;
      s/(\s*)\!(.*)//i;
      chop;
    }
    @includedfiles=grep(s/\A(\#{0,1})include//,@inputfile);
    if ($includedfiles[0]) {
      print "$_ has the following includes\n";
      foreach $f (@includedfiles) {
	$f=~s/(\s*)\"(\s*)//g;
	print "$f\n";
	if (/F90/) {
	  push @match,split(" ",$modulesinclude{$f}),"\n";
	}
      }
      print "\n";
    }
    if ($match[0]){
      foreach $mod (@match) {
	foreach $key (keys %modules) {
	  if ( $mod =~ /$modules{$key}/i ) {
	    $objfile=$key;
	    $objfile=~s/\.F(90{0,1})/\.o/;
	    print OUTFILE "$_ : $modpath{$key}/$objfile\n";
	  }
	}
      }
    }
    close(FILE);
    close(OUTFILE);
  }
}
