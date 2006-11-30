#!perl

print $#ARGV;
die "usage copy_arch.pl ARCH1 ARCH2" unless ($#ARGV==1);

$arch1=$ARGV[0];
$arch2=$ARGV[1];

system("tar cf $arch1.tar $arch1;mv $arch1 $arch2;tar xf $arch1.tar;rm -f $arch1.tar");
system("rm -f $arch2/lib/*.a");
system("rm -f $arch2/bin/*.x");
system("rm -f $arch2/config/*.h");
opendir DIR,"arch";
foreach (readdir DIR) {
    if (/$arch1/) {
	$n=$_;
	$n =~ s/$arch1/$arch2/g;
	system("cp arch/$_ arch/$n");
    }
}
1;

