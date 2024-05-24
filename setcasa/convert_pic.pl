#! /bin/perl 

my @f = glob "reduced/*.jpg";

my @one;
my @eight; 
foreach my $f (@f){
	my $cmd = 'identify -format \'%[exif:orientation]\' ' . $f;
	my $out = `$cmd`;	
	if ($out == '1'){push @one, $f;}
	elsif ($out == '8'){push @eight, $f;}
	else {print "$f\t$out\n";}
	#print scalar @one, "; ", scalar @eight, "\n";
}

foreach my $f (@one){
	my $str = "<img src=\"$f\" style=\"width:32%\">";
	print "$str\n";
}


foreach my $f (@eight){
	my $str = "<img src=\"$f\" style=\"width:32%\">";
	print "$str\n";
}


