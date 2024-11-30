#!/usr/bin/perl

use strict;
$|++;

my $year = 2024;
foreach my $day (1..25) {
    my $filepath = "./src/aoc/year${year}/day${day}.clj";
    if (-e $filepath) {
	print "# Already exists: $filepath\n"
    } else {
	print "# Creat $filepath\n";
	print "cp template.clj $filepath\n";
	print "sed -i -e 's/%year%/${year}/g' $filepath\n";
	print "sed -i -e 's/%day%/${day}/g' $filepath\n";
    }
}
