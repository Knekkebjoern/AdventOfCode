#!/usr/bin/perl

use strict;
use POSIX;

$|++;

my $t = 0;

while(my $line = <>) {
  my $fuel = 0;
  my $a_total = 0;
  my $b_total = 0;
  chomp $line;
  $fuel = (floor(int($line)/3)-2);
  $a_total += $fuel;

  while ($fuel > 0) {
    $fuel = (floor(int($fuel)/3)-2);
    $b_total += $fuel if $fuel > 0;
  }

  $t += ($a_total+$b_total)
}

print "Total: $t\n";
