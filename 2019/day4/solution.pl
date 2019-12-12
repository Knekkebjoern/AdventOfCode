#!/usr/bin/perl

use strict;
$|++;

my $start = 178416;
my $end = 676461;

my $cur = $start;
my $total = 0;

while ($cur <= $end) {
  $cur++;
  next if !increasing($cur);
  next if !doublechars($cur);
  $total++;
}
print "-> $total\n";

sub increasing {
  my @n = split(//, shift);

  foreach my $i (1..scalar(@n)-1) {
    return 0 if $n[$i] < $n[$i-1];
  }
  return 1;
}

sub doublechars {
  my @n = split(//, shift);

  my %d;
  my $p;
  my $c = 0;

  foreach my $x (@n) {
    if ($x == $p) {
      $c++;
    } else {
      return 1 if $c == 2;
      $c = 1;
    }
    $p = $x;
  }
  return 1 if $c == 2;
}
