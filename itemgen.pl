#!/usr/bin/perl


if (!$ARGV[1]) { print "usage: itemgen.pl <num. of items> <output file>\n"; exit; }

$file = $ARGV[1];
open(DATAFILE, ">$file");
print "Writing to: $file\n";

$capacity = 0;
$total_val = 0;
for ($i=0; $i < $ARGV[0]; $i++) {
    $rv = int (rand 200);
    $rw = int (rand 400);
    print "\"Item $i\" $rw $rv\n";
    print DATAFILE "\"Item $i\" $rw $rv\n";
    $capacity = $capacity + $rw;
    $total_val = $total_val + $rv;
}
$target = int (rand $capacity);
print "Max capacity: $capacity\nTarget capacity: $target\n";
print "Total value: $total_val\n";
