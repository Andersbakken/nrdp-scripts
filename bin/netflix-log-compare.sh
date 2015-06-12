#!/usr/bin/perl

use strict;

sub parseTime
{
    my ($line) = @_;
    $line =~ /^([0-9]+):([0-9]+):([0-9]+)\.([0-9]+)/;
    return ($1 * 3600 * 1000) + ($2 * 60 * 1000) + ($3 * 1000) + $4;
}

my $file1name = $ARGV[0];
my $file2name = $ARGV[1];
open(file1, "<$file1name") || die "Can't open $file1name";
my @lines2;

open(file2, "<$file2name") || die "Can't open $file2name";
while (<file2>) {
    my %line;
    $line{original} = $_;
    chomp($line{original});
    $line{string} = $line{original};
    $line{string} =~ s,^[^ ]* [^ ]* ,,;
    # $line{string} =~ s,[0-9],_,g;
    push @lines2, \%line;
}

# die $#lines2;

for (my $i1=0; <file1>; ++$i1) {
    # print "Line: $i1\n";
    my $origline1 = $_;
    chomp($origline1);
    my $line1 = $origline1;
    $line1 =~ s,^[^ ]* [^ ]* ,,;
    # $line1 =~ s,[0-9],_,g;

    for (my $i2=0; $i2<$#lines2; ++$i2) {
        my $line = $lines2[$i2];
        # print "${origline2}\n";
        if ($line && $line1 eq $line->{string}) {
            my $time1 = parseTime($origline1);
            my $origline2 = $line->{original};
            my $time2 = parseTime($origline2);
            my $diff = $time2 - $time1;
            # print "---------------\nFound $i1 ${time1}ms at $i2 ${time2}ms = ${diff}ms\n$origline1\n$origline2\n";
            print "Found $i1 ${time1}ms at $i2 ${time2}ms = ${diff}ms --- $origline1\n";
            $lines2[$i2] = undef;
        }
    }
    print STDERR "$i1\n" unless ($i1 % 500);
    #die $line1;
}
