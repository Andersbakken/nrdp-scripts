#!/usr/bin/perl

my @options;
my $choose;
my $argv0 = $ARGV[0];
my $show_help = 0;
my $read_option = 1;
my $read_prompt;
my $must_ask = 0;
my $unique = 0;
my $rest_options = 0;
my $options_from=0;
my $options_count;
my @matches;

sub choose_show {
    my(@options) = @_;
    my $counter = 1;
    printf STDERR "======================= ($options_from-" . ($options_from + $options_count) . "/" . ($#options+1) . ")\n";
    for(my $i = $options_from; $i < ($#options + 1) && (!$options_count || $counter <= $options_count); ++$counter, ++$i) {
        my $option = $options[$i];
        my $name = $option->{"name"};
        my $result = $option->{"result"};
        if ($name eq $result) {
            print STDERR "[$counter] $name\n";
        } else {
            print STDERR "[$counter] $name [$result]\n";
        }
    }
    return $counter;
}

sub filter_options {
    my @result = @options;
    if(@matches) {
        my @match_options;
        for(@result) {
            my $match = 0;
            my $option = $_;
            my $name = $option->{"name"};
            my $result = $option->{"result"};
            for(@matches) {
                #print STDERR "name $result $_\n";
                if ( $name =~ /$_/i ) {
                    ++$match;
                } elsif ( $result =~ /$_/i ) {
                    ++$match;
                }
            }
            #print STDERR "$match $#matches $name\n";
            push( @match_options, $option ) if($match == $#matches+1);
        }
        @result = @match_options;
    }
    return @result;
}

sub unique_options {
    my %seen = ();
    my @result;
    for(@options) {
        my $option = $_;
        my $name = $option->{"name"};
        if (!$seen{$name}) {
            $seen{$name} = 1;
            push @result, $option;
        }
    }
    return @result;
}

while(@ARGV) {
    my $option = shift @ARGV;
    if($rest_options || !($option =~ /^-/)) {
        my %o;
        $o{"name"} = $option;
        $o{"result"} = $option;
        push(@options, \%o);
    } elsif($option eq "-r") {
        my $name = shift @ARGV;
        my $result = shift @ARGV;
        my %o;
        $o{"name"} = $name;
        $o{"result"} = $result;
        push(@options, \%o);
    } elsif($option eq "-e") {
        $rest_options = 1;
    } elsif($option eq "-c") {
        $choose = shift @ARGV;
    } elsif($option eq "-u") {
        $unique = 1;
    } elsif($option eq "-m") {
        push(@matches, shift @ARGV);
    } elsif($option eq "-a") {
        $must_ask = 1;
    } elsif( $option eq "-h"  || $option eq "--help" || $option eq "-help" ) {
        $show_help = 1;
    } elsif( $option eq "-x" ) {
        my $exe = shift @ARGV;
        my $kid = open( EXE, "$exe |");
	$kid || die "Failure";
	(kill 0, $kid) || die "Failure";
        while(my $line = <EXE>) {
            chomp($line);
            my %o;
            $o{"name"} = $line;
            $o{"result"} = $line;
            push(@options, \%o);
        }
        close( EXE );
	$? && die "Failure";
    } elsif( $option =~ /^-\+([0-9]+)$/) {
        $options_from = "$1"
    } elsif( $option =~ /^-([0-9]+)$/) {
        $options_count = "$1"
    } elsif( $option eq "-p" ) {
        $read_prompt = shift @ARGV;
    } elsif( $option eq "-l" ) {
        $read_option = 0;
    }
}
if(!$show_help && $#options < 0) {
    print STDERR "Nothing to choose!\n";
    exit 1;
}
@options = unique_options() if ($unique);
if($show_help) {
    print STDERR "$argv0 [options] [choices]\n";
    print STDERR "\n";
    print STDERR "Options:\n";
    print STDERR "  -a        Always ask (don't default to first if only one)\n";
    print STDERR "  -c num    Just return the <num>th result.\n";
    print STDERR "  -e        Stop interpreting options.\n";
    print STDERR "  -m <s>    Only show choices that match <s>.\n";
    print STDERR "  -l        Just print options don't query.\n";
    print STDERR "  -u        Ensure unique options.\n";
    print STDERR "  -p <msg>  Display <msg> in prompt.\n";
    print STDERR "  -r <key> <value> Show key and return value.\n";
    print STDERR "  -x <exe>  Run <exe> to gather the list of choices.\n";
    exit 1;
}

my $result;
if($read_option) {
    if(defined($choose)) {
        $result = $options[$choose-1]->{"result"};
    } elsif($#options >= 0) {
        $| = 1;
        while(!$result) {
            my @choices = filter_options(@options);
            if($#choices < 1 && !$must_ask) {
                $result = $choices[0]->{"result"};
            } else {
                my $counter = choose_show( @choices );
                print STDERR "[1..", $counter-1, "]";
                print STDERR "($read_prompt)" if(defined($read_prompt));
                print STDERR "> ";
                my $input = <STDIN>;
                chomp $input;
                if( $input =~ /^([1-9]+[0-9]*)$/ ) {
                    $result = $choices[$options_from+$1-1]->{"result"}
                } elsif( $input eq "n" && $options_count ) {
                    $options_from += $options_count;
                } elsif( $input eq "p" && $options_count ) {
                    $options_from -= $options_count;
                } else {
                    push(@matches, $input);
                }
            }
        }
    }
} else {
    if(defined($choose)) {
        @options = ( $options[$choose-1] );
    }
    if($#options < 1 && !$must_ask) {
        $result = $options[0]->{"result"};
    }
    choose_show( @options );
}
if($result) {
    print "$result\n";
    exit 0;
}
exit 1;

