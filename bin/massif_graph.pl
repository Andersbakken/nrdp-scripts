#!/usr/bin/perl 
#
#This will convert the output of ms_print (massif 3.3.0) to a simple
#png graph representing heap info this uses gd::graph, and roughly
#represents the old .ps that massif (<3.3.0) used to generate. --Sam
#
# I tend to run like:
# cat nf.massif.out | c++filt >nf.massif.out.filt; ms_print --threshold=0.1 nf.massif.out.filt >nf.massif.ms_print
# massif_graph nf.massif.ms_print -blame library 

use strict;
use GD::Graph::area;
use GD::Graph::bars;
use GD::Graph::lines;
use GD::Graph::colour qw(:colours :lists :files :convert);

#input
die "Please provide ms_print output" if( $#ARGV < 0 );
my $ms_print = $ARGV[0]; 
unless( $ms_print =~ '/.ms_print$' ) {
  my $real_ms_print = "${ms_print}.ms_print";
  system("cat ${ms_print} | c++filt >${ms_print}.filt; ms_print --threshold=0.01 ${ms_print}.filt >${real_ms_print}") unless(-e "$real_ms_print");
  $ms_print = $real_ms_print;
}

#output options
my $chart_last = undef; #undef means chart all snapshots
my $chart_limit = undef; # must grow by this much % between two sample points
my $chart_width = 2048;
my $chart_height = 768;
my $print_all = 0;
my $chart_output = "chart.png";
my $chart_blame = "symbol";
my $chart_type = "area";
my $chart_points = "absolute";
my %chart_filter;
my @chart_label;
my @chart_ignore;
push @chart_ignore, "operator new";
for( my $a = 1; $a < $#ARGV+1; ++$a ) {
  my $arg = $ARGV[$a];
  if( $arg eq "-blame" ) {
    $chart_blame = $ARGV[++$a];
  } elsif( $arg eq "-type" ) {
    $chart_type = $ARGV[++$a];
  } elsif( $arg eq "-limit" ) {
    $chart_limit = $ARGV[++$a];
  } elsif( $arg eq "-points" ) {
    $chart_points = $ARGV[++$a];
  } elsif( $arg eq "-width" ) {
    $chart_width = $ARGV[++$a];
  } elsif( $arg eq "-height" ) {
    $chart_height = $ARGV[++$a];
  } elsif( "$arg" eq "-ignore" ) {
    push @chart_ignore, $ARGV[++$a];
  } elsif( "$arg" eq "-label" ) {
    push @chart_label, $ARGV[++$a];
  } elsif( "$arg" eq "-filter" ) {
    my $filter = $ARGV[++$a];
    if( $filter =~ /^include:/ || $filter =~ /^exclude:/ ) {
      $chart_filter{$filter} = 1;
    } else {
      $chart_filter{"exclude:all"} = 1;
      $chart_filter{"include:" . $filter} = 1;
    }
  } elsif( $arg eq "-last" ) {
    $chart_last = $ARGV[++$a];
    $chart_last = undef if( $chart_last eq "all" );
  } else {
    $chart_output = $arg;
  }
}

sub addr2line {
  my ($library, $addr, $default) = @_;
  my $ret = $default;
  if( open( ADDR2LINE, "addr2line -e $library $addr|" ) ) {
    my $line = <ADDR2LINE>;
    chomp( $line );
    $ret = $line unless( $line eq "??:0" );
    close( ADDR2LINE );
  }
  print( "ADDR2LINE: $library [$addr] == $ret\n" );
  return $ret;
}

#here goes...
my $current_snapshot = undef;
my @snapshots;
my %blames;
my %snapshot_data;
if ( open( MS_PRINT, "<$ms_print" ) ) {
  #collect data
  my $line_no = 0;
  my $ignore_depth = 0;
  LINE: while(my $line = <MS_PRINT>) {
    ++$line_no;
    chomp($line);
    if ( $line =~ /^ *([0-9]+) +[0-9,]+ +([0-9,]+)/ ) {
      $current_snapshot = $1;
      my $allocated = $2;

      $current_snapshot += 0;

      $allocated =~ s/,//g;
      $allocated += 0;
      $snapshot_data{$current_snapshot}{"ALLOCATED"} = $allocated;
      $snapshot_data{$current_snapshot}{"LINES"} = $line_no;
      #push @snapshots, $current_snapshot;
    } elsif( $line =~ /^((\| )*)->([0-9]*(\.[0-9]+))% +\(([0-9,]+).\) +(0x.*): +(.*)/ ) {
      my $depth = $1;
      my $percent = $3;
      my $size = $5;
      my $addr = $6;
      my $location = $7;
      my $symbol;
      my $library;
      my $blame = $location;
      my $charge_count = ($depth =~ tr/\|// );
      if( !$charge_count ) {
	$ignore_depth = 0;
      } elsif( $charge_count > $ignore_depth ) {
	next LINE;
      }
      if( $blame =~ /^(.*) \(in (.*)\)$/ ) {
	$symbol = $1;
	$library = $2;
	if( $chart_blame eq "symbol" ) {
	  $blame = $symbol;
	} elsif( $chart_blame eq "library" ) {
	  $blame = $library;
	} elsif( $chart_blame eq "line" ) {
	  $blame = addr2line( $library, $addr, "unknown" );
	}
      } elsif( $blame =~ /^\(within (.*)\)$/ ) {
	$symbol = $addr;
	$library = $1;
	if( $chart_blame eq "library" ) {
	  $blame = $library;
	} elsif( $chart_blame eq "symbol" ) {
	  $blame = $addr;
	  #$blame = addr2line( $library, $addr, $addr );
	} elsif( $chart_blame eq "line" ) {
	  $blame = addr2line( $library, $addr, "unknown" );
	}
      }
      #do some filtering
      foreach ( @chart_ignore ) {
	my $ignore = $_;
	if( (defined( $blame ) && $blame =~ /$ignore/ ) ||
            (defined( $symbol ) && "symbol:$symbol" =~ /$ignore/ ) ||
            (defined( $library ) && "library:$library" =~ /$ignore/ ) ||
            (defined( $addr ) && "addr:$addr" =~ /$ignore/ ) ) {
	  #print("Ignore($ignore) $blame $library $symbol $addr\n" );
	  ++$ignore_depth;
	  next LINE;
	}
      }
      if( defined( %chart_filter ) ) {
	if ( 
	    ( $chart_filter{"exclude:all" } ||
	      (defined( $symbol ) && $chart_filter{"exclude:symbol:" . $symbol}) ||
	      (defined( $library ) && $chart_filter{"exclude:library:" . $library}) ||
	      (defined( $addr ) && $chart_filter{"exclude:addr:" . $addr}) ) &&
	    !( (defined( $symbol ) && $chart_filter{"include:symbol:" . $symbol}) ||
	       (defined( $library ) && $chart_filter{"include:library:" . $library}) ||
	       (defined( $addr ) && $chart_filter{"include:addr:" . $addr}) ) ) {
	  #print("Filter) $symbol $library $addr\n" );
	  my $label;
	  $label = "other";
	  next LINE unless( $label );
	}
	#print("NoFilter) $symbol $library $addr\n" );
      }
      #labels
      foreach( @chart_label ) {
	my @label = split(/:/);
	if( $blame =~ /$label[0]/ ) {
	  $blame = $label[1];
	  last;
	}
      }

      $size =~ s/,//g;
      $size += 0;
      $percent += 0;
      $snapshot_data{$current_snapshot}{"COUNTED"} += $size;
      $blames{$blame} = 1; #record it

      my $snapshot = $snapshot_data{$current_snapshot}{$blame};
      if( !defined( $snapshot ) ) {
	my %tmp_snapshot;
	$snapshot_data{$current_snapshot}{$blame} = \%tmp_snapshot;
	$snapshot = $snapshot_data{$current_snapshot}{$blame};
      }
      $snapshot->{"percent"} += $percent;
      $snapshot->{"size"} += $size;
      $snapshot->{"blame"} += $blame;
      $snapshot->{"snapshot"} = $current_snapshot;
      $snapshot->{"lines"} .= "$line_no($size):";
      push @snapshots, $current_snapshot if( $current_snapshot != $snapshots[$#snapshots] );
    }
  }
  close( MS_PRINT );

  #print snapshot info
  @snapshots = @snapshots[$#snapshots-$chart_last+1..$#snapshots] if( defined($chart_last));
  my $max_counted = 0;
  my $max_allocated = 0;
  foreach ( @snapshots ) {
    my $snapshot = $_;
    my $snapshot_allocated = $snapshot_data{$snapshot}{"ALLOCATED"};
    my $snapshot_counted = $snapshot_data{$snapshot}{"COUNTED"};
    my $lines = $snapshot_data{$snapshot}{"LINES"};
    $max_counted = $snapshot_counted if( $snapshot_counted > $max_counted );
    $max_allocated = $snapshot_allocated if( $snapshot_allocated > $max_allocated );
    print "Snapshot $snapshot [$snapshot_allocated] [$snapshot_counted] [$lines]\n";
  }

  #create chart
  my $graph;
  if( $chart_type eq "bar" ) {
    $graph = GD::Graph::bars->new($chart_width, $chart_height);
    $graph->set(
		x_label => "Time",
		y_label => "Size",
		title => "massif",
	       ) or die $graph->error;
  } elsif( $chart_type eq "lines" ) {
    $graph = GD::Graph::lines->new($chart_width, $chart_height);
    $graph->set(
		x_label => "Time",
		y_label => "Size",
		title => "massif",
		transparent => 0,
		cumulate => 1,
		line_types => [1,2,4],
		line_width => 2,
	       ) or die $graph->error;
  } elsif( $chart_type eq "area" ) {
    $graph = GD::Graph::area->new($chart_width, $chart_height);
    my $y_max_value = $max_counted;
    if( $chart_points eq "allocated" ) {
      $y_max_value = $max_allocated;
    } elsif( $chart_points eq "percent" ) {
      $y_max_value = 100;
    }
    $graph->set(
		x_label => "Time",
		y_label => "Size",
		title => "massif",
		y_max_value => $y_max_value,
		transparent => 0,
		legend_placement => "RT",
		cumulate => 1,
	       ) or die $graph->error;
  }
  $graph->set(
      dclrs => [GD::Graph::colour::sorted_colour_list()],
  );

  #calculate points
  my @graph_legends;
  my @graph_points;
  push @graph_points, \@snapshots;
  foreach( reverse( keys %blames ) ) {
    my $skip = 1;
    my $blame = $_;
    print "Processing: $blame\n";
    my @points;
    my $delta_point = undef;
    foreach ( @snapshots ) {
      my $snapshot = $_;
      my $counted = $snapshot_data{$snapshot}{"COUNTED"};
      my $d = $snapshot_data{$snapshot}{$blame};
      if ( !defined( $d ) ) {
	push @points, 0;
	#print " $snapshot) $blame NONE\n";
      } else {
	my $percent = $d->{"percent"};
	my $size = $d->{"size"};
	my $lines = $d->{"lines"};
	my $point = $size;
	$point = (($size / $counted) * 100) if( $chart_points eq "percent" &&
						$chart_type eq "area");
	my $prev_delta_point = $delta_point;
	if( defined( $chart_limit ) ) {
	  my $print = $print_all;
	  my $delta = 0;
	  my $growth = 100;
	  if ( !defined( $delta_point ) || $point < $delta_point ) {
	    $delta_point = $point;
	  } elsif( $point != $delta_point ) {
	    $delta = $point - $delta_point;
	    $growth = ($delta / $delta_point) * 100;	    
	    if ( $growth >= $chart_limit ) {
	      $skip = 0;
	      $print = 1;
	      $delta_point = $point;
	    }
	  }
	  #print "   $snapshot) $point ($percent $size) [$point vs $prev_delta_point == $delta] ($growth) [$lines]\n" ;#if( $print );
	} else {
	  #print "   $snapshot) $point ($percent $size $counted) [$lines]\n";
	}

        push @points, $point;
	print " $snapshot) ($point) $percent $size [$counted] [$lines]\n";
      }
    }
    if ( !$skip || !defined( $chart_limit ) ) {
      push @graph_legends, $blame;
      push @graph_points, \@points;
    } else {
      print "   FILTERED!\n";
    }
  }
  if( $chart_points eq "allocated" ) {
    my $blame = "UNKNOWN";
    print "Processing: $blame\n";
    my @points;
    foreach ( @snapshots ) {
      my $snapshot = $_;
      my $counted = $snapshot_data{$snapshot}{"COUNTED"};
      my $allocated = $snapshot_data{$snapshot}{"ALLOCATED"};
      my $point = $counted - $allocated;
      push @points, $point;
      print " $snapshot) ALLOCATED ($point) [$allocated $counted]\n";
    }
    push @graph_legends, "$blame";
    push @graph_points, \@points;
  }

  #plot it
  $graph->set_legend(@graph_legends);
  my $gd = $graph->plot(\@graph_points) or die $graph->error;
  open(CHART, ">$chart_output");
  print CHART $gd->png;
  close( CHART );
}
