#!/usr/bin/perl

use Cwd;
use Cwd 'abs_path';
use File::Basename;
use File::Spec;

use strict;

my $verbose = 0;
my $write_default_file = 0;
my $read_devdir_list = 1;
my $detect_builds_list = 1;
my $display_only;
my $display_help = 0;
my $answer;

my $src_prefix = "src_";
my $build_prefix = "build_";

my $output;
my $root = 0;
my @matches;

while(@ARGV) {
    my $option = shift @ARGV;
    if($option eq "-w") {
        $write_default_file = 1;
    } elsif($option eq "-r") {
        $root = 1;
    } elsif($option eq "-d") {
        $read_devdir_list = -1;
    } elsif($option eq "-tp") {
        $answer = "path";
    } elsif($option eq "-tn") {
        $answer = "name";
    } elsif($option eq "-ta") {
        $answer = "all";
    } elsif($option eq "-a") {
        $read_devdir_list = 2;
    } elsif($option eq "-m") {
        $detect_builds_list = 0;
    } elsif($option eq "-l") {
        $display_only = "list";
    } elsif($option eq "-p") {
        $display_only = "current";
    } elsif($option eq "-h") {
        $display_help = 1;
    } elsif($option eq "-v") {
        $verbose = 1;
    } else {
        push @matches, "$option";
    }
}

if($display_only eq "current") {
    $read_devdir_list = 3;
    $answer = "name" if(!defined($answer));
} elsif($display_only eq "list") {
    $answer = "all" if(!defined($answer));
} else {
    $answer = "path" if(!defined($answer));
}
$display_only = "default" if(!defined($display_only) &&
                             $#matches == 0 && $matches[0] eq "-");


sub answer {
    my ($name, $path) = @_;
    if($answer eq "all") {
        print STDOUT "$name [$path]\n";
    } elsif($answer eq "name") {
        print STDOUT "$name\n";
    } elsif($answer eq "path") {
        print STDOUT "$path\n";
    }
}

sub display {
    foreach(@_) {
        print STDERR $_;
    }
}

sub showHelp {
    display "lsdev [options] [matches]\n";
    display "\n== Options ==\n";
    display "-w : Write out the selected directory to the relevant default file (for next invocation with -).\n";
    display "-m : Disable build directory detection which will find any build tree that isn't referenced explicitly.\n";
    display "-r : Select the root of the selected project (rather than guessing subdirs based on current path).\n";
    display "-d : Only include the relevant source dirs and shadow dirs in the selection output.\n";
    display "-a : Force inclusion of all source and shadow dirs in the selection output.\n";
    display "-l : Just list out the selections and do not actually request selection.\n";
    display "-p : Just list out the name of the current working directories project if possible.\n";
    display "\n== Matches ==\n";
    display "If no matches are provided then it will behave as if -d has been passed, otherwise it will behave as if -a has\n";
    display "had been passed. If either are passed, then they override these assumptions\n";
    display "If - is passed then the current 'default' directory will be jumped to, which is specific to the kind of directory\n";
    display "you are currently in.\n";
    display "If @ is passed then the current 'emacs' directory will be jumped to.\n";
    display "Otherwise any other strings are matched and all must match to be included in the selection output\n";
    exit 0;
}
showHelp() if($display_help);

sub uniq {
    my %seen = ();
    my @r = ();
    foreach my $a (@_) {
        unless ($seen{$a}) {
            push @r, $a;
            $seen{$a} = 1;
        }
    }
    return @r;
}

sub findAncestor {
    my ($file, $dir) = @_;
    $dir = Cwd::cwd() unless(defined($dir));
    for( ; $dir; $dir = dirname($dir)) {
        my $r = "$dir/$file";
        $r =~ s,//+,,g;
        return $r if(-e $r);
        last if(length($dir) <= 1);
    }
    return undef;
}

sub processBuildDir {
    my ($build_dir) = @_;
    my $src_dir;
    if(-e "${build_dir}/CMakeCache.txt") {
        my $cmake_cache = "${build_dir}/CMakeCache.txt";
        display " Found $cmake_cache!\n" if($verbose);
        if(open( CMAKE_CACHE, "<$cmake_cache")) {
            while(my $line = <CMAKE_CACHE>) {
                chomp($line);
                if($line =~ /CMAKE_HOME_DIRECTORY:INTERNAL=(.*)$/) {
                    $src_dir = $1;
                    last;
                }
            }
            close(CMAKE_CACHE);
        }
    } elsif(-e "${build_dir}/config.status") {
        my $config_status = "${build_dir}/config.status";
        display " Found $config_status!\n" if($verbose);
        if(open( CONFIG_STATUS, "<$config_status")) {
            while(my $line = <CONFIG_STATUS>) {
                chomp($line);
                if($line =~ /([^ ]*configure)/) {
                    $src_dir = dirname($1);
                    last;
                }
            }
            close(CONFIG_STATUS);
        }
    } elsif(-e "${build_dir}/.shadows_src") {
        my $shadows_src = "${build_dir}/.shadows_src";
        display " Found $shadows_src!\n" if($verbose);
        if(open( SHADOWS_SRC, "<$shadows_src")) {
            $src_dir = <SHADOWS_SRC>;
            chomp($src_dir);
            close(SHADOWS_SRC);
        }
    }
    $src_dir = canonicalize($src_dir, $build_dir) if(defined($src_dir));
    return $src_dir;
}

sub canonicalize {
    my ($file, $base) = @_;
    my @globs = glob($file);
    my $result = defined(@globs) ? $globs[0] : $file;
    if(!File::Spec->file_name_is_absolute($result) && defined($base)) {
        $result = File::Spec->rel2abs($result, $base);
    }
    my $before = $result;
    $result = abs_path($result) if(-e "$result");
    $result =~ s,/*$,,g;
    return $result;
}

sub parseFileMap {
    my ($file) = @_;
    my %result;
    display "Processing: $file\n" if($verbose);
    if(open(FILE, "<$file")) {
        while(my $line = <FILE>) {
            chomp($line);
            if($line =~ /^([^#].*)=(.*)$/) {
                my $name = $1;
                my $path = canonicalize($2, dirname($file));
                display " Mapped: $name -> $path\n" if($verbose);
                $result{$name} = $path;
            }
        }
        close(FILE);
    }
    return %result;
}

sub findFileMap {
    my ($find, $map) = @_;

    $find = canonicalize($find);
    my $result;
    my $result_path;
    foreach(keys %$map ) {
        my $name = $_;
        my $path = canonicalize($map->{$_});
        if($find eq $path && length($path) > length($result_path)) {
            $result_path = $path;
            $result = $name;
        }
    }
    display " FindFileMap: $find -> $result\n" if($verbose);
    return $result;
}

my %roots_names;
my @roots;

my $shadows_dir;
my $default_dir;
my $root_dir;
my %dev_roots = parseFileMap(glob("~/.dev_directories"));
my $project_name;

#figure out where I am in a shadow build and my relevent source dir
if(my $build_marker = findAncestor("CMakeCache.txt") || findAncestor("config.status") || findAncestor(".shadows_src")) {
    $root_dir = dirname($build_marker);
    $read_devdir_list = -2 if($read_devdir_list == 1 || ($#matches == 0 && $matches[0] eq "-"));
    if(my $src_dir = processBuildDir("$root_dir")) {
        display "SRCDIR: $root_dir -> $src_dir\n" if($verbose);
        $default_dir = $src_dir;
        $project_name = findFileMap($src_dir, \%dev_roots) unless(defined($project_name));
        $roots_names{$src_dir} = defined($project_name) ? "${src_prefix}${project_name}" : "${build_prefix}src";
        display __LINE__, ": Named Root [", $roots_names{$src_dir}, "] -> [$src_dir]\n" if($verbose);
        push(@roots, $src_dir);
        display __LINE__, ": Pushed Root [$src_dir]\n" if($verbose);
    }
}

#figure out all the shadows listed in my relevent source dir
if(my $shadows_file = findAncestor(".shadows", $shadows_dir)) {
    display " Found $shadows_file!\n" if($verbose);
    my $shadows_file_dir = dirname($shadows_file);
    $shadows_dir = $shadows_file_dir unless(defined($shadows_dir));
    $project_name = findFileMap($shadows_dir, \%dev_roots) unless(defined($project_name));
    my %shadows_roots = parseFileMap("$shadows_file");
    $read_devdir_list = -2 if($read_devdir_list == 1 ||
                             ($#matches == 0 && $matches[0] eq "-"));
    $root_dir = $shadows_file_dir unless(defined($root_dir));

    unless(defined($default_dir)) {
        my $shadows_default = "$shadows_dir/.shadows_default";
        if(open(SHADOWS_DEFAULT, "<$shadows_default")) {
            $default_dir = <SHADOWS_DEFAULT>;
            chomp($default_dir);
            close(SHADOWS_DEFAULT);
        } else {
            my @shadows_roots_keys = keys %shadows_roots;
            $default_dir = $shadows_roots{$shadows_roots_keys[0]} if($#shadows_roots_keys == 1);
        }
    }

    $roots_names{$shadows_dir} = defined($project_name) ? "${src_prefix}${project_name}" : "${build_prefix}src";
    display __LINE__, ": Named Root [", $roots_names{$shadows_dir}, "] -> [$shadows_dir]\n" if($verbose);
    push(@roots, $shadows_dir);
    display __LINE__, ": Pushed Root [$shadows_dir]\n" if($verbose);
    foreach(keys(%shadows_roots)) {
        my $shadows_root_name = $_;
        my $shadows_root = $shadows_roots{$_};
        if(defined($project_name)) {
            $shadows_root_name = "${project_name}_${shadows_root_name}";
        }
        $roots_names{$shadows_root} = "${build_prefix}${shadows_root_name}";
        display __LINE__, ": Named Root [", $roots_names{$shadows_root}, "] -> [$shadows_root]\n" if($verbose);
        push(@roots, $shadows_root);
        display __LINE__, ": Pushed Root [$shadows_dir]\n" if($verbose);
    }
}
if(my $src_marker = findAncestor("configure")) {
    $read_devdir_list = -2 if($read_devdir_list == 1 ||
                             ($#matches == 0 && $matches[0] eq "-"));
    $root_dir = dirname($src_marker) unless(defined($root_dir));
}

#process anything that looks like a build
if(defined($dev_roots{builds})) {
    my $builds = delete $dev_roots{builds};
    if( $detect_builds_list && -d "$builds" && opendir(BUILDS, $builds) ) {
        while(my $subdir = readdir(BUILDS)) {
            next if($subdir eq "." || $subdir eq "..");
            my $build_dir = "$builds/$subdir";
            my $src_dir = processBuildDir($build_dir) if(-d $build_dir);
            if(defined($src_dir) && ($read_devdir_list >= 1 || $src_dir eq $root_dir || $src_dir eq $default_dir)) {
                my $project_name = findFileMap($src_dir, \%dev_roots);
                $project_name = basename($src_dir) unless(defined($project_name));
                if(defined($project_name)) {
                    display "Build Detect: $build_dir ($src_dir) [$project_name]\n" if($verbose);
                    #src side
                    $roots_names{$src_dir} = "${src_prefix}${project_name}_detect";
                    display __LINE__, ": Named Root [", $roots_names{$src_dir}, "] -> [$src_dir]\n" if($verbose);
                    push(@roots, $src_dir);
                    display __LINE__, ": Pushed Root [$src_dir]\n" if($verbose);
                    #build side
                    $roots_names{$build_dir} = "${build_prefix}${project_name}_detect_${subdir}";
                    display __LINE__, ": Named Root [", $roots_names{$build_dir}, "] -> [$build_dir]\n" if($verbose);
                    push(@roots, $build_dir);
                    display __LINE__, ": Pushed Root [$build_dir]\n" if($verbose);
                }
            }
        }
        closedir(BUILDS);
    }
}

#figure out all the shadows and devdirs listed anywhere
foreach(keys(%dev_roots)) {
    my $dev_root_name = $_;
    my $dev_root = $dev_roots{$dev_root_name};
    if($read_devdir_list >= 1) {
        push(@roots, $dev_root);
        display __LINE__, ": Pushed Root [$dev_root]\n" if($verbose);
    }
    if(-e "$dev_root/.shadows" ) {
        my %shadows = parseFileMap("$dev_root/.shadows");
        foreach(keys(%shadows)) {
            my $shadow_root_name = $_;
            my $shadow_root = $shadows{$_};
            $roots_names{$shadow_root} = "${build_prefix}${dev_root_name}_${shadow_root_name}";
            display __LINE__, ": Named Root [", $roots_names{$shadow_root}, "] -> [$shadow_root]\n" if($verbose);
            if($read_devdir_list >= 1) {
                push(@roots, $shadow_root);
                display __LINE__, ": Pushed Root [$shadow_root]\n" if($verbose);
            }
        }
    }
    $roots_names{$dev_root} = "${src_prefix}${dev_root_name}";
    display __LINE__, ": Named Root [", $roots_names{$dev_root}, "] -> [$dev_root]\n" if($verbose);
}

if($display_only eq "current") { #display just the name of the directory request
    my $current;
    if($#matches == -1) {
        $current = Cwd::cwd();
    } elsif($#matches == 0 && $matches[0] eq "-") {
        $current = $default_dir;
    } else {
        $current = $matches[0];
    }
    for($current = canonicalize($current); $current;
        $current = dirname($current)) {
        my $root_name = $roots_names{$current};
        display "Trying: $current -> $root_name\n" if($verbose);
        if(defined($root_name)) {
            answer($root_name, $current);
            last;
        }
        last if(length($current) <= 1);
    }
} else { #display all matching directories, and finally answer with the chosen one
    my $rest_dir;
    my @choices;
    if($#matches == 0 && $matches[0] =~ /^@(.*)$/) {
        if(open(EMACSCLIENT, "emacsclient -e '(sam-find-directory \"$1\")'|")) {
            my $choice = <EMACSCLIENT>;
            chomp $choice;
            $choice =~ s,",,g;
            push @choices, $choice if($choice);
            close(EMACSCLIENT);
        } else {
            display "Unable to connect emacsclient!\n";
        }
    } elsif($#matches == 0 && $matches[0] eq "-") {
        push @choices, $default_dir;
    } else {
        my @match_roots;
        if($#matches == -1 || $read_devdir_list == -1) {
            @match_roots = uniq(@roots);
        } else {
            @match_roots = keys %roots_names;
        }

        for(my $i = 0; $i <= $#matches; ++$i) {
            my $match = $matches[$i];
            if($match =~ /\//) {
                if(!defined($rest_dir) && !$root) {
                    $rest_dir = $match;
                } else {
                    display "Illegal match: $match\n";
                }
                splice(@matches, $i, 1);
            }
        }

        foreach(@match_roots) {
            my $root = $_;
            my $root_name = $roots_names{$root};
            if($#matches != -1) {
                foreach(@matches) {
                    my $match = $_;
                    my $inverse = 0;
                    if($match =~ /^-(.*)/) {
                        $match = $1;
                        $inverse = 1;
                    }
                    my $matches = ($root_name =~ /$match/i);
                    $matches = !$matches if($inverse);
                    unless($matches) {
                        $root = undef;
                        last;
                    }
                }
            } elsif($root eq $root_dir) { #if there are no arguments filter out pwd
                next;
            }
            push @choices, $root if(defined($root));
        }
    }

    if(!defined($rest_dir) && !$root && $root_dir) {
        my $current = Cwd::cwd();
        $rest_dir = $1 if($current =~ /^$root_dir\/(.*)/);
    }

    my $index;
    if($display_only eq "list") {
        for(my $i = 0; $i < @choices; ++$i) {
            my $root = canonicalize($choices[$i]);
            my $root_name = $roots_names{$root};
            answer($root_name, $root);
        }
    } elsif($#choices == 0) {
        $index = 0;
    } elsif($#choices != -1) {
        for(my $i = 0; $i < @choices; ++$i) {
            my $root = canonicalize($choices[$i]);
            my $root_name = $roots_names{$root};
            display "[", $i + 1, "] $root_name [$root]\n";
        }
        display "[1..", $#choices+1, "]";
        display " ($rest_dir)" if(defined($rest_dir));
        display "> ";
        $index = <STDIN>;
        chomp $index;
        $index -= 1;
    }
    if(defined($index)) {
        my $root_dir = canonicalize($choices[$index]);
        my $root_name = $roots_names{$root_dir};

        my $dir = $root_dir;
        if(defined($rest_dir)) {
            my $rest_cd_dir = "$dir/$rest_dir";
            for(my $current = canonicalize($rest_cd_dir); $current && length($current) > length($dir);
                $current = dirname($current)) {
                if(-d $current) {
                    $dir = $current;
                    last;
                }
            }
        }
        answer($root_name, $dir);

        if($write_default_file && -d "$shadows_dir") {
            my $shadows_default = "$root_dir/.shadows_default";
            if(open(SHADOWS_DEFAULT, ">$shadows_default")) {
                #display "Writing $shadows_default -> $dir\n";
                print SHADOWS_DEFAULT "$dir\n";
                close(SHADOWS_DEFAULT);
            }
        }
    }
}


