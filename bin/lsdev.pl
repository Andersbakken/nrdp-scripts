#!/usr/bin/perl

use Cwd;
use Cwd 'abs_path';
use File::Basename;
use File::Spec;

use strict;

my $verbose = 0;
my $write_default_file = 0;
my $read_devdir_list = 1;
my $detect_devdirs = 1;
my $display_only;
my $display_help = 0;
my $answer;
my $cwd = Cwd::cwd();

my $src_prefix = "src_";
my $build_prefix = "build_";

my $output;
my $root = 0;
my @matches;

sub parseOptions {
    while(@_) {
        my $option = shift @_;
        if($option eq "-w") {
            $write_default_file = 1;
        } elsif($option eq "-r") {
            $root = 1;
        } elsif($option eq "-c") {
            $cwd = shift @_;
        } elsif($option eq "-b") {
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
            $detect_devdirs = 0;
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
}

parseOptions(split(/ /, $ENV{LSDEV_FLAGS})) if($ENV{LSDEV_FLAGS});
parseOptions(@ARGV);

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
    display "-m : Disable directory detection which will find any build tree that isn't referenced explicitly.\n";
    display "-r : Select the root of the selected project (rather than guessing subdirs based on current path).\n";
    display "-b : Only include the relevant source dirs and shadow dirs in the selection output.\n";
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
    $dir = $cwd unless(defined($dir));
    for( ; $dir; $dir = dirname($dir)) {
        my $r = "$dir/$file";
        $r =~ s,//+,/,g;
        return $r if(-e $r);
        last if(length($dir) <= 1);
    }
    return undef;
}

sub processSourceDir {
    my ($src_dir) = @_;
    if(-e "${src_dir}/configure" || -e "${src_dir}/Makefile" || -e "${src_dir}/*.pro" ||
       -e "${src_dir}/CMakeLists.txt") { #buildable
        return 1;
    } elsif(-e "${src_dir}/.lsdev_shadows" || -e "${src_dir}/.lsdev_config") { #lsdev
        return 1;
    } elsif(-d "${src_dir}/.git") { #really?
        return 1;
    }
    return 0;
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
    } elsif(-e "${build_dir}/.lsdev_config") {
        $src_dir = getProjectConfig(${build_dir}, "source_dir");
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

sub parseConfig {
    my ($file) = @_;
    my %result;
    display "Processing: $file\n" if($verbose);
    if(open(FILE, "<$file")) {
        while(my $line = <FILE>) {
            chomp($line);
            if($line =~ /^([^#].*)=(.*)$/) {
                $result{$1} = $2;
            }
        }
        close(FILE);
    }
    return %result;
}

sub parseFileMap {
    my ($file) = @_;
    my %result = parseConfig($file);
    foreach(keys %result ) {
        my $name = $_;
        my $path = canonicalize($result{$name}, dirname($file));
        display " Mapped: $name -> $path\n" if($verbose);
        $result{$name} = $path;
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

sub getProjectConfig {
    my ($path, $config) = @_;
    my $result;
    my $lsdev_config_file = "$path/.lsdev_config";
    if(-e $lsdev_config_file) {
        my %c = parseConfig($lsdev_config_file);
        $result = $c{$config};
    }
    display " ProjectConfig: $lsdev_config_file($config) -> $result\n" if($verbose);
    return $result;
}

sub getProjectName {
    my ($path) = @_;
    my $result = getProjectConfig($path, "name");
    return $result;
}

my %roots_names;
my @roots;

my $default_dir;
my $root_dir;
my %dev_roots = parseFileMap(glob("~/.dev_directories"));
my $project_name;

sub filterMatches {
    my @choices;
    my @match_roots = @_;
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
                my $matches = 0;
                if($match =~ /^path:(.*)/) {
                    $matches = ($root =~ /$1/i);
                } else {
                    $matches = ($root_name =~ /$match/i);
                }
                $matches = !$matches if($inverse);
                unless($matches) {
                    $root = undef;
                    last;
                }
            }
        } elsif($read_devdir_list != 2 && $root eq $root_dir) { #if there are no arguments filter out pwd
            next;
        }
        push @choices, $root if(defined($root));
    }
    @choices = sort { (stat($a))[9] < (stat($b))[9] } @choices;
    return @choices;
}

#figure out where I am in a shadow build and my relevent source dir
if(my $build_marker = findAncestor("CMakeCache.txt") || findAncestor("config.status") || findAncestor(".lsdev_config")) {
    $root_dir = dirname($build_marker);
    $read_devdir_list = -2 if($read_devdir_list == 1 &&
                             ($#matches == -1 || $matches[0] eq "-"));
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
if(my $shadows_file = findAncestor(".lsdev_shadows")) {
    display " Found $shadows_file!\n" if($verbose);
    my $shadows_file_dir = dirname($shadows_file);
    my $shadows_dir = $shadows_file_dir;
    $project_name = findFileMap($shadows_dir, \%dev_roots) unless(defined($project_name));
    my %shadows_roots = parseFileMap("$shadows_file");
    $read_devdir_list = -2 if($read_devdir_list == 1 &&
                             ($#matches == -1 || $matches[0] eq "-"));
    unless(defined($default_dir)) {
        my @shadows_roots_keys = keys %shadows_roots;
        $default_dir = $shadows_roots{$shadows_roots_keys[0]} if($#shadows_roots_keys == 1);
    }
    $root_dir = $shadows_file_dir unless(defined($root_dir));
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
} elsif(my $src_marker = findAncestor(".lsdev_config") || findAncestor("configure")) {
    $read_devdir_list = -2 if($read_devdir_list == 1 &&
                             ($#matches == -1 || $matches[0] eq "-"));
    $root_dir = dirname($src_marker) unless(defined($root_dir));
}


#figure out default
unless(defined($default_dir)) {
    my $lsdev_default_file = findAncestor(".lsdev_default", $root_dir);
    $lsdev_default_file = glob("~/.lsdev_default") unless($lsdev_default_file);
    if($lsdev_default_file && -e $lsdev_default_file) {
        display " Found $lsdev_default_file!\n" if($verbose);
        if(open(LSDEV_DEFAULT, "<$lsdev_default_file")) {
            $default_dir = <LSDEV_DEFAULT>;
            display "   Default $default_dir\n" if($verbose);
            chomp($default_dir);
            close(LSDEV_DEFAULT);
        }
    }
}


#process anything that looks like a build
if(defined($dev_roots{sources})) {
    my $sources = delete $dev_roots{sources};
    foreach(split(/,/, $sources)) {
        my $source = $_;
        display "Looking at source: $source\n" if($verbose);
        if( $detect_devdirs && -d "$source" && opendir(SOURCES, $source) ) {
            while(my $subdir = readdir(SOURCES)) {
                next if($subdir eq "." || $subdir eq "..");
                my $src_dir = "$source/$subdir";
                next if(getProjectConfig($src_dir, "ignore"));
                if(-d $src_dir && processSourceDir($src_dir)) {
                    my $project_name = getProjectName($src_dir);
                    unless(defined($project_name)) {
                        $project_name = findFileMap($src_dir, \%dev_roots);
                        $project_name = basename($src_dir) unless(defined($project_name));
                        #$project_name .= "_detect";
                    }
                    display "Source Detect: $src_dir [$project_name]\n" if($verbose);
                    $roots_names{$src_dir} = "${src_prefix}${project_name}";
                    display __LINE__, ": Named Root [", $roots_names{$src_dir}, "] -> [$src_dir]\n" if($verbose);
                    push(@roots, $src_dir);
                    display __LINE__, ": Pushed Root [$src_dir]\n" if($verbose);
                }
            }
            closedir(SOURCES);
        }
    }
}
#process anything that looks like a build
if(defined($dev_roots{builds})) {
    my $builds = delete $dev_roots{builds};
    foreach(split(/,/, $builds)) {
        my $build = $_;
        display "Looking at build: $build\n" if($verbose);
        if( $detect_devdirs && -d "$build" && opendir(BUILDS, $build) ) {
            while(my $subdir = readdir(BUILDS)) {
                next if($subdir eq "." || $subdir eq "..");
                my $build_dir = "$build/$subdir";
                next if(getProjectConfig($build_dir, "ignore"));
                my $src_dir;
                $src_dir = processBuildDir($build_dir) if(-d $build_dir);
                if(defined($src_dir) && ($read_devdir_list >= 1 || $src_dir eq $root_dir || $src_dir eq $default_dir)) {
                    my $project_name = getProjectName($src_dir);
                    unless(defined($project_name)) {
                        $project_name = findFileMap($src_dir, \%dev_roots);
                        $project_name = basename($src_dir) unless(defined($project_name));
                        #$project_name .= "_detect";
                    }
                    if(defined($project_name)) {
                        display "Build Detect: $build_dir ($src_dir) [$project_name]\n" if($verbose);
                        #src side
                        $roots_names{$src_dir} = "${src_prefix}${project_name}";
                        display __LINE__, ": Named Root [", $roots_names{$src_dir}, "] -> [$src_dir]\n" if($verbose);
                        push(@roots, $src_dir);
                        display __LINE__, ": Pushed Root [$src_dir]\n" if($verbose);
                        #build side
                        my $shadow_name = getProjectName($build_dir);
                        $shadow_name = "$subdir" unless(defined($shadow_name));
                        $roots_names{$build_dir} = "${build_prefix}${project_name}_${shadow_name}";
                        display __LINE__, ": Named Root [", $roots_names{$build_dir}, "] -> [$build_dir]\n" if($verbose);
                        push(@roots, $build_dir);
                        display __LINE__, ": Pushed Root [$build_dir]\n" if($verbose);
                    }
                }
            }
            closedir(BUILDS);
        }
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
    if(-e "$dev_root/.lsdev_shadows" ) {
        my %shadows = parseFileMap("$dev_root/.lsdev_shadows");
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
        $current = $cwd;
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
    for(my $i = 0; $i <= $#matches; ++$i) {
        my $match = $matches[$i];
        if($match =~ /\//) {
            if(!defined($rest_dir)) {
                $rest_dir = $match;
            } else {
                display "Illegal match: $match ($rest_dir)\n";
            }
            splice(@matches, $i, 1);
        }
    }

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
    } elsif($#matches == -1 && $root && $root_dir) {
        push @choices, $root_dir;
    } else {
        my @match_roots;
        if($#matches == -1 || $read_devdir_list == -1) {
            @match_roots = uniq(@roots);
        } else {
            @match_roots = keys %roots_names;
        }
        @choices = filterMatches(@match_roots);
    }

    if(!defined($rest_dir) && !$root && $root_dir) {
        my $current = $cwd;
        $rest_dir = $1 if($current =~ /^$root_dir\/(.*)/);
    }

    my $index;
    if($display_only eq "list") {
        for(my $i = 0; $i < @choices; ++$i) {
            my $root = canonicalize($choices[$i]);
            my $root_name = $roots_names{$root};
            answer($root_name, $root);
        }
    } else {
        while(1) {
            if($#choices <= 0) {
                $index = 0 if($#choices == 0);
                last;
            }
            for(my $i = 0; $i < @choices; ++$i) {
                my $root = canonicalize($choices[$i]);
                my $root_name = $roots_names{$root};
                display "[", $i + 1, "] $root_name [$root]\n";
            }
            display "[1..", $#choices+1, "]";
            display " ($rest_dir)" if(defined($rest_dir));
            display "> ";
            my $choice = <STDIN>;
            chomp $choice;
            if($choice =~ /[0-9]+/) {
                $index = $choice - 1;
                last if($index >= 0 && $index <= $#choices);
            }
            push @matches, "$choice";
            @choices = filterMatches(@choices);
        }
    }
    if(defined($index)) {
        my $ans_root_dir = canonicalize($choices[$index]);
        my $ans_root_name = $roots_names{$ans_root_dir};

        my $ans_dir = $ans_root_dir;
        if(defined($rest_dir)) {
            my $rest_cd_dir = "$ans_dir/$rest_dir";
            for(my $current = canonicalize($rest_cd_dir); $current && length($current) > length($ans_dir);
                $current = dirname($current)) {
                if(-d $current) {
                    $ans_dir = $current;
                    last;
                }
            }
        }
        answer($ans_root_name, $ans_dir);

        if($write_default_file) {
            my @lsdev_defaults;
            push(@lsdev_defaults, glob("~/.lsdev_default"));
            push(@lsdev_defaults, "$root_dir/.lsdev_default") if($root_dir && !($root_dir eq $ans_dir));
            foreach(@lsdev_defaults) {
                my $lsdev_default = $_;
                if(open(LSDEV_DEFAULT, ">$lsdev_default")) {
                    display "Writing $lsdev_default -> $ans_dir\n" if($verbose);
                    print LSDEV_DEFAULT "$ans_dir\n";
                    close(LSDEV_DEFAULT);
                }
            }
        }
    }
}


