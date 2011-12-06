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
my $cwd = Cwd::getcwd();
if (exists($ENV{PWD}) && $ENV{PWD} ne $cwd) {
    my $e = my ($e_dev, $e_node) = stat($ENV{PWD});
    my $c = my ($c_dev, $c_node) = stat($cwd);
    if ($e && $c && $e_dev == $c_dev && $e_node == $c_node) {
        $cwd = $ENV{PWD};
    }
}

my $src_prefix = "src_";
my $build_prefix = "build_";
my $detect_suffix = "_detect";
$detect_suffix = ""; #not used currently

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
    my ($root) = @_;
    if($answer eq "all") {
        print STDOUT generateName($root) . " [" . $root->{path} . "]\n";
    } elsif($answer eq "name") {
        print STDOUT generateName($root) . "\n";
    } elsif($answer eq "path") {
        print STDOUT $root->{path} . "\n";
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
                if($line =~ /(\W+\/configure)/) {
                    $src_dir = dirname($1);
                }
                last if($src_dir && length($src_dir));
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
    my ($file, $base, $readlink) = @_;
    $readlink = 1 unless(defined($readlink));
    my @globs = glob($file);
    my $result = defined(@globs) ? $globs[0] : $file;
    if(!File::Spec->file_name_is_absolute($result) && defined($base)) {
        $result = File::Spec->rel2abs($result, $base);
    }
    $result = abs_path($result) if(-e "$result" && $readlink);
    $result =~ s,/*$,,g;
    $result = "/" unless(length($result));
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

    $find = canonicalize($find, 0);
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

my %roots;

my $default_dir;
my $root_dir;
my %dev_roots = parseFileMap(glob("~/.dev_directories"));
my $project_name;

sub generateName {
    my ($root) = @_;
    my $name = $root->{name};
    $name =~ s,-,_,g;
    if($root->{src}) {
        my $src_root = $roots{$root->{src}};
        my $src_name = $src_root->{name};
        $src_name =~ s,-,_,g;
        $name = "${src_name}_${name}" unless($name =~ /$src_name/i);
        $name = "${build_prefix}${name}";
    } else {
        $name = "${src_prefix}${name}";
    }
    return $name;
}

sub findDevDirectory {
    my ($current) = @_;
    for($current = canonicalize($current, undef, 0); $current; $current = dirname($current)) {
        foreach(keys(%dev_roots)) {
            my $dev_root_name = $_;
            my $dev_root = $dev_roots{$dev_root_name};
            #display "findDevDirectory: $dev_root -> $current\n" if($verbose);
            return $current if($dev_root eq $current);
        }
        last if(length($current) <= 1);
    }
    return undef;
}

sub findRoot {
    my ($current) = @_;
    for($current = canonicalize($current, undef, 0); $current; $current = dirname($current)) {
        my $root = $roots{$current};
        #display "findRoot: $current -> $root\n" if($verbose);
        return $root->{path} if(defined($root));
        last if(length($current) <= 1);
    }
    return undef;
}

sub isRelated {
    my ($path1, $path2) = @_;
    return 3 if($path1 eq $path2);

    my $root1 = $roots{$path1};
    my $root2 = $roots{$path2};
    if($root1 && $root2) {
        if($root1->{src}) {
            return 1 if($root1->{src} eq $root2->{path});
            return 2 if($root1->{src} eq $root2->{src});
        } elsif($root2->{src}) {
            return 1 if($root1->{path} eq $root2->{src});
        }
    }
    return 0;
}

sub filterMatches {
    my @choices;
    my @match_roots = @_;
    foreach(@match_roots) {
        my $root = $roots{$_};
        if($#matches != -1) {
            foreach(@matches) {
                my $match = $_;
                my $inverse = 0;
                if($match =~ /^-(.*)/) {
                    $match = $1;
                    $inverse = 1;
                }
                my $matches = 0;
                if($match eq "src") {
                    $matches = !defined($root->{src});
                } elsif($match eq "build") {
                    $matches = defined($root->{src});
                } elsif($match =~ /^path:(.*)/) {
                    $matches = ($root->{path} =~ /$1/i);
                } else {
                    $matches = (generateName($root) =~ /$match/i);
                }
                $matches = !$matches if($inverse);
                unless($matches) {
                    $root = undef;
                    last;
                }
            }
        } elsif($read_devdir_list != 2 && $root->{path} eq $root_dir) { #if there are no arguments filter out pwd
            next;
        }
        push @choices, $root->{path} if(defined($root));
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

        my %root;
        $root{name} = defined($project_name) ? "${project_name}" : "src";
        $root{path} = $src_dir;
        $roots{$root{path}} = \%root;
        display __LINE__, ": Named Root [", $root{name}, "] -> [", $root{path}, "]\n" if($verbose);
    }
}

if(my $dev_directory = findDevDirectory($cwd)) {
    $read_devdir_list = -2 if($read_devdir_list == 1 &&
                              ($#matches == -1 || $matches[0] eq "-"));
    $root_dir = $dev_directory unless(defined($root_dir));
} elsif(my $shadows_file = findAncestor(".lsdev_shadows")) {    #figure out all the shadows listed in my relevent source dir
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

    my %src_root;
    $src_root{name} = defined($project_name) ? "${project_name}" : "src";
    $src_root{path} = $shadows_dir;
    $roots{$src_root{path}} = \%src_root;
    display __LINE__, ": Named Root [", $src_root{name}, "] -> [", $src_root{path}, "]\n" if($verbose);

    foreach(keys(%shadows_roots)) {
        my $shadows_root_name = $_;
        my $shadows_root = $shadows_roots{$_};

        my %root;
        $root{name} = "${shadows_root_name}";
        $root{path} = $shadows_root;
        $root{src} = $src_root{path};
        $roots{$root{path}} = \%root;
        display __LINE__, ": Named Root [", $root{name}, "] -> [", $root{path}, "]\n" if($verbose);
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

            unless(defined($roots{$default_dir})) {
                my %root;
                $root{name} = "default";
                $root{path} = $default_dir;
                if(my $src = processBuildDir($root{path})) {
                    $root{src} = $src;
                    # unless(defined($roots{$src})) {
                    #     my %src_root;
                    #     $src_root{name} = "${src_prefix}default";
                    #     $src_root{path} = $src;
                    #     $roots{$src_root{path}} = \%src_root;
                    # }
                }
                $roots{$root{path}} = \%root;
                display __LINE__, ": Named Root [", $root{name}, "] -> [", $root{path}, "]\n" if($verbose);
            }
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
                        $project_name .= $detect_suffix;
                    }
                    display "Source Detect: $src_dir [$project_name]\n" if($verbose);

                    my %root;
                    $root{name} = "${project_name}";
                    $root{path} = $src_dir;
                    $roots{$root{path}} = \%root;
                    display __LINE__, ": Named Root [", $root{name}, "] -> [", $root{path}, "]\n" if($verbose);
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
                        $project_name .= $detect_suffix;
                    }
                    if(defined($project_name)) {
                        display "Build Detect: $build_dir ($src_dir) [$project_name]\n" if($verbose);
                        #src side
                        my %src_root;
                        $src_root{name} = "${project_name}";
                        $src_root{path} = $src_dir;
                        $roots{$src_root{path}} = \%src_root;
                        display __LINE__, ": Named Root [", $src_root{name}, "] -> [", $src_root{path}, "]\n" if($verbose);
                        #build side
                        my $build_name = getProjectName($build_dir);
                        $build_name = "$subdir" unless(defined($build_name));
                        my %build_root;
                        $build_root{name} = "${build_name}";
                        $build_root{path} = $build_dir;
                        $build_root{src} = $src_dir;
                        $roots{$build_root{path}} = \%build_root;
                        display __LINE__, ": Named Root [", $build_root{name}, "] -> [", $build_root{path}, "]\n" if($verbose);
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
        my %root;
        $root{name} = "${dev_root_name}";
        $root{path} = $dev_root;
        $roots{$root{path}} = \%root;
        display __LINE__, ": Named Root [", $root{name}, "] -> [", $root{path}, "]\n" if($verbose);
    }
    if(-e "$dev_root/.lsdev_shadows" ) {
        my %shadows = parseFileMap("$dev_root/.lsdev_shadows");
        foreach(keys(%shadows)) {
            my $shadow_root_name = $_;
            my $shadow_root = $shadows{$_};
            my %root;
            $root{name} = "${shadow_root_name}";
            $root{path} = $shadow_root;
            $root{src} = $dev_root;
            $roots{$root{path}} = \%root;
            display __LINE__, ": Named Root [", $root{name}, "] -> [", $root{path}, "]\n" if($verbose);
        }
    }
}
display "root=$root_dir default=$default_dir cwd=$cwd\n" if($verbose);

if($display_only eq "current") { #display just the name of the directory request
    my $current;
    if($#matches == -1) {
        $current = $cwd;
    } elsif($#matches == 0 && $matches[0] eq "-") {
        $current = $default_dir;
    } else {
        $current = $matches[0];
    }
    if(my $root = findRoot($current)) {
        answer($roots{$root});
    }
} else { #display all matching directories, and finally answer with the chosen one
    my $rest_dir;
    for(my $i = 0; $i <= $#matches; ++$i) {
        my $match = $matches[$i];
        if($match =~ /\//) {
            if(!defined($rest_dir)) {
                $rest_dir = canonicalize($match);
            } else {
                display "Illegal match: $match ($rest_dir)\n";
            }
            splice(@matches, $i, 1);
        }
    }

    my @choices;
    if($#matches == -1 && $rest_dir && -d $rest_dir) {
        unless(defined($roots{$rest_dir})) {
            my %root;
            $root{name} = "passed";
            $root{path} = canonicalize($rest_dir);
            if(my $src = processBuildDir($root{path})) {
                $root{src} = $src;
                unless(defined($roots{$src})) {
                    my %src_root;
                    $src_root{name} = "${src_prefix}passed";
                    $src_root{path} = $src;
                    $roots{$src_root{path}} = \%src_root;
                }
            }
            $roots{$root{path}} = \%root;
            display __LINE__, ": Named Root [", $root{name}, "] -> [", $root{path}, "]\n" if($verbose);
        }
        push @choices, $rest_dir;
        $rest_dir = undef;
    } elsif($#matches == 0 && $matches[0] =~ /^@(.*)$/) {
        if(open(EMACSCLIENT, "emacsclient -e '(sam-find-directory \"$1\")'|")) {
            my $emacs_dir = <EMACSCLIENT>;
            chomp $emacs_dir;
            $emacs_dir =~ s,",,g;
            $emacs_dir = canonicalize($emacs_dir);
            $emacs_dir = findRoot($emacs_dir) if($root);
            display "Emacs detection: $emacs_dir\n" if($verbose);
            if($emacs_dir) {
                unless(defined($roots{$emacs_dir})) {
                    my %root;
                    $root{name} = "emacs";
                    $root{path} = $emacs_dir;
                    if(my $src = processBuildDir($root{path})) {
                        $root{src} = $src;
                        unless(defined($roots{$src})) {
                            my %src_root;
                            $src_root{name} = "${src_prefix}emacs";
                            $src_root{path} = $src;
                            $roots{$src_root{path}} = \%src_root;
                        }
                    }
                    $roots{$root{path}} = \%root;
                    display __LINE__, ": Named Root [", $root{name}, "] -> [", $root{path}, "]\n" if($verbose);
                }
                push @choices, $emacs_dir;
            }
            close(EMACSCLIENT);
        } else {
            display "Unable to connect emacsclient!\n";
        }
    } elsif($#matches == 0 && $matches[0] eq "-") {
        push @choices, $default_dir;
    } elsif($#matches == -1 && $root && $root_dir) {
        push @choices, $root_dir;
    } else {
        my @match_roots = keys %roots;
        @choices = filterMatches(@match_roots);

        if($read_devdir_list != 2) {
            my @related_choices;
            for(my $i = 0; $i < @choices; ++$i) {
                push @related_choices, $choices[$i] if(isRelated($choices[$i], $root_dir));
            }
            @choices = @related_choices if($#related_choices != -1);
        }
    }

    if(!defined($rest_dir) && !$root && $root_dir) {
        my $current = $cwd;
        $rest_dir = $1 if($current =~ /^$root_dir\/(.*)/);
    }

    my $index;
    if($display_only eq "list") {
        for(my $i = 0; $i < @choices; ++$i) {
            answer($roots{canonicalize($choices[$i])});
        }
    } else {
        while(1) {
            if($#choices <= 0) {
                $index = 0 if($#choices == 0);
                last;
            }
            for(my $i = 0; $i < @choices; ++$i) {
                my $root = $roots{canonicalize($choices[$i])};
                display "[", $i + 1, "] ", generateName($root), " [", $root->{path}, "]";
                if($root->{src}) {
                    my $src_root = $roots{$root->{src}};
                    display " [" . generateName($src_root) . "]";
                }
                display "\n";
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
        display "Chose: $root_dir: $index: '" . canonicalize($choices[$index]) . "' -> '" . $roots{canonicalize($choices[$index])} . "'\n" if($verbose);
        my %root = %{$roots{canonicalize($choices[$index])}};
        if(defined($rest_dir)) {     #handle the rest logic
            my $rest_cd_dir = $root{path} . "/$rest_dir";
            for(my $current = canonicalize($rest_cd_dir); $current && length($current) > length($root{path});
                $current = dirname($current)) {
                if(-d $current) {
                    $root{path} = $current;
                    last;
                }
            }
        }
        answer(\%root);

        if($write_default_file) {
            my @lsdev_defaults;
            push(@lsdev_defaults, glob("~/.lsdev_default"));
            push(@lsdev_defaults, "$root_dir/.lsdev_default") if($root_dir && isRelated($root_dir, $root{path}) == 1);
            foreach(@lsdev_defaults) {
                my $lsdev_default = $_;
                if(open(LSDEV_DEFAULT, ">$lsdev_default")) {
                    display "Writing $lsdev_default -> " . $root{path} . "\n" if($verbose);
                    print LSDEV_DEFAULT $root{path} . "\n";
                    close(LSDEV_DEFAULT);
                }
            }
        }
    }
}


