#!/usr/bin/perl

use Cwd;
use Cwd 'abs_path';
use File::Basename;
use File::Spec;

use strict;

my $verbose = 0;
my $write_default_file = 0;
my $read_devdir_list = 1;
my $detect_rest = 1;
my $detect_devdirs = 1;
my $display_only;
my $match_only;
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

my @matches;
my %dev_roots;
my @build_roots;

sub mycaller {
    my @c = caller(1);
    return $c[2];
}

sub display {
    #print STDERR mycaller(), ": ";
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

sub parseOptions {
    while(@_) {
        my $option = shift @_;
        if($option eq "-w") {
            $write_default_file = 1;
        } elsif($option eq "-r") {
            $detect_rest = 0;
        } elsif($option eq "-c") {
            $cwd = shift @_;
        } elsif($option eq "-me") {
            $match_only = "exact";
        } elsif($option eq "-mr") {
            $match_only = "regexp";
        } elsif($option eq "-mw") {
            $match_only = "word";
        } elsif($option eq "-mi") {
            $match_only = "ido";
        } elsif($option eq "-m") {
            $match_only = shift @_;
        } elsif($option eq "-d") {
            $display_only = "default";
        } elsif($option eq "-b") {
            $read_devdir_list = -1;
        } elsif($option eq "-tp") {
            $answer = "path";
        } elsif($option eq "-tn") {
            $answer = "name";
        } elsif($option eq "-ta") {
            $answer = "all";
        } elsif($option eq "-tr") {
            $answer = "rest";
        } elsif($option eq "-ts") {
            $answer = "simple_name";
        } elsif($option eq "-t") {
            $answer = shift @_;
        } elsif($option eq "-a") {
            $read_devdir_list = 2;
        } elsif($option eq "-m") {
            $detect_devdirs = 0;
        } elsif($option eq "-l") {
            $display_only = "list";
        } elsif($option eq "-p") {
            $display_only = "current";
        } elsif($option eq "-h") {
            showHelp();
        } elsif($option eq "-v") {
            $verbose = 1;
        } else {
            push @matches, "$option";
        }
    }
}

parseOptions(split(/ /, $ENV{LSDEV_FLAGS})) if($ENV{LSDEV_FLAGS});
parseOptions(@ARGV);

$display_only = "default" if(!defined($display_only) &&
                             $#matches == 0 && $matches[0] eq "-");

if($display_only eq "current") {
    $read_devdir_list = 3;
    $answer = "name" if(!defined($answer));
} elsif($display_only eq "list") {
    $answer = "all" if(!defined($answer));
} else {
    $answer = "path" if(!defined($answer));
}

sub answer {
    my ($root) = @_;
    my $output;
    if($answer eq "all") {
        $output = generateRootName($root) . " [" . $root->{path} . "]";
        if($root->{source}) {
            my $src_root = findRoot($root->{source});
            $output .= " [" . generateRootName($src_root) . "]";
        }
    } elsif($answer eq "simple_name") {
        $output = getPathConfig($root->{path}, "prompt");
        $output = generateRootName($root) unless($output);
    } elsif($answer eq "name") {
        $output = generateRootName($root);
    } elsif($answer eq "rest") {
        $output = getRestDir($root->{path});
        $output = "<root>" unless(length($output));
    } elsif($answer eq "path") {
        $output = $root->{path};
    }
    if($output) {
        print STDOUT "$output\n";
        display "Answering: $output\n" if($verbose);
    }
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
                if($line =~ /([\w-\/.]+\/configure)/) {
                    $src_dir = dirname($1);
                }
                last if($src_dir && length($src_dir));
            }
            close(CONFIG_STATUS);
        }
    } elsif(-e "${build_dir}/.lsdev_config") {
        my $source = getPathConfig(${build_dir}, "source");
        if($source && $dev_roots{$source}) {
            $src_dir = $dev_roots{$source};
        } else {
            $src_dir = $source;
        }
    }
    $src_dir = canonicalize($src_dir, $build_dir) if(defined($src_dir));
    return $src_dir;
}

sub resolveLinks {
    my ($file) = @_;
    $file = abs_path($file) if(-e "$file");
    return $file;
}

sub canonicalize {
    my ($file, $base) = @_;
    my @globs = glob($file);
    my $result = @globs ? $globs[0] : $file;
    if(!File::Spec->file_name_is_absolute($result) && defined($base)) {
        $result = File::Spec->rel2abs($result, $base);
    }
    $result =~ s,/*$,,g;
    $result =~ s,/+,/,g;
    $result = "/" unless(length($result));
    return $result;
}

sub parseConfig {
    my ($file) = @_;
    my %result;
    display "ProcessingConfigFile: $file\n" if($verbose);
    if(open(FILE, "<$file")) {
        while(my $line = <FILE>) {
            chomp($line);
            $line =~ s,#.*$,,;
            if($line =~ /^(.*)=(.*)$/) {
                $result{$1} = $2;
            }
        }
        close(FILE);
    }
    return \%result;
}

sub parseFileMap {
    my ($file) = @_;
    my %result = %{parseConfig($file)};
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

my %path_config;
if(-e glob("~/.dev_directories")) {
    my $dev_directories_path = glob("~/.dev_directories");
    display "ProcessingDevDirectories: $dev_directories_path\n" if($verbose);
    if(open(FILE, "<$dev_directories_path")) {
        my $path;
        while(my $line = <FILE>) {
            chomp($line);
            $line =~ s,#.*$,,;
            if($line =~ /^\[(.*)\]$/) {
                $path = $1;
            } elsif($line =~ /^(.*)=(.*)$/) {
                if($path) {
                    if(!exists $path_config{$path}) {
                        if(my $config = parsePathConfig($path)) {
                            $path_config{$path} = $config;
                        } else {
                            my %tmp_config;
                            $path_config{$path} = \%tmp_config;
                        }
                    }
                    my $k = $1;
                    my $v = $2;
                    $v = canonicalize($v, dirname($dev_directories_path)) if($k eq "path" || $k eq "source");
                    display "Found PathConfig: {$path}{$k} -> $v\n" if($verbose);
                    $path_config{$path}->{$k} = $v;
                } else {
                    my $n = $1;
                    my $v = $2;
                    if($n eq "builds") {
                        my @b = split(/,/, $v);
                        foreach(@b) {
                            push @build_roots, canonicalize($v, dirname($dev_directories_path));
                        }
                    } else {
                        my $p = canonicalize($v, dirname($dev_directories_path));
                        display "Found DevDirectory: $n -> $p\n" if($verbose);
                        $dev_roots{$n} = $p;
                        addRoot($n, $p);
                    }
                }
            }
        }
        close(FILE);
    }
}

sub parsePathConfig {
    my ($path) = @_;
    my $lsdev_config_file = "$path/.lsdev_config";
    return parseConfig($lsdev_config_file) if(-e $lsdev_config_file);
    return undef;
}

sub getPathConfig {
    my ($path, $key) = @_;
    $path_config{$path} = parsePathConfig($path) if(!exists $path_config{$path});
    my $result;
    $result = $path_config{$path}->{$key} if($path_config{$path} && exists $path_config{$path}->{$key});
    display " PathConfig: $path($key) -> '" . defined($result) ? $result : "(undef)" . "'\n" if($verbose);
    return $result;
}

sub addRestDir {
    my ($root_dir, $rest_dir) = @_;
    my $result = $root_dir;
    if(-d "$result/$rest_dir") {
        $result .= "/$rest_dir";
        for(my $current = canonicalize($result); $current && length($current) > length($root_dir);
            $current = dirname($current)) {
            if(-d $current) {
                $result = $current;
                last;
            }
        }
    }
    return $result;
}

sub getRestDir {
    my ($root_dir) = @_;
    my $rest_dir;
    $rest_dir = $1 if($cwd =~ /^$root_dir\/(.*)/);
    my $resolved_root_dir = resolveLinks($root_dir);
    my $resolved_cwd = resolveLinks($cwd);
    $rest_dir = $1 if($resolved_cwd =~ /^$resolved_root_dir\/(.*)/);
    display "GetRestDir($cwd): $root_dir: -> $rest_dir\n" if($verbose);
    return $rest_dir;
}

sub getProjectName {
    my ($path) = @_;
    my $result = getPathConfig($path, "name");
    return $result;
}

my %roots;
my $default_dir;
my $root_dir;

sub isRootSource {
    my ($root) = @_;
    return (!defined($root->{source}) || isPathSame($root->{path}, $root->{source}));
}

sub isRootBuild {
    my ($root) = @_;
    return defined($root->{source});
}

sub findRoot_internal {
    my ($path, $exact) = @_;
    my $result;
    $path = resolveLinks($path);
    foreach(keys(%roots)) {
        my $root = $roots{$_};
        my $root_path = resolveLinks($root->{path});
        if($exact) {
            return $root if($root_path eq $path);
        } elsif($path =~ /^$root_path/ && (length($root_path) > length($result->{path}))) {
            $result = $root;
        }
    }
    return $result;
}

sub findRoot {
    my ($path, $recurse) = @_;
    $path = canonicalize($path);
    my $result = findRoot_internal($path, 0);
    if(!$result) {
        my $last;
        for(my $current = $path, $last; $current; $current = dirname($current)) {
          last if($current eq $last);
          my $p = $current;
          $p = resolveLinks($current);
          if(my $root = findRoot_internal($p, 1)) {
              $result = $root;
              last;
          }
          last if(!$recurse || $current eq "/");
          $last = $current;
      }
    }
    display "FindRoot: $path: -> " . ($result ? $result->{name} : "(notfound)") . "\n" if($verbose);
    return $result;
}

sub sortRootPredicate {
    my ($root1, $root2) = @_;
    return 1 if(isRootBuild($root1) && !isRootBuild($root2));
    return -1 if(!isRootBuild($root1) && isRootBuild($root2));

    my $path1 = $root1->{path};
    my $path2 = $root2->{path};
    return (stat($path2))[9] <=> (stat($path1))[9];
}

sub sortRootPathPredicate {
    my ($path1, $path2) = @_;

    my $root1 = findRoot($path1);
    my $root2 = findRoot($path2);
    return 1 if(isRootBuild($root1) && !isRootBuild($root2));
    return -1 if(!isRootBuild($root1) && isRootBuild($root2));

    return (stat($path2))[9] <=> (stat($path1))[9];
}

sub findRootBuilds {
    my ($root) = @_;
    $root = findRoot($root->{source}) if(isRootBuild($root));

    my @result;
    foreach(keys(%roots)) {
        my $build_root = $roots{$_};
        push @result, $build_root if(isRootBuild($build_root) && (!$root || findRoot($build_root->{source}) == $root));
    }
    @result = sort { sortRootPredicate($a, $b) } @result;
    return @result;
}

sub findRootPath {
    my ($path, $recurse) = @_;
    if(my $root = findRoot($path, $recurse)) {
        return $root->{path};
    }
    return undef;
}

sub addRoot {
    my ($name, $path, $source) = @_;
    $path = canonicalize($path);
    $source = resolveLinks(canonicalize($source)) if($source);

    my $root_location = resolveLinks($path);
    if($source && isPathSame($path, $source)) {
        my $source_root = findRoot($path);
        if($source_root) {
            $source_root->{source} = $source;
            if($verbose) {
                display "AddedBuild(", $source_root->{key}, ") {" . $source . "} ($root_location) [" . mycaller() . "]\n";
            }
            return $source_root;
        }
    }

    my %root;
    %root = %{$path_config{$path}} if($path_config{$path} && exists $path_config{$path});
    $root{name} = $name;
    $root{path} = $path;
    $root{source} = $source;
    {
        my $root_key = $root_location;
        $root_key =~ s,/,_,g;
        if($source && !isPathSame($path, $source)) {
            $root_key = $build_prefix . "::" . $root_key;
        } else {
            $root_key = $src_prefix . "::" . $root_key;
        }
        $root_key .= "::" . $name;
        $root{key} = $root_key;
    }
    #$root{source} = $roots{$root_key}->{source} if(!$source && $roots{$root_key});
    $roots{$root{key}} = \%root;
    if($verbose) {
        display "Named Root(", $root{key}, ") [", $root{name}, "] -> [", $root{path}, "] {" . $root{source} . "} ($root_location) [" . mycaller() . "]\n";
    }
    return \%root;
}

sub generateBuildName {
    my ($root) = @_;

    my $name = $root->{name};
    if(isRootBuild($root)) {
        my $src_root = findRoot($root->{source});
        my $src_name = $src_root->{name};

        my @builds = findRootBuilds($src_root);
        $name = getProjectName($root->{path}) if($#builds == 0);
        unless($name =~ /$src_name/i) {
            $name = "_${name}" if(length($name));
            $name = "${src_name}${name}";
        }
    }
    display "Generated Name: $name\n" if($verbose);
    return $name;
}

sub generateRootName {
    my ($root) = @_;

    my $name = $root->{name};
    $name = generateBuildName($root) if(isRootBuild($root));
    return (isRootSource($root) ? $src_prefix : $build_prefix) . $name;
}

sub isPathSame {
    my ($path1, $path2, $resolve) = @_;
    $resolve = 1 unless(defined($resolve));
    my $result = 0;
    if($path1 eq $path2) {
        $result = 1;
    } elsif($resolve && resolveLinks($path1) eq resolveLinks($path2)) {
        $result = 1;
    }
    display "IsPathSame: '$path1' vs '$path2' :: $result\n" if($verbose);
    return $result;
}

sub findDevRootName {
    my ($path, $recurse) = @_;
    my $result = undef;
    $result = getProjectName($path);
    if(!$result) {
        foreach(keys(%dev_roots)) {
            my $dev_root_name = $_;
            my $dev_root_path = $dev_roots{$dev_root_name};
            if("$path/" =~ /^$dev_root_path\// && (length($dev_root_path) > length($dev_roots{$result}))) {
                $result = $dev_root_name;
            }
        }
    }
    if(!$result) {
      FIND: for(my $current = canonicalize($path); $current; $current = dirname($current)) {
          foreach(keys(%dev_roots)) {
              my $dev_root_name = $_;
              if(isPathSame($dev_roots{$dev_root_name}, $current)) {
                  $result = $dev_root_name;
                  last FIND;
              }
          }
          last if(length($current) <= 1 || !$recurse);
      }
    }
    display "FindDevRootName: $path -> $result\n" if($verbose);
    return $result;
}

sub findDevRoot {
    my ($path, $recurse) = @_;
    if(my $name = findDevRootName($path, $recurse)) {
        my $root = $dev_roots{$name};
        display "FindDevRoot: $path -> '$root' ($name)\n" if($verbose);
        return $root;
    }
    display "FindDevRoot: $path -> not found\n" if($verbose);
    return undef;
}

sub isRootRelated {
    my ($path1, $path2) = @_;

    my $result = 0;
    if(isPathSame($path1, $path2)) {
        $result = 3;
    } else {
        my $root1 = findRoot($path1);
        my $root2 = findRoot($path2);
        if($root1 && $root2) {
            if($root1->{source}) {
                if(isPathSame($root1->{source}, $root2->{path})) {
                    $result = 1;
                } elsif(isPathSame($root1->{source}, $root2->{source})) {
                    $result = 2;
                }
            } elsif($root2->{source}) {
                $result = 1 if(isPathSame($root1->{path}, $root2->{source}));
            }
        }
    }
    display "IsRootRelated: ${path1}::${path2} -> $result\n" if($verbose);
    return $result;
}

sub filterMatches_internal {
    my ($roots, $matches, $comparator) = @_;

    my @result;
    foreach(@{$roots}) {
        my $root = findRoot($_);
        next if($root->{ignore} || !$root->{path});
        if($#matches != -1) {
            foreach(@{$matches}) {
                my $match = $_;
                my $inverse = 0;
                if($match =~ /^-(.*)/) {
                    $match = $1;
                    $inverse = 1;
                }
                my $matched = 0;
                if($match eq "src" || $match eq "source") {
                    $matched = isRootSource($root);
                } elsif($match eq "build") {
                    $matched = isRootBuild($root);
                } elsif($match =~ /^path:(.*)/) {
                    $matched = ($root->{path} =~ /$1/i);
                } else {
                    $matched = $comparator->($match, $root);
                }
                $matched = !$matched if($inverse);
                #display "FilterMatches: $match: [" . $root->{name} . "::" . $root->{path} . "]: $matched\n" if($verbose);
                unless($matched) {
                    $root = undef;
                    last;
                }
            }
        } elsif($read_devdir_list != 2 && $read_devdir_list != -1 && isPathSame($root->{path}, $root_dir)) { #if there are no arguments filter out pwd
            next;
        }
        if(defined($root)) {
            display "AddChoice: [" . $root->{name} . "::" . $root->{path} . "::" . generateRootName($root) . "]\n" if($verbose);
            push @result, $root->{path} if(defined($root));
        }
    }
    @result = sort { sortRootPathPredicate($a, $b) } @result;
    return @result;
}

sub filterMatches {
    my ($roots, $matches) = @_;

    my @result;
    if($match_only eq "exact") {
        @result = filterMatches_internal($roots, $matches, sub {
            my ($match, $root) = @_;
            my $prefix;
            my $name = $match;
            if($match =~ /^${src_prefix}/) {
                $prefix = "${src_prefix}";
            } elsif($match =~ /^${build_prefix}/) {
                $prefix = "${build_prefix}";
            }
            $name = substr $name, length($prefix) if($prefix);
            return 1 if((!defined($prefix) || ($prefix eq $build_prefix && isRootBuild($root))) && ($name eq generateBuildName($root)));
            return 1 if((!defined($prefix) || ($prefix eq $src_prefix && isRootSource($root))) && ($name eq $root->{name}));
            return 0;
        });
    } else {
        if((!defined($match_only) || $match_only eq "word") && $#result == -1) {
            @result = filterMatches_internal($roots, $matches, sub {
                my ($match, $root) = @_;
                my $name = generateRootName($root);
                my $ws = "[_-]";
                return 1 if($name =~ /^${match}${ws}/);
                return 1 if($name =~ /$ws${match}$/);
                return 1 if($name =~ /$ws${match}$ws/);
                return 0;
            })
        }
        if((!defined($match_only) || $match_only eq "regexp") && $#result == -1) {
            @result = filterMatches_internal($roots, $matches, sub {
                my ($match, $root) = @_;
                return generateRootName($root) =~ /$match/i;
            })
        }
        if((!defined($match_only) || $match_only eq "ido") && $#result == -1) {
            @result = filterMatches_internal($roots, $matches, sub {
                my ($match, $root) = @_;
                $match =~ s,(.),\1.*,g;
                return generateRootName($root) =~ /$match/i;
            })
        }
    }
    return @result;
}

#figure out where I am in a shadow build and my relevent source dir
if(my $build_marker = findAncestor("CMakeCache.txt") || findAncestor("config.status") || findAncestor(".lsdev_config")) {
    $root_dir = dirname($build_marker);
    $read_devdir_list = -2 if($read_devdir_list == 1 &&
                             ($#matches == -1 || $matches[0] eq "-"));
    if(my $src_dir = processBuildDir("$root_dir")) {
        display "SRCDIR: $root_dir -> $src_dir\n" if($verbose);
        $default_dir = $src_dir;
        my $src_project_name = findDevRootName($src_dir);
        $src_project_name = basename($src_dir) unless(defined($src_project_name));
        addRoot($src_project_name ? "${src_project_name}" : "src", $src_dir);
        my $bld_project_name = findDevRootName($root_dir);
        addRoot($bld_project_name ? "${bld_project_name}" : basename($root_dir), $root_dir, $src_dir);
    } else {
        my $project_name = getProjectName($root_dir);
        unless(defined($project_name)) {
            $project_name = findDevRootName($root_dir);
            $project_name = basename($root_dir) unless(defined($project_name));
            $project_name .= $detect_suffix;
        }
        display "Source Detect: $root_dir [$project_name]\n" if($verbose);
        addRoot($project_name, $root_dir);
    }
}

if(my $dev_directory = findDevRoot($cwd, 1)) {
    $read_devdir_list = -2 if($read_devdir_list == 1 &&
                              ($#matches == -1 || $matches[0] eq "-"));
    $root_dir = $dev_directory unless(defined($root_dir));
} elsif(my $shadows_file = findAncestor(".lsdev_shadows")) {    #figure out all the shadows listed in my relevent source dir
    display " Found $shadows_file!\n" if($verbose);
    my $shadows_file_dir = dirname($shadows_file);
    my $shadows_dir = $shadows_file_dir;
    my $project_name = findDevRootName($shadows_dir);
    my %shadows = parseFileMap("$shadows_file");
    $read_devdir_list = -2 if($read_devdir_list == 1 &&
                              ($#matches == -1 || $matches[0] eq "-"));
    $root_dir = $shadows_file_dir unless(defined($root_dir));

    my $src_root = addRoot($project_name ? "${project_name}" : "src", $shadows_dir);
    foreach(keys(%shadows)) {
        my $shadow_root_name = $_;
        my $shadow_root = $shadows{$_};
        addRoot($shadow_root_name, $shadow_root, $src_root->{path});
    }
} elsif(my $src_marker = findAncestor(".lsdev_config") || findAncestor("configure")) {
    $read_devdir_list = -2 if($read_devdir_list == 1 &&
                              ($#matches == -1 || $matches[0] eq "-"));
    $root_dir = dirname($src_marker) unless(defined($root_dir));
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
                next if(getPathConfig($src_dir, "ignore"));
                if(-d $src_dir && processSourceDir($src_dir)) {
                    my $project_name = getProjectName($src_dir);
                    unless(defined($project_name)) {
                        $project_name = findDevRootName($src_dir);
                        $project_name = basename($src_dir) unless(defined($project_name));
                        $project_name .= $detect_suffix;
                    }
                    display "Source Detect: $src_dir [$project_name]\n" if($verbose);
                    addRoot($project_name, $src_dir);
                }
            }
            closedir(SOURCES);
        }
    }
}
#process anything that looks like a build
if(length(@build_roots)) {
    for(my $i = 0; $i <= $#build_roots; $i++) {
        my $build = $build_roots[$i];
        display "Looking at build: $build\n" if($verbose);
        if( $detect_devdirs && -d "$build" && opendir(BUILDS, $build) ) {
            while(my $subdir = readdir(BUILDS)) {
                next if($subdir eq "." || $subdir eq "..");
                my $build_dir = "$build/$subdir";
                next if(getPathConfig($build_dir, "ignore"));
                my $src_dir;
                $src_dir = processBuildDir($build_dir) if(-d $build_dir);
                if(defined($src_dir) && ($read_devdir_list >= 1 || isPathSame($src_dir, $root_dir) || isPathSame($src_dir, $default_dir))) {
                    my $project_name = getProjectName($src_dir);
                    unless(defined($project_name)) {
                        $project_name = findDevRootName($src_dir);
                        $project_name = basename($src_dir) unless(defined($project_name));
                        $project_name .= $detect_suffix;
                    }
                    if(defined($project_name)) {
                        display "Build Detect: $build_dir ($src_dir) [$project_name]\n" if($verbose);
                        #src side
                        my $src_root = addRoot($project_name, $src_dir);
                        #build side
                        my $build_name = getProjectName($build_dir);
                        $build_name = "$subdir" unless(defined($build_name));
                        addRoot($build_name, $build_dir, $src_root->{path});
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
        if(my $src_dir = processBuildDir("$dev_root")) {
            display "SRCDIR: $root_dir -> $src_dir\n" if($verbose);
            my $src_project_name = findDevRootName($src_dir);
            addRoot($src_project_name ? "${dev_root_name}" : "src", $src_dir);
            my $bld_project_name = findDevRootName($dev_root);
            addRoot($bld_project_name ? "${dev_root_name}" : basename($dev_root), $dev_root, $src_dir);

        } else {
            addRoot($dev_root_name, $dev_root);
        }
    }
    if(-e "$dev_root/.lsdev_shadows" ) {
        my %shadows = parseRoots("$dev_root/.lsdev_shadows");
        foreach(keys(%shadows)) {
            my $shadow_root_name = $_;
            my $shadow_root = $shadows{$_};
            addRoot($shadow_root_name, $shadow_root, $dev_root);
        }
    }
}

#figure out default
unless(defined($default_dir)) {
    my $lsdev_default_file = findAncestor(".lsdev_default", $root_dir);
    $lsdev_default_file = glob("~/.lsdev_default") unless($lsdev_default_file);
    if($lsdev_default_file && -e $lsdev_default_file) {
        my $lsdev_default_file_dir = dirname($lsdev_default_file);
        if(!$root_dir || length(resolveLinks($lsdev_default_file_dir)) >= length(resolveLinks($root_dir))) {
            display " Found $lsdev_default_file!\n" if($verbose);
            if(open(LSDEV_DEFAULT, "<$lsdev_default_file")) {
                $default_dir = <LSDEV_DEFAULT>;
                display "   Default $default_dir\n" if($verbose);
                chomp($default_dir);
                close(LSDEV_DEFAULT);
                addRoot("default", $default_dir) unless(defined(findRoot($default_dir)));
            }
        }
    }
}

display "root=$root_dir default=$default_dir cwd=$cwd\n" if($verbose);

if($display_only eq "default") { #display the currently mapped default
    if(my $root = findRoot($default_dir, 1)) {
        answer($root);
    }
}elsif($display_only eq "current") { #display just the name of the directory request
    my $current;
    if($#matches == -1) {
        $current = $cwd;
    } elsif($#matches == 0 && $matches[0] eq "-") {
        $current = $default_dir;
    } else {
        $current = $matches[0];
    }
    if(my $root = findRoot($current, 1)) {
        answer($root);
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
        unless(defined(findRoot($rest_dir))) {
            my $root = addRoot("passed", $rest_dir);
            if(my $src = processBuildDir($root->{path})) {
                $root->{source} = $src;
                addRoot("${src_prefix}passed", $src) unless(defined(findRoot($src)));
            }
        }
        push @choices, $rest_dir;
        $rest_dir = undef;
    } elsif($#matches == 0 && $matches[0] eq "-") {
        push @choices, $default_dir;
    } elsif($#matches == -1 && !$detect_rest && $root_dir) {
        push @choices, $root_dir;
    } else {
        foreach(keys %roots) {
            my $root = $roots{$_};
            push @choices, $root->{path};
        }
        @choices = filterMatches(\@choices, \@matches);
        if($read_devdir_list != 2) {
            my @related_choices;
            for(my $i = 0; $i < @choices; ++$i) {
                my $related = isRootRelated($choices[$i], $root_dir);
                push @related_choices, $choices[$i] if($related == 1 || $related == 3);
            }
            @choices = @related_choices if($#related_choices != -1);
        }
    }
    $rest_dir = getRestDir($root_dir) if(!defined($rest_dir) && $detect_rest && $root_dir);
    {
        my %seen;
        my %default;
        my @uniq_choices;
        for(my $i = 0; $i < @choices; ++$i) {
            my $root = findRoot($choices[$i]);
            my $root_name = generateRootName($root);
            $default{source} = $root->{source} unless(exists($default{source}));

            if($read_devdir_list != 2 && isPathSame($default{source}, $root->{source})) {
                $default{source} = $root->{source};
                if(getPathConfig($root->{path}, "default")) {
                    if($default{path}) {
                        display "Default: too many!\n" if($verbose);
                        $default{source} = "";
                        $default{path} = 0;
                    } else {
                        $default{path} = $root->{path};
                    }
                }
            } else {
                display "Default: incompatible: " . $default{source} . " vs " . $root->{source} . "\n" if($verbose);
                $default{source} = "";
                $default{path} = 0;
            }
            if($seen{$root_name}) {
                display "Filtered: $root_name\n" if($verbose);
            } else {
                push @uniq_choices, $root->{path};
                $seen{$root_name} = 1;
            }
        }
        if($default{path}) {
            @choices = ($default{path});
        } else {
            @choices = @uniq_choices;
        }
    }

    my $index;
    if($display_only eq "list") {
        for(my $i = 0; $i < @choices; ++$i) {
            my $root = findRoot($choices[$i]);
            if($root) {
                $root->{path} = addRestDir($root->{path}, $rest_dir) if(defined($rest_dir));
                answer($root);
            }
        }
    } else {
        while(1) {
            if($#choices <= 0) {
                $index = 0 if($#choices == 0);
                last;
            }
            for(my $i = 0; $i < @choices; ++$i) {
                my $root = findRoot($choices[$i]);
                display "[", $i + 1, "] ", generateRootName($root), " [", $root->{path}, "]";
                if($root->{source}) {
                    my $src_root = findRoot($root->{source});
                    display " [" . generateRootName($src_root) . "]";
                }
                display "\n";
            }
            display "[1..", $#choices+1, "]";
            display " ($rest_dir)" if(defined($rest_dir));
            display "> ";
            my $choice = <STDIN>;
            chomp $choice;
            if($choice =~ /^[0-9]+$/) {
                $index = $choice - 1;
                last if($index >= 0 && $index <= $#choices);
            }
            push @matches, "$choice";
            @choices = filterMatches(\@choices, \@matches);
        }
    }
    if(defined($index)) {
        display "Chose: $root_dir($rest_dir): $index: '" . canonicalize($choices[$index]) . "' -> '" . findRoot($choices[$index]) . "'\n" if($verbose);
        my %root = %{findRoot($choices[$index])};
        $root{path} = addRestDir($root{path}, $rest_dir) if(defined($rest_dir));
        answer(\%root);

        if($write_default_file) {
            my %lsdev_defaults;
            $lsdev_defaults{glob("~/.lsdev_default")} = $root{path};
            if($root_dir && isRootRelated($root_dir, $root{path}) == 1) {
                $lsdev_defaults{$root{path} . "/.lsdev_default"} = $root_dir;
                $lsdev_defaults{"$root_dir/.lsdev_default"} = $root{path};
            }
            foreach(keys(%lsdev_defaults)) {
                my $lsdev_default_file = $_;
                my $lsdev_default = $lsdev_defaults{$lsdev_default_file};
                if(open(LSDEV_DEFAULT, ">$lsdev_default_file")) {
                    display "Writing $lsdev_default_file -> " . $lsdev_default . "\n" if($verbose);
                    print LSDEV_DEFAULT $lsdev_default . "\n";
                    close(LSDEV_DEFAULT);
                }
            }
        }
    }
}


