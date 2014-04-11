#!/usr/bin/env perl

use v5.10.1;

use strict;
use feature qw(say);
use warnings;
use Text::ParseWords;
use Data::Dumper;

my (@incpath, @libpath, @options, @warnoptions, @inputs, @libs, @rest);

my $cmdline = <>;
$cmdline =~ s|-([ILl]) ([/\w.]+)|-$1$2|g;

my @words = shellwords($cmdline);
my $prog = shift @words;

foreach (@words) {
    if    (/^-I/) { push @incpath,     $'    }
    elsif (/^-W/) { push @warnoptions, "-$'" }
    elsif (/^-l/) { push @libs,        $'    }
    elsif (/^-L/) { push @libpath,     $'    }
    elsif (/^-/)  { push @options,     "-$'" }
    else          { push @rest,        $_    }
}

if (@options || @warnoptions) {
    say "# Options ($prog)";
    say join ' ', @options     if @options;
    say join ' ', @warnoptions if @warnoptions;
    say '';
}

if (@incpath) {
    say "# Include search path";
    say join "\n", @incpath;
    say '';
}

if (@libpath) {
    say "# Library search path";
    say join "\n", @libpath;
    say '';
}

if (@libs) {
    say "# Libraries";
    say join "\n", @libs;
    say '';
}

if (@rest) {
    say "# Input files";
    say join "\n", @rest;
    say '';
}
