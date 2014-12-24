#!/usr/bin/env perl

use strict;
use warnings;

use Text::Balanced::Marpa;

# -----------

my($count)    = 0;
my($maxlevel) = shift || 'debug'; # Try 'info' (without the quotes).
my($parser)   = Text::Balanced::Marpa -> new(maxlevel => $maxlevel);
my(@text)     =
(
	q||,
	q|a|,
	q|{a}|,
	q|[a]|,
	q|a{b}c|,
	q|a[b]c|,
	q|a{b{c}d}e|,
	q|a[b[c]d]e|,
	q|a{b[c]d}e|,
	q|a<b{c[d(e)f]g}h>i|, # No double quotes yet!
	q|{a nested { and } are okay as are () and <> pairs and escaped \}'s };|,
	q|{a nested\n{ and } are okay as are\n() and <> pairs and escaped \}'s };|,
#	q|{a nested { and } are okay as are unbalanced ( and < pairs and escaped \}'s };|,
);

for my $text (@text)
{
	$count++;

	print '-' x 50, "\n";
	print "Test $count: $text\n";
	$parser -> text($text);
	$parser -> run;
	print "Test $count: $text\n";
}

