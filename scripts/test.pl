#!/usr/bin/env perl

use strict;
use warnings;

use Text::Balanced::Marpa;

# -----------

my($parser) = Text::Balanced::Marpa -> new(maxlevel => 'debug');
my(@text)   =
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
#	q|{a nested { and } are okay as are () and <> pairs and escaped \}'s };|,
#	q|{a nested\n{ and } are okay as are\n() and <> pairs and escaped \}'s };|,
#	q|{a nested { and } are okay as are unbalanced ( and < pairs and escaped \}'s };|,
);

for my $text (@text)
{
	print '-' x 50, "\n";
	$parser -> text($text);
	$parser -> run;
}

