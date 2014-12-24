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
#	q||,
#	q|a|,
#	q|{a}|,
#	q|[a]|,
#	q|a {b} c|,
#	q|a [b] c|,
#	q|a {b {c} d} e|,
#	q|a [b [c] d] e|,
#	q|a {b [c] d} e|,
#	q|a <b {c [d (e) f] g} h> i|, # No double quotes yet!
#	q|<html><head><title>A Title</title></head><body><h1>A Heading</h1></body></html>|,
#	q|{a nested { and } are okay as are () and <> pairs and escaped \}'s };|,
#	q|{a nested\n{ and } are okay as are\n() and <> pairs and escaped \}'s };|,
##	q|{a nested { and } are okay as are unbalanced ( and < pairs and escaped \}'s };|,
	q|a "b" c|,
	q|a "b" c "d" e|,
	q|<: $b :>|,
	q|[% $a %]|,
);

for my $text (@text)
{
	$count++;

	print '-' x 50, "\n";
	print "Test $count. Text |$text|\n";
	$parser -> text($text);
	$parser -> parse;
	print "Test $count. |$text|\n";
}
