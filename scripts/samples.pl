#!/usr/bin/env perl

use strict;
use warnings;

use Text::Balanced::Marpa;

# -----------

my($count)    = 0;
my($maxlevel) = shift || 'debug'; # Try 'info' (without the quotes).
my($parser)   = Text::Balanced::Marpa -> new(maxlevel => $maxlevel, strict_nesting => 0);
my(@text)     =
(
	q||,
	q|a|,
	q|{a}|,
	q|[a]|,
	q|a {b} c|,
	q|a [b] c|,
	q|a {b {c} d} e|,
	q|a [b [c] d] e|,
	q|a {b [c] d} e|,
	q|a <b {c [d (e "f") g] h} i> j|,
	q|<html><head><title>A Title</title></head><body><h1>A Heading</h1></body></html>|,
	q|{a nested { and } are okay as are () and <> pairs and escaped \}\'s };|,
	q|{a nested\n{ and } are okay as are\n() and <> pairs and escaped \}\'s };|,
##	q|{a nested { and } are okay as are unbalanced ( and < pairs and escaped \}'s };|,
##	'q|a| \|b\| q|c| qd',
	q|a "b" c|,
	q|a 'b' c|,
	q|a "b" c "d" e|,
	q|a "b" c 'd' e|,
	q|<: $a :> < b >|,
	q|[% $a %]|,
	q|{Bold [Italic}]|,
	q|<i><b>Bold Italic</b></i>|,
	q|<i><b>Bold Italic</i></b>|,
);

for my $text (@text)
{
	$count++;

	if ($maxlevel ne 'notice')
	{
		print '-' x 50, "\n";
		print "Test $count. Text |$text|\n";
	}

	$parser -> text($text);
	$parser -> parse;

	if ($maxlevel ne 'notice')
	{
		print "Test $count. |$text|\n";
	}
}
