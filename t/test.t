#!/usr/bin/env perl

use strict;
use warnings;

use Test::More;

use Text::Balanced::Marpa;

# -----------

my($count)    = 0;
my($parser)   = Text::Balanced::Marpa -> new;
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
);

my($result);

for my $text (@text)
{
	$count++;

	$parser -> text($text);

	ok($parser -> run == 0, "Parsed $text");
}

done_testing($count);
