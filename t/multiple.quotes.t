#!/usr/bin/env perl

use strict;
use warnings;

use Test::More;

use Text::Balanced::Marpa ':constants';

# -----------

my($count)  = 0;
my($parser) = Text::Balanced::Marpa -> new
(
	open    => ['<', '{', '[', '(', '"', "'"],
	close   => ['>', '}', ']', ')', '"', "'"],
	options => overlap_is_fatal,
);
my(@text) =
(
	q||,
	q|a|,
	q|{a}|,
	q|[a]|,
	q|a {b} c|,
	q|a [b] c|,
	q|a {b {c} d} e|,
	q|a <b {c> d} e|, # overlap_is_fatal triggers an error here.
	q|a [b [c] d] e|,
	q|a {b [c] d} e|,
	q|a <b {c [d (e "f") g] h} i> j|,
	q|Do you realize I said "I sang 'Δ Lady'" at the karaoke bar? [Contains UTF8]|,
);

my($result);

for my $text (@text)
{
	$count++;

	$parser -> text($text);

	$result = $parser -> parse;

	if ($count == 8)
	{
		ok($result == 1, "Deliberate error: Failed to parse $text");
	}
	else
	{
		ok($result == 0, "Parsed $text");
	}

	diag join("\n", @{$parser -> tree -> tree2string});
}

done_testing($count);
