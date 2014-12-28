#!/usr/bin/env perl

use strict;
use warnings;

use Test::More;

use Text::Balanced::Marpa;

# -----------

my($count)  = 0;
my($parser) = Text::Balanced::Marpa -> new
(
	open  => ['['],
	close => [']'],
);
my(@text) =
(
	q||,
	q|a|,
	q|[a]|,
	q|a [b [c] d] e|,
	q|a {b [c] d} e|,
	q|a <b {c [d (e "f") g] h} i> j|,
);

for my $text (@text)
{
	$count++;

	ok($parser -> parse(\$text) == 0, "Parsed $text");

	#diag join("\n", @{$parser -> tree2string});
}

done_testing($count);
