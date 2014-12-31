#!/usr/bin/env perl

use strict;
use warnings;

use Test::More;

use Text::Balanced::Marpa;

# -----------

my($count)  = 0;
my($parser) = Text::Balanced::Marpa -> new
(
	open  => ['{'],
	close => ['}'],
);
my(@prefix) =
(
	'Skip me ->',
	"I've already parsed up to here ->",
);
my(@text) =
(
	q|a {b} c|,
	q|a {b {c} d} e|,
);

my($text);

for my $i (0 .. $#text)
{
	$count++;

	$text = $prefix[$i] . $text[$i];

	$parser -> pos(length $prefix[$i]);
	$parser -> length(length($text) - $parser -> pos);

	ok($parser -> parse(\$text) == 0, "Parsed: $text");

	#diag join("\n", @{$parser -> tree2string});
}

print "# Internal test count: $count\n";

done_testing($count);
