#!/usr/bin/env perl

use strict;
use warnings;

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
	q|a {b} c {|,
	q|a {b {c} d} e|,
);

my($result);
my($text);

for my $i (0 .. $#text)
{
	$count++;

	$text = $prefix[$i] . $text[$i];

	$parser -> pos(length $prefix[$i]);
	$parser -> length(length($text) - $parser -> pos);

	print '        | ';
	printf '%10d', $_ for (1 .. 9);
	print "\n";
	print '        |';
	print '0123456789' for (0 .. 8);
	print "0\n";
	print "Parsing |$text|. pos: ", $parser -> pos, '. length: ', $parser -> length, "\n";

	$result = $parser -> parse(\$text);

	print join("\n", @{$parser -> tree2string}), "\n";
	print "Parse result: $result (0 is success)\n";
}
