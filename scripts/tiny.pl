#!/usr/bin/env perl

use strict;
use warnings;

use Text::Balanced::Marpa ':constants';

# -----------

my($count)    = 0;
my($maxlevel) = shift || 'debug'; # Try 'info' (without the quotes).
my($parser)   = Text::Balanced::Marpa -> new
(
	open     => ['{'],
	close    => ['}'],
	maxlevel => $maxlevel,
);
my(@text) =
(
#	q||,
#	q|a|,
	q|{a}|,
);

my($result);

for my $text (@text)
{
	$count++;

	if ($maxlevel ne 'notice')
	{
		print '-' x 50, "\n";
		print "Start test  $count. Input |$text|\n";
	}

	$result = $parser -> parse(\$text);

	print join("\n", @{$parser -> tree2string}), "\n";
	print "Parse result: $result (0 is success)\n";

	if ($maxlevel ne 'notice')
	{
		print "Finish test $count. Input |$text|\n";
	}
}
