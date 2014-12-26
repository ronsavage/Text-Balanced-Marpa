#!/usr/bin/env perl

use strict;
use warnings;

use Text::Balanced::Marpa ':constants';

# -----------

my($count)  = 0;
my($parser) = Text::Balanced::Marpa -> new
(
	open    => ['<:' ,'[%'],
	close   => [':>', '%]'],
	options => nesting_is_fatal | print_warnings,
);
my(@text) =
(
	q|<: a :>|,
	q|a [% b <: c :> d %] e|,
	q|a <: b <: c :> d :> e|, # nesting_is_fatal triggers an error here.
);

my($result);

for my $text (@text)
{
	$count++;

	print "Parsing |$text|\n";

	$parser -> text($text);

	$result = $parser -> parse;

	if ($count == 3)
	{
		print "Deliberate error: Failed to parse $text\n";
		print 'Error number: ', $parser -> error_number, '. Error message: ', $parser -> error_message, "\n";
	}

	print '-' x 50, "\n";
}
