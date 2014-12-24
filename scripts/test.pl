#!/usr/bin/env perl

use strict;
use warnings;

use Text::Balanced::Marpa;

# -----------

my($parser) = Text::Balanced::Marpa -> new(maxlevel => 'debug');

for my $text ('<>', '<a>')
{
	print '-' x 50, "\n";
	$parser -> text($text);
	$parser -> run;
}

