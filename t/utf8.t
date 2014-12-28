#!/usr/bin/env perl

use strict;
use utf8;
use warnings;
use warnings qw(FATAL utf8); # Fatalize encoding glitches.
use open     qw(:std :utf8); # Undeclared streams in UTF-8.

use Test::More;

use Text::Balanced::Marpa ':constants';

# -----------

my($count)  = 0;
my($parser) = Text::Balanced::Marpa -> new
(
	open    => ['Δ'],
	close   => ['δ'],
	options => overlap_is_fatal,
);
my(@text) =
(
	q|Escaped opening delimiter\: \Δ.|,
	q|Escaped closing delimiter\: \δ.|,
	q|Δabcδ \ΔΠηληϊά\δεω Ἀχιλῆος\δ|,
	q|Δabc ΔΠηληϊά\δεω Ἀχιλῆοςδ δ Δʎ ʏ ʐ ʑ ʒ ʓ ʙ ʚδ|,
);

my($result);

for my $text (@text)
{
	$count++;

	$result = $parser -> parse(\$text);

	ok($result == 0, "Parsed $text");

	#diag join("\n", @{$parser -> tree2string});
}

done_testing($count);
