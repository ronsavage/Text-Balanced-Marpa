#!/usr/bin/env perl

use strict;
use warnings;

use Text::Balanced::Marpa;

# -----------

my($parser) = Text::Balanced::Marpa -> new
(
	open =>
	[
		'<html>',
		'<head>',
		'<title>',
		'<body>',
		'<h1>',
		'<table>',
		'<tr>',
		'<td>',
	],
	close =>
	[
		'</html>',
		'</head>',
		'</title>',
		'</body>',
		'</h1>',
		'</table>',
		'</tr>',
		'</td>',
	],
);
my($text) =
q|
<html>
	<head>
		<title>A Title</title>
	</head>
	<body>
		<h1>A H1 Heading</h1>
		<table>
			<tr>
				<td>A table cell</td>
			</tr>
		</table>
	</body>
</html>
|;

$parser -> text($text);

if ($parser -> parse == 0)
{
	my($attributes);
	my($indent);
	my($text);

	# Note: walk_down takes a hashref!

	$parser -> tree -> walk_down
	({
		callback => sub
		{
			my($node, $opts) = @_;

			return 1 if ($node -> is_root); # Keep walking.

			$attributes = $node -> attributes;
			$text       = $$attributes{text};
			$text       =~ s/^\s+//;
			$text       =~ s/\s+$//;
			$indent     = scalar($node -> ancestors) - 1;

			print "\t" x $indent, "$text\n" if (length($text) );

			return 1; # Keep walking.
		},
		_depth => 0,
	});
}
