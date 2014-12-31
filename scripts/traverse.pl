#!/usr/bin/env perl
#
# t/html.pl is another version of this program.

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

if ($parser -> parse(\$text) == 0)
{
	my($attributes);
	my($indent);
	my($text);

	for my $node ($parser -> tree -> traverse)
	{
		next if ($node -> is_root);

		$attributes = $node -> meta;
		$text       = $$attributes{text};
		$text       =~ s/^\s+//;
		$text       =~ s/\s+$//;
		$indent     = $node -> depth - 1;

		print "\t" x $indent, "$text\n" if (length($text) );
	}
}
