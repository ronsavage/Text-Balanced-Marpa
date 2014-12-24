package Text::Balanced::Marpa;

use strict;
use warnings;
use warnings qw(FATAL utf8); # Fatalize encoding glitches.
use open     qw(:std :utf8); # Undeclared streams in UTF-8.

use Log::Handler;

use Marpa::R2;

use Moo;

use Tree::DAG_Node;

use Types::Standard qw/Any ArrayRef HashRef Int Str/;

use Try::Tiny;

has bnf =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has grammar =>
(
	default  => sub {return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has known_events =>
(
	default  => sub{return {} },
	is       => 'rw',
	isa      => HashRef,
	required => 0,
);

has logger =>
(
	default  => sub{return undef},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has maxlevel =>
(
	default  => sub{return 'notice'},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has minlevel =>
(
	default  => sub{return 'error'},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has node_stack =>
(
	default  => sub{return []},
	is       => 'rw',
	isa      => ArrayRef,
	required => 0,
);

has quote_count =>
(
	default  => sub{return 0},
	is       => 'rw',
	isa      => Int,
	required => 0,
);

has recce =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has stats =>
(
	default  => sub{return {} },
	is       => 'rw',
	isa      => HashRef,
	required => 0,
);

has tree =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has text =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

our $VERSION = '1.00';

# ------------------------------------------------

sub BUILD
{
	my($self) = @_;

	if (! defined $self -> logger)
	{
		$self -> logger(Log::Handler -> new);
		$self -> logger -> add
		(
			screen =>
			{
				maxlevel       => $self -> maxlevel,
				message_layout => '%m',
				minlevel       => $self -> minlevel,
			}
		);
	}

	# Policy: Event names are always the same as the name of the corresponding lexeme.

	$self -> bnf
	(
<<'END_OF_GRAMMAR'

:default				::= action => [values]

lexeme default			= latm => 1

:start					::= input_text

input_text				::= input_string*

input_string			::= quoted_text
							| unquoted_text

quoted_text				::= open_delim input_text close_delim

unquoted_text			::= string

# Lexemes in alphabetical order.

bracket_char			~ [%:<>{}\[\]()"]	# Use " in comment for UltraEdit.

:lexeme					~ close_delim		pause => before		event => close_delim
close_delim				~ '%]'
close_delim				~ ':>'
close_delim				~ [>]
close_delim				~ [}]
close_delim				~ [\]]
close_delim				~ [)]
close_delim				~ ["]				# Use " in comment for UltraEdit.

escaped_char			~ '\' bracket_char	# Use ' in comment for UltraEdit.

# Warning: Do not add '+' to this set, even though it speeds up things.
# The problem is that the set then gobbles up any '\', so the following
# character is no longer recognized as being escaped.
# Trapping the exception then generated would be possible.

non_quote_char			~ [^%:<>{}\[\]()"]	# Use " in comment for UltraEdit.

:lexeme					~ open_delim		pause => before		event => open_delim
open_delim				~ '[%'
open_delim				~ '<:'
open_delim				~ [<]
open_delim				~ [{]
open_delim				~ [\[]
open_delim				~ [(]
open_delim				~ ["]				# Use " in comment for UltraEdit.

:lexeme					~ string			pause => before		event => string
string					~ escaped_char
							| non_quote_char

END_OF_GRAMMAR
	);

	$self -> grammar
	(
		Marpa::R2::Scanless::G -> new
		({
			source => \$self -> bnf
		})
	);

	my(%event);

	for my $line (split(/\n/, $self -> bnf) )
	{
		$event{$1} = 1 if ($line =~ /event\s+=>\s+(\w+)/);
	}

	$self -> known_events(\%event);

} # End of BUILD.

# ------------------------------------------------

sub _add_daughter
{
	my($self, $name, $attributes)  = @_;
	$attributes ||= {};
	my($node)   = Tree::DAG_Node -> new({name => $name, attributes => $attributes});
	my($stack)  = $self -> node_stack;

	$$stack[$#$stack] -> add_daughter($node);

} # End of _add_daughter.

# ------------------------------------------------

sub _decode_result
{
	my($self, $result) = @_;
	my(@worklist)      = $result;

	my($obj);
	my($ref_type);
	my(@stack);

	do
	{
		$obj      = shift @worklist;
		$ref_type = ref $obj;

		if ($ref_type eq 'ARRAY')
		{
			unshift @worklist, @$obj;
		}
		elsif ($ref_type eq 'HASH')
		{
			push @stack, {%$obj};
		}
		elsif ($ref_type)
		{
			die "Unsupported object type $ref_type\n";
		}
		else
		{
			push @stack, $obj;
		}

	} while (@worklist);

	return join('', @stack);

} # End of _decode_result.

# ------------------------------------------------

sub log
{
	my($self, $level, $s) = @_;

	$self -> logger -> log($level => $s) if ($self -> logger);

} # End of log.

# ------------------------------------------------

sub next_few_chars
{
	my($self, $s, $offset) = @_;
	$s = substr($s, $offset, 20);
	$s =~ tr/\n/ /;
	$s =~ s/^\s+//;
	$s =~ s/\s+$//;

	return $s;

} # End of next_few_chars.

# ------------------------------------------------

sub parse
{
	my($self) = @_;

	$self -> recce
	(
		Marpa::R2::Scanless::R -> new
		({
			grammar          => $self -> grammar,
			ranking_method   => 'high_rule_only',
			#trace_terminals => $self -> trace_terminals,
		})
	);

	# Since $self -> node_stack has not been initialized yet,
	# we can't call _add_daughter() until after this statement.

	$self -> tree(Tree::DAG_Node -> new({name => 'root', attributes => {} }));
	$self -> node_stack([$self -> tree -> root]);

	# Return 0 for success and 1 for failure.

	my($result) = 0;

	try
	{
		if (defined (my $value = $self -> _process) )
		{
			$self -> log(info => 'Parsed text:');
			$self -> log(info => join("\n", @{$self -> tree -> tree2string}) );
		}
		else
		{
			$result = 1;

			$self -> log(error => 'Parse failed');
			$self -> log(info => 'Parsed text:');
			$self -> log(info => join("\n", @{$self -> tree -> tree2string}) );
		}
	}
	catch
	{
		$result = 1;

		$self -> log(error => "Parse failed. Error: $_");
		$self -> log(info => 'Parsed text:');
		$self -> log(info => join("\n", @{$self -> tree -> tree2string}) );
	};

	$self -> log(info => "Parse result:  $result (0 is success)");

	# Return 0 for success and 1 for failure.

	return $result;

} # End of parse.

# ------------------------------------------------

sub _pop_node_stack
{
	my($self)  = @_;
	my($stack) = $self -> node_stack;

	pop @$stack;

	$self -> node_stack($stack);

} # End of _pop_node_stack.

# ------------------------------------------------

sub _process
{
	my($self)       = @_;
	my($string)     = $self -> text || ''; # Allow for undef.
	my($length)     = length $string;
	my($text)       = '';
	my($format)     = '%-20s    %5s    %5s    %5s    %-20s    %-20s';
	my($last_event) = '';
	my($pos)        = 0;

	$self -> log(debug => "Length of input: $length. Input |$string|");
	$self -> log(debug => sprintf($format, 'Event', 'Start', 'Span', 'Pos', 'Lexeme', 'Comment') );

	my($event_name);
	my(@fields);
	my($lexeme);
	my($node_name);
	my($original_lexeme);
	my($span, $start, $s, $stack, $stats);
	my($temp, $type);

	# We use read()/lexeme_read()/resume() because we pause at each lexeme.
	# Also, in read(), we use $pos and $length to avoid reading Ruby Slippers tokens (if any).

	for
	(
		$pos = $self -> recce -> read(\$string, $pos, $length);
		$pos < $length;
		$pos = $self -> recce -> resume($pos)
	)
	{
		($start, $span)            = $self -> recce -> pause_span;
		($event_name, $span, $pos) = $self -> _validate_event($string, $start, $span, $pos);
		$lexeme                    = $self -> recce -> literal($start, $span);
		$original_lexeme           = $lexeme;
		$pos                       = $self -> recce -> lexeme_read($event_name);
		#$stats                     = $self -> stats;

		die "lexeme_read($event_name) rejected lexeme |$lexeme|\n" if (! defined $pos);

		$self -> log(debug => sprintf($format, $event_name, $start, $span, $pos, $lexeme, '-') );

		$self -> quote_count($self -> quote_count + 1) if ($lexeme eq '"');

		if ($event_name ne 'string')
		{
			$self -> _save_text($text);

			$text = '';
		}

		if ($event_name eq 'close_delim')
		{
			$self -> _pop_node_stack;
			$self -> _add_daughter('close', {text => $lexeme});

			# We could keep stats on delimiter frequency to diagnose errors.

			#$$stats{$lexeme}++;

			#$self -> stats($stats);
		}
		elsif ($event_name eq 'open_delim')
		{
			$self -> _add_daughter('open', {text => $lexeme});
			$self -> _push_node_stack;

			#$$stats{$lexeme}++;

			#$self -> stats($stats);
		}
		elsif ($event_name eq 'string')
		{
			$text .= $lexeme;
		}

		$last_event = $event_name;
    }

	# Mop up any left-over chars.

	$self -> _save_text($text);

	if (my $ambiguous_status = $self -> recce -> ambiguous)
	{
		my($terminals) = $self -> recce -> terminals_expected;
		$terminals     = ['(None)'] if ($#$terminals < 0);

		$self -> log(info => 'Terminals expected: ' . join(', ', @$terminals) );
		$self -> log(info => "Parse is ambiguous. Status: $ambiguous_status");
	}

	# Return a defined value for success and undef for failure.

	return $self -> recce -> value;

} # End of _process.

# ------------------------------------------------

sub _push_node_stack
{
	my($self)      = @_;
	my($stack)     = $self -> node_stack;
	my(@daughters) = $$stack[$#$stack] -> daughters;

	push @$stack, $daughters[$#daughters];

} # End of _push_node_stack.

# ------------------------------------------------

sub _save_text
{
	my($self, $text) = @_;

	$self -> _add_daughter('string', {text => $text}) if (length($text) );

	return '';

} # End of _save_text.

# ------------------------------------------------

sub _validate_event
{
	my($self, $string, $start, $span, $pos) = @_;
	my(@event)         = @{$self -> recce -> events};
	my($event_count)   = scalar @event;
	my(@event_name)    = sort map{$$_[0]} @event;
	my($event_name)    = $event_name[0]; # Default.
	my($lexeme)        = substr($string, $start, $span);
	my($line, $column) = $self -> recce -> line_column($start);
	my($literal)       = $self -> next_few_chars($string, $start + $span);
	my($message)       = "Location: ($line, $column). Lexeme: |$lexeme|. Next few chars: |$literal|";
	$message           = "$message. Events: $event_count. Names: ";

	$self -> log(debug => $message . join(', ', @event_name) . '.');

	my(%event_name);

	@event_name{@event_name} = (1) x @event_name;

	for (@event_name)
	{
		die "Unexpected event name '$_'" if (! ${$self -> known_events}{$_});
	}

	if ($event_count > 1)
	{
		my(%special_case) =
		(
			'>'        => 'close',
			'}'        => 'close',
			']'        => 'close',
			')'        => 'close',
			'<'        => 'open',
			'{'        => 'open',
			'['        => 'open',
			')'        => 'open',
		);

		if ($event_name{string})
		{
			$event_name = $special_case{$lexeme};

			$self -> log(debug => "Disambiguated lexeme |$lexeme| as '$event_name'");
		}
		elsif ( ($lexeme eq '"') && (join(', ', @event_name) eq 'close_delim, open_delim') )
		{
			# At the time _validate_event() is called, the quote count has not yet been bumped.
			# If this is the 1st quote, then it's an open_delim.
			# If this is the 2nd quote, them it's a close delim.

			if ($self -> quote_count % 2 == 0)
			{
				$event_name = 'open_delim';

				$self -> log(debug => "Disambiguated lexeme |$lexeme| as '$event_name'");
			}
			else
			{
				$event_name = 'close_delim';

				$self -> log(debug => "Disambiguated lexeme |$lexeme| as '$event_name'");
			}
		}
		else
		{
			die "The code only handles 1 event at a time, or ('string', 'and others'). \n";
		}
	}

	return ($event_name, $span, $pos);

} # End of _validate_event.

# ------------------------------------------------

1;

=pod

=head1 NAME

C<Text::Balanced::Marpa> - Extract delimited text sequences from strings

=head1 Synopsis

	perl -Ilib scripts/samples.pl

The L</FAQ> discusses the way the parsed data is stored in RAM.

=head1 Description

L<Text::Balanced::Marpa> provides a L<Marpa::R2>-based parser for extracting delimited text
sequences from strings.

L<Marpa's homepage|http://savage.net.au/Marpa.html>.

=head1 Distributions

This module is available as a Unix-style distro (*.tgz).

See L<http://savage.net.au/Perl-modules/html/installing-a-module.html>
for help on unpacking and installing distros.

=head1 Installation

Install L<Text::Balanced::Marpa> as you would for any C<Perl> module:

Run:

	cpanm Text::Balanced::Marpa

or run:

	sudo cpan Text::Balanced::Marpa

or unpack the distro, and then either:

	perl Build.PL
	./Build
	./Build test
	sudo ./Build install

or:

	perl Makefile.PL
	make (or dmake or nmake)
	make test
	make install

=head1 Constructor and Initialization

C<new()> is called as C<< my($parser) = Text::Balanced::Marpa -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<Text::Balanced::Marpa>.

Key-value pairs accepted in the parameter list (see corresponding methods for details
[e.g. L<open([$delimiter_list])>]):

=over 4

=item o logger => $aLoggerObject

Specify a logger compatible with L<Log::Handler>, for the lexer and parser to use.

Default: A logger of type L<Log::Handler> which writes to the screen.

To disable logging, just set 'logger' to the empty string (not undef).

=item o maxlevel => $logOption1

This option affects L<Log::Handler>.

Typical values are 'notice' (the default), 'info' and 'debug'.

See the L<Log::Handler::Levels> docs.

Default: 'notice'.

=item o minlevel => $logOption2

This option affects L<Log::Handler>.

See the L<Log::Handler::Levels> docs.

Default: 'error'.

No lower levels are used.

=item o text => $the_string_to_be_parsed

Default: ''.

=back

=head1 Methods

=head2 log($level, $s)

If a logger is defined, this logs the message $s at level $level.

=head2 logger([$logger_object])

Here, the [] indicate an optional parameter.

Get or set the logger object.

To disable logging, just set 'logger' to the empty string (not undef), in the call to L</new()>.

This logger is passed to other modules.

'logger' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 maxlevel([$string])

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if an object of type L<Log::Handler> is created.
See L<Log::Handler::Levels>.

'maxlevel' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 minlevel([$string])

Here, the [] indicate an optional parameter.

Get or set the value used by the logger object.

This option is only used if an object of type L<Log::Handler> is created.
See L<Log::Handler::Levels>.

'minlevel' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 new()

See L</Constructor and Initialization> for details on the parameters accepted by L</new()>.

=head2 next_few_chars($s, $offset)

Returns a substring of $s, starting at $offset, for use in progress messages.

The default string length returned is 20 characters.

=head2 parse()

This is the only method the caller needs to call. All parameters are supplied to L</new()>
(or via other methods before C<parse()> is called).

See scripts/samples.pl.

Returns 0 for success and 1 for failure.

=head2 text([$string])

Here, the [] indicate an optional parameter.

Get or set the string to be parsed.

'text' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=item o tree()

Get the tree, managed by L<Tree::DAG_Node>, which holds the parsed data.

Obviously, it only makes sense to call C<tree()> after calling C<parse()>.

=head1 FAQ

=head2 How is the parsed data held in RAM?

The parsed output is held in a tree managed by L<Tree::DAG_Node>.

The tree always has a root node, which has nothing to do with the input data. So, even an empty
imput string will produce a tree with 1 node.

Nodes have a name and a hashref of attributes.

Note: The root of the tree has an empty hashref associated with it.

The name indicates the type of node. Names are one of these literals:

=over 4

=item o close

=item o open

=item o root

=item o string

=back

For 'open' and 'close', the delimiter is given by the value of the 'text' key in the hashref.

The (key => value) pairs in the hashref are:

=over 4

=item o text => $string

If the node name is 'open' and 'close', $string is the delimiter.

If the node name is 'string', $string is the text from the document.

=back

Try:

	perl -Ilib scripts/samples.pl info

=head2 How is HTML/XML handled?

The tree does not preserve the nested nature of HTML/XML.

Post-processing (valid) HTML could easily generate another view of the data.

=head2 What is the homepage of Marpa?

L<http://savage.net.au/Marpa.html>.

That page has a long list of links.

=head2 Does this package support Unicode?

Yes.

=head2 How do I run author tests?

This runs both standard and author tests:

	shell> perl Build.PL; ./Build; ./Build test; ./Build authortest

=head1 TODO

=over 4

=item o Error reporting

See L<https://jeffreykegler.github.io/Ocean-of-Awareness-blog/individual/2014/11/delimiter.html>.

=item o Parameters to new() to specify delimiters

=item o Extensible grammar based on user-supplied delimiters

=item o UTF8 data in samples.pl

=item o Implement some other delimiters

=over 4

=item o Double quotes

=item o Single quotes

=item o Asymmetric quotes

E.g.: '<:' and ':>' as used in L<Text::Xslate> and elsewhere.

=item o Perl's 'q' and 'qq' operators

E.g.: 'q|' and '|'.

=item o Back-ticks

=item o Hashes

=item o Slashes

=back

=back

=head1 See Also

L<Text::Balanced>.

=head1 Machine-Readable Change Log

The file CHANGES was converted into Changelog.ini by L<Module::Metadata::Changes>.

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

=head1 Thanks

Thanks to Jeffrey Kegler, who wrote Marpa and L<Marpa::R2>.

And thanks to rns (Ruslan Shvedov) for writing the grammar for double-quoted strings used in
L<MarpaX::Demo::SampleScripts>'s scripts/quoted.strings.02.pl. I adapted it to HTML (see
scripts/quoted.strings.05.pl in that module), and then incorporated the grammar into
L<GraphViz2::Marpa>, and into this module.

=head1 Repository

L<https://github.com/ronsavage/Text-Balanced-Marpa>

=head1 Support

Email the author, or log a bug on RT:

L<https://rt.cpan.org/Public/Dist/Display.html?Name=Text::Balanced::Marpa>.

=head1 Author

L<Text::Balanced::Marpa> was written by Ron Savage I<E<lt>ron@savage.net.auE<gt>> in 2014.

Marpa's homepage: <http://savage.net.au/Marpa.html>.

My homepage: L<http://savage.net.au/>.

=head1 Copyright

Australian copyright (c) 2014, Ron Savage.

	All Programs of mine are 'OSI Certified Open Source Software';
	you can redistribute them and/or modify them under the terms of
	The Artistic License 2.0, a copy of which is available at:
	http://opensource.org/licenses/alphabetical.

=cut
