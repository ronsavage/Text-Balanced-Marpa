package Text::Balanced::Marpa;

use strict;
use utf8;
use warnings;
use warnings qw(FATAL utf8); # Fatalize encoding glitches.
use open     qw(:std :utf8); # Undeclared streams in UTF-8.

use Const::Exporter constants =>
[
	nothing_is_fatal    =>  0, # The default.
	debug               =>  1,
	print_warnings      =>  2,
	overlap_is_fatal    =>  4,
	nesting_is_fatal    =>  8,
	ambiguity_is_fatal  => 16,
	exhaustion_is_fatal => 32,
];

use Marpa::R2;

use Moo;

use Tree;

use Types::Standard qw/Any ArrayRef HashRef Int ScalarRef Str/;

use Try::Tiny;

has bnf =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has close =>
(
	default  => sub{return []},
	is       => 'rw',
	isa      => ArrayRef,
	required => 0,
);

has delimiter_action =>
(
	default  => sub{return {} },
	is       => 'rw',
	isa      => HashRef,
	required => 0,
);

has delimiter_stack =>
(
	default  => sub{return []},
	is       => 'rw',
	isa      => ArrayRef,
	required => 0,
);

has delimiter_frequency =>
(
	default  => sub{return {} },
	is       => 'rw',
	isa      => HashRef,
	required => 0,
);

has error_message =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has error_number =>
(
	default  => sub{return 0},
	is       => 'rw',
	isa      => Int,
	required => 0,
);

has escape_char =>
(
	default  => sub{return '\\'},
	is       => 'rw',
	isa      => Str,
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

has length =>
(
	default  => sub{return 0},
	is       => 'rw',
	isa      => Int,
	required => 0,
);

has matching_delimiter =>
(
	default  => sub{return {} },
	is       => 'rw',
	isa      => HashRef,
	required => 0,
);

has next_few_limit =>
(
	default  => sub{return 20},
	is       => 'rw',
	isa      => Int,
	required => 0,
);

has node_stack =>
(
	default  => sub{return []},
	is       => 'rw',
	isa      => ArrayRef,
	required => 0,
);

has open =>
(
	default  => sub{return []},
	is       => 'rw',
	isa      => ArrayRef,
	required => 0,
);

has options =>
(
	default  => sub{return 0},
	is       => 'rw',
	isa      => Int,
	required => 0,
);

has pos =>
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

has tree =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Any,
	required => 0,
);

has text =>
(
	default  => sub{return \''},	# Use ' in comment for UltraEdit.
	is       => 'rw',
	isa      => ScalarRef[Str],
	required => 0,
);

has uid =>
(
	default  => sub{return 0},
	is       => 'rw',
	isa      => Int,
	required => 0,
);

our $VERSION = '1.04';

# ------------------------------------------------

sub BUILD
{
	my($self) = @_;

	# Policy: Event names are always the same as the name of the corresponding lexeme.
	#
	# Note:   Tokens of the form '_xxx_' are replaced just below, with values returned
	#			by the call to validate_open_close().

	my($bnf) = <<'END_OF_GRAMMAR';

:default				::= action => [values]

lexeme default			= latm => 1

:start					::= input_text

input_text				::= input_string*

input_string			::= quoted_text
							| unquoted_text

quoted_text				::= open_delim input_text close_delim

unquoted_text			::= text

# Lexemes in alphabetical order.

delimiter_char			~ [_delimiter_]

:lexeme					~ close_delim		pause => before		event => close_delim
_close_

escaped_char			~ '_escape_char_' delimiter_char	# Use ' in comment for UltraEdit.

# Warning: Do not add '+' to this set, even though it speeds up things.
# The problem is that the set then gobbles up any '\', so the following
# character is no longer recognized as being escaped.
# Trapping the exception then generated would be possible.

non_quote_char			~ [^_delimiter_]	# Use " in comment for UltraEdit.

:lexeme					~ open_delim		pause => before		event => open_delim
_open_

:lexeme					~ text				pause => before		event => text
text					~ escaped_char
							| non_quote_char
END_OF_GRAMMAR

	my($hashref)     = $self -> _validate_open_close;
	$bnf             =~ s/_open_/$$hashref{open}/;
	$bnf             =~ s/_close_/$$hashref{close}/;
	$bnf             =~ s/_delimiter_/$$hashref{delim}/g;
	my($escape_char) = $self -> escape_char;

	if ($escape_char eq "'")
	{
		my($message) = 'Single-quote is forbidden as an escape character';

		$self -> error_message($message);
		$self -> error_number(7);

		# This 'die' is not inside try {}catch{}, so we add the prefix 'Error: '.

		die "Error: $message\n";
	}

	$bnf =~ s/_escape_char_/$escape_char/g;

	$self -> bnf($bnf);
	$self -> grammar
	(
		Marpa::R2::Scanless::G -> new
		({
			source => \$self -> bnf
		})
	);

	# This hash does not contain the key "'exhausted" because the exhaustion
	# event is everywhere handled explicitly. Yes, it has a leading quote.

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
	my($self, $name, $text)  = @_;
	my($attributes) = {text => $text, uid => $self -> uid($self -> uid + 1)};
	my($stack)      = $self -> node_stack;
	my($node)       = Tree -> new($name);

	$node -> meta($attributes);

	$$stack[$#$stack] -> add_child({}, $node);

} # End of _add_daughter.

# -----------------------------------------------

sub format_node
{
	my($self, $options, $node) = @_;
	my($s) = $node -> value;
	$s     .= '. Attributes: ' . $self -> hashref2string($node -> meta) if (! $$options{no_attributes});

	return $s;

} # End of format_node.

# -----------------------------------------------

sub hashref2string
{
	my($self, $hashref) = @_;
	$hashref ||= {};

	return '{' . join(', ', map{qq|$_ => "$$hashref{$_}"|} sort keys %$hashref) . '}';

} # End of hashref2string.

# ------------------------------------------------

sub next_few_chars
{
	my($self, $stringref, $offset) = @_;
	my($s) = substr($$stringref, $offset, $self -> next_few_limit);
	$s     =~ tr/\n/ /;
	$s     =~ s/^\s+//;
	$s     =~ s/\s+$//;

	return $s;

} # End of next_few_chars.

# -----------------------------------------------

sub node2string
{
	my($self, $options, $is_last_node, $node, $vert_dashes) = @_;
	my($depth)         = $node -> depth;
	my($sibling_count) = defined $node -> is_root ? 1 : scalar $node -> parent -> children;
	my($offset)        = ' ' x 4;
	my(@indent)        = map{$$vert_dashes[$_] || $offset} 0 .. $depth - 1;
	@$vert_dashes      =
	(
		@indent,
		($sibling_count == 0 ? $offset : '   |'),
	);

	$indent[1] = '    ' if ($is_last_node && ($depth > 1) );

	return join('', @indent[1 .. $#indent]) . ($depth ? '   |--- ' : '') . $self -> format_node($options, $node);

} # End of node2string.

# ------------------------------------------------

sub parse
{
	my($self, $stringref) = @_;
	$self -> text($stringref) if (defined $stringref);

	$self -> recce
	(
		Marpa::R2::Scanless::R -> new
		({
			exhaustion     => 'event',
			grammar        => $self -> grammar,
			ranking_method => 'high_rule_only',
		})
	);

	# Since $self -> node_stack has not been initialized yet,
	# we can't call _add_daughter() until after this statement.

	$self -> uid(0);
	$self -> tree(Tree -> new('root') );
	$self -> tree -> meta({text => '', uid => $self -> uid});
	$self -> node_stack([$self -> tree -> root]);

	# Return 0 for success and 1 for failure.

	my($result) = 0;

	my($message);

	try
	{
		if (defined (my $value = $self -> _process) )
		{
		}
		else
		{
			$result = 1;

			print "Error: Parse failed\n";
		}
	}
	catch
	{
		$result = 1;

		print "Error: Parse failed. ${_}";
	};

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
	my($self)               = @_;
	my($stringref)          = $self -> text || \''; # Allow for undef. Use ' in comment for UltraEdit.
	my($pos)                = $self -> pos;
	my($first_pos)          = $pos;
	my($total_length)       = length($$stringref);
	my($length)             = $self -> length || $total_length;
	my($text)               = '';
	my($format)             = "%-20s    %5s    %5s    %5s    %-20s    %-20s\n";
	my($last_event)         = '';
	my($matching_delimiter) = $self -> matching_delimiter;

	if ($self -> options & debug)
	{
		print "Length of input: $length. Input |$$stringref|\n";
		print sprintf($format, 'Event', 'Start', 'Span', 'Pos', 'Lexeme', 'Comment');
	}

	my($delimiter_frequency, $delimiter_stack);
	my($event_name);
	my($lexeme);
	my($message);
	my($original_lexeme);
	my($span, $start);
	my($tos);

	# We use read()/lexeme_read()/resume() because we pause at each lexeme.
	# Also, in read(), we use $pos and $length to avoid reading Ruby Slippers tokens (if any).
	# For the latter, see scripts/match.parentheses.02.pl in MarpaX::Demo::SampleScripts.

	for
	(
		$pos = $self -> recce -> read($stringref, $pos, $length);
		($pos < $total_length) && ( ($pos - $first_pos) <= $length);
		$pos = $self -> recce -> resume($pos)
	)
	{
		$delimiter_frequency       = $self -> delimiter_frequency;
		$delimiter_stack           = $self -> delimiter_stack;
		($start, $span)            = $self -> recce -> pause_span;
		($event_name, $span, $pos) = $self -> _validate_event($stringref, $start, $span, $pos, $delimiter_frequency);

		# If the input is exhausted, we exit immediately so we don't try to use
		# the values of $start, $span or $pos. They are ignored upon exit.

		last if ($event_name eq "'exhausted"); # Yes, it has a leading quote.

		$lexeme          = $self -> recce -> literal($start, $span);
		$original_lexeme = $lexeme;
		$pos             = $self -> recce -> lexeme_read($event_name);

		die "lexeme_read($event_name) rejected lexeme |$lexeme|\n" if (! defined $pos);

		print sprintf($format, $event_name, $start, $span, $pos, $lexeme, '-') if ($self -> options & debug);

		if ($event_name ne 'text')
		{
			$self -> _save_text($text);

			$text = '';
		}

		if ($event_name eq 'close_delim')
		{
			$$delimiter_frequency{$lexeme}--;

			$self -> delimiter_frequency($delimiter_frequency);

			$tos = pop @$delimiter_stack;

			$self -> delimiter_stack($delimiter_stack);

			# If the top of the delimiter stack is not the lexeme corresponding to the
			# opening delimiter of the current closing delimiter, then there's an error.

			if ($$matching_delimiter{$$tos{lexeme} } ne $lexeme)
			{
				$message = "Last open delimiter: $$tos{lexeme}. Unexpected closing delimiter: $lexeme";

				$self -> error_message($message);
				$self -> error_number(1);

				# This 'die' is inside try{}catch{}, which adds the prefix 'Error: '.

				die "$message\n" if ($self -> options & overlap_is_fatal);

				# If we did not die, then it's a warning message.

				$self -> error_number(-1);

				print "Warning: $message\n" if ($self -> options & print_warnings);
			}

			$self -> _pop_node_stack;
			$self -> _add_daughter('close', $lexeme);
		}
		elsif ($event_name eq 'open_delim')
		{
			$$delimiter_frequency{$$matching_delimiter{$lexeme} }++;

			# If the top of the delimiter stack reaches 2, then there's an error.
			# Unlink mismatched delimiters (just above), this is never gets a warning.

			if ($$delimiter_frequency{$$matching_delimiter{$lexeme} } > 1)
			{
				$message = "Opened delimiter $lexeme again before closing previous one";

				$self -> error_message($message);
				$self -> error_number(2);

				# This 'die' is inside try {}catch{}, which adds the prefix 'Error: '.

				die "$message\n" if ($self -> options & nesting_is_fatal);

				# If we did not die, then it's a warning message.

				$self -> error_number(-2);

				print "Warning: $message\n" if ($self -> options & print_warnings);
			}

			$self -> delimiter_frequency($delimiter_frequency);

			push @$delimiter_stack,
				{
					count  => $$delimiter_frequency{$$matching_delimiter{$lexeme} },
					lexeme => $lexeme,
				};

			$self -> delimiter_stack($delimiter_stack);

			$self -> _add_daughter('open', $lexeme);
			$self -> _push_node_stack;
		}
		elsif ($event_name eq 'text')
		{
			$text .= $lexeme;
		}

		$last_event = $event_name;
    }

	# Mop up any left-over chars.

	$self -> _save_text($text);

	if ($self -> recce -> exhausted)
	{
		$message = 'Parse exhausted';

		$self -> error_message($message);
		$self -> error_number(6);

		if ($self -> options & exhaustion_is_fatal)
		{
			# This 'die' is inside try {}catch{}, which adds the prefix 'Error: '.

			die "$message\n";
		}
		else
		{
			$self -> error_number(-6);

			print "Warning: $message\n" if ($self -> options & print_warnings);
		}
	}
	elsif (my $status = $self -> recce -> ambiguous)
	{
		my($terminals) = $self -> recce -> terminals_expected;
		$terminals     = ['(None)'] if ($#$terminals < 0);
		$message       = "Ambiguous parse. Status: $status. Terminals expected: " . join(', ', @$terminals);

		$self -> error_message($message);
		$self -> error_number(3);

		if ($self -> options & ambiguity_is_fatal)
		{
			# This 'die' is inside try {}catch{}, which adds the prefix 'Error: '.

			die "$message\n";
		}
		elsif ($self -> options & print_warnings)
		{
			$self -> error_number(-3);

			print "Warning: $message\n";
		}
	}

	# Return a defined value for success and undef for failure.

	return $self -> recce -> value;

} # End of _process.

# ------------------------------------------------

sub _push_node_stack
{
	my($self)      = @_;
	my($stack)     = $self -> node_stack;
	my(@daughters) = $$stack[$#$stack] -> children;

	push @$stack, $daughters[$#daughters];

} # End of _push_node_stack.

# ------------------------------------------------

sub _save_text
{
	my($self, $text) = @_;

	$self -> _add_daughter('text', $text) if (length($text) );

	return '';

} # End of _save_text.

# -----------------------------------------------

sub tree2string
{
	my($self, $options, $tree) = @_;
	$options                   ||= {};
	$$options{no_attributes}   ||= 0;
	$tree                      ||= $self -> tree;
	my(@nodes)                 = $tree -> traverse;

	my(@out);
	my(@vert_dashes);

	for my $i (0 .. $#nodes)
	{
		push @out, $self -> node2string($options, $i == $#nodes, $nodes[$i], \@vert_dashes);
	}

	return [@out];

} # End of tree2string.

# ------------------------------------------------

sub _validate_event
{
	my($self, $stringref, $start, $span, $pos, $delimiter_frequency) = @_;
	my(@event)       = @{$self -> recce -> events};
	my($event_count) = scalar @event;
	my(@event_name)  = sort map{$$_[0]} @event;
	my($event_name)  = $event_name[0]; # Default.

	# If the input is exhausted, we return immediately so we don't try to use
	# the values of $start, $span or $pos. They are ignored upon return.

	if ($event_name eq "'exhausted") # Yes, it has a leading quote.
	{
		return ($event_name, $span, $pos);
	}

	my($lexeme)        = substr($$stringref, $start, $span);
	my($line, $column) = $self -> recce -> line_column($start);
	my($literal)       = $self -> next_few_chars($stringref, $start + $span);
	my($message)       = "Location: ($line, $column). Lexeme: |$lexeme|. Next few chars: |$literal|";
	$message           = "$message. Events: $event_count. Names: ";

	print $message, join(', ', @event_name), "\n" if ($self -> options & debug);

	my(%event_name);

	@event_name{@event_name} = (1) x @event_name;

	for (@event_name)
	{
		if (! ${$self -> known_events}{$_})
		{
			$message = "Unexpected event name '$_'";

			$self -> error_message($message);
			$self -> error_number(10);

			# This 'die' is inside try {}catch{}, which adds the prefix 'Error: '.

			die "$message\n";
		}
	}

	if ($event_count > 1)
	{
		my($delimiter_action) = $self -> delimiter_action;

		if (defined $event_name{string})
		{
			$event_name = $$delimiter_action{$lexeme};

			print "Disambiguated lexeme |$lexeme| as '$event_name'\n" if ($self -> options & debug);
		}
		elsif ( ($lexeme =~ /["']/) && (join(', ', @event_name) eq 'close_delim, open_delim') ) # ".
		{
			# At the time _validate_event() is called, the quote count has not yet been bumped.
			# If this is the 1st quote, then it's an open_delim.
			# If this is the 2nd quote, them it's a close delim.

			if ($$delimiter_frequency{$lexeme} % 2 == 0)
			{
				$event_name = 'open_delim';

				print "Disambiguated lexeme |$lexeme| as '$event_name'\n" if ($self -> options & debug);
			}
			else
			{
				$event_name = 'close_delim';

				print "Disambiguated lexeme |$lexeme| as '$event_name'\n" if ($self -> options & debug);
			}
		}
		else
		{
			$message = join(', ', @event_name);
			$message = "The code does not handle these events simultaneously: $message";

			$self -> error_message($message);
			$self -> error_number(11);

			# This 'die' is inside try {}catch{}, which adds the prefix 'Error: '.

			die "$message\n";
		}
	}

	return ($event_name, $span, $pos);

} # End of _validate_event.

# ------------------------------------------------

sub _validate_open_close
{
	my($self)  = @_;
	my($open)  = $self -> open;
	my($close) = $self -> close;

	my($message);

	if ( ($#$open < 0) || ($#$close < 0) )
	{
		$message = 'There must be at least 1 pair of open/close delimiters';

		$self -> error_message($message);
		$self -> error_number(8);

		# This 'die' is not inside try {}catch{}, so we add the prefix 'Error: '.

		die "Error: $message\n";
	}

	if ($#$open != $#$close)
	{
		$message = 'The # of open delimiters must match the # of close delimiters';

		$self -> error_message($message);
		$self -> error_number(9);

		# This 'die' is not inside try {}catch{}, so we add the prefix 'Error: '.

		die "Error: $message\n";
	}

	my(%substitute)         = (close => '', delim => '', open => '');
	my($matching_delimiter) = {};
	my(%seen)               = (close => {}, open => {});

	my($close_quote);
	my(%delimiter_action, %delimiter_frequency);
	my($open_quote);
	my($prefix, %prefix);

	for my $i (0 .. $#$open)
	{
		if ( ($$open[$i] =~ /\\/) || ($$close[$i] =~ /\\/) )
		{
			$message = 'Backslash is forbidden as a delimiter character';

			$self -> error_message($message);
			$self -> error_number(4);

			# This 'die' is not inside try {}catch{}, so we add the prefix 'Error: '.

			die "Error: $message\n";
		}

		if ( ( (length($$open[$i]) > 1) && ($$open[$i] =~ /'/) ) || ( (length($$close[$i]) > 1) && ($$close[$i] =~ /'/) ) )
		{
			$message = 'Single-quotes are forbidden in multi-character delimiters';

			$self -> error_message($message);
			$self -> error_number(5);

			# This 'die' is not inside try {}catch{}, so we add the prefix 'Error: '.

			die "Error: $message\n";
		}

		$seen{open}{$$open[$i]}   = 0 if (! $seen{open}{$$open[$i]});
		$seen{close}{$$close[$i]} = 0 if (! $seen{close}{$$close[$i]});

		$seen{open}{$$open[$i]}++;
		$seen{close}{$$close[$i]}++;

		$delimiter_action{$$open[$i]}     = 'open';
		$delimiter_action{$$close[$i]}    = 'close';
		$$matching_delimiter{$$open[$i]}  = $$close[$i];
		$delimiter_frequency{$$open[$i]}  = 0;
		$delimiter_frequency{$$close[$i]} = 0;

		if (length($$open[$i]) == 1)
		{
			$open_quote = $$open[$i] eq '[' ? "[\\$$open[$i]]" : "[$$open[$i]]";
		}
		else
		{
			# This fails if length > 1 and open contains a single quote.

			$open_quote = "'$$open[$i]'";
		}

		if (length($$close[$i]) == 1)
		{
			$close_quote = $$close[$i] eq ']' ? "[\\$$close[$i]]" : "[$$close[$i]]";
		}
		else
		{
			# This fails if length > 1 and close contains a single quote.

			$close_quote = "'$$close[$i]'";
		}

		$substitute{open}  .= "open_delim\t\t\t\~ $open_quote\n"   if ($seen{open}{$$open[$i]} <= 1);
		$substitute{close} .= "close_delim\t\t\t\~ $close_quote\n" if ($seen{close}{$$close[$i]} <= 1);
		$prefix            = substr($$open[$i], 0, 1);
		$prefix            = "\\$prefix" if ($prefix =~ /[\[\]]/);
		$prefix{$prefix}   = 0 if (! $prefix{$prefix});

		$prefix{$prefix}++;

		$substitute{delim} .= $prefix if ($prefix{$prefix} == 1);
		$prefix            = substr($$close[$i], 0, 1);
		$prefix            = "\\$prefix" if ($prefix =~ /[\[\]]/);
		$prefix{$prefix}   = 0 if (! $prefix{$prefix});

		$prefix{$prefix}++;

		$substitute{delim} .= $prefix if ($prefix{$prefix} == 1);
	}

	$self -> delimiter_action(\%delimiter_action);
	$self -> delimiter_frequency(\%delimiter_frequency);
	$self -> matching_delimiter($matching_delimiter);

	return \%substitute;

} # End of _validate_open_close.

# ------------------------------------------------

1;

=pod

=head1 NAME

C<Text::Balanced::Marpa> - Extract delimited text sequences from strings

=head1 Synopsis

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

		$result = $parser -> parse(\$text);

		print join("\n", @{$parser -> tree2string}), "\n";
		print "Parse result: $result (0 is success)\n";

		if ($count == 3)
		{
			print "Deliberate error: Failed to parse |$text|\n";
			print 'Error number: ', $parser -> error_number, '. Error message: ',
					$parser -> error_message, "\n";
		}

		print '-' x 50, "\n";
	}

See scripts/synopsis.pl.

This is the printout of synopsis.pl:

	Parsing |<: a :>|
	Parsed text:
	root. Attributes: {}
	   |--- open. Attributes: {text => "<:"}
	   |   |--- string. Attributes: {text => " a "}
	   |--- close. Attributes: {text => ":>"}
	Parse result: 0 (0 is success)
	--------------------------------------------------
	Parsing |a [% b <: c :> d %] e|
	Parsed text:
	root. Attributes: {}
	   |--- string. Attributes: {text => "a "}
	   |--- open. Attributes: {text => "[%"}
	   |   |--- string. Attributes: {text => " b "}
	   |   |--- open. Attributes: {text => "<:"}
	   |   |   |--- string. Attributes: {text => " c "}
	   |   |--- close. Attributes: {text => ":>"}
	   |   |--- string. Attributes: {text => " d "}
	   |--- close. Attributes: {text => "%]"}
	   |--- string. Attributes: {text => " e"}
	Parse result: 0 (0 is success)
	--------------------------------------------------
	Parsing |a <: b <: c :> d :> e|
	Error: Parse failed. Opened delimiter <: again before closing previous one
	Text parsed so far:
	root. Attributes: {}
	   |--- string. Attributes: {text => "a "}
	   |--- open. Attributes: {text => "<:"}
	       |--- string. Attributes: {text => " b "}
	Parse result: 1 (0 is success)
	Deliberate error: Failed to parse |a <: b <: c :> d :> e|
	Error number: 2. Error message: Opened delimiter <: again before closing previous one
	--------------------------------------------------

=head1 Description

L<Text::Balanced::Marpa> provides a L<Marpa::R2>-based parser for extracting delimited text
sequences from strings.

See the L</FAQ> for various topics, including:

=over 4

=item o UFT8 handling

See t/utf8.t.

=item o Escaping delimiters within the text

See t/escapes.t.

=item o Options to make nested and/or overlapped delimiters fatal errors

See t/colons.t.

=item o Using delimiters which are part of another delimiter

See t/escapes.t and t/perl.delimiters.

=item o Processing the tree-structured output

See scripts/traverse.pl.

=item o Emulating L<Text::Xslate>'s use of '<:' and ':>

See t/colons.t and t/percents.t.

=item o Implementing a really trivial HTML parser

See scripts/traverse.pl and t/html.t.

In the same vein, see t/angle.brackets.t, for code where the delimiters are just '<' and '>'.

=item o Handling multiple sets of delimiters

See t/multiple.delimiters.t.

=item o Skipping (leading) characters in the input string

See t/skip.prefix.t.

=item o Implementing hard-to-read text strings as delimiters

See t/silly.delimiters.

=back

=head1 Distributions

This module is available as a Unix-style distro (*.tgz).

See L<http://savage.net.au/Perl-modules/html/installing-a-module.html>
for help on unpacking and installing distros.

=head1 Installation

Install L<Text::Balanced::Marpa> as you would any C<Perl> module:

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
[e.g. L</text([$stringref])>]):

=over 4

=item o close => $arrayref

An arrayref of strings, each one a closing delimiter.

The # of elements must match the # of elements in the 'open' arrayref.

See the L</FAQ> for details and warnings.

A value for this option is mandatory.

Default: None.

=item o length => $integer

The maxiumum length of the input string to process.

This parameter works in conjunction with the C<pos> parameter.

See the L</FAQ> for details.

Default: Calls Perl's length() function on the input string.

=item o next_few_limit => $integer

This controls how many characters are printed when displaying 'the next few chars'.

It only affects debug output.

Default: 20.

=item o open => $arrayref

An arrayref of strings, each one an opening delimiter.

The # of elements must match the # of elements in the 'open' arrayref.

See the L</FAQ> for details and warnings.

A value for this option is mandatory.

Default: None.

=item o options => $bit_string

This allows you to turn on various options.

Default: 0 (nothing is fatal).

See the L</FAQ> for details.

=item o pos => $integer

The offset within the input string at which to start processing.

This parameter works in conjunction with the C<length> parameter.

See the L</FAQ> for details.

Note: The first character in the input string is at pos == 0.

Default: 0.

=item o text => $a_reference_to_the_string_to_be_parsed

Default: \''.

=back

=head1 Methods

=head2 bnf()

Returns a string containing the grammar constructed based on user input.

=head2 close()

Get the arrayref of closing delimiters.

See also L</open()>.

See the L</FAQ> for details and warnings.

'close' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 delimiter_action()

Returns a hashref, where the keys are delimiters and the values are either 'open' or 'close'.

=head2 delimiter_frequency()

Returns a hashref where the keys are opening and closing delimiters, and the values are the # of
times each delimiter appears in the input stream.

The value is incremented for each opening delimiter and decremented for each closing delimiter.

=head2 error_message()

Returns the last error or warning message set.

Error messages always start with 'Error: '. Messages never end with "\n".

Parsing error strings is not a good idea, ever though this module's format for them is fixed.

See L</error_number()>.

=head2 error_number()

Returns the last error or warning number set.

Warnings have values < 0, and errors have values > 0.

If the value is > 0, the message has the prefix 'Error: ', and if the value is < 0, it has the
prefix 'Warning: '. If this is not the case, it's a reportable bug.

Possible values for error_number() and error_message():

=over 4

=item o 0 => ""

This is the default value.

=item o 1/-1 => "Last open delimiter: $lexeme_1. Unexpected closing delimiter: $lexeme_2"

If L</error_number()> returns 1, it's an error, and if it returns -1 it's a warning.

You can set the option C<overlap_is_fatal> to make it fatal.

=item o 2/-2 => "Opened delimiter $lexeme again before closing previous one"

If L</error_number()> returns 2, it's an error, and if it returns -2 it's a warning.

You can set the option C<nesting_is_fatal> to make it fatal.

=item o 3/-3 => "Ambiguous parse. Status: $status. Terminals expected: a, b, ..."

This message is only produced when the parse is ambiguous.

If L</error_number()> returns 3, it's an error, and if it returns -3 it's a warning.

You can set the option C<ambiguity_is_fatal> to make it fatal.

=item o 4 => "Backslash is forbidden as a delimiter character"

This preempts some types of sabotage.

This message can never be just a warning message.

=item o 5 => "Single-quotes are forbidden in multi-character delimiters"

This limitation is due to the syntax of
L<Marpa's DSL|https://metacpan.org/pod/distribution/Marpa-R2/pod/Scanless/DSL.pod>.

This message can never be just a warning message.

=item o 6/-6 => "Parse exhausted"

If L</error_number()> returns 6, it's an error, and if it returns -6 it's a warning.

You can set the option C<exhaustion_is_fatal> to make it fatal.

=item o 7 => 'Single-quote is forbidden as an escape character'

This limitation is due to the syntax of
L<Marpa's DSL|https://metacpan.org/pod/distribution/Marpa-R2/pod/Scanless/DSL.pod>.

This message can never be just a warning message.

=item o 8 => "There must be at least 1 pair of open/close delimiters"

This message can never be just a warning message.

=item o 9 => "The # of open delimiters must match the # of close delimiters"

This message can never be just a warning message.

=item o 10 => "Unexpected event name 'xyz'"

Marpa has trigged an event and it's name is not in the hash of event names derived from the BNF.

This message can never be just a warning message.

=item o 11 => "The code does not handle these events simultaneously: a, b, ..."

The code is written to handle single events at a time, or in rare cases, 2 events at the same time.
But here, multiple events have been triggered and the code cannot handle the given combination.

This message can never be just a warning message.

=back

See L</error_message()>.

=head2 escape_char()

Get the escape char.

=head2 format_node($options, $node)

Returns a string consisting of the node's name and, optionally, it's attributes.

Possible keys in the $options hashref:

=over 4

=item o no_attributes => $Boolean

If 1, the node's attributes are not included in the string returned.

Default: 0 (include attributes).

=back

Calls L</hashref2string($hashref)>.

Called by L</node2string($options, $is_last_node, $node, $vert_dashes)>.

You would not normally call this method.

If you don't wish to supply options, use format_node({}, $node).

=head2 hashref2string($hashref)

Returns the given hashref as a string.

Called by L</format_node($options, $node)>.

=head2 known_events()

Returns a hashref where the keys are event names and the values are 1.

=head2 length([$integer])

Here, the [] indicate an optional parameter.

Get or set the length of the input string to process.

See also the L</FAQ> and L</pos([$integer])>.

'length' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 matching_delimiter()

Returns a hashref where the keys are opening delimiters and the values are the corresponding closing
delimiters.

=head2 new()

See L</Constructor and Initialization> for details on the parameters accepted by L</new()>.

=head2 next_few_chars($stringref, $offset)

Returns a substring of $s, starting at $offset, for use in debug messages.

See L<next_few_limit([$integer])>.

=head2 next_few_limit([$integer])

Here, the [] indicate an optional parameter.

Get or set the number of characters called 'the next few chars', which are printed during debugging.

'next_few_limit' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 node2string($options, $is_last_node, $node, $vert_dashes)

Returns a string of the node's name and attributes, with a leading indent, suitable for printing.

Possible keys in the $options hashref:

=over 4

=item o no_attributes => $Boolean

If 1, the node's attributes are not included in the string returned.

Default: 0 (include attributes).

=back

Ignore the parameter $vert_dashes. The code uses it as temporary storage.

Calls L</format_node($options, $node)>.

Called by L</tree2string($options, [$some_tree])>.

=head2 open()

Get the arrayref of opening delimiters.

See also L</close()>.

See the L</FAQ> for details and warnings.

'open' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 options([$bit_string])

Here, the [] indicate an optional parameter.

Get or set the option flags.

For typical usage, see scripts/synopsis.pl.

See the L</FAQ> for details.

'options' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 parse([$stringref])

Here, the [] indicate an optional parameter.

This is the only method the user needs to call. All data can be supplied when calling L</new()>.

You can of course call other methods (e.g. L</text([$stringref])> ) after calling L</new()> but
before calling C<parse()>.

Note: If a stringref is passed to C<parse()>, it takes precedence over any stringref passed to
C<< new(text => $stringref) >>, and over any stringref passed to L</text([$stringref])>. Further,
the stringref passed to C<parse()> is passed to L</text([$stringref])>, meaning any subsequent
call to C<text()> returns the stringref passed to C<parse()>.

See scripts/samples.pl.

Returns 0 for success and 1 for failure.

If the value is 1, you should call L</error_number()> to find out what happened.

=head2 pos([$integer])

Here, the [] indicate an optional parameter.

Get or set the offset within the input string at which to start processing.

See also the L</FAQ> and L</length([$integer])>.

'pos' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 text([$stringref])

Here, the [] indicate an optional parameter.

Get or set a reference to the string to be parsed.

'text' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 tree()

Returns an object of type L<Tree>, which holds the parsed data.

Obviously, it only makes sense to call C<tree()> after calling C<parse()>.

See scripts/traverse.pl for sample code which processes this tree's nodes.

=head2 tree2string($options, [$some_tree])

Here, the [] represent an optional parameter.

If $some_tree is not supplied, uses the calling object's tree ($self -> tree).

Returns an arrayref of lines, suitable for printing. These lines do not end in "\n".

Draws a nice ASCII-art representation of the tree structure.

The tree looks like:

	Root. Attributes: {# => "0"}
	   |--- I. Attributes: {# => "1"}
	   |   |--- J. Attributes: {# => "3"}
	   |   |   |--- K. Attributes: {# => "3"}
	   |   |--- J. Attributes: {# => "4"}
	   |       |--- L. Attributes: {# => "5"}
	   |           |--- M. Attributes: {# => "5"}
	   |               |--- N. Attributes: {# => "5"}
	   |                   |--- O. Attributes: {# => "5"}
	   |--- H. Attributes: {# => "2"}
	   |   |--- J. Attributes: {# => "3"}
	   |   |   |--- K. Attributes: {# => "3"}
	   |   |--- J. Attributes: {# => "4"}
	   |       |--- L. Attributes: {# => "5"}
	   |           |--- M. Attributes: {# => "5"}
	   |               |--- N. Attributes: {# => "5"}
	   |                   |--- O. Attributes: {# => "5"}

Or, without attributes:

	Root
	   |--- I
	   |   |--- J
	   |   |   |--- K
	   |   |--- J
	   |       |--- L
	   |           |--- M
	   |               |--- N
	   |                   |--- O
	   |--- H
	   |   |--- J
	   |   |   |--- K
	   |   |--- J
	   |       |--- L
	   |           |--- M
	   |               |--- N
	   |                   |--- O

See scripts/samples.pl.

Example usage:

  print map("$_\n", @{$tree -> tree2string});

Can be called with $some_tree set to any $node, and will print the tree assuming $node is the root.

If you don't wish to supply options, use tree2string({}, $node).

Possible keys in the $options hashref (which defaults to {}):

=over 4

=item o no_attributes => $Boolean

If 1, the node's attributes are not included in the string returned.

Default: 0 (include attributes).

=back

Calls L</node2string($options, $is_last_node, $node, $vert_dashes)>.

=head1 FAQ

=head2 Where are the error messages and numbers described?

See L</error_message()> and L</error_number()>.

=head2 How do I escape delimiters?

By backslash-escaping the first character of all open and close delimiters which appear in the
text.

As an example, if the delimiters are '<:' and ':>', this means you have to escape I<all> the '<'
chars and I<all> the colons in the text.

The backslash is preserved in the output.

If you don't want to use backslash for escaping, or can't, you can pass a different escape character
to L</new()>.

See t/escapes.t.

=head2 How do the length and pos parameters to new() work?

The recognizer - an object of type Marpa::R2::Scanless::R - is called in a loop, like this:

	for
	(
		$pos = $self -> recce -> read($stringref, $pos, $length);
		$pos < $length;
		$pos = $self -> recce -> resume($pos)
	)

L</pos([$integer])> and L</length([$integer])> can be used to initialize $pos and $length.

Note: The first character in the input string is at pos == 0.

See L<https://metacpan.org/pod/distribution/Marpa-R2/pod/Scanless/R.pod#read> for details.

=head2 Does this package support Unicode/UTF8?

Yes. See t/escapes.t, t/multiple.quotes.t and t/utf8.t.

=head2 Does this package handler Perl delimiters (e.g. q|..|, qq|..|, qr/../, qw/../)?

See t/perl.delimiters.t.

=head2 Warning: Calling mutators after calling new()

The only mutator which works after calling new() is L</text([$stringref])>.

In particular, you can't call L</escape_char()>, L</open()> or L</close()> after calling L</new()>.
This is because parameters passed to C<new()> are interpolated into the grammar before parsing
begins. And that's why the docs for those methods all say 'Get the...' and not 'Get and set the...'.

To make the code work, you would have to manually call _validate_open_close(). But even then
a lot of things would have to be re-initialized to give the code any hope of working.

=head2 What is the format of the 'open' and 'close' parameters to new()?

Each of these parameters takes an arrayref as a value.

The # of elements in the 2 arrayrefs must be the same.

The 1st element in the 'open' arrayref is the 1st user-chosen opening delimiter, and the 1st
element in the 'close' arrayref must be the corresponding closing delimiter.

It is possible to use a delimiter which is part of another delimiter.

See scripts/samples.pl. It uses both '<' and '<:' as opening delimiters and their corresponding
closing delimiters are '>' and ':>'. Neat, huh?

=head2 What are the possible values for the 'options' parameter to new()?

Firstly, to make these constants available, you must say:

	use Text::Balanced::Marpa ':constants';

Secondly, more detail on errors and warnings can be found at L</error_number()>.

Thirdly, for usage of these option flags, see t/angle.brackets.t, t/colons.t, t/escapes.t,
t/multiple.quotes.t, t/percents.t and scripts/samples.pl.

Now the flags themselves:

=over 4

=item o nothing_is_fatal

This is the default.

It's value is 0.

=item o debug

Print extra stuff if this flag is set.

It's value is 1.

=item o print_warnings

Print various warnings if this flag is set:

=over 4

=item o The ambiguity status and terminals expected, if the parse is ambiguous

=item o See L</error_number()> for other warnings which might be printed

Ambiguity is not, in and of itself, an error. But see the C<ambiguity_is_fatal> option, below.

=back

It's tempting to call this option C<warnings>, but Perl already has C<use warnings>, so I didn't.

It's value is 2.

=item o overlap_is_fatal

This means overlapping delimiters cause a fatal error.

So, setting C<overlap_is_fatal> means '{Bold [Italic}]' would be a fatal error.

I use this example since it gives me the opportunity to warn you, this will I<not> do what you want
if you try to use the delimiters of '<' and '>' for HTML. That is, '<i><b>Bold Italic</i></b>' is
not an error because what overlap are '<b>' and '</i>' BUT THEY ARE NOT TAGS. The tags are '<' and
'>', ok? See also t/html.t.

It's value is 4.

=item o nesting_is_fatal

This means nesting of identical opening delimiters is fatal.

So, using C<nesting_is_fatal> means 'a <: b <: c :> d :> e' would be a fatal error.

It's value is 8.

=item o ambiguity_is_fatal

This makes L</error_number()> return 3 rather than -3.

It's value is 16.

=item o exhaustion_is_fatal

This makes L</error_number()> return 6 rather than -6.

It's value is 32.

=back

=head2 How do I print the tree built by the parser?

See L</Synopsis>.

=head2 How do I make use of the tree built by the parser?

See scripts/traverse.pl. It is a copy of t/html.t with tree-walking code instead of test code.

=head2 How is the parsed data held in RAM?

The parsed output is held in a tree managed by L<Tree>.

The tree always has a root node, which has nothing to do with the input data. So, even an empty
imput string will produce a tree with 1 node. This root has an empty hashref associated with it.

Nodes have a name and a hashref of attributes.

The name indicates the type of node. Names are one of these literals:

=over 4

=item o close

=item o open

=item o root

=item o text

=back

For 'open' and 'close', the delimiter is given by the value of the 'text' key in the hashref.

The (key => value) pairs in the hashref are:

=over 4

=item o text => $string

If the node name is 'open' or 'close', $string is the delimiter.

If the node name is 'text', $string is the verbatim text from the document.

Verbatim means, for example, that backslashes in the input are preserved.

=back

Try:

	perl -Ilib scripts/samples.pl info

=head2 How is HTML/XML handled?

The tree does not preserve the nested nature of HTML/XML.

Post-processing (valid) HTML could easily generate another view of the data.

But anyway, to get perfect HTML you'd be grabbing the output of L<Marpa::R2::HTML>, right?

See scripts/traverse.pl and t/html.t for a trivial HTML parser.

=head2 What is the homepage of Marpa?

L<http://savage.net.au/Marpa.html>.

That page has a long list of links.

=head2 How do I run author tests?

This runs both standard and author tests:

	shell> perl Build.PL; ./Build; ./Build authortest

=head1 TODO

=over 4

=item o Advanced error reporting

See L<https://jeffreykegler.github.io/Ocean-of-Awareness-blog/individual/2014/11/delimiter.html>.

Perhaps this could be a sub-class?

=item o I8N support for error messages

=item o An explicit test program for parse exhaustion

=back

=head1 See Also

L<Text::Balanced>.

L<Tree> and L<Tree::Persist>.

L<MarpaX::Demo::SampleScripts> - for various usages of L<Marpa::R2>, but not of this module.

=head1 Machine-Readable Change Log

The file Changes was converted into Changelog.ini by L<Module::Metadata::Changes>.

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

=head1 Thanks

Thanks to Jeffrey Kegler, who wrote Marpa and L<Marpa::R2>.

And thanks to rns (Ruslan Shvedov) for writing the grammar for double-quoted strings used in
L<MarpaX::Demo::SampleScripts>'s scripts/quoted.strings.02.pl. I adapted it to HTML (see
scripts/quoted.strings.05.pl in that module), and then incorporated the grammar into
L<GraphViz2::Marpa>, and - after more extensions - into this module.

Lastly, thanks to Robert Rothenberg for L<Const::Exporter>, a module which works the same way
Perl does.

=head1 Repository

L<https://github.com/ronsavage/Text-Balanced-Marpa>

=head1 Support

Email the author, or log a bug on RT:

L<https://rt.cpan.org/Public/Dist/Display.html?Name=Text::Balanced::Marpa>.

=head1 Author

L<Text::Balanced::Marpa> was written by Ron Savage I<E<lt>ron@savage.net.auE<gt>> in 2014.

Marpa's homepage: L<http://savage.net.au/Marpa.html>.

My homepage: L<http://savage.net.au/>.

=head1 Copyright

Australian copyright (c) 2014, Ron Savage.

	All Programs of mine are 'OSI Certified Open Source Software';
	you can redistribute them and/or modify them under the terms of
	The Artistic License 2.0, a copy of which is available at:
	http://opensource.org/licenses/alphabetical.

=cut
