use strict;
use warnings;

use ExtUtils::MakeMaker;

# -----------------------------------------------

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

my(%params) =
(
	($] ge '5.005') ?
	(
		AUTHOR		=> 'Ron Savage (ron@savage.net.au)',
		ABSTRACT	=> 'Extract delimited text sequences from strings',
	) : (),
	clean =>
	{
		FILES => 'blib/* Makefile MANIFEST Text-Balanced-Marpa-*'
	},
	dist =>
	{
		COMPRESS	=> 'gzip',
		SUFFIX		=> 'gz'
	},
	DISTNAME	=> 'Text-Balanced-Marpa',
	LICENSE		=> 'perl',
	NAME		=> 'Text::Balanced::Marpa',
	PL_FILES	=> {},
	PREREQ_PM	=>
	{
		'Const::Exporter'	=> 'v0.2.3',
		'Marpa::R2'			=> 2.096000,
		'Moo'				=> 1.006000,
		'open'				=> 0,
		'strict'			=> 0,
		'Tree'				=> 1.06,
		'Try::Tiny'			=> 0.09,
		'Types::Standard'	=> 1.000004,
		'utf8'				=> 0,
		'warnings'			=> 0,
	},
	TEST_REQUIRES =>
	{
		'Test::More'	=> 0,
		'Test::Pod'		=> 0,
	},
	VERSION_FROM	=> 'lib/Text/Balanced/Marpa.pm',
	INSTALLDIRS		=> 'site',
	EXE_FILES		=> [],
);

if ( ($ExtUtils::MakeMaker::VERSION =~ /^\d\.\d\d$/) && ($ExtUtils::MakeMaker::VERSION > 6.30) )
{
	$params{LICENSE} = 'artistic_2';
}

if ($ExtUtils::MakeMaker::VERSION ge '6.46')
{
	$params{META_MERGE} =
	{
		'meta-spec' =>
		{
			version => 2,
		},
		resources   =>
		{
			bugtracker => 'https://rt.cpan.org/Public/Dist/Display.html?Name=Text-Balanced-Marpa',
			license    => 'http://opensource.org/licenses/Artistic-2.0',
			repository =>
			{
				type => 'git',
				url  => 'https://github.com/ronsavage/Text-Balanced-Marpa.git',
				web  => 'https://github.com/ronsavage/Text-Balanced-Marpa',
			},
		},
	};
}

WriteMakefile(%params);
