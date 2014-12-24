#!/bin/bash

DEST=$DR/Perl-modules/html/Text/Balanced

pod2html.pl -i lib/Text/Balanced/Marpa.pm -o $DEST/Marpa.html
