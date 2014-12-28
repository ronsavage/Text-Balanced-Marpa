#!/bin/bash

FILE=Perl-modules/html/Text/Balanced/Marpa.html

pod2html.pl -i lib/Text/Balanced/Marpa.pm -o $DR/$FILE

cp $DR/$FILE ~/savage.net.au/$FILE
